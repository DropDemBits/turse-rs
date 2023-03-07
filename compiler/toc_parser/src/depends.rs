//! File dependencies extracted from a file
use std::sync::Arc;

use indexmap::IndexMap;
use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, Span};
use toc_syntax::{
    ast,
    ast::{AstNode, ExternalItemOwner},
    AstPtr, SyntaxNode,
};

/// What other file sources a given file depends on
#[derive(Debug, PartialEq, Eq)]
pub struct FileDepends {
    depends: Vec<Dependency>,
}

impl FileDepends {
    pub fn dependencies(&self) -> &[Dependency] {
        &self.depends
    }
}

pub type ExternalLink = AstPtr<ast::ExternalRef>;

/// What other files each [`ast::ExternalRef`] refers to
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ExternalLinks {
    links: IndexMap<ExternalLink, FileId>,
}

impl ExternalLinks {
    /// Binds an external link to a file
    pub fn bind(&mut self, link: ExternalLink, to: FileId) {
        self.links.insert(link, to);
    }

    /// Looks up which file an external link might refer to
    pub fn links_to(&self, link: ExternalLink) -> Option<FileId> {
        self.links.get(&link).copied()
    }

    /// Iterator over all of the other files each [`ast::ExternalRef`] in a file refers to
    pub fn all_links(&self) -> impl Iterator<Item = FileId> + '_ {
        self.links.values().copied()
    }

    /// Iterator over all of the other files each importable [`ast::ExternalRef`] in a file refers to
    pub fn import_links(&self) -> impl Iterator<Item = FileId> + '_ {
        self.links.iter().flat_map(|(k, v)| {
            (k.syntax_node_ptr().kind() == toc_syntax::SyntaxKind::ExternalItem).then_some(*v)
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub link_from: ExternalLink,

    pub relative_path: String,
}

pub(crate) fn gather_dependencies(
    file: FileId,
    syntax: SyntaxNode,
) -> CompileResult<Arc<FileDepends>> {
    let mut messages = toc_reporting::MessageSink::new();
    let mut dependencies = vec![];

    let root = ast::Source::cast(syntax).unwrap();
    let stmt_list = root.stmt_list().unwrap();
    let first_stmt = stmt_list.stmts().next();
    let is_child_unit = root.unit_token().is_some();

    // Gather external items
    let external_items = if !is_child_unit {
        root.import_stmt().map(|node| node.external_items())
    } else if let Some(first_stmt) = first_stmt {
        match first_stmt {
            ast::Stmt::ModuleDecl(decl) => Some(decl.external_items()),
            ast::Stmt::ClassDecl(decl) => Some(decl.external_items()),
            ast::Stmt::MonitorDecl(decl) => Some(decl.external_items()),
            _ => None,
        }
    } else {
        // Not anything
        None
    }
    .unwrap_or_default();

    // Convert external items into paths
    let external_deps = external_items
        .into_iter()
        .filter_map(|item| {
            let path = if let Some(path) = item.path() {
                // From the given path
                extract_relative_path(path, file, &mut messages)?
            } else {
                // From the item name
                item.name()
                    .and_then(|name| name.identifier_token())
                    .map(|ident| ident.text().to_string())?
            };

            let link_from = AstPtr::new(&ast::ExternalRef::ExternalItem(item));
            Some((path, link_from))
        })
        .map(|(relative_path, link_from)| Dependency {
            link_from,
            relative_path,
        });

    dependencies.extend(external_deps);

    // Gather include stmts
    for node in root.syntax().descendants() {
        let Some(include) = ast::PPInclude::cast(node) else { continue; };
        let Some(path) = include.path()
            .and_then(|path| extract_relative_path(path, file, &mut messages))
        else {
            continue;
        };

        let link_from = AstPtr::new(&ast::ExternalRef::PPInclude(include));

        dependencies.push(Dependency {
            link_from,
            relative_path: path,
        });
    }

    let dependencies = FileDepends {
        depends: dependencies,
    };

    CompileResult::new(Arc::new(dependencies), messages.finish())
}

fn extract_relative_path(
    path: ast::LiteralExpr,
    file: FileId,
    messages: &mut MessageSink,
) -> Option<String> {
    let (value, errors) = path.literal().unwrap();
    let toc_syntax::LiteralValue::String(relative_path) = value
    else {
        // Always defined as a string literal value
        unreachable!()
    };

    if let Some(errors) = errors {
        // Report errors
        let range = path.syntax().text_range();
        let span = Span::new(file, range);
        let mut message = messages.error_detailed(errors.header(), span);

        for (msg, range) in errors.parts(range) {
            let span = Span::new(file, range);
            message = message.with_error(&msg, span);
        }

        message.finish();

        None
    } else {
        // Use the path
        Some(relative_path)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn get_deps(source: &str) -> CompileResult<(Arc<FileDepends>, SyntaxNode)> {
        let file_id = FileId::dummy(0);
        let parsed = crate::parse(file_id, source);
        gather_dependencies(file_id, parsed.result().syntax())
            .map(|deps| (deps, parsed.result().syntax()))
    }

    #[test]
    fn gather_no_deps() {
        let depend_res = get_deps(r#"moot"#);
        let (file_deps, _) = depend_res.result();
        assert!(file_deps.dependencies().is_empty());
    }

    #[test]
    fn gather_includes() {
        let depend_res = get_deps(
            r#"
    include "\uD2\u77\uD3_whats_this"
    include "me_time"
    include 'bad!' % Invalid include stmt
    "#,
        );
        let (file_deps, root) = depend_res.result();
        let dependencies = file_deps.dependencies();

        assert!(!dependencies.is_empty());

        assert!(matches!(
            dependencies[0].link_from.to_node(root),
            ast::ExternalRef::PPInclude(_)
        ));
        assert_eq!(
            dependencies[0].relative_path,
            "\u{D2}\u{77}\u{D3}_whats_this"
        );

        assert!(matches!(
            dependencies[1].link_from.to_node(root),
            ast::ExternalRef::PPInclude(_)
        ));
        assert_eq!(dependencies[1].relative_path, "me_time");

        assert_eq!(dependencies.get(2), None);
    }

    #[test]
    fn gather_main_imports() {
        let depend_res = get_deps(r#"import "a", name, and_ in "external_place""#);
        let (file_deps, root) = depend_res.result();
        let dependencies = file_deps.dependencies();

        assert!(!dependencies.is_empty());

        assert!(matches!(
            dependencies[0].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[0].relative_path, "a");

        assert!(matches!(
            dependencies[1].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[1].relative_path, "name");

        assert!(matches!(
            dependencies[2].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[2].relative_path, "external_place");
    }

    #[test]
    fn gather_no_deps_with_module() {
        // Module is not the root module
        let depend_res = get_deps(r#"module b import c end b"#);
        let (file_deps, _root) = depend_res.result();
        let dependencies = file_deps.dependencies();
        assert!(dependencies.is_empty());
    }

    #[test]
    fn gather_deps_child_class() {
        // Module is not the root module
        let depend_res = get_deps(
            r#"
    unit class b
        inherit a
        implement b
        implement by c
        import d
        export e
    end b"#,
        );
        let (file_deps, root) = depend_res.result();
        let dependencies = file_deps.dependencies();

        assert!(matches!(
            dependencies[0].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[0].relative_path, "a");

        assert!(matches!(
            dependencies[1].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[1].relative_path, "b");

        assert!(matches!(
            dependencies[2].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[2].relative_path, "c");

        assert!(matches!(
            dependencies[3].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));
        assert_eq!(dependencies[3].relative_path, "d");

        assert_eq!(dependencies.get(4), None);
    }

    #[test]
    fn gather_main_mixed_deps() {
        let depend_res = get_deps(
            r#"
    import "a", name, and_ in "external_place"
    include "bob"
    "#,
        );
        let (file_deps, root) = depend_res.result();
        let dependencies = file_deps.dependencies();

        assert!(!dependencies.is_empty());

        assert_eq!(dependencies[0].relative_path, "a");
        assert!(matches!(
            dependencies[0].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));

        assert_eq!(dependencies[1].relative_path, "name");
        assert!(matches!(
            dependencies[1].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));

        assert_eq!(dependencies[2].relative_path, "external_place");
        assert!(matches!(
            dependencies[2].link_from.to_node(root),
            ast::ExternalRef::ExternalItem(_)
        ));

        assert_eq!(dependencies[3].relative_path, "bob");
        assert!(matches!(
            dependencies[3].link_from.to_node(root),
            ast::ExternalRef::PPInclude(_)
        ));
    }

    #[test]
    fn gather_bad_paths() {
        let depend_res = get_deps(
            r#"
    import "a^"
    include "k\!"
    "#,
        );
        let (file_deps, _) = depend_res.result();
        let dependencies = file_deps.dependencies();

        assert!(dependencies.is_empty(), "{dependencies:?}");
        eprintln!("{:?}", depend_res.messages())
    }
}
