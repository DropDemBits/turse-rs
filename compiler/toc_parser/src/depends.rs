//! File dependencies extracted from a file
use std::sync::Arc;

use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, Span};
use toc_syntax::ast::{AstNode, ExternalItemOwner};
use toc_syntax::{ast, SyntaxNode};

/// What other file sources a given file depends on
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileDepends {
    depends: Arc<Vec<Dependency>>,
}

impl FileDepends {
    pub fn dependencies(&self) -> &[Dependency] {
        &self.depends
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub kind: DependencyKind,
    pub relative_path: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DependencyKind {
    /// The dependency is to an include file, and should not be added to the unit tree
    Include,
    /// The dependency is to a child unit, and should be added to the unit tree
    ///
    /// `import`, `inherit`, `implement` and `implement by` counts as import dependencies
    Import,
}

pub(crate) fn gather_dependencies(
    file: Option<FileId>,
    syntax: SyntaxNode,
) -> CompileResult<FileDepends> {
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
            if let Some(path) = item.path() {
                // From the given path
                extract_relative_path(path, file, &mut messages)
            } else {
                // From the item name
                item.name()
                    .and_then(|name| name.identifier_token())
                    .map(|ident| ident.text().to_string())
            }
        })
        .map(|relative_path| Dependency {
            kind: DependencyKind::Import,
            relative_path,
        });

    dependencies.extend(external_deps);

    // Gather include stmts
    for node in root.syntax().descendants() {
        ast::PPInclude::cast(node)
            .and_then(|node| node.path())
            .and_then(|path| extract_relative_path(path, file, &mut messages))
            .map(|path| {
                // Add dependency
                dependencies.push(Dependency {
                    kind: DependencyKind::Include,
                    relative_path: path,
                });
            });
    }

    let dependencies = FileDepends {
        depends: Arc::new(dependencies),
    };

    CompileResult::new(dependencies, messages.finish())
}

fn extract_relative_path(
    path: ast::LiteralExpr,
    file: Option<FileId>,
    messages: &mut MessageSink,
) -> Option<String> {
    let (value, errors) = path.literal().unwrap();
    let relative_path = if let toc_syntax::LiteralValue::String(path) = value {
        path
    } else {
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

#[test]
fn gather_no_deps() {
    let parsed = super::parse(None, r#"moot"#);
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();
    assert!(dependencies.is_empty());
}

#[test]
fn gather_includes() {
    let parsed = super::parse(
        None,
        r#"
    include "\uD2\u77\uD3_whats_this"
    include "me_time"
    include 'bad!' % Invalid include stmt
    "#,
    );
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();

    assert!(!dependencies.is_empty());
    assert_eq!(
        dependencies[0],
        Dependency {
            kind: DependencyKind::Include,
            relative_path: "\u{D2}\u{77}\u{D3}_whats_this".to_string()
        }
    );
    assert_eq!(
        dependencies[1],
        Dependency {
            kind: DependencyKind::Include,
            relative_path: "me_time".to_string()
        }
    );
    assert_eq!(dependencies.get(2), None);
}

#[test]
fn gather_main_imports() {
    let parsed = super::parse(
        None,
        r#"
    import "a", name, and_ in "external_place"
    "#,
    );
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();

    assert!(!dependencies.is_empty());
    assert_eq!(
        dependencies[0],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "a".to_string()
        }
    );
    assert_eq!(
        dependencies[1],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "name".to_string()
        }
    );
    assert_eq!(
        dependencies[2],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "external_place".to_string()
        }
    );
}

#[test]
fn gather_no_deps_with_module() {
    // Module is not the root module
    let parsed = super::parse(None, r#"module b import c end b"#);
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();
    assert!(dependencies.is_empty());
}

#[test]
fn gather_deps_child_class() {
    // Module is not the root module
    let parsed = super::parse(
        None,
        r#"
    unit class b
        inherit a
        implement b
        implement by c
        import d
        export e
    end b"#,
    );
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();

    assert_eq!(
        dependencies[0],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "a".to_string()
        }
    );
    assert_eq!(
        dependencies[1],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "b".to_string()
        }
    );
    assert_eq!(
        dependencies[2],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "c".to_string()
        }
    );
    assert_eq!(
        dependencies[3],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "d".to_string()
        }
    );
    assert_eq!(dependencies.get(4), None);
}

#[test]
fn gather_main_mixed_deps() {
    let parsed = super::parse(
        None,
        r#"
    import "a", name, and_ in "external_place"
    include "bob"
    "#,
    );
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();

    assert!(!dependencies.is_empty());
    assert_eq!(
        dependencies[0],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "a".to_string()
        }
    );
    assert_eq!(
        dependencies[1],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "name".to_string()
        }
    );
    assert_eq!(
        dependencies[2],
        Dependency {
            kind: DependencyKind::Import,
            relative_path: "external_place".to_string()
        }
    );
    assert_eq!(
        dependencies[3],
        Dependency {
            kind: DependencyKind::Include,
            relative_path: "bob".to_string()
        }
    );
}

#[test]
fn gather_bad_paths() {
    let parsed = super::parse(
        None,
        r#"
    import "a^"
    include "k\!"
    "#,
    );
    let depend_res = gather_dependencies(None, parsed.result().syntax());
    let dependencies = depend_res.result().dependencies();

    assert!(dependencies.is_empty(), "{:?}", dependencies);
    eprintln!("{:?}", depend_res.messages())
}
