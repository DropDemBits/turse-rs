//! Source file interpretation queries

use std::{
    collections::{BTreeSet, VecDeque},
    sync::Arc,
};

use toc_parser::ExternalLinks;
use toc_reporting::CompileResult;
use toc_span::FileId;
use toc_vfs_db::SourceFile;

use crate::Db;

/// What other files a given file refers to
#[salsa::tracked]
pub fn file_links(db: &dyn Db, source: SourceFile) -> Arc<toc_parser::ExternalLinks> {
    let mut links = ExternalLinks::default();
    let deps = crate::parse_depends(db, source);
    for dep in deps.result().dependencies() {
        // Get the target file
        let child = toc_vfs_db::resolve_path(
            db.upcast_to_vfs_db(),
            source.path(db.upcast_to_vfs_db()),
            dep.relative_path.clone(),
        );

        links.bind(dep.link_from.clone(), child);
    }

    Arc::new(links)
}

/// Parses the given file
#[salsa::tracked]
pub fn parse_file(db: &dyn Db, source: SourceFile) -> CompileResult<toc_parser::ParseTree> {
    // FIXME: If a load error is present, then add it to the parse result / create a new one
    toc_parser::parse(
        source.path(db.upcast_to_vfs_db()),
        source.contents(db.upcast_to_vfs_db()),
    )
}

/// Validates the file according to grammar validation rules
#[salsa::tracked]
pub fn validate_file(db: &dyn Db, source: SourceFile) -> CompileResult<()> {
    let cst = crate::parse_file(db, source);
    toc_validate::validate_ast(source.path(db.upcast_to_vfs_db()), cst.result().syntax())
}

/// Parse out the dependencies of a file
#[salsa::tracked]
pub fn parse_depends(
    db: &dyn Db,
    source: SourceFile,
) -> CompileResult<Arc<toc_parser::FileDepends>> {
    let cst = crate::parse_file(db, source);
    toc_parser::parse_depends(source.path(db.upcast_to_vfs_db()), cst.result().syntax())
}

/// Gets the [`ExternalLink`](toc_parser::ExternalLink)'s corresponding file
#[salsa::tracked]
pub fn file_link_of(
    db: &dyn Db,
    source: SourceFile,
    link: toc_parser::ExternalLink,
) -> Option<FileId> {
    crate::file_links(db, source).links_to(link)
}

/// Gets the set of all the transient file dependencies of `root`
#[salsa::tracked]
pub fn reachable_files(db: &dyn Db, root: SourceFile) -> Arc<BTreeSet<SourceFile>> {
    let mut files = BTreeSet::default();
    let mut pending_queue = VecDeque::default();

    pending_queue.push_back(root);

    while let Some(current_file) = pending_queue.pop_front() {
        // Skip if we've already explored this file
        if files.contains(&current_file) {
            continue;
        }
        files.insert(current_file);

        // add all of this file's linked bits to the queue
        pending_queue.extend(
            crate::file_links(db, current_file)
                .all_links()
                .map(|path| toc_vfs_db::source_of(db.upcast_to_vfs_db(), path)),
        );
    }

    Arc::new(files)
}

/// Gets the set of all the transient file imports of `root`
#[salsa::tracked]
pub fn reachable_imported_files(db: &dyn Db, root: SourceFile) -> Arc<BTreeSet<SourceFile>> {
    let mut files = BTreeSet::default();
    let mut pending_queue = VecDeque::default();

    pending_queue.push_back(root);

    while let Some(current_file) = pending_queue.pop_front() {
        // Skip if we've already explored this file
        if files.contains(&current_file) {
            continue;
        }
        files.insert(current_file);

        // add all of this file's linked bits to the queue
        pending_queue.extend(
            crate::file_links(db, current_file)
                .all_links()
                .map(|path| toc_vfs_db::source_of(db.upcast_to_vfs_db(), path)),
        );
    }

    Arc::new(files)
}

#[cfg(remove)]
impl<T> crate::db::AstDatabaseExt for T
where
    T: Db,
{
    fn rebuild_file_links(&mut self, loader: &dyn toc_vfs::FileLoader) {
        let db = self;
        let source_graph = db.source_graph();
        let mut pending_files: VecDeque<_> = VecDeque::default();
        let mut visited_files: HashSet<_> = HashSet::default();

        for (_, library) in source_graph.all_libraries() {
            pending_files.push_back(library.root);

            while let Some(current_file) = pending_files.pop_front() {
                // Don't visit files we've already encountered
                let already_visited = !visited_files.insert(current_file);
                if already_visited {
                    continue;
                }

                // Load in the file source
                let path = db.lookup_intern_path(current_file);
                let res = loader.load_file(path.as_std_path());
                db.update_file(current_file, res);

                let mut links = ExternalLinks::default();
                let deps = db.parse_depends(current_file);
                for dep in deps.result().dependencies() {
                    // Get the target file
                    let child = db.resolve_path(Some(current_file), &dep.relative_path, loader);

                    // Explore later
                    pending_files.push_back(child);

                    links.bind(dep.link_from.clone(), child);
                }

                // Update the file links
                db.set_file_links(current_file, Arc::new(links));
            }
        }
    }
}

#[cfg(remove)] // Would probably be useful during hir_lowering, since that where the source graph is used more
#[cfg(test)]
mod test {
    use super::*;

    use std::sync::Arc;
    use toc_salsa::salsa;
    use toc_source_graph::{ArtifactKind, SourceGraph, SourceLibrary};
    use toc_vfs_db::db::{FileSystemStorage, PathInternStorage};

    use crate::db::{AstDatabaseExt, SourceParser};

    #[salsa::database(FileSystemStorage, PathInternStorage, crate::db::SourceParserStorage)]
    #[derive(Default)]
    struct TestDb {
        storage: salsa::Storage<Self>,
    }

    impl salsa::Database for TestDb {}

    impl TestDb {
        fn from_source(source: &str) -> Self {
            let mut db = TestDb::default();
            let fixture = toc_vfs::generate_vfs(source).unwrap();
            db.insert_fixture(fixture);

            let root_file = db.intern_path("src/main.t".into());
            let mut source_graph = SourceGraph::default();
            let _lib = source_graph.add_library(SourceLibrary {
                artifact: ArtifactKind::Binary,
                name: "main".into(),
                root: root_file,
            });
            db.set_source_graph(Arc::new(source_graph));
            db.rebuild_file_links(&toc_vfs::DummyFileLoader);

            db
        }
    }

    #[test]
    fn test_load_cyclic_dep_include() {
        let _ = TestDb::from_source(
            r#"
        %%- src/main.t
        include "main.t"
        "#,
        );
    }

    #[test]
    fn test_load_cyclic_dep_import() {
        let _ = TestDb::from_source(
            r#"
        %%- src/main.t
        unit module a
            import a in "main.t"
        end a
        "#,
        );
    }

    #[test]
    fn test_load_mutual_cyclic_dep_include() {
        let _ = TestDb::from_source(
            r#"
        %%- src/main.t
        include "main.t"
        %%- src/other.t
        include "other.t"
        "#,
        );
    }

    #[test]
    fn test_load_mutual_cyclic_dep_import() {
        let _ = TestDb::from_source(
            r#"
        %%- src/main.t
        unit module a
            import b in "other.t"
        end a
        %%- src/other.t
        unit module b
            import a in "main.t"
        end b
        "#,
        );
    }
}
