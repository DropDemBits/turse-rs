//! Source file interpretation queries

use std::{
    collections::{BTreeSet, HashSet, VecDeque},
    sync::Arc,
};

use toc_parser::ExternalLinks;
use toc_reporting::CompileResult;
use toc_source_graph::{LibraryId, LibraryRef};
use toc_span::FileId;
use toc_vfs::{HasVfs, PathResolution};
use toc_vfs_db::db::VfsDatabaseExt;

use crate::db;

pub(crate) fn source_library(db: &dyn db::SourceParser, library_id: LibraryId) -> LibraryRef {
    LibraryRef::new(db.source_graph(), library_id)
}

pub(crate) fn parse_file(
    db: &dyn db::SourceParser,
    file_id: FileId,
) -> CompileResult<toc_parser::ParseTree> {
    let source = db.file_source(file_id);
    // FIXME: If a load error is present, then add it to the parse result / create a new one
    toc_parser::parse(file_id, &source.0)
}

pub(crate) fn validate_file(db: &dyn db::SourceParser, file_id: FileId) -> CompileResult<()> {
    let cst = db.parse_file(file_id);
    toc_validate::validate_ast(file_id, cst.result().syntax())
}

pub(crate) fn parse_depends(
    db: &dyn db::SourceParser,
    file_id: FileId,
) -> CompileResult<Arc<toc_parser::FileDepends>> {
    let cst = db.parse_file(file_id);
    toc_parser::parse_depends(file_id, cst.result().syntax())
}

pub(crate) fn file_link_of(
    db: &dyn db::SourceParser,
    file_id: FileId,
    link: toc_parser::ExternalLink,
) -> Option<FileId> {
    db.file_links(file_id).links_to(link)
}

pub(crate) fn reachable_files(db: &dyn db::SourceParser, root: FileId) -> Arc<BTreeSet<FileId>> {
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
        pending_queue.extend(db.file_links(current_file).all_links());
    }

    Arc::new(files)
}

pub(crate) fn reachable_imported_files(
    db: &dyn db::SourceParser,
    root: FileId,
) -> Arc<BTreeSet<FileId>> {
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
        pending_queue.extend(db.file_links(current_file).import_links());
    }

    Arc::new(files)
}

impl<T> db::AstDatabaseExt for T
where
    T: HasVfs + db::SourceParser,
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
                let path = db.get_vfs().lookup_path(current_file);
                let res = loader.load_file(path);
                db.update_file(current_file, res);

                let mut links = ExternalLinks::default();
                let deps = db.parse_depends(current_file);
                for dep in deps.result().dependencies() {
                    // Get the target file
                    let child = match db
                        .get_vfs()
                        .resolve_path(Some(current_file), &dep.relative_path)
                    {
                        PathResolution::Interned(id) => id,
                        PathResolution::NewPath(path) => {
                            let intern_path = loader.normalize_path(&path).unwrap_or(path);
                            db.get_vfs_mut().intern_path(intern_path)
                        }
                    };

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

#[cfg(test)]
mod test {
    use super::*;

    use std::sync::Arc;
    use toc_salsa::salsa;
    use toc_source_graph::{ArtifactKind, SourceGraph, SourceLibrary};

    use crate::db::{AstDatabaseExt, SourceParser};

    #[salsa::database(toc_vfs_db::db::FileSystemStorage, crate::db::SourceParserStorage)]
    #[derive(Default)]
    struct TestDb {
        storage: salsa::Storage<Self>,
        vfs: toc_vfs::Vfs,
    }

    impl salsa::Database for TestDb {}

    toc_vfs::impl_has_vfs!(TestDb, vfs);

    impl TestDb {
        fn from_source(source: &str) -> Self {
            let mut db = TestDb::default();
            let fixture = toc_vfs::generate_vfs(&mut db, source).unwrap();
            db.insert_fixture(fixture);

            let root_file = db.vfs.intern_path("src/main.t".into());
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
