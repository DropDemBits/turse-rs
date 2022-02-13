//! Source file interpretation queries

use std::{
    collections::{HashSet, VecDeque},
    sync::Arc,
};

use toc_reporting::CompileResult;
use toc_source_graph::{DependGraph, SourceDepend, SourceKind};
use toc_span::FileId;
use toc_vfs::{HasVfs, PathResolution};
use toc_vfs_db::db::{self as vfs_db, VfsDatabaseExt};

use crate::db;

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
) -> CompileResult<toc_parser::FileDepends> {
    let cst = db.parse_file(file_id);
    toc_parser::parse_depends(file_id, cst.result().syntax())
}

pub(crate) fn depend_of(
    db: &dyn db::SourceParser,
    library: FileId,
    from: FileId,
    relative_path: String,
) -> (FileId, SourceKind) {
    db.depend_graph(library).depend_of(from, relative_path)
}

impl<T> db::AstDatabaseExt for T
where
    T: HasVfs + vfs_db::FileSystem + db::SourceParser,
{
    fn invalidate_source_graph(&mut self, loader: &dyn toc_vfs::FileLoader) {
        let db = self;
        let source_graph = db.source_graph();
        let mut pending_files: VecDeque<_> = vec![].into();
        let mut visited_files: HashSet<_> = HashSet::default();

        for library_root in source_graph.library_roots() {
            let mut depend_graph = DependGraph::new(library_root);
            pending_files.push_back(library_root);

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

                let deps = db.parse_depends(current_file);
                for dep in deps.result().dependencies() {
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

                    // Update the resolution path
                    let kind = match dep.kind {
                        toc_parser::DependencyKind::Include => SourceKind::Include,
                        toc_parser::DependencyKind::Import => SourceKind::Unit,
                    };

                    let info = SourceDepend {
                        relative_path: dep.relative_path.clone(),
                        kind,
                    };
                    depend_graph.add_source_dep(current_file, child, info);
                    pending_files.push_back(child);
                }
            }

            // Update the depend graph
            db.set_depend_graph(library_root, Arc::new(depend_graph));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::sync::Arc;
    use toc_salsa::salsa;
    use toc_source_graph::SourceGraph;

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
            let fixture = toc_vfs::generate_vfs(&mut db, source);
            db.insert_fixture(fixture);

            let root_file = db.vfs.intern_path("src/main.t".into());
            let mut source_graph = SourceGraph::default();
            source_graph.add_root(root_file);
            db.set_source_graph(Arc::new(source_graph));
            db.invalidate_source_graph(&toc_vfs::DummyFileLoader);

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
