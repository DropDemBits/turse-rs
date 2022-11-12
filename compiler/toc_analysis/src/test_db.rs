//! Testing helpers

use std::sync::Arc;

use toc_ast_db::{
    db::{AstDatabaseExt, SourceParser},
    SourceGraph,
};
use toc_hir::library::LibraryId;
use toc_salsa::salsa;
use toc_vfs_db::db::VfsDatabaseExt;

#[salsa::database(
    toc_vfs_db::db::FileSystemStorage,
    toc_ast_db::db::SourceParserStorage,
    toc_hir_db::db::HirDatabaseStorage,
    crate::db::TypeInternStorage,
    crate::db::TypeDatabaseStorage,
    crate::db::ConstEvalStorage,
    crate::db::HirAnalysisStorage
)]
#[derive(Default)]
pub(crate) struct TestDb {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for TestDb {}

toc_vfs::impl_has_vfs!(TestDb, vfs);

impl TestDb {
    pub(crate) fn from_source(source: &str) -> (Self, LibraryId) {
        let mut db = TestDb::default();
        let fixture = toc_vfs::generate_vfs(&mut db, source).unwrap();
        db.insert_fixture(fixture);

        let root_file = db.vfs.intern_path("src/main.t".into());
        let mut source_graph = SourceGraph::default();
        let library_id = source_graph.add_library(toc_hir::library_graph::Library {
            name: "main".into(),
            root: root_file,
            artifact: toc_hir::library_graph::ArtifactKind::Binary,
        });
        db.set_source_graph(Arc::new(source_graph));
        db.invalidate_source_graph(&toc_vfs::DummyFileLoader);

        (db, library_id)
    }
}
