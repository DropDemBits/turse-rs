//! Testing helpers

use std::sync::Arc;

use toc_ast_db::{
    db::{AstDatabaseExt, SourceParser},
    SourceGraph,
};
use toc_hir::library::LibraryId;
use toc_hir_db::db::HirDatabase;
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
        let fixture = toc_vfs::generate_vfs(&mut db, source);
        db.insert_fixture(fixture);

        let root_file = db.vfs.intern_path("src/main.t".into());
        let mut source_graph = SourceGraph::default();
        source_graph.add_root(root_file);
        db.set_source_graph(Arc::new(source_graph));
        db.invalidate_source_graph(&toc_vfs::DummyFileLoader);

        let library_id = db.library_graph().library_of(root_file);

        (db, library_id)
    }
}
