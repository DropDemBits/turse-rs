//! Testing helpers

use toc_ast_db::db::SourceParser;
use toc_ast_db::SourceRoots;
use toc_hir::library::LibraryId;
use toc_hir_db::HirDatabase;
use toc_salsa::salsa;
use toc_vfs::db::VfsDatabaseExt;

#[salsa::database(
    toc_vfs::db::FileSystemStorage,
    toc_ast_db::db::SourceParserStorage,
    toc_hir_db::HirDatabaseStorage,
    crate::db::TypeInternStorage,
    crate::db::TypeDatabaseStorage,
    crate::db::ConstEvalStorage,
    crate::HirAnalysisStorage
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
        let root_file = db.vfs.intern_path("src/main.t".into());
        db.update_file(root_file, Some(source.into()));

        let source_roots = SourceRoots::new(vec![root_file]);
        db.set_source_roots(source_roots);

        let library_id = db.library_graph().result().library_of(root_file);

        (db, library_id)
    }
}
