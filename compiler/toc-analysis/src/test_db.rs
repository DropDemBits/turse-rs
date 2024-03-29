//! Testing helpers

use toc_hir::package_graph::SourcePackage;
use toc_paths::RawPath;
use toc_source_graph::{DependencyList, RootPackages};
use toc_vfs_db::{SourceTable, VfsBridge, VfsDbExt};

#[salsa::db(
    toc_paths::Jar,
    toc_vfs_db::Jar,
    toc_source_graph::Jar,
    toc_ast_db::Jar,
    toc_hir_lowering::Jar,
    toc_hir_db::Jar,
    crate::TypeJar,
    crate::ConstEvalJar,
    crate::AnalysisJar
)]
#[derive(Default)]
pub(crate) struct TestDb {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl VfsBridge for TestDb {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }
}

impl salsa::Database for TestDb {}

impl TestDb {
    pub(crate) fn from_source(source: &str) -> (Self, SourcePackage) {
        let mut db = TestDb::default();
        let fixture = toc_vfs::generate_vfs(source).unwrap();
        db.insert_fixture(fixture);

        let root_file = RawPath::new(&db, "src/main.t".into());
        let package = SourcePackage::new(
            &db,
            "main".into(),
            root_file,
            toc_hir::package_graph::ArtifactKind::Binary,
            DependencyList::empty(&db),
        );
        RootPackages::new(&db, vec![package]);

        (db, package)
    }
}
