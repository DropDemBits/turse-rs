//! Testing helpers

use camino::Utf8PathBuf;
use toc_hir::package_graph::SourcePackage;
use toc_paths::RawPath;
use toc_source_graph::{DependencyList, RootPackages};
use toc_vfs_db::{SourceTable, VfsBridge, VfsDbExt};

#[salsa::db]
#[derive(Default, Clone)]
pub(crate) struct TestDb {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl VfsBridge for TestDb {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }
}

#[salsa::db]
impl salsa::Database for TestDb {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

impl TestDb {
    pub(crate) fn from_source(source: &str) -> (Self, SourcePackage) {
        let mut db = TestDb::default();
        let fixture = toc_vfs::generate_vfs(source).unwrap();
        db.insert_fixture(fixture);

        let root_file = RawPath::new(&db, Utf8PathBuf::from("src/main.t"));
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

    pub(crate) fn span_to_location(&self, span: toc_span::Span) -> String {
        span.into_parts().map_or_else(
            || String::from("<unknown>:0..0"),
            |(file, range)| {
                let file = file.into_raw().raw_path(self).to_string();
                let (start, end) = (u32::from(range.start()), u32::from(range.end()));
                format!("{file}:{start}..{end}")
            },
        )
    }

    pub(crate) fn location_display(&self) -> impl toc_reporting::DisplayLocation<toc_span::Span> {
        toc_reporting::FnDisplay::new(|loc| self.span_to_location(*loc))
    }
}
