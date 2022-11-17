use std::{collections::HashSet, path::PathBuf, sync::Arc};

use toc_analysis::db::HirAnalysis;
use toc_ast_db::{
    db::{AstDatabaseExt, SourceParser},
    SourceGraph,
};
use toc_salsa::salsa;
use toc_vfs_db::db::VfsDatabaseExt;

// Workaround for the `afl` crate depending on `xdg`, which isn't compilable on platforms other than linux/unix
// Running cargo [build/run] [--release] isn't affected because this is never touched, but it still trips up
// rust-analyzer, resulting in errors outside of this repo
#[cfg(target_os = "linux")]
mod inner {

    pub(crate) fn do_fuzz() {
        afl::fuzz!(|data: &[u8]| {
            if let Ok(s) = std::str::from_utf8(data) {
                super::run(s);
            }
        });
    }
}

#[cfg(not(target_os = "linux"))]
mod inner {
    pub(crate) fn do_fuzz() {
        panic!("fuzzing is not supported on platforms other than linux");
    }
}

fn main() {
    inner::do_fuzz()
}

#[allow(dead_code)]
fn run(source: &str) {
    let mut db = FuzzDb::default();
    let fixture = match toc_vfs::generate_vfs(&mut db, source) {
        Ok(v) => v,
        Err(_) => return, // Don't care about invalid fixture files
    };
    let valid_files = fixture
        .files
        .iter()
        .map(|(id, _)| db.vfs.lookup_path(*id).to_owned())
        .collect::<HashSet<_>>();
    let file_loader = ValidFileLoader(valid_files);

    // Error out any escaped files so that we don't get false-positive crashes
    db.insert_fixture(fixture);
    db.invalidate_source_graph(&file_loader);

    let root_file = db.vfs.intern_path("src/main.t".into());
    let mut source_graph = SourceGraph::default();
    source_graph.add_library(toc_hir::library_graph::SourceLibrary {
        name: "main".into(),
        root: root_file,
        artifact: toc_hir::library_graph::ArtifactKind::Binary,
    });
    db.set_source_graph(Arc::new(source_graph));
    db.invalidate_source_graph(&toc_vfs::DummyFileLoader);

    // Run full analysis
    db.analyze_libraries();
}

#[salsa::database(
    toc_vfs_db::db::FileSystemStorage,
    toc_ast_db::db::SourceParserStorage,
    toc_hir_db::db::HirDatabaseStorage,
    toc_analysis::db::TypeInternStorage,
    toc_analysis::db::TypeDatabaseStorage,
    toc_analysis::db::ConstEvalStorage,
    toc_analysis::db::HirAnalysisStorage
)]
#[derive(Default)]
struct FuzzDb {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for FuzzDb {}

toc_vfs::impl_has_vfs!(FuzzDb, vfs);

#[derive(Default)]
struct ValidFileLoader(HashSet<PathBuf>);

impl toc_vfs::FileLoader for ValidFileLoader {
    fn load_file(&self, path: &std::path::Path) -> toc_vfs::LoadResult {
        if self.0.contains(path) {
            Ok(toc_vfs::LoadStatus::Unchanged)
        } else {
            Err(toc_vfs::LoadError::new(path, toc_vfs::ErrorKind::NotFound))
        }
    }

    fn normalize_path(&self, _path: &std::path::Path) -> Option<PathBuf> {
        None
    }
}
