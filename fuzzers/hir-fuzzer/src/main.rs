use std::sync::Arc;

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

fn run(source: &str) {
    let mut db = FuzzDb::default();
    let fixture = match toc_vfs::generate_vfs(&mut db, source) {
        Ok(v) => v,
        Err(_) => return, // Don't care about invalid fixture files
    };
    db.insert_fixture(fixture);

    let root_file = db.vfs.intern_path("src/main.t".into());
    let mut source_graph = SourceGraph::default();
    source_graph.add_root(root_file);
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
