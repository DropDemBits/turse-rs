use std::{collections::HashSet, path::PathBuf, sync::Arc};

use toc_analysis::db::HirAnalysis;
use toc_paths::RawPath;
use toc_source_graph::{DependencyList, RootLibraries};
use toc_vfs_db::{SourceTable, VfsBridge, VfsDbExt};

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
    let fixture = match toc_vfs::generate_vfs(source) {
        Ok(v) => v,
        Err(_) => return, // Don't care about invalid fixture files
    };
    let valid_files = fixture
        .files
        .iter()
        .map(|(path, _)| path.clone())
        .collect::<HashSet<_>>();

    // Error out any escaped files so that we don't get false-positive crashes
    db.insert_fixture(fixture);

    let root_file = RawPath::new(&db, "src/main.t".into());
    let root_library = toc_hir::library_graph::SourceLibrary::new(
        &db,
        "main".into(),
        root_file,
        toc_hir::library_graph::ArtifactKind::Binary,
        DependencyList::empty(&db),
    );
    RootLibraries::new(&db, vec![root_library]);

    // Run full analysis
    db.analyze_libraries();
}

#[salsa::db(
    toc_paths::Jar,
    toc_vfs_db::Jar,
    toc_source_graph::Jar,
    toc_ast_db::Jar,
    toc_hir_lowering::Jar,
    toc_hir_db::Jar,
    toc_analysis::TypeJar,
    toc_analysis::ConstEvalJar,
    toc_analysis::AnalysisJar
)]
#[derive(Default)]
struct FuzzDb {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl salsa::Database for FuzzDb {}

impl VfsBridge for FuzzDb {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }
}

// #[derive(Default)]
// struct ValidFileLoader(HashSet<PathBuf>);

// impl toc_vfs::FileLoader for ValidFileLoader {
//     fn load_file(&self, path: &std::path::Path) -> toc_vfs::LoadResult {
//         if self.0.contains(path) {
//             Ok(toc_vfs::LoadStatus::Unchanged)
//         } else {
//             Err(toc_vfs::LoadError::new(path, toc_vfs::ErrorKind::NotFound))
//         }
//     }

//     fn normalize_path(&self, _path: &std::path::Path) -> Option<PathBuf> {
//         None
//     }
// }
