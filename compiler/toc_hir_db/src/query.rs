//! Query implementions

use toc_hir::library_graph::{GraphBuilder, LibraryGraph};
use toc_reporting::CompileResult;

use crate::HirDatabase;

pub fn hir_library_graph(db: &dyn HirDatabase) -> CompileResult<LibraryGraph> {
    let mut messages = vec![];
    let source_roots = db.source_roots();
    let mut graph = GraphBuilder::new();

    for root in source_roots.roots() {
        let res = db.lower_library(root);
        res.bundle_messages(&mut messages);
        graph.add_library(root, res.result().clone());
    }

    CompileResult::new(graph.finish(), messages)
}
