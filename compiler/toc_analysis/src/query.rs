//! Analysis query implementation

use toc_reporting::{CompileResult, MessageBundle};

#[salsa::tracked(jar = crate::db::AnalysisJar)]
pub(crate) fn analyze_libraries(db: &dyn crate::db::HirAnalysis) -> CompileResult<()> {
    let mut messages = MessageBundle::default();

    // FIXME: Report cyclic dep errors
    let source_graph = toc_source_graph::source_graph(db.upcast_to_source_graph_db())
        .as_ref()
        .unwrap();
    let res = toc_hir_lowering::lower_source_graph(db.upcast_to_lowering_db());
    res.bundle_messages(&mut messages);

    for &library in source_graph.all_libraries(db.upcast_to_source_graph_db()) {
        db.typecheck_library(library).bundle_messages(&mut messages);
        db.lint_library(library).bundle_messages(&mut messages);
    }

    CompileResult::new((), messages)
}
