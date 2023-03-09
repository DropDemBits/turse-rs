//! Analysis query implementation

use toc_reporting::{CompileResult, MessageBundle};

#[salsa::tracked(jar = crate::db::AnalysisJar)]
pub(crate) fn analyze_libraries(db: &dyn crate::db::HirAnalysis) -> CompileResult<()> {
    let mut messages = MessageBundle::default();

    // FIXME: Report cyclic dep errors
    let source_graph = toc_source_graph::source_graph(db.up()).as_ref().unwrap();
    let res = toc_hir_lowering::lower_source_graph(db.up());
    res.bundle_messages(&mut messages);

    for &library in source_graph.all_libraries(db.up()) {
        db.typecheck_library(library).bundle_messages(&mut messages);
        db.lint_library(library).bundle_messages(&mut messages);
    }

    CompileResult::new((), messages)
}
