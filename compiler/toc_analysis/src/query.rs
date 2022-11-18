//! Analysis query implementation

use toc_reporting::{CompileResult, MessageBundle};

pub(crate) fn analyze_libraries(db: &dyn crate::db::HirAnalysis) -> CompileResult<()> {
    let mut messages = MessageBundle::default();

    let source_graph = db.source_graph();
    let res = db.lower_source_graph();
    res.bundle_messages(&mut messages);

    for (id, _) in source_graph.all_libraries() {
        db.typecheck_library(id).bundle_messages(&mut messages);
        db.lint_library(id).bundle_messages(&mut messages);
    }

    CompileResult::new((), messages)
}
