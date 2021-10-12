//! Analysis query implementation

use toc_reporting::{CompileResult, MessageBundle};

pub(crate) fn analyze_libraries(db: &dyn crate::db::HirAnalysis) -> CompileResult<()> {
    let mut messages = MessageBundle::default();

    let res = db.lower_library_graph();
    let lib_graph = res.result();
    res.bundle_messages(&mut messages);

    for (_, library) in lib_graph.library_roots() {
        db.typecheck_library(library).bundle_messages(&mut messages);
    }

    CompileResult::new((), messages)
}
