//! Analysis query implementation

use toc_reporting::CompileResult;

pub(crate) fn analyze_libraries(db: &dyn crate::db::HirAnalysis) -> CompileResult<()> {
    let mut messages = vec![];

    let res = db.library_graph();
    let lib_graph = res.result();
    res.bundle_messages(&mut messages);

    for (_, library) in lib_graph.library_roots() {
        db.typecheck_library(library).bundle_messages(&mut messages);
    }

    CompileResult::new((), messages)
}
