//! General code analysis, including type checking and dead code reporting

use toc_reporting::ReportMessage;
pub mod ty;

mod typeck;

pub struct AnalyzeResult {
    messages: Vec<ReportMessage>,
}

impl AnalyzeResult {
    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }
}

pub fn analyze_unit(unit: &toc_hir::Unit) -> AnalyzeResult {
    let (ty_ctx, messages) = typeck::typecheck_unit(&unit);

    println!("{}", ty::pretty_dump_typectx(&ty_ctx));

    AnalyzeResult { messages }
}
