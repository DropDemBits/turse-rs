//! General code analysis, including type checking and dead code reporting

use std::sync::Arc;

use toc_hir::UnitMap;
use toc_reporting::ReportMessage;

use crate::const_eval::ConstEvalCtx;
pub mod ty;

mod const_eval;
mod typeck;

pub struct AnalyzeResult {
    messages: Vec<ReportMessage>,
}

impl AnalyzeResult {
    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }
}

pub fn analyze_unit(unit_id: toc_hir::UnitId, unit_map: Arc<UnitMap>) -> AnalyzeResult {
    let unit = unit_map.get_unit(unit_id);

    let const_eval_ctx = Arc::new(ConstEvalCtx::new(unit_map.clone()));
    const_eval::collect_const_vars(unit, const_eval_ctx.clone());

    let (ty_ctx, messages) = typeck::typecheck_unit(unit, const_eval_ctx.clone());

    println!("{}", ty::pretty_dump_typectx(&ty_ctx));
    println!("{:#?}", const_eval_ctx);

    AnalyzeResult { messages }
}
