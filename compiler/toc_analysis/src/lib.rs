//! General code analysis, including type checking and dead code reporting

use std::sync::Arc;

use toc_hir::{db, unit};
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

pub fn analyze_unit(hir_db: db::HirDb, unit_id: unit::UnitId) -> AnalyzeResult {
    let unit = hir_db.get_unit(unit_id);

    let const_eval_ctx = Arc::new(ConstEvalCtx::new(hir_db.clone()));
    const_eval::collect_const_vars(hir_db.clone(), unit, const_eval_ctx.clone());

    let (ty_ctx, messages) = typeck::typecheck_unit(hir_db.clone(), unit, const_eval_ctx.clone());

    eprintln!("{}", ty::pretty_dump_typectx(&ty_ctx));
    eprintln!("{:#?}", const_eval_ctx);

    AnalyzeResult { messages }
}
