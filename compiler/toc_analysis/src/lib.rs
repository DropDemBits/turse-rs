//! General code analysis, including type checking and dead code reporting

use std::sync::Arc;

use toc_hir::{db, unit};
use toc_reporting::CompileResult;

use crate::const_eval::ConstEvalCtx;
pub mod ty;

mod const_eval;
mod typeck;

pub fn analyze_unit(hir_db: db::HirDb, unit_id: unit::UnitId) -> CompileResult<()> {
    let unit = hir_db.get_unit(unit_id);

    let const_eval_ctx = Arc::new(ConstEvalCtx::new(hir_db.clone()));
    const_eval::collect_const_vars(hir_db.clone(), unit, const_eval_ctx.clone());

    let typecheck_res = typeck::typecheck_unit(hir_db.clone(), unit, const_eval_ctx.clone());

    eprintln!("{}", ty::pretty_dump_typectx(typecheck_res.result()));
    eprintln!("{:#?}", const_eval_ctx);

    let mut messages = vec![];
    typecheck_res.bundle_messages(&mut messages);

    CompileResult::new((), messages)
}
