//! CTCE query definitions

use toc_salsa::salsa;

use crate::db;

use super::{query, Const, ConstResult, ConstValue, EvalParams};

#[salsa::query_group(ConstEvalStorage)]
pub trait ConstEval: db::TypeDatabase {
    /// Evaluates the given constant value
    #[salsa::invoke(query::evaluate_const)]
    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue>;
}
