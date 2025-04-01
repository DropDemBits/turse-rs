//! CTCE query definitions

use crate::db;

use super::{Const, ConstResult, ConstValue, EvalParams, query};

#[salsa::db]
pub trait ConstEval: db::TypeDatabase {
    /// Evaluates the given constant value
    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue>;
}

#[salsa::db]
impl<DB> ConstEval for DB
where
    DB: db::TypeDatabase,
{
    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue> {
        query::evaluate_const(self, expr, params)
    }
}
