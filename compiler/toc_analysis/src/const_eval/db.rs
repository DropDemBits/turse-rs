//! CTCE query definitions

use crate::db;

use super::{query, Const, ConstResult, ConstValue, EvalParams};

#[salsa::jar(db = ConstEval)]
pub struct ConstEvalJar();

pub trait ConstEval: salsa::DbWithJar<ConstEvalJar> + db::TypeDatabase {
    fn upcast_to_const_eval_db(&self) -> &dyn ConstEval;

    /// Evaluates the given constant value
    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue>;
}

impl<DB> ConstEval for DB
where
    DB: salsa::DbWithJar<ConstEvalJar> + db::TypeDatabase,
{
    fn upcast_to_const_eval_db(&self) -> &dyn ConstEval {
        self
    }

    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue> {
        query::evaluate_const(self, expr, params)
    }
}
