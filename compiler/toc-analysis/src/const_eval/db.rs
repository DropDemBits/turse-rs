//! CTCE query definitions

use upcast::{Upcast, UpcastFrom};

use crate::db;

use super::{query, Const, ConstResult, ConstValue, EvalParams};

#[salsa::jar(db = ConstEval)]
pub struct ConstEvalJar();

pub trait ConstEval:
    salsa::DbWithJar<ConstEvalJar> + db::TypeDatabase + Upcast<dyn db::TypeDatabase>
{
    /// Evaluates the given constant value
    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue>;
}

impl<DB> ConstEval for DB
where
    DB: salsa::DbWithJar<ConstEvalJar> + db::TypeDatabase + Upcast<dyn db::TypeDatabase>,
{
    fn evaluate_const(&self, expr: Const, params: EvalParams) -> ConstResult<ConstValue> {
        query::evaluate_const(self, expr, params)
    }
}

impl<'db, DB: ConstEval + 'db> UpcastFrom<DB> for dyn ConstEval + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}
