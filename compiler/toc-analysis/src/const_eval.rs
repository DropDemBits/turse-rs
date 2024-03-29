//! Compile-time constant evaluation

// ConstSources -> ... -> ConstError
// Any mismatched types should be double checked by typeck, where they'll actually be reported
//
// Possible const sources:
// HIR:
// - From imm value (e.g. for the type of char(n), we know n immediately)
// - From hir body (expr bodies + special identifiers only)
// Other:
// ? From constructed expressions (probably stored in a lowered format)
//
// For now, we'll reuse the hir tree as storing the const expr ops
// We can always split off interning if we decide if that's what we want

#[cfg(test)]
mod test;

pub(crate) mod db;
mod errors;
mod integer;
mod ops;
mod query;
mod value;

use toc_hir::{body::BodyId, expr::BodyExpr, package::PackageId};

pub(crate) use errors::ErrorKind;

pub use errors::{ConstError, NotConst};
pub use integer::ConstInt;
pub use value::ConstValue;

/// Compile-time constant value representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    /// Eagerly evaluated constant value
    Value(ConstValue),
    /// Unevaluated body
    Unevaluated(PackageId, BodyId),
    /// Unevaluated expression
    UnevaluatedExpr(PackageId, BodyExpr),
    /// Invalid constant value
    Error(ConstError),
}

impl Const {
    pub fn from_value(value: ConstValue) -> Self {
        Self::Value(value)
    }

    pub fn from_body(package: PackageId, body: BodyId) -> Self {
        Self::Unevaluated(package, body)
    }

    pub fn from_expr(package: PackageId, expr: BodyExpr) -> Self {
        Self::UnevaluatedExpr(package, expr)
    }
}

impl From<ConstResult<ConstInt>> for Const {
    fn from(v: ConstResult<ConstInt>) -> Self {
        match v {
            Ok(v) => Self::Value(ConstValue::Integer(v)),
            Err(err) => Self::Error(err),
        }
    }
}

impl From<ConstResult<ConstValue>> for Const {
    fn from(v: ConstResult<ConstValue>) -> Self {
        match v {
            Ok(v) => Self::Value(v),
            Err(err) => Self::Error(err),
        }
    }
}

/// A constant evaluation result, with the error containing a span associated with the error
pub type ConstResult<T> = Result<T, ConstError>;

/// Constant evaluation parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct EvalParams {
    /// Whether to allow 64-bit values to promote the size of other values
    pub allow_64bit_ops: bool,
}
