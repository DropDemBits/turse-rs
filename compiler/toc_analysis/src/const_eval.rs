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

use toc_hir::{body::BodyId, expr::BodyExpr, library::LibraryId};

pub(crate) use errors::ErrorKind;

pub use errors::ConstError;
pub use integer::ConstInt;
pub use value::ConstValue;

/// Compile-time constant value representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    /// Eagerly evaluated constant value
    Value(ConstValue),
    /// Unevaluated body
    Unevaluated(LibraryId, BodyId),
    /// Unevaluated expression
    UnevaluatedExpr(LibraryId, BodyExpr),
    /// Invalid constant value
    Error(ConstError),
}

impl Const {
    pub fn from_value(value: ConstValue) -> Self {
        Self::Value(value)
    }

    pub fn from_body(library: LibraryId, body: BodyId) -> Self {
        Self::Unevaluated(library, body)
    }

    pub fn from_expr(library: LibraryId, expr: BodyExpr) -> Self {
        Self::UnevaluatedExpr(library, expr)
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
