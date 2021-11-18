//! Compile-time values

use std::sync::Arc;

use toc_span::Span;

use crate::const_eval::{errors::ErrorKind, ConstError, ConstInt};

/// A compile-time constant literal
///
/// # Note
/// `Eq` and `PartialEq` only satisfy bitwise structural equality,
/// which may be different from value equality of the type.
#[derive(Debug, Clone)]
pub enum ConstValue {
    /// General integer value
    Integer(ConstInt),
    /// Floating point value
    Real(f64),
    /// Boolean value
    Bool(bool),
    /// Character value
    Char(char),
    /// String value (wrapped in `Arc` to be cheaply cloneable)
    String(Arc<String>),
    /// CharN value (wrapped in `Arc` to be cheaply cloneable)
    CharN(Arc<String>),
}

impl std::hash::Hash for ConstValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Real(l0), Self::Real(r0)) => l0.to_bits() == r0.to_bits(),
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::CharN(l0), Self::CharN(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for ConstValue {}

impl ConstValue {
    /// Unwraps a `ConstValue` into the corresponding `ConstInt`.
    ///
    /// The span provided is for reporting conversion errors
    ///
    /// # Returns
    /// If `self` is a `ConstValue::Integer`, returns the corresponding ConstInt value.
    /// Otherwise, returns `ConstError::WrongType`.
    pub fn into_int(self, span: Span) -> Result<ConstInt, ConstError> {
        match self {
            ConstValue::Integer(v) => Ok(v),
            _ => Err(ConstError::new(ErrorKind::WrongResultType, span)),
        }
    }

    /// Converts a `ConstValue` into a `ConstInt`.
    ///
    /// The only value types that are allowed to be cast into `ConstInt` are:
    ///
    /// - `Integer`
    pub(super) fn cast_into_int(self) -> Result<ConstInt, ConstError> {
        match self {
            ConstValue::Integer(v) => Ok(v),
            _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
        }
    }

    /// Converts a `ConstValue` into a `f64`.
    ///
    /// The only value types that are allowed to be cast into a `f64` are:
    ///
    /// - `Integer`
    /// - `Real`
    pub(super) fn cast_into_real(self) -> Result<f64, ConstError> {
        match self {
            ConstValue::Integer(v) => Ok(v.into_f64()),
            ConstValue::Real(v) => Ok(v),
            _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
        }
    }

    /// Converts a `ConstValue` into a `bool`.
    ///
    /// The only value types that are allowed to be cast into `bool` are:
    ///
    /// - `Bool`
    pub(super) fn cast_into_bool(self) -> Result<bool, ConstError> {
        match self {
            ConstValue::Bool(v) => Ok(v),
            _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
        }
    }
}
