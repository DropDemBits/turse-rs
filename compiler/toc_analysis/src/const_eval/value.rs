//! Compile-time values

use crate::const_eval::{errors::ErrorKind, ConstError, ConstInt, RestrictType};

#[derive(Debug, Clone)]
pub enum ConstValue {
    /// General integer value
    Integer(ConstInt),
    /// Floating point value
    Real(f64),
    /// Boolean value
    Bool(bool),
}

impl ConstValue {
    /// Unwraps a `ConstValue` into the corresponding `ConstInt`
    ///
    /// ## Returns
    /// If `self` is a `ConstValue::Integer`, returns the corresponding ConstInt value.
    /// Otherwise, returns `ConstError::WrongType`.
    pub fn into_int(self) -> Result<ConstInt, ConstError> {
        match self {
            ConstValue::Integer(v) => Ok(v),
            _ => Err(ConstError::new(ErrorKind::WrongResultType(
                self,
                RestrictType::Integer,
            ))),
        }
    }

    /// Gets the human readable version of the value's type
    pub fn type_name(&self) -> &str {
        match self {
            ConstValue::Integer(_) => "integer value",
            ConstValue::Real(_) => "real value",
            ConstValue::Bool(_) => "boolean value",
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
            _ => Err(ConstError::new(ErrorKind::WrongOperandType)),
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
            _ => Err(ConstError::new(ErrorKind::WrongOperandType)),
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
            _ => Err(ConstError::new(ErrorKind::WrongOperandType)),
        }
    }
}
