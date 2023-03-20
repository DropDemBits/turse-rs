//! Compile-time values

use std::sync::Arc;

use toc_span::Span;

use crate::ty;
use crate::{
    const_eval::{errors::ErrorKind, ConstError, ConstInt},
    ty::db,
};

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
    /// Enum variant (with type + ordinal)
    EnumVariant(ty::TypeId, usize),
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
    /// Formats the constant value for display.
    pub fn display(&self, db: &dyn db::TypeDatabase) -> String {
        match self {
            ConstValue::Integer(v) => format!("{v}"),
            ConstValue::Real(v) => format!("{v}"),
            ConstValue::Bool(v) => format!("{v}"),
            ConstValue::Char(v) => format!("'{v}'"),
            ConstValue::String(v) => format!(r#""{v}""#),
            ConstValue::CharN(v) => format!(r#"'{v}'"#),
            ConstValue::EnumVariant(ty_ref, ord) => {
                let ty::TypeKind::Enum(with_def, variants) = ty_ref.kind(db) else {
                    unreachable!("not enum ty for `EnumVariant` ty");
                };
                let def_id = with_def.def_id();
                let variant_def = variants.get(*ord).expect("bad ordinal for `EnumVariant`");

                let package = db.package(def_id.package());
                let ty_name = package.local_def(def_id.local()).name;
                let variant_name = package.local_def(variant_def.local()).name;

                format!("{ty_name}.{variant_name}")
            }
        }
    }

    /// Gets the corresponding ordinal value of this [`ConstValue`], or [`None`] if it
    /// isn't applicable
    pub fn ordinal(&self) -> Option<ConstInt> {
        match self {
            // Identity transform
            ConstValue::Integer(v) => Some(*v),
            // Simple mapping to 0 and 1
            ConstValue::Bool(v) => ConstInt::from_unsigned(u64::from(*v), false).ok(),
            // Just the codepoint
            ConstValue::Char(v) => ConstInt::from_unsigned((*v).into(), false).ok(),
            // From the ord value inside
            ConstValue::EnumVariant(_, ord) => {
                ConstInt::from_unsigned((*ord).try_into().ok()?, false).ok()
            }

            // Only applicable to values of length 1
            // Corresponds to the codepoint value of the first character
            ConstValue::String(v) | ConstValue::CharN(v) if v.len() == 1 => {
                ConstInt::from_unsigned(v.chars().next()?.into(), false).ok()
            }

            // The rest don't have a corresponding ordinal integer value
            _ => None,
        }
    }

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

    /// Converts a `ConstValue` into a `char`.
    ///
    /// The only value types that are allowed to be cast into `bool` are:
    ///
    /// - `char`
    /// - `char(N)` where N = 1
    /// - `string(N)` where N = 1
    pub(super) fn cast_into_char(self) -> Result<char, ConstError> {
        match self {
            ConstValue::Char(v) => Ok(v),
            ConstValue::String(v) | ConstValue::CharN(v) => {
                let mut chars = v.chars();

                // Only one character is allowed in the string
                chars
                    .next()
                    .filter(|_| chars.next().is_none())
                    .ok_or_else(|| (ConstError::without_span(ErrorKind::WrongOperandType)))
            }
            _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
        }
    }
}
