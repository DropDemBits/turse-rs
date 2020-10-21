//! Intermediate values for compile-time evaluation
use crate::ast::{BinaryOp, Expr, ExprKind, Literal, UnaryOp};
use crate::types::{self, PrimitiveType, SequenceSize, Type, TypeRef, TypeTable};
use toc_core::Location;

use std::cmp::Ordering;
use std::convert::TryFrom;
use std::fmt;

/// Errors returned from value folding
#[derive(Debug, PartialEq)]
pub enum ValueApplyError {
    Overflow,
    DivisionByZero,
    InvalidOperand,
    WrongTypes,
}

impl std::error::Error for ValueApplyError {}

impl fmt::Display for ValueApplyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueApplyError::Overflow => f.write_str("Overflow in compile-time expression"),
            ValueApplyError::DivisionByZero => {
                f.write_str("Compile-time 'div', 'mod', or 'rem' by zero")
            }
            ValueApplyError::InvalidOperand => {
                f.write_str("Invalid operand in compile-time expression")
            }
            ValueApplyError::WrongTypes => {
                f.write_str("Mismatched types in compile-time expression")
            }
        }
    }
}

/// Constraints for evaluation operations
pub struct EvalConstraints {
    /// Whether to perform operations at the 64-bit width for integer types
    pub as_64_bit: bool,
}

/// A single compile-time value
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// A string value. Both char(n) and string(n) are represented by this value
    StringValue(String),
    /// An int value.
    IntValue(i64),
    /// A nat value.
    NatValue(u64),
    /// A real value
    RealValue(f64),
    /// A boolean value
    BooleanValue(bool),
    /// An enum field value.
    /// The first `usize` is a reference to the original enum field type id.
    /// The second `usize` is a reference to the parent enum type id.
    /// The third `usize` is the ordinal value of the field.
    EnumValue(usize, usize, usize),
}

impl Value {
    /// Produces a value from an expression.
    pub fn from_expr(expr: Expr, type_table: &TypeTable) -> Result<Value, &'static str> {
        let eval_type = expr.get_eval_type();

        match expr.kind {
            ExprKind::Dot { .. } | ExprKind::Literal { .. }
                if types::is_enum_type(&eval_type, type_table) =>
            {
                if let Some(Type::EnumField { ordinal, enum_type }) =
                    type_table.type_from_ref(&eval_type)
                {
                    // Infaillable, must both be TypeRef's
                    let field_id = types::get_type_id(&eval_type)
                        .expect("Infaillable conversion from TypeRef to TypeId");
                    let enum_id = types::get_type_id(enum_type)
                        .expect("Infaillable conversion from TypeRef to TypeId");

                    Ok(Value::EnumValue(field_id, enum_id, *ordinal))
                } else {
                    Err("Cannot convert non-literal expression into a value")
                }
            }
            ExprKind::Literal { value } => Value::from_literal(value),
            _ => Err("Cannot convert non-literal expression into a value"),
        }
    }

    /// Converts a Literal into a Value
    pub fn from_literal(literal: Literal) -> Result<Value, &'static str> {
        match literal {
            Literal::Bool(v) => Ok(Value::BooleanValue(v)),
            Literal::Int(v) => Ok(Value::IntValue(v)),
            Literal::Nat(v) => Ok(Value::NatValue(v)),
            Literal::Real(v) => Ok(Value::RealValue(v)),
            Literal::StrSequence(v) => Ok(Value::StringValue(v)),
            Literal::CharSequence(v) => Ok(Value::StringValue(v)),
            _ => Err("Cannot convert complex literal into a value"),
        }
    }
}

impl TryFrom<Expr> for Value {
    type Error = &'static str;

    fn try_from(expr: Expr) -> Result<Value, Self::Error> {
        match expr.kind {
            ExprKind::Literal { value, .. } => Value::from_literal(value),
            _ => Err("Cannot convert non-literal expression into a value"),
        }
    }
}

impl TryFrom<Value> for Expr {
    type Error = &'static str;

    fn try_from(v: Value) -> Result<Expr, Self::Error> {
        use super::value;

        match v {
            Value::BooleanValue(v) => Ok(value::make_literal(
                Literal::Bool(v),
                TypeRef::Primitive(PrimitiveType::Boolean),
            )),
            Value::IntValue(v) => Ok(value::make_literal(
                Literal::Int(v),
                TypeRef::Primitive(types::get_int_kind(v)),
                //TypeRef::Primitive(PrimitiveType::IntNat),
            )),
            Value::NatValue(v) => Ok(value::make_literal(
                Literal::Nat(v),
                TypeRef::Primitive(types::get_intnat_kind(v)),
                //TypeRef::Primitive(PrimitiveType::IntNat),
            )),
            Value::RealValue(v) => Ok(value::make_literal(
                Literal::Real(v),
                TypeRef::Primitive(PrimitiveType::Real),
            )),
            Value::StringValue(v) => {
                // Convert into a string(n) to preserve char(n) assignment semantics
                let size = v.bytes().len();

                Ok(value::make_literal(
                    Literal::StrSequence(v),
                    TypeRef::Primitive(PrimitiveType::StringN(SequenceSize::Size(size))),
                ))
            }
            Value::EnumValue(field_id, _, ordinal) => {
                // While we do lose *some* type information, it can be inferred back from the eval_type
                Ok(value::make_literal(
                    Literal::Nat(ordinal as u64),
                    TypeRef::Named(field_id),
                ))
            }
        }
    }
}

// 'From's

impl From<bool> for Value {
    fn from(v: bool) -> Value {
        Value::BooleanValue(v)
    }
}

impl From<String> for Value {
    fn from(v: String) -> Value {
        Value::StringValue(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Value {
        Value::StringValue(String::from(v))
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Value {
        Value::IntValue(v)
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Value {
        Value::NatValue(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Value {
        Value::RealValue(v)
    }
}

// 'Into's
impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::BooleanValue(v) => v,
            _ => panic!("Can't convert into bool"),
        }
    }
}

impl Into<String> for Value {
    fn into(self) -> String {
        match self {
            Value::StringValue(v) => v,
            _ => panic!("Can't convert into String"),
        }
    }
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::IntValue(v) => v,
            _ => panic!("Can't convert into i64"),
        }
    }
}

impl Into<u64> for Value {
    fn into(self) -> u64 {
        match self {
            Value::NatValue(v) => v,
            _ => panic!("Can't convert into u64"),
        }
    }
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::RealValue(v) => v,
            _ => panic!("Can't convert into f64"),
        }
    }
}

// Type checks

pub fn is_boolean(value: &Value) -> bool {
    matches!(value, Value::BooleanValue(_))
}

pub fn is_string(value: &Value) -> bool {
    matches!(value, Value::StringValue(_))
}

pub fn is_int(value: &Value) -> bool {
    matches!(value, Value::IntValue(_))
}

pub fn is_nat(value: &Value) -> bool {
    matches!(value, Value::NatValue(_))
}

pub fn is_integer_value(value: &Value) -> bool {
    matches!(value, Value::IntValue(_) | Value::NatValue(_))
}

pub fn is_real(value: &Value) -> bool {
    matches!(value, Value::RealValue(_))
}

pub fn is_number_value(value: &Value) -> bool {
    matches!(value, Value::IntValue(_) | Value::NatValue(_) | Value::RealValue(_))
}

pub fn is_enum(value: &Value) -> bool {
    matches!(value, Value::EnumValue(..))
}

fn make_literal(value: Literal, eval_type: TypeRef) -> Expr {
    Expr {
        kind: ExprKind::Literal { value },
        is_compile_eval: true,
        eval_type,
        span: Location::new(),
    }
}

/// Forcefully converts a value into an i64, performing the neccessary casts
pub fn value_as_i64(v: Value) -> Result<i64, ValueApplyError> {
    match v {
        Value::NatValue(v) => i64::try_from(v).map_err(|_| ValueApplyError::Overflow),
        Value::IntValue(v) => Ok(v),
        Value::RealValue(v) => Ok(v as i64),
        Value::BooleanValue(v) => Ok(v as i64),
        Value::StringValue(v) => {
            // Only convert the first char
            v.chars().next().map(|c| Ok(c as i64)).unwrap_or(Ok(0))
        }
        Value::EnumValue(_, _, ordinal) => Ok(ordinal as i64),
    }
}

/// Tries to convert the value into the corresponding u64 byte representation.
/// Only used for bitwise operators
fn value_into_u64_bytes(v: Value) -> Result<u64, ValueApplyError> {
    match v {
        Value::NatValue(v) => Ok(v),
        Value::IntValue(v) => {
            // Into u64
            Ok(u64::from_ne_bytes(v.to_ne_bytes()))
        }
        _ => panic!("Tried to convert {:?} into a u64", v),
    }
}

/// Tries to convert the value into an i64. Consumes the value.
fn value_into_i64(v: Value) -> Result<i64, ValueApplyError> {
    match v {
        Value::NatValue(v) => i64::try_from(v).map_err(|_| ValueApplyError::Overflow),
        Value::IntValue(v) => Ok(v),
        Value::EnumValue(_, _, v) => i64::try_from(v).map_err(|_| ValueApplyError::Overflow),
        _ => panic!("Tried to convert {:?} into an i64", v),
    }
}

/// Tries to convert the value into an f64. Consumes the value.
fn value_into_f64(v: Value) -> Result<f64, ValueApplyError> {
    match v {
        Value::NatValue(v) => Ok(v as f64),
        Value::IntValue(v) => Ok(v as f64),
        Value::RealValue(v) => Ok(v),
        _ => panic!("Tried to convert {:?} into an f64", v),
    }
}

fn compare_values(
    lhs: Value,
    rhs: Value,
    equality_only: bool,
) -> Result<Ordering, ValueApplyError> {
    if is_boolean(&lhs) && is_boolean(&rhs) {
        let lvalue: bool = lhs.into();
        let rvalue: bool = rhs.into();

        if equality_only {
            lvalue
                .partial_cmp(&rvalue)
                .ok_or(ValueApplyError::InvalidOperand)
        } else {
            // Cannot perform ordering comparisons on boolean values
            Err(ValueApplyError::WrongTypes)
        }
    } else if is_string(&lhs) && is_string(&rhs) {
        let lvalue: String = lhs.into();
        let rvalue: String = rhs.into();

        lvalue
            .partial_cmp(&rvalue)
            .ok_or(ValueApplyError::InvalidOperand)
    } else if is_nat(&lhs) && is_nat(&rhs) {
        let lvalue: u64 = lhs.into();
        let rvalue: u64 = rhs.into();

        lvalue
            .partial_cmp(&rvalue)
            .ok_or(ValueApplyError::InvalidOperand)
    } else if (is_integer_value(&lhs) && is_integer_value(&rhs)) || (is_enum(&lhs) && is_enum(&rhs))
    {
        let lvalue: i64 = value_into_i64(lhs)?;
        let rvalue: i64 = value_into_i64(rhs)?;

        lvalue
            .partial_cmp(&rvalue)
            .ok_or(ValueApplyError::InvalidOperand)
    } else if is_number_value(&lhs) && is_number_value(&rhs) {
        let lvalue: f64 = value_into_f64(lhs)?;
        let rvalue: f64 = value_into_f64(rhs)?;

        // Floating point equality in Turing is also a simple
        // bit-for-bit test
        lvalue
            .partial_cmp(&rvalue)
            .ok_or(ValueApplyError::InvalidOperand)
    } else {
        Err(ValueApplyError::WrongTypes)
    }
}

fn apply_binary_integer<N, I>(
    lhs: Value,
    rhs: Value,
    nat_apply: N,
    int_apply: I,
) -> Result<Value, ValueApplyError>
where
    N: FnOnce(u64, u64) -> Result<Value, ValueApplyError>,
    I: FnOnce(i64, i64) -> Result<Value, ValueApplyError>,
{
    if is_nat(&lhs) && is_nat(&rhs) {
        let lvalue: u64 = lhs.into();
        let rvalue: u64 = rhs.into();

        Ok(nat_apply(lvalue, rvalue)?)
    } else if is_integer_value(&lhs) && is_integer_value(&rhs) {
        let lvalue = value_into_i64(lhs)?;
        let rvalue = value_into_i64(rhs)?;

        Ok(int_apply(lvalue, rvalue)?)
    } else {
        Err(ValueApplyError::WrongTypes)
    }
}

fn apply_binary_number<N, I, R>(
    lhs: Value,
    rhs: Value,
    nat_apply: N,
    int_apply: I,
    real_apply: R,
) -> Result<Value, ValueApplyError>
where
    N: FnOnce(u64, u64) -> Result<Value, ValueApplyError>,
    I: FnOnce(i64, i64) -> Result<Value, ValueApplyError>,
    R: FnOnce(f64, f64) -> Result<Value, ValueApplyError>,
{
    if is_nat(&lhs) && is_nat(&rhs) {
        let lvalue: u64 = lhs.into();
        let rvalue: u64 = rhs.into();

        Ok(nat_apply(lvalue, rvalue)?)
    } else if is_integer_value(&lhs) && is_integer_value(&rhs) {
        let lvalue = value_into_i64(lhs)?;
        let rvalue = value_into_i64(rhs)?;

        Ok(int_apply(lvalue, rvalue)?)
    } else if is_number_value(&lhs) && is_number_value(&rhs) {
        let lvalue = value_into_f64(lhs)?;
        let rvalue = value_into_f64(rhs)?;

        Ok(real_apply(lvalue, rvalue)?)
    } else {
        Err(ValueApplyError::WrongTypes)
    }
}

/// Performs floored modular arithmetic.
/// Computes `lhs - rhs * floor(lhs / rhs)`,
/// returning `ValueApplyError::DivisionByZero` if `rhs <= f64::EPSILON`
/// or `ValueApplyError::Overflow` if the result overflows.
fn mod_floor(lhs: f64, rhs: f64) -> Result<f64, ValueApplyError> {
    let result = lhs - rhs * f64::floor(lhs / rhs);

    if rhs.abs() <= f64::EPSILON {
        Err(ValueApplyError::DivisionByZero)
    } else if result.is_infinite() {
        Err(ValueApplyError::Overflow)
    } else {
        Ok(result)
    }
}

/// Checks if the given value is infinite.
/// Produces the value if there is no error, otherwise produces an Overflow error.
fn check_inf(v: f64) -> Result<f64, ValueApplyError> {
    if v.is_infinite() {
        Err(ValueApplyError::Overflow)
    } else {
        Ok(v)
    }
}

/// Applies the specified binary operation on the given value operands
///
/// `constraints` modifies how the binary operation is applied.
pub fn apply_binary(
    lhs: Value,
    op: BinaryOp,
    rhs: Value,
    constraints: EvalConstraints,
) -> Result<Value, ValueApplyError> {
    // constraints.as_64_bit affects the bit shift manipulation operators specifically, since they are size-sensitive
    // All values are sign-extended to 64-bits otherwise
    match op {
        BinaryOp::Add => {
            if is_string(&lhs) && is_string(&rhs) {
                let mut lvalue: String = lhs.into();
                let rvalue: String = rhs.into();
                lvalue.push_str(&rvalue);

                Ok(Value::from(lvalue))
            } else {
                apply_binary_number(
                    lhs,
                    rhs,
                    |l, r| {
                        Ok(Value::from(
                            l.checked_add(r).ok_or(ValueApplyError::Overflow)?,
                        ))
                    },
                    |l, r| {
                        Ok(Value::from(
                            l.checked_add(r).ok_or(ValueApplyError::Overflow)?,
                        ))
                    },
                    |l, r| Ok(Value::from(check_inf(l + r)?)),
                )
            }
        }
        BinaryOp::Sub => apply_binary_number(
            lhs,
            rhs,
            |l, r| {
                if l < r {
                    // Convert into an integer value
                    Ok(Value::from(
                        (l as i64)
                            .checked_sub(r as i64)
                            .ok_or(ValueApplyError::Overflow)?,
                    ))
                } else {
                    Ok(Value::from(
                        l.checked_sub(r).ok_or(ValueApplyError::Overflow)?,
                    ))
                }
            },
            |l, r| {
                Ok(Value::from(
                    l.checked_sub(r).ok_or(ValueApplyError::Overflow)?,
                ))
            },
            |l, r| Ok(Value::from(check_inf(l - r)?)),
        ),
        BinaryOp::Mul => apply_binary_number(
            lhs,
            rhs,
            |l, r| {
                Ok(Value::from(
                    l.checked_mul(r).ok_or(ValueApplyError::Overflow)?,
                ))
            },
            |l, r| {
                Ok(Value::from(
                    l.checked_mul(r).ok_or(ValueApplyError::Overflow)?,
                ))
            },
            |l, r| Ok(Value::from(check_inf(l * r)?)),
        ),
        BinaryOp::RealDiv => {
            // Real division
            if is_number_value(&lhs) && is_number_value(&rhs) {
                let lvalue = value_into_f64(lhs)?;
                let rvalue = value_into_f64(rhs)?;
                let res = lvalue / rvalue;

                if rvalue.abs() <= f64::EPSILON {
                    Err(ValueApplyError::DivisionByZero)
                } else {
                    Ok(Value::from(check_inf(res)?))
                }
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Div => {
            // Integer division
            if is_integer_value(&lhs) && is_integer_value(&rhs) {
                apply_binary_integer(
                    lhs,
                    rhs,
                    |l, r| {
                        Ok(Value::from(
                            l.checked_div(r)
                                .ok_or(ValueApplyError::DivisionByZero)
                                .map_err(|e| if r != 0 { ValueApplyError::Overflow } else { e })?,
                        ))
                    },
                    |l, r| {
                        Ok(Value::from(
                            l.checked_div(r)
                                .ok_or(ValueApplyError::DivisionByZero)
                                .map_err(|e| if r != 0 { ValueApplyError::Overflow } else { e })?,
                        ))
                    },
                )
            } else if is_number_value(&lhs) && is_number_value(&rhs) {
                let lvalue = value_into_f64(lhs)?;
                let rvalue = value_into_f64(rhs)?;
                let res = lvalue / rvalue;

                if rvalue.abs() <= f64::EPSILON {
                    Err(ValueApplyError::DivisionByZero)
                } else if res.is_infinite() || res > i64::MAX as f64 {
                    Err(ValueApplyError::Overflow)
                } else {
                    Ok(Value::from(res as i64))
                }
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Mod => apply_binary_number(
            lhs,
            rhs,
            |l, r| {
                Ok(Value::from(
                    mod_floor(l as f64, r as f64).map(|f| f as u64)?,
                ))
            },
            |l, r| {
                Ok(Value::from(
                    mod_floor(l as f64, r as f64).map(|f| f as i64)?,
                ))
            },
            |l, r| Ok(Value::from(mod_floor(l as f64, r as f64)?)),
        ),
        BinaryOp::Rem => apply_binary_number(
            lhs,
            rhs,
            |l, r| {
                Ok(Value::from(
                    l.checked_rem(r)
                        .ok_or(ValueApplyError::DivisionByZero)
                        .map_err(|e| if r != 0 { ValueApplyError::Overflow } else { e })?,
                ))
            },
            |l, r| {
                Ok(Value::from(
                    l.checked_rem(r)
                        .ok_or(ValueApplyError::DivisionByZero)
                        .map_err(|e| if r != 0 { ValueApplyError::Overflow } else { e })?,
                ))
            },
            |l, r| {
                let res = l % r;

                if r.abs() <= f64::EPSILON {
                    Err(ValueApplyError::DivisionByZero)
                } else {
                    Ok(Value::from(check_inf(res)?))
                }
            },
        ),
        BinaryOp::Exp => apply_binary_number(
            lhs,
            rhs,
            |l, r| {
                if r >= (u32::MAX as u64) {
                    Err(ValueApplyError::Overflow)
                } else {
                    Ok(Value::from(
                        l.checked_pow(r as u32).ok_or(ValueApplyError::Overflow)?,
                    ))
                }
            },
            |l, r| {
                if r < 0 {
                    Err(ValueApplyError::InvalidOperand)
                } else if r >= (u32::MAX as i64) {
                    Err(ValueApplyError::Overflow)
                } else {
                    Ok(Value::from(
                        l.checked_pow(r as u32).ok_or(ValueApplyError::Overflow)?,
                    ))
                }
            },
            |l, r| Ok(Value::from(check_inf(l.powf(r))?)),
        ),
        BinaryOp::And => {
            if is_integer_value(&lhs) && is_integer_value(&rhs) {
                // Bitwise And
                let lvalue = value_into_u64_bytes(lhs)?;
                let rvalue = value_into_u64_bytes(rhs)?;

                Ok(Value::from(lvalue & rvalue))
            } else if is_boolean(&lhs) && is_boolean(&rhs) {
                // Boolean And
                let lvalue: bool = lhs.into();
                let rvalue: bool = rhs.into();

                Ok(Value::from(lvalue && rvalue))
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Or => {
            if is_integer_value(&lhs) && is_integer_value(&rhs) {
                // Bitwise Or
                let lvalue = value_into_u64_bytes(lhs)?;
                let rvalue = value_into_u64_bytes(rhs)?;

                Ok(Value::from(lvalue | rvalue))
            } else if is_boolean(&lhs) && is_boolean(&rhs) {
                // Boolean Or
                let lvalue: bool = lhs.into();
                let rvalue: bool = rhs.into();

                Ok(Value::from(lvalue || rvalue))
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Xor => {
            if is_integer_value(&lhs) && is_integer_value(&rhs) {
                // Bitwise Xor
                let lvalue = value_into_u64_bytes(lhs)?;
                let rvalue = value_into_u64_bytes(rhs)?;

                Ok(Value::from(lvalue ^ rvalue))
            } else if is_boolean(&lhs) && is_boolean(&rhs) {
                // Boolean Xor
                let lvalue: bool = lhs.into();
                let rvalue: bool = rhs.into();

                Ok(Value::from(lvalue ^ rvalue))
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Shl => {
            // Bitshift left
            if is_integer_value(&lhs) && is_integer_value(&rhs) {
                let lvalue = value_into_u64_bytes(lhs)?;
                let rvalue = value_into_i64(rhs)?;

                if rvalue < 0 {
                    Err(ValueApplyError::InvalidOperand)
                } else {
                    let result = if constraints.as_64_bit {
                        // Mask into the 0 - 63 range for 64-bit integers
                        lvalue.overflowing_shl((rvalue as u32) & 0x3F).0 as u64
                    } else {
                        // Mask into the 0 - 31 range for 64-bit integers, and truncate to 32 bits
                        (lvalue.overflowing_shl((rvalue as u32) & 0x1F).0 as u64) & 0xFFFFFFFF
                    };

                    Ok(Value::from(result))
                }
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Shr => {
            // Bitshift right
            if is_integer_value(&lhs) && is_integer_value(&rhs) {
                let lvalue = value_into_u64_bytes(lhs)?;
                let rvalue = value_into_i64(rhs)?;

                if rvalue < 0 {
                    Err(ValueApplyError::InvalidOperand)
                } else {
                    let result = if constraints.as_64_bit {
                        // Mask into the 0 - 63 range for 64-bit integers
                        lvalue.overflowing_shr((rvalue as u32) & 0x3F).0 as u64
                    } else {
                        // Truncate lvalue to 32 bits and mask shift into the 0 - 31 range for 64-bit integers
                        (lvalue as u32).overflowing_shr((rvalue as u32) & 0x1F).0 as u64
                    };

                    Ok(Value::from(result))
                }
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Imply => {
            if is_boolean(&lhs) && is_boolean(&rhs) {
                // Boolean Imply
                let lvalue: bool = lhs.into();
                let rvalue: bool = rhs.into();

                Ok(Value::from((!lvalue) || rvalue))
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        BinaryOp::Less => Ok(Value::from(matches!(
            compare_values(lhs, rhs, false)?,
            Ordering::Less
        ))),
        BinaryOp::LessEq => Ok(Value::from(matches!(
            compare_values(lhs, rhs, false)?,
            Ordering::Less | Ordering::Equal
        ))),
        BinaryOp::Greater => Ok(Value::from(matches!(
            compare_values(lhs, rhs, false)?,
            Ordering::Greater
        ))),
        BinaryOp::GreaterEq => Ok(Value::from(matches!(
            compare_values(lhs, rhs, false)?,
            Ordering::Greater | Ordering::Equal
        ))),
        BinaryOp::Equal => Ok(Value::from(matches!(
            compare_values(lhs, rhs, true)?,
            Ordering::Equal
        ))),
        BinaryOp::NotEqual => Ok(Value::from(!matches!(
            compare_values(lhs, rhs, true)?,
            Ordering::Equal
        ))),
        _ => unreachable!(),
    }
}

/// Applies the specifies unary operation on the given value operand
///
/// `constraints` modifies how the binary operation is applied. Currently,
/// evaluation constraints are not used for unary operations.
pub fn apply_unary(
    op: UnaryOp,
    rhs: Value,
    _constraints: EvalConstraints,
) -> Result<Value, ValueApplyError> {
    match op {
        UnaryOp::Not => match rhs {
            Value::IntValue(_) | Value::NatValue(_) => {
                // Bitwise Not
                Ok(Value::from(!value_into_u64_bytes(rhs)?))
            }
            Value::BooleanValue(v) => {
                // Logical Not
                Ok(Value::from(!v))
            }
            _ => Err(ValueApplyError::WrongTypes),
        },
        UnaryOp::NatCheat => match rhs {
            // Coercion into Nat8
            Value::IntValue(_) | Value::NatValue(_) => Ok(Value::from(value_into_u64_bytes(rhs)?)),
            Value::RealValue(v) => {
                // Real value is first coerced into ne bytes, then swaps the u32 halves
                Ok(Value::from(
                    u64::from_ne_bytes(v.to_ne_bytes()).rotate_left(32),
                ))
            }
            Value::BooleanValue(v) => Ok(Value::from(v as u64)),
            Value::StringValue(v) => {
                // Pad & fold (big endian ordering) into a u64
                let value: u64 = v
                    .bytes()
                    .chain(std::iter::once(0).cycle())
                    .take(8)
                    .fold(0u64, |acc, v| ((acc << 8) | v as u64));

                // Produce the converted type (after swapping bytes)
                Ok(Value::from(value.swap_bytes()))
            }
            Value::EnumValue(_, _, ordinal) => Ok(Value::from(ordinal as u64)),
        },
        UnaryOp::Identity => {
            if is_number_value(&rhs) {
                // Return the same value
                Ok(rhs)
            } else {
                Err(ValueApplyError::WrongTypes)
            }
        }
        UnaryOp::Negate => match rhs {
            Value::NatValue(v) => {
                if v <= (i64::MAX as u64) {
                    Ok(Value::from(-(v as i64)))
                } else {
                    Err(ValueApplyError::Overflow)
                }
            }
            Value::IntValue(v) => {
                if v != i64::MIN {
                    Ok(Value::from(-v))
                } else {
                    Err(ValueApplyError::Overflow)
                }
            }
            Value::RealValue(v) => {
                if !v.is_infinite() {
                    Ok(Value::from(-v))
                } else {
                    Err(ValueApplyError::Overflow)
                }
            }
            _ => Err(ValueApplyError::WrongTypes),
        },
        _ => unreachable!(),
    }
}

/// Applies the 'ord' function on the given expression operand, producing a
/// new Value.
pub fn apply_ord(expr: &Expr, type_table: &TypeTable) -> Result<Value, ValueApplyError> {
    // Can apply 'ord' on:
    // - char
    // - boolean
    // - char(n) or string(n) where n == 1
    // - enum.field

    // 'ord' produces identity on
    // - integer-class types

    let eval_type = &expr.get_eval_type();

    if types::is_integer_type(eval_type) {
        // Simple direct conversion
        Value::try_from(expr.clone()).map_err(|_| ValueApplyError::WrongTypes)
    } else if types::is_boolean(eval_type) {
        // NatCheat boolean
        let bool_value = Value::try_from(expr.clone()).map_err(|_| ValueApplyError::WrongTypes)?;

        // Don't worry about constraints
        apply_unary(
            UnaryOp::NatCheat,
            bool_value,
            EvalConstraints { as_64_bit: false },
        )
    } else if types::is_char(eval_type) || types::is_char_seq_type(eval_type) {
        // Convert to Value, check len, cheat into NatValue
        let value = Value::try_from(expr.clone()).map_err(|_| ValueApplyError::WrongTypes)?;

        if let Value::StringValue(str_value) = value {
            let mut chars = str_value.chars();
            let first_char = chars.next();

            if let Some(first_char) = first_char {
                if chars.next().is_none() {
                    // Is valid, take first character
                    return Ok(Value::from(first_char as u64));
                }
            }
        }

        // Empty length string or too many characters
        Err(ValueApplyError::WrongTypes)
    } else if let Some(Type::EnumField { ordinal, .. }) = type_table.type_from_ref(eval_type) {
        // Grab ordinal value, convert into NatValue
        Ok(Value::from(*ordinal as u64))
    } else {
        // Can't apply to given expression
        Err(ValueApplyError::WrongTypes)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! make_bops {
        ($( $l:literal $op:ident $r:literal == $exp:expr );* $( ; )?) => {
            {
                use BinaryOp::*;
                let mut res = vec![];

                $(
                    res.push((Value::from($l), $op, Value::from($r), Value::from($exp)));
                )*

                res
            }
        };
    }

    macro_rules! make_error_bops {
        ($( $l:literal $op:ident $r:literal causes $exp:expr );* $( ; )*) => {
            {
                use BinaryOp::*;
                use ValueApplyError::*;
                let mut res = vec![];

                $(
                    res.push((Value::from($l), $op, Value::from($r), $exp));
                )*

                res
            }
        };
    }

    macro_rules! make_uops {
        ($( $op:ident $r:literal == $exp:expr );* $( ; )?) => {
            {
                use UnaryOp::*;
                let mut res = vec![];

                $(
                    res.push(($op, Value::from($r), Value::from($exp)));
                )*

                res
            }
        };
    }

    macro_rules! make_error_uops {
        ($( $op:ident $r:literal causes $exp:expr );* $( ; )*) => {
            {
                use UnaryOp::*;
                use ValueApplyError::*;
                let mut res = vec![];

                $(
                    res.push(($op, Value::from($r), $exp));
                )*

                res
            }
        };
    }

    #[test]
    fn test_32bit_binary_ops() {
        // Binary operations on
        // + - * / div mod rem ** and or xor shl shr =>
        // < > <= >= = ~=

        let arithmetics = make_bops![
            // Add
            1u64 Add 1u64 == 2u64;
            1i64 Add 1u64 == 2i64;
            1i64 Add 1.5f64 == 2.5f64;
            "Hello " Add "World!" == "Hello World!";

            // Subtraction
             2u64 Sub 1u64 == 1u64;
            21i64 Sub 1u64 == 20i64;
             2u64 Sub 0.5f64 == 1.5f64;

            // Multiply
              3u64 Mul 2u64 == 6u64;
              3u64 Mul 2i64 == 6i64;
            3.2f64 Mul 2.0f64 == 6.4f64;
            3.2f64 Mul 2u64 == 6.4f64;
            // Integer division
            4u64 Div 2u64 == 2u64;
            3i64 Div -2i64 == -1i64;
            3f64 Div 2i64 == 1i64;

            // Real division
            3u64 RealDiv 2u64 == 1.5f64;
            3i64 RealDiv 2i64 == 1.5f64;
            3f64 RealDiv 2f64 == 1.5f64;

            // Modulo (mod_floor)
               5u64 Mod 2u64 == 1u64;

               5i64 Mod  2i64 ==  1i64;
               5i64 Mod -2i64 == -1i64;
              -5i64 Mod  2i64 ==  1i64;
              -5i64 Mod -2i64 == -1i64;

             5.3f64 Mod  2f64 ==  1.3f64;
             5.3f64 Mod -2f64 == -0.7f64;
            -5.3f64 Mod  2f64 ==  0.7f64;
            -5.3f64 Mod -2f64 == -1.3f64;

            // Remainder
               5u64 Rem 2u64 == 1u64;

               5i64 Rem  2i64 ==  1i64;
               5i64 Rem -2i64 ==  1i64;
              -5i64 Rem  2i64 == -1i64;
              -5i64 Rem -2i64 == -1i64;

             5.3f64 Rem  2f64 ==  1.3f64;
             5.3f64 Rem -2f64 ==  1.3f64;
            -5.3f64 Rem  2f64 == -1.3f64;
            -5.3f64 Rem -2f64 == -1.3f64;

            // Exponentiation
             2u64 Exp 3u64 == 8u64;

             3i64 Exp 3u64 ==  27i64;
            -3i64 Exp 3i64 == -27i64;
            -3i64 Exp 2i64 ==   9i64;

            -2f64 Exp  3f64 == -8f64;
            -2f64 Exp -3f64 == -0.125f64;

            // Bitwise And
             3u64 And 1u64 == 1u64;
            -3i64 And 2i64 == 0u64;

            // Boolean And
             true And true == true;
            false And true == false;
            false And false == false;

            // Bitwise Or
            2u64 Or 1i64 == 3u64;
            2u64 Or 4u64 == 6u64;

            // Boolean Or
            true Or true == true;
            true Or false == true;
            false Or false == false;

            // Bitwise Xor
            2u64 Xor 1i64 == 3u64;
            6u64 Xor 4u64 == 2u64;

            // Boolean Xor
            true Xor true == false;
            true Xor false == true;
            false Xor false == false;

            // Bitshift left
            // 32-bit masking should give the same result
            1u64 Shl 1u64 == 2u64;
            1u64 Shl 33u64 == 2u64;
            3i64 Shl 33i64 == 6u64;
            3i64 Shl 65i64 == 6u64;

            // Bitshift right
            // Should perform a 32-bit logical shift right
            1u64 Shr 64u64 == 1u64;
            1u64 Shr 32u64 == 1u64;
            2i64 Shr 1i64 == 1u64;
            -8i64 Shr 1i64 == 0x7FFFFFFCu64;

            // Imply
            true Imply true == true;
            true Imply false == false;
            false Imply true == true;
            false Imply false == true;

            // Equality
            true Equal true == true;
            true Equal false == false;

            10u64 Equal 10i64 == true;
            -1i64 Equal -1f64 == true;

            "Hello!" Equal "Hello!" == true;
            "Hello!" Equal "World!" == false;

            // Inequality
            true NotEqual false == true;
            true NotEqual true == false;

            "Hello!" NotEqual "Hello!" == false;
            "Hello!" NotEqual "World!" == true;

            // Less Than
            1f64 Less 10u64 == true;
            "H" Less "L" == true;

            45f64 Less 1i64 == false;
            "Z" Less "A" == false;

            // Greater Than
            1f64 Greater 10u64 == false;
            "H" Greater "L" == false;

            45f64 Greater 1i64 == true;
            "Z" Greater "A" == true;

            // Greater Equal
            45f64 GreaterEq 44.9f64 == true;
            "Z" GreaterEq "Z" == true;

            // Less Equal
            21f64 LessEq 45i64 == true;
            45f64 LessEq 45i64 == true;
            "A" LessEq "Z" == true;
            "Z" LessEq "Z" == true;
        ];

        for (test_num, validate) in arithmetics.into_iter().enumerate() {
            let (lhs, op, rhs, result) = validate;

            let eval = apply_binary(lhs, op, rhs, EvalConstraints { as_64_bit: false }).unwrap();

            if is_real(&eval) && is_real(&result) {
                // Use epsilon
                let eval: f64 = eval.into();
                let result: f64 = result.into();

                assert!(
                    (eval - result).abs() <= f64::EPSILON,
                    "FP Test #{} failed ({} > {})",
                    (test_num + 1),
                    (eval - result).abs(),
                    f64::EPSILON,
                );
            } else {
                assert_eq!(eval, result, "Test #{} (on {}) failed", (test_num + 1), op);
            }
        }
    }

    #[test]
    fn test_64bit_binary_ops() {
        // Binary operations on size-sensitive operations
        // shl shr

        let arithmetics = make_bops![
            // Bitshift left
            // 64-bit masking should give different results from the 32 - 63 range
            1u64 Shl 1u64 == 2u64;
            3u64 Shl 1u64 == 6u64;
            1u64 Shl 33u64 == 0x2_00000000u64;
            3i64 Shl 33i64 == 0x6_00000000u64;
            1i64 Shl 65i64 == 2u64;
            3i64 Shl 65i64 == 6u64;

            // Bitshift right
            // Should perform a logical shift right
            // 64-bit masking should give different results from the 32 - 63 range
            1u64 Shr 64u64 == 1u64;
            1u64 Shr 32u64 == 0u64;
            2i64 Shr 1i64 == 1u64;
            -8i64 Shr 1i64 == 0x7FFFFFFFFFFFFFFCu64;
        ];

        for (test_num, validate) in arithmetics.into_iter().enumerate() {
            let (lhs, op, rhs, result) = validate;

            let eval = apply_binary(lhs, op, rhs, EvalConstraints { as_64_bit: true }).unwrap();

            if is_real(&eval) && is_real(&result) {
                // Use epsilon
                let eval: f64 = eval.into();
                let result: f64 = result.into();

                assert!(
                    (eval - result).abs() <= f64::EPSILON,
                    "FP Test #{} failed ({} > {})",
                    (test_num + 1),
                    (eval - result).abs(),
                    f64::EPSILON,
                );
            } else {
                assert_eq!(eval, result, "Test #{} (on {}) failed", (test_num + 1), op);
            }
        }
    }

    #[test]
    fn test_binary_op_errors() {
        let error_tests = make_error_bops![
            // Invalid operands for shift
            1u64 Shl -1i64 causes InvalidOperand;
            1u64 Shr -1i64 causes InvalidOperand;

            // Division by 0
            1u64 Div 0u64 causes DivisionByZero;
            1f64 Div 0f64 causes DivisionByZero;
            1u64 RealDiv 0u64 causes DivisionByZero;
            1f64 RealDiv 0f64 causes DivisionByZero;
            1u64 Mod 0u64 causes DivisionByZero;
            1f64 Mod 0f64 causes DivisionByZero;
            1u64 Rem 0u64 causes DivisionByZero;
            1f64 Rem 0f64 causes DivisionByZero;

            // Exp cannot produce real values in int expression
            1i64 Exp -1i64 causes InvalidOperand;
            1u64 Exp -1i64 causes InvalidOperand;

            // Overflow
            0xFFFFFFFF_FFFFFFFFu64 Add 1u64 causes Overflow;
            0xFFFFFFFF_FFFFFFFFu64 Sub 1i64 causes Overflow; // Conversion overflow
            -9223372036854775808i64 Sub 1u64 causes Overflow;
            0xFFFFFFFF_FFFFFFFFu64 Mul 0xFFFFFFFF_FFFFFFFFu64 causes Overflow;
            -9223372036854775808i64 Mul -1i64 causes Overflow;
            10e303f64 Div 0.00000001f64 causes Overflow;
            // Mod and Rem can't be easily checked for overflow as those capture 0 as a division by zero
            -3i64 Exp 128u64 causes Overflow;
            10f64 Exp 310i64 causes Overflow;
            // Power overflow
            1u64 Exp 0x1_00000000u64 causes Overflow;
            -1i64 Exp 0x1_00000000i64 causes Overflow;
            10i64 Exp 310f64 causes Overflow;

            // Mismatched types
            "1" Add 1u64 causes WrongTypes;
            true Add 1u64 causes WrongTypes;

            "1" Sub 1u64 causes WrongTypes;
            true Sub 1u64 causes WrongTypes;

            "1" Mul 1u64 causes WrongTypes;
            true Mul 1u64 causes WrongTypes;

            "1" Div 1u64 causes WrongTypes;
            true Div 1u64 causes WrongTypes;

            "1" RealDiv 1u64 causes WrongTypes;
            true RealDiv 1u64 causes WrongTypes;

            "1" Mod 1u64 causes WrongTypes;
            true Mod 1u64 causes WrongTypes;

            "1" Rem 1u64 causes WrongTypes;
            true Rem 1u64 causes WrongTypes;

            "1" Exp 1u64 causes WrongTypes;
            true Exp 1u64 causes WrongTypes;

            "1" And 1u64 causes WrongTypes;
            true And 1u64 causes WrongTypes;
            1.0f64 And 1u64 causes WrongTypes;

            "1" Or 1u64 causes WrongTypes;
            true Or 1u64 causes WrongTypes;
            1.0f64 Or 1u64 causes WrongTypes;

            "1" Xor 1u64 causes WrongTypes;
            true Xor 1u64 causes WrongTypes;
            1.0f64 Xor 1u64 causes WrongTypes;

            "1" Shl 1u64 causes WrongTypes;
            true Shl 1u64 causes WrongTypes;
            1.0f64 Shl 1u64 causes WrongTypes;

            "1" Shr 1u64 causes WrongTypes;
            true Shr 1u64 causes WrongTypes;
            1.0f64 Shr 1u64 causes WrongTypes;

            "1" Imply true causes WrongTypes;
            true Imply 1u64 causes WrongTypes;
            1.0f64 Imply true causes WrongTypes;

            // Cannot compare ordering with booleans
            true Less true causes WrongTypes;
            true LessEq true causes WrongTypes;
            true Greater true causes WrongTypes;
            true GreaterEq true causes WrongTypes;
        ];

        for (test_num, validate) in error_tests.into_iter().enumerate() {
            let (lhs, op, rhs, expect_error) = validate;

            let error =
                apply_binary(lhs, op, rhs, EvalConstraints { as_64_bit: false }).expect_err(
                    &format!("Test #{} did not produce an error", (test_num + 1)),
                );

            assert_eq!(error, expect_error, "Test #{} failed", (test_num + 1));
        }
    }

    #[test]
    fn test_unary_ops() {
        let arithmetics = make_uops![
            Identity 1u64 == 1u64;
            Identity -2i64 == -2i64;
            Identity 1.5f64 == 1.5f64;

            Negate 3u64 == -3i64;
            Negate -2i64 == 2i64;
            Negate 1.5f64 == -1.5f64;

            Not 0xAAAAAAAA_AAAAAAAA_u64 == 0x55555555_55555555_u64;
            Not 0x55555555_55555555_i64 == 0xAAAAAAAA_AAAAAAAA_u64;
            Not true == false;
            Not false == true;

            NatCheat 1u64 == 1u64;
            NatCheat -2i64 == 0xFFFFFFFF_FFFFFFFEu64;
            // Halves are swapped around due to how Turing stores values on the operand stack
            // Done here to also preserve compatibility
            NatCheat -1f64 == 0x00000000_BFF00000u64;
            NatCheat true == 1u64;
            NatCheat false == 0u64;
            // Take the bytewise UTF-8 representation of the string, padded with zeros
            NatCheat "A" == 0x41u64;
            NatCheat "AA" == 0x4141u64;
            NatCheat "AAB" == 0x424141u64;
            NatCheat "AABB" == 0x42424141u64;
            NatCheat "AABBC" == 0x4342424141u64;
            NatCheat "AABBCC" == 0x434342424141u64;
            NatCheat "AABBCCD" == 0x44434342424141u64;
            NatCheat "AABBCCDD" == 0x4444434342424141u64;
            NatCheat "💛" == 0x9B929FF0u64; // U+1F49B -> F0 9F 92 9B
        ];

        for (test_num, validate) in arithmetics.into_iter().enumerate() {
            let (op, rhs, result) = validate;

            // Unary operations ignore constraints
            let eval = apply_unary(op, rhs, EvalConstraints { as_64_bit: false }).unwrap();

            if is_real(&eval) && is_real(&result) {
                // Use epsilon
                let eval: f64 = eval.into();
                let result: f64 = result.into();

                assert!(
                    (eval - result).abs() <= f64::EPSILON,
                    "FP Test #{} failed ({} > {})",
                    (test_num + 1),
                    (eval - result).abs(),
                    f64::EPSILON,
                );
            } else {
                assert_eq!(eval, result, "Test #{} failed", (test_num + 1));
            }
        }
    }

    #[test]
    fn test_unary_op_errors() {
        let error_tests = make_error_uops![
            // Overflow
            Negate 0x80000000_00000000_u64 causes Overflow;
            Negate -0x80000000_00000000_i64 causes Overflow;
            // Can't test Minus(f64) overflow, no way to make infs

            // Wrong types
            Identity true causes WrongTypes;
            Identity "no" causes WrongTypes;

            Negate false causes WrongTypes;
            Negate "nada" causes WrongTypes;

            Not "nada" causes WrongTypes;
            Not 1f64 causes WrongTypes;

            // Compile-time pound cheats are infallible
        ];

        for (test_num, validate) in error_tests.into_iter().enumerate() {
            let (op, rhs, expect_error) = validate;

            // Unary operations ignore constraints
            let error = apply_unary(op, rhs, EvalConstraints { as_64_bit: false }).expect_err(
                &format!("Test #{} did not produce an error", (test_num + 1)),
            );

            assert_eq!(error, expect_error, "Test #{} failed", (test_num + 1));
        }
    }
}
