//! Intermediate values for compile-time evaluation
use crate::compiler::ast::Expr;
use crate::compiler::token::{Token, TokenType};
use crate::compiler::types::{PrimitiveType, TypeRef};
use crate::compiler::Location;
use std::convert::TryFrom;

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
}

impl TryFrom<Expr> for Value {
    type Error = &'static str;

    fn try_from(value: Expr) -> Result<Value, Self::Error> {
        match value {
            Expr::Literal { value, .. } => match value.token_type {
                TokenType::BoolLiteral(v) => Ok(Value::BooleanValue(v)),
                TokenType::IntLiteral(v) => Ok(Value::IntValue(v)),
                TokenType::NatLiteral(v) => Ok(Value::NatValue(v)),
                TokenType::RealLiteral(v) => Ok(Value::RealValue(v)),
                TokenType::StringLiteral(v) => Ok(Value::StringValue(v)),
                TokenType::CharLiteral(v) => Ok(Value::StringValue(v)),
                _ => Err("Cannot convert complex literal into a value"),
            },
            _ => Err("Cannot convert non-literal expression into a value"),
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

fn is_boolean_value(value: &Value) -> bool {
    matches!(value, Value::BooleanValue(_))
}

fn is_string_value(value: &Value) -> bool {
    matches!(value, Value::StringValue(_))
}

fn is_int_value(value: &Value) -> bool {
    matches!(value, Value::IntValue(_))
}

fn is_nat_value(value: &Value) -> bool {
    matches!(value, Value::NatValue(_))
}

fn is_integer(value: &Value) -> bool {
    matches!(value, Value::IntValue(_) | Value::NatValue(_))
}

fn is_real_value(value: &Value) -> bool {
    matches!(value, Value::RealValue(_))
}

fn is_number(value: &Value) -> bool {
    matches!(value, Value::IntValue(_) | Value::NatValue(_) | Value::RealValue(_))
}

fn make_literal(kind: TokenType, eval_type: TypeRef) -> Expr {
    Expr::Literal {
        value: Token {
            location: Location::new(),
            token_type: kind,
        },
        eval_type,
    }
}

impl TryFrom<Value> for Expr {
    type Error = &'static str;

    fn try_from(v: Value) -> Result<Expr, Self::Error> {
        use super::value;

        match v {
            Value::BooleanValue(v) => Ok(value::make_literal(
                TokenType::BoolLiteral(v),
                TypeRef::Primitive(PrimitiveType::Boolean),
            )),
            Value::IntValue(v) => Ok(value::make_literal(
                TokenType::IntLiteral(v),
                TypeRef::Primitive(PrimitiveType::Int),
            )),
            Value::NatValue(v) => Ok(value::make_literal(
                TokenType::NatLiteral(v),
                TypeRef::Primitive(PrimitiveType::Nat),
            )),
            Value::RealValue(v) => Ok(value::make_literal(
                TokenType::RealLiteral(v),
                TypeRef::Primitive(PrimitiveType::Real),
            )),
            Value::StringValue(v) => Ok(value::make_literal(
                TokenType::StringLiteral(v),
                TypeRef::Primitive(PrimitiveType::String_),
            )),
        }
    }
}

/// Errors returned from value folding
#[derive(Debug)]
pub enum ValueApplyError {
    Overflow,
    DivisionByZero,
}

// Remaining operations to apply

// i/n mod n/i -> i
//   n mod n   -> n
// num mod num -> r

// i/n rem n/i -> i
//   n rem n   -> n
// num rem num -> r

// i/n ** n/i -> i
//   n ** n   -> n
// num ** num -> r

// i/n and n/i -> n
//   n and n   -> n
//   b and b   -> b

// i/n or n/i -> n
//   n or n   -> n
//   b or b   -> b

// i/n xor n/i -> n
//   n xor n   -> n
//   b xor b   -> b

// i/n shl n/i -> n
//   n shl n   -> n

// i/n shr n/i -> n
//   n shr n   -> n

//   b => b   -> b

/// Tries to convert the value into an i64. Consumes the value.
fn value_into_i64(v: Value) -> Result<i64, ValueApplyError> {
    match v {
        Value::NatValue(v) => {
            let v: u64 = v.into();
            i64::try_from(v).map_err(|_| ValueApplyError::Overflow)
        }
        Value::IntValue(v) => Ok(v),
        _ => panic!("Tried to convert {:?} into an i64", v),
    }
}

/// Tries to convert the value into an f64. Consumes the value.
fn value_into_f64(v: Value) -> Result<f64, ValueApplyError> {
    match v {
        Value::NatValue(v) => {
            let v: u64 = v.into();
            Ok(v as f64)
        }
        Value::IntValue(v) => {
            let v: i64 = v.into();
            Ok(v as f64)
        }
        Value::RealValue(v) => Ok(v),
        _ => panic!("Tried to convert {:?} into an f64", v),
    }
}

fn apply_binary_integer<N, I>(
    lhs: Value,
    rhs: Value,
    nat_apply: N,
    int_apply: I,
) -> Result<Value, ValueApplyError>
where
    N: FnOnce(u64, u64) -> Result<u64, ValueApplyError>,
    I: FnOnce(i64, i64) -> Result<i64, ValueApplyError>,
{
    if is_nat_value(&lhs) && is_nat_value(&rhs) {
        let lvalue: u64 = lhs.into();
        let rvalue: u64 = rhs.into();

        Ok(Value::from(nat_apply(lvalue, rvalue)?))
    } else if is_integer(&lhs) && is_integer(&rhs) {
        let lvalue = value_into_i64(lhs)?;
        let rvalue = value_into_i64(rhs)?;

        Ok(Value::from(int_apply(lvalue, rvalue)?))
    } else {
        unreachable!()
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
    N: FnOnce(u64, u64) -> Result<u64, ValueApplyError>,
    I: FnOnce(i64, i64) -> Result<i64, ValueApplyError>,
    R: FnOnce(f64, f64) -> Result<f64, ValueApplyError>,
{
    if is_nat_value(&lhs) && is_nat_value(&rhs) {
        let lvalue: u64 = lhs.into();
        let rvalue: u64 = rhs.into();

        Ok(Value::from(nat_apply(lvalue, rvalue)?))
    } else if is_integer(&lhs) && is_integer(&rhs) {
        let lvalue = value_into_i64(lhs)?;
        let rvalue = value_into_i64(rhs)?;

        Ok(Value::from(int_apply(lvalue, rvalue)?))
    } else if is_number(&lhs) && is_number(&rhs) {
        let lvalue = value_into_f64(lhs)?;
        let rvalue = value_into_f64(rhs)?;

        Ok(Value::from(real_apply(lvalue, rvalue)?))
    } else {
        unreachable!()
    }
}

// Remaining operators
// mod, rem -> num
// >, >=, <, <=, =, ~= -> b

/// Applies the specifies binary operation on the given value operands
pub fn apply_binary(lhs: Value, op: &TokenType, rhs: Value) -> Result<Value, ValueApplyError> {
    match op {
        TokenType::Plus => {
            if is_string_value(&lhs) && is_string_value(&rhs) {
                let mut lvalue: String = lhs.into();
                let rvalue: String = rhs.into();
                lvalue.push_str(&rvalue);

                Ok(Value::from(lvalue))
            } else {
                apply_binary_number(
                    lhs,
                    rhs,
                    |l, r| l.checked_add(r).ok_or(ValueApplyError::Overflow),
                    |l, r| l.checked_add(r).ok_or(ValueApplyError::Overflow),
                    |l, r| Ok(l + r),
                )
            }
        }
        TokenType::Minus => apply_binary_number(
            lhs,
            rhs,
            |l, r| l.checked_sub(r).ok_or(ValueApplyError::Overflow),
            |l, r| l.checked_sub(r).ok_or(ValueApplyError::Overflow),
            |l, r| Ok(l - r),
        ),
        TokenType::Star => apply_binary_number(
            lhs,
            rhs,
            |l, r| l.checked_mul(r).ok_or(ValueApplyError::Overflow),
            |l, r| l.checked_mul(r).ok_or(ValueApplyError::Overflow),
            |l, r| Ok(l * r),
        ),
        TokenType::Slash => {
            // Real division
            if is_number(&lhs) && is_number(&rhs) {
                let lvalue = value_into_f64(lhs)?;
                let rvalue = value_into_f64(rhs)?;
                let res = lvalue / rvalue;

                if res.is_infinite() {
                    Err(ValueApplyError::DivisionByZero)
                } else {
                    Ok(Value::from(res))
                }
            } else {
                unreachable!()
            }
        }
        TokenType::Div => {
            // Integer division
            if is_integer(&lhs) && is_integer(&rhs) {
                apply_binary_integer(
                    lhs,
                    rhs,
                    |l, r| {
                        if r == 0 {
                            Err(ValueApplyError::DivisionByZero)
                        } else {
                            Ok(l / r)
                        }
                    },
                    |l, r| {
                        if r == 0 {
                            Err(ValueApplyError::DivisionByZero)
                        } else {
                            Ok(l / r)
                        }
                    },
                )
            } else if is_number(&lhs) && is_number(&rhs) {
                let lvalue = value_into_f64(lhs)?;
                let rvalue = value_into_f64(rhs)?;
                let res = lvalue.div_euclid(rvalue);

                if rvalue.abs() < f64::EPSILON || res.is_infinite() {
                    Err(ValueApplyError::DivisionByZero)
                } else {
                    Ok(Value::from(res as i64))
                }
            } else {
                unreachable!()
            }
        }
        TokenType::Mod => todo!(),
        TokenType::Rem => todo!(),
        TokenType::Exp => todo!(),
        TokenType::And => todo!(),
        TokenType::Or => todo!(),
        TokenType::Xor => todo!(),
        TokenType::Shl => todo!(),
        TokenType::Shr => todo!(),
        TokenType::Imply => todo!(),
        TokenType::Less => todo!(),
        TokenType::LessEqu => todo!(),
        TokenType::Greater => todo!(),
        TokenType::GreaterEqu => todo!(),
        TokenType::Equ => todo!(),
        TokenType::NotEq => todo!(),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! make_bops {
		($( $l:literal $op:ident $r:literal == $exp:expr );* $( ; )?) => {
			{
				use TokenType::*;
				let mut res = vec![];

				$(
					res.push((Value::from($l), $op, Value::from($r), Value::from($exp)));
				)*

				res
			}
		};
	}

    #[test]
    fn test_str_concat() {
        assert_eq!(
            apply_binary(
                Value::from(String::from("Hello ")),
                &TokenType::Plus,
                Value::from(String::from("World!"))
            )
            .unwrap(),
            Value::from(String::from("Hello World!"))
        );
    }

    #[test]
    fn test_basic_arithmetic() {
        // Binary operations on
        // + - * / div

        let arithmetics = make_bops![
            // Add
            1u64 Plus 1u64 == 2u64;
            1i64 Plus 1u64 == 2i64;
            1i64 Plus 1.5f64 == 2.5f64;
            // Subtraction
            2u64 Minus 1u64 == 1u64;
            21i64 Minus 1u64 == 20i64;
            2u64 Minus 0.5f64 == 1.5f64;
            // Multiply
            3u64 Star 2u64 == 6u64;
            3u64 Star 2i64 == 6i64;
            3.2f64 Star 2.0f64 == 6.4f64;
            3.2f64 Star 2u64 == 6.4f64;
            // Integer division
            4u64 Div 2u64 == 2u64;
            3i64 Div 2i64 == 1i64;
            3f64 Div 2i64 == 1i64;
            // Real division
            3u64 Slash 2u64 == 1.5f64;
            3i64 Slash 2i64 == 1.5f64;
            3f64 Slash 2f64 == 1.5f64;
        ];

        for (test_num, validate) in arithmetics.into_iter().enumerate() {
            let (lhs, op, rhs, result) = validate;

            assert_eq!(
                apply_binary(lhs, &op, rhs).unwrap(),
                result,
                "Test #{} failed",
                (test_num + 1)
            );
        }
    }
}
