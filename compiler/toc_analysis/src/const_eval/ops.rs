//! All valid compile-time operations

use crate::const_eval::{ConstError, ConstInt, ConstValue};

#[derive(Debug, Clone, Copy)]
pub(super) enum ConstOp {
    // Binary operations
    Add,
    Sub,
    Mul,
    Div,
    RealDiv,
    Mod,
    Rem,
    Exp,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    NotEqual,
    Imply,
    // Unary operations
    Not,
    Identity,
    Negate,
}

impl ConstOp {
    pub(super) fn evaluate(
        &self,
        operand_stack: &mut Vec<ConstValue>,
        allow_64bit_ops: bool,
    ) -> Result<ConstValue, ConstError> {
        match self {
            ConstOp::Add => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs + rhs {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_add(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::Sub => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs - rhs {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_sub(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::Mul => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs * rhs {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_mul(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::Div => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        // Divide & truncate, then convert to an integer
                        match (lhs / rhs).trunc() {
                            _ if rhs == 0.0 => Err(ConstError::DivByZero),
                            v => ConstInt::from_signed_real(v, allow_64bit_ops)
                                .map(|v| ConstValue::Integer(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_div(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::RealDiv => todo!(),
            ConstOp::Mod => todo!(),
            ConstOp::Rem => todo!(),
            ConstOp::Exp => todo!(),
            ConstOp::And => todo!(),
            ConstOp::Or => todo!(),
            ConstOp::Xor => todo!(),
            ConstOp::Shl => todo!(),
            ConstOp::Shr => todo!(),
            ConstOp::Less => todo!(),
            ConstOp::LessEq => todo!(),
            ConstOp::Greater => todo!(),
            ConstOp::GreaterEq => todo!(),
            ConstOp::Equal => todo!(),
            ConstOp::NotEqual => todo!(),
            ConstOp::Imply => todo!(),
            ConstOp::Not => todo!(),
            ConstOp::Identity => {
                // Rhs must be a number
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    rhs @ ConstValue::Integer(_) => Ok(rhs),
                    rhs @ ConstValue::Real(_) => Ok(rhs),
                }
            }
            ConstOp::Negate => {
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    ConstValue::Integer(v) => v.negate().map(|v| ConstValue::Integer(v)),
                    ConstValue::Real(v) => Ok(ConstValue::Real(-v)),
                }
            }
        }
    }
}
