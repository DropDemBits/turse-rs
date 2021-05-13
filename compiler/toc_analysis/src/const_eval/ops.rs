//! All valid compile-time operations

use std::convert::TryFrom;

use toc_hir::expr;
use toc_span::Spanned;

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
                        lhs.checked_add(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
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
                        lhs.checked_sub(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
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
                        lhs.checked_mul(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
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
                                .map(ConstValue::Integer),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_div(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::RealDiv => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Integer(_), rhs)
                    | (lhs @ ConstValue::Real(_), rhs)
                    | (lhs, rhs @ ConstValue::Integer(_))
                    | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs / rhs {
                            _ if rhs == 0.0 => Err(ConstError::DivByZero),
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Mod => {
                // modulus with floored division
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);
                        let floored_mod = lhs - rhs * (lhs / rhs).floor();

                        match floored_mod {
                            _ if rhs == 0.0 => Err(ConstError::DivByZero),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_mod(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Rem => {
                // checked_rem
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs % lhs {
                            _ if rhs == 0.0 => Err(ConstError::DivByZero),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_rem(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Exp => {
                // checked_pow
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs.powf(rhs) {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_pow(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::And => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        Ok(ConstValue::Integer(lhs.and(rhs)))
                    }
                    (lhs @ ConstValue::Bool(_), rhs) | (lhs, rhs @ ConstValue::Bool(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_bool()?, rhs.cast_into_bool()?);
                        Ok(ConstValue::Bool(lhs & rhs))
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Or => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        Ok(ConstValue::Integer(lhs.or(rhs)))
                    }
                    (lhs @ ConstValue::Bool(_), rhs) | (lhs, rhs @ ConstValue::Bool(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_bool()?, rhs.cast_into_bool()?);
                        Ok(ConstValue::Bool(lhs | rhs))
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Xor => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        Ok(ConstValue::Integer(lhs.xor(rhs)))
                    }
                    (lhs @ ConstValue::Bool(_), rhs) | (lhs, rhs @ ConstValue::Bool(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_bool()?, rhs.cast_into_bool()?);
                        Ok(ConstValue::Bool(lhs ^ rhs))
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Shl => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_shl(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Shr => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_shr(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Less => todo!(),
            ConstOp::LessEq => todo!(),
            ConstOp::Greater => todo!(),
            ConstOp::GreaterEq => todo!(),
            ConstOp::Equal => todo!(),
            ConstOp::NotEqual => todo!(),
            ConstOp::Imply => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Bool(_), rhs) | (lhs, rhs @ ConstValue::Bool(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_bool()?, rhs.cast_into_bool()?);
                        Ok(ConstValue::Bool(!lhs | rhs))
                    }
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Not => {
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    ConstValue::Integer(v) => Ok(ConstValue::Integer(v.not())),
                    ConstValue::Bool(v) => Ok(ConstValue::Bool(!v)),
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Identity => {
                // Rhs must be a number
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    rhs @ ConstValue::Integer(_) => Ok(rhs),
                    rhs @ ConstValue::Real(_) => Ok(rhs),
                    _ => Err(ConstError::WrongType),
                }
            }
            ConstOp::Negate => {
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    ConstValue::Integer(v) => v.negate().map(ConstValue::Integer),
                    ConstValue::Real(v) => Ok(ConstValue::Real(-v)),
                    _ => Err(ConstError::WrongType),
                }
            }
        }
    }
}

impl TryFrom<Spanned<expr::BinaryOp>> for ConstOp {
    type Error = Spanned<ConstError>;

    fn try_from(op: Spanned<expr::BinaryOp>) -> Result<Self, Self::Error> {
        Ok(match op.item() {
            expr::BinaryOp::Add => Self::Add,
            expr::BinaryOp::Sub => Self::Sub,
            expr::BinaryOp::Mul => Self::Mul,
            expr::BinaryOp::Div => Self::Div,
            expr::BinaryOp::RealDiv => Self::RealDiv,
            expr::BinaryOp::Mod => Self::Mod,
            expr::BinaryOp::Rem => Self::Rem,
            expr::BinaryOp::Exp => Self::Exp,
            expr::BinaryOp::And => Self::And,
            expr::BinaryOp::Or => Self::Or,
            expr::BinaryOp::Xor => Self::Xor,
            expr::BinaryOp::Shl => Self::Shl,
            expr::BinaryOp::Shr => Self::Shr,
            expr::BinaryOp::Less => Self::Less,
            expr::BinaryOp::LessEq => Self::LessEq,
            expr::BinaryOp::Greater => Self::Greater,
            expr::BinaryOp::GreaterEq => Self::GreaterEq,
            expr::BinaryOp::Equal => Self::Equal,
            expr::BinaryOp::NotEqual => Self::NotEqual,
            expr::BinaryOp::Imply => Self::Imply,
            // Not a compile-time operation
            _ => return Err(Spanned::new(ConstError::NotConstOp, op.span())),
        })
    }
}

impl TryFrom<Spanned<expr::UnaryOp>> for ConstOp {
    type Error = Spanned<ConstError>;

    fn try_from(op: Spanned<expr::UnaryOp>) -> Result<Self, Self::Error> {
        match op.item() {
            expr::UnaryOp::Not => Ok(Self::Not),
            expr::UnaryOp::Identity => Ok(Self::Identity),
            expr::UnaryOp::Negate => Ok(Self::Negate),
        }
    }
}
