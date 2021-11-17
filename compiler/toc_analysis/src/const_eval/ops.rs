//! All valid compile-time operations

use std::convert::TryFrom;
use std::sync::Arc;

use toc_hir::expr;
use toc_span::Spanned;

use crate::const_eval::{errors::ErrorKind, ConstError, ConstInt, ConstValue};

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
    In,
    NotIn,
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
                    // Arithmetic Add
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs + rhs {
                            v if v.is_infinite() => {
                                Err(ConstError::without_span(ErrorKind::RealOverflow))
                            }
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_add(rhs).map(ConstValue::Integer)
                    }
                    // String Concatenation
                    // TODO: Check if we generate strings / character strings that are too big
                    // Makes CharN
                    (ConstValue::Char(lhs), ConstValue::Char(rhs)) => {
                        // Char, Char => Char(2)
                        Ok(ConstValue::CharN(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::Char(lhs), ConstValue::CharN(rhs)) => {
                        // Char, Char(N) => Char(N+1)
                        Ok(ConstValue::CharN(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::CharN(lhs), ConstValue::Char(rhs)) => {
                        // Char(N), Char => Char(N+1)
                        Ok(ConstValue::CharN(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::CharN(lhs), ConstValue::CharN(rhs)) => {
                        // Char(N), Char(M) => Char(N+M)
                        Ok(ConstValue::CharN(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    // Makes String
                    (ConstValue::String(lhs), ConstValue::Char(rhs)) => {
                        Ok(ConstValue::String(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::Char(lhs), ConstValue::String(rhs)) => {
                        Ok(ConstValue::String(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::String(lhs), ConstValue::CharN(rhs)) => {
                        Ok(ConstValue::String(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::CharN(lhs), ConstValue::String(rhs)) => {
                        Ok(ConstValue::String(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    (ConstValue::String(lhs), ConstValue::String(rhs)) => {
                        Ok(ConstValue::String(Arc::new(format!("{}{}", lhs, rhs))))
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
            ConstOp::Sub => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs - rhs {
                            v if v.is_infinite() => {
                                Err(ConstError::without_span(ErrorKind::RealOverflow))
                            }
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_sub(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
            ConstOp::Mul => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs * rhs {
                            v if v.is_infinite() => {
                                Err(ConstError::without_span(ErrorKind::RealOverflow))
                            }
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_mul(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                            _ if rhs == 0.0 => Err(ConstError::without_span(ErrorKind::DivByZero)),
                            v => ConstInt::from_signed_real(v, allow_64bit_ops)
                                .map(ConstValue::Integer),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_div(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                            _ if rhs == 0.0 => Err(ConstError::without_span(ErrorKind::DivByZero)),
                            v if v.is_infinite() => {
                                Err(ConstError::without_span(ErrorKind::RealOverflow))
                            }
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                            _ if rhs == 0.0 => Err(ConstError::without_span(ErrorKind::DivByZero)),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_mod(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                            _ if rhs == 0.0 => Err(ConstError::without_span(ErrorKind::DivByZero)),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_rem(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                            v if v.is_infinite() => {
                                Err(ConstError::without_span(ErrorKind::RealOverflow))
                            }
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_pow(rhs).map(ConstValue::Integer)
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
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
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
            ConstOp::Less
            | ConstOp::LessEq
            | ConstOp::Greater
            | ConstOp::GreaterEq
            | ConstOp::Equal
            | ConstOp::NotEqual
            | ConstOp::In
            | ConstOp::NotIn => Err(ConstError::without_span(ErrorKind::UnsupportedOp)),
            ConstOp::Imply => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Bool(_), rhs) | (lhs, rhs @ ConstValue::Bool(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_bool()?, rhs.cast_into_bool()?);
                        Ok(ConstValue::Bool(!lhs | rhs))
                    }
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
            ConstOp::Not => {
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    ConstValue::Integer(v) => Ok(ConstValue::Integer(v.not())),
                    ConstValue::Bool(v) => Ok(ConstValue::Bool(!v)),
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
            ConstOp::Identity => {
                // Rhs must be a number
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    rhs @ ConstValue::Integer(_) => Ok(rhs),
                    rhs @ ConstValue::Real(_) => Ok(rhs),
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
            ConstOp::Negate => {
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    ConstValue::Integer(v) => v.negate().map(ConstValue::Integer),
                    ConstValue::Real(v) => Ok(ConstValue::Real(-v)),
                    _ => Err(ConstError::without_span(ErrorKind::WrongOperandType)),
                }
            }
        }
    }
}

impl TryFrom<Spanned<expr::BinaryOp>> for ConstOp {
    type Error = ConstError;

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
            expr::BinaryOp::In => Self::In,
            expr::BinaryOp::NotIn => Self::NotIn,
            expr::BinaryOp::Imply => Self::Imply,
        })
    }
}

impl TryFrom<Spanned<expr::UnaryOp>> for ConstOp {
    type Error = ConstError;

    fn try_from(op: Spanned<expr::UnaryOp>) -> Result<Self, Self::Error> {
        match op.item() {
            expr::UnaryOp::Not => Ok(Self::Not),
            expr::UnaryOp::Identity => Ok(Self::Identity),
            expr::UnaryOp::Negate => Ok(Self::Negate),
        }
    }
}
