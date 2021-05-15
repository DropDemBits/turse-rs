//! Rules for type interactions
use toc_hir::expr;
use toc_reporting::{MessageKind, MessageSink};
use toc_span::Spanned;

use crate::ty::{SeqSize, TyRef, Type};

/// Type for associated mismatch binary operand types
pub struct MismatchedBinaryTypes {
    _lhs: Spanned<TyRef>,
    op: Spanned<expr::BinaryOp>,
    _rhs: Spanned<TyRef>,
}

/// Type for associated mismatch unary operand types
pub struct MismatchedUnaryTypes {
    op: Spanned<expr::UnaryOp>,
    _rhs: Spanned<TyRef>,
}

/// Returns `Some(is_assignable)`, or `None` if either type is `ty::Error`
pub fn is_ty_assignable_to(l_value_ty: TyRef, r_value_ty: TyRef) -> Option<bool> {
    /// Maximum length of a `string`
    const MAX_STRING_LEN: u32 = 256;

    // Current assignability rules:
    // boolean :=
    //   boolean
    //
    // Int(_) :=
    //   Int(_)
    // | Nat(_) [runtime checked]
    // | Integer [runtime checked]
    //
    // Nat(_) :=
    //   Int(_) [runtime checked]
    // | Nat(_)
    // | Integer [runtime checked]
    //
    // Real(_) :=
    //   Real(_)
    // | Int(_) [runtime checked]
    // | Nat(_)
    // | Integer [runtime checked]
    //
    // Char :=
    //   Char
    // | Char(1)
    // | String(1)
    // | String [runtime checked]
    //
    // Char(N) :=
    //   Char if N = 1
    // | Char(M) where N = M
    // | String(M) where N = M
    // | String [runtime checked]
    //
    // String :=
    //   Char
    // | String
    // | Char(N) where N < 256
    // | String(N)
    //
    // String(N) :=
    //   String(M) where N >= M
    // | Char(M) where N >= M and M < 256
    // | Char
    // | String [runtime checked]
    //

    let is_assignable = match (&*l_value_ty, &*r_value_ty) {
        // Short-circuiting error types
        (Type::Error, _) | (_, Type::Error) => return None,

        // Boolean types are assignable to each other
        (Type::Boolean, Type::Boolean) => true,

        // Integer types are assignable to each other
        (Type::Nat(_), other) | (other, Type::Nat(_)) if is_integer(other) => true,
        (Type::Int(_), other) | (other, Type::Int(_)) if is_integer(other) => true,

        // All numeric types are assignable into a real
        (Type::Real(_), rhs) if is_number(rhs) => true,

        // Char rules:
        // - String(1), Char(1), and Char are assignable into Char
        // - String is assignable into Char, but checked at runtime
        (Type::Char, Type::Char) => true,
        (Type::Char, Type::CharN(SeqSize::Fixed(size))) if size.get() == 1 => true,
        (Type::Char, Type::StringN(SeqSize::Fixed(size))) if size.get() == 1 => true,
        (Type::Char, Type::String) => true,

        // Char(N) rules:
        // - Char is assignable into Char(N) if N = 1
        // - Char(M) is assignable into Char(N) if N = M
        // - String(M) is assignable into Char(N) if N <= M, but double checked at runtime
        // - String is assignable into Char(N), but checked at runtime
        (Type::CharN(SeqSize::Fixed(n)), Type::Char) => n.get() == 1,
        (Type::CharN(SeqSize::Fixed(n)), Type::CharN(SeqSize::Fixed(m))) => n == m,
        (Type::CharN(SeqSize::Fixed(n)), Type::StringN(SeqSize::Fixed(m))) => n <= m,
        (Type::CharN(SeqSize::Fixed(_)), Type::String) => true,

        // String rules:
        // - Char, String(N) and String are assignable into String
        // - Char(N) is assignable into String if N < `MAX_STRING_LEN`
        (Type::String, Type::String) => true,
        (Type::String, Type::StringN(SeqSize::Fixed(_))) => true,
        (Type::String, Type::Char) => true,
        (Type::String, Type::CharN(SeqSize::Fixed(n))) => n.get() < MAX_STRING_LEN,

        // String(N) rules:
        // - Char is assignable into String(N)
        // - String is assignable into String(N), but checked at runtime
        // - String(M) is assignable into String(N), but is double checked at runtime
        // - Char(M) is assignable into String(N) if n >= m and m < `MAX_STRING_LEN`
        (Type::StringN(SeqSize::Fixed(_)), Type::Char) => true,
        (Type::StringN(SeqSize::Fixed(_)), Type::String) => true,
        (Type::StringN(SeqSize::Fixed(_)), Type::StringN(SeqSize::Fixed(_))) => true,
        (Type::StringN(SeqSize::Fixed(n)), Type::CharN(SeqSize::Fixed(m))) => {
            n >= m && m.get() < MAX_STRING_LEN
        }

        // Not assignable otherwise
        _ => false,
    };

    Some(is_assignable)
}

pub fn is_number(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Integer | Type::Real(_) | Type::Int(_) | Type::Nat(_)
    )
}

pub fn is_integer(ty: &Type) -> bool {
    matches!(ty, Type::Integer | Type::Int(_) | Type::Nat(_))
}

pub fn is_nat(ty: &Type) -> bool {
    matches!(ty, Type::Integer | Type::Nat(_))
}

pub fn is_error(ty: &Type) -> bool {
    matches!(ty, Type::Error)
}

pub fn check_binary_operands(
    lhs_ty_ref: Spanned<TyRef>,
    op: Spanned<expr::BinaryOp>,
    rhs_ty_ref: Spanned<TyRef>,
) -> Result<Type, MismatchedBinaryTypes> {
    // TODO: Handle 32-bit vs 64-bit integer widths

    // Type classes:
    // `charseq`: string, char, string(n), char(n)
    // `number`: real, int, nat
    // `integer`: int, nat

    // lhs - rhs
    // (int/nat) - real => real
    // real - (int/nat) => real
    // int - (int/nat) => int
    // (int/nat) - int => int
    // nat - nat => nat
    use crate::ty::{IntSize, NatSize, RealSize};

    fn check_arithmetic_operands(lhs_ty: &Type, rhs_ty: &Type) -> Option<Type> {
        match (lhs_ty, rhs_ty) {
            // Pass through integer inference
            (Type::Integer, Type::Integer) => Some(Type::Integer),

            // Normal operands
            (operand, Type::Real(_)) | (Type::Real(_), operand) if is_number(operand) => {
                Some(Type::Real(RealSize::Real))
            }
            (operand, Type::Int(_)) | (Type::Int(_), operand) if is_integer(operand) => {
                Some(Type::Int(IntSize::Int))
            }
            (operand, Type::Nat(_)) | (Type::Nat(_), operand) if is_nat(operand) => {
                Some(Type::Nat(NatSize::Nat))
            }
            _ => None,
        }
    }

    fn check_bitwise_operands(lhs_ty: &Type, rhs_ty: &Type) -> Option<Type> {
        match (lhs_ty, rhs_ty) {
            // Normal operands
            // Integer inference is not passed through
            (lhs, rhs) if is_integer(lhs) && is_integer(rhs) => Some(Type::Nat(NatSize::Nat)),
            _ => None,
        }
    }

    fn create_binary_type_error(
        lhs_ty_ref: Spanned<TyRef>,
        op: Spanned<expr::BinaryOp>,
        rhs_ty_ref: Spanned<TyRef>,
    ) -> Result<Type, MismatchedBinaryTypes> {
        Err(MismatchedBinaryTypes {
            _lhs: lhs_ty_ref,
            op,
            _rhs: rhs_ty_ref,
        })
    }

    let (lhs_ty, rhs_ty) = (&**lhs_ty_ref.item(), &**rhs_ty_ref.item());

    // Short circuit for error types
    // Don't duplicate errors
    if is_error(&lhs_ty) || is_error(&rhs_ty) {
        return Ok(Type::Error);
    }

    match op.item() {
        // Arithmetic operators
        expr::BinaryOp::Add => {
            // Operations:
            // x String concatenation (charseq, charseq => charseq)
            // x Set union (set, set => set)
            // - Addition (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(&lhs_ty, &rhs_ty) {
                // Addition
                Ok(result_ty)
            } else {
                // Type error
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Sub => {
            // Operations:
            // x Set difference (set, set => set)
            // - Subtraction (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(&lhs_ty, &rhs_ty) {
                // Subtraction
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Mul => {
            // Operations:
            // x Set intersection (set, set => set)
            // - Multiplication (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(&lhs_ty, &rhs_ty) {
                // Multiplication
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Div => {
            // Operations:
            // - Integer division (number, number => integer)

            match (&*lhs_ty, &*rhs_ty) {
                // Pass through type inference
                (Type::Integer, Type::Integer) => Ok(Type::Integer),
                (operand, Type::Nat(_)) | (Type::Nat(_), operand) if is_nat(operand) => {
                    Ok(Type::Nat(NatSize::Nat))
                }
                (lhs, rhs) if is_number(lhs) && is_number(rhs) => Ok(Type::Int(IntSize::Int)),
                _ => create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref),
            }
        }
        expr::BinaryOp::RealDiv => {
            // Operations:
            // - Floating point division (number, number => real)

            if is_number(&lhs_ty) && is_number(&rhs_ty) {
                Ok(Type::Real(RealSize::Real))
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Mod => {
            // Operations:
            // - Modulo (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(&lhs_ty, &rhs_ty) {
                // Modulo
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Rem => {
            // Operations:
            // - Remainder (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(&lhs_ty, &rhs_ty) {
                // Remainder
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Exp => {
            // Operations:
            // - Exponentiation (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(&lhs_ty, &rhs_ty) {
                // Exponentiation
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        // Bitwise operators (integer, integer => nat)
        // + Logical operators (boolean, boolean => boolean)
        expr::BinaryOp::And => {
            // Operations:
            // - Bitwise And (integer, integer => nat)
            // - Logical And (boolean, boolean => boolean)

            if let Some(result_ty) = check_bitwise_operands(&lhs_ty, &rhs_ty) {
                // Bitwise And
                Ok(result_ty)
            } else if let (Type::Boolean, Type::Boolean) = (&lhs_ty, &rhs_ty) {
                // Logical And
                Ok(Type::Boolean)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Or => {
            // Operations:
            // - Bitwise Or (integer, integer => nat)
            // - Logical Or (boolean, boolean => boolean)

            if let Some(result_ty) = check_bitwise_operands(&lhs_ty, &rhs_ty) {
                // Bitwise Or
                Ok(result_ty)
            } else if let (Type::Boolean, Type::Boolean) = (&lhs_ty, &rhs_ty) {
                // Logical Or
                Ok(Type::Boolean)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Xor => {
            // Operations:
            // - Bitwise Xor (integer, integer => nat)
            // - Logical Xor (boolean, boolean => boolean)

            if let Some(result_ty) = check_bitwise_operands(&lhs_ty, &rhs_ty) {
                // Bitwise Xor
                Ok(result_ty)
            } else if let (Type::Boolean, Type::Boolean) = (&lhs_ty, &rhs_ty) {
                // Logical Xor
                Ok(Type::Boolean)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        // Pure bitwise operators
        expr::BinaryOp::Shl => {
            // Operations:
            // - Bitwise Shl (integer, integer => nat)

            if let Some(result_ty) = check_bitwise_operands(&lhs_ty, &rhs_ty) {
                // Bitwise Shl
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        expr::BinaryOp::Shr => {
            // Operations:
            // - Bitwise Shr (integer, integer => nat)

            if let Some(result_ty) = check_bitwise_operands(&lhs_ty, &rhs_ty) {
                // Bitwise Shr
                Ok(result_ty)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        // Pure logical operator
        expr::BinaryOp::Imply => {
            // Operations:
            // - Imply (boolean, boolean => boolean)

            if let (Type::Boolean, Type::Boolean) = (&lhs_ty, &rhs_ty) {
                // Logical Xor
                Ok(Type::Boolean)
            } else {
                create_binary_type_error(lhs_ty_ref, op, rhs_ty_ref)
            }
        }
        // Comparison (a, b => boolean where a, b: Comparable)
        expr::BinaryOp::Less => todo!(),
        expr::BinaryOp::LessEq => todo!(),
        expr::BinaryOp::Greater => todo!(),
        expr::BinaryOp::GreaterEq => todo!(),
        expr::BinaryOp::Equal => todo!(),
        expr::BinaryOp::NotEqual => todo!(),
        // Set membership tests (set(a), a => boolean)
        expr::BinaryOp::In => todo!(),
        expr::BinaryOp::NotIn => todo!(),
    }
}

pub fn report_binary_typecheck_error(err: MismatchedBinaryTypes, reporter: &mut MessageSink) {
    let MismatchedBinaryTypes { op, .. } = err;
    let op_name = match op.item() {
        expr::BinaryOp::Add => "addition",
        expr::BinaryOp::Sub => "subtraction",
        expr::BinaryOp::Mul => "multiplication",
        expr::BinaryOp::Div => "integer division",
        expr::BinaryOp::RealDiv => "real division",
        expr::BinaryOp::Mod => "modulus",
        expr::BinaryOp::Rem => "remainder",
        expr::BinaryOp::Exp => "exponentiation",
        expr::BinaryOp::And => "`and`",
        expr::BinaryOp::Or => "`or`",
        expr::BinaryOp::Xor => "`xor`",
        expr::BinaryOp::Shl => "`shl`",
        expr::BinaryOp::Shr => "`shr`",
        expr::BinaryOp::Imply => "`=>`",
        expr::BinaryOp::Less => "`<`",
        expr::BinaryOp::LessEq => "`<=`",
        expr::BinaryOp::Greater => "`>`",
        expr::BinaryOp::GreaterEq => "`>=`",
        expr::BinaryOp::Equal => "`=`",
        expr::BinaryOp::NotEqual => "`not =`",
        expr::BinaryOp::In => "`in`",
        expr::BinaryOp::NotIn => "`not in`",
    };

    let msg = reporter.report_detailed(
        MessageKind::Error,
        &format!("incompatible types for {}", op_name),
        op.span(),
    );
    let msg = match op.item() {
        // Arithmetic operators
        expr::BinaryOp::Add => {
            msg.with_info("operands must both be numbers, strings, or sets", None)
        }
        expr::BinaryOp::Sub | expr::BinaryOp::Mul => {
            msg.with_info("operands must both be numbers or sets", None)
        }
        expr::BinaryOp::Div
        | expr::BinaryOp::RealDiv
        | expr::BinaryOp::Mod
        | expr::BinaryOp::Rem
        | expr::BinaryOp::Exp => msg.with_info("operands must both be numbers", None),
        // Bitwise operators (integer, integer => nat)
        // + Logical operators (boolean, boolean => boolean)
        expr::BinaryOp::And | expr::BinaryOp::Or | expr::BinaryOp::Xor => {
            msg.with_info("operands must both be integers or booleans", None)
        }
        // Pure bitwise operators
        expr::BinaryOp::Shl | expr::BinaryOp::Shr => {
            msg.with_info("operands must both be integers", None)
        }
        // Pure logical operator
        expr::BinaryOp::Imply => msg.with_info("operands must both be booleans", None),
        // Comparison (a, b => boolean where a, b: Comparable)
        expr::BinaryOp::Less => todo!(),
        expr::BinaryOp::LessEq => todo!(),
        expr::BinaryOp::Greater => todo!(),
        expr::BinaryOp::GreaterEq => todo!(),
        expr::BinaryOp::Equal => todo!(),
        expr::BinaryOp::NotEqual => todo!(),
        // Set membership tests (set(a), a => boolean)
        expr::BinaryOp::In => todo!(),
        expr::BinaryOp::NotIn => todo!(),
    };
    msg.finish();
}

pub fn check_unary_operands(
    op: Spanned<expr::UnaryOp>,
    rhs_ty_ref: Spanned<TyRef>,
) -> Result<Type, MismatchedUnaryTypes> {
    use crate::ty::{IntSize, NatSize, RealSize};

    fn create_unary_type_error(
        op: Spanned<expr::UnaryOp>,
        rhs_ty_ref: Spanned<TyRef>,
    ) -> Result<Type, MismatchedUnaryTypes> {
        Err(MismatchedUnaryTypes {
            op,
            _rhs: rhs_ty_ref,
        })
    }

    let rhs_ty = &**rhs_ty_ref.item();

    // Short circuit for error types
    // Don't duplicate errors
    if is_error(&rhs_ty) {
        return Ok(Type::Error);
    }

    match op.item() {
        expr::UnaryOp::Not => {
            if is_integer(rhs_ty) {
                // Bitwise Not
                Ok(Type::Nat(NatSize::Nat))
            } else if let Type::Boolean = &rhs_ty {
                // Logical Not
                Ok(Type::Boolean)
            } else {
                create_unary_type_error(op, rhs_ty_ref)
            }
        }
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            match rhs_ty {
                // Pass through integer inference
                Type::Integer => Ok(Type::Integer),

                // Normal operands
                Type::Real(_) => Ok(Type::Real(RealSize::Real)),
                Type::Int(_) => Ok(Type::Int(IntSize::Int)),
                Type::Nat(_) => Ok(Type::Nat(NatSize::Nat)),
                _ => create_unary_type_error(op, rhs_ty_ref),
            }
        }
    }
}

pub fn report_unary_typecheck_error(err: MismatchedUnaryTypes, reporter: &mut MessageSink) {
    let MismatchedUnaryTypes { op, .. } = err;
    let op_name = match op.item() {
        expr::UnaryOp::Not => "`not`",
        expr::UnaryOp::Identity => "unary `+`",
        expr::UnaryOp::Negate => "unary `-`",
    };

    let msg = reporter.report_detailed(
        MessageKind::Error,
        &format!("incompatible types for {}", op_name),
        op.span(),
    );
    let msg = match op.item() {
        expr::UnaryOp::Not => msg.with_info("operand must be an integer or boolean", None),
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            msg.with_info("operand must be a number", None)
        }
    };
    msg.finish();
}
