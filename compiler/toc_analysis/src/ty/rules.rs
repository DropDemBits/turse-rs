//! Rules for type interactions
use toc_hir::expr;
use toc_reporting::MessageSink;
use toc_span::Span;

use crate::{
    const_eval::{ConstInt, ConstResult},
    db,
    ty::{Mutability, SeqSize, TypeId, TypeKind},
};

use super::{TyRef, TyRefKind, TypeData};

impl TypeKind {
    pub fn is_number(&self) -> bool {
        matches!(
            self,
            TypeKind::Integer | TypeKind::Real(_) | TypeKind::Int(_) | TypeKind::Nat(_)
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            TypeKind::Integer | TypeKind::Int(_) | TypeKind::Nat(_)
        )
    }

    pub fn is_nat(&self) -> bool {
        matches!(self, TypeKind::Integer | TypeKind::Nat(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self, TypeKind::Error)
    }
}

// Conversions of type
impl<'db, DB> TyRef<'db, DB>
where
    DB: db::TypeDatabase + ?Sized + 'db,
{
    /// Applies a deref transformation
    pub fn as_deref(self) -> Option<Self> {
        match *self.kind() {
            TypeKind::Error => Some(self), // Propagate error
            TypeKind::Ref(_, ty) => Some(ty.in_db(self.db)),
            _ => None,
        }
    }

    /// Applies a deref transformation, requiring var mutability
    pub fn as_deref_mut(self) -> Option<Self> {
        match *self.kind() {
            TypeKind::Error => Some(self), // Propagate error
            TypeKind::Ref(Mutability::Var, ty) => Some(ty.in_db(self.db)),
            _ => None,
        }
    }

    /// Returns the type id pointed to by a ref, or itself if it's not a type
    ///
    /// # Example
    ///
    /// ```text
    /// Boolean -> Boolean
    /// Ref(Var, Boolean) -> Boolean
    /// Ref(Const, Ref(Const, Boolean)) -> Ref(Const, Boolean)
    /// ```
    pub fn peel_ref(self) -> Self {
        match self.as_deref() {
            Some(to) => to,
            None => self,
        }
    }

    pub fn data(self) -> TypeData {
        self.db.lookup_intern_type(self.id)
    }

    pub fn kind(self) -> TyRefKind {
        TyRefKind(self.data())
    }

    pub fn id(self) -> TypeId {
        self.id
    }
}

impl SeqSize {
    pub fn fixed_len<T: db::ConstEval + ?Sized>(
        &self,
        db: &T,
        span: Span,
    ) -> ConstResult<Option<ConstInt>> {
        let size = match self {
            SeqSize::Dynamic => return Ok(None),
            SeqSize::Fixed(size) => size,
        };

        // Always eagerly evaluate the expr
        // Never allow 64-bit ops (size is always less than 2^32)
        db.evaluate_const(size.clone(), Default::default())
            .and_then(|v| v.into_int(span))
            .map(Some)
    }
}

/// Type for associated mismatch binary operand types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidBinaryOp {
    op: expr::BinaryOp,
    unsupported: bool,
}

/// Type for associated mismatch unary operand types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnaryOp {
    op: expr::UnaryOp,
}

/// Returns `Some(is_assignable)`, or `None` if the the l_value is not deref'able.
/// `ignore_mut` is only to be used during initializers
///
/// l_value should be left as a ref it is one.
pub fn is_assignable<T: db::ConstEval + ?Sized>(
    db: &T,
    left: TypeId,
    right: TypeId,
    ignore_mut: bool,
) -> Option<bool> {
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
    // | Char(*) [runtime checked]
    // | String(*) [runtime checked]
    //
    // Char(N | *) :=
    //   Char where N = 1
    // | Char(M) where N = M
    // | String(M) where N = M
    // | String [runtime checked]
    // | Char(*) [runtime checked]
    // | String(*) [runtime checked]
    //
    // String :=
    //   Char
    // | String
    // | Char(N) where N < 256
    // | String(N)
    // | Char(*) [runtime checked]
    // | String(*)
    //
    // String(N | *) :=
    //   String(M) where N >= M
    // | Char(M) where N >= M and M < 256
    // | Char
    // | String [runtime checked]
    // | Char(*) [runtime checked]
    // | String(*) [runtime checked]
    //

    /// Maximum length of a `string`
    const MAX_STRING_LEN: u32 = 256;

    let left = left.in_db(db);
    let right = right.in_db(db);

    let left = if ignore_mut {
        left.as_deref()?
    } else {
        left.as_deref_mut()?
    };
    let right = right.peel_ref();

    /// Gets a sequence size suitable for assignment checking.
    /// All errors (overflow, other const error) and dynamic sizes are ignored.
    fn seq_size<T: db::ConstEval + ?Sized>(db: &T, seq_size: &SeqSize) -> Option<u32> {
        match seq_size.fixed_len(db, Default::default()) {
            Ok(Some(v)) => {
                // if overflow or zero, it's silently ignored (reported in typeck)
                v.into_u32().filter(|v| *v != 0)
            }
            Ok(None) => None, // dynamic, runtime checked
            Err(_) => None,   // silently hide, gets reported in typeck
        }
    }

    let is_assignable = match (&*left.kind(), &*right.kind()) {
        // Short-circuiting error types
        // Always pass
        (TypeKind::Error, _) | (_, TypeKind::Error) => return Some(true),

        // Boolean types are assignable to each other
        (TypeKind::Boolean, TypeKind::Boolean) => true,

        // Integer types are assignable to each other
        (TypeKind::Nat(_), other) | (other, TypeKind::Nat(_)) if other.is_integer() => true,
        (TypeKind::Int(_), other) | (other, TypeKind::Int(_)) if other.is_integer() => true,

        // All numeric types are assignable into a real
        (TypeKind::Real(_), rhs) if rhs.is_number() => true,

        // Char rules:
        // - String(1), Char(1), and Char are assignable into Char
        // - String is assignable into Char, but checked at runtime
        // - String(*) and Char(*) is assignable into Char, but checked at runtime
        (TypeKind::Char, TypeKind::Char | TypeKind::String) => true,
        (TypeKind::Char, TypeKind::StringN(size)) | (TypeKind::Char, TypeKind::CharN(size)) => {
            seq_size(db, size).map(|n| n == 1).unwrap_or(true)
        }

        // Char(N) rules:
        // - Char is assignable into Char(N) if N = 1
        // - Char(M) is assignable into Char(N) if N = M
        // - String(M) is assignable into Char(N) if N <= M, but double checked at runtime
        // - String is assignable into Char(N), but checked at runtime
        // - All of these are assignable into Char(*), but checked at runtime
        (TypeKind::CharN(left), rhs) => match (seq_size(db, left), rhs) {
            // Char(N) := Char where N = 1
            (Some(n), TypeKind::Char) => n == 1,
            // Char(N) := Char(* | M) where N = M
            (Some(n), TypeKind::CharN(right)) => {
                seq_size(db, right).map(|m| n == m).unwrap_or(true)
            }
            // Char(N) := String(* | M) where N <= M [also runtime checked]
            (Some(n), TypeKind::StringN(right)) => {
                seq_size(db, right).map(|m| n <= m).unwrap_or(true)
            }
            // Char(N) := String [runtime checked]
            (Some(_n), TypeKind::String) => true,
            // Char(*) := [runtime checked]
            //   Char
            // | Char(* | N)
            // | String
            // | String(* | N)
            (
                None,
                TypeKind::Char | TypeKind::CharN(_) | TypeKind::String | TypeKind::StringN(_),
            ) => true,
            (_, _) => false,
        },

        // String rules:
        // - Char, String(N), String(*) and String are assignable into String
        // - Char(N) is assignable into String if N < `MAX_STRING_LEN`
        // - Char(*) is assignable into String, but is checked at runtime

        // String(N) rules:
        // - Char is assignable into String(N) and String(*)
        // - String is assignable into String(N) and String(*), but checked at runtime
        // - String(M) is assignable into String(N) and String(*), but String(N) is double checked at runtime
        // - Char(M) is assignable into String(N) and String(*) if n >= m and m < `MAX_STRING_LEN`

        // String | String(* | N) :=
        //   Char
        // | String
        // | String(* | N)
        (
            TypeKind::String | TypeKind::StringN(_),
            TypeKind::Char | TypeKind::StringN(_) | TypeKind::String,
        ) => true,
        (TypeKind::String, TypeKind::CharN(size)) => match seq_size(db, size) {
            // String := Char(N) if N < `MAX_STRING_LEN`
            Some(n) => n < MAX_STRING_LEN,
            // String := Char(*) [runtime checked]
            None => true,
        },
        (TypeKind::StringN(left), TypeKind::CharN(right)) => {
            match (seq_size(db, left), seq_size(db, right)) {
                // String(N) := Char(M) if M in [N..MAX_STRING_LEN)
                (Some(n), Some(m)) => n >= m && m < MAX_STRING_LEN,
                // String(*) := Char(N) if M < MAX_STRING_LEN
                (None, Some(m)) => m < MAX_STRING_LEN,
                // String(N) := Char(*) [runtime checked]
                (Some(_n), None) => true,
                // String(*) := Char(*) [runtime checked]
                (None, None) => true,
            }
        }

        // Not assignable otherwise
        _ => false,
    };

    Some(is_assignable)
}

/// Gets the type produced by the given binary operation
pub fn check_binary_op<T: ?Sized + db::TypeDatabase>(
    db: &T,
    left: TypeId,
    op: expr::BinaryOp,
    right: TypeId,
) -> Result<TypeId, InvalidBinaryOp> {
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

    fn check_arithmetic_operands<T: ?Sized + db::TypeDatabase>(
        db: &T,
        left: &TypeKind,
        right: &TypeKind,
    ) -> Option<TypeId> {
        match (left, right) {
            // Pass through integer inference
            (TypeKind::Integer, TypeKind::Integer) => Some(db.mk_integer()),

            // Normal operands
            (operand, TypeKind::Real(_)) | (TypeKind::Real(_), operand) if operand.is_number() => {
                Some(db.mk_real(RealSize::Real))
            }
            (operand, TypeKind::Int(_)) | (TypeKind::Int(_), operand) if operand.is_integer() => {
                Some(db.mk_int(IntSize::Int))
            }
            (operand, TypeKind::Nat(_)) | (TypeKind::Nat(_), operand) if operand.is_nat() => {
                Some(db.mk_nat(NatSize::Nat))
            }
            _ => None,
        }
    }

    fn check_bitwise_operands<T: ?Sized + db::TypeDatabase>(
        db: &T,
        left: &TypeKind,
        right: &TypeKind,
    ) -> Option<TypeId> {
        // Normal operands
        // Integer inference is not passed through

        if left.is_integer() && right.is_integer() {
            Some(db.mk_nat(NatSize::Nat))
        } else {
            None
        }
    }

    fn create_binary_type_error(op: expr::BinaryOp) -> Result<TypeId, InvalidBinaryOp> {
        Err(InvalidBinaryOp {
            op,
            unsupported: false,
        })
    }

    fn create_unsupported_binary_op(op: expr::BinaryOp) -> Result<TypeId, InvalidBinaryOp> {
        Err(InvalidBinaryOp {
            op,
            unsupported: true,
        })
    }

    let left = left.in_db(db);
    let right = right.in_db(db);

    let (left_ty, right_ty) = (left.peel_ref(), right.peel_ref());
    let (lhs_kind, rhs_kind) = (&*left_ty.kind(), &*right_ty.kind());

    // Short circuit for error types
    // Don't duplicate errors
    if lhs_kind.is_error() || rhs_kind.is_error() {
        return Ok(db.mk_error());
    }

    match op {
        // Arithmetic operators
        expr::BinaryOp::Add => {
            // Operations:
            // x String concatenation (charseq, charseq => charseq)
            // x Set union (set, set => set)
            // - Addition (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Addition
                Ok(result_ty)
            } else {
                // Type error
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Sub => {
            // Operations:
            // x Set difference (set, set => set)
            // - Subtraction (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Subtraction
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Mul => {
            // Operations:
            // x Set intersection (set, set => set)
            // - Multiplication (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Multiplication
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Div => {
            // Operations:
            // - Integer division (number, number => integer)

            match (lhs_kind, rhs_kind) {
                // Pass through type inference
                (TypeKind::Integer, TypeKind::Integer) => Ok(db.mk_integer()),
                (operand, TypeKind::Nat(_)) | (TypeKind::Nat(_), operand) if operand.is_nat() => {
                    Ok(db.mk_nat(NatSize::Nat))
                }
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => Ok(db.mk_int(IntSize::Int)),
                _ => create_binary_type_error(op),
            }
        }
        expr::BinaryOp::RealDiv => {
            // Operations:
            // - Floating point division (number, number => real)

            if lhs_kind.is_number() && rhs_kind.is_number() {
                Ok(db.mk_real(RealSize::Real))
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Mod => {
            // Operations:
            // - Modulo (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Modulo
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Rem => {
            // Operations:
            // - Remainder (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Remainder
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Exp => {
            // Operations:
            // - Exponentiation (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Exponentiation
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        // Bitwise operators (integer, integer => nat)
        // + Logical operators (boolean, boolean => boolean)
        expr::BinaryOp::And => {
            // Operations:
            // - Bitwise And (integer, integer => nat)
            // - Logical And (boolean, boolean => boolean)

            if let Some(result_ty) = check_bitwise_operands(db, lhs_kind, rhs_kind) {
                // Bitwise And
                Ok(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (&lhs_kind, &rhs_kind) {
                // Logical And
                Ok(db.mk_boolean())
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Or => {
            // Operations:
            // - Bitwise Or (integer, integer => nat)
            // - Logical Or (boolean, boolean => boolean)

            if let Some(result_ty) = check_bitwise_operands(db, lhs_kind, rhs_kind) {
                // Bitwise Or
                Ok(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (&lhs_kind, &rhs_kind) {
                // Logical Or
                Ok(db.mk_boolean())
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Xor => {
            // Operations:
            // - Bitwise Xor (integer, integer => nat)
            // - Logical Xor (boolean, boolean => boolean)

            if let Some(result_ty) = check_bitwise_operands(db, lhs_kind, rhs_kind) {
                // Bitwise Xor
                Ok(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (&lhs_kind, &rhs_kind) {
                // Logical Xor
                Ok(db.mk_boolean())
            } else {
                create_binary_type_error(op)
            }
        }
        // Pure bitwise operators
        expr::BinaryOp::Shl => {
            // Operations:
            // - Bitwise Shl (integer, integer => nat)

            if let Some(result_ty) = check_bitwise_operands(db, lhs_kind, rhs_kind) {
                // Bitwise Shl
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        expr::BinaryOp::Shr => {
            // Operations:
            // - Bitwise Shr (integer, integer => nat)

            if let Some(result_ty) = check_bitwise_operands(db, lhs_kind, rhs_kind) {
                // Bitwise Shr
                Ok(result_ty)
            } else {
                create_binary_type_error(op)
            }
        }
        // Pure logical operator
        expr::BinaryOp::Imply => {
            // Operations:
            // - Imply (boolean, boolean => boolean)

            if let (TypeKind::Boolean, TypeKind::Boolean) = (&lhs_kind, &rhs_kind) {
                // Logical Xor
                Ok(db.mk_boolean())
            } else {
                create_binary_type_error(op)
            }
        }
        // Comparison (a, b => boolean where a, b: Comparable)
        expr::BinaryOp::Less => create_unsupported_binary_op(op),
        expr::BinaryOp::LessEq => create_unsupported_binary_op(op),
        expr::BinaryOp::Greater => create_unsupported_binary_op(op),
        expr::BinaryOp::GreaterEq => create_unsupported_binary_op(op),
        expr::BinaryOp::Equal => create_unsupported_binary_op(op),
        expr::BinaryOp::NotEqual => create_unsupported_binary_op(op),
        // Set membership tests (set(a), a => boolean)
        expr::BinaryOp::In => create_unsupported_binary_op(op),
        expr::BinaryOp::NotIn => create_unsupported_binary_op(op),
    }
}

pub fn report_invalid_bin_op(err: InvalidBinaryOp, op_span: Span, reporter: &mut MessageSink) {
    let InvalidBinaryOp {
        op, unsupported, ..
    } = err;
    let op_name = match op {
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

    if unsupported {
        reporter.error(
            "unsupported operation",
            "operation is not type-checked yet",
            op_span,
        );
        return;
    }

    let msg = reporter.error_detailed(&format!("incompatible types for {}", op_name), op_span);
    let msg = match op {
        // Arithmetic operators
        expr::BinaryOp::Add => msg.with_info("operands must both be numbers, strings, or sets"),
        expr::BinaryOp::Sub | expr::BinaryOp::Mul => {
            msg.with_info("operands must both be numbers or sets")
        }
        expr::BinaryOp::Div
        | expr::BinaryOp::RealDiv
        | expr::BinaryOp::Mod
        | expr::BinaryOp::Rem
        | expr::BinaryOp::Exp => msg.with_info("operands must both be numbers"),
        // Bitwise operators (integer, integer => nat)
        // + Logical operators (boolean, boolean => boolean)
        expr::BinaryOp::And | expr::BinaryOp::Or | expr::BinaryOp::Xor => {
            msg.with_info("operands must both be integers or booleans")
        }
        // Pure bitwise operators
        expr::BinaryOp::Shl | expr::BinaryOp::Shr => {
            msg.with_info("operands must both be integers")
        }
        // Pure logical operator
        expr::BinaryOp::Imply => msg.with_info("operands must both be booleans"),
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

/// Implemented as a query in TypeDatabase
pub fn check_unary_op<T: ?Sized + db::TypeDatabase>(
    db: &T,
    op: expr::UnaryOp,
    right: TypeId,
) -> Result<TypeId, InvalidUnaryOp> {
    use crate::ty::{IntSize, NatSize, RealSize};

    fn create_unary_type_error(op: expr::UnaryOp) -> Result<TypeId, InvalidUnaryOp> {
        Err(InvalidUnaryOp { op })
    }

    let right_ty = right.in_db(db).peel_ref();
    let rhs_kind = &*right_ty.kind();

    // Short circuit for error types
    // Don't duplicate errors
    if rhs_kind.is_error() {
        return Ok(db.mk_error());
    }

    match op {
        expr::UnaryOp::Not => {
            if rhs_kind.is_integer() {
                // Bitwise Not
                Ok(db.mk_nat(NatSize::Nat))
            } else if let TypeKind::Boolean = &rhs_kind {
                // Logical Not
                Ok(db.mk_boolean())
            } else {
                create_unary_type_error(op)
            }
        }
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            match rhs_kind {
                // Pass through integer inference
                TypeKind::Integer => Ok(db.mk_integer()),

                // Normal operands
                TypeKind::Real(_) => Ok(db.mk_real(RealSize::Real)),
                TypeKind::Int(_) => Ok(db.mk_int(IntSize::Int)),
                TypeKind::Nat(_) => Ok(db.mk_nat(NatSize::Nat)),
                _ => create_unary_type_error(op),
            }
        }
    }
}

pub fn report_invalid_unary_op(err: InvalidUnaryOp, op_span: Span, reporter: &mut MessageSink) {
    let InvalidUnaryOp { op, .. } = err;
    let op_name = match op {
        expr::UnaryOp::Not => "`not`",
        expr::UnaryOp::Identity => "unary `+`",
        expr::UnaryOp::Negate => "unary `-`",
    };

    let msg = reporter.error_detailed(&format!("incompatible types for {}", op_name), op_span);
    let msg = match op {
        expr::UnaryOp::Not => msg.with_info("operand must be an integer or boolean"),
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            msg.with_info("operand must be a number")
        }
    };
    msg.finish();
}
