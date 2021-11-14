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

    pub fn is_boolean(&self) -> bool {
        matches!(self, TypeKind::Boolean)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, TypeKind::Error)
    }

    /// charseq types includes `String`, `StringN`, `Char`, and `CharN` types
    pub fn is_charseq(&self) -> bool {
        matches!(
            self,
            TypeKind::String | TypeKind::StringN(_) | TypeKind::Char | TypeKind::CharN(_)
        )
    }

    /// index types includes all integer types, `Char`, `Boolean`, `Enum`, and `Range` types
    pub fn is_index(&self) -> bool {
        self.is_integer() || matches!(self, TypeKind::Char | TypeKind::Boolean)
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

// TODO: Document type equivalence
// steal from the old compiler
pub fn is_equivalent<T: db::ConstEval + ?Sized>(db: &T, left: TypeId, right: TypeId) -> bool {
    let left = left.in_db(db).peel_ref();
    let right = right.in_db(db).peel_ref();

    let (left_kind, right_kind) = (left.kind(), right.kind());

    // Quick bailouts
    if (left.id() == right.id()) || (left_kind.is_error() || right_kind.is_error()) {
        // Same type id implies trivial equivalency
        // Error types get treated as equivalent to everything
        return true;
    }

    match (&*left_kind, &*right_kind) {
        (TypeKind::Int(left_size), TypeKind::Int(right_size)) => left_size == right_size,
        (TypeKind::Nat(left_size), TypeKind::Nat(right_size)) => left_size == right_size,
        (TypeKind::Real(left_size), TypeKind::Real(right_size)) => left_size == right_size,
        (TypeKind::Integer, other) | (other, TypeKind::Integer) => other.is_number(),
        (TypeKind::String, TypeKind::String) => true,
        (TypeKind::Char, TypeKind::Char) => true,
        (TypeKind::CharN(left_sz), TypeKind::CharN(right_sz))
        | (TypeKind::StringN(left_sz), TypeKind::StringN(right_sz)) => {
            let left_sz = left_sz.fixed_len(db, Default::default());
            let right_sz = right_sz.fixed_len(db, Default::default());

            let (left_sz, right_sz) = if let (Ok(left_sz), Ok(right_sz)) = (left_sz, right_sz) {
                (left_sz, right_sz)
            } else {
                return false;
            };

            if let Some((left_sz, right_sz)) = left_sz.zip(right_sz) {
                left_sz.cmp(right_sz).is_eq()
            } else {
                // `charseq(*)` treated as equivalent to either type
                true
            }
        }

        (TypeKind::Ref(_, _), _) | (_, TypeKind::Ref(_, _)) => unreachable!(),
        _ => false,
    }
}

// TODO: Document type assignability
// steal from the old compiler
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

/// Type for associated mismatch binary operand types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidBinaryOp {
    left: TypeId,
    op: expr::BinaryOp,
    right: TypeId,
    unsupported: bool,
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

    fn check_charseq_operands<T: ?Sized + db::TypeDatabase>(
        db: &T,
        left: &TypeKind,
        right: &TypeKind,
    ) -> Option<TypeId> {
        // For now, don't deal with the following special cases:
        // - char, char
        // - char(N), char / char, char(N)
        // - char(N), char(M)
        //
        // These require building compile time expressions not from the HIR, which we don't support yet
        // TODO: Handle special cases once we can lazy evaluate constant expressions
        //
        // Testing note: special cases with char(N) can go over the max char size, make sure we check for that

        if left.is_charseq() && right.is_charseq() {
            // The rest of the cases fall back on producing string
            Some(db.mk_string())
        } else {
            None
        }
    }

    let left = left.in_db(db).peel_ref();
    let right = right.in_db(db).peel_ref();
    let (lhs_kind, rhs_kind) = (&*left.kind(), &*right.kind());

    // Use the peeled versions of the types for reporting
    let mk_type_error = || {
        Err(InvalidBinaryOp {
            left: left.id,
            op,
            right: right.id,
            unsupported: false,
        })
    };

    let unsupported_op = || {
        Err(InvalidBinaryOp {
            left: left.id,
            op,
            right: right.id,
            unsupported: true,
        })
    };

    // Short circuit for error types
    // Don't duplicate errors
    if lhs_kind.is_error() || rhs_kind.is_error() {
        return Ok(db.mk_error());
    }

    match op {
        // Arithmetic operators
        expr::BinaryOp::Add => {
            // Operations:
            // - String concatenation (charseq, charseq => charseq)
            // x Set union (set, set => set)
            // - Addition (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Addition
                Ok(result_ty)
            } else if let Some(result_ty) = check_charseq_operands(db, lhs_kind, rhs_kind) {
                // String concatenation
                Ok(result_ty)
            } else {
                // Type error
                mk_type_error()
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
                mk_type_error()
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
                mk_type_error()
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
                _ => mk_type_error(),
            }
        }
        expr::BinaryOp::RealDiv => {
            // Operations:
            // - Floating point division (number, number => real)

            if lhs_kind.is_number() && rhs_kind.is_number() {
                Ok(db.mk_real(RealSize::Real))
            } else {
                mk_type_error()
            }
        }
        expr::BinaryOp::Mod => {
            // Operations:
            // - Modulo (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Modulo
                Ok(result_ty)
            } else {
                mk_type_error()
            }
        }
        expr::BinaryOp::Rem => {
            // Operations:
            // - Remainder (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Remainder
                Ok(result_ty)
            } else {
                mk_type_error()
            }
        }
        expr::BinaryOp::Exp => {
            // Operations:
            // - Exponentiation (number, number => number)

            if let Some(result_ty) = check_arithmetic_operands(db, lhs_kind, rhs_kind) {
                // Exponentiation
                Ok(result_ty)
            } else {
                mk_type_error()
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
                mk_type_error()
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
                mk_type_error()
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
                mk_type_error()
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
                mk_type_error()
            }
        }
        expr::BinaryOp::Shr => {
            // Operations:
            // - Bitwise Shr (integer, integer => nat)

            if let Some(result_ty) = check_bitwise_operands(db, lhs_kind, rhs_kind) {
                // Bitwise Shr
                Ok(result_ty)
            } else {
                mk_type_error()
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
                mk_type_error()
            }
        }
        // Comparison (a, b => boolean where a, b: Comparable)
        expr::BinaryOp::Less => unsupported_op(),
        expr::BinaryOp::LessEq => unsupported_op(),
        expr::BinaryOp::Greater => unsupported_op(),
        expr::BinaryOp::GreaterEq => unsupported_op(),
        expr::BinaryOp::Equal => unsupported_op(),
        expr::BinaryOp::NotEqual => unsupported_op(),
        // Set membership tests (set(a), a => boolean)
        expr::BinaryOp::In => unsupported_op(),
        expr::BinaryOp::NotIn => unsupported_op(),
    }
}

pub fn report_invalid_bin_op<'db, DB>(
    db: &'db DB,
    err: InvalidBinaryOp,
    left_span: Span,
    op_span: Span,
    right_span: Span,
    reporter: &mut MessageSink,
) where
    DB: ?Sized + db::ConstEval + 'db,
{
    let InvalidBinaryOp {
        left: left_ty,
        op,
        right: right_ty,
        unsupported,
        ..
    } = err;

    if unsupported {
        reporter.error(
            "unsupported operation",
            "operation is not type-checked yet",
            op_span,
        );
        return;
    }

    let left_ty = left_ty.in_db(db);
    let right_ty = right_ty.in_db(db);
    // Try logical operation if either is a boolean
    let is_logical = left_ty.kind().is_boolean() || right_ty.kind().is_boolean();
    // Try string concat operation if either is a charseq
    let is_string_concat = left_ty.kind().is_charseq() || right_ty.kind().is_charseq();

    let op_name = match op {
        expr::BinaryOp::Add if is_string_concat => "string concatenation",
        expr::BinaryOp::Add => "addition",
        expr::BinaryOp::Sub => "subtraction",
        expr::BinaryOp::Mul => "multiplication",
        expr::BinaryOp::Div => "integer division",
        expr::BinaryOp::RealDiv => "real division",
        expr::BinaryOp::Mod => "modulus",
        expr::BinaryOp::Rem => "remainder",
        expr::BinaryOp::Exp => "exponentiation",
        expr::BinaryOp::And if is_logical => "logical `and`",
        expr::BinaryOp::Or if is_logical => "logical `or`",
        expr::BinaryOp::Xor if is_logical => "logical `xor`",
        expr::BinaryOp::And => "bitwise `and`",
        expr::BinaryOp::Or => "bitwise `or`",
        expr::BinaryOp::Xor => "bitwise `xor`",
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
    let verb_phrase = match op {
        expr::BinaryOp::Add if is_string_concat => "concatenated to",
        expr::BinaryOp::Add => "added to",
        expr::BinaryOp::Sub => "subtracted by",
        expr::BinaryOp::Mul => "multiplied by",
        expr::BinaryOp::Div | expr::BinaryOp::RealDiv => "divided by",
        expr::BinaryOp::Mod => "`mod`'d by",
        expr::BinaryOp::Rem => "`rem`'d by",
        expr::BinaryOp::Exp => "exponentiated by",
        expr::BinaryOp::And if is_logical => "`and`ed logically by",
        expr::BinaryOp::Or if is_logical => "`or`ed by",
        expr::BinaryOp::Xor if is_logical => "`xor`ed logically by",
        expr::BinaryOp::And => "`and`ed bitwise by",
        expr::BinaryOp::Or => "`or`ed bitwise by",
        expr::BinaryOp::Xor => "`xor`ed bitwise by",
        expr::BinaryOp::Shl => "shifted left by",
        expr::BinaryOp::Shr => "shifted right by",
        expr::BinaryOp::Less
        | expr::BinaryOp::LessEq
        | expr::BinaryOp::Greater
        | expr::BinaryOp::GreaterEq
        | expr::BinaryOp::Equal
        | expr::BinaryOp::NotEqual
        | expr::BinaryOp::In
        | expr::BinaryOp::NotIn
        | expr::BinaryOp::Imply => "compared to",
    };

    let msg = reporter
        .error_detailed(&format!("mismatched types for {}", op_name), op_span)
        .with_note(
            &format!("this is of type `{left}`", left = left_ty),
            left_span,
        )
        .with_note(
            &format!("this is of type `{right}`", right = right_ty),
            right_span,
        )
        .with_error(
            &format!(
                "`{left}` cannot be {verb_phrase} `{right}`",
                left = left_ty,
                verb_phrase = verb_phrase,
                right = right_ty
            ),
            op_span,
        );

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

/// Type for associated mismatch unary operand types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnaryOp {
    op: expr::UnaryOp,
    right: TypeId,
}

/// Implemented as a query in TypeDatabase
pub fn check_unary_op<T: ?Sized + db::TypeDatabase>(
    db: &T,
    op: expr::UnaryOp,
    right: TypeId,
) -> Result<TypeId, InvalidUnaryOp> {
    use crate::ty::{IntSize, NatSize, RealSize};

    let right = right.in_db(db).peel_ref();
    let rhs_kind = &*right.kind();

    // Use the peeled version of the type
    let type_error = || {
        Err(InvalidUnaryOp {
            op,
            right: right.id,
        })
    };

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
                type_error()
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
                _ => type_error(),
            }
        }
    }
}

pub fn report_invalid_unary_op<'db, DB>(
    db: &'db DB,
    err: InvalidUnaryOp,
    op_span: Span,
    right_span: Span,
    reporter: &mut MessageSink,
) where
    DB: ?Sized + db::ConstEval + 'db,
{
    let InvalidUnaryOp {
        op,
        right: right_ty,
        ..
    } = err;
    let right_ty = right_ty.in_db(db);
    let is_bitwise = right_ty.kind().is_integer();

    let op_name = match op {
        expr::UnaryOp::Not if is_bitwise => "bitwise `not`",
        expr::UnaryOp::Not => "logical `not`",
        expr::UnaryOp::Identity => "unary `+`",
        expr::UnaryOp::Negate => "unary `-`",
    };
    let verb_phrase = match op {
        expr::UnaryOp::Not if is_bitwise => "bitwise `not`",
        expr::UnaryOp::Not => "logical `not`",
        expr::UnaryOp::Identity => "identity",
        expr::UnaryOp::Negate => "negation",
    };

    let msg = reporter
        .error_detailed(&format!("mismatched types for {}", op_name), op_span)
        .with_note(
            &format!("this is of type `{right}`", right = right_ty),
            right_span,
        )
        .with_error(
            &format!(
                "cannot apply {verb_phrase} to `{right}`",
                verb_phrase = verb_phrase,
                right = right_ty
            ),
            op_span,
        );

    let msg = match op {
        expr::UnaryOp::Not => msg.with_info("operand must be an integer or boolean"),
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            msg.with_info("operand must be a number")
        }
    };
    msg.finish();
}
