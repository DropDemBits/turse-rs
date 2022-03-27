//! Rules for type interactions
use toc_hir::library::LibraryId;
use toc_hir::symbol::NotBinding;
use toc_hir::{body, expr};
use toc_reporting::MessageSink;
use toc_span::Span;

use crate::{
    db,
    ty::{self, NotFixedLen, SeqSize, TypeId, TypeKind},
};

use super::TyRef;

impl TypeKind {
    // ???: Do we want to move all of these into `TyRef`?

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

    pub fn is_forward(&self) -> bool {
        matches!(self, TypeKind::Forward)
    }

    pub fn is_alias(&self) -> bool {
        matches!(self, TypeKind::Alias(_, _))
    }

    pub fn is_set(&self) -> bool {
        matches!(self, TypeKind::Set(..))
    }

    /// charseq types includes `String`, `StringN`, `Char`, and `CharN` types
    pub fn is_charseq(&self) -> bool {
        matches!(
            self,
            TypeKind::String | TypeKind::StringN(_) | TypeKind::Char | TypeKind::CharN(_)
        )
    }

    /// comparable charseq types includes `String`, `StringN`, `Char`, and `CharN` (except `CharN(*)`) types
    pub fn is_cmp_charseq(&self) -> bool {
        matches!(
            self,
            TypeKind::String
                | TypeKind::StringN(_)
                | TypeKind::Char
                | TypeKind::CharN(SeqSize::Fixed(_))
        )
    }

    /// sized charseq types includes `String`, `StringN` (fixed-only), and `CharN` (fixed-only) types
    pub fn is_sized_charseq(&self) -> bool {
        matches!(
            self,
            TypeKind::String
                | TypeKind::StringN(SeqSize::Fixed(_))
                | TypeKind::CharN(SeqSize::Fixed(_))
        )
    }

    // char-like includes `Char` and `CharN` (any kind)
    pub fn is_char_like(&self) -> bool {
        matches!(self, TypeKind::Char | TypeKind::CharN(_))
    }

    // string-like includes `String` and `StringN` (any kind)
    pub fn is_string_like(&self) -> bool {
        matches!(self, TypeKind::String | TypeKind::StringN(_))
    }

    /// index types includes all integer types, `Char`, `Boolean`, `Enum`, and `Range` types
    pub fn is_index(&self) -> bool {
        self.is_integer() || matches!(self, TypeKind::Char | TypeKind::Boolean)
    }

    /// If the type can be printed in a `put` stmt
    ///
    /// Can be one of the following:
    /// - Int
    /// - Nat
    /// - Real
    /// - Char
    /// - Char(N)
    /// - String
    /// - String(N)
    /// - Boolean
    /// - Enum
    pub fn is_printable(&self) -> bool {
        // missing: Enum
        matches!(
            self,
            TypeKind::Boolean
                | TypeKind::Int(_)
                | TypeKind::Nat(_)
                | TypeKind::Real(_)
                | TypeKind::Integer
                | TypeKind::Char
                | TypeKind::CharN(_)
                | TypeKind::String
                | TypeKind::StringN(_)
        )
    }

    /// scalar types are types representing only 1 value
    pub fn is_scalar(&self) -> bool {
        match self {
            TypeKind::Error
            | TypeKind::Boolean
            | TypeKind::Int(_)
            | TypeKind::Nat(_)
            | TypeKind::Real(_)
            | TypeKind::Integer
            | TypeKind::Char
            | TypeKind::Subprogram(..) => true,
            // Missing scalars:
            // - subrange
            // - pointer
            // - enum
            TypeKind::String | TypeKind::CharN(_) | TypeKind::StringN(_) => {
                // Aggregate types of characters
                false
            }
            TypeKind::Alias(_, _) => {
                // This should be peeled first, but it's okay to conservatively treat it as
                // not one
                false
            }
            TypeKind::Forward => {
                // Forward types are never scalars, since they never represent any type
                false
            }
            TypeKind::Set(..) => {
                // Variable-sized sets
                false
            }
            TypeKind::Void => false,
        }
    }

    pub fn is_sized_string(&self) -> bool {
        matches!(
            self,
            TypeKind::String | TypeKind::StringN(SeqSize::Fixed(_))
        )
    }
}

// Conversions of type
impl<'db, DB> TyRef<'db, DB>
where
    DB: db::TypeDatabase + ?Sized + 'db,
{
    /// Returns the type id pointed to by an alias, or itself if it's not an alias type.
    /// This peels through all aliases, since they never point to other aliases.
    ///
    /// # Example
    ///
    /// ```text
    /// Boolean -> Boolean
    /// Alias(Boolean) -> Boolean
    /// Alias(Alias(Boolean)) -> ! (never happens)
    /// ```
    pub fn peel_aliases(self) -> Self {
        match self.kind() {
            TypeKind::Alias(_, to_ty) => {
                let ty = to_ty.in_db(self.db);
                assert!(!ty.kind().is_alias());
                ty
            }
            _ => self,
        }
    }

    /// Transforms the type into its base representation.
    ///
    /// Turns ranges into its common type, and peels aliases.
    pub fn to_base_type(self) -> Self {
        // TODO: Range -> base type
        self.peel_aliases()
    }
}

// TODO: Document type equivalence
// steal from the old compiler
pub fn is_equivalent<T: db::ConstEval + ?Sized>(db: &T, left: TypeId, right: TypeId) -> bool {
    let left = left.in_db(db).to_base_type();
    let right = right.in_db(db).to_base_type();

    // Quick bailout
    if left.id() == right.id() {
        // Same type id implies trivial equivalency
        return true;
    }

    match (left.kind(), right.kind()) {
        // Error types get treated as equivalent to everything
        (TypeKind::Error, _) | (_, TypeKind::Error) => true,

        // Fundamental numeric types are equivalent if they are the exact same size & semantics
        (TypeKind::Int(left_size), TypeKind::Int(right_size)) => left_size == right_size,
        (TypeKind::Nat(left_size), TypeKind::Nat(right_size)) => left_size == right_size,
        (TypeKind::Real(left_size), TypeKind::Real(right_size)) => left_size == right_size,

        // Integer is treated as equivalent to the other numeric types
        (TypeKind::Integer, other) | (other, TypeKind::Integer) => other.is_number(),

        (TypeKind::String, TypeKind::String) => true,
        (TypeKind::Char, TypeKind::Char) => true,

        // Sized charseqs are equivalent to each other if they have the same size
        // Any-sized charseqs are not equivalent to anything
        (TypeKind::CharN(left_sz), TypeKind::CharN(right_sz))
        | (TypeKind::StringN(left_sz), TypeKind::StringN(right_sz)) => {
            let left_sz = left_sz.fixed_len(db, Default::default());
            let right_sz = right_sz.fixed_len(db, Default::default());

            match (&left_sz, &right_sz) {
                // `charseq(*)` treated as not equivalent to either type
                (Err(NotFixedLen::AnySize), _) | (_, Err(NotFixedLen::AnySize)) => return false,
                _ => (),
            }

            let (left_sz, right_sz) = if let Some(sizes) = left_sz.ok().zip(right_sz.ok()) {
                sizes
            } else {
                // Invalid evaluations treated as equivalent types
                return true;
            };

            // sized charseqs are treated as equivalent types if the sizes are equal
            left_sz.cmp(right_sz).is_eq()
        }
        // Set types are equivalent if they come from the same definition
        (TypeKind::Set(left_def, _), TypeKind::Set(right_def, _)) => left_def == right_def,
        // Subprograms are equivalent if:
        // - they are the same [`SubprogramKind`]
        // - the formal lists are of the same length
        // - the formal types are equivalent
        // - the formal pass-by kinds are equivalent
        // - the result types are equivalent
        (
            TypeKind::Subprogram(left_kind, left_params, left_res),
            TypeKind::Subprogram(right_kind, right_params, right_res),
        ) => {
            let left_params = left_params.as_ref();
            let right_params = right_params.as_ref();

            if left_kind != right_kind {
                // Not the same subprogram kind
                return false;
            }
            if left_params.map_or(0, |params| params.len())
                != right_params.map_or(0, |params| params.len())
            {
                // Formal list lengths aren't the same
                return false;
            }
            if !is_equivalent(db, *left_res, *right_res) {
                // Result types aren't the same
                return false;
            }

            if let Some((left_params, right_params)) = left_params.zip(right_params) {
                if left_params.is_empty() && right_params.is_empty() {
                    // Both are parameterless
                    return true;
                }

                // Formals must have the same `PassBy` and equivalent types.
                //
                // Formals do not need to have the same `register` attr presence, since that only affects
                // behaviour inside of the body.
                // Formals do not need to have the same `cheat` attr presence, since
                // - passing non-`cheat` to `cheat` formals already guarantees that they're equivalent types
                // - passing `cheat` to non-`cheat` formals has the guarantee that they will be reinterpreted
                //   into equivalent types.
                left_params
                    .iter()
                    .zip(right_params.iter())
                    .all(|(left, right)| {
                        left.pass_by == right.pass_by
                            && is_equivalent(db, left.param_ty, right.param_ty)
                    })
            } else {
                // Either one is bare, and we've checked that the other has no params
                debug_assert_eq!(
                    left_params.map_or(0, |params| params.len()),
                    right_params.map_or(0, |params| params.len())
                );

                true
            }
        }
        // `void` is equivalent to itself
        (TypeKind::Void, TypeKind::Void) => true,
        _ => false,
    }
}

/// Tests if `rhs` can be implicitly coerced into the `lhs` parameter type.
///
/// Mostly equivalent to [`is_equivalent`], except that it accepts coercion
/// into `char(*)` and `string(*)`, with this property also being transitive
/// over arrays and ranges.
pub fn is_coercible_into_param<T: ?Sized + db::ConstEval>(
    db: &T,
    lhs: TypeId,
    rhs: TypeId,
) -> bool {
    let left = lhs.in_db(db).to_base_type();
    let right = rhs.in_db(db).to_base_type();

    // Equivalent types imply trivial coercion
    if is_equivalent(db, left.id(), right.id()) {
        return true;
    }

    // FIXME: Be transitive over arrays and ranges
    match (left.kind(), right.kind()) {
        (TypeKind::CharN(SeqSize::Any), _) => right.kind().is_char_like(),
        (TypeKind::StringN(SeqSize::Any), _) => right.kind().is_string_like(),
        _ => false,
    }
}

/// Tests if `rhs` can be implicitly coerced into the `lhs` type.
///
/// Implicit coercing rules are essentially the same as the assignability rules,
/// but without the requirement of `lhs` being a ref type.
/// See [`is_assignable`] for specifics on these rules.
///
/// This function is not symmetric, use [`is_either_coercible`] if that property
/// is desired.
pub fn is_coercible_into<T: ?Sized + db::ConstEval>(db: &T, lhs: TypeId, rhs: TypeId) -> bool {
    // Current coercion rules:
    // All equivalence rules, plus
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

    let left = lhs.in_db(db).to_base_type();
    let right = rhs.in_db(db).to_base_type();

    /// Gets a sequence size suitable for coercion checking.
    /// All errors (overflow, other const error) and dynamic sizes are ignored.
    fn seq_size<T: db::ConstEval + ?Sized>(db: &T, seq_size: &SeqSize) -> Option<u32> {
        match seq_size.fixed_len(db, Default::default()) {
            Ok(v) => {
                // if overflow or zero, it's silently ignored (reported in typeck)
                v.into_u32().filter(|v| *v != 0)
            }
            // either dynamic (runtime checked), or an eval error
            // silently hide eval error, gets reported in typeck
            Err(_) => None,
        }
    }

    // Equivalent types imply trivial coercion
    if is_equivalent(db, left.id(), right.id()) {
        return true;
    }

    match (left.kind(), right.kind()) {
        // Integer types can be coerced to each other
        (TypeKind::Nat(_) | TypeKind::Int(_), rhs) if rhs.is_integer() => true,

        // All numeric types are coercible into a real
        (TypeKind::Real(_), rhs) if rhs.is_number() => true,

        // Char rules:
        // - String(1), Char(1), and Char are coercible into Char
        // - String is coercible into Char, but checked at runtime
        // - String(*) and Char(*) is coercible into Char, but checked at runtime
        (TypeKind::Char, TypeKind::Char | TypeKind::String) => true,
        (TypeKind::Char, TypeKind::StringN(size)) | (TypeKind::Char, TypeKind::CharN(size)) => {
            seq_size(db, size).map(|n| n == 1).unwrap_or(true)
        }

        // Char(N) rules:
        // - Char is coercible into Char(N) if N = 1
        // - Char(M) is coercible into Char(N) if N = M
        // - String(M) is coercible into Char(N) if N <= M, but double checked at runtime
        // - String is coercible into Char(N), but checked at runtime
        // - All of these are coercible into Char(*), but checked at runtime
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
        // - Char, String(N), String(*) and String are coercible into String
        // - Char(N) is coercible into String if N < `MAX_STRING_LEN`
        // - Char(*) is coercible into String, but is checked at runtime

        // String(N) rules:
        // - Char is coercible into String(N) and String(*)
        // - String is coercible into String(N) and String(*), but checked at runtime
        // - String(M) is coercible into String(N) and String(*), but String(N) is double checked at runtime
        // - Char(M) is coercible into String(N) and String(*) if n >= m and m < `MAX_STRING_LEN`

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
            Some(n) => n < ty::MAX_STRING_LEN,
            // String := Char(*) [runtime checked]
            None => true,
        },
        (TypeKind::StringN(left), TypeKind::CharN(right)) => {
            match (seq_size(db, left), seq_size(db, right)) {
                // String(N) := Char(M) if M in [N..MAX_STRING_LEN)
                (Some(n), Some(m)) => n >= m && m < ty::MAX_STRING_LEN,
                // String(*) := Char(N) if M < MAX_STRING_LEN
                (None, Some(m)) => m < ty::MAX_STRING_LEN,
                // String(N) := Char(*) [runtime checked]
                (Some(_n), None) => true,
                // String(*) := Char(*) [runtime checked]
                (None, None) => true,
            }
        }

        // FIXME: Coercion rule for Opaque to base type

        // Not coercible otherwise
        _ => false,
    }
}

/// Tests if `rhs` can be implicitly coerced into the `lhs` type, or if the
/// swapped version is true.
///
/// This is the symmetric version of [`is_coercible_into`]
pub fn is_either_coercible<T: ?Sized + db::ConstEval>(db: &T, left: TypeId, right: TypeId) -> bool {
    is_coercible_into(db, left, right) || is_coercible_into(db, right, left)
}

// TODO: Document type assignability
// steal from the old compiler
/// Alias of [`is_coercible_into`]
pub fn is_assignable<T: ?Sized + db::ConstEval>(db: &T, left: TypeId, right: TypeId) -> bool {
    let left = left.in_db(db);
    let right = right.in_db(db);

    // Defer to the coercion rules
    is_coercible_into(db, left.id(), right.id())
}

/// Checks that the operands for the binary op are values, or types when appropriate
pub fn check_binary_op_values<T: ?Sized + db::TypeDatabase>(
    db: &T,
    library_id: LibraryId,
    body_id: body::BodyId,
    expr: &expr::Binary,
) -> Result<(), InvalidBinaryValues> {
    use crate::db::{NotValueErrExt, ValueKind};

    let left_src = (library_id, body_id, expr.lhs).into();
    let right_src = (library_id, body_id, expr.rhs).into();

    // Only comparison ops support referring to types, but only on classes
    let left_value = db.value_produced(left_src);
    let right_value = db.value_produced(right_src);

    let is_left_value = left_value.map(ValueKind::is_value).or_missing();
    let is_right_value = right_value.map(ValueKind::is_value).or_missing();

    if is_left_value && is_right_value {
        Ok(())
    } else {
        let library = db.library(library_id);

        Err(InvalidBinaryValues {
            left_info: (!is_left_value).then(|| {
                (
                    left_src,
                    library
                        .body(body_id)
                        .expr(expr.lhs)
                        .span
                        .lookup_in(&library.span_map),
                )
            }),
            right_info: (!is_right_value).then(|| {
                (
                    right_src,
                    library
                        .body(body_id)
                        .expr(expr.rhs)
                        .span
                        .lookup_in(&library.span_map),
                )
            }),
        })
    }
}

/// Checks that the operand for the unary op is a value
pub fn check_unary_op_values<T: ?Sized + db::TypeDatabase>(
    db: &T,
    library_id: LibraryId,
    body_id: body::BodyId,
    expr: &expr::Unary,
) -> Result<(), InvalidUnaryValue> {
    use crate::db::{NotValueErrExt, ValueKind};

    // All unary ops only support value operands
    let right_src = (library_id, body_id, expr.rhs).into();
    let right_value = db.value_produced(right_src);
    let is_right_value = right_value.map(ValueKind::is_value).or_missing();

    if is_right_value {
        Ok(())
    } else {
        let library = db.library(library_id);

        Err(InvalidUnaryValue {
            right_info: (
                right_src,
                library
                    .body(body_id)
                    .expr(expr.rhs)
                    .span
                    .lookup_in(&library.span_map),
            ),
        })
    }
}

/// Result of inferring a type from an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InferTy {
    /// This is the proper type for this operation, so no additional type
    /// checking needs to be done.
    Complete(TypeId),
    /// This is not quite the proper type for this operation, so we'll need
    /// to do additional type checking to verify if it's actually valid. In
    /// the mean time, using this type as the inference result is okay as
    /// will give the right semantics for most cases.
    Partial(TypeId),
    /// This is the error type for this operation, since the constraints of
    /// the operation were not met. No additional type checking needs to be
    /// done.
    Error(TypeId),
}

impl InferTy {
    /// Extracts the associated type from the inference result.
    /// Used when we only care about what type was inferred.
    pub fn extract_ty(self) -> TypeId {
        match self {
            InferTy::Complete(ty) | InferTy::Partial(ty) | InferTy::Error(ty) => ty,
        }
    }
}

/// Infers the correct type for the binary operation
///
/// # Returns
///
/// `OK(ty)` for the proper inferred type,
/// `Err(ty)` if the input types aren't appropriate for the operation
pub fn infer_binary_op<T: ?Sized + db::TypeDatabase>(
    db: &T,
    left: TypeId,
    op: expr::BinaryOp,
    right: TypeId,
) -> InferTy {
    // TODO: do full binexpr typechecks
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

    fn infer_arithmetic_operands<T: ?Sized + db::TypeDatabase>(
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

    fn infer_bitwise_operands<T: ?Sized + db::TypeDatabase>(
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

    fn infer_charseq_operands<T: ?Sized + db::TypeDatabase>(
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

    let left = left.in_db(db).to_base_type();
    let right = right.in_db(db).to_base_type();

    // Propagate error type as complete so that we don't duplicate the error
    if left.kind().is_error() || right.kind().is_error() {
        return InferTy::Complete(db.mk_error());
    }

    match op {
        // Arithmetic operators
        expr::BinaryOp::Add => {
            // Operations:
            // - String concatenation (charseq, charseq => charseq)
            // - Set union (set, set => set)
            // - Addition (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(), right.kind()) {
                // Addition
                InferTy::Complete(result_ty)
            } else if let Some(result_ty) = infer_charseq_operands(db, left.kind(), right.kind()) {
                // String concatenation
                InferTy::Complete(result_ty)
            } else if left.kind().is_set() && right.kind().is_set() {
                // Set union (depends on `is_equivalent`, but okay to infer from left)
                InferTy::Partial(left.id())
            } else {
                // Type error
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Sub => {
            // Operations:
            // - Set difference (set, set => set)
            // - Subtraction (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(), right.kind()) {
                // Subtraction
                InferTy::Complete(result_ty)
            } else if left.kind().is_set() && right.kind().is_set() {
                // Set difference (depends on `is_equivalent`, but okay to infer from left)
                InferTy::Partial(left.id())
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Mul => {
            // Operations:
            // - Set intersection (set, set => set)
            // - Multiplication (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(), right.kind()) {
                // Multiplication
                InferTy::Complete(result_ty)
            } else if left.kind().is_set() && right.kind().is_set() {
                // Set intersection (depends on `is_equivalent`, but okay to infer from left)
                InferTy::Partial(left.id())
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Div => {
            // Operations:
            // - Integer division (number, number => integer)

            match (left.kind(), right.kind()) {
                // Pass through type inference
                (TypeKind::Integer, TypeKind::Integer) => InferTy::Complete(db.mk_integer()),
                (operand, TypeKind::Nat(_)) | (TypeKind::Nat(_), operand) if operand.is_nat() => {
                    InferTy::Complete(db.mk_nat(NatSize::Nat))
                }
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => {
                    InferTy::Complete(db.mk_int(IntSize::Int))
                }
                _ => InferTy::Error(db.mk_error()),
            }
        }
        expr::BinaryOp::RealDiv => {
            // Operations:
            // - Floating point division (number, number => real)

            if left.kind().is_number() && right.kind().is_number() {
                InferTy::Complete(db.mk_real(RealSize::Real))
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Mod => {
            // Operations:
            // - Modulo (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(), right.kind()) {
                // Modulo
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Rem => {
            // Operations:
            // - Remainder (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(), right.kind()) {
                // Remainder
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Exp => {
            // Operations:
            // - Exponentiation (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(), right.kind()) {
                // Exponentiation
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        // Bitwise operators (integer, integer => nat)
        // + Logical operators (boolean, boolean => boolean)
        expr::BinaryOp::And => {
            // Operations:
            // - Bitwise And (integer, integer => nat)
            // - Logical And (boolean, boolean => boolean)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(), right.kind()) {
                // Bitwise And
                InferTy::Complete(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(), right.kind()) {
                // Logical And
                InferTy::Complete(db.mk_boolean())
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Or => {
            // Operations:
            // - Bitwise Or (integer, integer => nat)
            // - Logical Or (boolean, boolean => boolean)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(), right.kind()) {
                // Bitwise Or
                InferTy::Complete(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(), right.kind()) {
                // Logical Or
                InferTy::Complete(db.mk_boolean())
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Xor => {
            // Operations:
            // - Bitwise Xor (integer, integer => nat)
            // - Logical Xor (boolean, boolean => boolean)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(), right.kind()) {
                // Bitwise Xor
                InferTy::Complete(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(), right.kind()) {
                // Logical Xor
                InferTy::Complete(db.mk_boolean())
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        // Pure bitwise operators
        expr::BinaryOp::Shl => {
            // Operations:
            // - Bitwise Shl (integer, integer => nat)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(), right.kind()) {
                // Bitwise Shl
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::BinaryOp::Shr => {
            // Operations:
            // - Bitwise Shr (integer, integer => nat)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(), right.kind()) {
                // Bitwise Shr
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        // Pure logical operator
        expr::BinaryOp::Imply => {
            // Operations:
            // - Imply (boolean, boolean => boolean)

            if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(), right.kind()) {
                // Imply
                InferTy::Complete(db.mk_boolean())
            } else {
                InferTy::Error(db.mk_error())
            }
        }

        // Comparison (a, b => boolean where a, b: Comparable)
        // Comparison operations require a const eval context (since they depend on `is_equivalent`),
        // so we'll need to do further type checking
        expr::BinaryOp::Less
        | expr::BinaryOp::LessEq
        | expr::BinaryOp::Greater
        | expr::BinaryOp::GreaterEq
        | expr::BinaryOp::Equal
        | expr::BinaryOp::NotEqual => InferTy::Partial(db.mk_boolean()),
        // Set membership tests (set(a), a => boolean)
        // These ops are not implemented yet, but inferring them into a boolean type is ok
        expr::BinaryOp::In | expr::BinaryOp::NotIn => InferTy::Partial(db.mk_boolean()),
    }
}

/// Performs the full type checks for the given binary operation and operand types.
///
/// # Returns
///
/// `Ok(())` if the operation is valid for the given operand types, or `Err(err)`
/// with `err` containing information about the error
pub fn check_binary_op<T: ?Sized + db::ConstEval>(
    db: &T,
    left: TypeId,
    op: expr::BinaryOp,
    right: TypeId,
) -> Result<(), InvalidBinaryOp> {
    let left = left.in_db(db);
    let right = right.in_db(db);

    // Use the peeled versions of the types for reporting
    let mk_type_error = move || {
        Err(InvalidBinaryOp {
            left: left.id,
            op,
            right: right.id,
            unsupported: false,
        })
    };

    let unsupported_op = move || {
        Err(InvalidBinaryOp {
            left: left.id,
            op,
            right: right.id,
            unsupported: true,
        })
    };

    let left = left.to_base_type();
    let right = right.to_base_type();

    // Start from the inference code
    match infer_binary_op(db, left.id(), op, right.id()) {
        InferTy::Complete(_) => return Ok(()),
        InferTy::Error(_) => return mk_type_error(),
        InferTy::Partial(_) => (),
    }

    // Inference code covers most of the operations, perform the remaining checks
    match op {
        // Set operators (set, set => set)
        expr::BinaryOp::Add | expr::BinaryOp::Sub | expr::BinaryOp::Mul => {
            // Operations:
            // - Set union
            // - Set difference
            // - Set intersection

            // Set types must be equivalent
            if is_equivalent(db, left.id(), right.id()) && left.kind().is_set() {
                Ok(())
            } else {
                mk_type_error()
            }
        }
        // Comparison (a, b => boolean where a, b: Comparable)
        expr::BinaryOp::Less
        | expr::BinaryOp::LessEq
        | expr::BinaryOp::Greater
        | expr::BinaryOp::GreaterEq => {
            // Operations:
            // - Numeric compare (number, number => boolean)
            // - Charseq compare (charseq, charseq => boolean)
            // x Enum compare (enum, enum => boolean)
            // - Set sub/supersets (set, set => boolean)
            // x Class hierarchy (class, class => boolean)

            match (left.kind(), right.kind()) {
                // All numbers are comparable to each other
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => Ok(()),
                // Charseqs that can be coerced to a sized type are comparable
                (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => Ok(()),
                // Sets that are equivalent are comparable
                (lhs, _) if is_equivalent(db, left.id(), right.id()) && lhs.is_set() => Ok(()),
                // All other types aren't comparable
                _ => mk_type_error(),
            }
        }
        expr::BinaryOp::Equal | expr::BinaryOp::NotEqual => {
            // Operations:
            // - Numeric equality (number, number => boolean)
            // - Charseq equality (charseq, charseq => boolean)
            // x Class equality (class, class => boolean)
            // x Pointer equality (pointer, pointer => boolean)
            // - Set equality (set, set => boolean)
            // - Scalar equality (scalar, scalar => boolean)

            match (left.kind(), right.kind()) {
                // All numbers are comparable to each other
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => Ok(()),
                // Charseqs that can be coerced to a sized type are comparable
                (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => Ok(()),
                // All scalar types can be tested for equality if they are of equivalent types
                (lhs, rhs) if lhs.is_scalar() && rhs.is_scalar() => {
                    // Types should be coercible into each other
                    // TODO: This really only starts to matter when we lower range types, so add tests for this then
                    if is_either_coercible(db, left.id(), right.id()) {
                        Ok(())
                    } else {
                        mk_type_error()
                    }
                }
                (lhs, _) if is_equivalent(db, left.id(), right.id()) => {
                    // Sets that are equivalent can be tested for equality
                    if lhs.is_set() {
                        Ok(())
                    } else {
                        mk_type_error()
                    }
                }
                // All other types aren't comparable
                _ => mk_type_error(),
            }
        }
        // Set membership tests (set(a), a => boolean)
        expr::BinaryOp::In => unsupported_op(),
        expr::BinaryOp::NotIn => unsupported_op(),
        _ => unreachable!(),
    }
}

pub fn infer_unary_op<T: ?Sized + db::TypeDatabase>(
    db: &T,
    op: expr::UnaryOp,
    right: TypeId,
) -> InferTy {
    use crate::ty::{IntSize, NatSize, RealSize};

    let right = right.in_db(db);

    // Propagate error type as complete so that we don't duplicate the error
    if right.kind().is_error() {
        return InferTy::Complete(db.mk_error());
    }

    let right = right.to_base_type();

    match op {
        expr::UnaryOp::Not => {
            if right.kind().is_integer() {
                // Bitwise Not
                InferTy::Complete(db.mk_nat(NatSize::Nat))
            } else if let TypeKind::Boolean = &right.kind() {
                // Logical Not
                InferTy::Complete(db.mk_boolean())
            } else {
                InferTy::Error(db.mk_error())
            }
        }
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            match right.kind() {
                // Pass through integer inference
                TypeKind::Integer => InferTy::Complete(db.mk_integer()),

                // Normal operands
                TypeKind::Real(_) => InferTy::Complete(db.mk_real(RealSize::Real)),
                TypeKind::Int(_) => InferTy::Complete(db.mk_int(IntSize::Int)),
                TypeKind::Nat(_) => InferTy::Complete(db.mk_nat(NatSize::Nat)),
                _ => InferTy::Error(db.mk_error()),
            }
        }
    }
}
/// Performs the full type checks for the given unary operation and operand type.
///
/// # Returns
///
/// `Ok(())` if the operation is valid for the given operand type, or `Err(err)`
/// with `err` containing information about the error
pub fn check_unary_op<T: ?Sized + db::TypeDatabase>(
    db: &T,
    op: expr::UnaryOp,
    right: TypeId,
) -> Result<(), InvalidUnaryOp> {
    let right = right.in_db(db);

    // Infer unary type currently does all of the work
    match infer_unary_op(db, op, right.id()) {
        InferTy::Complete(_) => Ok(()),
        InferTy::Error(_) => {
            // Use the peeled version of the type
            Err(InvalidUnaryOp {
                op,
                right: right.id(),
            })
        }
        InferTy::Partial(_) => unreachable!(),
    }
}

/// An error representing binary operands which aren't values
#[derive(Debug)]
pub struct InvalidBinaryValues {
    left_info: Option<(crate::db::ValueSource, Span)>,
    right_info: Option<(crate::db::ValueSource, Span)>,
}

pub fn report_invalid_bin_values<'db, DB>(
    db: &'db DB,
    err: InvalidBinaryValues,
    reporter: &mut MessageSink,
) where
    DB: ?Sized + db::TypeDatabase + 'db,
{
    if let Some((value_src, span)) = err.left_info {
        report_not_value(db, value_src, span, reporter);
    }

    if let Some((value_src, span)) = err.right_info {
        report_not_value(db, value_src, span, reporter);
    }
}

/// An error representing a unary operand which isn't a value
#[derive(Debug)]
pub struct InvalidUnaryValue {
    right_info: (crate::db::ValueSource, Span),
}

pub fn report_invalid_unary_value<'db, DB>(
    db: &'db DB,
    err: InvalidUnaryValue,
    reporter: &mut MessageSink,
) where
    DB: ?Sized + db::TypeDatabase + 'db,
{
    let (value_src, span) = err.right_info;
    report_not_value(db, value_src, span, reporter);
}

/// An error representing mismatched binary operand types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidBinaryOp {
    left: TypeId,
    op: expr::BinaryOp,
    right: TypeId,
    unsupported: bool,
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
    let (peeled_left, peeled_right) = (
        left_ty.clone().to_base_type(),
        right_ty.clone().to_base_type(),
    );

    let msg = reporter
        .error_detailed(format!("mismatched types for {op_name}"), op_span)
        .with_note(format!("this is of type `{right_ty}`"), right_span)
        .with_note(format!("this is of type `{left_ty}`"), left_span)
        .with_error(
            format!("`{peeled_left}` cannot be {verb_phrase} `{peeled_right}`",),
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
        expr::BinaryOp::Less
        | expr::BinaryOp::LessEq
        | expr::BinaryOp::Greater
        | expr::BinaryOp::GreaterEq
        | expr::BinaryOp::Equal
        | expr::BinaryOp::NotEqual => {
            if is_equivalent(db, left_ty.id(), right_ty.id()) {
                msg.with_info("operands must both be scalars, sets, or strings")
            } else {
                msg.with_info("operands must both be the same type")
            }
        }
        // Set membership tests (set(a), a => boolean)
        expr::BinaryOp::In => todo!(),
        expr::BinaryOp::NotIn => todo!(),
    };
    msg.finish();
}

/// An error representing a mismatched unary operand type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnaryOp {
    op: expr::UnaryOp,
    right: TypeId,
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
    let peeled_right_ty = right_ty.clone().to_base_type();

    let msg = reporter
        .error_detailed(format!("mismatched types for {op_name}"), op_span)
        .with_note(format!("this is of type `{right_ty}`"), right_span)
        .with_error(
            format!("cannot apply {verb_phrase} to `{peeled_right_ty}`",),
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

fn report_not_value<'db, DB>(
    db: &'db DB,
    value_src: crate::db::ValueSource,
    span: Span,
    reporter: &mut MessageSink,
) where
    DB: ?Sized + db::TypeDatabase + 'db,
{
    // either:
    // "cannot use `{name}` as an expression"
    // "`{name}` is a reference to {binding_to}, not a variable"
    // "`{name}` declared here"
    //
    // or
    // "cannot use `{name}` as a type"
    // "`{name}` is a reference to {binding_to}, not a type"
    // "`{name}` declared here"

    let binding_src = match value_src {
        db::ValueSource::Body(lib_id, body_id) => (lib_id, body_id).into(),
        db::ValueSource::BodyExpr(lib_id, body_expr) => (lib_id, body_expr).into(),
    };

    let (binding_def, binding_to) = if let Some(def_id) = db.binding_def(binding_src) {
        let binding_to = match db.binding_to(def_id.into()) {
            Ok(binding_to) => binding_to,
            Err(NotBinding::Missing) => return, // all values are accepted
            Err(NotBinding::NotBinding) => unreachable!("from DefId"),
        };

        (def_id, binding_to)
    } else {
        // All values are accepted
        return;
    };

    let def_library = db.library(binding_def.0);
    let def_info = def_library.local_def(binding_def.1);
    let name = def_info.name.item();
    let def_at = def_info.name.span().lookup_in(&def_library.span_map);

    reporter
        .error_detailed(format!("cannot use `{name}` as an expression"), span)
        .with_error(
            format!("`{name}` is a reference to {binding_to}, not a variable"),
            span,
        )
        .with_note(format!("`{name}` declared here"), def_at)
        .finish();
}
