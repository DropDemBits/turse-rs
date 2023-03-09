//! Rules for type interactions
use toc_hir::library::LibraryId;
use toc_hir::{body, expr};
use toc_reporting::MessageSink;
use toc_span::Span;

use crate::{
    db::{self},
    ty::{self, make, NotFixedLen, SeqSize, TypeId, TypeKind},
};

use super::{AllowDyn, ArraySizing};

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
        matches!(self, TypeKind::Alias(..))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, TypeKind::Array(..))
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, TypeKind::Enum(..))
    }

    pub fn is_set(&self) -> bool {
        matches!(self, TypeKind::Set(..))
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeKind::Pointer(..))
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

    /// index types includes the following types:
    ///
    /// - all integer types
    /// - `Char`
    /// - `Boolean`
    /// - `Enum`
    /// - `Constrained` (only if the base type is)
    pub fn is_index(&self) -> bool {
        self.is_integer()
            || matches!(
                self,
                TypeKind::Char | TypeKind::Boolean | TypeKind::Enum(..)
            )
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
                | TypeKind::Enum(..)
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
            | TypeKind::Enum(..)
            | TypeKind::Pointer(..)
            | TypeKind::Subprogram(..) => true,
            TypeKind::Constrained(..) => {
                // Only if the base type is, but we don't have access to db
                unreachable!("missing to_base_type")
            }
            TypeKind::Array(..) => {
                // Aggregate types of elements
                false
            }
            TypeKind::String | TypeKind::CharN(_) | TypeKind::StringN(_) => {
                // Aggregate types of characters
                false
            }
            TypeKind::Alias(_, _) => {
                // This should be peeled first, but it's okay to conservatively treat it as
                // not one
                false
            }
            TypeKind::Opaque(_, _) => {
                // Considered distinct from its alias type
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
impl TypeId {
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
    pub fn peel_aliases(self, db: &dyn db::TypeDatabase) -> Self {
        match self.kind(db) {
            TypeKind::Alias(_, to_ty) => {
                assert!(!to_ty.kind(db).is_alias());
                *to_ty
            }
            _ => self,
        }
    }

    /// Peels the opaque wrapper into an alias, but only if it would be visible
    /// from `in_module`. Otherwise, returns itself.
    pub fn peel_opaque(
        self,
        db: &dyn db::TypeDatabase,
        in_module: toc_hir::item::ModuleId,
    ) -> Self {
        match self.kind(db) {
            TypeKind::Opaque(def_id, hidden_ty) => {
                use toc_hir::library::InLibrary;

                let item_of @ InLibrary(library_id, _) =
                    db.item_of(*def_id).expect("opaque not from type def");
                let def_module = db.inside_module(item_of.into());

                if db.is_module_ancestor(
                    InLibrary(library_id, def_module),
                    InLibrary(library_id, in_module),
                ) {
                    // Convert into an alias type (if needed)
                    match hidden_ty.kind(db) {
                        // Enums, sets, records, and unions don't need an alias
                        // FIXME: add special cases for records and unions once lowered
                        TypeKind::Enum(..) | TypeKind::Set(..) => *hidden_ty,
                        TypeKind::Alias(..) => unreachable!("found alias wrapped in opaque"),
                        _ => make::alias(db, *def_id, *hidden_ty),
                    }
                } else {
                    // Not visible, don't peel
                    self
                }
            }
            _ => self,
        }
    }

    /// Transforms the type into its base representation.
    ///
    /// Turns [`TypeKind::Constrained`] into its common type, and peels aliases.
    pub fn to_base_type(self, db: &dyn db::TypeDatabase) -> Self {
        let ty_ref = self.peel_aliases(db);

        match ty_ref.kind(db) {
            TypeKind::Constrained(base_ty, ..) => *base_ty,
            _ => ty_ref,
        }
    }
}

// TODO: Document type equivalence
// steal from the old compiler
/// Tests if the types are equivalent.
///
/// This is a symmetric relation.
pub fn is_equivalent<T: db::ConstEval + ?Sized>(db: &T, left: TypeId, right: TypeId) -> bool {
    let left = left.peel_aliases(db.up());
    let right = right.peel_aliases(db.up());

    // Quick bailout
    if left == right {
        // Same type id implies trivial equivalency
        return true;
    }

    match (left.kind(db.up()), right.kind(db.up())) {
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

            let Some((left_sz, right_sz)) = left_sz.ok().zip(right_sz.ok()) else {
                // Invalid evaluations treated as equivalent types
                return true;
            };

            // sized charseqs are treated as equivalent types if the sizes are equal
            left_sz.cmp(right_sz).is_eq()
        }

        // Constrained types are equivalent if their bounds are equal
        (
            TypeKind::Constrained(_, left_start, left_end),
            TypeKind::Constrained(_, right_start, right_end),
        ) => {
            // FIXME: use the correct eval params
            let eval_params = Default::default();

            let left_start = match db.evaluate_const(left_start.clone(), eval_params) {
                Ok(v) => v,
                Err(_) => return true, // invalid evaluations treated as equivalent types
            };
            let right_start = match db.evaluate_const(right_start.clone(), eval_params) {
                Ok(v) => v,
                Err(_) => return true, // invalid evaluations treated as equivalent types
            };

            // Bail if the start bounds weren't the same
            if left_start != right_start {
                return false;
            }

            // Fully equivalent if the end bounds are also the same
            match (left_end, right_end) {
                (
                    ty::EndBound::Expr(left_end, left_dyn),
                    ty::EndBound::Expr(right_end, right_dyn),
                ) => {
                    let eval_bound = |cons| {
                        db.evaluate_const(cons, eval_params).map_err(|err| {
                            if err.is_not_compile_time() {
                                // Only treat as an invalid expression (and therefore equivalent) if
                                // neither allow dynamic expressions, since dynamic expressions are
                                // never equivalent
                                matches!(left_dyn, AllowDyn::No)
                                    && matches!(right_dyn, AllowDyn::No)
                            } else {
                                // invalid evaluations treated as equivalent types
                                true
                            }
                        })
                    };

                    let left_end = match eval_bound(left_end.clone()) {
                        Ok(v) => v,
                        Err(equi) => return equi,
                    };
                    let right_end = match eval_bound(right_end.clone()) {
                        Ok(v) => v,
                        Err(equi) => return equi,
                    };

                    left_end == right_end
                }
                (ty::EndBound::Unsized(left_end), ty::EndBound::Unsized(right_end)) => {
                    left_end == right_end
                }
                // Different end bound kinds
                _ => false,
            }
        }

        // The following types are equivalent to the other type if they both come from the same definition:
        // - Enum
        // - Set
        // x Record
        // x Union
        (TypeKind::Enum(left_def, _), TypeKind::Enum(right_def, _))
        | (TypeKind::Set(left_def, _), TypeKind::Set(right_def, _)) => {
            left_def.def_id() == right_def.def_id()
        }

        // Array types are equivalent if both the ranges and the element type are equivalent, and
        // if they have the same flexibility
        (
            TypeKind::Array(left_sizing, left_ranges, left_elem),
            TypeKind::Array(right_sizing, right_ranges, right_elem),
        ) => {
            let same_flex = match (left_sizing, right_sizing) {
                // If either is flexible, the other must be too
                (ArraySizing::Flexible, other) | (other, ArraySizing::Flexible) => {
                    *other == ArraySizing::Flexible
                }
                // The rest are equivalent
                _ => true,
            };

            if !same_flex {
                return false;
            }

            // Element types must be equivalent
            if !is_equivalent(db, *left_elem, *right_elem) {
                return false;
            }

            // Ranges must be the same length, and all ranges must be equivalent types
            left_ranges.len() == right_ranges.len()
                && std::iter::zip(left_ranges.iter(), right_ranges.iter())
                    .all(|(left, right)| is_equivalent(db, *left, *right))
        }

        // Pointer types are equivalent if they have the same checkedness and equivalent target types
        (TypeKind::Pointer(left_chk, left_to), TypeKind::Pointer(right_chk, right_to)) => {
            left_chk == right_chk && is_equivalent(db, *left_to, *right_to)
        }
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
        // Opaque types are equivalent to their original alias definition
        (TypeKind::Opaque(opaque_def, _), TypeKind::Alias(alias_def, _))
        | (TypeKind::Alias(alias_def, _), TypeKind::Opaque(opaque_def, _)) => {
            opaque_def == alias_def
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
    let left = lhs.peel_aliases(db.up());
    let right = rhs.peel_aliases(db.up());

    // Equivalent types imply trivial coercion
    if is_equivalent(db, left, right) {
        return true;
    }

    match (left.kind(db.up()), right.kind(db.up())) {
        // CharSeq-likes are coercible into the corresponding any-sized CharSeq
        (TypeKind::CharN(SeqSize::Any), _) => right.kind(db.up()).is_char_like(),
        (TypeKind::StringN(SeqSize::Any), _) => right.kind(db.up()).is_string_like(),
        // Arrays are coercible if the ranges and elem ty are
        (
            TypeKind::Array(left_sizing, left_ranges, left_elem),
            TypeKind::Array(right_sizing, right_ranges, right_elem),
        ) => {
            let same_flex = match (left_sizing, right_sizing) {
                // If either is flexible, the other must be too
                (ArraySizing::Flexible, other) | (other, ArraySizing::Flexible) => {
                    other == &ArraySizing::Flexible
                }
                // The rest are equivalent
                _ => true,
            };

            if !same_flex {
                return false;
            }

            // Element types must be param-coercible
            if !is_coercible_into_param(db, *left_elem, *right_elem) {
                return false;
            }

            // Ranges must be the same length, and all ranges must be param-coercible types
            left_ranges.len() == right_ranges.len()
                && std::iter::zip(left_ranges.iter(), right_ranges.iter())
                    .all(|(left, right)| is_coercible_into_param(db, *left, *right))
        }
        (
            TypeKind::Constrained(left_base, left_start, left_end),
            TypeKind::Constrained(right_base, right_start, right_end),
        ) => {
            // Base types must be param-coercible
            if !is_coercible_into_param(db, *left_base, *right_base) {
                return false;
            }

            // FIXME: use the correct eval params
            let eval_params = Default::default();

            let left_start = match db.evaluate_const(left_start.clone(), eval_params) {
                Ok(v) => v,
                Err(_) => return true, // invalid evaluations treated as equivalent types
            };
            let right_start = match db.evaluate_const(right_start.clone(), eval_params) {
                Ok(v) => v,
                Err(_) => return true, // invalid evaluations treated as equivalent types
            };

            // Bail if the start bounds weren't the same
            if left_start != right_start {
                return false;
            }

            // Fully equivalent if the end bounds are also the same
            match (left_end, right_end) {
                (
                    ty::EndBound::Expr(left_end, left_dyn),
                    ty::EndBound::Expr(right_end, right_dyn),
                ) => {
                    let eval_bound = |cons| {
                        db.evaluate_const(cons, eval_params).map_err(|err| {
                            if err.is_not_compile_time() {
                                // Only treat as an invalid expression (and therefore equivalent) if
                                // neither allow dynamic expressions, since dynamic expressions are
                                // never equivalent
                                matches!(left_dyn, AllowDyn::No)
                                    && matches!(right_dyn, AllowDyn::No)
                            } else {
                                // invalid evaluations treated as equivalent types
                                true
                            }
                        })
                    };

                    let left_end = match eval_bound(left_end.clone()) {
                        Ok(v) => v,
                        Err(equi) => return equi,
                    };
                    let right_end = match eval_bound(right_end.clone()) {
                        Ok(v) => v,
                        Err(equi) => return equi,
                    };

                    left_end == right_end
                }
                // Unsized bounds must be the same length (both are known to start at the same element)
                (ty::EndBound::Unsized(left_end), ty::EndBound::Unsized(right_end)) => {
                    left_end == right_end
                }
                // Any bound is trivially param-coercible into an `any`-sized bound
                (ty::EndBound::Any, _) => true,
                // Different end bound kinds
                _ => false,
            }
        }
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

    let left = lhs.to_base_type(db.up());
    let right = rhs.to_base_type(db.up());

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
    if is_equivalent(db, left, right) {
        return true;
    }

    match (left.kind(db.up()), right.kind(db.up())) {
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

        // [`TypeKind::Opaque`] can be coerced into an alias of the equivalent type
        // Handled by [`peel_opaque`] conversions

        // [`TypeKind::Constrained`] can be coerced into its base type
        // Handled by [`to_base_type`] conversions

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
    // Defer to the coercion rules
    is_coercible_into(db, left, right)
}

/// Checks that the operands for the binary op are values, or types when appropriate
pub fn check_binary_op_values<T: ?Sized + db::TypeDatabase>(
    db: &T,
    library_id: LibraryId,
    body_id: body::BodyId,
    expr: &expr::Binary,
) -> Result<(), InvalidBinaryValues> {
    use crate::db::NotValueErrExt;

    let left_src = (library_id, body_id, expr.lhs).into();
    let right_src = (library_id, body_id, expr.rhs).into();

    // Only comparison ops support referring to types, but only on classes
    let lhs = db.value_produced(left_src);
    let rhs = db.value_produced(right_src);

    if lhs.is_any_value() && rhs.is_any_value() {
        Ok(())
    } else {
        let library = db.library(library_id);

        Err(InvalidBinaryValues {
            left_info: (!lhs.is_any_value()).then(|| {
                (
                    left_src,
                    library
                        .body(body_id)
                        .expr(expr.lhs)
                        .span
                        .lookup_in(&library),
                )
            }),
            right_info: (!rhs.is_any_value()).then(|| {
                (
                    right_src,
                    library
                        .body(body_id)
                        .expr(expr.rhs)
                        .span
                        .lookup_in(&library),
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
    use crate::db::NotValueErrExt;

    // All unary ops only support value operands
    let right_src = (library_id, body_id, expr.rhs).into();
    let rhs = db.value_produced(right_src);

    if rhs.is_any_value() {
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
                    .lookup_in(&library),
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
pub fn infer_binary_op(
    db: &dyn db::TypeDatabase,
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

    fn infer_arithmetic_operands(
        db: &dyn db::TypeDatabase,
        left: &TypeKind,
        right: &TypeKind,
    ) -> Option<TypeId> {
        match (left, right) {
            // Pass through integer inference
            (TypeKind::Integer, TypeKind::Integer) => Some(make::integer(db)),

            // Normal operands
            (operand, TypeKind::Real(_)) | (TypeKind::Real(_), operand) if operand.is_number() => {
                Some(make::real(db, RealSize::Real))
            }
            (operand, TypeKind::Int(_)) | (TypeKind::Int(_), operand) if operand.is_integer() => {
                Some(make::int(db, IntSize::Int))
            }
            (operand, TypeKind::Nat(_)) | (TypeKind::Nat(_), operand) if operand.is_nat() => {
                Some(make::nat(db, NatSize::Nat))
            }
            _ => None,
        }
    }

    fn infer_bitwise_operands(
        db: &dyn db::TypeDatabase,
        left: &TypeKind,
        right: &TypeKind,
    ) -> Option<TypeId> {
        // Normal operands
        // Integer inference is not passed through

        if left.is_integer() && right.is_integer() {
            Some(make::nat(db, NatSize::Nat))
        } else {
            None
        }
    }

    fn infer_charseq_operands(
        db: &dyn db::TypeDatabase,
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
            Some(make::string(db))
        } else {
            None
        }
    }

    let left = left.to_base_type(db);
    let right = right.to_base_type(db);

    // Propagate error type as complete so that we don't duplicate the error
    if left.kind(db).is_error() || right.kind(db).is_error() {
        return InferTy::Complete(make::error(db));
    }

    match op {
        // Arithmetic operators
        expr::BinaryOp::Add => {
            // Operations:
            // - String concatenation (charseq, charseq => charseq)
            // - Set union (set, set => set)
            // - Addition (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(db), right.kind(db)) {
                // Addition
                InferTy::Complete(result_ty)
            } else if let Some(result_ty) =
                infer_charseq_operands(db, left.kind(db), right.kind(db))
            {
                // String concatenation
                InferTy::Complete(result_ty)
            } else if left.kind(db).is_set() && right.kind(db).is_set() {
                // Set union (depends on `is_equivalent`, but okay to infer from left)
                InferTy::Partial(left)
            } else {
                // Type error
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Sub => {
            // Operations:
            // - Set difference (set, set => set)
            // - Subtraction (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(db), right.kind(db)) {
                // Subtraction
                InferTy::Complete(result_ty)
            } else if left.kind(db).is_set() && right.kind(db).is_set() {
                // Set difference (depends on `is_equivalent`, but okay to infer from left)
                InferTy::Partial(left)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Mul => {
            // Operations:
            // - Set intersection (set, set => set)
            // - Multiplication (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(db), right.kind(db)) {
                // Multiplication
                InferTy::Complete(result_ty)
            } else if left.kind(db).is_set() && right.kind(db).is_set() {
                // Set intersection (depends on `is_equivalent`, but okay to infer from left)
                InferTy::Partial(left)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Div => {
            // Operations:
            // - Integer division (number, number => integer)

            match (left.kind(db), right.kind(db)) {
                // Pass through type inference
                (TypeKind::Integer, TypeKind::Integer) => InferTy::Complete(make::integer(db)),
                (operand, TypeKind::Nat(_)) | (TypeKind::Nat(_), operand) if operand.is_nat() => {
                    InferTy::Complete(make::nat(db, NatSize::Nat))
                }
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => {
                    InferTy::Complete(make::int(db, IntSize::Int))
                }
                _ => InferTy::Error(make::error(db)),
            }
        }
        expr::BinaryOp::RealDiv => {
            // Operations:
            // - Floating point division (number, number => real)

            if left.kind(db).is_number() && right.kind(db).is_number() {
                InferTy::Complete(make::real(db, RealSize::Real))
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Mod => {
            // Operations:
            // - Modulo (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(db), right.kind(db)) {
                // Modulo
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Rem => {
            // Operations:
            // - Remainder (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(db), right.kind(db)) {
                // Remainder
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Exp => {
            // Operations:
            // - Exponentiation (number, number => number)

            if let Some(result_ty) = infer_arithmetic_operands(db, left.kind(db), right.kind(db)) {
                // Exponentiation
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        // Bitwise operators (integer, integer => nat)
        // + Logical operators (boolean, boolean => boolean)
        expr::BinaryOp::And => {
            // Operations:
            // - Bitwise And (integer, integer => nat)
            // - Logical And (boolean, boolean => boolean)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(db), right.kind(db)) {
                // Bitwise And
                InferTy::Complete(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(db), right.kind(db)) {
                // Logical And
                InferTy::Complete(make::boolean(db))
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Or => {
            // Operations:
            // - Bitwise Or (integer, integer => nat)
            // - Logical Or (boolean, boolean => boolean)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(db), right.kind(db)) {
                // Bitwise Or
                InferTy::Complete(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(db), right.kind(db)) {
                // Logical Or
                InferTy::Complete(make::boolean(db))
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Xor => {
            // Operations:
            // - Bitwise Xor (integer, integer => nat)
            // - Logical Xor (boolean, boolean => boolean)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(db), right.kind(db)) {
                // Bitwise Xor
                InferTy::Complete(result_ty)
            } else if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(db), right.kind(db)) {
                // Logical Xor
                InferTy::Complete(make::boolean(db))
            } else {
                InferTy::Error(make::error(db))
            }
        }
        // Pure bitwise operators
        expr::BinaryOp::Shl => {
            // Operations:
            // - Bitwise Shl (integer, integer => nat)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(db), right.kind(db)) {
                // Bitwise Shl
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::BinaryOp::Shr => {
            // Operations:
            // - Bitwise Shr (integer, integer => nat)

            if let Some(result_ty) = infer_bitwise_operands(db, left.kind(db), right.kind(db)) {
                // Bitwise Shr
                InferTy::Complete(result_ty)
            } else {
                InferTy::Error(make::error(db))
            }
        }
        // Pure logical operator
        expr::BinaryOp::Imply => {
            // Operations:
            // - Imply (boolean, boolean => boolean)

            if let (TypeKind::Boolean, TypeKind::Boolean) = (left.kind(db), right.kind(db)) {
                // Imply
                InferTy::Complete(make::boolean(db))
            } else {
                InferTy::Error(make::error(db))
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
        | expr::BinaryOp::NotEqual => InferTy::Partial(make::boolean(db)),
        // Set membership tests (a, set(a) => boolean)
        // Requires `is_equivalent`, but inferring them into a boolean type is ok
        expr::BinaryOp::In | expr::BinaryOp::NotIn => InferTy::Partial(make::boolean(db)),
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
    let left = left;
    let right = right;

    // Use the peeled versions of the types for reporting
    let mk_type_error = move || Err(InvalidBinaryOp { left, op, right });

    let left = left.to_base_type(db.up());
    let right = right.to_base_type(db.up());

    // Start from the inference code
    match infer_binary_op(db.upcast_to_type_db(), left, op, right) {
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
            if is_equivalent(db, left, right) && left.kind(db.up()).is_set() {
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
            // - Enum compare (enum, enum => boolean)
            // - Set sub/supersets (set, set => boolean)
            // x Class hierarchy (class, class => boolean)

            match (left.kind(db.up()), right.kind(db.up())) {
                // All numbers are comparable to each other
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => Ok(()),
                // Charseqs that can be coerced to a sized type are comparable
                (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => Ok(()),
                // Types that are equivalent may be compared
                (lhs, _) if is_equivalent(db, left, right) => {
                    // This only applies to the following types:
                    // - Sets
                    // - Enums
                    if lhs.is_set() || lhs.is_enum() {
                        Ok(())
                    } else {
                        mk_type_error()
                    }
                }
                // All other types aren't comparable
                _ => mk_type_error(),
            }
        }
        expr::BinaryOp::Equal | expr::BinaryOp::NotEqual => {
            // Operations:
            // - Numeric equality (number, number => boolean)
            // - Charseq equality (charseq, charseq => boolean)
            // x Class equality (class, class => boolean)
            // - Pointer equality (pointer, pointer => boolean)
            // - Set equality (set, set => boolean)
            // - Scalar equality (scalar, scalar => boolean)

            match (left.kind(db.up()), right.kind(db.up())) {
                // All numbers are comparable to each other
                (lhs, rhs) if lhs.is_number() && rhs.is_number() => Ok(()),
                // Charseqs that can be coerced to a sized type are comparable
                (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => Ok(()),
                // All scalar types can be tested for equality if they are of equivalent types
                (lhs, rhs) if lhs.is_scalar() && rhs.is_scalar() => {
                    // Types should be coercible into each other
                    // TODO: This really only starts to matter when we lower range types, so add tests for this then
                    if is_either_coercible(db, left, right) {
                        Ok(())
                    } else {
                        mk_type_error()
                    }
                }
                // Types that are equivalent may be tested for equality
                (lhs, _) if is_equivalent(db, left, right) => {
                    // This only applies to the following types:
                    // - Sets
                    // - Enums
                    // - Pointers
                    if lhs.is_set() || lhs.is_pointer() {
                        Ok(())
                    } else {
                        mk_type_error()
                    }
                }
                // All other types aren't comparable
                _ => mk_type_error(),
            }
        }
        // Set membership tests (a, set(a) => boolean)
        expr::BinaryOp::In | expr::BinaryOp::NotIn => {
            // `right` must be a set
            let TypeKind::Set(_, elem_ty) = right.kind(db.up()) else {
                return mk_type_error();
            };

            // Element type & `left` type must be coercible
            if is_coercible_into(db, *elem_ty, left) {
                Ok(())
            } else {
                mk_type_error()
            }
        }
        _ => unreachable!(),
    }
}

pub fn infer_unary_op(db: &dyn db::TypeDatabase, op: expr::UnaryOp, right: TypeId) -> InferTy {
    use crate::ty::{IntSize, NatSize, RealSize};

    let right = right;

    // Propagate error type as complete so that we don't duplicate the error
    if right.kind(db).is_error() {
        return InferTy::Complete(make::error(db));
    }

    let right = right.to_base_type(db);

    match op {
        expr::UnaryOp::Not => {
            if right.kind(db).is_integer() {
                // Bitwise Not
                InferTy::Complete(make::nat(db, NatSize::Nat))
            } else if let TypeKind::Boolean = &right.kind(db) {
                // Logical Not
                InferTy::Complete(make::boolean(db))
            } else {
                InferTy::Error(make::error(db))
            }
        }
        expr::UnaryOp::Identity | expr::UnaryOp::Negate => {
            match right.kind(db) {
                // Pass through integer inference
                TypeKind::Integer => InferTy::Complete(make::integer(db)),

                // Normal operands
                TypeKind::Real(_) => InferTy::Complete(make::real(db, RealSize::Real)),
                TypeKind::Int(_) => InferTy::Complete(make::int(db, IntSize::Int)),
                TypeKind::Nat(_) => InferTy::Complete(make::nat(db, NatSize::Nat)),
                _ => InferTy::Error(make::error(db)),
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
    let right = right;

    // Infer unary type currently does all of the work
    match infer_unary_op(db.upcast_to_type_db(), op, right) {
        InferTy::Complete(_) => Ok(()),
        InferTy::Error(_) => {
            // Use the peeled version of the type
            Err(InvalidUnaryOp { op, right })
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
}

pub fn report_invalid_bin_op(
    db: &dyn db::ConstEval,
    err: InvalidBinaryOp,
    left_span: Span,
    op_span: Span,
    right_span: Span,
    reporter: &mut MessageSink,
) {
    let InvalidBinaryOp {
        left: left_ty,
        op,
        right: right_ty,
        ..
    } = err;

    let left_ty = left_ty;
    let right_ty = right_ty;
    // Try logical operation if either is a boolean
    let is_logical = left_ty.kind(db.up()).is_boolean() || right_ty.kind(db.up()).is_boolean();
    // Try string concat operation if either is a charseq
    let is_string_concat =
        left_ty.kind(db.up()).is_charseq() || right_ty.kind(db.up()).is_charseq();

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
        left_ty.to_base_type(db.up()),
        right_ty.to_base_type(db.up()),
    );

    // Specialize message for set member ops
    if matches!(op, expr::BinaryOp::In | expr::BinaryOp::NotIn) {
        // Set membership tests (set(a), a => boolean)
        if let TypeKind::Set(_, elem_ty) = peeled_right.kind(db.up()) {
            // Incompatible element types
            let elem_ty = elem_ty.to_base_type(db.up());

            reporter
                .error_detailed(format!("mismatched types for {op_name}"), op_span)
                .with_note(
                    format!(
                        "this is of type `{right_ty}`",
                        right_ty = right_ty.display(db)
                    ),
                    right_span,
                )
                .with_note(
                    format!("this is of type `{left_ty}`", left_ty = left_ty.display(db)),
                    left_span,
                )
                .with_error(
                    format!(
                        "`{peeled_left}` is not the same as `{elem_ty}`",
                        peeled_left = peeled_left.display(db),
                        elem_ty = elem_ty.display(db)
                    ),
                    right_span,
                )
                .with_info("operand and element type must be the same")
                .finish();
        } else {
            // Not a set
            reporter
                .error_detailed(format!("mismatched types for {op_name}"), op_span)
                .with_note(
                    format!(
                        "this is of type `{right_ty}`",
                        right_ty = right_ty.display(db)
                    ),
                    right_span,
                )
                .with_error(
                    format!(
                        "`{peeled_right}` is not a set type",
                        peeled_right = peeled_right.display(db)
                    ),
                    right_span,
                )
                .with_info("operand must be a set")
                .finish();
        }
        return;
    }

    let msg = {
        reporter
            .error_detailed(format!("mismatched types for {op_name}"), op_span)
            .with_note(
                format!(
                    "this is of type `{right_ty}`",
                    right_ty = right_ty.display(db)
                ),
                right_span,
            )
            .with_note(
                format!("this is of type `{left_ty}`", left_ty = left_ty.display(db)),
                left_span,
            )
            .with_error(
                format!(
                    "`{peeled_left}` cannot be {verb_phrase} `{peeled_right}`",
                    peeled_left = peeled_left.display(db),
                    peeled_right = peeled_right.display(db)
                ),
                op_span,
            )
    };

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
            if is_equivalent(db, left_ty, right_ty) {
                msg.with_info("operands must both be scalars, sets, or strings")
            } else {
                // Specialize for pointers of different checkedness
                match (peeled_left.kind(db.up()), peeled_right.kind(db.up())) {
                    (
                        TypeKind::Pointer(left_check, left_ty),
                        TypeKind::Pointer(right_check, right_ty),
                    ) if left_check != right_check && is_equivalent(db, *left_ty, *right_ty) => {
                        // Of different checked kinds
                        msg.with_info("pointers must have be the same checkedness")
                    }
                    _ => msg.with_info("operands must both be the same type"),
                }
            }
        }
        // Set membership tests (set(a), a => boolean)
        // Already specialized from above
        expr::BinaryOp::In | expr::BinaryOp::NotIn => unreachable!(),
    };
    msg.finish();
}

/// An error representing a mismatched unary operand type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidUnaryOp {
    op: expr::UnaryOp,
    right: TypeId,
}

pub fn report_invalid_unary_op(
    db: &dyn db::ConstEval,
    err: InvalidUnaryOp,
    op_span: Span,
    right_span: Span,
    reporter: &mut MessageSink,
) {
    let InvalidUnaryOp {
        op,
        right: right_ty,
        ..
    } = err;
    let right_ty = right_ty;
    let is_bitwise = right_ty.kind(db.up()).is_integer();

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
    let peeled_right_ty = right_ty.to_base_type(db.up());

    let msg = reporter
        .error_detailed(format!("mismatched types for {op_name}"), op_span)
        .with_note(
            format!(
                "this is of type `{right_ty}`",
                right_ty = right_ty.display(db),
            ),
            right_span,
        )
        .with_error(
            format!(
                "cannot apply {verb_phrase} to `{peeled_right_ty}`",
                peeled_right_ty = peeled_right_ty.display(db)
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
        db::ValueSource::Def(def_id) => def_id.into(),
        db::ValueSource::Body(lib_id, body_id) => (lib_id, body_id).into(),
        db::ValueSource::BodyExpr(lib_id, body_expr) => (lib_id, body_expr).into(),
    };

    let (binding_def, binding_to) = if let Some(def_id) = db.binding_def(binding_src) {
        // not being a symbol implies that it's a value, and all values are accepted
        let Some(binding_to) = db.symbol_kind(def_id) else { return };

        (def_id, binding_to)
    } else {
        // All values are accepted
        return;
    };

    let def_library = db.library(binding_def.0);
    let def_info = def_library.local_def(binding_def.1);
    let name = def_info.name;
    let def_at = def_info.def_at.lookup_in(&def_library);

    reporter
        .error_detailed(format!("cannot use `{name}` as an expression"), span)
        .with_error(
            format!("`{name}` is a reference to {binding_to}, not a variable"),
            span,
        )
        .with_note(format!("`{name}` declared here"), def_at)
        .finish();
}
