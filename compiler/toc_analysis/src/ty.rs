//! Representation of Turing types

use std::{fmt::Debug, sync::Arc};

use toc_hir::symbol::{self, DefId};
use toc_span::Span;

use crate::const_eval::{Const, ConstError, ConstInt, ConstValue};

pub(crate) mod db;
mod lower;
mod pretty;
pub(crate) mod query;
pub mod rules;

/// Maximum length of a `string`
pub const MAX_STRING_LEN: u32 = 256;

/// Maximum length of a `char(N)`
pub const MAX_CHAR_N_LEN: u32 = 32768;

// Constructible vs Well-formed (valid)
//
// Constructible, Well-formed  => Type itself is real, and all dependencies of it are real
// Constructible, ill-formed   => Type itself is real, but maybe not its dependencies
//                                e.g. `char(<invalid expr>)`, `set of <error>`
// Unconstructible, ill-formed => A type cannot be reified
//                                e.g. `<error>`, or from a TyKind::Missing

/// General concrete type
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
}

impl Type {
    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// Unconstructable type.
    /// Produced so that errors aren't duplicated.
    Error,
    /// Boolean type.
    Boolean,
    /// Signed integer types (e.g. `int4`, `int`).
    Int(IntSize),
    /// Unsigned integer types (e.g. `nat4`, `nat`).
    /// Also includes `addressint`, with the size being dependent on the target.
    /// machine.
    Nat(NatSize),
    /// Floating point types (e.g. `real8`, `real`).
    Real(RealSize),
    /// General integer type. Infers to adjacent concrete types (in the case of
    /// binary expressions) or to `int`.
    Integer,
    /// Single character type.
    Char,
    /// Simple string type.
    String,
    /// Fixed-size character type.
    CharN(SeqSize),
    /// Fixed-size string type.
    StringN(SeqSize),
    /// Named alias to another type, pointing to the base (un-aliased) type
    Alias(DefId, TypeId),
    /// Forward declaration of a type.
    /// This is used to prevent cyclic type declarations, which we can't detect
    /// yet.
    Forward,
    /// An alias exported as an opaque type. Points to the base (un-aliased) alias,
    /// with the [`DefId`] pointing to the original alias.
    Opaque(DefId, TypeId),
    /// Constrained value type, with base type, range start, and range end.
    /// Base type is already de-aliased
    Constrained(TypeId, Const, EndBound),
    /// Array type, with flexibility, types for each index, and the element type.
    Array(ArraySizing, Vec<TypeId>, TypeId),
    /// An enumeration type, with associated definition point and variants.
    Enum(WithDef, Vec<DefId>),
    /// Set type, with associated definition point
    Set(WithDef, TypeId),
    /// Set type, with a given checkedness
    Pointer(Checked, TypeId),
    /// Subprogram type, from (`procedure`, `function`, and `process`).
    Subprogram(symbol::SubprogramKind, Option<Vec<Param>>, TypeId),
    /// Void type, returned from (`procedure` and `process`)
    Void,
}

// Other types to add:
// - array
// - range
// - enum
// - union
// - record
// - set
// - pointer
// - condition
// - collection

/// Size variant of an `Int`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntSize {
    Int1,
    Int2,
    Int4,
    /// Initialization checked version of `Int4`
    Int,
}

/// Size variant of a Nat
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NatSize {
    Nat1,
    Nat2,
    Nat4,
    /// Initialization checked version of `Nat4`
    Nat,
    /// Address sized integer, dependent on target machine
    AddressInt,
}

/// Size variant of a Real
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RealSize {
    Real4,
    Real8,
    /// Initialization checked version of `Real8`
    Real,
}

/// Size of a CharSeq
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SeqSize {
    /// Any sized (only accepted for parameters)
    Any,
    /// Fixed size, may be compile-time evaluable
    Fixed(Const),
}

impl SeqSize {
    /// Tries to compute a compile-time size from this sequence size.
    ///
    /// If this is an any-length sequence, [`NotFixedLen::AnySize`] is produced.
    /// If an error occurs during length computation, `NotFixedLen::ConstError(err)` is produced.
    pub fn fixed_len<T: crate::db::ConstEval + ?Sized>(
        &self,
        db: &T,
        span: Span,
    ) -> Result<ConstInt, NotFixedLen> {
        let size = match self {
            SeqSize::Any => return Err(NotFixedLen::AnySize),
            SeqSize::Fixed(size) => size,
        };

        // Always eagerly evaluate the expr
        // Never allow 64-bit ops (size is always less than 2^32)
        db.evaluate_const(size.clone(), Default::default())
            .and_then(|v| v.into_int(span))
            .map_err(NotFixedLen::ConstError)
    }
}

/// Error from trying to compute the fixed length of a [`SeqSize`].
pub enum NotFixedLen {
    /// Trying to compute from an any-sized sequence
    AnySize,
    /// Error while trying to evaluate the sequence
    ConstError(ConstError),
}

/// If dynamic (i.e. runtime-evaluable) expressions are allowed
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AllowDyn {
    No,
    Yes,
}

/// End bound of a [`TypeKind::Constrained`]
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum EndBound {
    /// From a constant expression
    Expr(Const, AllowDyn),
    /// From an element count
    Unsized(u32),
    /// Derived at runtime
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArraySizing {
    /// Array size is not fixed, and zero-sized ranges are allowed.
    /// Initialization from dynamic values is implied to be allowed.
    Flexible,
    /// Array size is fixed, but allowed to be derived from runtime values
    MaybeDyn,
    /// Array size is fixed, and must be known at compile-time
    Static,
}

/// Type with an associated definition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WithDef {
    /// True name of the type
    Named(DefId),
    /// Anonymously named type
    Anonymous(DefId),
}

impl WithDef {
    pub fn def_id(self) -> DefId {
        match self {
            WithDef::Named(def_id) | WithDef::Anonymous(def_id) => def_id,
        }
    }
}

/// Pointer checkedness
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Checked {
    /// Pointer with additional correctness data
    Checked,
    /// Raw pointer, no extra metadata
    Unchecked,
}

/// Parameter for a [`TypeKind::Subprogram`]
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Param {
    /// How the parameter should be passed in as
    pub pass_by: PassBy,
    /// If the value should be bound to a register
    pub is_register: bool,
    /// If the passed in value should be coerced to the target's type
    pub coerced_type: bool,
    /// Parameter's type
    pub param_ty: TypeId,
}

/// How a parameter should be passed in
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PassBy {
    /// Pass by value
    Value,
    /// Pass by reference
    Reference(symbol::Mutability),
}

/// Wrapper type for making it easier to work with TypeIds
#[derive(PartialEq, Eq)]
pub struct TyRef<'db, DB: ?Sized + 'db> {
    db: &'db DB,
    id: TypeId,
    data: TypeData,
}

impl<'db, DB: ?Sized + 'db> TyRef<'db, DB> {
    pub fn kind(&self) -> &TypeKind {
        &self.data.kind
    }

    pub fn id(&self) -> TypeId {
        self.id
    }

    /// If this type has an uninitialized pattern
    pub fn has_uninit(&self) -> bool {
        matches!(
            self.kind(),
            TypeKind::Boolean
                | TypeKind::Int(IntSize::Int)
                | TypeKind::Nat(NatSize::AddressInt | NatSize::Nat)
                | TypeKind::Real(RealSize::Real)
                | TypeKind::String
        )
    }
}

impl<'db, DB> TyRef<'db, DB>
where
    DB: crate::db::ConstEval + ?Sized + 'db,
{
    /// Alignment of a type, or `None` if it isn't representable
    pub fn align_of(&self) -> Option<usize> {
        const POINTER_ALIGNMENT: usize = 4;

        let align_of = match self.kind() {
            TypeKind::Error => return None,
            TypeKind::Boolean => 1,
            TypeKind::Int(IntSize::Int1) => 1,
            TypeKind::Int(IntSize::Int2) => 2,
            TypeKind::Int(IntSize::Int4) => 4,
            TypeKind::Int(IntSize::Int) => 4,
            TypeKind::Nat(NatSize::Nat1) => 1,
            TypeKind::Nat(NatSize::Nat2) => 2,
            TypeKind::Nat(NatSize::Nat4) => 4,
            TypeKind::Nat(NatSize::Nat) => 4,
            TypeKind::Nat(NatSize::AddressInt) => POINTER_ALIGNMENT,
            TypeKind::Real(RealSize::Real4) => 4,
            TypeKind::Real(RealSize::Real8) => 4,
            TypeKind::Real(RealSize::Real) => 4,
            TypeKind::Integer => return None,
            TypeKind::Char => 1,
            TypeKind::String => 1,
            TypeKind::CharN(_) => 1,
            TypeKind::StringN(_) => 1,
            TypeKind::Subprogram(..) => POINTER_ALIGNMENT,
            TypeKind::Void => return None,
            TypeKind::Constrained(base_ty, ..) => {
                // Defer to the base type
                return base_ty.in_db(self.db).peel_aliases().align_of();
            }
            TypeKind::Array(..) => POINTER_ALIGNMENT, // ???: It's to an array descriptor
            TypeKind::Enum(..) => 4, // ???: Alignment based on user-specified size?
            TypeKind::Set(..) => 2,
            TypeKind::Pointer(_, _) => POINTER_ALIGNMENT,
            // Defer to the aliased type
            TypeKind::Alias(_, base_ty) => return base_ty.in_db(self.db).align_of(),
            TypeKind::Opaque(_, base_ty) => {
                return base_ty.in_db(self.db).peel_aliases().align_of()
            }
            TypeKind::Forward => return None,
        };

        Some(align_of)
    }

    /// Size of a type, or `None` if it isn't representable
    pub fn size_of(&self) -> Option<usize> {
        const POINTER_SIZE: usize = 4;
        let size_of = match self.kind() {
            TypeKind::Boolean => 1,
            TypeKind::Int(IntSize::Int1) => 1,
            TypeKind::Int(IntSize::Int2) => 2,
            TypeKind::Int(IntSize::Int4) => 4,
            TypeKind::Int(IntSize::Int) => 4,
            TypeKind::Nat(NatSize::Nat1) => 1,
            TypeKind::Nat(NatSize::Nat2) => 2,
            TypeKind::Nat(NatSize::Nat4) => 4,
            TypeKind::Nat(NatSize::Nat) => 4,
            TypeKind::Nat(NatSize::AddressInt) => POINTER_SIZE,
            TypeKind::Real(RealSize::Real4) => 4,
            TypeKind::Real(RealSize::Real8) => 8,
            TypeKind::Real(RealSize::Real) => 8,
            TypeKind::Char => 1,
            TypeKind::String | TypeKind::StringN(_) | TypeKind::CharN(_) => {
                let length_of = self.length_of()?;

                if matches!(self.kind(), TypeKind::String | TypeKind::StringN(_)) {
                    // Storage size for strings includes the always present null terminator
                    length_of + 1
                } else {
                    // Storage size for char(N)'s is always rounded up to the nearest 2-byte boundary
                    // ???: This can always be to the nearest byte boundary, but this depends on the
                    // alignment of char(N)
                    // TODO: Align up sizes for other types according to the type's alignment
                    align_up_to(length_of, 2)
                }
            }
            TypeKind::Subprogram(..) => POINTER_SIZE,
            TypeKind::Void => return None,
            TypeKind::Constrained(base_ty, ..) => {
                // Defer to the base type
                return base_ty.in_db(self.db).peel_aliases().size_of();
            }
            TypeKind::Array(..) => POINTER_SIZE, // To an array descriptor
            TypeKind::Enum(..) => 4, // FIXME: Have size be based on a user-specified size
            TypeKind::Set(_, _elem_ty) => return None, // FIXME: Compute size of sets
            TypeKind::Pointer(Checked::Checked, _) => POINTER_SIZE * 2, // address + metadata
            TypeKind::Pointer(Checked::Unchecked, _) => POINTER_SIZE, // address only
            // Defer to the aliased type
            TypeKind::Alias(_, base_ty) => return base_ty.in_db(self.db).size_of(),
            TypeKind::Opaque(_, base_ty) => return base_ty.in_db(self.db).size_of(),
            TypeKind::Integer | TypeKind::Forward | TypeKind::Error => return None,
        };

        Some(size_of)
    }

    /// Length of a type, or `None` if it isn't representable or a charseq
    pub fn length_of(&self) -> Option<usize> {
        let length = match self.kind() {
            TypeKind::String => {
                // max chars (excluding null terminator)
                255
            }
            TypeKind::CharN(seq_size) | TypeKind::StringN(seq_size) => {
                let char_len = seq_size.fixed_len(self.db, Span::default()).ok()?;

                (char_len.into_u32()?) as usize
            }
            TypeKind::Error => unreachable!(),
            _ => return None,
        };

        Some(length)
    }

    /// If this type were to be used as a range, computes the number of elements in the range,
    /// or `None` if it can't be used as a range
    pub fn element_count(&self) -> Result<ConstInt, NotInteger> {
        match self.kind() {
            TypeKind::Array(ArraySizing::Static | ArraySizing::MaybeDyn, ranges, _) => {
                // Element count is just the product of the range element counts
                let mut count = ConstInt::ONE;

                for &range_ty in ranges {
                    // Forcefully poke through opaque
                    // Observing through is okay, since we don't leak the type itself
                    let range_tyref = {
                        let ty_ref = range_ty.in_db(self.db);
                        if let TypeKind::Opaque(_, type_id) = ty_ref.kind() {
                            type_id.in_db(self.db)
                        } else {
                            ty_ref.peel_aliases()
                        }
                    };

                    // If this isn't an index type, stop
                    // Prevents a cycle if we happen to have the same array type as one of the
                    // range types.
                    if !range_tyref.clone().to_base_type().kind().is_index() {
                        return Err(NotInteger::NotInteger);
                    }

                    let index_count = range_tyref.element_count()?;

                    count = count
                        .checked_mul(index_count)
                        .map_err(NotInteger::ConstError)?;
                }

                Ok(count)
            }
            _ => {
                // Simple range subtraction
                let min = self.min_int_of()?;
                let max = self.max_int_of()?;

                // Range is inclusive, so also add 1
                max.checked_sub(min)
                    .and_then(|count| count.checked_add(ConstInt::ONE))
                    .map_err(NotInteger::ConstError)
            }
        }
    }

    /// Minimum integer value of this type, or `None` if this is not an integer
    pub fn min_int_of(&self) -> Result<ConstInt, NotInteger> {
        let value = match self.kind() {
            TypeKind::Char => ConstValue::Char('\x00')
                .ordinal()
                .expect("const construction"),
            TypeKind::Boolean => ConstValue::Bool(false)
                .ordinal()
                .expect("const construction"),
            TypeKind::Int(size) => {
                let (min, allow_64bit_ops) = match size {
                    IntSize::Int1 => (0x80, false),
                    IntSize::Int2 => (0x8000, false),
                    IntSize::Int4 => (0x8000_0000, false),
                    // 0x8000_0000 is reserved as the uninitialized pattern
                    IntSize::Int => (0x7FFF_FFFF, false),
                };

                ConstInt::from_unsigned(min, allow_64bit_ops)
                    .and_then(ConstInt::negate)
                    .expect("const construction")
            }
            TypeKind::Nat(size) => {
                let (max, allow_64bit_ops) = match size {
                    NatSize::Nat1 | NatSize::Nat2 | NatSize::Nat4 | NatSize::Nat => (0, false),
                    NatSize::AddressInt => (0, false),
                };

                ConstInt::from_unsigned(max, allow_64bit_ops).expect("const construction")
            }
            TypeKind::Integer => unreachable!("integer should be concrete"),
            TypeKind::Enum(_, _) => {
                // Always zero
                ConstInt::from_unsigned(0, false).expect("const construction")
            }
            TypeKind::Constrained(_, start_bound, _end_bound) => {
                // FIXME: use the correct eval params
                // Just the ordinal value of the start bound
                self.db
                    .evaluate_const(start_bound.clone(), Default::default())
                    .map_err(NotInteger::ConstError)?
                    .ordinal()
                    .ok_or(NotInteger::NotInteger)?
            }
            _ => return Err(NotInteger::NotInteger),
        };

        Ok(value)
    }

    /// Maximum integer value of this type, or `None` if this is not an integer
    pub fn max_int_of(&self) -> Result<ConstInt, NotInteger> {
        let value = match self.kind() {
            TypeKind::Char => {
                // Codepoint can only fit inside of a u8
                ConstValue::Char('\u{FF}')
                    .ordinal()
                    .expect("const construction")
            }
            TypeKind::Boolean => ConstValue::Bool(true)
                .ordinal()
                .expect("const construction"),
            TypeKind::Int(size) => {
                let (max, allow_64bit_ops) = match size {
                    IntSize::Int1 => (0x7F, false),
                    IntSize::Int2 => (0x7FFF, false),
                    IntSize::Int4 | IntSize::Int => (0x7FFF_FFFF, false),
                };

                ConstInt::from_unsigned(max, allow_64bit_ops).expect("const construction")
            }
            TypeKind::Nat(size) => {
                let (max, allow_64bit_ops) = match size {
                    NatSize::Nat1 => (0xFF, false),
                    NatSize::Nat2 => (0xFFFF, false),
                    NatSize::Nat4 => (0xFFFF_FFFF, false),
                    // 0xFFFF_FFFF is reserved as the uninitialized pattern
                    NatSize::Nat => (0xFFFF_FFFE, false),
                    NatSize::AddressInt => (0xFFFF_FFFF, false),
                };

                ConstInt::from_unsigned(max, allow_64bit_ops).expect("const construction")
            }
            TypeKind::Integer => unreachable!("integer should be concrete"),
            TypeKind::Enum(_, variants) => {
                // Always the ordinal of the last variant
                let last_ord = variants
                    .len()
                    .checked_sub(1)
                    .and_then(|last_ord| u64::try_from(last_ord).ok())
                    .ok_or(NotInteger::NotInteger)?;

                ConstInt::from_unsigned(last_ord, false).expect("const construction")
            }
            TypeKind::Constrained(_, start_bound, end_bound) => {
                // FIXME: use the correct eval params
                let eval_params = Default::default();

                match end_bound {
                    EndBound::Expr(end_bound, _) => {
                        // Just the ordinal value of the end bound
                        self.db
                            .evaluate_const(end_bound.clone(), eval_params)
                            .map_err(NotInteger::ConstError)?
                            .ordinal()
                            .ok_or(NotInteger::NotInteger)?
                    }
                    EndBound::Unsized(count) => {
                        // Based on the ordinal value of the start bound
                        let start_bound = self
                            .db
                            .evaluate_const(start_bound.clone(), eval_params)
                            .map_err(NotInteger::ConstError)?
                            .ordinal()
                            .ok_or(NotInteger::NotInteger)?;
                        let count =
                            ConstInt::from_unsigned((*count).into(), eval_params.allow_64bit_ops)
                                .map_err(NotInteger::ConstError)?;

                        // Offset by the gathered count
                        // Sub by 1 since it's an inclusive range
                        count
                            .checked_sub(ConstInt::ONE)
                            .and_then(|count| count.checked_add(start_bound))
                            .map_err(NotInteger::ConstError)?
                    }
                    EndBound::Any => {
                        // FIXME: This should be a non-const error?
                        return Err(NotInteger::NotInteger);
                    }
                }
            }
            _ => return Err(NotInteger::NotInteger),
        };

        Ok(value)
    }
}

// FIXME: replace with just constructing a WrongOperandType error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NotInteger {
    ConstError(ConstError),
    NotInteger,
}

impl<'db, DB: ?Sized + 'db> Clone for TyRef<'db, DB> {
    fn clone(&self) -> Self {
        Self {
            db: self.db,
            id: self.id,
            data: self.data.clone(),
        }
    }
}

/// Aligns `size` up to the next `align` boundary.
/// `align` must be a power of two.
pub fn align_up_to(size: usize, align: usize) -> usize {
    assert!(align.is_power_of_two());
    let mask = align - 1;

    (size + mask) & !mask
}

toc_salsa::create_intern_key!(
    /// Id referencing an interned type.
    pub TypeId;
);

impl TypeId {
    pub fn in_db<'db, DB>(self, db: &'db DB) -> TyRef<'db, DB>
    where
        DB: db::TypeDatabase + ?Sized + 'db,
    {
        TyRef {
            db,
            id: self,
            data: db.lookup_intern_type(self),
        }
    }
}

/// Interned type data
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeData {
    data: Arc<Type>,
}

impl std::ops::Deref for TypeData {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        self.data.deref()
    }
}

impl From<Type> for TypeData {
    fn from(ty: Type) -> Self {
        Self { data: Arc::new(ty) }
    }
}
