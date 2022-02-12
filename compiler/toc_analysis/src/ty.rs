//! Representation of Turing types

use std::fmt::Debug;
use std::sync::Arc;

use toc_hir::symbol::{self, DefId};
use toc_span::Span;

use crate::const_eval::{Const, ConstError, ConstInt};

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
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum IntSize {
    Int1,
    Int2,
    Int4,
    /// Initialization checked version of `Int4`
    Int,
}

/// Size variant of a Nat
#[derive(Debug, PartialEq, Eq, Hash)]
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
    /// Runtime sized (only accepted for parameters)
    Dynamic,
    /// Fixed, compile-time size
    Fixed(Const),
}

impl SeqSize {
    /// Tries to compute a compile-time size from this sequence size.
    ///
    /// If this is a dynamic length sequence, `NotFixedLen::DynSize` is produced.
    /// If an error occurs during length computation, `NotFixedLen::ConstError(err)` is produced.
    pub fn fixed_len<T: crate::db::ConstEval + ?Sized>(
        &self,
        db: &T,
        span: Span,
    ) -> Result<ConstInt, NotFixedLen> {
        let size = match self {
            SeqSize::Dynamic => return Err(NotFixedLen::DynSize),
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
    /// Trying to compute from a dynamically sized sequence
    DynSize,
    /// Error while trying to evaluate the sequence
    ConstError(ConstError),
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
            // Defer to the aliased type
            TypeKind::Alias(_, base_ty) => return base_ty.in_db(self.db).align_of(),
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
            // Defer to the aliased type
            TypeKind::Alias(_, base_ty) => return base_ty.in_db(self.db).align_of(),
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

    /// Minimum integer value of this type, or `None` if this is not an integer
    pub fn min_int_of(&self) -> Option<ConstInt> {
        let value = match self.kind() {
            TypeKind::Char => ConstInt::from_unsigned(0, false).expect("const construction"),
            TypeKind::Boolean => {
                // acts like 0 in a range
                ConstInt::from_unsigned(0, false).expect("const construction")
            }
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
            _ => return None,
        };

        Some(value)
    }

    /// Maximum integer value of this type, or `None` if this is not an integer
    pub fn max_int_of(&self) -> Option<ConstInt> {
        let value = match self.kind() {
            TypeKind::Char => {
                // Can only hold 1 UTF-8 code scalar
                ConstInt::from_unsigned(u8::MAX.into(), false).expect("const construction")
            }
            TypeKind::Boolean => {
                // acts like 1 in a range
                ConstInt::from_unsigned(1, false).expect("const construction")
            }
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
            _ => return None,
        };

        Some(value)
    }
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
