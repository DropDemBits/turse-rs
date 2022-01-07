//! Representation of Turing types

use std::fmt::Debug;
use std::sync::Arc;

use toc_span::Span;

use crate::const_eval::{Const, ConstInt, ConstResult};

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
    /// Reference type.
    ///
    /// This type does not appear in syntax (except for parameter binding),
    /// and is an implementation detail.
    Ref(Mutability, TypeId),
}

// Other types to add:
// - array
// - range
// - enum
// - union
// - record
// - set
// - pointer
// - alias
// - function (fcn/proc/process)
// - condition
// - collection

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Var,
}

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
    pub fn fixed_len<T: crate::db::ConstEval + ?Sized>(
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
    pub fn align_of(&self) -> Option<usize> {
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
            TypeKind::Nat(NatSize::AddressInt) => 4,
            TypeKind::Real(RealSize::Real4) => 4,
            TypeKind::Real(RealSize::Real8) => 4,
            TypeKind::Real(RealSize::Real) => 4,
            TypeKind::Integer => return None,
            TypeKind::Char => 1,
            TypeKind::String => 1,
            TypeKind::CharN(_) => 1,
            TypeKind::StringN(_) => 1,
            TypeKind::Ref(_, _) => return None,
        };

        Some(align_of)
    }

    pub fn size_of(&self) -> Option<usize> {
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
            TypeKind::Nat(NatSize::AddressInt) => 4,
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
            TypeKind::Integer | TypeKind::Ref(_, _) | TypeKind::Error => return None,
        };

        Some(size_of)
    }

    pub fn length_of(&self) -> Option<usize> {
        let length = match self.kind() {
            TypeKind::String => {
                // max chars (excluding null terminator)
                255
            }
            TypeKind::CharN(seq_size) | TypeKind::StringN(seq_size) => {
                let char_len = seq_size
                    .fixed_len(self.db, Span::default())
                    .ok()
                    .flatten()?;

                (char_len.into_u32()?) as usize
            }
            TypeKind::Ref(_, _) | TypeKind::Error => unreachable!(),
            _ => return None,
        };

        Some(length)
    }
}

/// Aligns `size` up to the next `align` boundary.
/// `align` must be a power of two.
pub fn align_up_to(size: usize, align: usize) -> usize {
    assert!(align.is_power_of_two());
    let mask = align - 1;

    (size + mask) & !mask
}
