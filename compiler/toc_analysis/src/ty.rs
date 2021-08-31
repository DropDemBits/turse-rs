//! Representation of Turing types

use std::fmt::Debug;
use std::sync::Arc;

use crate::db;

mod lower;
mod pretty;
pub(crate) mod query;
pub mod rules;

toc_salsa::create_intern_key!(
    /// Id referencing an interned type.
    pub TypeId;
);

impl TypeId {
    pub fn in_db<'db, DB>(self, db: &'db DB) -> TyRef<'db, DB>
    where
        DB: db::TypeDatabase + ?Sized + 'db,
    {
        TyRef { db, id: self }
    }
}

/// Interened type data
// ???(ra bug): Can't rename
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
#[derive(Debug, PartialEq, Eq)]
pub struct TyRef<'db, DB: ?Sized + 'db> {
    db: &'db DB,
    id: TypeId,
}

impl<'db, DB: ?Sized + 'db> Clone for TyRef<'db, DB> {
    fn clone(&self) -> Self {
        Self {
            db: self.db,
            id: self.id,
        }
    }
}

impl<'db, DB: ?Sized + 'db> Copy for TyRef<'db, DB> {}

/// Wrapper type for getting a [`TypeKind`] from a [`TyRef`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyRefKind(TypeData);

impl std::ops::Deref for TyRefKind {
    type Target = TypeKind;

    fn deref(&self) -> &Self::Target {
        self.0.kind()
    }
}

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
    Fixed(toc_hir::body::BodyId),
}
