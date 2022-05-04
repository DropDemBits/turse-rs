//! Type related HIR nodes

use std::convert::TryInto;

use indexmap::{IndexMap, IndexSet};
use toc_span::{SpanId, Spanned};

pub use crate::ids::{FieldId, TypeId};

use crate::{
    body,
    ids::TypeIndex,
    symbol::{self, Symbol},
};

/// An interner for HIR types
#[derive(Debug, Default, PartialEq, Eq)]
pub struct TypeTable {
    types: IndexSet<Type>,
}

// FIXME: "Unintern" HIR types
// They're not actually interned because they have spans attached to them, effectively making them all unique
impl TypeTable {
    /// Interns the given type.
    /// Note: not actually interned because of spans.
    pub(crate) fn intern_type(&mut self, ty: Type) -> TypeId {
        let id = self.types.insert_full(ty).0;
        let raw = id
            .wrapping_add(1)
            .try_into()
            .ok()
            .and_then(TypeIndex::new)
            .expect("too many types");
        TypeId(raw)
    }

    /// Looks up the given type
    pub(crate) fn lookup_type(&self, type_id: TypeId) -> &Type {
        self.types.get_index(type_id.0.get() as usize - 1).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub span: SpanId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// Error Type, only used to represent invalid code
    Missing,
    /// Primitive Type
    Primitive(Primitive),
    /// Alias Type
    Alias(Alias),
    /// Constrained Value Type
    Constrained(Constrained),
    /// Enum Type
    Enum(Enum),
    /// Array type
    Array(Array),
    /// Set type
    Set(Set),
    /// Pointer type
    Pointer(Pointer),
    /// Subprogram Type
    Subprogram(Subprogram),
    /// Void Type, returned from `procedures` and `processes`
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Int,
    Int1,
    Int2,
    Int4,
    Nat,
    Nat1,
    Nat2,
    Nat4,
    Real,
    Real4,
    Real8,
    Boolean,
    AddressInt,
    Char,
    String,
    SizedChar(SeqLength),
    SizedString(SeqLength),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SeqLength {
    /// Sequence length is decided at runtime.
    Any,
    /// Sequence length is an expression that might be computable at compile time.
    Expr(body::BodyId),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Alias {
    /// [`LocalDefId`](symbol::LocalDefId) to start working through
    pub base_def: Spanned<symbol::LocalDefId>,
    /// Names of each segment to lookup
    pub segments: Vec<Spanned<Symbol>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Constrained {
    /// Minimum accepted value
    pub start: body::BodyId,
    /// Maximum accepted value, based on a [`ConstrainedEnd`]
    pub end: ConstrainedEnd,
}

/// Possible ending values of a constrained range
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstrainedEnd {
    /// From an expression
    Expr(body::BodyId),
    /// An unsized bound, where the element count is taken from
    /// the closest `init` initializer
    Unsized(Spanned<Option<u32>>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Enum {
    /// Definition uniquely identifying this type
    pub def_id: symbol::LocalDefId,
    /// Variants on this enum
    pub variants: Vec<symbol::LocalDefId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Array {
    /// Specification of the array's size
    pub sizing: ArraySize,
    /// Types of each of the array's dimensions
    pub ranges: Vec<TypeId>,
    /// Type of the array's elements
    pub elem_ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArraySize {
    /// Array size is not fixed, and zero-sized ranges are allowed.
    /// Initialization from dynamic values is implied to be allowed.
    Flexible,
    /// Array size is fixed, but allowed to be derived from runtime values
    MaybeDyn,
    /// Array size is fixed, and must be known at compile-time
    Static,
}

impl ArraySize {
    /// If the size specification allows dynamic values in the size computation
    pub fn allow_dyn(self) -> bool {
        matches!(self, Self::Flexible | Self::MaybeDyn)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Set {
    /// Definition uniquely identifying this type
    pub def_id: symbol::LocalDefId,
    /// Type of the set's elements
    pub elem_ty: TypeId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Pointer {
    /// Checkedness of this pointer
    pub checked: Checked,
    /// Target type
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Checked {
    /// Pointer with additional correctness data
    Checked,
    /// Raw pointer, no extra metadata
    Unchecked,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Subprogram {
    pub kind: symbol::SubprogramKind,
    pub param_list: Option<Vec<Parameter>>,
    pub result_ty: TypeId,
}

/// Parameter for a [`Subprogram`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
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
    /// Pass by reference, with the specified mutability
    Reference(symbol::Mutability),
}

/// Owner of a [`Type`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeOwner {
    Item(crate::item::ItemId),
    Type(TypeId),
    // exprs can also own types, but nothing right now impls that
}

/// Table of type owners
#[derive(Debug, Default, PartialEq, Eq)]
pub struct TypeOwners {
    owners: IndexMap<TypeId, TypeOwner>,
}

impl TypeOwners {
    pub fn add_owner(&mut self, type_id: TypeId, owner: TypeOwner) {
        self.owners.insert(type_id, owner);
    }

    pub fn lookup_owner(&self, type_id: TypeId) -> TypeOwner {
        self.owners
            .get(&type_id)
            .copied()
            .expect("missing type owner")
    }
}
