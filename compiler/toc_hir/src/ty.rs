//! Type related HIR nodes

use std::convert::TryInto;

use indexmap::IndexSet;
use toc_span::SpanId;

pub use crate::ids::TypeId;

use crate::body;
use crate::ids::TypeIndex;

/// An interner for HIR types
#[derive(Debug, Default, PartialEq, Eq)]
pub struct TypeTable {
    types: IndexSet<Type>,
}

impl TypeTable {
    /// Interns the given type
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
    Dynamic,
    /// Sequence length is an expression
    /// that might be computable at compile time.
    Expr(body::BodyId),
}
