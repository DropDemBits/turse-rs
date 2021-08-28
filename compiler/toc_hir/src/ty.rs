//! Type related HIR nodes

use std::num::NonZeroU32;

use toc_span::SpanId;

use crate::body;

/// An interned reference to a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeId(pub TypeIndex);
pub type TypeIndex = NonZeroU32;

/// An interner for HIR types
pub trait TypeInterner {
    fn intern_type(&self, ty: Type) -> TypeId;

    fn lookup_type(&self, type_id: TypeId) -> std::sync::Arc<Type>;
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
