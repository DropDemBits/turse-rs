//! Type related HIR nodes

use crate::expr;

crate::hir_id_wrapper!(TypeId);

#[derive(Debug, PartialEq)]
pub enum Type {
    /// Error Type, only used to represent invalid code
    Missing,
    /// Primitive Type
    Primitive(Primitive),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeqLength {
    Dynamic,
    Expr(expr::ExprId),
}
