//! Type facilities
/// Unique type handle for a type
pub type TypeRef = usize;

// Primitive Types
#[derive(Debug, PartialEq)]
pub enum PrimativeType {
    Unknown,
    Int,
    IntN(usize),
    Nat,
    NatN(usize),
    Real,
    RealN(usize),
    String_,
    StringN(usize),
    CharN(usize),
}
