//! Type facilities
/// Unique type reference for a type
#[derive(Debug)]
pub enum TypeRef {
    /// Unknown type reference, to be resolved in type analysis
    Unknown,
    /// Reference to a primitive type
    Primitive(PrimitiveType),
    /// Reference to a named type, with the unit type id included
    Named(usize),
}

/// Enum of basic primitive types
/// Not included in the unit type table
#[derive(Debug)]
pub enum PrimitiveType {
    Int,
}
