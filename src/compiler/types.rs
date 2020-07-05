//! Type facilities
//! A type is considered a 'Named' type if it is:
//! - A pointer to another type
//! - A complex or compound type, which includes
//!   - Any type including a range (e.g. 'set', 'array')
//!   - Any type including a grouping of other types
//! Otherwise, the type is considered to be a 'Primative' type

/// Minimum string size, in bytes
/// Includes the null terminator
pub const MIN_STRING_LENGTH: usize = 256;

/// Maximum string size, in bytes
/// Includes the null terminator
pub const MAX_STRING_LENGTH: usize = 65536;

/// Unique type reference for a type
#[derive(Debug, PartialEq, Clone, Copy)]
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrimitiveType {
    // Base primative types
    /// Boolean type
    Boolean,
    /// Basic integer type, equivalent to 'i32'
    Int,
    /// Sized integer type, equivalent to 'i8'
    Int1,
    /// Sized integer type, equivalent to 'i16'
    Int2,
    /// Sized integer type, equivalent to 'i32'
    /// Allows for the assignment of 0x80000000, or UNINIT_INT
    Int4,
    /// Sized integer type, equivalent to 'i64'
    /// Allows for the assignment of 0x8000000080000000, or UNINIT_LONG_INT
    Int8,
    /// Basic unsigned integer type, equivalent to 'u32'
    Nat,
    /// Sized unsigned integer type, equivalent to 'u8'
    Nat1,
    /// Sized unsigned integer type, equivalent to 'u16'
    Nat2,
    /// Sized unsigned integer type, equivalent to 'u32'
    /// Allows for the assignment of 0xFFFFFFFF, or UNINIT_NAT
    Nat4,
    /// Sized unsigned integer type, equivalent to 'u64'
    /// Allows for the assignment of 0xFFFFFFFFFFFFFFFF, or UNINIT_LONG_NAT
    Nat8,
    /// Basic, checked long integer type, equivalent to 'i64'
    LongInt,
    /// Basic, checked long natural type, equivalent to 'u64'
    LongNat,
    /// Basic real type, equivalent to 'f64'
    Real,
    /// Sized real type, equivalent to 'f32'
    Real4,
    /// Sized real type, equivalent to 'f64'
    /// Allows for the assignment of the denormalized 0x800000000800000000, or UNINIT_REAL
    Real8,
    /// String of ASCII characters (u8's)
    /// Assignable to other StrN's of the same or larger size
    StringN(usize),
    /// Single or multiple ASCII characters (u8's)
    /// Assignable to other CharN's of the same or larger size
    /// The primitive type "char" is represented as a CharN(1)
    CharN(usize),
    /// A type able to store a pointer address
    /// The size of an AddressInt varies between compiling for 32-bit or 64-bit machines
    /// If compiling for 32-bit, the pointer size is 4 bytes
    /// If compiling for 64-bit, the pointer size is 8 bytes
    AddressInt,
    /// General nil pointer type
    Nil,
}

// --- Helpers for deriving the appropriate type ---
/// Makes the appropriate `StringN` type for the given `String`
pub fn get_string_kind(s: &String) -> PrimitiveType {
    let size = s.bytes().count();

    PrimitiveType::StringN(size)
}

/// Makes the appropriate `CharN` type for the given `String`
pub fn get_char_kind(s: &String) -> PrimitiveType {
    let size = s.bytes().count();

    PrimitiveType::CharN(size)
}
