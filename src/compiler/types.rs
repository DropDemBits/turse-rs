//! Type facilities
//! A type is considered a 'Named' type if it is:
//! - A pointer to another type
//! - A complex or compound type, which includes
//!   - Any type including a range (e.g. 'set', 'array')
//!   - Any type including a grouping of other types
//! Otherwise, the type is considered to be a 'Primative' type
use crate::compiler::ast::{Expr, Identifier};

/// Default string size, in bytes
/// This is the default size for a string if it is not specified
/// Includes the null terminator
pub const DEFAULT_STRING_SIZE: usize = 256;

/// Maximum string size, in bytes
/// Includes the null terminator
pub const MAX_STRING_SIZE: usize = 65536;

/// Unique type reference for a type
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeRef {
    /// Unknown type reference, to be resolved in type resolution
    Unknown,
    /// Error during type resolution or parsing
    TypeError,
    /// Reference to a primitive type
    Primitive(PrimitiveType),
    /// Reference to a named type, with the unit type
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
    /// Variable-sized string of ASCII characters (u8's)
    /// The default size of a string is `DEFAULT_STRING_SIZE`, but can grow to
    // accommodate larger strings
    String_,
    /// Fixed-size string of ASCII characters (u8's)
    /// `usize` is the maximum length storable in the string
    /// A size of zero indicates a dynamic length type, or a '*' size specifier
    /// Assignable to other StrN's of the same or larger size
    StringN(usize),
    /// A single ASCII character
    Char,
    /// Multiple ASCII characters (u8's)
    /// `usize` is the maximum length storable in the string
    /// A size of zero indicates a dynamic length type, or a '*' size specifier
    /// Assignable to other CharN's of the same or larger size
    CharN(usize),
    /// A type able to store a pointer address
    /// The size of an AddressInt varies between compiling for 32-bit or 64-bit machines
    /// If compiling for 32-bit, the pointer size is 4 bytes
    /// If compiling for 64-bit, the pointer size is 8 bytes
    AddressInt,
    /// General nil pointer type
    Nil,
}

/// Parameter definition
#[derive(Debug)]
pub struct FuncParam(pub String, pub TypeRef);

/// Base Type Root
#[derive(Debug)]
pub enum Type {
    /// Alias to another Type
    Alias {
        /// other Type aliased by the current Type
        to: TypeRef,
        /// TypeRef to the end of the aliasing chain
        derived: TypeRef,
    },
    /// Array Type
    Array {
        /// Ranges for the array
        ranges: Vec<TypeRef>,
        /// Element type of the array
        elem_type: TypeRef,
        /// If the array can be resized at runtime
        is_flexible: bool,
    },
    /// Forward reference to the type with the given name
    Forward { name: String },
    /// Function / Procedure definition \
    /// Having both as Options allows differentiation between parameter and
    /// parameterless declarations, and between functions and procedures
    Function {
        /// Parameter specification for the function
        params: Option<Vec<FuncParam>>,
        /// Result type for the function
        result: Option<TypeRef>,
    },
    /// A named type.
    /// This type is resolved into the real type at type resolution stage,
    /// as imports are resovled before type resolution
    Named { ident: Identifier },
    /// Pointer to a given TypeRef
    Pointer { to: TypeRef },
    /// Inclusive range type, encoding `start` .. `end` and `start` .. * \
    /// `start` must evaluate to be less than or equal to `end` \
    /// Expressions are used, as dynamic arrays have an upper bound
    /// that can be a runtime dependent value
    Range {
        /// Start of the range
        start: Expr,
        /// End of the range
        /// None is equivalent to specifiying *
        end: Option<Expr>,
    },
}

/// Table of all named references defined in the scope
#[derive(Debug)]
pub struct TypeTable {
    /// Next type id
    next_id: usize,
    /// Type Table
    types: Vec<Type>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            types: vec![],
        }
    }

    /// Declares a new type in the current unit, returning the type id
    pub fn declare_type(&mut self, type_info: Type) -> usize {
        let id = self.next_id;
        self.next_id = self
            .next_id
            .checked_add(1)
            .expect("Too many types defined in the unit");
        self.types.push(type_info);
        id
    }

    /// Gets a reference to a defined type
    pub fn get_type(&self, type_id: usize) -> &Type {
        &self.types[type_id]
    }

    /// Gets a mutable reference to a defined type
    pub fn get_type_mut(&mut self, type_id: usize) -> &mut Type {
        &mut self.types[type_id]
    }

    /// Checks if the given type is an alias for another type
    pub fn is_alias(&self, type_id: usize) -> bool {
        if let Type::Alias { .. } = self.get_type(type_id) {
            true
        } else {
            false
        }
    }
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
