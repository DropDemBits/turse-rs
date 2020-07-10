//! Type facilities
//! A type is considered a 'Named' type if it is:
//! - A pointer to another type
//! - A complex or compound type, which includes
//!   - Any type including a range (e.g. 'set', 'array')
//!   - Any type including a grouping of other types
//! Otherwise, the type is considered to be a 'Primative' type
use crate::compiler::ast::{Expr, Identifier};
use std::collections::HashMap;

/// Default string size, in bytes
/// This is the default size for a string if it is not specified
/// Includes the null terminator
pub const DEFAULT_STRING_SIZE: usize = 256;

/// Maximum string size, in bytes
/// Includes the null terminator
pub const MAX_STRING_SIZE: usize = 65536;

lazy_static! {
    static ref PRIMITIVE_TYPE_INFO: HashMap<PrimitiveType, Type> = {
        let mut map = HashMap::new();

        map.insert(
            PrimitiveType::Boolean,
            Type::Primitive {
                kind: PrimitiveType::Boolean,
            },
        );
        map.insert(
            PrimitiveType::Int,
            Type::Primitive {
                kind: PrimitiveType::Int,
            },
        );
        map.insert(
            PrimitiveType::Int1,
            Type::Primitive {
                kind: PrimitiveType::Int1,
            },
        );
        map.insert(
            PrimitiveType::Int2,
            Type::Primitive {
                kind: PrimitiveType::Int2,
            },
        );
        map.insert(
            PrimitiveType::Int4,
            Type::Primitive {
                kind: PrimitiveType::Int4,
            },
        );
        map.insert(
            PrimitiveType::Int8,
            Type::Primitive {
                kind: PrimitiveType::Int8,
            },
        );
        map.insert(
            PrimitiveType::Nat,
            Type::Primitive {
                kind: PrimitiveType::Nat,
            },
        );
        map.insert(
            PrimitiveType::Nat1,
            Type::Primitive {
                kind: PrimitiveType::Nat1,
            },
        );
        map.insert(
            PrimitiveType::Nat2,
            Type::Primitive {
                kind: PrimitiveType::Nat2,
            },
        );
        map.insert(
            PrimitiveType::Nat4,
            Type::Primitive {
                kind: PrimitiveType::Nat4,
            },
        );
        map.insert(
            PrimitiveType::Nat8,
            Type::Primitive {
                kind: PrimitiveType::Nat8,
            },
        );
        map.insert(
            PrimitiveType::LongInt,
            Type::Primitive {
                kind: PrimitiveType::LongInt,
            },
        );
        map.insert(
            PrimitiveType::LongNat,
            Type::Primitive {
                kind: PrimitiveType::LongNat,
            },
        );
        map.insert(
            PrimitiveType::Real,
            Type::Primitive {
                kind: PrimitiveType::Real,
            },
        );
        map.insert(
            PrimitiveType::Real4,
            Type::Primitive {
                kind: PrimitiveType::Real4,
            },
        );
        map.insert(
            PrimitiveType::Real8,
            Type::Primitive {
                kind: PrimitiveType::Real8,
            },
        );
        map.insert(
            PrimitiveType::String_,
            Type::Primitive {
                kind: PrimitiveType::String_,
            },
        );
        map.insert(
            PrimitiveType::Char,
            Type::Primitive {
                kind: PrimitiveType::Char,
            },
        );
        map.insert(
            PrimitiveType::AddressInt,
            Type::Primitive {
                kind: PrimitiveType::AddressInt,
            },
        );
        map.insert(
            PrimitiveType::Nil,
            Type::Primitive {
                kind: PrimitiveType::Nil,
            },
        );

        map
    };
}

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
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
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
    /// Primitive type. Only used during type resolution, and is never stored
    /// in the unit type table
    Primitive { kind: PrimitiveType },
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

    /// Converts the `type_ref` into the associated type info
    pub fn ref_to_type(&self, type_ref: &TypeRef) -> Option<&Type> {
        match type_ref {
            TypeRef::Primitive(kind) => match kind {
                PrimitiveType::CharN(_) | PrimitiveType::StringN(_) => {
                    panic!("Sized char sequences are supposed to be in the type table")
                }
                _ => Some(PRIMITIVE_TYPE_INFO.get(kind).unwrap()),
            },
            TypeRef::Named(type_id) => Some(self.get_type(*type_id)),
            _ => None,
        }
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

// Helpers for comparing types

pub fn is_error(type_ref: &TypeRef) -> bool {
    matches!(type_ref, TypeRef::TypeError)
}

/// Checks if the given `type_ref` references an unsized string type (String_)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_string(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(kind, PrimitiveType::String_),
        _ => false,
    }
}

/// Checks if the given `type_ref` references a sized char type (CharN(x))
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_charn(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(kind, PrimitiveType::CharN(_)),
        _ => false,
    }
}

/// Checks if the given `type_ref` references a string-class type (String_ & StringN(x))
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_string_type(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => {
            matches!(kind, PrimitiveType::String_ | PrimitiveType::StringN(_))
        }
        _ => false,
    }
}

/// Checks if the given `type_ref` references a char sequence class type (String_, StringN(x), CharN(x))
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_char_seq_type(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => {
            matches!(kind, PrimitiveType::String_ | PrimitiveType::StringN(_) | PrimitiveType::CharN(_))
        }
        _ => false,
    }
}

/// Checks if the given `type_ref` references a sized char sequence class type (StringN(x), CharN(x))
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_sized_char_seq_type(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => {
            matches!(kind, PrimitiveType::StringN(_) | PrimitiveType::CharN(_))
        }
        _ => false,
    }
}

pub fn get_sized_len(type_ref: &TypeRef) -> Option<usize> {
    match type_ref {
        TypeRef::Primitive(kind) => match kind {
            PrimitiveType::StringN(s) | PrimitiveType::CharN(s) => Some(*s),
            _ => None,
        },
        _ => None,
    }
}

/// Checks if the given `type_ref` references a real type (real, real4, real8)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_real(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(
            kind,
            PrimitiveType::Real | PrimitiveType::Real4 | PrimitiveType::Real8
        ),
        _ => false,
    }
}

/// Checks if the given `type_ref` references an integer class type (int, nat, long int, long nat, addressint)
pub fn is_integer_type(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(
            kind,
            PrimitiveType::Int
                | PrimitiveType::Int1
                | PrimitiveType::Int2
                | PrimitiveType::Int4
                | PrimitiveType::Nat
                | PrimitiveType::Nat1
                | PrimitiveType::Nat2
                | PrimitiveType::Nat4
                | PrimitiveType::LongInt
                | PrimitiveType::Int8
                | PrimitiveType::LongNat
                | PrimitiveType::Nat8
                | PrimitiveType::AddressInt
        ),
        _ => false,
    }
}

/// Checks if the given `type_ref` references an int (long int, int, int1, int2, int4, int8)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_int(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(
            kind,
            PrimitiveType::Int
                | PrimitiveType::Int1
                | PrimitiveType::Int2
                | PrimitiveType::Int4
                | PrimitiveType::LongInt
                | PrimitiveType::Int8
        ),
        _ => false,
    }
}

/// Checks if the given `type_ref` references a nat (addressint, long nat, nat, nat1, nat2, nat4, nat8)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_nat(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(
            kind,
            PrimitiveType::Nat
                | PrimitiveType::Nat1
                | PrimitiveType::Nat2
                | PrimitiveType::Nat4
                | PrimitiveType::LongNat
                | PrimitiveType::Nat8
                | PrimitiveType::AddressInt
        ),
        _ => false,
    }
}

/// Checks if the given `type_ref` references number type (real, int, nat, long int, long nat)
pub fn is_number_type(type_ref: &TypeRef) -> bool {
    is_real(type_ref) || is_integer_type(type_ref)
}

/// Converts a primitive into a type info
pub fn primitive_to_type_info(type_ref: &TypeRef) -> Option<Type> {
    match type_ref {
        TypeRef::Primitive(kind) => Some(Type::Primitive { kind: *kind }),
        _ => None,
    }
}

/// Gets the common type between the two given type refs
pub fn common_type<'a>(
    lhs: &'a TypeRef,
    rhs: &'a TypeRef,
    type_table: &'_ TypeTable,
) -> Option<&'a TypeRef> {
    // TODO: Between strings, stringNs, charNs, chars, sets, classes, pointers, etc.
    if lhs == rhs {
        // Both are the same type, so they're both in common with eachother
        Some(lhs)
    } else if (is_real(lhs) && is_number_type(rhs)) || (is_number_type(lhs) && is_real(rhs)) {
        // Number types get promoted to real types if any 'real' exists
        if is_real(lhs) {
            Some(lhs)
        } else {
            Some(rhs)
        }
    } else if (is_int(lhs) && is_integer_type(rhs)) || (is_integer_type(lhs) && is_int(rhs)) {
        // Integer types get promoted to int types if any 'int' exists
        if is_int(lhs) {
            Some(lhs)
        } else {
            Some(rhs)
        }
    } else if is_nat(lhs) && is_nat(rhs) {
        // Common nat types produce 'nat's
        Some(lhs)
    } else {
        // No common type
        None
    }
}

/// Checks if `rvalue` is assignable into `lvalue`
///
/// # Root types
/// Most types have their root types defined as themselves. However, range types and string-class types have different root types.
/// - All string-class types (i.e. `string` and `string(n)`) have `string` as the root type.
/// - All integer range types (e.g. `3 .. 20`) have `int` as the root type (i.e. all `int`s are assignable to the given range).
/// - All boolean range types (e.g. `false .. true`) have `boolean` as the root type (i.e. all `boolean`s are assignable to the given range).
///
/// # Assignability rules
/// - If two types (after de-aliasing) have the same `type_id` (i.e, have the same root definition) \
/// - If two types (after de-aliasing) have the same or equvalent root type \
/// - If `lvalue` is 'real' and `rvalue` is either `real` or an integer-class type (`rvalue` is converted into an int) \
/// - If `lvalue` is a range and `rvalue` both is the same type kind, as well as existing in the given range \
/// - If `lvalue` is a `char` and `rvalue` is a `char(1)` (or vice versa)
/// - If `lvalue` is a `char` and `rvalue` is a char sequence of length 1 (i.e. `string(1)` or `char(1)`)
/// - If `lvalue` is a string-class type and `rvalue` is a `char` (producing a string of length 1)
/// - If `lvalue` is a `char(n)` and `rvalue` is a `string(n)` (`rvalue` gets converted into a `string(n)`)
/// - If `lvalue` and `rvalue` are pointers to classes and they are the same class or share a common ancestor
pub fn is_assignable_to(lvalue: &TypeRef, rvalue: &TypeRef, type_table: &TypeTable) -> bool {
    if rvalue == lvalue {
        // Same types are assignable

        // Also Covers:
        // - Set assignment
        // - Array assignment
        // - Function/Procedure assignment
        //
        true
    } else if is_integer_type(lvalue) && is_integer_type(rvalue) {
        // Integer-class types are mutually assignable to eachother
        true
    } else if is_real(lvalue) && is_number_type(rvalue) {
        // Number-class types are assignable into 'real' types
        true
    } else if is_string(lvalue) && is_char_seq_type(rvalue) {
        // Char Sequence types are assignable into unsized 'string's
        true
    } else if is_charn(lvalue) && is_char_seq_type(rvalue) {
        if is_charn(rvalue) {
            // Must check length
            let lvalue_len = get_sized_len(lvalue).unwrap();
            let rvalue_len = get_sized_len(rvalue).unwrap();

            // Assignable if lvalue is a char(*) or if the rvalue can be contained inside of the lvalue
            lvalue_len == 0 || lvalue_len >= rvalue_len
        } else {
            // Is string or stringN(), okay to assign (checked at runtime)
            true
        }
    } else {
        false
    }
}
