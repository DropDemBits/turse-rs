//! Type facilities
//! A type is considered a 'Named' type if it is:
//! - A pointer to another type
//! - A complex or compound type, which includes
//!   - Any type including a range (e.g. 'set', 'array')
//!   - Any type including a grouping of other types
//! Otherwise, the type is considered to be a 'Primative' type
use crate::ast::expr::{BinaryOp, Expr};
use crate::value::{self, Value};

use std::collections::HashMap;
use std::convert::TryFrom;

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
    /// Unknown type reference, to be resolved in the validator stage
    Unknown,
    /// Error during type validation or parsing
    TypeError,
    /// Reference to a primitive type
    Primitive(PrimitiveType),
    /// Reference to a named type, with the unit type
    Named(usize),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum SequenceSize {
    /// Compile-time expression, resolved to Size at validator time.
    /// Points into the type table.
    CompileExpr(usize),
    /// Constant parsed. 0 indicates any length
    Size(usize),
}

/// Enum of basic primitive types
/// Not included in the unit type table
#[derive(Debug, PartialEq, Copy, Clone)]
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
    /// Ambiguious int/nat type, produced by literals.
    /// Converts into the int or nat of the appropriate size, by default into
    /// an int.
    IntNat,
    /// Basic real type, equivalent to 'f64'
    Real,
    /// Sized real type, equivalent to 'f32'
    Real4,
    /// Sized real type, equivalent to 'f64'
    /// Allows for the assignment of the denormalized 0x800000000_800000000, or
    /// UNINIT_REAL
    Real8,
    /// Variable-sized string of ASCII characters (u8's)
    /// The default size of a string is `DEFAULT_STRING_SIZE`, but can grow to
    // accommodate larger strings
    String_,
    /// Fixed-size string of ASCII characters (u8's)
    /// `SequenceSize` is the maximum length storable in the string
    /// A size of zero indicates a dynamic length type, or a '*' size specifier
    /// Assignable to other StrN's of the same or larger size
    StringN(SequenceSize),
    /// A single ASCII character
    Char,
    /// Multiple ASCII characters (u8's)
    /// `SequenceSize` is the maximum length storable in the string
    /// A size of zero indicates a dynamic length type, or a '*' size specifier
    /// Assignable to other CharN's of the same or larger size
    CharN(SequenceSize),
    /// A type able to store a pointer address
    /// The size of an AddressInt varies between compiling for 32-bit or 64-bit machines
    /// If compiling for 32-bit, the pointer size is 4 bytes
    /// If compiling for 64-bit, the pointer size is 8 bytes
    AddressInt,
    /// General nil pointer type
    Nil,
}

/// Parameter definition
/// Two parameter definitions (`ParamDef`'s) are equivalent if, and only if, all
/// fields except for name are equivalent.
#[derive(Debug, Clone)]
pub struct ParamDef {
    /// The name of the parameter
    pub name: String,
    /// The type_spec for the parameter
    pub type_spec: TypeRef,
    // Whether to pass the parameter by reference, allowing the function to modify the value (specified by "var")
    pub pass_by_ref: bool,
    // Whether to bind the parameter into a register (specified by "register")
    pub bind_to_register: bool,
    /// Whether to coerece the type of the input argument into binding the declared type
    pub force_type: bool,
}

impl PartialEq for ParamDef {
    fn eq(&self, other: &Self) -> bool {
        self.type_spec == other.type_spec
            && self.pass_by_ref == other.pass_by_ref
            && self.bind_to_register == other.bind_to_register
            && self.force_type == other.force_type
    }
}

/// Base Type Root
#[derive(Debug, Clone)]
pub enum Type {
    /// Alias to another Type
    Alias {
        /// other Type aliased by the current Type
        to: TypeRef,
    },
    /// Array Type
    Array {
        /// Ranges for the array
        ranges: Vec<TypeRef>,
        /// Element type of the array
        element_type: TypeRef,
        /// If the array can be resized at runtime
        is_flexible: bool,
        /// If the array has an upper bound based on the initializing expression
        is_init_sized: bool,
    },
    /// Enum Type
    Enum {
        /// Valid enumeration fields, and the associated enum field
        fields: HashMap<String, TypeRef>,
    },
    /// Enum field type
    EnumField {
        /// Reference to the base enum type.
        /// Always points to the dealiased type
        enum_type: TypeRef,
        /// The ordinal value of the field
        ordinal: usize,
    },
    /// Forward reference to a type
    Forward {
        /// If the reference has been resolved in the current unit
        is_resolved: bool,
    },
    /// Function / Procedure definition \
    /// Having both as Options allows differentiation between parameter and
    /// parameterless declarations, and between functions and procedures
    Function {
        /// Parameter specification for the function
        params: Option<Vec<ParamDef>>,
        /// Result type for the function
        result: Option<TypeRef>,
    },
    /// Pointer to a given TypeRef
    Pointer {
        /// The pointed-to type
        to: TypeRef,
        /// If the pointer is unchecked
        is_unchecked: bool,
    },
    /// Inclusive range type, encoding `start` .. `end` and `start` .. * \
    /// `start` must evaluate to be less than or equal to `end` \
    /// Expressions are used, as dynamic arrays have an upper bound
    /// that can be a runtime dependent value
    Range {
        /// Start of the range
        start: Box<Expr>,
        /// End of the range
        /// None is equivalent to specifiying *
        end: Option<Box<Expr>>,
        /// Base type for the range.
        /// Can be an int, enum type, char, or boolean, depending on the range evaluation.
        /// This is always a de-aliased type.
        base_type: TypeRef,
        /// Size of the range, in elements
        /// May or may not be computed
        size: Option<usize>,
    },
    /// A reference to a named type.
    /// This type is resolved into the corresponding type at the validation stage,
    /// as imports are resovled before validation
    Reference { expr: Box<Expr> },
    /// Set of values in a given range.
    /// The start and end expressions of the range must be compile-time evaluable.
    Set { range: TypeRef },
    /// Expression holding the size of a Char(n) or a String(n)
    SizeExpr { expr: Box<Expr> },
}

/// Table of all named references defined in the scope
#[derive(Debug, Default)]
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

    /// Replaces an existing type in the unit with the given type info
    pub fn replace_type(&mut self, replace_id: usize, type_info: Type) {
        self.types[replace_id] = type_info;
    }

    /// Converts the `type_ref` into the corresponding type info
    pub fn type_from_ref(&self, type_ref: &TypeRef) -> Option<&Type> {
        if let TypeRef::Named(type_id) = type_ref {
            Some(self.get_type(*type_id))
        } else {
            None
        }
    }

    /// Gets a reference to a defined type
    pub fn get_type(&self, type_id: usize) -> &Type {
        &self.types[type_id]
    }

    /// Checks if the given type is an indirect alias for another type.
    /// This includes both Alias and Reference types.
    pub fn is_indirect_alias(&self, type_id: usize) -> bool {
        matches!(self.get_type(type_id), Type::Alias{ .. } | Type::Reference { .. })
    }
}

// --- Helpers for deriving the appropriate type ---
/// Makes the appropriate `StringN` type for the given `String`
pub fn get_string_kind(s: &str) -> PrimitiveType {
    let size = s.bytes().count();

    PrimitiveType::StringN(SequenceSize::Size(size))
}

/// Makes the appropriate `CharN` type for the given `String`
pub fn get_char_kind(s: &str) -> PrimitiveType {
    let size = s.bytes().count();

    PrimitiveType::CharN(SequenceSize::Size(size))
}

/// Gets the appropriate int type for the given integer value
pub fn get_int_kind(v: i64) -> PrimitiveType {
    if i32::try_from(v).is_ok() {
        PrimitiveType::Int
    } else {
        PrimitiveType::LongInt
    }
}

/// Gets the appropriate int/nat type for the given integer value
pub fn get_intnat_kind(v: u64) -> PrimitiveType {
    if i32::try_from(v).is_ok() {
        PrimitiveType::IntNat
    } else if u32::try_from(v).is_ok() {
        PrimitiveType::Nat
    } else if i64::try_from(v).is_ok() {
        PrimitiveType::LongInt
    } else {
        PrimitiveType::LongNat
    }
}

/// Gets the appropriate character sequence base type for the given
/// primitive `TypeRef`.
pub fn get_char_seq_base_type(seq_ref: TypeRef) -> TypeRef {
    if let TypeRef::Primitive(primitive) = seq_ref {
        let base_primivite = match primitive {
            PrimitiveType::StringN(_) | PrimitiveType::String_ => PrimitiveType::String_,
            PrimitiveType::CharN(_) | PrimitiveType::Char => PrimitiveType::Char,
            _ => panic!("Tried to convert a non char sequence type into a char sequence base type"),
        };

        TypeRef::Primitive(base_primivite)
    } else {
        panic!("Tried to convert non primitive type ref into a primitive char sequence type");
    }
}

// Helpers for comparing types

/// Checks if the given `type_ref` is a type error
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_error(type_ref: &TypeRef) -> bool {
    matches!(type_ref, TypeRef::TypeError)
}

/// Checks if the given `type_ref` is a primitive reference
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_primitive(type_ref: &TypeRef) -> bool {
    matches!(type_ref, TypeRef::Primitive(_))
}

/// Checks if the given `type_ref` is a named reference
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_named(type_ref: &TypeRef) -> bool {
    matches!(type_ref, TypeRef::Named(_))
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

/// Checks if the given `type_ref` references a single char type (Char)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_char(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(kind, PrimitiveType::Char),
        _ => false,
    }
}

/// Checks if the given `type_ref` references a string-class type (String_ & StringN(x)).
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
            PrimitiveType::StringN(SequenceSize::Size(s))
            | PrimitiveType::CharN(SequenceSize::Size(s)) => Some(*s),
            _ => None, // Can't resolve a compile-time expression or not a string sequence
        },
        _ => None,
    }
}

/// Gets a type id from a type reference
/// Returns `None` if the type is not named
pub fn get_type_id(type_ref: &TypeRef) -> Option<usize> {
    if let TypeRef::Named(type_id) = type_ref {
        Some(*type_id)
    } else {
        None
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
                | PrimitiveType::IntNat
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

/// Checks if the given `type_ref` references an int/nat convertable type (int & nat class types)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_intnat(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(kind, PrimitiveType::IntNat),
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

/// Checks if the given `type_ref` references a boolean
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_boolean(type_ref: &TypeRef) -> bool {
    match type_ref {
        TypeRef::Primitive(kind) => matches!(kind, PrimitiveType::Boolean),
        _ => false,
    }
}

/// Checks if the given `type_ref` references a base type (i.e. the
/// reference does not point to a `Type::Alias`, `Type::Reference`, `Type::Forward`, or `TypeRef::Unknown`)
pub fn is_base_type(type_ref: &TypeRef, type_table: &TypeTable) -> bool {
    match type_ref {
        TypeRef::Unknown => false,
        TypeRef::Primitive(_) | TypeRef::TypeError => true,
        TypeRef::Named(type_id) => !matches!(
            type_table.get_type(*type_id),
            Type::Alias { .. } | Type::Reference { .. } | Type::Forward { .. }
        ),
    }
}

/// Checks if the given `type_ref` references a set type (`Type::Set`)
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type)
pub fn is_set(type_ref: &TypeRef, type_table: &TypeTable) -> bool {
    matches!(type_table.type_from_ref(type_ref), Some(Type::Set { .. }))
}

/// Checks if the given `type_ref` references either an enum type, or a field of an enum type
/// (`Type::Enum`, `Type::EnumField`).
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type).
pub fn is_enum_type(type_ref: &TypeRef, type_table: &TypeTable) -> bool {
    matches!(type_table.type_from_ref(type_ref), Some(Type::Enum { .. }) | Some(Type::EnumField { .. }))
}

/// Checks if the given `type_ref` references a pointer type (`Type::Pointer`).
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type).
pub fn is_pointer(type_ref: &TypeRef, type_table: &TypeTable) -> bool {
    matches!(
        type_table.type_from_ref(type_ref),
        Some(Type::Pointer { .. })
    )
}

/// Checks if the given `type_ref` references an index-class type (char, boolean, enum, range).
/// Requires that `type_ref` is de-aliased (i.e. all aliased references are
/// forwarded to the base type).
pub fn is_index_type(type_ref: &TypeRef, type_table: &TypeTable) -> bool {
    matches!(
        type_ref,
        TypeRef::Primitive(PrimitiveType::Char) | TypeRef::Primitive(PrimitiveType::Boolean)
    ) || matches!(type_table.type_from_ref(type_ref), Some(Type::Range { .. }))
        || matches!(type_table.type_from_ref(type_ref), Some(Type::Enum { .. }))
}

/// Gets the common type between the two given type refs
pub fn common_type<'a>(
    lhs: &'a TypeRef,
    rhs: &'a TypeRef,
    _type_table: &'_ TypeTable,
) -> Option<&'a TypeRef> {
    // TODO: Between strings, stringNs, charNs, chars, sets, classes, pointers, etc.
    if lhs == rhs && !(is_intnat(lhs) && is_intnat(rhs)) {
        // Both are the same type, so they're both in common with eachother
        Some(lhs)
    } else if (is_real(lhs) && is_number_type(rhs)) || (is_number_type(lhs) && is_real(rhs)) {
        // Number types get promoted to real types if any 'real' exists
        Some(&TypeRef::Primitive(PrimitiveType::Real))
    } else if (is_int(lhs) && is_integer_type(rhs)) || (is_integer_type(lhs) && is_int(rhs)) {
        // Integer types get promoted to int types if any 'int' exists
        if is_int(lhs) {
            Some(lhs)
        } else {
            Some(rhs)
        }
    } else if (is_intnat(lhs) && is_integer_type(rhs)) || (is_integer_type(lhs) && is_intnat(rhs)) {
        // Int/Nats get converted into Ints by default
        if !is_intnat(lhs) {
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
/// - All enum range types have the enum type as the root type
/// - All boolean range types (e.g. `false .. true`) have `boolean` as the root type (i.e. all `boolean`s are assignable to the given range).
///
/// # Assignability rules
/// - If two types (after de-aliasing) have the same `type_id` (i.e, have the same root definition) \
/// - If two types (after de-aliasing) have equvalent root types \
/// - If `lvalue` is 'real' and `rvalue` is either `real` or an integer-class type (`rvalue` is converted into an int) \
/// - If `lvalue` is a range and `rvalue` both is the same type kind, as well as existing in the given range \
/// - If `lvalue` is a `char` and `rvalue` is a `char(1)` (or vice versa)
/// - If `lvalue` is a `char` and `rvalue` is a char sequence of length 1 (i.e. `string(1)` or `char(1)`)
/// - If `lvalue` is a string-class type and `rvalue` is a `char` (producing a string of length 1)
/// - If `lvalue` is a `char(n)` and `rvalue` is a `string(n)` (`rvalue` gets converted into a `string(n)`)
/// - If `lvalue` and `rvalue` are pointers to classes and they are the same class or share a common ancestor
/// As well as:
/// - If `lvalue` is a string(x) and `rvalue` is a char
/// - If `lvalue` is a char(x) and `rvalue` is a char
/// - If `lvalue` is a string(x) and `rvalue` is a char(y), and if x >= y
/// - If `lvalue` is a string(n) or a char(n) and `rvalue` is a `string` (the assignment is checked at runtime)
pub fn is_assignable_to(lvalue: &TypeRef, rvalue: &TypeRef, type_table: &TypeTable) -> bool {
    // TODO: Not all of the assignability rules listed above are checked yet, still need to do
    // - Other equivalencies
    // - pointer class inheritance
    // - and array equivalency
    if rvalue == lvalue {
        // Same types are assignable / equivalent

        // Also Covers:
        // - Record, Enum, Union assignment
        // - Opaque assignment

        // Somewhat covers
        // - Pointer to non-class type assignment (need to check pointed type equivalency)
        // - Set assignment (need to check for range equivalency)
        // - Array assignment (need to check for both ranges and element equivalency)
        // - Function/Procedure assignment (need to check param & result type equivalency)
        true
    } else if is_error(lvalue) || is_error(rvalue) {
        // Quick escape for type errors
        false
    } else if is_integer_type(lvalue) && is_integer_type(rvalue) {
        // Integer-class types are mutually assignable to eachother
        true
    } else if is_real(lvalue) && is_number_type(rvalue) {
        // Number-class types are assignable into 'real' types
        true
    } else if is_string(lvalue) && is_char_seq_type(rvalue) {
        // Char Sequence types are assignable into unsized 'string's
        true
    } else if is_sized_char_seq_type(lvalue) && is_sized_char_seq_type(rvalue) {
        // Must check length
        let lvalue_len = get_sized_len(lvalue).unwrap();
        let rvalue_len = get_sized_len(rvalue).unwrap();

        // Assignable if lvalue is a char(*), string(*) or if the rvalue can be contained inside of the lvalue
        lvalue_len == 0 || lvalue_len >= rvalue_len
    } else if is_sized_char_seq_type(lvalue) && is_string(rvalue) {
        // String assignment into char(x) or string(y) is checked at runtime
        true
    } else if (is_char_seq_type(lvalue) && is_char(rvalue))
        || (is_char(lvalue) && is_char_seq_type(rvalue))
    {
        // Must check length
        let lvalue_len = get_sized_len(lvalue).unwrap_or(1);
        let rvalue_len = get_sized_len(rvalue).unwrap_or(1);

        // `char` is only assignable into `char(n)` iff `char(n)` is of length 1 or greater
        // `char` is only assignable into `string(n)` iff `string(n)` is of length 1 or greater
        // `charn(n)` is only assignable into `char` iff `char(n)` is of length 1
        // `string(n)` is only assignable into `char` iff `string(n)` is of length 1
        // `char` is not assignable into `char(*)`
        // `char` is not assignable into `string(*)`

        // For the `char` <- `string` case, we have to default to true,
        // as the value of an unsized `string` can only be checked at runtime
        lvalue_len >= rvalue_len
    } else if let Some(Type::Range { base_type, .. }) = type_table.type_from_ref(lvalue) {
        // A value is assignable inside of a range type if the value is assignable to the
        // range's base type
        is_assignable_to(base_type, rvalue, type_table)
    } else if let Some(Type::Range { base_type, .. }) = type_table.type_from_ref(rvalue) {
        // A range type is assignable to a value if the range's base type is assignable
        // to the given value
        is_assignable_to(lvalue, base_type, type_table)
    } else if let Some(Type::Enum { .. }) = type_table.type_from_ref(lvalue) {
        if let Some(Type::EnumField { enum_type, .. }) = type_table.type_from_ref(rvalue) {
            // Enum field is assignable into the parent enum type
            lvalue == enum_type
        } else {
            is_equivalent_to(lvalue, rvalue, type_table)
        }
    } else {
        // This check is last as it performs very heavy type checking
        is_equivalent_to(lvalue, rvalue, type_table)
    }
}

/// Checks if the types are equivalent
pub fn is_equivalent_to(lhs: &TypeRef, rhs: &TypeRef, type_table: &TypeTable) -> bool {
    if is_error(lhs) || is_error(rhs) {
        // Quick escape for type errors
        return false;
    }

    if lhs == rhs {
        // Quick escape for simple equivalent types (e.g. primitives)
        return true;
    }

    // Other primitives
    if is_integer_type(lhs) && is_integer_type(rhs) {
        // Integer class types are equivalent
        return true;
    } else if is_real(lhs) && is_real(rhs) {
        // Real types are equivalent
        return true;
    } else if is_char(lhs) && is_char(rhs) {
        // Char types are equivalent
        return true;
    } else if (is_char(lhs) && is_charn(rhs)) || (is_charn(lhs) && is_char(rhs)) {
        // Must check length
        let lvalue_len = get_sized_len(lhs).unwrap_or(1);
        let rvalue_len = get_sized_len(rhs).unwrap_or(1);

        // char is equivalent to char(1), but not general char(n)
        return lvalue_len == rvalue_len;
    }

    // Perform equivalence testing based on the type info
    // TODO: Finish the equivalency cases
    // Unions, Records, Enums, and Collections have equivalency based on the type_id
    if let TypeRef::Named(left_id) = lhs {
        if let TypeRef::Named(right_id) = rhs {
            let left_info = type_table.get_type(*left_id);
            let right_info = type_table.get_type(*right_id);

            match left_info {
                Type::Enum { .. } => {
                    // Only equivalent if the base types are
                    return lhs == rhs;
                }
                Type::EnumField { enum_type, .. } => {
                    if let Type::EnumField {
                        enum_type: other_type,
                        ..
                    } = right_info
                    {
                        // Enum fields only equivalent if the parent enum types are
                        return enum_type == other_type;
                    }
                }
                Type::Function { params, result } => {
                    if let Type::Function {
                        params: other_params,
                        result: other_result,
                    } = right_info
                    {
                        return params == other_params && result == other_result;
                    }
                }
                Type::Set { range } => {
                    if let Type::Set { range: other_range } = right_info {
                        // Sets are equivalent if the range types are equivalent
                        return is_equivalent_to(range, other_range, type_table);
                    }
                }
                Type::Pointer { to, is_unchecked } => {
                    if let Type::Pointer {
                        to: other_to,
                        is_unchecked: other_unchecked,
                    } = right_info
                    {
                        // Pointer types are equivalent if the 'to' types are, and if they have
                        // equivalent checked/uncheckedness
                        return is_equivalent_to(to, other_to, type_table)
                            && is_unchecked == other_unchecked;
                    }
                }
                Type::Range {
                    start,
                    end,
                    base_type,
                    size,
                } => {
                    if let Type::Range {
                        start: other_start,
                        end: other_end,
                        base_type: other_type,
                        size: other_size,
                    } = right_info
                    {
                        // Range type equivalency follows base type equivalency
                        if !is_equivalent_to(base_type, other_type, type_table) {
                            return false;
                        }

                        // Check the end range presence
                        // If either is absent, the ranges are not equivalent
                        if !end.is_none() && other_end.is_none() {
                            return false;
                        }

                        // Compare the start ranges
                        let is_start_eq = {
                            let start_value = Value::try_from(*start.clone()).ok();
                            let other_start_value = Value::try_from(*other_start.clone()).ok();

                            start_value
                                .zip(other_start_value)
                                .and_then(|(a, b)| {
                                    value::apply_binary(
                                        a,
                                        BinaryOp::Equal,
                                        b,
                                        value::EvalConstraints { as_64_bit: false },
                                    )
                                    .ok()
                                })
                                .map_or(false, |v| {
                                    let is_eq: bool = v.into();
                                    is_eq
                                })
                        };

                        // Compare the range sizes
                        let is_end_eq = size == other_size;

                        return is_start_eq && is_end_eq;
                    }
                }
                _ => todo!("??? {:?} & {:?}", left_info, right_info),
            }
        }
    }

    false
}

/// Dealiases the given ref, using the given type table.
/// Does not perform resolving of any types, and requires the previous
/// resolution of any `Type::Reference` found along the chain.
pub fn dealias_ref(type_ref: &TypeRef, type_table: &TypeTable) -> TypeRef {
    let mut current_ref = type_ref;

    loop {
        let type_id = if let Some(id) = get_type_id(current_ref) {
            id
        } else {
            // Reached a primitive type
            break *current_ref;
        };

        if let Type::Alias { to } = type_table.get_type(type_id) {
            // Advance the chain
            current_ref = to;
        } else {
            // Reached a non alias type
            debug_assert!(
                !matches!(type_table.get_type(type_id), Type::Reference { .. }),
                "A reference was not resolved at this point"
            );
            break *current_ref;
        }
    }
}

/// Gets the length of an index type
pub fn get_index_length(index_ref: &TypeRef, type_table: &TypeTable) -> usize {
    if let TypeRef::Primitive(prim_type) = index_ref {
        match prim_type {
            PrimitiveType::Boolean => 2,
            PrimitiveType::Char => 0x20_0000, // Over the entire range of unicode characters
            _ => 0,
        }
    } else if let TypeRef::Named(type_id) = index_ref {
        // Based on the range type info
        match type_table.get_type(*type_id) {
            Type::Range { size, .. } => {
                // Based on the size
                size.unwrap_or(0)
            }
            Type::Enum { fields } => {
                // Based on the number of fields
                fields.keys().count()
            }
            Type::EnumField { .. } => panic!("Enum field cannot be used as an index type"),
            _ => {
                // No length
                0
            }
        }
    } else {
        // No length
        0
    }
}

/// Computes the number of elements inside of the array
pub fn get_array_element_count(ranges: &[TypeRef], type_table: &TypeTable) -> (usize, bool) {
    if ranges.is_empty() {
        // No ranges means that there is no array
        return (0, false);
    }

    ranges
        .iter()
        .map(|index| get_index_length(index, type_table))
        .fold((1, false), |acc, elem| {
            let (last_count, did_overflow) = acc;
            let (new_count, will_overflow) = last_count.overflowing_mul(elem);

            // Carry overflow status
            (new_count, did_overflow || will_overflow)
        })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_typeref_eq() {
        // PartialEq is a type-wise comparison
        assert_eq!(TypeRef::TypeError, TypeRef::TypeError);
        assert_eq!(TypeRef::Unknown, TypeRef::Unknown);
        assert_eq!(
            TypeRef::Primitive(PrimitiveType::LongInt),
            TypeRef::Primitive(PrimitiveType::LongInt)
        );
        assert_eq!(TypeRef::Named(0), TypeRef::Named(0));

        assert_ne!(
            TypeRef::Primitive(PrimitiveType::LongInt),
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_ne!(TypeRef::Named(1), TypeRef::Named(5));
    }
}

mod pretty_print {
    use super::{ParamDef, Type, TypeRef, TypeTable};
    use crate::pretty_print;
    use std::fmt;

    impl fmt::Display for TypeRef {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                TypeRef::Unknown => f.write_str("ty_unknown"),
                TypeRef::TypeError => f.write_str("ty_error"),
                TypeRef::Primitive(prim) => f.write_fmt(format_args!("ty_prim[{:?}]", prim)),
                TypeRef::Named(id) => f.write_fmt(format_args!("ty_id[{}]", id)),
            }
        }
    }

    impl fmt::Display for ParamDef {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let props = [
                ("cheat", self.force_type),
                ("var", self.pass_by_ref),
                ("register", self.bind_to_register),
            ];

            for (name, is_present) in &props {
                if *is_present {
                    f.write_str(name)?;
                    f.write_str(" ")?;
                }
            }

            f.write_fmt(format_args!("{} : {}", self.name, self.type_spec))
        }
    }

    impl fmt::Display for Type {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Type::Reference { expr } => f.write_fmt(format_args!("{{ ref_expr {} }}", expr))?,
                Type::SizeExpr { expr } => f.write_fmt(format_args!("{{ size_expr {} }}", expr))?,
                Type::Range {
                    start,
                    end,
                    size,
                    base_type,
                } => {
                    f.write_str("{ range ")?;

                    if let Some(end) = end {
                        f.write_fmt(format_args!("{} .. {} ", start, end))?;
                    } else {
                        f.write_fmt(format_args!("{} .. * ", start))?;
                    }

                    if let Some(size) = size {
                        f.write_fmt(format_args!("({})", size))?;
                    }

                    f.write_fmt(format_args!(" {} }}", base_type))?;
                }
                Type::Alias { to } => f.write_fmt(format_args!("{{ alias to {} }}", to))?,
                Type::Array {
                    ranges,
                    element_type,
                    is_flexible,
                    ..
                } => {
                    f.write_str("Array { ")?;
                    if *is_flexible {
                        f.write_str("flexible ")?;
                    }
                    pretty_print::print_list(f, ranges.iter())?;
                    f.write_fmt(format_args!(" of {} }}", element_type))?;
                }
                Type::Enum { fields } => {
                    f.write_str("{ enum ( ")?;

                    // Sort fields to maintain stable printing order
                    let mut fields: Vec<(&String, &TypeRef)> = fields.iter().collect();
                    fields.sort_by(|a, b| match a.1 {
                        TypeRef::Named(a) => match b.1 {
                            TypeRef::Named(b) => a.cmp(b),
                            _ => std::cmp::Ordering::Equal,
                        },
                        _ => std::cmp::Ordering::Equal,
                    });

                    for (name, id) in fields {
                        f.write_fmt(format_args!("{}({}) ", name, id))?;
                    }
                    f.write_str(")")?;
                }
                Type::EnumField { enum_type, ordinal } => f.write_fmt(format_args!(
                    "{{ enum_field({}) of {} }}",
                    ordinal, enum_type
                ))?,
                Type::Set { range } => f.write_fmt(format_args! {"{{ set of {} }}", range})?,
                Type::Forward { is_resolved } => {
                    if *is_resolved {
                        f.write_str("{ resolved forward }")?;
                    } else {
                        f.write_str("{ forward }")?;
                    }
                }
                Type::Pointer { to, is_unchecked } => {
                    if *is_unchecked {
                        f.write_fmt(format_args!("{{ unchecked pointer to {} }}", to))?
                    } else {
                        f.write_fmt(format_args!("{{ pointer to {} }}", to))?
                    }
                }
                Type::Function { params, result } => {
                    if result.is_some() {
                        f.write_str("{ function ")?;
                    } else {
                        f.write_str("{ procedure ")?;
                    }

                    if let Some(params) = params {
                        f.write_str("(")?;
                        pretty_print::print_list(f, params.iter())?;
                        f.write_str(") ")?;
                    }

                    if let Some(result) = result {
                        f.write_fmt(format_args!("-> {} ", result))?
                    }

                    f.write_str("}")?;
                }
            }

            Ok(())
        }
    }

    impl fmt::Display for TypeTable {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("[\n")?;
            for (id, ty) in self.types.iter().enumerate() {
                f.write_fmt(format_args!("{:8} -> {}\n", id, ty))?;
            }
            f.write_str("]")
        }
    }
}
