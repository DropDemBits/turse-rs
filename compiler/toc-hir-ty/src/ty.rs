//! A type participant within the type system.

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    /// A type hole derived from a type system error. This allows type inference
    /// to make forward progress and discover more type errors without having to
    /// stop at the first type error.
    Error,
    /// Boolean type.
    Boolean,
    /// Signed integer types (e.g. `int4`, `int`).
    Int(IntSize),
    /// Unsigned integer types (e.g. `nat4`, `nat`).
    /// Also includes `addressint`, with the size being dependent on the target.
    /// machine.
    Nat(NatSize),
    /// Floating point types (e.g. `real8`, `real`).
    Real(RealSize),
    /// Integer-variable type. This is a phony type that defaults to `int` if it
    /// is not unified with a concrete `int`, `nat`, or `real` type.
    Integer,
    /// Integer-variable type. This is a phony type that defaults to `real` if it
    /// is not unified with a concrete `real` type.
    Number,
    /// Single character type.
    Char,
    /// Simple string type.
    String,
    /// Fixed-size character type.
    #[cfg(feature = "unimplemented")]
    CharN(SeqSize),
    /// Fixed-size string type.
    #[cfg(feature = "unimplemented")]
    StringN(SeqSize),
    /// Named alias to another type, pointing to the base (un-aliased) type
    #[cfg(feature = "unimplemented")]
    Alias(AliasTy),
    /// An alias exported as an opaque type. Points to the base (un-aliased) alias,
    /// with the [`DefId`] pointing to the original alias.
    #[cfg(feature = "unimplemented")]
    Opaque(DefId, TypeId),
    /// Constrained value type, with base type, range start, and range end.
    /// Base type is already de-aliased
    #[cfg(feature = "unimplemented")]
    Constrained(TypeId, Const, EndBound),
    /// Array type, with flexibility, types for each index, and the element type.
    #[cfg(feature = "unimplemented")]
    Array(ArraySizing, Vec<TypeId>, TypeId),
    /// An enumeration type, with associated definition point and variants.
    #[cfg(feature = "unimplemented")]
    Enum(WithDef, Vec<DefId>),
    /// Set type, with associated definition point
    #[cfg(feature = "unimplemented")]
    Set(WithDef, TypeId),
    /// Set type, with a given checkedness
    #[cfg(feature = "unimplemented")]
    Pointer(Checked, TypeId),
    /// Subprogram type, from (`procedure`, `function`, and `process`).
    #[cfg(feature = "unimplemented")]
    Subprogram(symbol::SubprogramKind, Option<Vec<Param>>, TypeId),
    /// Void type, returned from (`procedure` and `process`)
    #[cfg(feature = "unimplemented")]
    Void,
    // Other types to add:
    // - array
    // - range
    // - enum
    // - union
    // - record
    // - set
    // - pointer
    // - condition
    // - collection
}

/// Variants of an integer-class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntSize {
    Int1,
    Int2,
    Int4,
    /// Initialization checked version of `Int4`
    Int,
}

/// Size variant of a natural-class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NatSize {
    Nat1,
    Nat2,
    Nat4,
    /// Initialization checked version of `Nat4`
    Nat,
    /// Address sized integer, dependent on target machine
    AddressInt,
}

/// Size variant of a number-class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RealSize {
    Real4,
    Real8,
    /// Initialization checked version of `Real8`
    Real,
}
