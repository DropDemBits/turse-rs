//! References to entities that are used within the bytecode spec.

#[allow(unused_imports)]
use crate::{BytecodeSpec, Instruction};

macro_rules! entity_impl {
    ($name:ident) => {
        impl $name {
            pub fn from_usize(index: usize) -> Self {
                Self(index)
            }

            pub fn index(self) -> usize {
                self.0
            }
        }
    };
}

macro_rules! from_impls {
    ($($from:ident),+ for $target:ident) => {
        $(impl From<$from> for $target {
            fn from(value: $from) -> $target {
                value.0
            }
        })+
    };
}

/// Refers to a specific immediate operand in an [`Instruction`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImmediateOperand(usize);
entity_impl!(ImmediateOperand);

/// Refers to a specific stack_before operand in an [`Instruction`].
/// This may be a part of a specific conditional decode group.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackBeforeOperand(usize);
entity_impl!(StackBeforeOperand);

/// Refers to a specific stack_after operand in an [`Instruction`].
/// This may be a part of a specific conditional decode group.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackAfterOperand(usize);
entity_impl!(StackAfterOperand);

/// Refers to a specific decode group in an [`Instruction`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConditionalDecodeRef(usize);
entity_impl!(ConditionalDecodeRef);

/// Refers to a specific type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRef(usize);
entity_impl!(TypeRef);

/// Refers to a specific scalar type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScalarRef(TypeRef);

impl ScalarRef {
    /// Underlying [`TypeRef`] that this ref refers to.
    pub fn type_ref(self) -> TypeRef {
        self.0
    }
}

/// Refers to a specific struct type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructRef(TypeRef);

impl StructRef {
    /// Underlying [`TypeRef`] that this ref refers to.
    pub fn type_ref(self) -> TypeRef {
        self.0
    }
}

/// Refers to a specific enum type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumRef(TypeRef);

impl EnumRef {
    /// Underlying [`TypeRef`] that this ref refers to.
    pub fn type_ref(self) -> TypeRef {
        self.0
    }
}

from_impls!(ScalarRef, StructRef, EnumRef for TypeRef);

/// Refers to a specific enum type variant in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariantRef(EnumRef, usize);

impl VariantRef {
    /// Which enum type this variant is a part of.
    pub fn ty(self) -> EnumRef {
        self.0
    }
}
