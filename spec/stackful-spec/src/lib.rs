//! # `stackful-spec` - `ungrammar` for stack-based bytecodes
//!
//! Defines an abstract description of a stack-based bytecode.
//!
//! Originally inspired by being tired from writing the same opcode list over
//! and over again, this aims to describe the abstract operation of each
//! instruction in a bytecode, for both code generation and documentation.
//!
//! At a high level, each instruction has a set of immediate operands,
//! an expected stack state, and a resultant stack state. Operands are
//! assumed to be placed one after the other, without caring for alignment.
//! Each operand may be of a specific type, which are defined in the `types`
//! node of the document.
//!
//! Because instructions may differ in operation based on operands, there is
//! a condition node that applies based on certain conditions. Instructions
//! are assumed to always be decoded in the same way, thus dynamically decoded
//! operands are out of the scope of this specification.

use std::{
    collections::BTreeMap,
    fmt::Display,
    ops::{Index, Range},
    str::FromStr,
};

mod parse;

type Str = String;

/// A parsed specification of bytecode instructions.
#[derive(Debug)]
#[non_exhaustive]
pub struct BytecodeSpec {
    /// The types available for operands.
    pub types: Types,
    /// The defined instructions.
    pub instructions: Box<[Instruction]>,
    /// Groups of related instructions.
    pub groups: Box<[(Group, Range<usize>)]>,
}

impl BytecodeSpec {
    pub fn by_groups(&self) -> impl Iterator<Item = InstructionEntry<'_>> {
        let mut groups = self.groups.iter().peekable();
        let mut next_instr = self.instructions.iter().enumerate();

        std::iter::from_fn(move || {
            let (index, instr) = next_instr.next()?;

            if let Some((group_info, group_range)) = groups.peek() {
                if group_range.contains(&index) {
                    // Skip over group and instructions that are part of a group.
                    dbg!(groups.next());
                    // We've already consumed one instruction of the group, so we don't need to consume it again.
                    for _ in 0..(group_range.len().saturating_sub(1)) {
                        dbg!(next_instr.next());
                    }

                    return dbg!(Some(InstructionEntry::Group(
                        group_info,
                        self.instructions
                            .get(group_range.clone())
                            .expect("group range should be within instruction slice"),
                    )));
                }
            }

            return dbg!(Some(InstructionEntry::Instruction(instr)));
        })
    }
}

impl FromStr for BytecodeSpec {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::parse_spec(s)
    }
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    KdlError(#[from] kdl::KdlError),

    #[error("expected a string argument")]
    #[diagnostic(code(bytecode_spec::expected_string))]
    ExpectedString(#[label] miette::SourceSpan),

    #[error("missing a string argument")]
    #[diagnostic(code(bytecode_spec::missing_string))]
    MissingString(#[label] miette::SourceSpan),

    #[error("missing required node `{0}`")]
    #[diagnostic(code(bytecode_spec::node_required))]
    NodeRequired(CommonNodes, #[label] miette::SourceSpan),

    #[error("node does not have any children")]
    #[diagnostic(code(bytecode_spec::children_required))]
    ChildrenRequired(#[label] miette::SourceSpan),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CommonNodes {
    InstructionList,
    TypeList,
    GroupNode,
    OpcodeNode,
}

impl CommonNodes {
    pub fn node_name(self) -> &'static str {
        match self {
            CommonNodes::InstructionList => "instructions",
            CommonNodes::TypeList => "types",
            CommonNodes::GroupNode => "group",
            CommonNodes::OpcodeNode => "group",
        }
    }
}

impl Display for CommonNodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.node_name())
    }
}

#[derive(Debug)]
pub struct Types {
    pub types: Box<[Type]>,
    by_name: BTreeMap<String, TypeRef>,
}

impl Types {
    /// Finds a type by name
    pub fn get(&self, name: &str) -> Option<TypeRef> {
        self.by_name.get(name).copied()
    }
}

impl Index<TypeRef> for Types {
    type Output = Type;

    fn index(&self, index: TypeRef) -> &Self::Output {
        &self.types[index.0]
    }
}

impl Index<ScalarRef> for Types {
    type Output = Scalar;

    fn index(&self, index: ScalarRef) -> &Self::Output {
        self[index.0].as_scalar().expect("type should be a scalar")
    }
}

impl Index<StructRef> for Types {
    type Output = Struct;

    fn index(&self, index: StructRef) -> &Self::Output {
        self[index.0].as_struct().expect("type should be a struct")
    }
}

impl Index<EnumRef> for Types {
    type Output = Enum;

    fn index(&self, index: EnumRef) -> &Self::Output {
        self[index.0].as_enum().expect("type should be an enum")
    }
}

/// An entry within the instruction node.
#[derive(Debug)]
#[non_exhaustive]
pub enum InstructionEntry<'spec> {
    /// An individual instruction.
    Instruction(&'spec Instruction),
    /// A group of related instructions, with an optional heading and description.
    Group(&'spec Group, &'spec [Instruction]),
}

/// Related group of instructions. Intended for documentation purposes, as this
/// should have no effect semantically.
#[derive(Debug)]
pub struct Group {
    heading: Option<Str>,
    description: Option<Str>,
}

impl Group {
    /// Group heading, which can be used as a section title.
    pub fn heading(&self) -> Option<&str> {
        self.heading.as_deref()
    }

    /// Detailed description of the group.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }
}

#[derive(Debug)]
pub struct Instruction {
    mnemonic: Str,
    opcode: u32,
    heading: Option<Str>,
    description: Option<Str>,
    // don't have to expose these immediately
    immediate_operands: Box<[Operand]>,
    stack_before_operands: Box<[Operand]>,
    stack_after_operands: Box<[Operand]>,
    conditional_decodes: Option<Box<[ConditionalDecode]>>,
}

// q: providing access to decode variants?

impl Instruction {
    /// Instruction mnemonic.
    pub fn mnemonic(&self) -> &str {
        &self.mnemonic
    }

    /// Instruction opcode.
    pub fn opcode(&self) -> u32 {
        self.opcode
    }

    /// Instruction heading, which can be used as a section title.
    pub fn heading(&self) -> Option<&str> {
        self.heading.as_deref()
    }

    /// Detailed description of the instruction.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Operand {
    /// Name of the operand.
    pub name: Str,
    /// Operand type, refers to the top-level type list.
    pub ty: TypeRef,
    /// Brief description of how this operand is used.
    pub description: Option<Str>,
    /// If this operand is not used by the instruction.
    /// Intended for documentation purposes to mark operands as
    /// never being touched, or operands that may have had some use in the past.
    pub unused: bool,
    /// If this is a variadic operand whose element count is computed at runtime.
    /// Only applicable for stack_before operands.
    pub variadic: bool,
    /// Which stack_before operand this preserves, which could potentially be in a
    /// different stack slot.
    /// Only applicable for stack_after operands.
    pub preserves: Option<StackBeforeOperand>,
    pub computed: Option<ComputedExpr>,
}

/// Named reference to an operand
#[derive(Debug)]
#[non_exhaustive]
pub enum OperandRef {
    Immediate(ImmediateOperand),
    StackBefore(StackBeforeOperand),
    StackAfter(StackAfterOperand),
}

#[derive(Debug)]
pub struct ConditionalDecode {
    predicate: Option<DecodePredicate>,
}

impl ConditionalDecode {
    /// When does this decode apply, or if this applies when all of the other
    /// predicates don't apply.
    pub fn predicate(&self) -> Option<&DecodePredicate> {
        self.predicate.as_ref()
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct DecodePredicate {
    pub lhs: PredicateOperand,
    pub op: PredicateOp,
    pub rhs: PredicateOperand,
}

#[derive(Debug)]
pub enum PredicateOp {
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum PredicateOperand {
    /// Refers to a specific [`ImmediateOperand`].
    ImmediateOperand(ImmediateOperand),
    /// Refers to a specific enum's variant
    VariantRef(VariantRef),
}

#[derive(Debug)]
pub enum ComputedExpr {
    /// Refers to a specific [`ImmediateOperand`].
    ImmediateOperand(ImmediateOperand),
    /// Refers to a specific enum's variant
    VariantRef(VariantRef),
    Add(Box<ComputedExpr>, Box<ComputedExpr>),
    Sub(Box<ComputedExpr>, Box<ComputedExpr>),
    Mul(Box<ComputedExpr>, Box<ComputedExpr>),
    Div(Box<ComputedExpr>, Box<ComputedExpr>),
    /// Conditionally evaluates to one value or the other, based on the predicate.
    If {
        predicate: Box<ComputedPredicate>,
        if_true: Box<ComputedExpr>,
        if_false: Box<ComputedExpr>,
    },
}

#[derive(Debug)]
pub enum ComputedPredicate {
    Eq(Box<ComputedExpr>, Box<ComputedExpr>),
    NotEq(Box<ComputedExpr>, Box<ComputedExpr>),
    LessThan(Box<ComputedExpr>, Box<ComputedExpr>),
    GreaterThan(Box<ComputedExpr>, Box<ComputedExpr>),
    GreaterEq(Box<ComputedExpr>, Box<ComputedExpr>),
    LessEq(Box<ComputedExpr>, Box<ComputedExpr>),
    And(Box<ComputedPredicate>, Box<ComputedPredicate>),
    Or(Box<ComputedPredicate>, Box<ComputedPredicate>),
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Type {
    Scalar(Scalar),
    Struct(Struct),
    Enum(Enum),
}

impl Type {
    pub fn as_scalar(&self) -> Option<&Scalar> {
        match self {
            Type::Scalar(it) => Some(it),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&Struct> {
        match self {
            Type::Struct(it) => Some(it),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&Enum> {
        match self {
            Type::Enum(it) => Some(it),
            _ => None,
        }
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Scalar {
    pub name: Str,
    pub description: Option<Str>,
    pub size: Option<u32>,
    pub repr_type: Option<Str>,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Struct {
    pub name: Str,
    pub description: Option<Str>,
    pub size: Option<u32>,
    pub fields: Box<[StructField]>,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct StructField {
    pub name: Str,
    pub ty: Str,
    pub description: Option<Str>,
    pub size: Option<u32>,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Enum {
    pub name: Str,
    pub description: Option<Str>,
    pub size: Option<u32>,
    pub repr_type: Option<Str>,
    pub variants: Box<[EnumVariant]>,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct EnumVariant {
    pub name: Str,
    pub ordinal: u32,
    pub properties: BTreeMap<Str, PropertyValue>,
}

/// Value of a potential property.
#[derive(Debug)]
#[non_exhaustive]
pub enum PropertyValue {
    String(Str),
    Number(u128),
}

/// Refers to a specific immediate operand in an [`Instruction`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImmediateOperand(usize);

/// Refers to a specific stack_before operand in an [`Instruction`].
/// This may be a part of a specific conditional decode group.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackBeforeOperand(usize);

/// Refers to a specific stack_after operand in an [`Instruction`].
/// This may be a part of a specific conditional decode group.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackAfterOperand(usize);

/// Refers to a specific decode group in an [`Instruction`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConditionalDecodeRef(usize);

/// Refers to a specific type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRef(usize);

/// Refers to a specific scalar type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScalarRef(TypeRef);

/// Refers to the artificial computed type to indicate an operand whose size is computed at decode time.
pub const COMPUTED_TYPE: ScalarRef = ScalarRef(TypeRef(usize::MAX));

/// Refers to a specific struct type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructRef(TypeRef);
///
/// Refers to a specific enum type in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumRef(TypeRef);

/// Refers to a specific enum type variant in a [`BytecodeSpec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariantRef(EnumRef, usize);

impl VariantRef {
    /// Which enum type this variant is a part of.
    pub fn ty(self) -> EnumRef {
        self.0
    }
}

#[cfg(test)]
mod tests {}
