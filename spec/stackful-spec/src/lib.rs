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
//! are assumed to always be decoded in the same way from the opcode and
//! immediate operands alone, thus dynamically decoded operands are out of the
//! scope of this specification. Variably-sized instructions must immediately
//! have a discriminator value before the variable-sized decode section, which
//! is achieved through using [`Union`] operands.

use std::{
    collections::BTreeMap,
    fmt::Display,
    ops::{Index, Range},
    str::FromStr,
};

use entities::{
    EnumRef, EnumVariantRef, ImmediateOperand, ScalarRef, StackAfterOperand, StackBeforeOperand,
    StructRef, TypeRef, UnionVariantRef,
};

pub mod entities;

mod parse;

type Str = String;

/// A parsed specification of bytecode instructions.
#[derive(Debug)]
#[non_exhaustive]
pub struct BytecodeSpec {
    /// The types used by operands.
    pub types: Types,
    /// The instructions defined for the bytecode.
    ///
    /// Instructions are stored in definition order.
    pub instructions: Box<[Instruction]>,
    /// Groups of related instructions.
    pub groups: Box<[(Group, Range<usize>)]>,
}

impl BytecodeSpec {
    /// Gets all instructions with respect to groupings of instructions.
    pub fn by_groups(&self) -> impl Iterator<Item = InstructionEntry<'_>> {
        let mut groups = self.groups.iter().peekable();
        let mut next_instr = self.instructions.iter().enumerate();

        std::iter::from_fn(move || {
            let (index, instr) = next_instr.next()?;

            if let Some((group_info, group_range)) = groups.peek() {
                if group_range.contains(&index) {
                    // Skip over group and instructions that are part of a group.
                    _ = groups.next();
                    // We've already consumed one instruction of the group, so we don't need to consume it again.
                    for _ in 0..(group_range.len().saturating_sub(1)) {
                        _ = next_instr.next();
                    }

                    return Some(InstructionEntry::Group(
                        group_info,
                        self.instructions
                            .get(group_range.clone())
                            .expect("group range should be within instruction slice"),
                    ));
                }
            }

            Some(InstructionEntry::Instruction(instr))
        })
    }
}

impl FromStr for BytecodeSpec {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::parse_spec(s)
    }
}

/// Error encountered while parsing the bytecode spec.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    KdlError(#[from] kdl::KdlError),

    #[error("expected a value")]
    #[diagnostic(code(bytecode_spec::expected_value))]
    ExpectedValue(#[label] miette::SourceSpan),

    #[error("value type cannot be used")]
    #[diagnostic(code(bytecode_spec::expected_value))]
    InvalidValue(#[label] miette::SourceSpan),

    #[error("expected a string value")]
    #[diagnostic(code(bytecode_spec::expected_string))]
    ExpectedString(#[label] miette::SourceSpan),

    #[error("expected an integer value")]
    #[diagnostic(code(bytecode_spec::expected_integer))]
    ExpectedInteger(#[label] miette::SourceSpan),

    #[error("expected a positive integer value")]
    #[diagnostic(code(bytecode_spec::expected_positive_integer))]
    ExpectedPositiveInteger(#[label] miette::SourceSpan),

    #[error("integer is too big")]
    #[diagnostic(code(bytecode_spec::integer_too_big))]
    IntegerTooBig(#[label("expected a value less than u32::MAX")] miette::SourceSpan),

    #[error("missing a string argument")]
    #[diagnostic(code(bytecode_spec::missing_string))]
    MissingString(#[label] miette::SourceSpan),

    #[error("missing an integer argument")]
    #[diagnostic(code(bytecode_spec::missing_integer))]
    MissingInteger(#[label] miette::SourceSpan),

    #[error("missing `{0}` property")]
    #[diagnostic(code(bytecode_spec::missing_property))]
    MissingProperty(String, #[label] miette::SourceSpan),

    #[error("missing required node `{0}`")]
    #[diagnostic(code(bytecode_spec::node_required))]
    NodeRequired(CommonNodes, #[label] miette::SourceSpan),

    #[error("node does not have any children")]
    #[diagnostic(code(bytecode_spec::children_required))]
    ChildrenRequired(
        #[label("`{1}` nodes must have curly braces")] miette::SourceSpan,
        CommonNodes,
    ),

    #[error("invalid type kind")]
    #[diagnostic(code(bytecode_spec::invalid_type_kind))]
    InvalidTypeKind(#[label("expected `scalar`, `struct` or `enum`")] miette::SourceSpan),

    #[error("unknown type name `{0}`")]
    #[diagnostic(code(bytecode_spec::unknown_type_name))]
    UnknownTypeName(
        String,
        #[label("not found in `types` list")] miette::SourceSpan,
    ),

    #[error("duplicate {0} `{1}`")]
    #[diagnostic(code(bytecode_spec::duplicate_name))]
    DuplicateName(
        NameKind,
        String,
        #[label(primary, "first defined here")] miette::SourceSpan,
        #[label("defined here again")] miette::SourceSpan,
    ),

    #[error("type `{0}` is infinitely sized")]
    #[diagnostic(code(bytecode_spec::cyclic_type))]
    CyclicType(
        String,
        #[label(primary, "type defined here")] miette::SourceSpan,
        #[label(collection, "which refers to this type")] Vec<miette::SourceSpan>,
    ),

    #[error("variant is missing properties")]
    #[diagnostic(code(bytecode_spec::missing_properties))]
    MissingVariantProperties(#[label("missing {1}")] miette::SourceSpan, StringList),

    #[error("mismatched property types")]
    #[diagnostic(code(bytecode_spec::mismatched_property_value))]
    MismatchedPropertyTypes(
        #[label("expected {1}, found {2}")] miette::SourceSpan,
        PropertyKind,
        PropertyKind,
    ),
}

/// Common node names used for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CommonNodes {
    InstructionList,
    TypeList,
    GroupNode,
    OperandsList,
    OpcodeNode,
    StructNode,
    EnumNode,
    UnionNode,
    UnionVariantNode,
}

impl CommonNodes {
    pub fn node_name(self) -> &'static str {
        match self {
            CommonNodes::InstructionList => "instructions",
            CommonNodes::TypeList => "types",
            CommonNodes::GroupNode => "group",
            CommonNodes::OperandsList => "operands",
            CommonNodes::OpcodeNode => "opcode",
            CommonNodes::StructNode => "struct",
            CommonNodes::EnumNode => "enum",
            CommonNodes::UnionNode => "union",
            CommonNodes::UnionVariantNode => "union variant",
        }
    }
}

impl Display for CommonNodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.node_name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NameKind {
    Generic,
    Type,
    Variant,
    Property,
    Field,
    Operand,
    Mnemonic,
    InstructionOpcode,
}

impl Display for NameKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            NameKind::Generic => "name",
            NameKind::Type => "type name",
            NameKind::Variant => "variant name",
            NameKind::Property => "property name",
            NameKind::Field => "field name",
            NameKind::Operand => "operand",
            NameKind::Mnemonic => "mnemonic",
            NameKind::InstructionOpcode => "instruction opcode",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringList(pub Vec<String>);

impl Display for StringList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut entries = self.0.iter();

        let Some(first) = entries.next() else {
            return Ok(());
        };
        f.write_fmt(format_args!("`{first}`"))?;

        for entry in entries {
            f.write_fmt(format_args!(", `{entry}`"))?;
        }

        Ok(())
    }
}

/// Contains the definitions for types used by all operands.
#[derive(Debug)]
pub struct Types {
    /// Definitions of every type.
    pub types: Box<[Type]>,
    by_name: BTreeMap<Str, TypeRef>,
}

impl Types {
    /// Provides an iterator over all type definitions with the associated type ref.
    pub fn defs(&self) -> impl Iterator<Item = (TypeRef, &Type)> {
        self.types
            .iter()
            .enumerate()
            .map(|(index, ty)| (TypeRef::from_usize(index), ty))
    }

    /// Finds a type by name
    pub fn get(&self, name: &str) -> Option<TypeRef> {
        self.by_name.get(name).copied()
    }
}

impl Index<TypeRef> for Types {
    type Output = Type;

    fn index(&self, index: TypeRef) -> &Self::Output {
        &self.types[index.index()]
    }
}

impl Index<ScalarRef> for Types {
    type Output = Scalar;

    fn index(&self, index: ScalarRef) -> &Self::Output {
        self[index.type_ref()]
            .as_scalar()
            .expect("type should be a scalar")
    }
}

impl Index<StructRef> for Types {
    type Output = Struct;

    fn index(&self, index: StructRef) -> &Self::Output {
        self[index.type_ref()]
            .as_struct()
            .expect("type should be a struct")
    }
}

impl Index<EnumRef> for Types {
    type Output = Enum;

    fn index(&self, index: EnumRef) -> &Self::Output {
        self[index.type_ref()]
            .as_enum()
            .expect("type should be an enum")
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
#[allow(dead_code)] // figuring out stack opcode decode predicates, is complicated!
pub struct Instruction {
    mnemonic: Str,
    opcode: u32,
    heading: Option<Str>,
    description: Option<Str>,
    // don't have to expose these immediately
    immediate_operands: Box<[Operand]>,
    // representing stack operands is hard because
    // - decode may be conditional
    // stack operand refs refer to specific named slots, not instances hidden behind conditional decode
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

    /// Immediately decoded operands.
    pub fn immediate_operands(&self) -> &[Operand] {
        &self.immediate_operands
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
    /// Expression to compute the element count at decode time, if applicable.
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
    VariantRef(EnumVariantRef),
}

#[derive(Debug)]
pub enum ComputedExpr {
    /// Refers to a specific [`ImmediateOperand`].
    ImmediateOperand(ImmediateOperand),
    /// Refers to a specific enum's variant
    VariantRef(EnumVariantRef),
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
    Union(Union),
}

impl Type {
    /// Name of the type.
    pub fn name(&self) -> &str {
        match self {
            Type::Scalar(it) => &it.name,
            Type::Struct(it) => &it.name,
            Type::Enum(it) => &it.name,
            Type::Union(it) => &it.name,
        }
    }

    /// Detailed description of the type.
    pub fn description(&self) -> Option<&str> {
        match self {
            Type::Scalar(it) => it.description.as_deref(),
            Type::Struct(it) => it.description.as_deref(),
            Type::Enum(it) => it.description.as_deref(),
            Type::Union(it) => it.description.as_deref(),
        }
    }

    /// Size of the type, typically in bytes.
    pub fn size(&self) -> u32 {
        match self {
            Type::Scalar(it) => it.size,
            Type::Struct(it) => it.size,
            Type::Enum(it) => it.size,
            Type::Union(it) => it.size,
        }
    }

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

    pub fn as_union(&self) -> Option<&Union> {
        match self {
            Type::Union(it) => Some(it),
            _ => None,
        }
    }
}

/// A simple primitive type, used as the building block for other types.
#[derive(Debug)]
pub struct Scalar {
    name: Str,
    description: Option<Str>,
    size: u32,
    repr_type: Option<Str>,
}

impl Scalar {
    /// Name of the type.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the type.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Size of the type, typically in bytes.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// What this type may be represented as when generating code.
    pub fn repr_type(&self) -> Option<&str> {
        self.repr_type.as_deref()
    }
}

/// A compound grouping of types.
#[derive(Debug)]
pub struct Struct {
    name: Str,
    description: Option<Str>,
    size: u32,
    fields: Box<[AdtField]>,
}

impl Struct {
    /// Name of the type.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the type.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Size of the type, typically in bytes.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// Fields of the struct.
    pub fn fields(&self) -> &[AdtField] {
        &self.fields
    }
}

/// A field within a [`Struct`] or [`Union`].
#[derive(Debug)]
pub struct AdtField {
    name: Str,
    ty: Str,
    description: Option<Str>,
}

impl AdtField {
    /// Name of the field.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the field.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Type of the field.
    pub fn ty(&self) -> &str {
        &self.ty
    }
}

/// A value with possible variants.
#[derive(Debug)]
pub struct Enum {
    name: Str,
    description: Option<Str>,
    size: u32,
    repr_type: Option<Str>,
    variants: Box<[EnumVariant]>,
    by_name: BTreeMap<Str, EnumVariantRef>,
}

impl Enum {
    /// Name of the type.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the type.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Size of the type, typically in bytes.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// What this type may be represented as when generating code.
    pub fn repr_type(&self) -> Option<&str> {
        self.repr_type.as_deref()
    }

    /// Variants of this enum.
    pub fn variants(&self) -> &[EnumVariant] {
        &self.variants
    }

    /// Gets a ref to a specific variant.
    pub fn get_variant(&self, variant: &str) -> Option<EnumVariantRef> {
        self.by_name.get(variant).copied()
    }
}

/// A variant of an enum type.
#[derive(Debug)]
pub struct EnumVariant {
    name: Str,
    description: Option<Str>,
    ordinal: u32,
    properties: BTreeMap<Str, Property>,
}

impl EnumVariant {
    /// Name of the variant
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the variant
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Ordinal index of the variant
    pub fn ordinal(&self) -> u32 {
        self.ordinal
    }

    /// Specific property of this variant
    pub fn property(&self, name: &str) -> Option<&Property> {
        self.properties.get(name)
    }

    /// Properties of this variant
    pub fn properties(&self) -> impl Iterator<Item = (&str, &Property)> {
        self.properties
            .iter()
            .map(|(name, value)| (name.as_str(), value))
    }
}

/// A property of an enum variant.
///
/// All variants of an enum must have the same set of property names.
#[derive(Debug)]
pub struct Property {
    name: Str,
    value: PropertyValue,
}

impl Property {
    /// Name of this property.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Value of this property.
    pub fn value(&self) -> &PropertyValue {
        &self.value
    }

    /// What kind of value this property has.
    pub fn kind(&self) -> PropertyKind {
        self.value.kind()
    }
}

/// Value of a potential property.
#[derive(Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum PropertyValue {
    String(Str),
    Number(i128),
    Bool(bool),
}

impl PropertyValue {
    /// What kind of value this value is.
    pub fn kind(&self) -> PropertyKind {
        match self {
            PropertyValue::String(_) => PropertyKind::String,
            PropertyValue::Number(_) => PropertyKind::Number,
            PropertyValue::Bool(_) => PropertyKind::Bool,
        }
    }
}

/// Possible values that a property can store.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum PropertyKind {
    String,
    Number,
    Bool,
}

impl Display for PropertyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            PropertyKind::String => "string",
            PropertyKind::Number => "integer",
            PropertyKind::Bool => "bool",
        })
    }
}

/// A value with possible variants, each containing a different set of dynamic
/// data.
#[derive(Debug)]
pub struct Union {
    name: Str,
    description: Option<Str>,
    // size of the tag
    tag_size: u32,
    // size of the tag + size of the largest variant
    size: u32,
    repr_type: Option<Str>,
    variants: Box<[UnionVariant]>,
    by_name: BTreeMap<Str, UnionVariantRef>,
}

impl Union {
    /// Name of the type.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the type.
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Size of the tag type, typically in bytes.
    pub fn tag_size(&self) -> u32 {
        self.tag_size
    }

    /// Size of the type, typically in bytes.
    ///
    /// This is computed from the sum of the tag size and the size of the
    /// largest variant.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// What the tag of this type may be represented as when generating code.
    pub fn repr_type(&self) -> Option<&str> {
        self.repr_type.as_deref()
    }

    /// Variants of this union.
    pub fn variants(&self) -> &[UnionVariant] {
        &self.variants
    }

    /// Gets a ref to a specific variant.
    pub fn get_variant(&self, variant: &str) -> Option<UnionVariantRef> {
        self.by_name.get(variant).copied()
    }
}

/// A variant of a union type.
#[derive(Debug)]
pub struct UnionVariant {
    name: Str,
    description: Option<Str>,
    size: u32,
    ordinal: u32,
    fields: Box<[AdtField]>,
}

impl UnionVariant {
    /// Name of the variant
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Detailed description of the variant
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    /// Size of the variant, typically in bytes.
    pub fn size(&self) -> u32 {
        self.size
    }

    /// Ordinal index of the variant
    pub fn ordinal(&self) -> u32 {
        self.ordinal
    }

    /// Fields of the variant.
    pub fn fields(&self) -> &[AdtField] {
        &self.fields
    }
}

#[cfg(test)]
mod tests {}
