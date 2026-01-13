use std::{
    borrow::Borrow,
    collections::{BTreeMap, HashMap, HashSet},
    hash::Hash,
    num::NonZeroUsize,
    ops::{ControlFlow, Index},
};

use either::Either;
use indexmap::IndexSet;

use crate::{
    AdtField, BytecodeSpec, CommonNodes, ConditionalExpr, Enum, EnumVariant, Group, Immediate,
    Instruction, KnownAttrs, NameKind, Operand, ParseError, PredicateOp, PredicateValue, Property,
    PropertyValue, Scalar, StackAfter, StackBefore, StackEffect, StringList, Struct, Type,
    TypeKindNames, Types, Union, UnionVariant, ast,
    entities::{EnumRef, EnumVariantRef, ImmediateOperandRef, TypeRef, UnionRef, UnionVariantRef},
};

const SCALAR_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description, KnownAttrs::ReprType];
const ENUM_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description, KnownAttrs::ReprType];
const ENUM_VARIANT_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];
const STRUCT_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];
const UNION_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description, KnownAttrs::ReprType];
const UNION_VARIANT_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];
const ADT_FIELD_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];

const GROUP_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];

const NO_ATTRS: &[KnownAttrs] = &[];
const INSTRUCTION_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];
const IMMEDIATE_OPERAND_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];
const STACK_BEFORE_OPERAND_ATTRS: &[KnownAttrs] = &[
    KnownAttrs::Description,
    KnownAttrs::Computed,
    KnownAttrs::ComputedOffset,
];
const STACK_AFTER_OPERAND_ATTRS: &[KnownAttrs] = &[
    KnownAttrs::Description,
    KnownAttrs::Computed,
    KnownAttrs::ComputedOffset,
    KnownAttrs::Preserves,
    KnownAttrs::PushedIf,
];
const EXCEPTION_CASE_ATTRS: &[KnownAttrs] = &[KnownAttrs::Description];

const NO_CHILDREN: &[&str] = &[];
const TOP_LEVEL_CHILD_NODES: &[&str] = &["types", "exceptions", "instructions"];
const INSTRUCTION_CHILD_NODES: &[&str] = &["operands", "stack_before", "stack_after", "exceptions"];

const NO_AT_NODES: &[&str] = &[];
const STACK_OPERAND_LIST_AT_NODES: &[&str] = &["@conditional", "@conditional-case"];
const CONDITIONAL_CASE_AT_NODES: &[&str] = &["@case"];
const CONDITIONAL_AT_NODES: &[&str] = &["@predicate"];

pub(crate) fn parse_spec(text: &str) -> Result<BytecodeSpec, ParseError> {
    let kdl: kdl::KdlDocument = text.parse()?;
    expect_child_names(Some(&kdl), TOP_LEVEL_CHILD_NODES)?;

    let types = get_required_node(&kdl, CommonNodes::TypeList)?;
    expect_attribute_names(types, NO_ATTRS)?;
    let types = get_required_children(types, CommonNodes::TypeList)?;
    let types = parse_types(types)?;

    let instructions = get_required_node(&kdl, CommonNodes::InstructionList)?;
    expect_attribute_names(instructions, NO_ATTRS)?;
    let instructions = get_required_children(instructions, CommonNodes::InstructionList)?;

    let InstructionList(instr_defs, group_defs) = parse_instruction_list(instructions, &types)?;

    Ok(BytecodeSpec {
        types,
        instructions: instr_defs.into_boxed_slice(),
        groups: group_defs.into_boxed_slice(),
    })
}

struct InstructionList(Vec<Instruction>, Vec<(Group, std::ops::Range<usize>)>);

fn parse_instruction_list(
    instructions: &kdl::KdlDocument,
    types: &Types,
) -> Result<InstructionList, ParseError> {
    let mut instr_defs = vec![];
    let mut group_defs = vec![];

    let mut mnemonic_defs = DefsTracker::<_>::new(NameKind::Mnemonic);
    let mut opcode_defs = DefsTracker::<_>::new(NameKind::InstructionOpcode);

    let mut record_duplicates =
        |def: Instruction, entry: &kdl::KdlNode| -> Result<Instruction, ParseError> {
            mnemonic_defs.track_def(def.mnemonic().to_owned(), entry.name().span())?;

            let opcode_span = entry.entry(0).expect("should have opcode number").span();
            opcode_defs.track_def(def.opcode(), opcode_span)?;

            Ok(def)
        };

    for entry in instructions.nodes() {
        if entry.name().value() == "group" {
            expect_attribute_names(entry, GROUP_ATTRS)?;

            // as group!
            let children = get_required_children(entry, CommonNodes::GroupNode)?;
            let heading = parse_heading(entry, 0)?;
            let description = parse_description(entry)?;

            let group_start = instr_defs.len();

            for entry in children.nodes() {
                match entry.name().value() {
                    "group" => {
                        // can't have group here!
                        panic!(
                            "`group` cannot be used as an instruction mnemonic {:?}",
                            entry.name().span()
                        );
                    }
                    "-" => {
                        // skip attribute nodes
                    }
                    _ => {
                        // within!
                        instr_defs
                            .push(record_duplicates(parse_instruction(entry, types)?, entry)?);
                    }
                }
            }

            group_defs.push((
                Group {
                    heading: heading.map(String::from),
                    description: description.map(String::from),
                },
                group_start..instr_defs.len(),
            ));
        } else {
            // as single instruction
            instr_defs.push(record_duplicates(parse_instruction(entry, types)?, entry)?);
        }
    }

    Ok(InstructionList(instr_defs, group_defs))
}

fn parse_types(types: &kdl::KdlDocument) -> Result<Types, ParseError> {
    let mut type_defs = vec![];
    let mut types_by_name = BTreeMap::new();

    let mut name_defs = DefsTracker::new(NameKind::Type);
    let mut field_ty_checks = vec![];

    for entry in types.nodes() {
        let slot = TypeRef::from_usize(type_defs.len());

        let def = match entry.name().value() {
            "scalar" => Type::Scalar(parse_scalar(entry)?),
            "struct" => Type::Struct(parse_struct(entry, &mut field_ty_checks)?),
            "enum" => Type::Enum(parse_enum(entry, slot)?),
            "union" => Type::Union(parse_union(entry, slot, &mut field_ty_checks)?),
            _ => return Err(ParseError::InvalidTypeKind(entry.name().span())),
        };

        let def_span = entry.name().span();
        name_defs.track_def(def.name().to_owned(), def_span)?;

        types_by_name.insert(def.name().to_owned(), slot);
        type_defs.push(def);
    }

    let types = Types {
        types: type_defs.into_boxed_slice(),
        by_name: types_by_name,
    };

    check_field_types(&types, field_ty_checks)?;
    check_acyclic_types(&types).map_err(|err| {
        let mut tys: Vec<_> = err.participant_tys.iter().map(|ty| &types[*ty]).collect();
        let first = tys.remove(0).name().to_owned();
        let first_span = name_defs[first.as_str()];
        let participant_spans = tys.iter().map(|ty| name_defs[ty.name()]).collect();

        ParseError::CyclicType(first, first_span, participant_spans)
    })?;
    // TODO: struct size compute validation

    Ok(types)
}

fn check_field_types(
    types: &Types,
    checks: Vec<(String, miette::SourceSpan)>,
) -> Result<(), ParseError> {
    for (ty, field_ty_span) in checks {
        if types.get(&ty).is_none() {
            return Err(ParseError::UnknownTypeName(ty, field_ty_span));
        }
    }

    Ok(())
}

struct CycleError {
    participant_tys: Vec<TypeRef>,
}

fn check_acyclic_types(types: &Types) -> Result<(), CycleError> {
    let mut ty_stack = vec![];
    let mut visited_tys = HashSet::new();

    fn check_fields(
        ty: &Type,
        types: &Types,
        ty_stack: &mut Vec<TypeRef>,
        visited_tys: &mut HashSet<TypeRef>,
    ) -> Result<(), CycleError> {
        if matches!(ty, Type::Enum(_) | Type::Scalar(_)) {
            // Enums and scalars are leaf types, no need to traverse them.
            return Ok(());
        }

        let self_ref = types.get(ty.name()).expect("self ty should exist");

        ty_stack.push(self_ref);
        {
            if !visited_tys.insert(self_ref) {
                // Type has been visited before, fails occurs-check
                let cycle_start = ty_stack
                    .iter()
                    .position(|it| *it == self_ref)
                    .expect("cycle start ref should be in the type stack");

                return Err(CycleError {
                    participant_tys: Vec::from(&ty_stack[cycle_start..]),
                });
            }

            let mut visted_in_siblings = HashSet::new();

            let err = for_each_field(ty, |field| {
                let field_ty = types.get(field.ty()).expect("type should be known");

                if !visted_in_siblings.insert(field_ty) {
                    // Already visited in another sibling field
                    return ControlFlow::Continue(());
                }

                match check_fields(&types[field_ty], types, ty_stack, visited_tys) {
                    Ok(_) => ControlFlow::Continue(()),
                    Err(err) => ControlFlow::Break(err),
                }
            });

            if let Some(err) = err {
                return Err(err);
            }
        }
        ty_stack.pop();

        Ok(())
    }

    for (ty_ref, ty) in types.defs() {
        if visited_tys.contains(&ty_ref) {
            continue;
        }

        check_fields(ty, types, &mut ty_stack, &mut visited_tys)?;
    }

    Ok(())
}

fn parse_scalar(node: &kdl::KdlNode) -> Result<Scalar, ParseError> {
    expect_attribute_names(node, SCALAR_ATTRS)?;

    let name = parse_required_string_entry(node, 0)?;
    let size = parse_required_u32_entry(node, "size")?;
    let description = parse_description(node)?;
    let repr_type = parse_repr_type(node)?;

    Ok(Scalar {
        name: name.to_owned(),
        description: description.map(String::from),
        size,
        repr_type: repr_type.map(String::from),
    })
}

fn parse_struct(
    node: &kdl::KdlNode,
    field_ty_checks: &mut Vec<(String, miette::SourceSpan)>,
) -> Result<Struct, ParseError> {
    expect_attribute_names(node, STRUCT_ATTRS)?;

    let name = parse_required_string_entry(node, 0)?;
    let size = parse_required_u32_entry(node, "size")?;
    let description = parse_description(node)?;
    let fields = parse_adt_fields(node, CommonNodes::StructNode, field_ty_checks)?;

    Ok(Struct {
        name: name.to_owned(),
        description: description.map(String::from),
        size,
        fields: fields.into_boxed_slice(),
    })
}

fn parse_adt_fields(
    node: &kdl::KdlNode,
    node_type: CommonNodes,
    field_ty_checks: &mut Vec<(String, miette::SourceSpan)>,
) -> Result<Vec<AdtField>, ParseError> {
    let fields = get_required_children(node, node_type)?;
    let mut field_defs = vec![];
    let mut field_name_defs = DefsTracker::new(NameKind::Field);

    for field_entry in fields.nodes() {
        let name = field_entry.name().value();

        // Skip attribute nodes
        if matches!(name, "-") {
            continue;
        }

        expect_attribute_names(field_entry, ADT_FIELD_ATTRS)?;

        let ty = parse_required_string_entry(field_entry, 0)?;
        let description = parse_description(field_entry)?;

        field_ty_checks.push((
            ty.to_owned(),
            field_entry.entry(0).expect("field should have type").span(),
        ));

        field_name_defs.track_def(name.to_owned(), field_entry.name().span())?;

        field_defs.push(AdtField {
            name: name.to_owned(),
            ty: ty.to_owned(),
            description: description.map(String::from),
        });
    }

    Ok(field_defs)
}

fn parse_enum(node: &kdl::KdlNode, slot: TypeRef) -> Result<Enum, ParseError> {
    expect_attribute_names(node, ENUM_ATTRS)?;

    let slot = EnumRef(slot);

    let name = parse_required_string_entry(node, 0)?;
    let size = parse_required_u32_entry(node, "size")?;
    let description = parse_description(node)?;
    let repr_type = parse_repr_type(node)?;

    let variants = get_required_children(node, CommonNodes::EnumNode)?;
    let mut variant_names = DefsTracker::new(NameKind::Variant);
    let mut variant_defs = vec![];
    let mut ordinal_iota = 0;
    let mut property_types = HashMap::new();

    for variant_entry in variants.nodes() {
        let name = variant_entry.name().value();

        // Skip top-level attribute nodes
        if matches!(name, "-") {
            continue;
        }

        expect_attribute_names(variant_entry, ENUM_VARIANT_ATTRS)?;

        let ordinal = parse_u32_entry(variant_entry, 0)?.unwrap_or(ordinal_iota);
        let description = parse_description(variant_entry)?;
        let mut properties = BTreeMap::new();

        if let Some(variant_properties) = variant_entry.children().map(|it| it.nodes()) {
            let mut property_names = DefsTracker::new(NameKind::Property);

            for property_entry in variant_properties {
                let name = property_entry.name().value();

                // Skip attribute nodes
                if matches!(name, "-") {
                    continue;
                }

                let value = property_entry
                    .entry(0)
                    .ok_or_else(|| ParseError::ExpectedValue(property_entry.name().span()))?;
                let value_span = value.span();
                let value = match value.value() {
                    kdl::KdlValue::String(v) => PropertyValue::String(v.to_owned()),
                    kdl::KdlValue::Integer(v) => PropertyValue::Number(*v),
                    kdl::KdlValue::Bool(v) => PropertyValue::Bool(*v),
                    kdl::KdlValue::Float(_) | kdl::KdlValue::Null => {
                        return Err(ParseError::InvalidValue(value.span()));
                    }
                };

                // Ensure property type is consistent
                if let Some(expected_ty) = property_types.insert(name.to_owned(), value.kind())
                    && expected_ty != value.kind() {
                        return Err(ParseError::MismatchedPropertyTypes(
                            value_span,
                            expected_ty,
                            value.kind(),
                        ));
                    }

                property_names.track_def(name.to_owned(), property_entry.name().span())?;

                properties.insert(
                    name.to_owned(),
                    Property {
                        name: name.to_owned(),
                        value,
                    },
                );
            }
        }

        variant_names.track_def(name.to_owned(), variant_entry.name().span())?;

        variant_defs.push(EnumVariant {
            name: name.to_owned(),
            ordinal,
            description: description.map(String::from),
            properties,
        });

        ordinal_iota = ordinal + 1;
    }

    // Ensure variants all have the same properties
    let all_properties: HashSet<_> = variant_defs
        .iter()
        .flat_map(|variant| variant.properties().map(|(name, _)| name))
        .collect();

    for variant in &variant_defs {
        let has_properties: HashSet<_> = variant.properties().map(|(name, _)| name).collect();

        let mut missing: Vec<_> = all_properties
            .difference(&has_properties)
            .copied()
            .map(String::from)
            .collect();

        if !missing.is_empty() {
            missing.sort();

            return Err(ParseError::MissingVariantProperties(
                variant_names[variant.name()],
                StringList(missing),
            ));
        }
    }

    let by_name = variant_defs
        .iter()
        .enumerate()
        .map(|(index, variant)| (variant.name().to_owned(), EnumVariantRef(slot, index)))
        .collect();

    Ok(Enum {
        slot,
        name: name.to_owned(),
        description: description.map(String::from),
        size,
        repr_type: repr_type.map(String::from),
        variants: variant_defs.into_boxed_slice(),
        by_name,
    })
}

fn parse_union(
    node: &kdl::KdlNode,
    slot: TypeRef,
    field_ty_checks: &mut Vec<(String, miette::SourceSpan)>,
) -> Result<Union, ParseError> {
    expect_attribute_names(node, UNION_ATTRS)?;

    let slot = UnionRef(slot);

    let name = parse_required_string_entry(node, 0)?;
    let tag_size = parse_required_u32_entry(node, "tag_size")?;
    let description = parse_description(node)?;
    let repr_type = parse_repr_type(node)?;

    let variants = get_required_children(node, CommonNodes::UnionNode)?;
    let mut variant_names = DefsTracker::new(NameKind::Variant);
    let mut variant_defs = vec![];
    let mut ordinal_iota = 0;

    for variant_entry in variants.nodes() {
        let name = variant_entry.name().value();

        // Skip attribute nodes
        if matches!(name, "-") {
            continue;
        }

        expect_attribute_names(variant_entry, UNION_VARIANT_ATTRS)?;

        let size = parse_required_u32_entry(variant_entry, "size")?;
        let ordinal = parse_u32_entry(variant_entry, 0)?.unwrap_or(ordinal_iota);
        let description = parse_description(variant_entry)?;
        let fields = parse_adt_fields(
            variant_entry,
            CommonNodes::UnionVariantNode,
            field_ty_checks,
        )?;

        variant_names.track_def(name.to_owned(), variant_entry.name().span())?;

        variant_defs.push(UnionVariant {
            name: name.to_owned(),
            description: description.map(String::from),
            size,
            ordinal,
            fields: fields.into_boxed_slice(),
        });

        ordinal_iota = ordinal + 1;
    }

    let by_name = variant_defs
        .iter()
        .enumerate()
        .map(|(index, variant)| (variant.name().to_owned(), UnionVariantRef(slot, index)))
        .collect();

    let size = tag_size
        + variant_defs
            .iter()
            .map(|variant| variant.size())
            .max()
            .unwrap_or(0u32);

    Ok(Union {
        slot,
        name: name.to_owned(),
        description: description.map(String::from),
        tag_size,
        size,
        repr_type: repr_type.map(String::from),
        variants: variant_defs.into_boxed_slice(),
        by_name,
    })
}

fn parse_instruction(node: &kdl::KdlNode, types: &Types) -> Result<Instruction, ParseError> {
    expect_attribute_names(node, INSTRUCTION_ATTRS)?;
    expect_child_names(node.children(), INSTRUCTION_CHILD_NODES)?;

    let opcode = parse_required_u32_entry(node, 0)?;
    let heading = parse_heading(node, 1)?;
    let description = parse_description(node)?;

    let immediate_operands = if let Some(operands) = find_child(node, "operands") {
        expect_attribute_names(operands, NO_ATTRS)?;

        let operands = get_required_children(operands, CommonNodes::OperandsList)?;
        let mut operand_defs = vec![];
        let mut operand_names = DefsTracker::new(NameKind::ImmediateOperand);

        for entry in operands.nodes() {
            let def = parse_immediate_operand(entry, types)?;
            operand_names.track_def(def.name.to_owned(), entry.name().span())?;
            operand_defs.push(def);
        }

        operand_defs
    } else {
        // No explicit immediate operands
        vec![]
    };
    let immediate_operand_refs = NameLookup::from_names(
        NameKind::ImmediateOperand,
        immediate_operands
            .iter()
            .enumerate()
            .map(|(index, operand)| {
                (
                    operand.name(),
                    (ImmediateOperandRef::from_usize(index), operand),
                )
            }),
    );

    // notes:
    // - enums: be exhaustive, only allow eq or not eq (invert not eq to other cases)
    // - numbers: don't allow overlapping ranges (i.e. only lt le gt ge comparison with one number)
    let stack_before_list = parse_stack_before_list(node)?.unwrap_or_default();
    let stack_after_list = parse_stack_after_list(node)?.unwrap_or_default();
    let stack_effects = lower_stack_effects(
        stack_before_list,
        stack_after_list,
        types,
        &immediate_operand_refs,
        &immediate_operands,
    )?;

    Ok(Instruction {
        mnemonic: node.name().value().to_owned(),
        opcode,
        heading: heading.map(String::from),
        description: description.map(String::from),
        immediate_operands: immediate_operands.into_boxed_slice(),
        stack_effects: stack_effects.into_boxed_slice(),
    })
}

fn parse_immediate_operand(
    node: &kdl::KdlNode,
    types: &Types,
) -> Result<Operand<Immediate>, ParseError> {
    expect_attribute_names(node, IMMEDIATE_OPERAND_ATTRS)?;

    let name = node.name().value();
    let ty = parse_required_string_entry(node, 0)?;
    let Some(ty) = types.get(ty) else {
        let ty_span = node.entry(0).expect("must have type entry");
        return Err(ParseError::UnknownTypeName(ty.to_owned(), ty_span.span()));
    };
    let description = parse_description(node)?;
    let unused = parse_bool_entry(node, "unused")?.unwrap_or(false);

    // other things!
    Ok(Operand {
        name: name.to_owned(),
        ty,
        description: description.map(String::from),
        unused,
        special: Immediate {},
    })
}

fn parse_stack_before_list<'kdl>(
    node: &'kdl kdl::KdlNode,
) -> Result<Option<ast::StackBeforeList<'kdl>>, ParseError> {
    let Some(node) = find_child(node, "stack_before") else {
        return Ok(None);
    };

    expect_attribute_names(node, NO_ATTRS)?;
    expect_child_at_names(node.children(), STACK_OPERAND_LIST_AT_NODES)?;

    let operands = get_required_children(node, CommonNodes::OperandsList)?;
    let operand_list = ast::StackBeforeList {
        entries: parse_maybe_conditional_nodes(operands, parse_stack_before_operand)?,
    };

    Ok(Some(operand_list))
}

fn parse_stack_after_list<'kdl>(
    node: &'kdl kdl::KdlNode,
) -> Result<Option<ast::StackAfterList<'kdl>>, ParseError> {
    let Some(node) = find_child(node, "stack_after") else {
        return Ok(None);
    };

    expect_attribute_names(node, NO_ATTRS)?;
    expect_child_at_names(node.children(), STACK_OPERAND_LIST_AT_NODES)?;

    let operands = get_required_children(node, CommonNodes::OperandsList)?;
    let operand_list = ast::StackAfterList {
        entries: parse_maybe_conditional_nodes(operands, parse_stack_after_operand)?,
    };

    Ok(Some(operand_list))
}

fn parse_stack_before_operand<'kdl>(
    node: &'kdl kdl::KdlNode,
) -> Result<ast::Spanned<ast::StackBeforeOperand<'kdl>>, ParseError> {
    expect_attribute_names(node, STACK_BEFORE_OPERAND_ATTRS)?;

    let name = ast::Spanned::new(node.name().value(), node.name().span());
    let ty = parse_required_spanned_string_entry(node, 0)?;
    let description = parse_spanned_description(node)?;
    let unused = parse_spanned_bool_entry(node, "unused")?;
    let variadic = parse_spanned_bool_entry(node, "variadic")?;

    // TODO: Computed element/offset operands
    // TODO: Reject preserves, preserves-if
    Ok(ast::Spanned::new(
        ast::StackBeforeOperand {
            name,
            ty,
            description,
            unused,
            variadic,
            computed: None,
            computed_offset: None,
        },
        node.name().span(),
    ))
}

fn parse_stack_after_operand<'kdl>(
    node: &'kdl kdl::KdlNode,
) -> Result<ast::Spanned<ast::StackAfterOperand<'kdl>>, ParseError> {
    expect_attribute_names(node, STACK_AFTER_OPERAND_ATTRS)?;

    let name = ast::Spanned::new(node.name().value(), node.name().span());
    let ty = parse_required_spanned_string_entry(node, 0)?;
    let description = parse_spanned_description(node)?;
    let unused = parse_spanned_bool_entry(node, "unused")?;

    // TODO: Preserves, Push-If, Computed element/offset operands
    // TODO: Reject variadic
    Ok(ast::Spanned::new(
        ast::StackAfterOperand {
            name,
            ty,
            description,
            unused,
            preserves: None,
            computed: None,
            computed_offset: None,
        },
        node.name().span(),
    ))
}

fn lower_stack_effects(
    stack_before_list: ast::StackBeforeList<'_>,
    stack_after_list: ast::StackAfterList<'_>,
    types: &Types,
    immediate_operand_refs: &NameLookup<&str, (ImmediateOperandRef, &Operand<Immediate>)>,
    immediate_operands: &[Operand<Immediate>],
) -> Result<Vec<StackEffect>, ParseError> {
    let mut conditional_defs = ConditionalDefs::default();
    let mut used_immediates = vec![];
    let mut stack_effects = vec![];

    let stack_before_operands = collect_stack_operands(
        &stack_before_list.entries,
        &mut conditional_defs,
        &mut used_immediates,
        immediate_operand_refs,
        types,
    )?;

    let stack_after_operands = collect_stack_operands(
        &stack_after_list.entries,
        &mut conditional_defs,
        &mut used_immediates,
        immediate_operand_refs,
        types,
    )?;

    // Push no-predicate stack effect first
    {
        let stack_before = lower_stack_operands(
            &stack_before_operands.pick_operands(&stack_before_list.entries, None),
            |operand| lower_stack_before_operand(operand, types, immediate_operand_refs),
            NameKind::StackBeforeOperand,
        )?;

        let stack_after = lower_stack_operands(
            &stack_after_operands.pick_operands(&stack_after_list.entries, None),
            |operand| lower_stack_after_operand(operand, types, immediate_operand_refs),
            NameKind::StackAfterOperand,
        )?;

        stack_effects.push(StackEffect {
            predicate: None,
            stack_before: stack_before.into_boxed_slice(),
            stack_after: stack_after.into_boxed_slice(),
        });
    }

    let Some(same_immediate) = used_immediates.first() else {
        // No conditional decodes to process
        return Ok(stack_effects);
    };

    // Ensure that the used immediates refer to the same operand
    let different_immediates: Vec<_> = used_immediates
        .iter()
        .flat_map(|it| (it.value() != same_immediate.value()).then_some(it.span()))
        .collect();

    if !different_immediates.is_empty() {
        return Err(ParseError::MismatchedImmediateOperands(
            immediate_operands[same_immediate.value().index()]
                .name()
                .to_owned(),
            same_immediate.span(),
            different_immediates,
        ));
    }

    match &types[immediate_operands[same_immediate.value().index()].ty()] {
        Type::Scalar(_) => {
            // Pick first condition, if applicable
            if let Some(predicate) = conditional_defs
                .get_single_condition()
                .expect("more than one scalar condition")
            {
                let stack_before = lower_stack_operands(
                    &stack_before_operands
                        .pick_operands(&stack_before_list.entries, Some(predicate)),
                    |operand| lower_stack_before_operand(operand, types, immediate_operand_refs),
                    NameKind::StackBeforeOperand,
                )?;

                let stack_after = lower_stack_operands(
                    &stack_after_operands.pick_operands(&stack_after_list.entries, Some(predicate)),
                    |operand| lower_stack_after_operand(operand, types, immediate_operand_refs),
                    NameKind::StackAfterOperand,
                )?;

                let (op, value) = conditional_defs[predicate];
                let predicate = Box::new(ConditionalExpr {
                    lhs: *same_immediate.value(),
                    op,
                    rhs: value,
                });

                // insert before the default stack condition
                stack_effects.insert(
                    0,
                    StackEffect {
                        predicate: Some(predicate),
                        stack_before: stack_before.into_boxed_slice(),
                        stack_after: stack_after.into_boxed_slice(),
                    },
                );
            }
        }
        Type::Struct(_) => unreachable!(),
        Type::Enum(ty) => {
            // Enums are treated as being non-exhaustive, we'll push all variant predicates here
            stack_effects.clear();

            // Declare stack effect predicates in definition order
            for (variant_ref, _) in ty.variants_with_refs() {
                let value = PredicateValue::EnumVariantRef(variant_ref);
                let predicate = conditional_defs.get_condition(PredicateOp::Eq, value);

                let stack_before = lower_stack_operands(
                    &stack_before_operands.pick_operands(&stack_before_list.entries, predicate),
                    |operand| lower_stack_before_operand(operand, types, immediate_operand_refs),
                    NameKind::StackBeforeOperand,
                )?;

                let stack_after = lower_stack_operands(
                    &stack_after_operands.pick_operands(&stack_after_list.entries, predicate),
                    |operand| lower_stack_after_operand(operand, types, immediate_operand_refs),
                    NameKind::StackAfterOperand,
                )?;

                stack_effects.push(StackEffect {
                    predicate: Some(Box::new(ConditionalExpr {
                        lhs: *same_immediate.value(),
                        op: PredicateOp::Eq,
                        rhs: value,
                    })),
                    stack_before: stack_before.into_boxed_slice(),
                    stack_after: stack_after.into_boxed_slice(),
                });
            }
        }
        Type::Union(ty) => {
            // Unions are treated as being non-exhaustive, we'll push all variant predicates here
            stack_effects.clear();

            // Declare stack effect predicates in definition order
            for (variant_ref, _) in ty.variants_with_refs() {
                let value = PredicateValue::UnionVariantRef(variant_ref);
                let predicate = conditional_defs.get_condition(PredicateOp::Eq, value);

                let stack_before = lower_stack_operands(
                    &stack_before_operands.pick_operands(&stack_before_list.entries, predicate),
                    |operand| lower_stack_before_operand(operand, types, immediate_operand_refs),
                    NameKind::StackBeforeOperand,
                )?;

                let stack_after = lower_stack_operands(
                    &stack_after_operands.pick_operands(&stack_after_list.entries, predicate),
                    |operand| lower_stack_after_operand(operand, types, immediate_operand_refs),
                    NameKind::StackAfterOperand,
                )?;

                stack_effects.push(StackEffect {
                    predicate: Some(Box::new(ConditionalExpr {
                        lhs: *same_immediate.value(),
                        op: PredicateOp::Eq,
                        rhs: value,
                    })),
                    stack_before: stack_before.into_boxed_slice(),
                    stack_after: stack_after.into_boxed_slice(),
                });
            }
        }
    }

    Ok(stack_effects)
}

fn collect_stack_operands<V>(
    entries: &[ast::Spanned<ast::MaybeConditional<'_, V>>],
    conditional_defs: &mut ConditionalDefs,
    used_immediates: &mut Vec<ast::Spanned<ImmediateOperandRef>>,
    immediate_operand_refs: &NameLookup<&str, (ImmediateOperandRef, &Operand<Immediate>)>,
    types: &Types,
) -> Result<StackOperands, ParseError> {
    let mut stack_operands = StackOperands::default();

    for (index, entry) in entries.iter().enumerate() {
        match entry.value() {
            ast::MaybeConditional::Conditional(conditional) => {
                let slot = ConditionalEntry::Predicate(index);

                match &conditional.predicates {
                    Either::Left(predicates) => {
                        for predicate in predicates {
                            let predicate = predicate.value();

                            let imm = predicate
                                .immediate_operand
                                .try_map(|it| immediate_operand_refs.lookup(it).copied())?;
                            let (imm_ref, imm) = imm.fold_inner(|it| {
                                let span = it.span();
                                let (imm_ref, imm) = it.into_inner();
                                (
                                    ast::Spanned::new(imm_ref, span),
                                    ast::Spanned::new(imm, span),
                                )
                            });
                            used_immediates.push(imm_ref);

                            let value = lower_predicate_value(&predicate.value, types)?;
                            check_predicate_op_value(
                                imm.map(|it| it.ty()),
                                &predicate.op,
                                &value,
                                types,
                            )?;

                            match (&types[imm.value().ty()], value.value()) {
                                (Type::Scalar(_), PredicateValue::Number(_)) => {
                                    // No special conditions for dealing with predicates, aside from there being only one condition
                                    let conditional_slot = conditional_defs.insert_condition(
                                        *predicate.op.value(),
                                        value.into_inner(),
                                    );

                                    if conditional_slot.index() > 0 {
                                        return Err(ParseError::MultipleScalarPredicates(
                                            predicate.op.span(),
                                        ));
                                    }

                                    stack_operands
                                        .visible_when
                                        .entry(conditional_slot)
                                        .or_default()
                                        .push(slot);
                                }
                                (Type::Enum(ty), PredicateValue::EnumVariantRef(not_variant))
                                    if predicate.op.value() == &PredicateOp::NotEq =>
                                {
                                    // Invert condition
                                    let other_variants: Vec<_> = ty
                                        .variants_with_refs()
                                        .flat_map(|(variant_ref, _)| {
                                            (variant_ref != *not_variant).then_some(variant_ref)
                                        })
                                        .collect();

                                    // Splat out conditions
                                    for variant in other_variants {
                                        let conditional_slot = conditional_defs.insert_condition(
                                            PredicateOp::Eq,
                                            PredicateValue::EnumVariantRef(variant),
                                        );

                                        stack_operands
                                            .visible_when
                                            .entry(conditional_slot)
                                            .or_default()
                                            .push(slot);
                                    }
                                }
                                (Type::Union(ty), PredicateValue::UnionVariantRef(not_variant))
                                    if predicate.op.value() == &PredicateOp::NotEq =>
                                {
                                    // Invert condition
                                    let other_variants: Vec<_> = ty
                                        .variants_with_refs()
                                        .flat_map(|(variant_ref, _)| {
                                            (variant_ref != *not_variant).then_some(variant_ref)
                                        })
                                        .collect();

                                    // Splat out conditions
                                    for variant in other_variants {
                                        let conditional_slot = conditional_defs.insert_condition(
                                            PredicateOp::Eq,
                                            PredicateValue::UnionVariantRef(variant),
                                        );

                                        stack_operands
                                            .visible_when
                                            .entry(conditional_slot)
                                            .or_default()
                                            .push(slot);
                                    }
                                }
                                (Type::Enum(_), PredicateValue::EnumVariantRef(_))
                                | (Type::Union(_), PredicateValue::UnionVariantRef(_)) => {
                                    // Variant predicate that doesn't need to be handled specially
                                    let conditional_slot = conditional_defs.insert_condition(
                                        *predicate.op.value(),
                                        value.into_inner(),
                                    );

                                    stack_operands
                                        .visible_when
                                        .entry(conditional_slot)
                                        .or_default()
                                        .push(slot);
                                }
                                _ => unreachable!(),
                            }
                        }
                    }

                    Either::Right(_otherwise) => stack_operands.otherwise.push(slot),
                }
            }
            ast::MaybeConditional::ConditionalCase(conditional_case) => {
                let imm = conditional_case
                    .immediate_decode_match
                    .try_map(|it| immediate_operand_refs.lookup(it).copied())?;
                let (imm_ref, imm) = imm.fold_inner(|it| {
                    let span = it.span();
                    let (imm_ref, imm) = it.into_inner();
                    (
                        ast::Spanned::new(imm_ref, span),
                        ast::Spanned::new(imm, span),
                    )
                });
                used_immediates.push(imm_ref);

                for (arm_index, arm) in conditional_case.arms.iter().enumerate() {
                    let slot = ConditionalEntry::Case(index, arm_index);

                    // Treat case as equivalent to a series of eq predicates
                    for value in &arm.value().match_values {
                        let value = lower_predicate_value(value, types)?;
                        check_predicate_op_value(
                            imm.map(|it| it.ty()),
                            &ast::Spanned::new(PredicateOp::Eq, arm.span()),
                            &value,
                            types,
                        )?;

                        let conditional_slot =
                            conditional_defs.insert_condition(PredicateOp::Eq, value.into_inner());

                        stack_operands
                            .visible_when
                            .entry(conditional_slot)
                            .or_default()
                            .push(slot);
                    }
                }
            }
            ast::MaybeConditional::Operand(_) => {
                stack_operands.always.push(ConditionalEntry::Always(index))
            }
        }
    }

    Ok(stack_operands)
}

fn check_predicate_op_value(
    imm_ty: ast::Spanned<TypeRef>,
    op: &ast::Spanned<PredicateOp>,
    value: &ast::Spanned<PredicateValue>,
    types: &Types,
) -> Result<(), ParseError> {
    // Ensure that the op & value can be used for the type
    match &types[*imm_ty.value()] {
        Type::Scalar(_) => {
            // Scalars can use all of the comparison operations
            // Scalars can only use numbers
            if !matches!(value.value(), PredicateValue::Number(_)) {
                return Err(ParseError::ExpectedInteger(value.span()));
            }
        }
        Type::Struct(_) => {
            // Structs can never be used in conditionals
            return Err(ParseError::UnexpectedTypeKind(
                TypeKindNames::Struct,
                imm_ty.span(),
                StringList(
                    [
                        TypeKindNames::Scalar,
                        TypeKindNames::Enum,
                        TypeKindNames::Union,
                    ]
                    .into_iter()
                    .map(|it| it.to_string())
                    .collect(),
                ),
            ));
        }
        Type::Enum(_) | Type::Union(_) => {
            if !matches!(op.value(), PredicateOp::Eq | PredicateOp::NotEq) {
                return Err(ParseError::InvalidPredicateInequality(op.span()));
            }

            let value_ty = match value.value() {
                PredicateValue::EnumVariantRef(variant_ref) => variant_ref.ty().type_ref(),
                PredicateValue::UnionVariantRef(variant_ref) => variant_ref.ty().type_ref(),
                PredicateValue::Number(_) => return Err(ParseError::ExpectedVariant(value.span())),
            };

            if *imm_ty.value() != value_ty {
                return Err(ParseError::MismatchedTypes(
                    types[*imm_ty.value()].name().to_owned(),
                    types[value_ty].name().to_owned(),
                    value.span(),
                ));
            }
        }
    }

    Ok(())
}

fn lower_predicate_value(
    value: &ast::Spanned<ast::PredicateValue<'_>>,
    types: &Types,
) -> Result<ast::Spanned<PredicateValue>, ParseError> {
    match value.value() {
        ast::PredicateValue::SumTypeVariant {
            enum_ty: base_ty,
            variant,
        } => {
            let Some(sum_ty) = types.get(base_ty.value()) else {
                return Err(ParseError::UnknownTypeName(
                    (*base_ty.value()).to_owned(),
                    base_ty.span(),
                ));
            };

            variant.try_map(|variant| match &types[sum_ty] {
                Type::Enum(ty) => {
                    let Some(variant) = ty.get_variant(variant.value()) else {
                        let variant_names = ty
                            .variants()
                            .iter()
                            .map(|it| it.name().to_owned())
                            .collect();

                        return Err(ParseError::UnknownName(
                            NameKind::Variant,
                            (*variant.value()).to_owned(),
                            variant.span(),
                            StringList(variant_names),
                        ));
                    };

                    Ok(PredicateValue::EnumVariantRef(variant))
                }
                Type::Union(ty) => {
                    let Some(variant) = ty.get_variant(variant.value()) else {
                        let variant_names = ty
                            .variants()
                            .iter()
                            .map(|it| it.name().to_owned())
                            .collect();

                        return Err(ParseError::UnknownName(
                            NameKind::Variant,
                            (*variant.value()).to_owned(),
                            variant.span(),
                            StringList(variant_names),
                        ));
                    };

                    Ok(PredicateValue::UnionVariantRef(variant))
                }
                other_ty @ (Type::Scalar(_) | Type::Struct(_)) => {
                    Err(ParseError::UnexpectedTypeKind(
                        TypeKindNames::from(other_ty),
                        base_ty.span(),
                        StringList(
                            [TypeKindNames::Enum, TypeKindNames::Union]
                                .into_iter()
                                .map(|it| it.to_string())
                                .collect(),
                        ),
                    ))
                }
            })
        }
        ast::PredicateValue::Number(v) => {
            Ok(ast::Spanned::new(PredicateValue::Number(*v), value.span()))
        }
    }
}

fn lower_stack_operands<'src, V, U>(
    entries: &[V],
    lower_operand: impl Fn(&V) -> Result<U, ParseError>,
    name_kind: NameKind,
) -> Result<Vec<U>, ParseError>
where
    V: 'src + ast::HasName,
{
    let mut operands = vec![];
    let mut defs_tracker = DefsTracker::new(name_kind);

    for entry in entries {
        let name = entry.name();
        defs_tracker.track_def(*name.value(), name.span())?;

        operands.push(lower_operand(entry)?);
    }

    Ok(operands)
}

fn lower_stack_before_operand<'kdl>(
    ast: &ast::StackBeforeOperand<'_>,
    types: &Types,
    _immediate_operands: &NameLookup<&'_ str, (ImmediateOperandRef, &Operand<Immediate>)>,
) -> Result<Operand<StackBefore>, ParseError> {
    let Some(ty) = types.get(ast.ty.value()) else {
        return Err(ParseError::UnknownTypeName(
            String::from(*ast.ty.value()),
            ast.ty.span(),
        ));
    };

    Ok(Operand {
        name: ast.name.map_inner(String::from),
        ty,
        description: ast.description.map(|it| it.map_inner(String::from)),
        unused: ast.unused.is_some_and(|it| it.into_inner()),
        special: StackBefore {
            variadic: ast.variadic.is_some_and(|it| it.into_inner()),
            computed: None,
            computed_offset: None,
        },
    })
}

fn lower_stack_after_operand<'kdl>(
    ast: &ast::StackAfterOperand<'_>,
    types: &Types,
    _immediate_operands: &NameLookup<&'_ str, (ImmediateOperandRef, &Operand<Immediate>)>,
) -> Result<Operand<StackAfter>, ParseError> {
    let Some(ty) = types.get(ast.ty.value()) else {
        return Err(ParseError::UnknownTypeName(
            String::from(*ast.ty.value()),
            ast.ty.span(),
        ));
    };

    Ok(Operand {
        name: ast.name.map_inner(String::from),
        ty,
        description: ast.description.map(|it| it.map_inner(String::from)),
        unused: ast.unused.is_some_and(|it| it.into_inner()),
        special: StackAfter {
            preserves: None,
            computed: None,
            computed_offset: None,
        },
    })
}

fn parse_maybe_conditional_nodes<'node, V>(
    children: &'node kdl::KdlDocument,
    parse_operand: impl Copy + Fn(&'node kdl::KdlNode) -> Result<ast::Spanned<V>, ParseError>,
) -> Result<Vec<ast::Spanned<ast::MaybeConditional<'node, V>>>, ParseError>
where
    V: 'node,
{
    let mut entries = vec![];

    for entry in children.nodes() {
        let entry = match entry.name().value() {
            "@conditional" => parse_conditional_node(entry, parse_operand)?
                .map(ast::MaybeConditional::Conditional),
            "@conditional-case" => parse_conditional_case_node(entry, parse_operand)?
                .map(ast::MaybeConditional::ConditionalCase),
            name if name.starts_with('@') => continue,
            _ => (parse_operand(entry)?).map(ast::MaybeConditional::Operand),
        };

        entries.push(entry);
    }

    Ok(entries)
}

fn parse_conditional_node<'node, V>(
    node: &'node kdl::KdlNode,
    parse_operand: impl Fn(&'node kdl::KdlNode) -> Result<ast::Spanned<V>, ParseError>,
) -> Result<ast::Spanned<ast::Conditional<'node, V>>, ParseError>
where
    V: 'node,
{
    let children = get_required_children(node, CommonNodes::ConditionalOperandList)?;
    expect_attribute_names(node, NO_ATTRS)?;
    expect_child_at_names(Some(children), CONDITIONAL_AT_NODES)?;

    // parse opcode entries
    let mut predicates = vec![];
    let mut otherwises = vec![];
    let mut operands = vec![];

    for entry in children.nodes() {
        match entry.name().value() {
            "@predicate" => parse_conditional_predicate_node(entry)?.either(
                |predicate| predicates.push(predicate),
                |otherwise| otherwises.push(otherwise),
            ),
            name if name.starts_with('@') => continue,
            _ => operands.push(parse_operand(entry)?),
        }
    }

    // Pick an otherwise to be the representative type
    // TODO: reject other non-otherwise predicates
    let predicates = if let Some(otherwise) = otherwises.pop() {
        Either::Right(otherwise)
    } else {
        Either::Left(predicates)
    };

    Ok(ast::Spanned::new(
        ast::Conditional {
            predicates,
            operands,
        },
        node.name().span(),
    ))
}

fn parse_conditional_predicate_node(
    node: &kdl::KdlNode,
) -> Result<
    Either<ast::Spanned<ast::ConditionalPredicate<'_>>, ast::OtherwisePredicate<'_>>,
    ParseError,
> {
    expect_attribute_names(node, NO_ATTRS)?;
    expect_child_names(node.children(), NO_CHILDREN)?;

    if let Some(maybe_otherwise) = node.entry(0)
        && maybe_otherwise.ty().is_none()
            && maybe_otherwise
                .value()
                .as_string()
                .is_some_and(|it| it == "otherwise")
        {
            return Ok(Either::Right(ast::OtherwisePredicate {
                _otherwise: ast::Spanned::new(
                    maybe_otherwise.value().as_string().unwrap(),
                    maybe_otherwise.span(),
                ),
            }));
        }

    let (_, immediate_operand) = parse_required_spanned_field_entry(node, 0, Some(&["operands"]))?;
    let op = parse_required_spanned_string_entry(node, 1)?.try_fold(|it| {
        let op: PredicateOp = it
            .value()
            .parse()
            .map_err(|_| ParseError::InvalidPredicateOp(it.span()))?;
        Ok::<_, ParseError>(ast::Spanned::new(op, it.span()))
    })?;
    let value = parse_predicate_value(node, 2)?;

    Ok(Either::Left(ast::Spanned::new(
        ast::ConditionalPredicate {
            immediate_operand,
            op,
            value,
        },
        node.name().span(),
    )))
}

fn parse_conditional_case_node<'node, V>(
    node: &'node kdl::KdlNode,
    parse_operand: impl Copy + Fn(&'node kdl::KdlNode) -> Result<ast::Spanned<V>, ParseError>,
) -> Result<ast::Spanned<ast::ConditionalCase<'node, V>>, ParseError>
where
    V: 'node,
{
    let children = get_required_children(node, CommonNodes::ConditionalCaseOperandList)?;
    expect_attribute_names(node, NO_ATTRS)?;
    expect_child_at_names(Some(children), CONDITIONAL_CASE_AT_NODES)?;

    let (_, immediate_decode_match) =
        parse_required_spanned_field_entry(node, 0, Some(&["operands"]))?;

    let mut arms = vec![];

    for entry in children.nodes() {
        match entry.name().value() {
            "@case" => arms.push(parse_conditional_case_arm(entry, parse_operand)?),
            _ => {
                return Err(ParseError::UnexpectedChildNode(
                    entry.name().span(),
                    StringList(vec!["@case".into()]),
                ));
            }
        };
    }

    Ok(ast::Spanned::new(
        ast::ConditionalCase {
            immediate_decode_match,
            arms,
        },
        node.name().span(),
    ))
}

fn parse_conditional_case_arm<'node, V>(
    node: &'node kdl::KdlNode,
    parse_operand: impl Fn(&'node kdl::KdlNode) -> Result<ast::Spanned<V>, ParseError>,
) -> Result<ast::Spanned<ast::ConditonalArm<'node, V>>, ParseError>
where
    V: 'node,
{
    let children = get_required_children(node, CommonNodes::ConditionalCaseArmOperandList)?;
    expect_attribute_names(node, NO_ATTRS)?;
    expect_child_at_names(Some(children), NO_AT_NODES)?;

    let mut match_values = vec![];
    for value in (0..node.entries().len()).map(|key| parse_predicate_value(node, key)) {
        match_values.push(value?);
    }

    let mut operands = vec![];
    for entry in children.nodes() {
        operands.push(parse_operand(entry)?);
    }

    Ok(ast::Spanned::new(
        ast::ConditonalArm {
            match_values,
            operands,
        },
        node.name().span(),
    ))
}

fn parse_predicate_value(
    node: &kdl::KdlNode,
    key: usize,
) -> Result<ast::Spanned<ast::PredicateValue<'_>>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Err(ParseError::ExpectedValue(node.name().span()));
    };

    let value = match entry.value() {
        kdl::KdlValue::String(_) => {
            // Always treat strings as fields
            let (field_base, field) = parse_required_spanned_field_entry(node, key, None)?;

            ast::PredicateValue::SumTypeVariant {
                enum_ty: field_base,
                variant: field,
            }
        }
        kdl::KdlValue::Integer(_) if entry.ty().is_some() => {
            return Err(ParseError::ExpectedInteger(entry.span()));
        }
        kdl::KdlValue::Integer(value) => ast::PredicateValue::Number(*value),
        _ => return Err(ParseError::InvalidValue(entry.span())),
    };

    Ok(ast::Spanned::new(value, entry.span()))
}

fn get_required_node(
    children: &kdl::KdlDocument,
    node_type: CommonNodes,
) -> Result<&kdl::KdlNode, ParseError> {
    match children.get(node_type.node_name()) {
        Some(it) => Ok(it),
        None => Err(ParseError::NodeRequired(node_type, children.span())),
    }
}

fn get_required_children(
    node: &kdl::KdlNode,
    node_type: CommonNodes,
) -> Result<&kdl::KdlDocument, ParseError> {
    let Some(children) = node.children() else {
        return Err(ParseError::ChildrenRequired(node.span(), node_type));
    };

    Ok(children)
}

/// Ensure that there are no unexpected or duplicate child nodes for a given node.
fn expect_child_names(
    children: Option<&kdl::KdlDocument>,
    accepted_childs: &[&'static str],
) -> Result<(), ParseError> {
    let Some(children) = children else {
        return Ok(());
    };

    // validate node names
    for child in children.nodes().iter() {
        if matches!(child.name().value(), "-") {
            // Skip attribute nodes, those are separately checked
            continue;
        }

        if !accepted_childs.contains(&child.name().value()) {
            return Err(ParseError::UnexpectedChildNode(
                child.name().span(),
                StringList(accepted_childs.iter().map(|it| String::from(*it)).collect()),
            ));
        }
    }

    // validate that accepted child names only exist once in the tree
    for &child_name in accepted_childs {
        let mut instances: Vec<_> = children
            .nodes()
            .iter()
            .filter(|child| child.name().value() == child_name)
            .collect();

        if instances.len() > 1 {
            let first_instance = instances.remove(0);

            return Err(ParseError::DuplicateChildNode(
                child_name.to_owned(),
                first_instance.name().span(),
                instances
                    .into_iter()
                    .map(|node| node.name().span())
                    .collect(),
            ));
        }
    }

    Ok(())
}

/// Ensure that there are no unexpected `@` nodes for nodes that are intermixed with user-defined node names.
fn expect_child_at_names(
    children: Option<&kdl::KdlDocument>,
    accepted_childs: &[&'static str],
) -> Result<(), ParseError> {
    let Some(children) = children else {
        return Ok(());
    };

    for child in children.nodes().iter() {
        if !child.name().value().starts_with("@") {
            // We're only checking for nodes that start with @'s
            continue;
        }

        if !accepted_childs.contains(&child.name().value()) {
            return Err(ParseError::UnexpectedChildNode(
                child.name().span(),
                StringList(accepted_childs.iter().map(|it| String::from(*it)).collect()),
            ));
        }
    }

    Ok(())
}

/// Parses a heading argument node
fn parse_heading(node: &kdl::KdlNode, at_arg: usize) -> Result<Option<&str>, ParseError> {
    parse_string_entry(node, at_arg)
}

/// Parses a description attribute node
fn parse_description(node: &kdl::KdlNode) -> Result<Option<&str>, ParseError> {
    parse_string_attribute(node, "description")
}

/// Parses a description attribute node
fn parse_spanned_description(
    node: &kdl::KdlNode,
) -> Result<Option<ast::Spanned<&str>>, ParseError> {
    parse_spanned_string_attribute(node, "description")
}

/// Parses a repr_type attribute node
fn parse_repr_type(node: &kdl::KdlNode) -> Result<Option<&str>, ParseError> {
    parse_string_attribute(node, "repr_type")
}

/// Ensure that there are no unexpected attributes for a certain node.
fn expect_attribute_names(
    node: &kdl::KdlNode,
    accepted_attrs: &'static [KnownAttrs],
) -> Result<(), ParseError> {
    let Some(children) = node.children() else {
        return Ok(());
    };

    for child in children
        .nodes()
        .iter()
        .filter(|it| it.name().value() == "-")
    {
        let Some(attr_name) = child.entry(0) else {
            return Err(ParseError::MissingString(child.name().span()));
        };

        let Some(attr_value) = attr_name.value().as_string() else {
            return Err(ParseError::ExpectedString(attr_name.span()));
        };

        let known_attr: KnownAttrs = attr_value
            .parse()
            .map_err(|_| ParseError::UnknownAttribute(attr_value.to_owned(), attr_name.span()))?;

        if !accepted_attrs.contains(&known_attr) {
            return Err(ParseError::UnexpectedAttribute(
                known_attr,
                attr_name.span(),
            ));
        }
    }

    Ok(())
}

/// Parses a stringy attribute node
fn parse_string_attribute<'node>(
    node: &'node kdl::KdlNode,
    attr_name: &'static str,
) -> Result<Option<&'node str>, ParseError> {
    let Some(attr_node) = find_attribute_node(node, attr_name) else {
        return Ok(None);
    };

    parse_required_string_entry(attr_node, 1).map(Some)
}

/// Parses a stringy attribute node
fn parse_spanned_string_attribute<'node>(
    node: &'node kdl::KdlNode,
    attr_name: &'static str,
) -> Result<Option<ast::Spanned<&'node str>>, ParseError> {
    let Some(attr_node) = find_attribute_node(node, attr_name) else {
        return Ok(None);
    };

    parse_required_spanned_string_entry(attr_node, 1).map(Some)
}

fn find_attribute_node<'node>(
    node: &'node kdl::KdlNode,
    attr_name: &'static str,
) -> Option<&'node kdl::KdlNode> {
    find_child_with(node, "-", |it| {
        it.entry(0)
            .filter(|key| key.name().is_none())
            .and_then(|key| key.value().as_string())
            == Some(attr_name)
    })
}

fn parse_bool_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<Option<bool>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Ok(None);
    };

    match entry.value().as_bool() {
        Some(value) => Ok(Some(value)),
        None => Err(ParseError::ExpectedBool(entry.span())),
    }
}

fn parse_spanned_bool_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<Option<ast::Spanned<bool>>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Ok(None);
    };

    match entry.value().as_bool() {
        Some(value) => Ok(Some(ast::Spanned::new(value, entry.span()))),
        None => Err(ParseError::ExpectedBool(entry.span())),
    }
}

fn parse_string_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<Option<&str>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Ok(None);
    };

    match entry.value().as_string() {
        Some(value) => Ok(Some(value)),
        None => Err(ParseError::ExpectedString(entry.span())),
    }
}

fn parse_required_string_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<&str, ParseError> {
    let key = key.into();
    let label = match &key {
        kdl::NodeKey::Key(ident) => Some(ident.value().to_owned()),
        kdl::NodeKey::Index(_) => None,
    };

    parse_string_entry(node, key)?.ok_or_else(|| match label {
        Some(label) => ParseError::MissingProperty(label, node.name().span()),
        None => ParseError::MissingString(node.name().span()),
    })
}

fn parse_spanned_string_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<Option<ast::Spanned<&str>>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Ok(None);
    };

    match entry.value().as_string() {
        Some(value) => Ok(Some(ast::Spanned::new(value, entry.span()))),
        None => Err(ParseError::ExpectedString(entry.span())),
    }
}

fn parse_required_spanned_string_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<ast::Spanned<&str>, ParseError> {
    let key = key.into();
    let label = match &key {
        kdl::NodeKey::Key(ident) => Some(ident.value().to_owned()),
        kdl::NodeKey::Index(_) => None,
    };

    parse_spanned_string_entry(node, key)?.ok_or_else(|| match label {
        Some(label) => ParseError::MissingProperty(label, node.name().span()),
        None => ParseError::MissingString(node.name().span()),
    })
}

fn parse_spanned_field_entry<'node>(
    node: &'node kdl::KdlNode,
    key: usize,
    accepted_bases: Option<&'static [&'static str]>,
) -> Result<Option<(ast::Spanned<&'node str>, ast::Spanned<&'node str>)>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Ok(None);
    };

    // ensure it has (field_base) for conventions
    let Some(field_base) = entry.ty() else {
        return Err(ParseError::MissingFieldBase(entry.span()));
    };

    let Some(field_name) = entry
        .value()
        .as_string()
        .and_then(|it| it.strip_prefix('.'))
    else {
        return Err(ParseError::InvalidFieldName(entry.span()));
    };

    if let Some(accepted_bases) = accepted_bases
        && !accepted_bases.contains(&field_base.value()) {
            return Err(ParseError::InvalidFieldBase(
                field_base.span(),
                StringList(accepted_bases.iter().map(|it| String::from(*it)).collect()),
            ));
        }

    Ok(Some((
        ast::Spanned::new(field_base.value(), field_base.span()),
        ast::Spanned::new(field_name, entry.span()),
    )))
}

fn parse_required_spanned_field_entry<'node>(
    node: &'node kdl::KdlNode,
    key: usize,
    accepted_bases: Option<&'static [&'static str]>,
) -> Result<(ast::Spanned<&'node str>, ast::Spanned<&'node str>), ParseError> {
    parse_spanned_field_entry(node, key, accepted_bases)?
        .ok_or_else(|| ParseError::MissingField(node.name().span()))
}

fn parse_required_u32_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<u32, ParseError> {
    let key = key.into();
    let label = match &key {
        kdl::NodeKey::Key(ident) => Some(ident.value().to_owned()),
        kdl::NodeKey::Index(_) => None,
    };

    parse_u32_entry(node, key)?.ok_or_else(|| match label {
        Some(label) => ParseError::MissingProperty(label, node.name().span()),
        None => ParseError::MissingInteger(node.name().span()),
    })
}

fn parse_u32_entry(
    node: &kdl::KdlNode,
    key: impl Into<kdl::NodeKey>,
) -> Result<Option<u32>, ParseError> {
    let Some(entry) = node.entry(key) else {
        return Ok(None);
    };
    let Some(value) = entry.value().as_integer() else {
        return Err(ParseError::ExpectedInteger(entry.span()));
    };
    let value: u32 = match value.try_into() {
        Ok(value) => value,
        Err(_) if value.is_negative() => todo!(),
        Err(_) => todo!(),
    };

    Ok(Some(value))
}

fn find_child<'node>(
    node: &'node kdl::KdlNode,
    child_name: &'static str,
) -> Option<&'node kdl::KdlNode> {
    node.children()?
        .nodes()
        .iter()
        .find(|it| it.name().value() == child_name)
}

fn find_child_with<'node>(
    node: &'node kdl::KdlNode,
    child_name: &'static str,
    f: impl Fn(&'node kdl::KdlNode) -> bool,
) -> Option<&'node kdl::KdlNode> {
    node.children()?
        .nodes()
        .iter()
        .find(|it| it.name().value() == child_name && f(it))
}

fn for_each_field<R>(ty: &Type, mut it: impl FnMut(&AdtField) -> ControlFlow<R>) -> Option<R> {
    match ty {
        // Enums and scalars are leaf types, no need to check them for fields.
        Type::Enum(_) | Type::Scalar(_) => {}
        Type::Struct(strukt) => {
            for field in strukt.fields() {
                match it(field) {
                    ControlFlow::Continue(_) => {}
                    ControlFlow::Break(res) => return Some(res),
                }
            }
        }
        Type::Union(union) => {
            for field in union.variants().iter().flat_map(|variant| variant.fields()) {
                match it(field) {
                    ControlFlow::Continue(_) => {}
                    ControlFlow::Break(res) => return Some(res),
                }
            }
        }
    }

    None
}

struct DefsTracker<K> {
    defs: HashMap<K, miette::SourceSpan>,
    name_kind: NameKind,
}

impl<K> DefsTracker<K> {
    fn new(name_kind: NameKind) -> Self {
        Self {
            defs: HashMap::new(),
            name_kind,
        }
    }
}

impl<K> DefsTracker<K>
where
    K: Eq + Hash,
{
    fn track_def(&mut self, def: K, def_span: miette::SourceSpan) -> Result<(), ParseError>
    where
        K: ToString,
    {
        use std::collections::hash_map::Entry;

        match self.defs.entry(def) {
            Entry::Occupied(occupied_entry) => Err(ParseError::DuplicateName(
                self.name_kind,
                occupied_entry.key().to_string(),
                *occupied_entry.get(),
                def_span,
            )),
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(def_span);
                Ok(())
            }
        }
    }
}

impl<K, Q> Index<&Q> for DefsTracker<K>
where
    K: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash + ?Sized,
{
    type Output = miette::SourceSpan;

    fn index(&self, index: &Q) -> &Self::Output {
        &self.defs[index]
    }
}

struct NameLookup<K, V> {
    names: BTreeMap<K, V>,
    name_kind: NameKind,
}

impl<K, V> NameLookup<K, V>
where
    K: Ord,
{
    fn from_names(name_kind: NameKind, names: impl IntoIterator<Item = (K, V)>) -> Self {
        Self {
            names: names.into_iter().collect(),
            name_kind,
        }
    }

    fn lookup<Q>(&self, name: ast::Spanned<Q>) -> Result<&V, ParseError>
    where
        Q: Ord + ToString,
        K: Borrow<Q> + ToString,
    {
        self.names.get(name.value().borrow()).ok_or_else(|| {
            let names = self.names.keys().map(|it| it.to_string()).collect();
            ParseError::UnknownName(
                self.name_kind,
                name.value().to_string(),
                name.span(),
                StringList(names),
            )
        })
    }
}

#[derive(Debug, Default)]
struct ConditionalDefs {
    conditions: IndexSet<(PredicateOp, PredicateValue)>,
}

impl ConditionalDefs {
    fn insert_condition(&mut self, op: PredicateOp, value: PredicateValue) -> ConditionalExprRef {
        let (slot, _) = self.conditions.insert_full((op, value));
        ConditionalExprRef::from_usize(slot)
    }

    fn get_condition(&self, op: PredicateOp, value: PredicateValue) -> Option<ConditionalExprRef> {
        let slot = self.conditions.get_index_of(&(op, value))?;
        Some(ConditionalExprRef::from_usize(slot))
    }

    fn get_single_condition(&self) -> Result<Option<ConditionalExprRef>, ()> {
        match self.conditions.len() {
            0 => Ok(None),
            1 => Ok(Some(ConditionalExprRef::from_usize(0))),
            _ => Err(()),
        }
    }
}

impl Index<ConditionalExprRef> for ConditionalDefs {
    type Output = (PredicateOp, PredicateValue);

    fn index(&self, index: ConditionalExprRef) -> &Self::Output {
        self.conditions
            .get_index(index.index())
            .expect("should be valid conditional expr ref")
    }
}

#[derive(Debug, Default)]
struct StackOperands {
    visible_when: BTreeMap<ConditionalExprRef, Vec<ConditionalEntry>>,
    otherwise: Vec<ConditionalEntry>,
    always: Vec<ConditionalEntry>,
}

impl StackOperands {
    fn pick_operands<'entries, V>(
        &self,
        entries: &'entries [ast::Spanned<ast::MaybeConditional<'entries, V>>],
        predicate: Option<ConditionalExprRef>,
    ) -> Vec<&'entries V>
    where
        V: 'entries,
    {
        let mut slots = vec![];
        let mut operands = vec![];

        let conditional_slots = match predicate {
            Some(predicate) => self.visible_when.get(&predicate),
            None => Some(&self.otherwise),
        };

        if let Some(conditional_slots) = conditional_slots {
            slots.extend_from_slice(conditional_slots);
        }
        slots.extend_from_slice(&self.always);

        // Ensure entries and operands are added in definition order
        slots.sort_by_key(|key| (key.index(), key.arm()));

        for slot in slots {
            match slot {
                ConditionalEntry::Predicate(index) => {
                    let ast::MaybeConditional::Conditional(conditional) = entries[index].value()
                    else {
                        unreachable!()
                    };

                    operands.extend(conditional.operands.iter().map(|it| it.value()));
                }
                ConditionalEntry::Case(index, arm) => {
                    let ast::MaybeConditional::ConditionalCase(conditional_case) =
                        entries[index].value()
                    else {
                        unreachable!()
                    };

                    operands.extend(
                        conditional_case.arms[arm]
                            .value()
                            .operands
                            .iter()
                            .map(|it| it.value()),
                    );
                }
                ConditionalEntry::Always(index) => {
                    let ast::MaybeConditional::Operand(operand) = entries[index].value() else {
                        unreachable!()
                    };

                    operands.push(operand);
                }
            }
        }

        operands
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ConditionalExprRef(NonZeroUsize);

impl ConditionalExprRef {
    fn from_usize(value: usize) -> Self {
        NonZeroUsize::new(value + 1)
            .map(Self)
            .expect("too many values")
    }

    fn index(self) -> usize {
        self.0.get() - 1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConditionalEntry {
    Predicate(usize),
    Case(usize, usize),
    Always(usize),
}

impl ConditionalEntry {
    fn index(self) -> usize {
        match self {
            ConditionalEntry::Predicate(index)
            | ConditionalEntry::Case(index, _)
            | ConditionalEntry::Always(index) => index,
        }
    }

    fn arm(self) -> usize {
        match self {
            ConditionalEntry::Case(_, arm) => arm,
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Property, PropertyValue};

    use super::*;

    #[track_caller]
    fn parse_pass(src: &str) -> BytecodeSpec {
        let out = parse_spec(src);
        match out {
            Ok(ok) => ok,
            Err(err) => {
                panic!("{err:#?}")
            }
        }
    }

    #[track_caller]
    fn parse_fail(src: &str) -> ParseError {
        let out = parse_spec(src);
        assert!(matches!(out, Err(_)), "{out:#?}");
        out.unwrap_err()
    }

    #[test]
    fn parse_simple_file() {
        parse_pass(
            r#"
            types {}
            instructions {}
            "#,
        );
    }

    #[test]
    fn parse_all_top_level() {
        let _ = parse_pass(
            r#"
            types {}
            exceptions {}
            instructions {}
            "#,
        );
    }

    #[test]
    fn parse_fail_top_level_unknown_child() {
        let err = parse_fail(
            r#"
            not_a_top_level_child {}
            "#,
        );

        assert!(
            matches!(err, ParseError::UnexpectedChildNode(..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_only_instructions() {
        let out = parse_pass(
            r#"
            types {}
            instructions {
                INSTR 0x0 {}
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 1);
        assert_eq!(out.instructions[0].opcode(), 0);

        let out = parse_pass(
            r#"
            types {}
            instructions {
                INSTR 0x0 "heading" { - description "blah" }
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 1);
        assert_eq!(out.instructions[0].heading(), Some("heading"));
        assert_eq!(out.instructions[0].description(), Some("blah"));

        let out = parse_pass(
            r#"
            types {}
            instructions {
                INSTR_A 0x0 "heading" {}
                INSTR_B 0x1 {}
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 2);
    }

    #[test]
    fn parse_instruction_and_group() {
        let out = parse_pass(
            r#"
            types {}
            instructions {
                INSTR_A 0x0 "heading" {}
                // empty group
                group {}
                INSTR_B 0x1 {}
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 2);

        let out = parse_pass(
            r#"
            types {}
            instructions {
                INSTR_A 0x0 {}
                group {
                    INSTR_B1 0x1 {}
                    INSTR_B2 0x2 {}
                }
                INSTR_C 0x4 {}
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 4);
        assert_eq!(out.groups.len(), 1);
        assert_eq!(out.by_groups().count(), 3);
    }

    #[test]
    fn parse_instruction_expected_children() {
        let _ = parse_pass(
            r#"
            types {}

            instructions {
                INSTR_A 0x0 {
                    - description "some description"

                    operands {}
                    stack_before {}
                    stack_after {}
                    exceptions {}
                }
            }
            "#,
        );
    }

    #[test]
    fn parse_fail_instruction_unknown_child() {
        let err = parse_fail(
            r#"
            types {}

            instructions {
                INSTR_A 0x0 {
                    not_a_child {}
                }
            }
            "#,
        );

        assert!(
            matches!(err, ParseError::UnexpectedChildNode(..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_instruction_immediate_operands() {
        let out = parse_pass(
            r#"
            types {
                scalar int4 size=4
            }
            instructions {
                INSTR_A 0x0 {
                    operands {
                        op1 "int4"
                        op2 "int4" unused=#true { - description "op2" }
                        op3 "int4" { - description "op3" }
                    }
                }
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 1);
        let instr = &out.instructions[0];
        let int4 = out.types.get("int4").expect("should have int4 type");

        assert_eq!(instr.immediate_operands().len(), 3);

        {
            let operand = &instr.immediate_operands()[0];
            assert_eq!(operand.name, "op1");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description, None);
        }

        {
            let operand = &instr.immediate_operands()[1];
            assert_eq!(operand.name, "op2");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.unused, true);
            assert_eq!(operand.description.as_deref(), Some("op2"));
        }

        {
            let operand = &instr.immediate_operands()[2];
            assert_eq!(operand.name, "op3");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description.as_deref(), Some("op3"));
        }
    }

    #[test]
    fn parse_fail_instruction_duplicate_immediate_operand() {
        let err = parse_fail(
            r#"
            types {
                scalar "int4" size=4
            }

            instructions {
                INSTR_A 0x0 {
                    operands {
                       op1 "int4" 
                       op1 "int4" 
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::DuplicateName(NameKind::ImmediateOperand, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_instruction_stack_before_operands_no_conditional_decode() {
        // TODO: computed & computed_offset
        let out = parse_pass(
            r#"
            types {
                scalar int4 size=4
            }
            instructions {
                INSTR_A 0x0 {
                    operands {}
                    stack_before {
                        op1 "int4"
                        op2 "int4" unused=#true variadic=#true { - description "op2" }
                        op3 "int4" { - description "op3" }
                    }
                }
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 1);
        let instr = &out.instructions[0];
        let int4 = out.types.get("int4").expect("should have int4 type");

        assert_eq!(instr.stack_effects[0].stack_before().len(), 3);

        {
            let operand = &instr.stack_effects[0].stack_before()[0];
            assert_eq!(operand.name, "op1");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description, None);
        }

        {
            let operand = &instr.stack_effects[0].stack_before()[1];
            assert_eq!(operand.name, "op2");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.unused, true);
            assert_eq!(operand.special.variadic, true);
            assert_eq!(operand.description.as_deref(), Some("op2"));
        }

        {
            let operand = &instr.stack_effects[0].stack_before()[2];
            assert_eq!(operand.name, "op3");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description.as_deref(), Some("op3"));
        }
    }

    #[test]
    fn parse_instruction_stack_before_operands_conditional_predicate_number() {
        // TODO: computed & computed_offset
        let out = parse_pass(
            r#"
            types {
                scalar int4 size=4
            }
            instructions {
                INSTR_A 0x0 {
                    operands {
                        thing "int4"
                    }
                    stack_before {
                        @conditional {
                            @predicate (operands).thing ">=" 4

                            op1 "int4"
                        }
                        @conditional {
                            @predicate "otherwise"

                            op1 "int4" unused=#true variadic=#true { - description "op1" }
                        }
                        op2 "int4" { - description "op2" }
                    }
                }
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 1);
        let instr = &out.instructions[0];
        let int4 = out.types.get("int4").expect("should have int4 type");

        assert_eq!(instr.stack_effects().len(), 2);
        assert_eq!(instr.stack_effects()[0].stack_before().len(), 2);
        assert_eq!(instr.stack_effects()[1].stack_before().len(), 2);

        {
            let operand = &instr.stack_effects()[0].stack_before()[0];
            assert_eq!(operand.name, "op1");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description, None);
        }

        {
            let operand = &instr.stack_effects()[1].stack_before()[0];
            assert_eq!(operand.name, "op1");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.unused, true);
            assert_eq!(operand.special.variadic, true);
            assert_eq!(operand.description.as_deref(), Some("op1"));
        }

        for effect in 0..=1 {
            let operand = &instr.stack_effects()[effect].stack_before()[1];
            assert_eq!(operand.name, "op2");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description.as_deref(), Some("op2"));
        }
    }

    #[test]
    fn parse_fail_instruction_duplicate_stack_before_operand_no_conditional_decode() {
        let err = parse_fail(
            r#"
            types {
                scalar "int4" size=4
            }

            instructions {
                INSTR_A 0x0 {
                    stack_before {
                       op1 "int4" 
                       op1 "int4" 
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::DuplicateName(NameKind::StackBeforeOperand, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_instruction_duplicate_stack_before_operand_conditional_with_always() {
        let err = parse_fail(
            r#"
            types {
                scalar "int4" size=4
            }

            instructions {
                INSTR_A 0x0 {
                    operands {
                        thing "int4"
                    }
                    stack_before {
                        @conditional {
                            @predicate (operands).thing "==" 1

                            op1 "int4"
                        }

                        op1 "int4"
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::DuplicateName(NameKind::StackBeforeOperand, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_instruction_duplicate_stack_before_operand_conditional_with_otherwise() {
        let err = parse_fail(
            r#"
            types {
                scalar "int4" size=4
            }

            instructions {
                INSTR_A 0x0 {
                    operands {
                        thing "int4"
                    }
                    stack_before {
                        @conditional {
                            @predicate "otherwise"

                            op1 "int4"
                        }

                        op1 "int4"
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::DuplicateName(NameKind::StackBeforeOperand, ..)
            ),
            "{err:#?}"
        );
    }

    // FIXME: test other conditional decode:
    // - pass: exhaustive enum (only eq predicates)
    // - pass: exhaustive union (only eq predicates)
    // - pass: conditional case union
    // - pass: conditional case enum
    // - fail: invalid predicate inequality
    // - fail: multiple scalar predicates (conditonal)
    // - fail: multiple scalar predicates (conditonal case)
    // - fail: multiple immediate operands
    // - fail: mismatched types
    // - fail: expected variant name

    #[test]
    fn parse_instruction_stack_after_operands_no_conditional_decode() {
        // TODO: preserves, computed & computed_offset
        let out = parse_pass(
            r#"
            types {
                scalar int4 size=4
            }
            instructions {
                INSTR_A 0x0 {
                    operands {}
                    stack_before {
                        op1 "int4"
                    }
                    stack_after {
                        op1 "int4"
                        op2 "int4" unused=#true { - description "op2" }
                        op3 "int4" { - description "op3" }
                    }
                }
            }
            "#,
        );

        assert_eq!(out.instructions.len(), 1);
        let instr = &out.instructions[0];
        let int4 = out.types.get("int4").expect("should have int4 type");

        assert_eq!(instr.stack_effects[0].stack_after().len(), 3);

        {
            let operand = &instr.stack_effects[0].stack_after()[0];
            assert_eq!(operand.name, "op1");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description, None);
        }

        {
            let operand = &instr.stack_effects[0].stack_after()[1];
            assert_eq!(operand.name, "op2");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.unused, true);
            assert_eq!(operand.description.as_deref(), Some("op2"));
        }

        {
            let operand = &instr.stack_effects[0].stack_after()[2];
            assert_eq!(operand.name, "op3");
            assert_eq!(operand.ty, int4);
            assert_eq!(operand.description.as_deref(), Some("op3"));
        }
    }

    #[test]
    fn parse_fail_instruction_duplicate_stack_after_operand_no_conditional_decode() {
        let err = parse_fail(
            r#"
            types {
                scalar "int4" size=4
            }

            instructions {
                INSTR_A 0x0 {
                    stack_after {
                       op1 "int4" 
                       op1 "int4" 
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::DuplicateName(NameKind::StackAfterOperand, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_instruction_duplicate_mnemonic() {
        let err = parse_fail(
            r#"
            types {}
            instructions {
                INSTR_A 0x0 {}
                INSTR_A 0x1 {}
            }
            "#,
        );

        assert!(
            matches!(&err, ParseError::DuplicateName(NameKind::Mnemonic, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_instruction_duplicate_opcode() {
        let err = parse_fail(
            r#"
            types {}
            instructions {
                INSTR_A 0x0 {}
                INSTR_B 0x0 {}
            }
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::DuplicateName(NameKind::InstructionOpcode, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_instruction_operand_unknown_type() {
        let err = parse_fail(
            r#"
            types {}
            instructions {
                INSTR_A 0x0 {
                    operands {
                        op1 "unknown"
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownTypeName(name, ..) if name == "unknown"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_type_scalar() {
        let out = parse_pass(
            r#"
            types {
                scalar "one" size=4 {
                    - description "some - description"
                    - repr_type "u8"
                }
            }
            instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);
        let scalar = out.types.types[0].as_scalar().unwrap();

        assert_eq!(scalar.name, "one");
        assert_eq!(scalar.description.as_deref(), Some("some - description"));
        assert_eq!(scalar.size, 4);
        assert_eq!(scalar.repr_type.as_deref(), Some("u8"));
    }

    #[test]
    fn parse_type_scalar_optional_fields() {
        let out = parse_pass(
            r#"
            types {
                scalar one size=2
            }
            instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);
        let scalar = out.types.types[0].as_scalar().unwrap();

        assert_eq!(scalar.name, "one");
        assert_eq!(scalar.size, 2);
    }

    #[test]
    fn parse_type_struct() {
        let out = parse_pass(
            r#"
                types {
                    struct some_struct size=8 {
                        - description "brief - description"
                        field_1 ty1
                        field_2 ty2 { - description "some - description" }
                    }

                    scalar ty1 size=4
                    scalar ty2 size=4
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 3);
        let strukt = out.types.types[0].as_struct().unwrap();

        assert_eq!(strukt.name(), "some_struct");
        assert_eq!(strukt.description(), Some("brief - description"));
        assert_eq!(strukt.size(), 8);
        assert_eq!(strukt.fields().len(), 2);

        {
            let field = &strukt.fields()[0];
            assert_eq!(field.name(), "field_1");
            assert_eq!(field.ty(), "ty1");
            assert_eq!(field.description(), None);
        }

        {
            let field = &strukt.fields()[1];
            assert_eq!(field.name(), "field_2");
            assert_eq!(field.ty(), "ty2");
            assert_eq!(field.description(), Some("some - description"));
        }
    }

    #[test]
    fn parse_type_struct_optional_fields() {
        let out = parse_pass(
            r#"
                types {
                    struct some_struct size=0 {
                    }
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);
        let strukt = out.types.types[0].as_struct().unwrap();

        assert_eq!(strukt.name(), "some_struct");
        assert_eq!(strukt.description(), None);
        assert_eq!(strukt.size(), 0);
        assert_eq!(strukt.fields().len(), 0);
    }

    #[test]
    fn parse_type_struct_nested_siblings() {
        // occurs check & existence should pass this
        let out = parse_pass(
            r#"
                types {
                    struct some_struct size=8 {
                        inner1 another_struct
                        inner2 another_struct
                    }

                    struct another_struct size=4 {
                        child int
                    }

                    scalar int size=4
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 3);

        let strukt = out.types.types[0].as_struct().unwrap();
        assert_eq!(strukt.name(), "some_struct");
        assert_eq!(strukt.description(), None);
        assert_eq!(strukt.size(), 8);
        assert_eq!(strukt.fields().len(), 2);

        {
            let field = &strukt.fields()[0];
            assert_eq!(field.name(), "inner1");
            assert_eq!(field.ty(), "another_struct");
            assert_eq!(field.description(), None);
        }

        {
            let field = &strukt.fields()[1];
            assert_eq!(field.name(), "inner2");
            assert_eq!(field.ty(), "another_struct");
            assert_eq!(field.description(), None);
        }

        let strukt = out.types.types[1].as_struct().unwrap();
        assert_eq!(strukt.name(), "another_struct");
        assert_eq!(strukt.description(), None);
        assert_eq!(strukt.size(), 4);
        assert_eq!(strukt.fields().len(), 1);

        {
            let field = &strukt.fields()[0];
            assert_eq!(field.name(), "child");
            assert_eq!(field.ty(), "int");
            assert_eq!(field.description(), None);
        }
    }

    #[test]
    fn parse_fail_unknown_struct_field_type() {
        let err = parse_fail(
            r#"
            types {
                struct something size=4 {
                    field "unknown_type"
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::UnknownTypeName(ty_name, ..) if ty_name == "unknown_type"
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_cyclic_struct_defs() {
        let err = parse_fail(
            r#"
            types {
                // root should not be included in the cycle participants
                struct root size=4 {
                    field A
                }

                struct A size=4 {
                    field B
                }

                struct B size=4 {
                    field C
                }

                struct C size=4 {
                    field A
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::CyclicType(ty_name, _, ty_chain) if ty_name == "A" && ty_chain.len() == 3
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_duplicate_struct_field_names() {
        let err = parse_fail(
            r#"
            types {
                struct some_struct size=4 {
                    field1 int
                    field1 int
                }
                scalar int size=4
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::DuplicateName(NameKind::Field, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_type_enum() {
        let out = parse_pass(
            r#"
                types {
                   enum with_variants size=4 {
                       - description "top-level"
                       - repr_type u32

                       variant_0 0 {
                           - description "yep"
                           strn "something"
                           int 1
                       }
                       variant_1 1 {
                           strn "another thing"
                           int -2
                       }
                       variant_2 2 {
                           - description "also yep"
                           strn "more thing"
                           int 2
                       }
                   } 
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);

        let eenum = out.types.types[0].as_enum().unwrap();
        assert_eq!(eenum.name(), "with_variants");
        assert_eq!(eenum.description(), Some("top-level"));
        assert_eq!(eenum.size(), 4);
        assert_eq!(eenum.repr_type(), Some("u32"));
        assert_eq!(eenum.variants().len(), 3);

        {
            let variant = &eenum.variants()[0];
            assert_eq!(variant.name(), "variant_0");
            assert_eq!(variant.ordinal(), 0);
            assert_eq!(variant.description(), Some("yep"));
            assert_eq!(variant.properties().count(), 2);
            assert_eq!(
                variant.property("strn").map(Property::value),
                Some(&PropertyValue::String(String::from("something")))
            );
            assert_eq!(
                variant.property("int").map(Property::value),
                Some(&PropertyValue::Number(1))
            );
        }

        {
            let variant = &eenum.variants()[1];
            assert_eq!(variant.name(), "variant_1");
            assert_eq!(variant.ordinal(), 1);
            assert_eq!(variant.description(), None);
            assert_eq!(variant.properties().count(), 2);
            assert_eq!(
                variant.property("strn").map(Property::value),
                Some(&PropertyValue::String(String::from("another thing")))
            );
            assert_eq!(
                variant.property("int").map(Property::value),
                Some(&PropertyValue::Number(-2))
            );
        }

        {
            let variant = &eenum.variants()[2];
            assert_eq!(variant.name(), "variant_2");
            assert_eq!(variant.ordinal(), 2);
            assert_eq!(variant.description(), Some("also yep"));
            assert_eq!(variant.properties().count(), 2);
            assert_eq!(
                variant.property("strn").map(Property::value),
                Some(&PropertyValue::String(String::from("more thing")))
            );
            assert_eq!(
                variant.property("int").map(Property::value),
                Some(&PropertyValue::Number(2))
            );
        }
    }

    #[test]
    fn parse_type_enum_optional_iota() {
        let out = parse_pass(
            r#"
                types {
                   enum implict_iota size=4 {
                       - repr_type u32

                       variant_0 0
                       variant_1
                       variant_2
                       variant_4 4
                       variant_5
                       variant_6
                   } 
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);

        let eenum = out.types.types[0].as_enum().unwrap();
        assert_eq!(eenum.variants().len(), 6);

        let expected_ordinals = &[0, 1, 2, 4, 5, 6];

        for (variant, expected) in eenum.variants().iter().zip(expected_ordinals.iter()) {
            assert_eq!(
                variant.ordinal(),
                *expected,
                "for variant {}",
                variant.name()
            );
        }
    }

    #[test]
    fn parse_fail_duplicate_enum_variant_names() {
        let err = parse_fail(
            r#"
            types {
                enum e size=4 {
                    v1
                    v1
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::DuplicateName(NameKind::Variant, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_duplicate_enum_variant_property_names() {
        let err = parse_fail(
            r#"
            types {
                enum e size=4 {
                    v1 {
                        int 1
                        int 1
                    }
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::DuplicateName(NameKind::Property, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_enum_variant_missing_properties() {
        let err = parse_fail(
            r#"
            types {
                enum e size=4 {
                    v1 {
                        a 1
                        b 1
                        c 1
                    }
                    v2 {
                        a 1
                        b 1
                    }
                    v3 {
                        a 1
                        c 1
                    }
                    v4 {
                        a 1
                        b 1
                        d 1
                    }
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::MissingVariantProperties(..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_enum_variant_mismatched_property_types() {
        let err = parse_fail(
            r#"
            types {
                enum e size=4 {
                    v1 { a 1 }
                    v1 { a "thing" }
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::MismatchedPropertyTypes(..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_type_union() {
        let out = parse_pass(
            r#"
            types {
                union "tagged_kinds" tag_size=4 {
                    - description "a - description"
                    - repr_type "u32"

                    v1 1 size=4 {
                        - description "variant 1"
                        field1 "int4" { - description "some field 1" }
                    }
                    v2 2 size=8 {
                        field2 "int4" { - description "some field 2" }
                        field3 "int4" { - description "some field 3" }
                    }
                }
                scalar "int4" size=4 {}
            }
            instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 2);

        let union = out.types.types[0].as_union().unwrap();

        assert_eq!(union.name(), "tagged_kinds");
        assert_eq!(union.description(), Some("a - description"));
        assert_eq!(union.tag_size(), 4);
        assert_eq!(union.size(), 12); // sum of the largest variant
        assert_eq!(union.repr_type(), Some("u32"));
        assert_eq!(union.variants().len(), 2);

        {
            let variant = &union.variants()[0];
            assert_eq!(variant.name(), "v1");
            assert_eq!(variant.description(), Some("variant 1"));
            assert_eq!(variant.size(), 4);
            assert_eq!(variant.ordinal(), 1);
            assert_eq!(variant.fields().len(), 1);

            {
                let field = &variant.fields()[0];
                assert_eq!(field.name(), "field1");
                assert_eq!(field.ty(), "int4");
                assert_eq!(field.description(), Some("some field 1"));
            }
        }

        {
            let variant = &union.variants()[1];
            assert_eq!(variant.name(), "v2");
            assert_eq!(variant.description(), None);
            assert_eq!(variant.size(), 8);
            assert_eq!(variant.ordinal(), 2);
            assert_eq!(variant.fields().len(), 2);

            {
                let field = &variant.fields()[0];
                assert_eq!(field.name(), "field2");
                assert_eq!(field.ty(), "int4");
                assert_eq!(field.description(), Some("some field 2"));
            }

            {
                let field = &variant.fields()[1];
                assert_eq!(field.name(), "field3");
                assert_eq!(field.ty(), "int4");
                assert_eq!(field.description(), Some("some field 3"));
            }
        }
    }

    #[test]
    fn parse_type_union_optional_fields() {
        let out = parse_pass(
            r#"
                types {
                    union "minimal" tag_size=4 {
                    }
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);
        let union = out.types.types[0].as_union().expect("must be union");

        assert_eq!(union.name(), "minimal");
        assert_eq!(union.description(), None);
        assert_eq!(union.tag_size(), 4);
        assert_eq!(union.size(), 4);
        assert_eq!(union.variants().len(), 0);
    }

    #[test]
    fn parse_type_union_variant_optional_fields() {
        let out = parse_pass(
            r#"
                types {
                    union "minimal_variant" tag_size=4 {
                        v1 size=0 {}
                    }
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);
        let union = out.types.types[0].as_union().expect("must be union");

        assert_eq!(union.name(), "minimal_variant");
        assert_eq!(union.description(), None);
        assert_eq!(union.tag_size(), 4);
        assert_eq!(union.size(), 4);
        assert_eq!(union.variants().len(), 1);

        {
            let variant = &union.variants()[0];
            assert_eq!(variant.name(), "v1");
            assert_eq!(variant.description(), None);
            assert_eq!(variant.size(), 0);
            assert_eq!(variant.ordinal(), 0);
            assert_eq!(variant.fields().len(), 0);
        }
    }

    #[test]
    fn parse_type_union_optional_iota() {
        let out = parse_pass(
            r#"
                types {
                   union implict_iota tag_size=4 {
                       variant_0 0 size=0 {}
                       variant_1 size=0 {}
                       variant_2 size=0 {}
                       variant_4 4 size=0 {}
                       variant_5 size=0 {}
                       variant_6 size=0 {}
                   } 
                }
                instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);

        let union = out.types.types[0].as_union().unwrap();
        assert_eq!(union.variants().len(), 6);

        let expected_ordinals = &[0, 1, 2, 4, 5, 6];

        for (variant, expected) in union.variants().iter().zip(expected_ordinals.iter()) {
            assert_eq!(
                variant.ordinal(),
                *expected,
                "for variant {}",
                variant.name()
            );
        }
    }

    #[test]
    fn parse_fail_duplicate_union_variant_names() {
        let err = parse_fail(
            r#"
            types {
                union e tag_size=4 {
                    v1 size=0 {}
                    v1 size=0 {}
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::DuplicateName(NameKind::Variant, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_duplicate_union_variant_field_names() {
        let err = parse_fail(
            r#"
            types {
                union some_union tag_size=4 {
                    v1 size=8 {
                        field1 int
                        field1 int
                    }
                }
                scalar int size=4
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::DuplicateName(NameKind::Field, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_union_variant_field_type() {
        let err = parse_fail(
            r#"
            types {
                union something tag_size=4 {
                    v1 size=4 {
                        field "unknown_type"
                    }
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::UnknownTypeName(ty_name, ..) if ty_name == "unknown_type"
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_cyclic_union_defs() {
        let err = parse_fail(
            r#"
            types {
                // root should not be included in the cycle participants
                union root tag_size=4 {
                    v1 size=4 {
                        field A
                    }
                }

                union A tag_size=4 {
                    v1 size=4 {
                        field B
                    }
                }

                union B tag_size=4 {
                    v1 size=4 {
                        field C
                    }
                }

                union C tag_size=4 {
                    v1 size=4 {
                        field A
                    }
                }
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::CyclicType(ty_name, _, ty_chain) if ty_name == "A" && ty_chain.len() == 3
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_cyclic_struct_defs_with_union_participant() {
        let err = parse_fail(
            r#"
            types {
                // root should not be included in the cycle participants
                struct root size=4 {
                    field A
                }

                struct A size=4 {
                    field B
                }

                union B tag_size=4 {
                    v1 size=4 {
                        field leaf
                    }
                    v2 size=8 {
                        field1 leaf
                        field2 leaf
                    }
                    v3 size=4 {
                        field C
                    }
                }

                struct C size=4 {
                    field A
                }

                scalar leaf size=4
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(
                &err,
                ParseError::CyclicType(ty_name, _, ty_chain) if ty_name == "A" && ty_chain.len() == 3
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_duplicate_type_names() {
        let err = parse_fail(
            r#"
            types {
                scalar "one" size=4 {}
                scalar "one" size=4 {}
            }
            instructions {}
            "#,
        );

        assert!(
            matches!(err, ParseError::DuplicateName(NameKind::Type, ..)),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_type_list() {
        let err = parse_fail(
            r#"
            types {
                - description "this is not allowed :>"
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Description, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_type_list() {
        let err = parse_fail(
            r#"
            types {
                - descrumptulous "typo"
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_struct() {
        let err = parse_fail(
            r#"
            types {
                struct one size=4 {
                    - description "this is allowed!"
                    - preserves "this is not :>"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_struct() {
        let err = parse_fail(
            r#"
            types {
                struct one size=4 {
                    - descrumptulous "typo"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_adt_field() {
        let err = parse_fail(
            r#"
            types {
                struct one size=4 {
                    field "other" {
                        - description "this is allowed!"
                        - preserves "this is not :>"
                    }
                }
                scalar other size=4
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_adt_field() {
        let err = parse_fail(
            r#"
            types {
                struct one size=4 {
                    field "other" {
                        - descrumptulous "typo"
                    }
                }
                scalar other size=4
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_scalar() {
        let err = parse_fail(
            r#"
            types {
                scalar one size=4 {
                    - description "this is allowed!"
                    - preserves "this is not :>"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_scalar() {
        let err = parse_fail(
            r#"
            types {
                scalar one size=4 {
                    - descrumptulous "typo"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_enum() {
        let err = parse_fail(
            r#"
            types {
                enum one size=4 {
                    - description "this is allowed!"
                    - preserves "this is not :>"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_enum() {
        let err = parse_fail(
            r#"
            types {
                enum one size=4 {
                    - descrumptulous "typo"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_enum_variant() {
        let err = parse_fail(
            r#"
            types {
                enum one size=4 {
                    v1 {
                        - description "this is allowed!"
                        - preserves "this is not :>"
                    }
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_enum_variant() {
        let err = parse_fail(
            r#"
            types {
                enum one size=4 {
                    v1 {
                        - descrumptulous "typo"
                    }
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_union() {
        let err = parse_fail(
            r#"
            types {
                union one tag_size=4 {
                    - description "this is allowed!"
                    - preserves "this is not :>"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_union() {
        let err = parse_fail(
            r#"
            types {
                union one tag_size=4 {
                    - descrumptulous "typo"
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_union_variant() {
        let err = parse_fail(
            r#"
            types {
                union one tag_size=4 {
                    v1 size=1 {
                        - description "this is allowed!"
                        - preserves "this is not :>"
                    }
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Preserves, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_instruction_list() {
        let err = parse_fail(
            r#"
            types {}

            instructions {
                - description "this is not allowed :>"
            }
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Description, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_instruction_list() {
        let err = parse_fail(
            r#"
            types {}

            instructions {
                - descrumptulous "typo"
            }
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_group() {
        let err = parse_fail(
            r#"
            types {
                scalar ty size=4
            }

            instructions {
                group {
                    - description "this is allowed!"
                    - repr_type "this is not :>"
                }
            }
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::ReprType, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_group() {
        let err = parse_fail(
            r#"
            types {
                scalar ty size=4
            }

            instructions {
                group {
                    - descrumptulous "typo"
                }
            }
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_union_variant() {
        let err = parse_fail(
            r#"
            types {
                union one tag_size=4 {
                    v1 size=1 {
                        - descrumptulous "typo"
                    }
                }
            }

            instructions {}
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_instruction() {
        let err = parse_fail(
            r#"
            types {
                scalar ty size=4
            }

            instructions {
                INST_0 0x00 {
                    - description "this is allowed!"
                    - repr_type "this is not :>"
                }
            }
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::ReprType, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_instruction() {
        let err = parse_fail(
            r#"
            types {
                scalar ty size=4
            }

            instructions {
                INST_0 0x00 {
                    - descrumptulous "typo"
                }
            }
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unexpected_attribute_operands_list() {
        let err = parse_fail(
            r#"
            types {
                scalar ty size=4
            }

            instructions {
                INST_0 0x00 {
                    operands {
                        - description "this is not allowed :>"
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(
                err,
                ParseError::UnexpectedAttribute(KnownAttrs::Description, ..)
            ),
            "{err:#?}"
        );
    }

    #[test]
    fn parse_fail_unknown_attribute_operands_list() {
        let err = parse_fail(
            r#"
            types {
                scalar ty size=4
            }

            instructions {
                INST_0 0x00 {
                    operands {
                        - descrumptulous "typo"
                    }
                }
            }
            "#,
        );

        assert!(
            matches!(&err, ParseError::UnknownAttribute(name, ..) if name == "descrumptulous"),
            "{err:#?}"
        );
    }
}
