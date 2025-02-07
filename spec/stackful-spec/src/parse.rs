use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::ControlFlow,
};

use crate::{
    entities::TypeRef, BytecodeSpec, CommonNodes, Enum, EnumVariant, Group, Instruction, NameKind,
    Operand, ParseError, Property, PropertyValue, Scalar, StringList, Struct, StructField, Type,
    Types,
};

pub(crate) fn parse_spec(text: &str) -> Result<BytecodeSpec, ParseError> {
    let kdl: kdl::KdlDocument = text.parse()?;

    let types = get_required_node(&kdl, CommonNodes::TypeList)?;
    let types = get_required_children(types, CommonNodes::TypeList)?;
    let types = parse_types(types)?;

    let instructions = get_required_node(&kdl, CommonNodes::InstructionList)?;
    let instructions = get_required_children(instructions, CommonNodes::InstructionList)?;

    let (instr_defs, group_defs) = parse_instruction_list(instructions, &types)?;

    Ok(BytecodeSpec {
        types,
        instructions: instr_defs.into_boxed_slice(),
        groups: group_defs.into_boxed_slice(),
    })
}

fn parse_instruction_list(
    instructions: &kdl::KdlDocument,
    types: &Types,
) -> Result<(Vec<Instruction>, Vec<(Group, std::ops::Range<usize>)>), ParseError> {
    let mut instr_defs = vec![];
    let mut group_defs = vec![];

    let mut mnemonic_names = HashMap::new();
    let mut opcode_nums = HashMap::new();

    let mut record_duplicates = |def: Instruction,
                                 entry: &kdl::KdlNode|
     -> Result<Instruction, ParseError> {
        let mnemonic_span = entry.name().span();
        if let Some(existing_def) = mnemonic_names.insert(def.mnemonic().to_owned(), mnemonic_span)
        {
            return Err(ParseError::DuplicateName(
                NameKind::Mnemonic,
                entry.name().value().to_owned(),
                existing_def,
                mnemonic_span,
            ));
        }

        let opcode_span = entry.entry(0).expect("should have opcode number").span();
        if let Some(existing_def) = opcode_nums.insert(def.opcode(), opcode_span) {
            return Err(ParseError::DuplicateOpcode(
                def.opcode(),
                existing_def,
                opcode_span,
            ));
        }

        Ok(def)
    };

    for entry in instructions.nodes() {
        if entry.name().value() == "group" {
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
                    "description" => {
                        // skip description node
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

    Ok((instr_defs, group_defs))
}

fn parse_types(types: &kdl::KdlDocument) -> Result<Types, ParseError> {
    let mut type_defs = vec![];
    let mut types_by_name = BTreeMap::new();

    let mut type_names = HashMap::new();
    let mut field_tys = HashMap::new();

    for entry in types.nodes() {
        let slot = TypeRef::from_usize(type_defs.len());

        let def = match entry.name().value() {
            "scalar" => Type::Scalar(parse_scalar(entry)?),
            "struct" => Type::Struct(parse_struct(entry, slot, &mut field_tys)?),
            "enum" => Type::Enum(parse_enum(entry)?),
            _ => return Err(ParseError::InvalidTypeKind(entry.name().span())),
        };

        let def_span = entry.name().span();

        if let Some(existing_def) = type_names.insert(def.name().to_owned(), def_span) {
            return Err(ParseError::DuplicateName(
                NameKind::Type,
                def.name().to_owned(),
                existing_def,
                def_span,
            ));
        }

        types_by_name.insert(def.name().to_owned(), slot);
        type_defs.push(def);
    }

    let types = Types {
        types: type_defs.into_boxed_slice(),
        by_name: types_by_name,
    };

    check_acyclic_types(&types, &type_names, &field_tys)?;
    // TODO: struct size compute validation

    Ok(types)
}

fn check_acyclic_types(
    types: &Types,
    def_spans: &HashMap<String, miette::SourceSpan>,
    field_ty_spans: &HashMap<TypeRef, HashMap<String, miette::SourceSpan>>,
) -> Result<(), ParseError> {
    let mut ty_stack = vec![];
    let mut visited_tys = HashSet::new();

    fn check_struct(
        strukt: &Struct,
        types: &Types,
        ty_stack: &mut Vec<TypeRef>,
        visited_tys: &mut HashSet<TypeRef>,
        def_spans: &HashMap<String, miette::SourceSpan>,
        field_ty_spans: &HashMap<TypeRef, HashMap<String, miette::SourceSpan>>,
    ) -> Result<(), ParseError> {
        let strukt_ref = types.get(strukt.name()).expect("self struct should exist");

        ty_stack.push(strukt_ref);
        {
            if !visited_tys.insert(strukt_ref) {
                // Type has been visited before, fails occurs-check
                let cycle_start = ty_stack
                    .iter()
                    .position(|it| *it == strukt_ref)
                    .expect("struct ref should be in the type stack");
                let participant_spans: Vec<_> = ty_stack[(cycle_start + 1)..]
                    .iter()
                    .map(|ty_ref| def_spans[types[*ty_ref].name()])
                    .collect();

                return Err(ParseError::CyclicType(
                    strukt.name().to_owned(),
                    def_spans[strukt.name()],
                    participant_spans,
                ));
            }

            let err = for_each_field(strukt, |field| {
                let Some(field_ty) = types.get(field.ty()) else {
                    // Field refers to an unknown type
                    return ControlFlow::Break(ParseError::UnknownTypeName(
                        field.ty().to_owned(),
                        field_ty_spans[&strukt_ref][field.name()],
                    ));
                };

                let strukt = match &types[field_ty] {
                    Type::Struct(strukt) => strukt,
                    // Enums and scalars are "leaf" types, no need to explore them
                    Type::Enum(_) | Type::Scalar(_) => return ControlFlow::Continue(()),
                };

                match check_struct(
                    strukt,
                    types,
                    ty_stack,
                    visited_tys,
                    def_spans,
                    field_ty_spans,
                ) {
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

    for (ty_ref, ty) in types.types.iter().enumerate() {
        let Some(strukt) = ty.as_struct() else {
            continue;
        };
        let ty_ref = TypeRef::from_usize(ty_ref);

        if !visited_tys.contains(&ty_ref) {
            check_struct(
                strukt,
                types,
                &mut ty_stack,
                &mut visited_tys,
                def_spans,
                field_ty_spans,
            )?;
        }
    }

    Ok(())
}

fn parse_scalar(node: &kdl::KdlNode) -> Result<Scalar, ParseError> {
    let name = parse_required_string_entry(node, 0)?;
    let size = parse_required_u32_entry(node, "size")?;
    let description = parse_description(node)?;
    let repr_type = parse_string_child(node, "repr_type")?;

    Ok(Scalar {
        name: name.to_owned(),
        description: description.map(String::from),
        size,
        repr_type: repr_type.map(String::from),
    })
}

fn parse_struct(
    node: &kdl::KdlNode,
    slot: TypeRef,
    field_tys: &mut HashMap<TypeRef, HashMap<String, miette::SourceSpan>>,
) -> Result<Struct, ParseError> {
    let name = parse_required_string_entry(node, 0)?;
    let size = parse_required_u32_entry(node, "size")?;
    let description = parse_description(node)?;

    let fields = get_required_children(node, CommonNodes::StructNode)?;
    let mut field_defs = vec![];
    let mut field_names = HashMap::new();
    let mut field_ty_spans = HashMap::new();

    for field_entry in fields.nodes() {
        let name = field_entry.name().value();

        // Skip nodes with special names
        if matches!(name, "description") {
            continue;
        }

        let ty = parse_required_string_entry(field_entry, 0)?;
        let description = parse_description(field_entry)?;

        field_ty_spans.insert(
            name.to_owned(),
            field_entry.entry(0).expect("field should have type").span(),
        );

        let def_span = field_entry.name().span();
        if let Some(existing_field) = field_names.insert(name.to_owned(), def_span) {
            return Err(ParseError::DuplicateName(
                NameKind::Field,
                name.to_owned(),
                existing_field,
                def_span,
            ));
        }

        field_defs.push(StructField {
            name: name.to_owned(),
            ty: ty.to_owned(),
            description: description.map(String::from),
        });
    }

    field_tys.insert(slot, field_ty_spans);

    Ok(Struct {
        name: name.to_owned(),
        description: description.map(String::from),
        size,
        fields: field_defs.into_boxed_slice(),
    })
}

fn parse_enum(node: &kdl::KdlNode) -> Result<Enum, ParseError> {
    let name = parse_required_string_entry(node, 0)?;
    let size = parse_required_u32_entry(node, "size")?;
    let description = parse_description(node)?;
    let repr_type = parse_string_child(node, "repr_type")?;

    let variants = get_required_children(node, CommonNodes::EnumNode)?;
    let mut variant_names = HashMap::new();
    let mut variant_defs = vec![];
    let mut ordinal_iota = 0;
    let mut property_types = HashMap::new();

    for variant_entry in variants.nodes() {
        let name = variant_entry.name().value();

        // Skip nodes with special names
        if matches!(name, "description" | "repr_type") {
            continue;
        }

        let ordinal = parse_u32_entry(variant_entry, 0)?.unwrap_or(ordinal_iota);
        let description = parse_description(variant_entry)?;
        let mut properties = BTreeMap::new();

        if let Some(variant_properties) = variant_entry.children().map(|it| it.nodes()) {
            let mut property_names = HashMap::new();

            for property_entry in variant_properties {
                let name = property_entry.name().value();

                // Skip nodes with special names
                if matches!(name, "description") {
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
                        return Err(ParseError::InvalidValue(value.span()))
                    }
                };

                // Ensure property type is consistent
                if let Some(expected_ty) = property_types.insert(name.to_owned(), value.kind()) {
                    if expected_ty != value.kind() {
                        return Err(ParseError::MismatchedPropertyTypes(
                            value_span,
                            expected_ty,
                            value.kind(),
                        ));
                    }
                }

                properties.insert(
                    name.to_owned(),
                    Property {
                        name: name.to_owned(),
                        value,
                    },
                );

                let def_span = property_entry.name().span();
                if let Some(existing_field) = property_names.insert(name.to_owned(), def_span) {
                    return Err(ParseError::DuplicateName(
                        NameKind::Property,
                        name.to_owned(),
                        existing_field,
                        def_span,
                    ));
                }
            }
        }

        variant_defs.push(EnumVariant {
            name: name.to_owned(),
            ordinal,
            description: description.map(String::from),
            properties,
        });

        let def_span = variant_entry.name().span();
        if let Some(existing_variant) = variant_names.insert(name.to_owned(), def_span) {
            return Err(ParseError::DuplicateName(
                NameKind::Variant,
                name.to_owned(),
                existing_variant,
                def_span,
            ));
        }

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

    Ok(Enum {
        name: name.to_owned(),
        description: description.map(String::from),
        size,
        repr_type: repr_type.map(String::from),
        variants: variant_defs.into_boxed_slice(),
    })
}

fn parse_instruction(node: &kdl::KdlNode, types: &Types) -> Result<Instruction, ParseError> {
    let opcode = parse_required_u32_entry(node, 0)?;
    let heading = parse_heading(node, 1)?;
    let description = parse_description(node)?;

    let immediate_operands = if let Some(operands) = find_child(node, "operands") {
        let operands = get_required_children(operands, CommonNodes::OperandsList)?;
        let mut operand_defs = vec![];
        let mut operand_names = HashMap::new();

        for entry in operands.nodes() {
            let def = parse_operand(entry, types)?;

            let def_span = entry.name().span();

            if let Some(existing_name) = operand_names.insert(def.name.to_owned(), def_span) {
                return Err(ParseError::DuplicateName(
                    NameKind::Operand,
                    def.name.to_owned(),
                    existing_name,
                    def_span,
                ));
            }

            operand_defs.push(def);
        }

        operand_defs
    } else {
        // No explicit immediate operands
        vec![]
    };

    // TODO: parse stack operands and conditional decodes

    Ok(Instruction {
        mnemonic: node.name().value().to_owned(),
        opcode,
        heading: heading.map(String::from),
        description: description.map(String::from),
        immediate_operands: immediate_operands.into_boxed_slice(),
        stack_before_operands: Box::new([]),
        stack_after_operands: Box::new([]),
        conditional_decodes: None,
    })
}

fn parse_operand(node: &kdl::KdlNode, types: &Types) -> Result<Operand, ParseError> {
    let name = node.name().value();
    let ty = parse_required_string_entry(node, 0)?;
    let Some(ty) = types.get(ty) else {
        let ty_span = node.entry(0).expect("must have type entry");
        return Err(ParseError::UnknownTypeName(ty.to_owned(), ty_span.span()));
    };
    let description = parse_description(node)?;
    let unused = node
        .entry("unused")
        .and_then(|it| it.value().as_bool()) // expect bool
        .unwrap_or(false);

    // other things!
    Ok(Operand {
        name: name.to_owned(),
        ty,
        description: description.map(String::from),
        unused,
        variadic: false,
        preserves: None,
        computed: None,
    })
}

fn get_required_node(
    children: &kdl::KdlDocument,
    node_type: CommonNodes,
) -> Result<&kdl::KdlNode, ParseError> {
    match children.get(&node_type.node_name()) {
        Some(it) => Ok(it),
        None => Err(ParseError::NodeRequired(
            crate::CommonNodes::TypeList,
            children.span(),
        )),
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

/// Parses a heading argument node
fn parse_heading(node: &kdl::KdlNode, at_arg: usize) -> Result<Option<&str>, ParseError> {
    parse_string_entry(node, at_arg)
}

/// Parses a child description node
fn parse_description(node: &kdl::KdlNode) -> Result<Option<&str>, ParseError> {
    parse_string_child(node, "description")
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

/// Parses a child description node with a string value
fn parse_string_child<'node>(
    node: &'node kdl::KdlNode,
    child_name: &'static str,
) -> Result<Option<&'node str>, ParseError> {
    let Some(child_node) = find_child(node, child_name) else {
        return Ok(None);
    };

    Some(parse_required_string_entry(child_node, 0)).transpose()
}

fn for_each_field<R>(
    strukt: &Struct,
    mut it: impl FnMut(&StructField) -> ControlFlow<R>,
) -> Option<R> {
    for field in strukt.fields() {
        match it(field) {
            ControlFlow::Continue(_) => {}
            ControlFlow::Break(res) => return Some(res),
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::{Property, PropertyValue};

    use super::*;

    #[track_caller]
    fn parse_pass(src: &str) -> BytecodeSpec {
        let out = parse_spec(src);
        assert!(matches!(out, Ok(_)), "{out:#?}");
        out.unwrap()
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
                INSTR 0x0 "heading" { description "blah" }
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
    fn parse_instruction_operands() {
        let out = parse_pass(
            r#"
            types {
                scalar int4 size=4
            }
            instructions {
                INSTR_A 0x0 {
                    operands {
                        op1 "int4"
                        op2 "int4" unused=#true { description "op2" }
                        op3 "int4" { description "op3" }
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
            matches!(&err, ParseError::DuplicateOpcode(0, ..)),
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
                    description "some description"
                    repr_type "u8"
                }
            }
            instructions {}
            "#,
        );

        assert_eq!(out.types.types.len(), 1);
        let scalar = out.types.types[0].as_scalar().unwrap();

        assert_eq!(scalar.name, "one");
        assert_eq!(scalar.description.as_deref(), Some("some description"));
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
                        description "brief description"
                        field_1 ty1
                        field_2 ty2 { description "some description" }
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
        assert_eq!(strukt.description(), Some("brief description"));
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
            assert_eq!(field.description(), Some("some description"));
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
    fn parse_type_struct_nested() {
        // occurs check & existence should pass this
        let out = parse_pass(
            r#"
                types {
                    struct some_struct size=4 {
                        inner another_struct
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
        assert_eq!(strukt.size(), 4);
        assert_eq!(strukt.fields().len(), 1);

        {
            let field = &strukt.fields()[0];
            assert_eq!(field.name(), "inner");
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
    fn parse_fail_duplicate_field_names() {
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
    fn parse_enum() {
        let out = parse_pass(
            r#"
                types {
                   enum with_variants size=4 {
                       description "top-level"
                       repr_type u32

                       variant_0 0 {
                           description "yep"
                           strn "something"
                           int 1
                       }
                       variant_1 1 {
                           strn "another thing"
                           int -2
                       }
                       variant_2 2 {
                           description "also yep"
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
    fn parse_enum_optional_iota() {
        let out = parse_pass(
            r#"
                types {
                   enum implict_iota size=4 {
                       repr_type u32

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
}
