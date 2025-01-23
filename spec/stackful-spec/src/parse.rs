use std::collections::BTreeMap;

use kdl::KdlDocument;

use crate::{BytecodeSpec, CommonNodes, Group, Instruction, ParseError, Types};

pub(crate) fn parse_spec(text: &str) -> Result<BytecodeSpec, ParseError> {
    let kdl: KdlDocument = text.parse()?;

    let types = get_required_node(&kdl, CommonNodes::TypeList)?;
    let instructions = get_required_node(&kdl, CommonNodes::InstructionList)?;
    let instructions = get_required_children(instructions, CommonNodes::InstructionList)?;

    let mut instr_defs = vec![];
    let mut group_defs = vec![];

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
                        instr_defs.push(parse_instruction(entry)?);
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
            instr_defs.push(parse_instruction(entry)?);
        }
    }

    Ok(BytecodeSpec {
        types: Types {
            types: Box::new([]),
            by_name: BTreeMap::new(),
        },
        instructions: instr_defs.into_boxed_slice(),
        groups: group_defs.into_boxed_slice(),
    })
}

fn parse_instruction(node: &kdl::KdlNode) -> Result<Instruction, ParseError> {
    let entry = node.entries();
    let Some(opcode) = entry.get(0) else {
        panic!("missing instruction opcode {:?}", node.span())
    };
    let Some(opcode) = opcode.value().as_integer() else {
        panic!(
            "invalid instruction opcode {:?} {:?}",
            opcode.span(),
            opcode
        );
    };

    let heading = parse_heading(node, 1)?;
    let description = parse_description(node)?;

    Ok(Instruction {
        mnemonic: node.name().value().to_owned(),
        opcode: opcode as u32,
        heading: heading.map(String::from),
        description: description.map(String::from),
        immediate_operands: Box::new([]),
        stack_before_operands: Box::new([]),
        stack_after_operands: Box::new([]),
        conditional_decodes: None,
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
        panic!("node must have children {node_type:?} {:?}", node.span());
    };

    Ok(children)
}

/// Parses a heading argument node
fn parse_heading(node: &kdl::KdlNode, at_arg: usize) -> Result<Option<&str>, ParseError> {
    let Some(heading) = node.entries().get(at_arg) else {
        return Ok(None);
    };

    match heading.value().as_string() {
        Some(value) => Ok(Some(value)),
        None => Err(ParseError::ExpectedString(heading.span())),
    }
}

/// Parses a child description node
fn parse_description(node: &kdl::KdlNode) -> Result<Option<&str>, ParseError> {
    let Some(children) = node.children() else {
        return Ok(None);
    };
    let Some(description) = children
        .nodes()
        .iter()
        .find(|it| it.name().value() == "description")
    else {
        return Ok(None);
    };
    let Some(description_text) = description.entries().get(0) else {
        return Err(ParseError::MissingString(description.name().span()));
    };

    match description_text.value().as_string() {
        Some(value) => Ok(Some(value)),
        None => Err(ParseError::ExpectedString(description.span())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // need to validate:
    // - unique mnemonics
    // - unique opcodes

    #[track_caller]
    fn parse_pass(src: &str) -> BytecodeSpec {
        let out = parse_spec(src);
        assert!(matches!(out, Ok(_)), "{out:#?}");
        out.unwrap()
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
        assert_eq!(out.by_groups().count(), 3, "expected 3 entries");
    }
}
