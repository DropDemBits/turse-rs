//! Builds an IR Control-Flow-Graph from a given CodeUnit
use crate::compiler::ast;
use crate::compiler::block::{BlockKind, CodeUnit};
use crate::compiler::ir::*;
use crate::compiler::types;
use crate::compiler::value;
use crate::compiler::Operator;
use petgraph::stable_graph::StableDiGraph;

// From somewhere else
#[derive(Debug)]
pub struct IrGraph {
    blocks: StableDiGraph<Block, (), BlockIndexType>,
}

impl IrGraph {
    /// Creates a new IR graph
    pub fn new() -> Self {
        Self {
            blocks: StableDiGraph::new(),
        }
    }

    /// Creates a new block, returning the block index
    pub fn create_block(&mut self) -> BlockIndex {
        self.blocks.add_node(Block::new())
    }
}

pub struct IrBuilder {
    /// The code unit to generate IR for
    unit: CodeUnit,
}

impl IrBuilder {
    /// Creates a new IrBuilder, with the CodeUnit to generate from
    pub fn new(unit: CodeUnit) -> Self {
        Self { unit }
    }

    /// Generates IR for the given IR, returning the IR representation
    pub fn generate_ir(&self) -> Option<IrGraph> {
        let mut ir_visitor = IrVisitor::new(self.unit.root_block().borrow().block_kind);
        self.unit.visit_ast(&mut ir_visitor);
        let graph = ir_visitor.take_graph();
        Some(graph)
    }
}

struct IrVisitor {
    /// The generated IR graph
    graph: Option<IrGraph>,
    /// Next temporary identifier
    next_temporary: usize,
    /// Block Context
    block_context: BlockKind,
    /// Current block being inserted into
    insert_block: BlockIndex,
    /// Current reference scope
    reference_scope: ReferenceNode,
}

impl IrVisitor {
    /// Creates a new IR visitor.
    ///
    /// `block_context`: The starting context for the block
    pub fn new(block_context: BlockKind) -> Self {
        let mut graph = IrGraph::new();
        let root_block = graph.create_block();

        Self {
            graph: Some(graph),
            next_temporary: 0,
            block_context,
            insert_block: root_block,
            reference_scope: ReferenceNode::new(),
        }
    }

    /// Takes the generated IR graph from the visitor
    pub fn take_graph(&mut self) -> IrGraph {
        self.graph.take().unwrap()
    }

    /// Checks if variable allocations need to be made to the global space
    fn in_global_context(&self) -> bool {
        matches!(self.block_context, BlockKind::Main)
    }

    /// Inserts an instruction into the current block,
    /// returning the insertion index for future modification.
    fn insert_instruction(&mut self, inst: Instruction) -> usize {
        let insert_block = &mut self.graph.as_mut().unwrap().blocks[self.insert_block];
        insert_block.insert_instruction(inst)
    }

    /// Creates a new reference to an identifier, for use in assignments
    fn make_assign_ref(
        &mut self,
        name: &str,
        type_ref: &types::TypeRef,
        alloc_space: AddressSpace,
    ) -> Reference {
        self.reference_scope.assign_ref(name, type_ref, alloc_space)

        // TODO: Keep track of modified references inside of a block
    }

    /// Uses an existsing reference from any reference scope
    fn make_use_ref(&self, ident: &ast::Identifier) -> Reference {
        self.reference_scope.use_ref(&ident.name)
    }

    /// Makes a new temporary reference with the given `type_ref`
    fn make_temporary(&mut self, type_ref: types::TypeRef) -> Reference {
        let temp_name = format!("$t{}", self.next_temporary);
        self.next_temporary += 1;

        Reference {
            name: temp_name,
            generation: 0,
            type_ref,
            address_space: AddressSpace::Local, // Temporaries always live on the stack
        }
    }
}

impl ast::Visitor<(), Reference> for IrVisitor {
    fn visit_stmt(&mut self, stmt: &ast::Stmt) -> () {
        match stmt {
            ast::Stmt::VarDecl { idents, value, .. } => {
                // ???: Handle constant variables?
                // Const Variables are just single assignment vars, don't see a need to handle them

                // Target IR (var, global, asn value):
                // $t0 := value
                // [ident] := alloc_global [type_spec]
                // store [ident] $t0
                let value_ref = if let Some(expr) = value {
                    Some(self.visit_expr(&expr))
                } else {
                    None
                };

                // For now, always allocate space in the locals area
                let alloc_space = AddressSpace::Local;

                // Build assignments for each identifier
                idents.iter().for_each(|ident| {
                    // TODO: If something is a global var, allocate space for it
                    let ident_ref =
                        self.make_assign_ref(&ident.name, &ident.type_spec, alloc_space);

                    // If there is an assignment value, assign it to everyone
                    if let Some(ref value_ref) = value_ref {
                        // TODO: Decide whether to generate stores or moves, based on address space
                        let asn_inst = Instruction::new(
                            &ident.token.location,
                            InstructionOp::Move {
                                dest: ident_ref,
                                src: value_ref.clone(),
                            },
                        );

                        self.insert_instruction(asn_inst);
                    }
                });
            }
            ast::Stmt::Assign { var_ref, op, value } => {
                let asn_ref = self.visit_expr(var_ref);
                let value_ref = self.visit_expr(value);
                let asn_op = Operator::from_binary(&op.token_type);

                let value_ref = if !matches!(asn_op, Operator::NoOp) {
                    // Build a binary expression involving the operator
                    let new_value = self.make_temporary(asn_ref.type_ref);

                    let bin_op = Instruction::new(
                        var_ref.get_span(),
                        InstructionOp::BinaryOp {
                            dest: new_value.clone(),
                            lhs: asn_ref.clone(),
                            op: asn_op,
                            rhs: value_ref,
                        },
                    );

                    self.insert_instruction(bin_op);

                    // Give back new assignment value
                    new_value
                } else {
                    // Simple assignment, give back the value
                    value_ref
                };

                // Make an assignment reference
                let asn_ref =
                    self.make_assign_ref(&asn_ref.name, &asn_ref.type_ref, asn_ref.address_space);

                let asn_inst = Instruction::new(
                    var_ref.get_span(),
                    InstructionOp::Move {
                        dest: asn_ref,
                        src: value_ref,
                    },
                );

                self.insert_instruction(asn_inst);
            }
            _ => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: &ast::Expr) -> Reference {
        match expr {
            ast::Expr::BinaryOp {
                left,
                op,
                right,
                eval_type,
                ..
            } => {
                let left_eval = self.visit_expr(&left);
                let right_eval = self.visit_expr(&right);
                let eval_ref = self.make_temporary(*eval_type);

                // TODO: Check if any conversions are needed
                // TODO: If any are global refs, load values from locations

                let binop_inst = Instruction::new(
                    &op.location,
                    InstructionOp::BinaryOp {
                        dest: eval_ref.clone(),
                        lhs: left_eval,
                        op: Operator::from_binary(&op.token_type),
                        rhs: right_eval,
                    },
                );

                self.insert_instruction(binop_inst);

                // Give back the eval reference
                eval_ref
            }
            ast::Expr::UnaryOp {
                op,
                right,
                eval_type,
                ..
            } => {
                let right_eval = self.visit_expr(&right);
                let eval_ref = self.make_temporary(*eval_type);

                // TODO: Check if any conversions are needed
                // TODO: If `right` is global refs, load value from location

                let unop_inst = Instruction::new(
                    &op.location,
                    InstructionOp::UnaryOp {
                        dest: eval_ref.clone(),
                        op: Operator::from_unary(&op.token_type),
                        rhs: right_eval,
                    },
                );

                self.insert_instruction(unop_inst);

                // Give back the eval reference
                eval_ref
            }
            ast::Expr::Literal { value, eval_type } => {
                let eval_ref = self.make_temporary(*eval_type);

                // TODO: May want to use a constant table reference, but for now, use a Value
                let const_inst = Instruction::new(
                    &value.location,
                    InstructionOp::LoadConst {
                        dest: eval_ref.clone(),
                        constant: value::Value::from(value.token_type.clone()),
                    },
                );

                self.insert_instruction(const_inst);

                // Give back the eval reference
                eval_ref
            }
            ast::Expr::Reference { ident } => {
                // Fetch the reference
                self.make_use_ref(ident)
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::block::CodeUnit;
    use crate::compiler::frontend::parser::Parser;
    use crate::compiler::frontend::scanner::Scanner;
    use crate::compiler::frontend::validator::Validator;

    /// Generates a graph from a string source
    fn generate_graph(source: &str) -> (bool, Option<IrGraph>) {
        // Yoinked again from main.rs
        // Build the main unit
        let code_unit = CodeUnit::new(true);

        let mut scanner = Scanner::new(&source);
        assert!(scanner.scan_tokens(), "Scanner failed to scan the source");

        let mut parser = Parser::new(scanner.tokens, &source, code_unit);
        assert!(parser.parse(), "Parser failed to parse the source");

        // Take the unit back from the parser
        let mut code_unit = parser.take_unit();
        let type_table = code_unit.take_types();

        // Validate AST
        let mut validator = Validator::new(code_unit.root_block(), type_table);
        code_unit.visit_ast_mut(&mut validator);
        code_unit.put_types(validator.take_types());

        assert!(
            !validator.reporter.has_error(),
            "Validator failed to validate the AST"
        );

        // Generate IR for the given unit
        let ir_builder = IrBuilder::new(code_unit);
        ir_builder
            .generate_ir()
            .map(|g| (true, Some(g)))
            .unwrap_or((false, None))
    }
}
