//! Builds an IR Control-Flow-Graph from a given `CodeUnit`
use crate::graph::{BlockIndex, Instruction, InstructionOp, IrGraph, Reference};
use crate::{AddressSpace, ReferenceNode};
use toc_ast::ast::{self, expr, ident, stmt, stmt::BlockKind, types as ty_ast, Visitor};
use toc_ast::scope::UnitScope;
use toc_ast::types;
use toc_ast::unit::CodeUnit;
use toc_ast::value;
use toc_core::Location;

pub struct IrBuilder {
    /// The code unit to generate IR for
    unit: CodeUnit,
}

impl IrBuilder {
    /// Creates a new `IrBuilder`, with the `CodeUnit` to generate from
    #[must_use]
    pub fn new(unit: CodeUnit) -> Self {
        Self { unit }
    }

    /// Generates IR for the given IR, returning the IR representation
    pub fn generate_ir(&self) -> Option<IrGraph> {
        let block_context = BlockKind::Main;
        let unit_context = &self.unit.unit_scope;
        let mut visitor = IrVisitor::new(block_context, unit_context);

        // Prepare the visitor for building the IrGraph
        // Create the root block
        let graph = visitor.graph.as_mut().unwrap();
        let root_block = graph.create_block();
        visitor.insert_block = root_block;

        // Create the init function
        // No parent or function type
        graph.create_function("<init>", None, None, root_block);

        // Build the graph
        visitor.visit_stmt(&self.unit.root_stmt);
        let graph = visitor.take_graph();
        Some(graph)
    }
}

struct IrVisitor<'unit> {
    /// The generated IR graph
    graph: Option<IrGraph>,
    /// Next temporary identifier
    next_temporary: usize,
    /// Current insertion block context
    block_context: BlockKind,
    /// Current block being inserted into
    insert_block: BlockIndex,
    /// Current reference scope
    reference_scope: ReferenceNode,
    /// Most recent line location from the last inserted instruction
    line: u32,
    /// Most recent unit location from the last inserted instruction
    unit_id: u32,
    /// UnitScope for all identifiers
    unit_scope: &'unit UnitScope,
}

impl<'unit> IrVisitor<'unit> {
    /// Creates a new IR visitor.
    ///
    /// - `block_context`: The starting context for the block
    /// - `unit`: The unit the block is part of
    pub fn new(block_context: BlockKind, unit_scope: &'unit UnitScope) -> IrVisitor<'unit> {
        Self {
            graph: Some(IrGraph::new()),
            next_temporary: 0,
            block_context,
            insert_block: BlockIndex::end(),
            reference_scope: ReferenceNode::new(),
            line: 0,
            unit_id: 0,
            unit_scope,
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

    /// Creates a location from the last inserted instruction
    fn location_from_last(&self) -> Location {
        let mut loc = Location::new();
        loc.line = self.line as usize;
        // TODO: track unit number in location information

        loc
    }

    /// Inserts an instruction into the current block,
    /// returning the insertion index for future modification.
    fn insert_instruction(&mut self, inst: Instruction) -> usize {
        // Update the last locations
        self.line = inst.line;
        self.unit_id = inst.unit;

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
        let new_ref = self.reference_scope.assign_ref(name, type_ref, alloc_space);

        // Keep track of modified references inside of a block
        let insert_block = &mut self.graph.as_mut().unwrap().blocks[self.insert_block];
        insert_block.modified_refs.insert(name.to_string());

        new_ref
    }

    /// Uses an existsing reference from any reference scope
    fn make_use_ref(&self, ident: &ident::Identifier) -> Reference {
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

impl ast::Visitor<(), Reference, ()> for IrVisitor<'_> {
    fn visit_stmt(&mut self, stmt: &stmt::Stmt) {
        match &stmt.kind {
            stmt::StmtKind::VarDecl { idents, value, .. } => {
                if idents.is_none() {
                    // Skip over this, no use
                    return;
                }

                let idents = idents.as_ref().unwrap();

                // ???: Handle constant variables?
                // Const Variables are just single assignment vars, don't see a need to handle them

                // Target IR (var, global, asn value):
                // $t0 := value
                // [ident] := alloc_global [type_spec]
                // store [ident] $t0
                let value_ref = value.as_ref().map(|expr| self.visit_expr(expr));

                // For now, always allocate space in the locals area
                let alloc_space = AddressSpace::Local;

                // Build assignments for each identifier
                idents.iter().for_each(|ident| {
                    // TODO: If something is a global var, allocate space for it
                    let info = self.unit_scope.get_ident_info(&ident.id);
                    let ident_ref = self.make_assign_ref(&info.name, &info.type_spec, alloc_space);

                    // If there is an assignment value, assign it to everyone
                    if let Some(ref value_ref) = value_ref {
                        // TODO: Decide whether to generate stores or moves, based on address space
                        let asn_inst = Instruction::new(
                            &stmt.span,
                            InstructionOp::Move {
                                dest: ident_ref,
                                src: value_ref.clone(),
                            },
                        );

                        self.insert_instruction(asn_inst);
                    }
                });
            }
            stmt::StmtKind::Assign { var_ref, op, value } => {
                let asn_ref = self.visit_expr(var_ref);
                let value_ref = self.visit_expr(value);

                let value_ref = if let Some(op) = op {
                    // Build a binary expression involving the operator
                    let new_value = self.make_temporary(asn_ref.type_ref);

                    let bin_op = Instruction::new(
                        &stmt.span,
                        InstructionOp::BinaryOp {
                            dest: new_value.clone(),
                            lhs: asn_ref.clone(),
                            op: *op,
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
                    &stmt.span,
                    InstructionOp::Move {
                        dest: asn_ref,
                        src: value_ref,
                    },
                );

                self.insert_instruction(asn_inst);
            }
            stmt::StmtKind::Block { block, .. } => {
                // As a test, split block up into other things
                let inner_block = self.graph.as_mut().unwrap().create_block();

                // Link the last insertion block into the new block
                let br_inst = Instruction::new(
                    &self.location_from_last(),
                    InstructionOp::Branch { to: inner_block },
                );
                self.insert_instruction(br_inst);

                // Link the blocks together
                self.graph
                    .as_mut()
                    .unwrap()
                    .blocks
                    .add_edge(self.insert_block, inner_block, ());

                // Change the block we're inserting into
                self.insert_block = inner_block;

                // Insert the rest statements into the block
                for stmt in &block.stmts {
                    self.visit_stmt(stmt);
                }
            }
            _ => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: &expr::Expr) -> Reference {
        match &expr.kind {
            expr::ExprKind::BinaryOp {
                left, op, right, ..
            } => {
                let left_eval = self.visit_expr(&left);
                let right_eval = self.visit_expr(&right);
                let eval_ref = self.make_temporary(expr.eval_type);

                // TODO: Check if any conversions are needed
                // TODO: If any are global refs, load values from locations

                let binop_inst = Instruction::new(
                    &op.1,
                    InstructionOp::BinaryOp {
                        dest: eval_ref.clone(),
                        lhs: left_eval,
                        op: op.0,
                        rhs: right_eval,
                    },
                );

                self.insert_instruction(binop_inst);

                // Give back the eval reference
                eval_ref
            }
            expr::ExprKind::UnaryOp { op, right, .. } => {
                let right_eval = self.visit_expr(&right);
                let eval_ref = self.make_temporary(expr.eval_type);

                // TODO: Check if any conversions are needed
                // TODO: If `right` is global refs, load value from location

                let unop_inst = Instruction::new(
                    &op.1,
                    InstructionOp::UnaryOp {
                        dest: eval_ref.clone(),
                        op: op.0,
                        rhs: right_eval,
                    },
                );

                self.insert_instruction(unop_inst);

                // Give back the eval reference
                eval_ref
            }
            expr::ExprKind::Literal { value } => {
                let eval_ref = self.make_temporary(expr.eval_type);

                // TODO: May want to use a constant table reference, but for now, use a Value
                let const_inst = Instruction::new(
                    expr.get_span(),
                    InstructionOp::LoadConst {
                        dest: eval_ref.clone(),
                        constant: value::Value::from_literal(value.clone())
                            .unwrap_or_else(|_| value::Value::from(0_i64)),
                    },
                );

                self.insert_instruction(const_inst);

                // Give back the eval reference
                eval_ref
            }
            expr::ExprKind::Reference { ident, .. } => {
                // Fetch the reference
                let info = self.unit_scope.get_ident_info(&ident.id);
                self.make_use_ref(&info)
            }
            _ => todo!(),
        }
    }

    fn visit_type(&mut self, _ty: &ty_ast::Type) {}
}

#[cfg(test)]
mod test {
    extern crate toc_frontend;

    use super::*;
    use std::sync::Arc;
    use toc_ast::ast::VisitorMut;
    use toc_frontend::{
        context::{CompileContext, SourceMap},
        parser::Parser,
        scanner::Scanner,
        validator::Validator,
    };

    /// Generates a graph from a string source
    fn generate_graph(source: &str) -> (bool, Option<IrGraph>) {
        // Build the main unit
        let context = Arc::new(CompileContext::new(SourceMap::new()));

        let scanner = Scanner::scan_source(source);
        let mut parser = Parser::new(scanner, true, context.clone());
        assert!(parser.parse(), "Parser failed to parse the source");
        context.aggregate_messages(&mut parser);

        // Take the unit back from the parser
        let mut code_unit = parser.take_unit();

        // Validate AST
        let mut validator = Validator::new(
            &mut code_unit.unit_scope,
            &mut code_unit.type_table,
            context.clone(),
        );
        validator.visit_stmt(&mut code_unit.root_stmt);
        context.aggregate_messages(&mut validator);

        let has_errors =
            toc_core::StatusReporter::report_messages(context.messages().iter(), false);

        assert!(!has_errors, "Validator failed to validate the AST");

        // Generate IR for the given unit
        let ir_builder = IrBuilder::new(code_unit);
        ir_builder
            .generate_ir()
            .map(|g| (true, Some(g)))
            .unwrap_or((false, None))
    }
}
