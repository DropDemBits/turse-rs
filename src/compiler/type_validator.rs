//! Type validator upon the AST tree
use crate::compiler::ast::{ASTVisitorMut, Expr, Stmt};
use crate::compiler::block::{CodeBlock, CodeUnit};
use crate::compiler::token::TokenType;
use crate::compiler::types::{self, PrimitiveType, TypeRef, TypeTable};
use crate::status_reporter::StatusReporter;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct TypeValidator {
    /// Status reporter for the type validator
    reporter: StatusReporter,
    type_table: Weak<RefCell<TypeTable>>,
    active_scope: Weak<RefCell<CodeBlock>>,
}

impl TypeValidator {
    pub fn new(validate_unit: &Rc<RefCell<CodeUnit>>, type_table: &Rc<RefCell<TypeTable>>) -> Self {
        Self {
            reporter: StatusReporter::new(),
            type_table: Rc::downgrade(type_table),
            active_scope: Rc::downgrade(&validate_unit.borrow().blocks()[0]),
        }
    }
}

impl ASTVisitorMut<()> for TypeValidator {
    fn visit_stmt(&mut self, visit_stmt: &mut Stmt) {
        match visit_stmt {
            Stmt::VarDecl {
                idents,
                type_spec,
                value,
                ..
            } => {
                if types::is_error(type_spec) {
                    // Nothing to do, the identifiers should all have matching values
                    debug_assert!(
                        idents
                            .iter()
                            .filter(|ident| !types::is_error(&ident.type_spec))
                            .count()
                            == 0
                    );
                    return;
                } else if *type_spec == TypeRef::Unknown {
                    // Unknown type, use the type of the expr
                    // Safe to unwrap as if no expr was provided, the type_spec would be TypeError
                    let expr = value.as_mut().unwrap();
                    self.visit_expr(expr);

                    *type_spec = expr.get_eval_type();
                } else if value.is_some() {
                    let asn_type = value.as_ref().unwrap().get_eval_type();

                    if asn_type != TypeRef::TypeError {
                        // Validate that the types are assignable
                        let type_table = self.type_table.upgrade().unwrap();
                        if !types::is_assignable_to(type_spec, &asn_type, &type_table.borrow()) {
                            // Value to assign is the wrong type
                            self.reporter.report_error(
                                &idents.last().as_ref().unwrap().token.location,
                                format_args!("Assignment value is the wrong type"),
                            );
                            *type_spec = TypeRef::TypeError;
                        }
                    } else {
                        // Silently propogate type error (error has been reported)
                        *type_spec = TypeRef::TypeError;
                    }
                }
                // Variable declarations with no assignment value will have the type already given

                // Update the identifiers to the new assignment value
                for ident in idents.iter_mut() {
                    ident.type_spec = *type_spec;
                    self.active_scope
                        .upgrade()
                        .unwrap()
                        .borrow_mut()
                        .scope
                        .resolve_ident(&ident.name, *type_spec, ident.is_const, ident.is_typedef);
                }
            }
            Stmt::Assign {
                var_ref,
                op: _,
                value,
            } => {
                self.visit_expr(var_ref);
                self.visit_expr(value);

                // TODO Validate that the types are assignable for the given operation
            }
            Stmt::ProcedureCall { proc_ref } => self.visit_expr(proc_ref),
            Stmt::Block { block } => {
                // Change our active scope
                for stmt in block.borrow_mut().stmts.iter_mut() {
                    self.visit_stmt(stmt);
                }
                // Revert to previous scope
            }
        }
    }

    // Note: If the eval_type is still TypeRef::Unknown, propagate the type error
    fn visit_expr(&mut self, visit_expr: &mut Expr) {
        match visit_expr {
            Expr::Grouping { expr, eval_type } => {
                self.visit_expr(expr);
                *eval_type = expr.get_eval_type();
            }
            Expr::BinaryOp {
                left,
                op,
                right,
                eval_type,
            } => {
                self.visit_expr(left);
                self.visit_expr(right);

                // Validate that the types are assignable with the given operation
                // eval_type is the type of the expr result

                let loc = &op.location;
                let op = &op.token_type;

                // TODO: Resolve & De-alias type refs
                let type_table = self.type_table.upgrade().unwrap();
                let left_type = &left.get_eval_type();
                let right_type = &right.get_eval_type();

                if left_type == &TypeRef::TypeError || right_type == &TypeRef::TypeError {
                    // Either one is a type error
                    // Set default type & return (no need to report an error as this is just propoagtion)
                    *eval_type = binary_default(op);
                    return;
                }

                // TODO: (xor, shl, shr) -> (nat, TypeError), (>, >=, <, <=, =, ~=, in, ~in, =>) -> boolean, (and, or) -> (nat, boolean)

                match op {
                    TokenType::Plus => {
                        // Valid conditions:
                        // - Both types are strings
                        // - Both types are numerics (real, int, nat, etc)
                        // - Both types are sets (not checked right now)
                        // Otherwise, TypeError is produced
                        if types::is_char_seq_type(left_type) && types::is_char_seq_type(right_type)
                        {
                            // String expr, concatenation
                            *eval_type = TypeRef::Primitive(PrimitiveType::String_);
                        } else if types::is_number_type(left_type)
                            && types::is_number_type(right_type)
                        {
                            // Number expr, sum
                            *eval_type =
                                *types::common_type(left_type, right_type, &type_table.borrow())
                                    .unwrap();
                        } else {
                            // error, report!
                            self.reporter.report_error(
                                loc, 
                                format_args!(
                                    "Operands of '{}' must both be scalars (int, real, or nat), strings, or compatible sets",
                                    op
                                ),
                            );
                            *eval_type = binary_default(op);
                        }
                    }
                    TokenType::Minus | TokenType::Star => {
                        // Valid conditions:
                        // - Both types are numerics (real, int, nat, etc)
                        // - Both types are sets (not checked right now)
                        // Otherwise, TypeError is produced
                        if types::is_number_type(left_type) && types::is_number_type(right_type) {
                            // Number expr, minus & mul
                            *eval_type =
                                *types::common_type(left_type, right_type, &type_table.borrow())
                                    .unwrap();
                        } else {
                            // error, report!
                            self.reporter.report_error(
                                loc,
                                format_args!(
                                    "Operands of '{}' must both be scalars (int, real, or nat), or compatible sets",
                                    op
                                ),
                            );
                            *eval_type = binary_default(op);
                        }
                    }
                    TokenType::Slash => {
                        // Valid conditions:
                        // - Both types are numerics (real, int, nat, etc)
                        // Otherwise, TypeError is produced
                        if types::is_number_type(left_type) && types::is_number_type(right_type) {
                            // Number expr, real div
                            *eval_type = TypeRef::Primitive(PrimitiveType::Real);
                        } else {
                            // error, report!
                            self.reporter.report_error(
                                loc,
                                format_args!(
                                    "Operands of '{}' must both be scalars (int, real, or nat)",
                                    op
                                ),
                            );
                            *eval_type = binary_default(op);
                        }
                    }
                    TokenType::Div => {
                        // Valid conditions:
                        // - Both types are numerics (real, int, nat, etc)
                        // Otherwise, TypeError is produced
                        if types::is_number_type(left_type) && types::is_number_type(right_type) {
                            // Number expr, int div
                            *eval_type = TypeRef::Primitive(PrimitiveType::Int);
                        } else {
                            // error, report!
                            self.reporter.report_error(
                                loc,
                                format_args!(
                                    "Operands of '{}' must both be scalars (int, real, or nat)",
                                    op
                                ),
                            );
                            *eval_type = binary_default(op);
                        }
                    }
                    TokenType::Mod | TokenType::Rem | TokenType::Exp => {
                        // Valid conditions:
                        // - Both types are numerics (real, int, nat, etc)
                        // Otherwise, TypeError is produced
                        if types::is_number_type(left_type) && types::is_number_type(right_type) {
                            // Number expr, mod, rem & exp
                            *eval_type =
                                *types::common_type(left_type, right_type, &type_table.borrow())
                                    .unwrap();
                        } else {
                            // error, report!
                            self.reporter.report_error(
                                loc,
                                format_args!(
                                    "Operands of '{}' must both be scalars (int, real, or nat)",
                                    op
                                ),
                            );
                            *eval_type = binary_default(op);
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Expr::UnaryOp {
                op: _,
                right,
                eval_type,
            } => {
                self.visit_expr(right);

                // Validate that the unary operator can be applied to the rhs
                // eval_type is the usually same type as the rhs
                *eval_type = right.get_eval_type();
            }
            Expr::Call {
                left,
                op: _,
                arg_list,
                eval_type: _,
            } => {
                self.visit_expr(left);
                arg_list.iter_mut().for_each(|expr| self.visit_expr(expr));

                // Validate that 'left' is "callable"
                // Validate that the argument types match the type_spec using the given reference
                // eval_type is the result of the call expr
            }
            Expr::Dot {
                left,
                field: _,
                eval_type: _,
            } => {
                self.visit_expr(left);

                // Validate that the field exists in the given type
                // eval_type is the field type
            }
            Expr::Reference { ident } => {
                if ident.is_declared {
                    // Should either have a valid type, or TypeRef::Unknown
                    assert_ne!(ident.type_spec, TypeRef::TypeError);

                    // Fetch the updated ident's `type_spec` from the type table

                    // tall boi
                    let new_ident = self
                        .active_scope
                        .upgrade()
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .scope
                        .get_ident(&ident.name)
                        .unwrap()
                        .clone();
                    *ident = new_ident.clone();
                } else {
                    // Not declared, don't touch the `type_spec` (preserves error checking "correctness")
                }
            }
            Expr::Literal { .. } => {} // Literal values already have the type resolved
        }
    }
}

/// Default type in a binary expression
fn binary_default(op: &TokenType) -> TypeRef {
    match op {
        TokenType::Less
        | TokenType::Greater
        | TokenType::LessEqu
        | TokenType::GreaterEqu
        | TokenType::Equ
        | TokenType::NotEq
        | TokenType::In
        | TokenType::NotIn
        | TokenType::And
        | TokenType::Or
        | TokenType::Imply => TypeRef::Primitive(PrimitiveType::Boolean),
        _ => TypeRef::TypeError,
    }
}
