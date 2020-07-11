//! Type validator upon the AST tree
use crate::compiler::ast::{ASTVisitorMut, Expr, Stmt};
use crate::compiler::block::CodeBlock;
use crate::compiler::token::TokenType;
use crate::compiler::types::{self, PrimitiveType, TypeRef, TypeTable};
use crate::status_reporter::StatusReporter;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct TypeValidator<'a> {
    /// Status reporter for the type validator
    reporter: StatusReporter,
    type_table: &'a mut TypeTable,
    active_scope: Weak<RefCell<CodeBlock>>,
}

impl<'a> TypeValidator<'a> {
    pub fn new(root_block: &Rc<RefCell<CodeBlock>>, type_table: &'a mut TypeTable) -> Self {
        Self {
            reporter: StatusReporter::new(),
            type_table,
            active_scope: Rc::downgrade(root_block),
        }
    }
}

impl ASTVisitorMut<()> for TypeValidator<'_> {
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
                    // TODO: Resolve & De-alias type refs
                    let asn_type = value.as_ref().unwrap().get_eval_type();
                    let resolved_spec = &type_spec;

                    if asn_type != TypeRef::TypeError {
                        // Validate that the types are assignable
                        if !types::is_assignable_to(resolved_spec, &asn_type, &self.type_table) {
                            // Value to assign is the wrong type
                            self.reporter.report_error(
                                &idents.last().as_ref().unwrap().token.location,
                                format_args!("Initialization value is the wrong type"),
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
                op,
                value,
            } => {
                self.visit_expr(var_ref);
                self.visit_expr(value);

                // TODO: Resolve & De-alias type refs
                let ref_type = &var_ref.get_eval_type();
                let value_type = &value.get_eval_type();

                // Validate that the types are assignable for the given operation
                if types::is_error(value_type) {
                    // Silently drop propogated TypeErrors
                } else if op.token_type == TokenType::Assign {
                    if !types::is_assignable_to(ref_type, value_type, &self.type_table) {
                        // Value to assign is the wrong type
                        self.reporter.report_error(
                            &op.location,
                            format_args!("Assignment value is the wrong type"),
                        );
                    }
                } else {
                    if check_binary_operands(ref_type, &op.token_type, value_type, &self.type_table).is_err() {
                        // Value to assign is the wrong type
                        self.reporter.report_error(
                            &op.location,
                            format_args!("Assignment value is the wrong type"),
                        );
                    }
                }
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
                let left_type = &left.get_eval_type();
                let right_type = &right.get_eval_type();

                if left_type == &TypeRef::TypeError || right_type == &TypeRef::TypeError {
                    // Either one is a type error
                    // Set default type & return (no need to report an error as this is just propoagtion)
                    *eval_type = binary_default(op);
                    return;
                }

                match check_binary_operands(left_type, op, right_type, &self.type_table) {
                    Ok(good_eval) => *eval_type = good_eval,
                    Err(bad_eval) => {
                        *eval_type = bad_eval;

                        match op {
                            TokenType::Plus => 
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat), strings, or compatible sets", op), ),
                            TokenType::Minus | TokenType::Star => 
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat), or compatible sets", op)),
                            TokenType::Slash | TokenType::Div | TokenType::Mod | TokenType::Rem | TokenType::Exp => 
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat)", op)),
                            _ => todo!(),
                        }
                    }
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

fn check_binary_operands(
    left_type: &TypeRef,
    op: &TokenType,
    right_type: &TypeRef,
    type_table: &TypeTable,
) -> Result<TypeRef, TypeRef> {
    // TODO: (xor, shl, shr) -> (nat, TypeError), (>, >=, <, <=, =, ~=, in, ~in, =>) -> boolean, (and, or) -> (nat, boolean)

    match op {
        TokenType::Plus => {
            // Valid conditions:
            // - Both types are strings
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are sets (not checked right now)
            // Otherwise, TypeError is produced
            if types::is_char_seq_type(left_type) && types::is_char_seq_type(right_type) {
                // String expr, concatenation
                return Ok(TypeRef::Primitive(PrimitiveType::String_));
            } else if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, sum
                return Ok(*types::common_type(left_type, right_type, &type_table).unwrap());
            }
        }
        TokenType::Minus | TokenType::Star => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are sets (not checked right now)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, minus & mul
                return Ok(*types::common_type(left_type, right_type, &type_table).unwrap());
            }
        }
        TokenType::Slash => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, real div
                return Ok(TypeRef::Primitive(PrimitiveType::Real));
            }
        }
        TokenType::Div => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, int div
                return Ok(TypeRef::Primitive(PrimitiveType::Int));
            }
        }
        TokenType::Mod | TokenType::Rem | TokenType::Exp => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, mod, rem & exp
                return Ok(*types::common_type(left_type, right_type, &type_table).unwrap());
            }
        }
        _ => todo!()
    }

    Err(binary_default(op))
}
