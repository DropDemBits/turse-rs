//! Type validator upon the AST tree
use crate::compiler::ast::{ASTVisitor, Expr, Stmt};
use crate::compiler::types::TypeRef;
use crate::status_reporter::StatusReporter;

pub struct TypeValidator {
    /// Status reporter for the type validator
    reporter: StatusReporter,
}

impl TypeValidator {
    // TODO: Pass in a reference to the resolved type table
    pub fn new() -> Self {
        Self {
            reporter: StatusReporter::new(),
        }
    }
}

impl ASTVisitor for TypeValidator {
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::VarDecl {
                idents,
                type_spec,
                value,
                ..
            } => {
                if *type_spec == TypeRef::TypeError {
                    // Nothing to do, all the correct types
                    return;
                } else if *type_spec == TypeRef::Unknown {
                    // Unknown type, use the type of the expr
                    // Safe to unwrap as if no expr was provided, the type_spec would be TypeError
                    let expr = value.as_mut().unwrap();
                    self.visit_expr(expr);

                    *type_spec = expr.get_eval_type();
                } else {
                    // TODO: Validate that the types are assignable
                }

                // Update the types
                for ident in idents.iter_mut() {
                    ident.type_spec = *type_spec;
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

    // Note: If the eval_type is still TypeRef::Unknown, propagate the known type??
    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Grouping { expr, eval_type } => {
                self.visit_expr(expr);
                *eval_type = expr.get_eval_type();
            }
            Expr::BinaryOp {
                left,
                op: _,
                right,
                eval_type: _,
            } => {
                self.visit_expr(left);
                self.visit_expr(right);

                // Validate that the types are assignable with the given operation
                // eval_type is the type of the expr result
            }
            Expr::UnaryOp {
                op: _,
                right,
                eval_type,
            } => {
                self.visit_expr(right);

                // Validate that the unary operator can be applied to the rhs
                // Validate that the types are assignable with the given operation
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
                // Validate that the types are assignable using the given reference
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
            Expr::Reference { ident: _ } => {
                // Fetch the type_spec from the type table
            }
            Expr::Literal { .. } => {} // Literal values already have the type resolved
        }
    }
}
