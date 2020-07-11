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
                let mut is_compile_eval = false;

                if types::is_error(type_spec) {
                    // Nothing to do, the identifiers should all have matching values
                    debug_assert_eq!(
                        idents
                            .iter()
                            .filter(|ident| !types::is_error(&ident.type_spec))
                            .count(),
                        0
                    );
                    return;
                } else if *type_spec == TypeRef::Unknown {
                    // Unknown type, use the type of the expr
                    // Safe to unwrap as if no expr was provided, the type_spec would be TypeError
                    let expr = value.as_mut().unwrap();
                    self.visit_expr(expr);

                    *type_spec = expr.get_eval_type();
                    is_compile_eval = expr.is_compile_eval();
                } else if value.is_some() {
                    let asn_type = &value.as_ref().unwrap().get_eval_type();
                    let resolved_spec = &type_spec;

                    // TODO: Resolve & De-alias type refs
                    debug_assert!(types::is_base_type(asn_type, &self.type_table));
                    debug_assert!(types::is_base_type(resolved_spec, &self.type_table));

                    if !types::is_error(asn_type) {
                        // Validate that the types are assignable
                        if !types::is_assignable_to(resolved_spec, &asn_type, &self.type_table) {
                            // Value to assign is the wrong type
                            self.reporter.report_error(
                                &idents.last().as_ref().unwrap().token.location,
                                format_args!("Initialization value is the wrong type"),
                            );
                            *type_spec = TypeRef::TypeError;
                        } else {
                            is_compile_eval = value.as_ref().unwrap().is_compile_eval();
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
                    // Only compile-time evaluable if the identifier referencences a constant
                    ident.is_compile_eval = is_compile_eval && ident.is_const;
                    self.active_scope
                        .upgrade()
                        .unwrap()
                        .borrow_mut()
                        .scope
                        .resolve_ident(&ident.name, &ident);
                }
            }
            Stmt::TypeDecl { ident: _ } => {
                // TODO: Verify that the ident refers to a valid type
            }
            Stmt::Assign {
                var_ref,
                op,
                value,
            } => {
                self.visit_expr(var_ref);
                self.visit_expr(value);

                let ref_type = &var_ref.get_eval_type();
                let value_type = &value.get_eval_type();

                // Validate that the types are assignable for the given operation
                if types::is_error(value_type) {
                    // Silently drop propogated TypeErrors
                    return;
                }

                // TODO: Resolve & De-alias type refs
                debug_assert!(types::is_base_type(ref_type, &self.type_table));
                debug_assert!(types::is_base_type(value_type, &self.type_table));
                
                if op.token_type == TokenType::Assign {
                    if !types::is_assignable_to(ref_type, value_type, &self.type_table) {
                        // Value to assign is the wrong type
                        self.reporter.report_error(
                            &op.location,
                            format_args!("Assignment value is the wrong type"),
                        );
                    }
                } else {
                    if check_binary_operands((&var_ref, ref_type), &op.token_type, (&value, value_type), &self.type_table).is_err() {
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
            Expr::Grouping { expr, eval_type, is_compile_eval } => {
                self.visit_expr(expr);
                *eval_type = expr.get_eval_type();
                *is_compile_eval = expr.is_compile_eval();
            }
            Expr::BinaryOp {
                left,
                op,
                right,
                eval_type,
                is_compile_eval,
            } => {
                self.visit_expr(left);
                self.visit_expr(right);

                // Validate that the types are assignable with the given operation
                // eval_type is the type of the expr result

                let loc = &op.location;
                let op = &op.token_type;

                let left_type = &left.get_eval_type();
                let right_type = &right.get_eval_type();

                // TODO: Resolve & De-alias type refs
                debug_assert!(types::is_base_type(left_type, &self.type_table));
                debug_assert!(types::is_base_type(right_type, &self.type_table));

                if types::is_error(left_type) || types::is_error(right_type) {
                    // Either one is a type error
                    // Set default type & return (no need to report an error as this is just propoagtion)
                    *eval_type = binary_default(op);
                    *is_compile_eval = false;
                    return;
                }

                match check_binary_operands((&left, left_type), op, (&right, right_type), &self.type_table) {
                    Ok(good_eval) => {
                        // Only evaluable if the operands are not type errors and are applicable
                        *is_compile_eval = left.is_compile_eval() && right.is_compile_eval();
                        *eval_type = good_eval
                    },
                    Err(bad_eval) => {
                        *eval_type = bad_eval;
                        *is_compile_eval = false;

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
                is_compile_eval,
            } => {
                self.visit_expr(right);

                // Validate that the unary operator can be applied to the rhs
                // eval_type is the usually same type as the rhs
                *eval_type = right.get_eval_type();

                // is_compile_eval is the usually same type as the rhs
                *is_compile_eval = right.is_compile_eval();
            }
            Expr::Call {
                left,
                op: _,
                arg_list,
                eval_type: _,
                is_compile_eval,
            } => {
                self.visit_expr(left);
                arg_list.iter_mut().for_each(|expr| self.visit_expr(expr));

                // Validate that 'left' is "callable"
                // 3 things that fall under this expression
                // - set cons
                // - array subscript
                // - fcn / proc call
                // Distinguished by the identifier type

                // Validate that the argument types match the type_spec using the given reference
                // eval_type is the result of the call expr

                // For now, call expressions default to runtime-time only
                // A call expression would be compile-time evaluable if it had no side effects,
                // but we don't check that right now
                *is_compile_eval = false;
            }
            Expr::Dot {
                left,
                field: _,
                eval_type: _,
                is_compile_eval,
            } => {
                self.visit_expr(left);

                // Validate that the field exists in the given type
                // eval_type is the field type

                // For now, dot expressions default to runtime-time only
                *is_compile_eval = false;
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

/// Check if the binary operands are valid for the given operation
/// Assumes that the left and right types are de-aliased and resolved (i.e.
/// they are the base types) \
/// `left`                  Tuple containing the left expression and the left
///                         base type
/// `op`                    The operator to check for \
/// `right`                 Tuple containing the left expression and the left
///                         base type \
/// `type_table`            Type table to resolve named types \
/// `check_compile_eval`    Whether to check for compile-time evaluability for
///                         certain operations (e.g. shl, shr)
fn check_binary_operands(
    left: (&Expr, &TypeRef),
    op: &TokenType,
    right: (&Expr, &TypeRef),
    type_table: &TypeTable,
) -> Result<TypeRef, TypeRef> {
    // TODO: (>, >=, <, <=) -> boolean, (=, ~=) -> boolean, (>, >=, <, <=, =, ~=) -> boolean, (+, *, -, in, ~in) -> (default) for sets
    // Ordering comparisons require sets, enums, and objectclass types
    // Equality comparisons require the above and full equivalence checking

    let (_, left_type) = left;
    let (_, right_type) = right;

    debug_assert!(types::is_base_type(left_type, &type_table));
    debug_assert!(types::is_base_type(right_type, &type_table));

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
        TokenType::And | TokenType::Or | TokenType::Xor => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are booleans
            // Otherwise, TypeError is produced
            if types::is_integer_type(left_type) && types::is_integer_type(right_type) {
                // Integer expr, produce nat
                return Ok(TypeRef::Primitive(PrimitiveType::Nat));
            } else if types::is_boolean(left_type) && types::is_boolean(right_type) {
                // Boolean expr, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            }
        }
        TokenType::Shl | TokenType::Shr => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_integer_type(left_type) && types::is_integer_type(right_type) {
                // Integer expr, produce nat
                return Ok(TypeRef::Primitive(PrimitiveType::Nat));
            }
        }
        TokenType::Imply => {
            // Valid conditions:
            // - Both types are booleans
            // Otherwise, TypeError is produced
            if types::is_boolean(left_type) && types::is_boolean(right_type) {
                // Boolean expr, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            }
        }
        _ => todo!()
    }

    Err(binary_default(op))
}
