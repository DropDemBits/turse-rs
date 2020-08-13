//! Validator fragment, resolves all expressions
use super::Validator;

use crate::compiler::ast::{Expr, Identifier, VisitorMut};
use crate::compiler::frontend::token::{Token, TokenType};
use crate::compiler::types::{self, PrimitiveType, Type, TypeRef, TypeTable};
use crate::compiler::value::{self, Value, ValueApplyError};
use std::convert::TryFrom;

impl Validator {
    // --- Expr Resolvers --- //

    // Resolves an "init" expression
    pub(super) fn resolve_expr_init(&mut self, exprs: &mut Vec<Expr>) -> Option<Value> {
        for expr in exprs.iter_mut() {
            let value = self.visit_expr(expr);

            // Replace with folded expression
            if value.is_some() {
                let span = expr.get_span().clone();
                *expr = Expr::try_from(value.unwrap())
                    .expect("Unable to convert folded value back into an expression");
                expr.set_span(span);
            }

            if !matches!(expr, Expr::Empty) && !expr.is_compile_eval() {
                self.reporter.report_error(
                    expr.get_span(),
                    format_args!("Expression is not a compile-time expression"),
                );
            } else if super::is_type_reference(expr) {
                self.reporter.report_error(
                    expr.get_span(),
                    format_args!("Reference does not refer to a variable or constant"),
                );
            }
        }
        None
    }

    pub(super) fn resolve_expr_grouping(
        &mut self,
        expr: &mut Box<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) -> Option<Value> {
        // Visit the expr
        let eval = self.visit_expr(expr);

        // Try to replace the inner expression with the folded value
        if eval.is_some() {
            let span = expr.get_span().clone();
            *expr = Box::new(Expr::try_from(eval.clone().unwrap()).unwrap());
            expr.set_span(span);
        }

        *eval_type = expr.get_eval_type();
        *is_compile_eval = expr.is_compile_eval();

        // Propogate the folded value
        return eval;
    }

    pub(super) fn resolve_expr_binary(
        &mut self,
        left: &mut Box<Expr>,
        op: &Token,
        right: &mut Box<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) -> Option<Value> {
        let left_eval = self.visit_expr(left);
        let right_eval = self.visit_expr(right);

        // Try to replace the operands with the folded values
        if left_eval.is_some() {
            let span = left.get_span().clone();
            *left = Box::new(Expr::try_from(left_eval.unwrap()).unwrap());
            left.set_span(span);
        }
        if right_eval.is_some() {
            let span = right.get_span().clone();
            *right = Box::new(Expr::try_from(right_eval.unwrap()).unwrap());
            right.set_span(span);
        }

        // Validate that the types are assignable with the given operation
        // eval_type is the type of the expr result

        if super::is_type_reference(left) || super::is_type_reference(right) {
            // Left or right operand is a type reference, can't perform operations on them
            *eval_type = TypeRef::TypeError;
            *is_compile_eval = false;

            if super::is_type_reference(left) {
                self.reporter.report_error(
                    left.get_span(),
                    format_args!("Operand is not a variable or constant reference"),
                );
            }
            if super::is_type_reference(right) {
                self.reporter.report_error(
                    right.get_span(),
                    format_args!("Operand is not a variable or constant reference"),
                );
            }

            return None;
        }

        let loc = &op.location;
        let op = &op.token_type;

        let left_type = &self.dealias_resolve_type(left.get_eval_type());
        let right_type = &self.dealias_resolve_type(right.get_eval_type());

        if types::is_error(left_type) || types::is_error(right_type) {
            // Either one is a type error
            // Set default type & return no value (no need to report an error as this is just propoagtion)
            *eval_type = binary_default(op);
            *is_compile_eval = false;
            return None;
        }

        debug_assert!(types::is_base_type(left_type, &self.type_table));
        debug_assert!(types::is_base_type(right_type, &self.type_table));

        match check_binary_operands(left_type, op, right_type, &self.type_table) {
            Ok(good_eval) => {
                // Only evaluable if the operands are not type errors and are applicable to the current op
                *is_compile_eval = left.is_compile_eval() && right.is_compile_eval();
                *eval_type = good_eval;

                if *is_compile_eval {
                    // Try to fold the current expression
                    let lvalue = Value::from_expr(*left.clone(), &self.type_table).expect(
                        &format!("Left operand is not a compile-time value {:?}", left),
                    );
                    let rvalue = Value::from_expr(*right.clone(), &self.type_table).expect(
                        &format!("Right operand is not a compile-time value {:?}", right),
                    );

                    let result = value::apply_binary(lvalue, op, rvalue);

                    return match result {
                        Ok(v) => Some(v),
                        Err(msg) => {
                            // Report the error message!
                            match msg {
                                ValueApplyError::Overflow => self.reporter.report_error(
                                    &loc,
                                    format_args!("Overflow in compile-time expression"),
                                ),
                                ValueApplyError::InvalidOperand => match op {
                                    TokenType::Shl | TokenType::Shr => self.reporter.report_error(
                                        right.get_span(),
                                        format_args!(
                                            "Negative shift amount in compile-time '{}' expression",
                                            op
                                        ),
                                    ),
                                    _ => self.reporter.report_error(
                                        right.get_span(),
                                        format_args!("Invalid operand in compile-time expression"),
                                    ),
                                },
                                ValueApplyError::DivisionByZero => {
                                    // Recoverable
                                    self.reporter.report_warning(
                                        &loc,
                                        format_args!("Compile-time '{}' by zero", op),
                                    );
                                }
                                ValueApplyError::WrongTypes => unreachable!(), // Types are guarranteed to be compatible
                            }

                            // Remove the compile-time evaluability status
                            *is_compile_eval = false;
                            None
                        }
                    };
                } else {
                    // Can't fold the current expression
                    return None;
                }
            }
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
					TokenType::And | TokenType::Or | TokenType::Xor =>
					self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat) or booleans", op)),
					TokenType::Shl | TokenType::Shr => 
					self.reporter.report_error(loc, format_args!("Operands of '{}' must both be integers (int, or nat)", op)),
					TokenType::Less | TokenType::LessEqu | TokenType::Greater | TokenType::GreaterEqu => {
						if !types::is_equivalent_to(left_type, right_type, &self.type_table) {
							self.reporter.report_error(loc, format_args!("Operands of '{}' must be the same type", op))
						} else {
							self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat), sets, enumerations, strings, or object classes", op))
						}
					},
					TokenType::NotEqu | TokenType::Equ => {
						if !types::is_equivalent_to(left_type, right_type, &self.type_table) {
							self.reporter.report_error(loc, format_args!("Operands of '{}' must be the same type", op));
						} else {
							self.reporter.report_error(loc, format_args!("Operands of '{}' must both be booleans, scalars (int, real, or nat), sets, enumerations, strings, object classes, or pointers of equivalent types", op));
						}
					},
					TokenType::In | TokenType::NotIn => {
						if !types::is_set(right_type, &self.type_table) {
							self.reporter.report_error(loc, format_args!("Right operand of '{}' must be a set type", op));
						} else {
							self.reporter.report_error(loc, format_args!("Left operand of '{}' must be compatible with the set's index type", op));
						}
					},
					TokenType::Imply =>
					self.reporter.report_error(loc, format_args!("Operands of '{}' must both be booleans", op)),
					_ => unreachable!(),
				}

                // Produce no value
                return None;
            }
        }
    }

    pub(super) fn resolve_expr_unary(
        &mut self,
        op: &Token,
        right: &mut Box<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) -> Option<Value> {
        let right_eval = self.visit_expr(right);

        // Try to replace operand with the folded value
        if right_eval.is_some() {
            let span = right.get_span().clone();
            *right = Box::new(Expr::try_from(right_eval.unwrap()).unwrap());
            right.set_span(span);
        }

        // Validate that the unary operator can be applied to the rhs
        // eval_type is the result of the operation (usually the same
        // as rhs)

        let loc = &op.location;
        let op = &op.token_type;

        if super::is_type_reference(right) {
            // Operand is a type reference, can't perform operations on it
            *eval_type = TypeRef::TypeError;
            *is_compile_eval = false;
            self.reporter.report_error(
                right.get_span(),
                format_args!("Operand is not a variable or constant reference"),
            );
            return None;
        }

        let right_type = &self.dealias_resolve_type(right.get_eval_type());

        if types::is_error(right_type) {
            // Right operand is a type error

            // Propogate error
            *eval_type = binary_default(op);
            *is_compile_eval = false;
            return None;
        }

        debug_assert!(types::is_base_type(right_type, &self.type_table));

        match check_unary_operand(op, right_type, &self.type_table) {
            Ok(good_eval) => {
                *eval_type = good_eval;
                // Compile-time evaluability is dependend on the right operand
                *is_compile_eval = right.is_compile_eval();

                if *op == TokenType::Caret {
                    // Pointers are never compile-time evaluable
                    *is_compile_eval = false;
                }

                if *is_compile_eval {
                    // Try to fold the expression
                    let rvalue = Value::from_expr(*right.clone(), &self.type_table).expect(
                        &format!("Right operand is not a compile-time value {:?}", right),
                    );

                    let result = value::apply_unary(&op, rvalue);

                    return match result {
                        Ok(v) => Some(v),
                        Err(msg) => {
                            match msg {
                                ValueApplyError::Overflow => self.reporter.report_error(
                                    &right.get_span(),
                                    format_args!("Overflow in compile-time expression"),
                                ),
                                _ => unreachable!(), // Overflow is the only error produced, WrongTypes captured by typecheck above
                            }

                            // Revoke compile-time evaluability status
                            *is_compile_eval = false;
                            None
                        }
                    };
                }

                // Produce no value
                return None;
            }
            Err(bad_eval) => {
                *eval_type = bad_eval;
                *is_compile_eval = false;

                match op {
						TokenType::Not => self.reporter.report_error(loc, format_args!("Operand of 'not' must be an integer (int or nat) or a boolean")),
						TokenType::Plus => self.reporter.report_error(loc, format_args!("Operand of prefix '+' must be a scalar (int, real, or nat)")),
						TokenType::Minus => self.reporter.report_error(loc, format_args!("Operand of unary negation must be a scalar (int, real, or nat)")),
						TokenType::Caret => self.reporter.report_error(loc, format_args!("Operand of pointer dereference must be a pointer")),
						TokenType::Pound => self.reporter.report_error(loc, format_args!("Operand of nat cheat must be a literal, or a reference to a variable or constant")),
						_ => unreachable!()
					}

                // Produce no value
                return None;
            }
        }
    }

    pub(super) fn resolve_expr_call(
        &mut self,
        left: &mut Box<Expr>,
        arg_list: &mut Vec<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) -> Option<Value> {
        self.visit_expr(left);
        arg_list.iter_mut().for_each(|expr| {
            let value = self.visit_expr(expr);

            if value.is_some() {
                // Substitute value with folded expression
                *expr = Expr::try_from(value.unwrap()).unwrap();
            }
        });

        // Validate that 'left' is "callable"
        // 3 things that fall under this expression
        // - set cons
        // - array subscript
        // - fcn / proc call
        // Distinguished by the identifier type

        // Validate that the argument types match the type_spec using the given reference
        // eval_type is the result of the call expr

        // For now, call expressions default to runtime-time only, and evaluating to a
        // TypeError.
        // A call expression would be compile-time evaluable if it had no side effects,
        // but we don't check that right now
        *is_compile_eval = false;
        *eval_type = TypeRef::TypeError;

        // TODO: Type check call expressions
        self.reporter.report_error(&left.get_span(), format_args!("Call and subscript expressions are not supported yet"));
        None
    }

    pub(super) fn resolve_expr_dot(
        &mut self,
        left: &mut Box<Expr>,
        field: &mut Identifier,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) -> Option<Value> {
        self.visit_expr(left);
        // Validate that the field exists in the given type
        // eval_type is the field type

        // 4 things falling under this expression
        // - Enum fields references
        // - Record/Union field references
        // - Class public member/method references (through desugared arrow)
        // - Module/Monitor exported references

        // `left` must be of the following types (x indicates not checked):
        // - Type::Enum
        // x Type::Record
        // x Type::Union
        // x Type::Class
        // x Type::Module
        // x Type::Monitor

        // All dot expressions default to runtime evaluation
        *is_compile_eval = false;

        // Dealias & resolve left type
        let left_ref = self.dealias_resolve_type(left.get_eval_type());

        debug_assert!(types::is_base_type(&left_ref, &self.type_table), "Of type {:?}", left_ref);

        if types::is_error(&left_ref) {
            // Is a type error, silently propogate error
            field.type_spec = TypeRef::TypeError;
            *eval_type = TypeRef::TypeError;
            return None;
        }

        if let Some(type_info) = self.type_table.type_from_ref(&left_ref) {
            // Match based on the type info
            match type_info {
                Type::Enum { fields } => {
                    let enum_field = fields.get(&field.name);
                    // Check if the field is a part of the compound type
                    if let Some(field_ref) = enum_field {
                        // Link field type_spec & our type_spec to the field reference
                        field.type_spec = *field_ref;
                        *eval_type = *field_ref;

                        // Update the field ident info
                        field.is_const = true; // Not mutable
                        field.is_typedef = false; // Not typedef

                        // Enum fields are compile-time evaluable
                        field.is_compile_eval = true;
                        *is_compile_eval = true;

                        let enum_id = types::get_type_id(&left_ref).unwrap();
                        let field_id = types::get_type_id(&field_ref).unwrap();
                        let ordinal = if let Type::EnumField { ordinal, .. } =
                            self.type_table.get_type(field_id)
                        {
                            *ordinal
                        } else {
                            0
                        };

                        return Some(Value::EnumValue(field_id, enum_id, ordinal));
                    } else {
                        // Field name is not a part of the enum
                        field.type_spec = TypeRef::TypeError;
                        *eval_type = TypeRef::TypeError;

                        // Grabbing the identifier as the enum type name is a best-effort guess
                        self.reporter.report_error(
                            &field.token.location,
                            format_args!(
                                "'{}' is not a field of the enum type '{}'",
                                field.name,
                                super::get_reference_ident(left)
                                    .map(|ident| ident.name.as_str())
                                    .unwrap_or("<unknown>")
                            ),
                        );
                    }
                }
                Type::Pointer { .. } => {
                    // Not a compound type, special report (for using ->)
                    *eval_type = TypeRef::TypeError;
                    self.reporter.report_error(
                        &left.get_span(),
                        format_args!(
                            "Left side of '.' is not a compound type (did you mean to use '->')"
                        ),
                    );
                }
                _ => {
                    // Not a compound type, produces a type error
                    *eval_type = TypeRef::TypeError;
                    self.reporter.report_error(
                        &left.get_span(),
                        format_args!("Left side of '.' is not a compound type"),
                    );
                }
            }
        } else {
            // Not a compound type, produces a type error
            *eval_type = TypeRef::TypeError;
            self.reporter.report_error(
                &left.get_span(),
                format_args!("Left side of '.' is not a compound type"),
            );
        }

        None
    }

    pub(super) fn resolve_expr_reference(&mut self, ident: &mut Identifier) -> Option<Value> {
        // Use the identifier and grab the associated value
        let (compile_value, is_declared) = self.scope_infos.last_mut().unwrap().use_ident(&ident);

        if !is_declared {
            // Identifier has not been declared at all before this point, report it
            // Only reported once everytime something is not declared
            self.reporter.report_error(
                &ident.token.location,
                format_args!("'{}' has not been declared yet", ident.name),
            );
        }

        if ident.is_declared {
            // Grab the correct identifier information (including the
            // type_spec) in the current scope info
            let new_info = self.scope_infos.last().unwrap()
                .get_ident(&ident.name, ident.instance.into())
                .unwrap();

            // Update the necessary info
            ident.import_index = new_info.import_index;
            ident.is_declared = new_info.is_declared;
            ident.is_typedef = new_info.is_typedef;

            if matches!(new_info.type_spec, TypeRef::Unknown) {
                // If the type is still unknown at this point, force it into a TypeError
                ident.type_spec = TypeRef::TypeError;
            } else {
                ident.type_spec = new_info.type_spec;
            }

            // An identifier is compile-time evaluable if and only if there is an associated expression
            ident.is_compile_eval = compile_value.is_some();
        } else {
            // Not declared, don't touch the `type_spec` (preserves error checking "correctness")
        }

        // Return the reference's associated compile-time value
        return compile_value;
    }

    pub(super) fn resolve_expr_literal(&mut self, value: &mut Token, eval_type: &mut TypeRef) -> Option<Value> {
        // Literal values already have the type resolved, unless the eval type is an IntNat

        if matches!(eval_type, TypeRef::Primitive(PrimitiveType::IntNat)) {
            // Force IntNats into Ints
            *eval_type = TypeRef::Primitive(PrimitiveType::Int);
        }

        if matches!(value.token_type, TokenType::Nil) {
            // Don't produce a compile-time value for 'nil'
            None
        } else {
            // Produce the corresponding literal value
            let tok_type = value.token_type.clone();
            let v = Value::from_token_type(tok_type)
                .expect(&format!("Literal '{:?}' cannot be converted into a compile-time value", value.token_type));

            Some(v)
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
        | TokenType::NotEqu
        | TokenType::In
        | TokenType::NotIn
        | TokenType::And
        | TokenType::Or
        | TokenType::Imply => TypeRef::Primitive(PrimitiveType::Boolean),
        _ => TypeRef::TypeError,
    }
}

/// Default type in a unary expression
fn unary_default(op: &TokenType) -> TypeRef {
    match op {
        TokenType::Not => TypeRef::Primitive(PrimitiveType::Boolean),
        TokenType::Pound => TypeRef::Primitive(PrimitiveType::Nat4),
        TokenType::Plus | TokenType::Minus | TokenType::Caret => TypeRef::TypeError,
        _ => unreachable!(),
    }
}

/// Check if the binary operands are valid for the given operation
/// Assumes that the left and right types are de-aliased and resolved (i.e.
/// they are the base types)
///
/// `left`                  The base type of the right operand \
/// `op`                    The operator to check for \
/// `right`                 The base type of the left operand \
/// `type_table`            Type table to resolve named types \
/// `check_compile_eval`    Whether to check for compile-time evaluability for
///                         certain operations (e.g. shl, shr)
pub(super) fn check_binary_operands(
    left_type: &TypeRef,
    op: &TokenType,
    right_type: &TypeRef,
    type_table: &TypeTable,
) -> Result<TypeRef, TypeRef> {
    // Remaining ordering comparisons require sets, enums, and objectclass types
    // Remaining equality comparisons require the above and full equivalence checking (includng pointers)

    debug_assert!(types::is_base_type(left_type, type_table));
    debug_assert!(types::is_base_type(right_type, type_table));

    match op {
        TokenType::Plus => {
            // Valid conditions:
            // - Both types are strings
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are equivalent sets (not checked right now)
            // Otherwise, TypeError is produced
            if types::is_char_seq_type(left_type) && types::is_char_seq_type(right_type) {
                // String expr, concatenation
                return Ok(TypeRef::Primitive(PrimitiveType::String_));
            } else if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, sum
                return Ok(*types::common_type(left_type, right_type, type_table).unwrap());
            } else if types::is_set(left_type, type_table)
                && types::is_set(right_type, type_table)
                && types::is_equivalent_to(left_type, right_type, type_table)
            {
                // Set union, join
                // Type doesn't matter, as they should be equivalent
                return Ok(*left_type);
            }
        }
        TokenType::Minus | TokenType::Star => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are equivalent sets
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, minus & mul
                return Ok(*types::common_type(left_type, right_type, type_table).unwrap());
            } else if types::is_set(left_type, type_table)
                && types::is_set(right_type, type_table)
                && types::is_equivalent_to(left_type, right_type, type_table)
            {
                // Set operation
                // Type doesn't matter, as they should be equivalent
                return Ok(*left_type);
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
                return Ok(*types::common_type(left_type, right_type, type_table).unwrap());
            }
        }
        TokenType::And | TokenType::Or | TokenType::Xor => {
            // Valid conditions:
            // - Both types are integers (int, nat, etc)
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
            // - Both types are integers (int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_integer_type(left_type) && types::is_integer_type(right_type) {
                // Integer expr, produce nat
                return Ok(TypeRef::Primitive(PrimitiveType::Nat));
            }
        }
        TokenType::Less | TokenType::LessEqu | TokenType::Greater | TokenType::GreaterEqu => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are char or strings/character sequence class types (string, string(n), char(n), or char)
            // - Both types are sets (not necessarily equivalent)
            // - Both types are enums (not necessarily equivalent)
            // Valid, but not checked:
            // - Both types are (object) classes (not necessarily equivalent)
            // Otherwise, Boolean (as an error) is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number compare, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if (types::is_char_seq_type(left_type) || types::is_char(left_type))
                && (types::is_char_seq_type(right_type) || types::is_char(right_type))
            {
                // String/char seq or char class type, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_set(left_type, type_table) && types::is_set(right_type, type_table)
            {
                // Set comparision, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_enum_type(left_type, type_table)
                && types::is_enum_type(right_type, type_table)
            {
                // Enum ordering comparison, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            }
            // TODO: Check remaining type (object class)
        }
        TokenType::Equ | TokenType::NotEqu => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are char or strings class types (string, string(n), char, char(n))
            // - Both types are sets (not necessarily equivalent)
            // - Both types are booleans
            // - Both types are enums (not necessarily equivalent)
            // - Both types are pointers (with equivalent types)
            // Valid, but not checked:
            // - Both types are (object) classes (not necessarily equivalent)
            // - Both types are class pointers (not necessarily equivalent)
            // Otherwise, Boolean (as an error) is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number equality, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_boolean(left_type) && types::is_boolean(right_type) {
                // Boolean equality, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if (types::is_char_seq_type(left_type) || types::is_char(left_type))
                && (types::is_char_seq_type(right_type) || types::is_char(right_type))
            {
                // String/char seq or char class type, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_set(left_type, type_table) && types::is_set(right_type, type_table)
            {
                // Set equality, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_enum_type(left_type, type_table)
                && types::is_enum_type(right_type, type_table)
            {
                // Enum equality comparison, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_pointer(left_type, type_table)
                && types::is_pointer(right_type, type_table)
            {
                // Pointer comparison, not necessarily the same type (but should have the same checkedness)
                if let Some(Type::Pointer { is_unchecked, .. }) =
                    type_table.type_from_ref(left_type)
                {
                    if let Some(Type::Pointer {
                        is_unchecked: other_unchecked,
                        ..
                    }) = type_table.type_from_ref(right_type)
                    {
                        if is_unchecked == other_unchecked {
                            // Same checkedness
                            return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
                        }
                    }
                }
            }
            // TODO: Check remaining types (class pointers, object class)
        }
        TokenType::In | TokenType::NotIn => {
            // Valid conditions:
            // - Left type matches the set's index type
            // - Right type is a set
            // Otherwise, Boolean (as error) is produced
            if let Some(Type::Set { range }) = type_table.type_from_ref(right_type) {
                // Use 'is_assignable_to' is used for checking range compatibility with the left operand
                if types::is_assignable_to(range, left_type, type_table) {
                    // Element test with equivalent primitive types produces boolean
                    return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
                }
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
        _ => unreachable!(),
    }

    Err(binary_default(op))
}

fn check_unary_operand(
    op: &TokenType,
    right_type: &TypeRef,
    type_table: &TypeTable,
) -> Result<TypeRef, TypeRef> {
    debug_assert!(types::is_base_type(right_type, &type_table));

    match op {
        TokenType::Not => {
            // Valid conditions:
            // - Operand is an integer (int, nat)
            // - Operand is a boolean
            // Otherwise, boolean is produced (as an error)
            if types::is_integer_type(right_type) {
                // Produce an unsized nat
                return Ok(TypeRef::Primitive(PrimitiveType::Nat));
            } else if types::is_boolean(right_type) {
                // Produce a boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            }
        }
        TokenType::Pound => {
            // Pound type cheat always forces the current operand into a nat4
            return Ok(TypeRef::Primitive(PrimitiveType::Nat4));
        }
        TokenType::Plus | TokenType::Minus => {
            // Valid conditions:
            // - Operand is a numeric (real, int, nat)
            // Otherwise, TypeError is produced (as an error)
            if types::is_number_type(right_type) {
                // Produce the same type as the operand
                return Ok(*right_type);
            }
        }
        TokenType::Caret => {
            // Valid conditions:
            // - Operand is a pointer type (produces the pointer's type)
            // Otherwise, TypeError is produced (as an error)
            if let Some(Type::Pointer { to, .. }) = type_table.type_from_ref(right_type) {
                // Produce the type pointed to by the type ref
                return Ok(*to);
            }
        }
        _ => unreachable!(),
    }

    Err(unary_default(op))
}
