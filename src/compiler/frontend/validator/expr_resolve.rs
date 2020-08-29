//! Validator fragment, resolves all expressions
use super::Validator;

use crate::compiler::ast::{Expr, Identifier, VisitorMut};
use crate::compiler::frontend::token::{Token, TokenType};
use crate::compiler::types::{self, ParamDef, PrimitiveType, Type, TypeRef, TypeTable};
use crate::compiler::value::{self, Value, ValueApplyError};
use crate::compiler::Location;
use std::cmp::Ordering;

impl Validator {
    // --- Expr Resolvers --- //

    // Resolves an "init" expression
    pub(super) fn resolve_expr_init(&mut self, exprs: &mut Vec<Expr>) -> Option<Value> {
        for expr in exprs.iter_mut() {
            let value = self.visit_expr(expr);

            // Replace with folded expression
            super::replace_with_folded(expr, value);

            if super::is_type_reference(expr) {
                self.reporter.report_error(
                    expr.get_span(),
                    format_args!("Reference does not refer to a variable or constant"),
                );
            } else if !matches!(expr, Expr::Empty) && !expr.is_compile_eval() {
                self.reporter.report_error(
                    expr.get_span(),
                    format_args!("Expression is not a compile-time expression"),
                );
            }
        }
        None
    }

    /// Resolves an indirection expression
    pub(super) fn resolve_expr_indirect(
        &mut self,
        reference: &mut Option<Box<Expr>>,
        addr: &mut Box<Expr>,
        eval_type: &mut TypeRef,
    ) -> Option<Value> {
        if let Some(reference) = reference {
            let ref_eval = self.visit_expr(reference);
            super::replace_with_folded(reference, ref_eval);
        }

        let addr_eval = self.visit_expr(addr);
        super::replace_with_folded(addr, addr_eval);

        // `reference` must be a type reference
        if let Some(reference) = reference {
            if !super::is_type_reference(reference) {
                self.reporter.report_error(
                    reference.get_span(),
                    format_args!("Reference does not refer to a type"),
                );

                if *eval_type == TypeRef::Unknown {
                    // Force into a type error
                    *eval_type = TypeRef::TypeError;
                }
            } else if *eval_type == TypeRef::Unknown {
                // Use type eval of reference expr
                *eval_type = reference.get_eval_type();
            }
        }

        // Resolve `eval_type` & de-alias it
        if !types::is_error(eval_type) {
            *eval_type = self.dealias_resolve_type(*eval_type);
        }

        // `addr` must evaluate to a `nat` or `int` type, not evaluating to a type reference
        if super::is_type_reference(addr) {
            self.reporter.report_error(
                addr.get_span(),
                format_args!("Indirection address reference is not a 'var' or 'const' reference"),
            );
        } else if !matches!(**addr, Expr::Empty) {
            let dealiased_addr = types::dealias_ref(&addr.get_eval_type(), &self.type_table);

            if !types::is_assignable_to(
                &TypeRef::Primitive(PrimitiveType::Int),
                &dealiased_addr,
                &self.type_table,
            ) {
                self.reporter.report_error(
                    addr.get_span(),
                    format_args!(
                        "Indirection address expression does not evaluate to an integer type",
                    ),
                );
            }
        }

        // ???: Warn about null address accesses?

        // Never return a value
        None
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

        // Replace the operands with the folded values
        super::replace_with_folded(left, left_eval);
        super::replace_with_folded(right, right_eval);

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
                    let lvalue =
                        Value::from_expr(*left.clone(), &self.type_table).unwrap_or_else(|msg| {
                            panic!(
                                "Left operand is not a compile-time value {:?} ({})",
                                left, msg
                            )
                        });
                    let rvalue =
                        Value::from_expr(*right.clone(), &self.type_table).unwrap_or_else(|msg| {
                            panic!(
                                "Right operand is not a compile-time value {:?} ({})",
                                left, msg
                            )
                        });

                    let result = value::apply_binary(lvalue, op, rvalue);

                    match result {
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
                    }
                } else {
                    // Can't fold the current expression
                    None
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
                None
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

        // Replace operand with the folded value
        super::replace_with_folded(right, right_eval);

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
                    let rvalue =
                        Value::from_expr(*right.clone(), &self.type_table).unwrap_or_else(|msg| {
                            panic!(
                                "Right operand is not a compile-time value {:?} ({})",
                                right, msg
                            )
                        });

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
                None
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
                None
            }
        }
    }

    // `allow_procedure`: Should procedure calls be allowed in this position
    pub(super) fn resolve_expr_call(
        &mut self,
        left: &mut Box<Expr>,
        op: &mut Token,
        args: &mut Vec<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
        allow_procedure: bool,
    ) -> Option<Value> {
        // Visit given exprs
        self.visit_expr(left);

        args.iter_mut().for_each(|expr| {
            let value = self.visit_expr(expr);

            // Don't fold for dot and reference exprs
            if !matches!(expr, Expr::Reference { .. } | Expr::Dot { .. }) {
                super::replace_with_folded(expr, value);
            }
        });

        // Validate that 'left' is "callable"
        // 5 things that fall under this expression
        // - set cons
        // - pointer specialization
        // - array subscript
        // - string/string(n)/char(n) subscript
        // - fcn / proc call
        // Distinguished by the identifier type

        // TODO: Type check call expressions
        // For now, call expressions default to runtime-time only, and evaluating to a
        // TypeError.
        // A call expression would be compile-time evaluable if it had no side effects,
        // but we don't check that right now
        *is_compile_eval = false;
        *eval_type = TypeRef::TypeError;

        let left_dealiased = types::dealias_ref(&left.get_eval_type(), &self.type_table);

        // TODO: Need tests for:
        // - Set Cons (incompatible element arg, arg is type ref)
        // x Pointer specialization (not a pointer, wrong arg count, etc...)
        // x CharSeq subscript
        // - Array subscript
        // - Procedure & Function call
        //   - Too few
        //   - Too many
        //   - tyref
        //   - const expr for ref param
        //   - var for ref param
        //   - other for ref param
        //   - empty for ref param
        //   - wrong type no coerece
        //   - wrong type no coerece empty
        //   - wrong type coerce
        // - Bare Procedure & Function call
        // - Bare function calls in expr position

        // TODO: Check for bare function calls in reference & dot exprs

        // Specialize the call types
        // - Discriminants:
        //   - If left is a type ref
        //   - Type of left
        if super::is_type_reference(left) {
            // Call expression should fall under the 2 types
            // - Set constructor (left.eval_type = Type::Set)
            //   - Params must be equivalent to base index type
            // - Pointer specialization (left.eval_type = Type::Pointer{ to }, matches!(to, Type::Class | Type::Collection))
            //   - Must be 1 param, param must be equivalent to pointer `to` type
            // Otherwise, type cannot be called or subscripted

            match self.type_table.type_from_ref(&left_dealiased) {
                Some(Type::Set { range }) => {
                    // Set constructor
                    self.typecheck_set_constructor(range, args, &op.location);

                    // Evaluate to the given set type decl
                    *eval_type = left.get_eval_type();
                    // Never compile-time evaluable
                    *is_compile_eval = false;

                    None
                }
                Some(Type::Pointer { to: _, .. }) => {
                    // Pointer specialization
                    if args.len() != 1 {
                        self.reporter.report_error(
                            &op.location,
                            format_args!(
                                "Pointer specialization requires only 1 argument to be present"
                            ),
                        )
                    }

                    // TODO: Fill out for both collection & class types
                    self.reporter.report_error(
                        &left.get_span(),
                        format_args!("Pointer specialization expressions are not supported yet"),
                    );

                    None
                }
                _ => {
                    // Can't call given expr
                    self.report_uncallable_expr(left);
                    None
                }
            }
        } else if types::is_char_seq_type(&left_dealiased) {
            // Call expression is a CharSeq subscripting
            // - Must be 1 param, param type should be an expr, a subscript, or a subscript pair
            // TODO: Fill out after parsing subscript and subscript pairs
            self.reporter.report_error(
                &left.get_span(),
                format_args!("String subscript expressions are not supported yet"),
            );

            None
        } else {
            // Call expression should fall under the 2 types
            // - Array subscripting
            //   - Param types must correspond to equivalent array range types
            // - Procedure / function call
            //   - Param types must correspond to equivalent parameters
            // Otherwise, type cannot be called or subscripted
            match self.type_table.type_from_ref(&left_dealiased) {
                Some(Type::Array {
                    ranges,
                    element_type,
                    ..
                }) => {
                    self.typecheck_array_dimensions(ranges, args, &op.location);

                    // Evaluates to the element type
                    *eval_type = *element_type;
                    // Never compile-time evaluable
                    *is_compile_eval = false;

                    None
                }
                Some(Type::Function { params, result }) => {
                    // Typecheck the function arguments
                    self.typecheck_function_arguments(params.as_ref(), args, &op.location);

                    // allow_procedure
                    *eval_type = if let Some(result_type) = result {
                        // Get function result type
                        *result_type
                    } else if allow_procedure {
                        // Procedures allowed in this position (e.g. as procedure calls)
                        TypeRef::TypeError
                    } else {
                        // Procedures not allowed in this position
                        self.reporter.report_error(
                            left.get_span(),
                            format_args!(
                                "Reference is to a procedure and cannot be used in expressions"
                            ),
                        );
                        TypeRef::TypeError
                    };

                    // We don't know if the expr call is compile-time unless it's a predef,
                    // so by default, everything is not compile-time evaluable
                    *is_compile_eval = false;

                    None
                }
                _ => {
                    // Can't call given expr
                    self.report_uncallable_expr(left);
                    None
                }
            }
        }
    }

    fn report_uncallable_expr(&self, left: &Expr) {
        if let Some(ident) = super::get_reference_ident(left) {
            self.reporter.report_error(
                &ident.token.location,
                format_args!("'{}' cannot be called or have subscripts", ident.name),
            );
        } else if !matches!(*left, Expr::Empty) {
            self.reporter.report_error(
                left.get_span(),
                format_args!("Expression cannot be called or have subscripts",),
            );
        }
    }

    /// Typechecks the given set constructor, checking for type compatibility
    fn typecheck_set_constructor(&self, index_type: &TypeRef, args: &[Expr], at_paren: &Location) {
        // All params must be assignable to range type
        // TODO: Handle `all` token
        if args.is_empty() {
            self.reporter.report_error(
                &at_paren,
                format_args!("Set constructors require at least 1 parameter"),
            );
        }

        args.iter().for_each(|element| {
            let element_dealiased = types::dealias_ref(&element.get_eval_type(), &self.type_table);

            if !types::is_assignable_to(index_type, &element_dealiased, &self.type_table) {
                self.reporter.report_error(
                    element.get_span(),
                    format_args!("Element parameter is not compatible with set element type"),
                );
            }
        });
    }

    /// Typechecks the given function arguments, checking for type compatibility
    /// and the correct argument count.
    pub(super) fn typecheck_function_arguments(
        &self,
        params: Option<&Vec<ParamDef>>,
        args: &[Expr],
        at_paren: &Location,
    ) {
        if let Some(param_types) = params {
            for (param_def, arg) in param_types.iter().zip(args.iter()) {
                let param_dealias = types::dealias_ref(&param_def.type_spec, &self.type_table);
                let arg_dealias = types::dealias_ref(&arg.get_eval_type(), &self.type_table);

                if super::is_type_reference(arg) {
                    // Is a type ref, error!
                    self.reporter.report_error(
                        arg.get_span(),
                        format_args!(
                            "Cannot use a type reference as a function or procedure parameter"
                        ),
                    );
                } else {
                    if param_def.pass_by_ref {
                        // Parameter must be var to be passed as a ref
                        if let Some(ident) = super::get_reference_ident(arg) {
                            if ident.is_const {
                                self.reporter.report_error(
                                    arg.get_span(),
                                    format_args!(
                                        "Cannot pass a reference parameter to a constant reference"
                                    ),
                                );
                            }
                        } else if !matches!(arg, Expr::Empty) {
                            // Not a var!
                            self.reporter.report_error(
                                arg.get_span(),
                                format_args!("Cannot pass a reference parameter to an expression"),
                            );
                        }
                    }

                    if !param_def.force_type
                        && !types::is_assignable_to(&param_dealias, &arg_dealias, &self.type_table)
                        && !matches!(arg, Expr::Empty)
                    {
                        // Report error if the expr is not an empty expr and the type isn't coerced
                        self.reporter.report_error(
                            arg.get_span(),
                            format_args!("Argument is the wrong type"),
                        );
                    }
                }
            }
        }

        // Check arg count
        let param_type_count = if let Some(param_types) = params {
            param_types.len()
        } else {
            0
        };
        let arg_count = args.len();

        match arg_count.cmp(&param_type_count) {
            Ordering::Less => {
                // Not enough arguments
                self.reporter.report_error(
                    &at_paren,
                    format_args!(
                        "Missing {} arguments for call expression",
                        param_type_count - arg_count
                    ),
                );
            }
            Ordering::Greater => {
                // Too many arguments
                self.reporter.report_error(
                    &at_paren,
                    format_args!(
                        "{} extra arguments for call expression",
                        arg_count - param_type_count
                    ),
                );
            }
            _ => {}
        }
    }

    /// Typechecks the given array dimensions, checking for type compatibility
    /// and the correct dimension count
    fn typecheck_array_dimensions(&self, ranges: &[TypeRef], args: &[Expr], at_paren: &Location) {
        // Check dimension type compatibility
        for (dim_type, arg) in ranges.iter().zip(args.iter()) {
            if !types::is_assignable_to(dim_type, &arg.get_eval_type(), &self.type_table)
                && !matches!(arg, Expr::Empty)
            {
                self.reporter.report_error(
                    arg.get_span(),
                    format_args!("Expression evaluates to the wrong type"),
                );
            }
        }

        // Check range arg count
        // ???: This is duplicated with function param length, check?
        let dim_count = ranges.len();
        let arg_count = args.len();

        match arg_count.cmp(&dim_count) {
            Ordering::Less => {
                // Not enough arguments
                self.reporter.report_error(
                    &at_paren,
                    format_args!(
                        "Missing {} dimensions for array subscript",
                        dim_count - arg_count
                    ),
                );
            }
            Ordering::Greater => {
                // Too many arguments
                self.reporter.report_error(
                    &at_paren,
                    format_args!(
                        "{} extra dimensions for array subscript",
                        arg_count - dim_count
                    ),
                );
            }
            _ => {}
        }
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

        debug_assert!(
            types::is_base_type(&left_ref, &self.type_table),
            "Of type {:?}",
            left_ref
        );

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
            let new_info = self
                .scope_infos
                .last()
                .unwrap()
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
        compile_value
    }

    pub(super) fn resolve_expr_literal(
        &mut self,
        value: &mut Token,
        eval_type: &mut TypeRef,
    ) -> Option<Value> {
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
            let v = Value::from_token_type(tok_type).unwrap_or_else(|msg| {
                panic!(
                    "Literal '{:?}' cannot be converted into a compile-time value ({})",
                    value.token_type, msg
                );
            });

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
