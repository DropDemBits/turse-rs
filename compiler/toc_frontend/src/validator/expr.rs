//! Validator fragment, resolves all expressions
use super::Validator;

use std::cmp::Ordering;
use toc_ast::ast::expr::{BinaryOp, Expr, ExprKind, FieldDef, Literal, UnaryOp};
use toc_ast::ast::ident::{IdentRef, RefKind};
use toc_ast::ast::types::{Type as TypeNode, TypeKind};
use toc_ast::ast::VisitorMut;
use toc_ast::types::{self, ParamInfo, PrimitiveType, Type, TypeRef, TypeTable};
use toc_core::Location;

impl Validator<'_> {
    // --- Expr Resolvers --- //

    // Resolves a parentheses expression
    pub(super) fn resolve_expr_parens(
        &mut self,
        inner: &mut Box<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) {
        self.visit_expr(inner);

        // Carry over properties
        *eval_type = inner.eval_type;
        *is_compile_eval = inner.is_compile_eval;
    }

    // Resolves an "init" expression
    pub(super) fn resolve_expr_init(&mut self, exprs: &mut Vec<Expr>) {
        for expr in exprs.iter_mut() {
            self.visit_expr(expr);

            // Replace with folded expression
            let value = self.eval_expr(expr).ok().flatten();
            super::replace_with_folded(expr, value);

            if self.is_type_reference(expr) {
                self.reporter.borrow_mut().report_error(
                    expr.get_span(),
                    format_args!("Reference does not refer to a variable or constant"),
                );
            } else if !expr.is_compile_eval() {
                self.reporter.borrow_mut().report_error(
                    expr.get_span(),
                    format_args!("Expression is not a compile-time expression"),
                );
            }
        }
    }

    /// Resolves an indirection expression
    pub(super) fn resolve_expr_indirect(
        &mut self,
        indirect_type: &mut Box<TypeNode>,
        addr: &mut Box<Expr>,
        eval_type: &mut TypeRef,
    ) {
        self.visit_expr(addr);
        self.visit_type(indirect_type);

        // Try to fold the address expression
        let addr_value = self.eval_expr(addr).ok().flatten();
        super::replace_with_folded(addr, addr_value);

        // `reference` must be a type reference or a primitive type
        if let TypeKind::Reference { ref_expr, .. } = &indirect_type.kind {
            if !self.is_type_reference(ref_expr) {
                self.reporter.borrow_mut().report_error(
                    &indirect_type.span,
                    format_args!("Reference does not refer to a type"),
                );

                if *eval_type == TypeRef::Unknown {
                    // Force into a type error
                    *eval_type = TypeRef::TypeError;
                }
            }
        }

        // Use resolved type
        *eval_type = types::dealias_ref(indirect_type.type_ref(), &self.type_table);

        // `addr` must evaluate to a `nat` or `int` type, not evaluating to a type reference
        if self.is_type_reference(addr) {
            self.reporter.borrow_mut().report_error(
                addr.get_span(),
                format_args!("Indirection address reference is not a 'var' or 'const' reference"),
            );
        } else {
            let dealiased_addr = types::dealias_ref(&addr.get_eval_type(), &self.type_table);

            if !types::is_assignable_to(
                &TypeRef::Primitive(PrimitiveType::Int),
                &dealiased_addr,
                &self.type_table,
            ) {
                self.reporter.borrow_mut().report_error(
                    addr.get_span(),
                    format_args!(
                        "Indirection address expression does not evaluate to an integer type",
                    ),
                );
            }
        }

        // ???: Warn about null address accesses?
    }

    pub(super) fn resolve_expr_binary(
        &mut self,
        left: &mut Box<Expr>,
        (op, location): &(BinaryOp, Location),
        right: &mut Box<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) {
        self.visit_expr(left);
        self.visit_expr(right);

        // Validate that the types are assignable with the given operation
        // eval_type is the type of the expr result

        if self.is_type_reference(left) || self.is_type_reference(right) {
            // Left or right operand is a type reference, can't perform operations on them
            *eval_type = TypeRef::TypeError;
            *is_compile_eval = false;

            if self.is_type_reference(left) {
                self.reporter.borrow_mut().report_error(
                    left.get_span(),
                    format_args!("Operand is not a variable or constant reference"),
                );
            }
            if self.is_type_reference(right) {
                self.reporter.borrow_mut().report_error(
                    right.get_span(),
                    format_args!("Operand is not a variable or constant reference"),
                );
            }

            return;
        }

        let left_type = &types::dealias_ref(&left.get_eval_type(), &self.type_table);
        let right_type = &types::dealias_ref(&right.get_eval_type(), &self.type_table);

        if types::is_error(left_type) || types::is_error(right_type) {
            // Either one is a type error
            // Set default type & return no value (no need to report an error as this is just propoagtion)
            *eval_type = binary_default(*op);
            *is_compile_eval = false;
            return;
        }

        debug_assert!(types::is_base_type(left_type, &self.type_table));
        debug_assert!(types::is_base_type(right_type, &self.type_table));

        match check_binary_operands(left_type, *op, right_type, &self.type_table) {
            Ok(good_eval) => {
                // Only evaluable if the operands are not type errors and are applicable to the current op
                *is_compile_eval = left.is_compile_eval() && right.is_compile_eval();
                *eval_type = good_eval;
            }
            Err(bad_eval) => {
                *eval_type = bad_eval;
                *is_compile_eval = false;

                match op {
                    BinaryOp::Add =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be scalars (int, real, or nat), strings, or compatible sets", op), ),
                    BinaryOp::Sub | BinaryOp::Mul =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be scalars (int, real, or nat), or compatible sets", op)),
                    BinaryOp::RealDiv | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Rem | BinaryOp::Exp =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be scalars (int, real, or nat)", op)),
                    BinaryOp::And | BinaryOp::Or | BinaryOp::Xor =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be scalars (int, real, or nat) or booleans", op)),
                    BinaryOp::Shl | BinaryOp::Shr =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be integers (int, or nat)", op)),
                    BinaryOp::Less | BinaryOp::LessEq | BinaryOp::Greater | BinaryOp::GreaterEq => {
                        if types::is_equivalent_to(left_type, right_type, &self.type_table) {
                            self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be scalars (int, real, or nat), sets, enumerations, strings, or object classes", op))
                        } else {
                            self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must be the same type", op))
                        }
                    },
                    BinaryOp::NotEqual | BinaryOp::Equal => {
                        if types::is_equivalent_to(left_type, right_type, &self.type_table) {
                            self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be booleans, scalars (int, real, or nat), sets, enumerations, strings, object classes, or pointers of equivalent types", op));
                        } else {
                            self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must be the same type", op));
                        }
                    },
                    BinaryOp::In | BinaryOp::NotIn => {
                        if types::is_set(right_type, &self.type_table) {
                            self.reporter.borrow_mut().report_error(&location, format_args!("Left operand of '{}' must be compatible with the set's index type", op));
                        } else {
                            self.reporter.borrow_mut().report_error(&location, format_args!("Right operand of '{}' must be a set type", op));
                        }
                    },
                    BinaryOp::Imply =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operands of '{}' must both be booleans", op)),
                    _ => unreachable!(),
                }
            }
        }
    }

    pub(super) fn resolve_expr_unary(
        &mut self,
        (op, location): &(UnaryOp, Location),
        right: &mut Box<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) {
        self.visit_expr(right);

        // Validate that the unary operator can be applied to the rhs
        // eval_type is the result of the operation (usually the same
        // as rhs)

        if self.is_type_reference(right) {
            // Operand is a type reference, can't perform operations on it
            *eval_type = TypeRef::TypeError;
            *is_compile_eval = false;

            self.reporter.borrow_mut().report_error(
                right.get_span(),
                format_args!("Operand is not a variable or constant reference"),
            );

            return;
        }

        let right_type = &types::dealias_ref(&right.get_eval_type(), &self.type_table);

        if types::is_error(right_type) {
            // Right operand is a type error

            // Propogate error
            *eval_type = unary_default(*op);
            *is_compile_eval = false;

            return;
        }

        debug_assert!(types::is_base_type(right_type, &self.type_table));

        match check_unary_operand(*op, right_type, &self.type_table) {
            Ok(good_eval) => {
                *eval_type = good_eval;
                // Compile-time evaluability is dependend on the right operand
                *is_compile_eval = right.is_compile_eval();

                if op == &UnaryOp::Deref {
                    // Pointers are never compile-time evaluable
                    *is_compile_eval = false;
                }
            }
            Err(bad_eval) => {
                *eval_type = bad_eval;
                *is_compile_eval = false;

                match op {
                    UnaryOp::Not =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operand of 'not' must be an integer (int or nat) or a boolean")),
                    UnaryOp::Identity =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operand of prefix '+' must be a scalar (int, real, or nat)")),
                    UnaryOp::Negate =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operand of unary negation must be a scalar (int, real, or nat)")),
                    UnaryOp::Deref =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operand of pointer dereference must be a pointer")),
                    UnaryOp::NatCheat =>
                        self.reporter.borrow_mut().report_error(&location, format_args!("Operand of nat cheat must be a literal, or a reference to a variable or constant")),
                }
            }
        }
    }

    /// `allow_procedure`: Should procedure calls be allowed in this position
    pub(super) fn resolve_expr_call(
        &mut self,
        left: &mut Box<Expr>,
        paren_at: &mut Location,
        args: &mut Vec<Expr>,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
        allow_procedure: bool,
    ) {
        // Visit given exprs
        self.visit_expr(left);

        args.iter_mut().for_each(|expr| {
            self.visit_expr(expr);

            // Don't fold for dot and reference exprs
            if !matches!(expr.kind, ExprKind::Reference { .. } | ExprKind::Dot { .. }) {
                let value = self.eval_expr(expr).ok().flatten();
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

        // TODO: Type check the rest of the call expressions
        // Call expressions default to runtime-time only and evaluating to a
        // TypeError if type check fails.
        // A call expression would be compile-time evaluable if it had no side effects,
        // but we don't check that right now
        *is_compile_eval = false;
        *eval_type = TypeRef::TypeError;

        let left_type = if let Some((_, tyspec, _, _)) = self.get_reference_ident(&left) {
            // We care about the subroutine reference and not the evaluation type in a reference expr
            *tyspec
        } else {
            // Take the eval type as we may get the callable from an expr eval
            left.get_eval_type()
        };

        let left_dealiased = types::dealias_ref(&left_type, &self.type_table);

        // TODO: Need tests for:
        // - Set Cons (incompatible element arg, arg is type ref)
        // x Pointer specialization (not a pointer, wrong arg count, etc...)
        // x CharSeq subscript
        // - Array subscript

        // TODO: Check for bare function calls in dot exprs with fields (records, unions, classes, modules, monitors)

        // Specialize the call types
        // - Discriminants:
        //   - If left is a type ref
        //   - Type of left
        if self.is_type_reference(left) {
            // Call expression should fall under the 2 types
            // - Set constructor (left.eval_type = Type::Set)
            //   - Params must be equivalent to base index type
            // - Pointer specialization (left.eval_type = Type::Pointer{ to }, matches!(to, Type::Class | Type::Collection))
            //   - Must be 1 param, param must be equivalent to pointer `to` type
            // Otherwise, type cannot be called or subscripted

            match self.type_table.type_from_ref(&left_dealiased) {
                Some(Type::Set { range }) => {
                    // Set constructor
                    self.typecheck_set_constructor(range, args, paren_at);

                    // Evaluate to the given set type decl
                    *eval_type = left.get_eval_type();
                    // Never compile-time evaluable
                    *is_compile_eval = false;
                }
                Some(Type::Pointer { to: _, .. }) => {
                    // Pointer specialization
                    if args.len() > 1 {
                        // Too many args
                        let last_arg = args
                            .last()
                            .map(Expr::get_span)
                            .expect("No args in a many args expr");

                        self.reporter.borrow_mut().report_error(
                            last_arg,
                            format_args!(
                                "Too many arguments for pointer specialization (expected 1, found {})", args.len()
                            ),
                        )
                    } else if args.is_empty() {
                        // Not enough args
                        self.reporter.borrow_mut().report_error(
                            paren_at,
                            format_args!("Pointer specialization requires 1 argument"),
                        )
                    }

                    if let Some(expr) = args.first() {
                        if self.is_type_reference(expr) {
                            // Not a const/var ref

                            self.reporter.borrow_mut().report_error(
                                &expr.get_span(),
                                format_args!(
                                    "Expression refers to a type, and is not allowed here"
                                ),
                            )
                        }
                    }

                    // TODO: Fill out for both collection & class types
                    self.reporter.borrow_mut().report_error(
                        &left.get_span(),
                        format_args!("Pointer specialization expressions are not supported yet"),
                    );
                }
                _ => {
                    // Can't call given expr
                    self.report_uncallable_expr(left);
                }
            }
        } else if types::is_char_seq_type(&left_dealiased) {
            // Call expression is a CharSeq subscripting
            // - Must be 1 param, param type should be an expr, a subscript, or a subscript pair
            // TODO: Fill out after parsing subscript and subscript pairs
            self.reporter.borrow_mut().report_error(
                &left.get_span(),
                format_args!("String subscript expressions are not supported yet"),
            );
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
                    self.typecheck_array_dimensions(ranges, args, paren_at);

                    // Evaluates to the element type
                    *eval_type = *element_type;
                    // Never compile-time evaluable
                    *is_compile_eval = false;
                }
                Some(Type::Function { params, result }) => {
                    // Typecheck the function arguments
                    self.typecheck_function_arguments(params.as_ref(), args, paren_at);

                    // allow_procedure
                    *eval_type = if let Some(result_type) = result {
                        // Get function result type
                        *result_type
                    } else if allow_procedure {
                        // Procedures allowed in this position (e.g. as procedure calls)
                        TypeRef::TypeError
                    } else {
                        // Procedures not allowed in this position
                        self.reporter.borrow_mut().report_error(
                            left.get_span(),
                            format_args!(
                                "Reference is to a procedure and cannot be used in expressions"
                            ),
                        );
                        TypeRef::TypeError
                    };

                    // We don't know if the expr call is compile-time unless it's a predef or builtin,
                    // so by default, everything is not compile-time evaluable
                    *is_compile_eval = false;
                }
                _ => {
                    // Can't call given expr
                    self.report_uncallable_expr(left);
                }
            }
        }
    }

    fn report_uncallable_expr(&self, left: &Expr) {
        if let Some((name, .., location)) = self.get_reference_ident(left) {
            self.reporter.borrow_mut().report_error(
                location,
                format_args!("'{}' cannot be called or have subscripts", name),
            );
        } else {
            self.reporter.borrow_mut().report_error(
                left.get_span(),
                format_args!("Expression cannot be called or have subscripts",),
            );
        }
    }

    /// Typechecks the given set constructor, checking for type compatibility
    fn typecheck_set_constructor(&self, index_type: &TypeRef, args: &[Expr], at_paren: &Location) {
        // TODO: Handle `all` token
        if args.is_empty() {
            self.reporter.borrow_mut().report_error(
                &at_paren,
                format_args!("Set constructors require at least 1 parameter"),
            );
        }

        // All params must be assignable to range type
        args.iter().for_each(|element| {
            let element_dealiased = types::dealias_ref(&element.get_eval_type(), &self.type_table);

            if !types::is_assignable_to(index_type, &element_dealiased, &self.type_table) {
                self.reporter.borrow_mut().report_error(
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
        params: Option<&Vec<(TypeRef, ParamInfo)>>,
        args: &[Expr],
        at_paren: &Location,
    ) {
        if let Some(param_types) = params {
            for ((param_type, param_def), arg) in param_types.iter().zip(args.iter()) {
                let param_dealias = types::dealias_ref(&param_type, &self.type_table);
                let arg_dealias = types::dealias_ref(&arg.get_eval_type(), &self.type_table);

                if self.is_type_reference(arg) {
                    // Is a type ref, error!
                    self.reporter.borrow_mut().report_error(
                        arg.get_span(),
                        format_args!(
                            "Cannot use a type reference as a function or procedure parameter"
                        ),
                    );
                } else {
                    if param_def.pass_by_ref {
                        // Parameter must be var to be passed as a ref
                        if let Some((_, _, ref_kind, _)) = self.get_reference_ident(arg) {
                            if *ref_kind != RefKind::Var {
                                self.reporter.borrow_mut().report_error(
                                    arg.get_span(),
                                    format_args!(
                                        "Cannot pass a reference parameter to a constant reference"
                                    ),
                                );
                            }
                        } else {
                            // Not a var!
                            self.reporter.borrow_mut().report_error(
                                arg.get_span(),
                                format_args!("Cannot pass a reference parameter to an expression"),
                            );
                        }
                    }

                    if !param_def.force_type
                        && !types::is_assignable_to(&param_dealias, &arg_dealias, &self.type_table)
                    {
                        // Report error if the expr is not an empty expr and the type isn't coerced
                        self.reporter.borrow_mut().report_error(
                            arg.get_span(),
                            format_args!("Argument is the wrong type"),
                        );
                    }
                }
            }
        }

        // Check arg count
        let param_type_count = params.map_or(0, Vec::len);
        let arg_count = args.len();

        match arg_count.cmp(&param_type_count) {
            Ordering::Less => {
                // Not enough arguments
                self.reporter.borrow_mut().report_error(
                    &at_paren,
                    format_args!(
                        "Missing {} arguments for call expression",
                        param_type_count - arg_count
                    ),
                );
            }
            Ordering::Greater => {
                // Too many arguments
                self.reporter.borrow_mut().report_error(
                    &at_paren,
                    format_args!(
                        "{} extra arguments for call expression",
                        arg_count - param_type_count
                    ),
                );
            }
            Ordering::Equal => {} // Nothing to do
        }
    }

    /// Typechecks the given array dimensions, checking for type compatibility
    /// and the correct dimension count
    fn typecheck_array_dimensions(&self, ranges: &[TypeRef], args: &[Expr], at_paren: &Location) {
        // Check dimension type compatibility
        for (dim_type, arg) in ranges.iter().zip(args.iter()) {
            if !types::is_assignable_to(dim_type, &arg.get_eval_type(), &self.type_table)
                && !matches!(arg.kind, ExprKind::Error)
            {
                self.reporter.borrow_mut().report_error(
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
                self.reporter.borrow_mut().report_error(
                    &at_paren,
                    format_args!(
                        "Missing {} dimensions for array subscript",
                        dim_count - arg_count
                    ),
                );
            }
            Ordering::Greater => {
                // Too many arguments
                self.reporter.borrow_mut().report_error(
                    &at_paren,
                    format_args!(
                        "{} extra dimensions for array subscript",
                        arg_count - dim_count
                    ),
                );
            }
            Ordering::Equal => {} // Nothing to do
        }
    }

    pub(super) fn resolve_expr_dot(
        &mut self,
        left: &mut Box<Expr>,
        field_def: &mut (FieldDef, Location),
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) {
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

        // TODO: For fields that are bare function references, take the result type as the `eval_type`

        // All dot expressions default to runtime evaluation
        *is_compile_eval = false;

        // Dealias left type
        let left_ref = types::dealias_ref(&left.get_eval_type(), &self.type_table);

        debug_assert!(
            types::is_base_type(&left_ref, &self.type_table),
            "Of type {:?}",
            left_ref
        );

        let field = &mut field_def.0;

        if types::is_error(&left_ref) {
            // Is a type error, silently propogate error
            field.type_spec = TypeRef::TypeError;
            *eval_type = TypeRef::TypeError;
            return;
        }

        if let Some(type_info) = self.type_table.type_from_ref(&left_ref) {
            // Match based on the type info
            match type_info {
                Type::Enum { fields, .. } => {
                    let enum_field = fields.get(&field.name);
                    // Check if the field is a part of the compound type
                    if let Some(field_ref) = enum_field {
                        // Link field type_spec & our type_spec to the field reference
                        field.type_spec = TypeRef::TypeError;
                        *eval_type = *field_ref;

                        // Update the field ident info
                        field.ref_kind = RefKind::Const; // Not mutable, not typedef

                        // Enum fields are compile-time evaluable
                        *is_compile_eval = true;
                    } else {
                        // Field name is not a part of the enum
                        field.type_spec = TypeRef::TypeError;
                        *eval_type = TypeRef::TypeError;

                        // Grabbing the identifier as the enum type name is a best-effort guess
                        self.reporter.borrow_mut().report_error(
                            &field_def.1,
                            format_args!(
                                "'{}' is not a field of the enum type '{}'",
                                field.name,
                                self.get_reference_ident(left)
                                    .map_or("<unknown>", |(name, ..)| name.as_str())
                            ),
                        );

                        // Not compile-time evaluable
                        *is_compile_eval = false;
                    }
                }
                Type::Pointer { .. } => {
                    // Not a compound type, special report (for using ->)
                    *eval_type = TypeRef::TypeError;
                    self.reporter.borrow_mut().report_error(
                        &left.get_span(),
                        format_args!(
                            "Left side of '.' is not a compound type (did you mean to use '->')"
                        ),
                    );
                }
                _ => {
                    // Not a compound type, produces a type error
                    *eval_type = TypeRef::TypeError;
                    self.reporter.borrow_mut().report_error(
                        &left.get_span(),
                        format_args!("Left side of '.' is not a compound type"),
                    );
                }
            }
        } else {
            // Not a compound type, produces a type error
            *eval_type = TypeRef::TypeError;
            self.reporter.borrow_mut().report_error(
                &left.get_span(),
                format_args!("Left side of '.' is not a compound type"),
            );
        }
    }

    pub(super) fn resolve_expr_arrow(
        &mut self,
        left: &mut Box<Expr>,
        field_def: &mut (FieldDef, Location),
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) {
        self.visit_expr(left);

        // Arrow expressions are not validated yet
        // TODO: Validate arrow expressions (blocking on compound types)
        self.reporter.borrow_mut().report_error(
            &field_def.1,
            format_args!("Arrow expressions are not validated yet"),
        );

        *eval_type = TypeRef::TypeError;
        *is_compile_eval = false;
    }

    pub(super) fn resolve_expr_reference(
        &mut self,
        ident: &mut IdentRef,
        eval_type: &mut TypeRef,
        is_compile_eval: &mut bool,
    ) {
        // Use the identifier and grab the associated value
        let compile_value = self.compile_values.get(&ident.id);
        let info = self.unit_scope.get_ident_info_mut(&ident.id);

        if !info.is_declared {
            // Identifier has not been declared at all before this point, report it
            // Only reported once everytime something is not declared
            self.reporter.borrow_mut().report_error(
                &ident.location,
                format_args!("'{}' has not been declared yet", info.name),
            );
        }

        if info.is_declared {
            if matches!(info.type_spec, TypeRef::Unknown) {
                // If the type is still unknown at this point, force it into a TypeError
                info.type_spec = TypeRef::TypeError;
            }

            // Fetch the eval type based on the ident type spec
            if info.ref_kind == RefKind::Type {
                // Take the type from the type spec
                *eval_type = info.type_spec;
            } else if let Some(Type::Function {
                params: None,
                result: Some(result),
            }) = self.type_table.type_from_ref(&info.type_spec)
            {
                // Evaluation type is the bare function's result type
                *eval_type = *result;
            } else {
                *eval_type = info.type_spec;
            }

            // An identifier is compile-time evaluable if and only if there is an associated expression
            info.is_compile_eval = compile_value.is_some();
            *is_compile_eval = compile_value.is_some();
        } else {
            // Not declared, don't touch the `type_spec` (preserves error checking "correctness")
            *eval_type = info.type_spec;
        }
    }

    pub(super) fn resolve_expr_literal(&mut self, _value: &mut Literal, eval_type: &mut TypeRef) {
        // Literal values already have the type resolved, unless the eval type is an IntNat
        if matches!(eval_type, TypeRef::Primitive(PrimitiveType::IntNat)) {
            // Force IntNats into Ints
            *eval_type = TypeRef::Primitive(PrimitiveType::Int);
        }
    }
}

/// Default type in a binary expression
fn binary_default(op: BinaryOp) -> TypeRef {
    match op {
        BinaryOp::Less
        | BinaryOp::Greater
        | BinaryOp::LessEq
        | BinaryOp::GreaterEq
        | BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::In
        | BinaryOp::NotIn
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::Imply => TypeRef::Primitive(PrimitiveType::Boolean),
        _ => TypeRef::TypeError,
    }
}

/// Default type in a unary expression
fn unary_default(op: UnaryOp) -> TypeRef {
    match op {
        UnaryOp::Not => TypeRef::Primitive(PrimitiveType::Boolean),
        UnaryOp::NatCheat => TypeRef::Primitive(PrimitiveType::Nat4),
        _ => TypeRef::TypeError,
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
    op: BinaryOp,
    right_type: &TypeRef,
    type_table: &TypeTable,
) -> Result<TypeRef, TypeRef> {
    // Remaining ordering comparisons require sets, enums, and objectclass types
    // Remaining equality comparisons require the above and full equivalence checking (includng pointers)

    debug_assert!(types::is_base_type(left_type, type_table));
    debug_assert!(types::is_base_type(right_type, type_table));

    match op {
        BinaryOp::Add => {
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
        BinaryOp::Sub | BinaryOp::Mul => {
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
        BinaryOp::RealDiv => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, real div
                return Ok(TypeRef::Primitive(PrimitiveType::Real));
            }
        }
        BinaryOp::Div => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, int div
                return Ok(TypeRef::Primitive(PrimitiveType::Int));
            }
        }
        BinaryOp::Mod | BinaryOp::Rem | BinaryOp::Exp => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number expr, mod, rem & exp
                return Ok(*types::common_type(left_type, right_type, type_table).unwrap());
            }
        }
        BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
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
        BinaryOp::Shl | BinaryOp::Shr => {
            // Valid conditions:
            // - Both types are integers (int, nat, etc)
            // Otherwise, TypeError is produced
            if types::is_integer_type(left_type) && types::is_integer_type(right_type) {
                // Integer expr, produce nat
                return Ok(TypeRef::Primitive(PrimitiveType::Nat));
            }
        }
        BinaryOp::Less | BinaryOp::LessEq | BinaryOp::Greater | BinaryOp::GreaterEq => {
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
        BinaryOp::Equal | BinaryOp::NotEqual => {
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
        BinaryOp::In | BinaryOp::NotIn => {
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
        BinaryOp::Imply => {
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
    op: UnaryOp,
    right_type: &TypeRef,
    type_table: &TypeTable,
) -> Result<TypeRef, TypeRef> {
    debug_assert!(types::is_base_type(right_type, &type_table));

    match op {
        UnaryOp::Not => {
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
        UnaryOp::NatCheat => {
            // Pound type cheat always forces the current operand into a nat4
            return Ok(TypeRef::Primitive(PrimitiveType::Nat4));
        }
        UnaryOp::Identity | UnaryOp::Negate => {
            // Valid conditions:
            // - Operand is a numeric (real, int, nat)
            // Otherwise, TypeError is produced (as an error)
            if types::is_number_type(right_type) {
                // Produce the same type as the operand
                return Ok(*right_type);
            }
        }
        UnaryOp::Deref => {
            // Valid conditions:
            // - Operand is a pointer type (produces the pointer's type)
            // Otherwise, TypeError is produced (as an error)
            if let Some(Type::Pointer { to, .. }) = type_table.type_from_ref(right_type) {
                // Produce the type pointed to by the type ref
                return Ok(*to);
            }
        }
    }

    Err(unary_default(op))
}
