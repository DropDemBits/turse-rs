//! Validator fragment, resolves all statements and declarations
use super::expr;
use super::{ResolveContext, Validator};

use toc_ast::ast::expr::{BinaryOp, Expr, ExprKind, UnaryOp};
use toc_ast::ast::ident::{IdentRef, RefKind};
use toc_ast::ast::stmt::{self, Stmt};
use toc_ast::ast::VisitorMut;
use toc_ast::types::{self, PrimitiveType, Type, TypeRef, TypeTable};
use toc_ast::value::Value;

impl Validator {
    // --- Decl Resolvers --- //

    pub(super) fn resolve_decl_var(
        &mut self,
        idents: &mut Option<Vec<IdentRef>>,
        type_spec: &mut TypeRef,
        value: &mut Option<Box<Expr>>,
        is_const: bool,
    ) {
        if idents.is_none() {
            // This is a dummy var declare, and only provides resolving access
            // Resolve the type, and return
            let dummy_ref = self.resolve_type(*type_spec, ResolveContext::CompileTime(false));
            *type_spec = dummy_ref;

            return;
        }

        let idents = idents.as_mut().unwrap();
        let mut is_compile_eval = false;

        if types::is_error(type_spec) {
            // The identifiers should all have matching values
            debug_assert_eq!(
                idents
                    .iter()
                    .filter(|ident| !types::is_error(
                        &self.unit_scope.get_ident_info(&ident.id).type_spec
                    ))
                    .count(),
                0
            );
        }

        // Visit the expression to update the eval type
        if value.is_some() {
            let expr = value.as_mut().unwrap();
            self.visit_expr(expr);
            let init_eval = self.eval_expr(expr).ok().flatten();

            // Try to replace the initializer value with the folded value
            super::replace_with_folded(expr, init_eval);

            is_compile_eval = expr.is_compile_eval();

            if self.is_type_reference(expr) {
                self.context.borrow_mut().reporter.report_error(
                    expr.get_span(),
                    format_args!("A type reference cannot be used as an initializer value"),
                );
                *value = None;
            }
        }

        // Resolve the identifier type spec or sized char sequence type spec, if possible
        if types::is_named(type_spec) || types::is_sized_char_seq_type(type_spec) {
            // Only required to be compile-time if the decl is a const decl, or if the type spec
            // is not directly an array
            let resolving_context = if is_const
                || !matches!(
                    self.type_table.type_from_ref(type_spec),
                    Some(Type::Array { .. })
                ) {
                ResolveContext::CompileTime(false)
            } else {
                ResolveContext::Any
            };

            *type_spec = self.resolve_type(*type_spec, resolving_context);

            // If the `type_spec` is a range, verify it is not a zero sized range
            if let Some(Type::Range {
                start, end, size, ..
            }) = self.type_table.type_from_ref(&type_spec)
            {
                // No guarrantess that end is a Some
                if end.is_none() {
                    // Error should be reported by parser
                    *type_spec = TypeRef::TypeError;
                } else if *size == Some(0) {
                    // Zero sized ranges aren't allowed in variable/constant range types
                    let range_span = start.get_span().span_to(end.as_ref().unwrap().get_span());
                    self.context.borrow_mut().reporter.report_error(
                        &range_span,
                        format_args!("Range bounds creates a zero-sized range"),
                    );
                    *type_spec = TypeRef::TypeError;
                }
            }
        }

        // Handle the type spec propogation
        if *type_spec == TypeRef::Unknown {
            // Unknown type, use the type of the expr
            // Safe to unwrap as if no expr was provided, the type_spec would be TypeError

            let expr = value.as_ref().unwrap();
            *type_spec = expr.get_eval_type();

            if types::is_intnat(type_spec) {
                // Always convert IntNats into Ints
                // (larger sizes are automatically converted into the appropriate type)
                *type_spec = TypeRef::Primitive(PrimitiveType::Int);
            }
        } else if value.is_some() {
            // Type of the identifier is known, validate that the types are assignable
            let expr = value.as_ref().unwrap();

            let left_type = &self.dealias_resolve_type(*type_spec);
            let right_type = &self.dealias_resolve_type(expr.get_eval_type());

            // If both of the types are not an error, check for assignability
            if !types::is_error(left_type) && !types::is_error(right_type) {
                debug_assert!(
                    types::is_base_type(left_type, &self.type_table),
                    "Of type {:?}",
                    left_type
                );
                debug_assert!(
                    types::is_base_type(right_type, &self.type_table),
                    "Of type {:?}",
                    right_type
                );

                // Validate that the types are assignable
                if !types::is_assignable_to(&left_type, &right_type, &self.type_table) {
                    // Value to assign is the wrong type, just report the error
                    self.context.borrow_mut().reporter.report_error(
                        &expr.get_span(),
                        format_args!("Initialization value is the wrong type"),
                    );
                } else {
                    // Update compile-time evaluability status
                    is_compile_eval = value.as_ref().unwrap().is_compile_eval();
                }
            }
        }
        // Variable declarations with no assignment value will have the type already given

        // If value is an init expression, verify compatibility
        if !types::is_error(type_spec) {
            if let Some(expr) = value {
                self.check_init_value_in_initializer(expr, type_spec);
            }
        }

        // Grab the compile-time value
        let const_val = if is_compile_eval && is_const {
            // Create a value to clone from
            let value = Value::from_expr(*value.as_ref().unwrap().clone(), &self.type_table)
                .unwrap_or_else(|msg| {
                    panic!(
                        "Initializer value '{:?}' is not a compile-time expression ({})",
                        value, msg
                    )
                });
            Some(value)
        } else {
            // No compile-time value is produced
            None
        };

        // Update the identifiers to the new identifier type
        for ident in idents.iter_mut() {
            let info = self.unit_scope.get_ident_info_mut(&ident.id);
            info.type_spec = *type_spec;

            // Only compile-time evaluable if the identifier referencences a constant
            info.is_compile_eval = is_compile_eval && info.ref_kind == RefKind::Const;
            // Post compile time value
            if let Some(const_val) = &const_val {
                self.compile_values.insert(ident.id, const_val.clone());
            }
        }
    }

    /// Check compatibility with initializer and `type_spec`
    fn check_init_value_in_initializer(&self, expr: &Expr, type_spec: &TypeRef) {
        if let ExprKind::Init { init, exprs, .. } = &expr.kind {
            // Check if the type can accept the "init"
            // Only valid for arrays, records, and unions
            let mut field_types = if let Some(type_info) = self.type_table.type_from_ref(type_spec)
            {
                match type_info {
                    Type::Array {
                        ranges,
                        element_type,
                        is_init_sized,
                        is_flexible,
                    } => {
                        // `did_overflow` indicates it was capped at usize::MAX
                        let (elem_count, did_overflow) =
                            types::get_array_element_count(ranges, &self.type_table);

                        if ranges.is_empty() {
                            // No ranges on the array, error reported by the parser
                            None
                        } else if *is_flexible {
                            self.context.borrow_mut().reporter.report_error(
                                init,
                                format_args!(
                                    "'init' initializers are not allowed for flexible arrays"
                                ),
                            );
                            None
                        } else if elem_count == 0 && !is_init_sized {
                            // We know it to be dynamic, as one of the ranges isn't a compile-time expression and it isn't a flexible array
                            self.context.borrow_mut().reporter.report_error(
                                init,
                                format_args!(
                                    "'init' initializers are not allowed for dynamic arrays"
                                ),
                            );
                            None
                        } else if did_overflow {
                            // Array has more elements than can be handled
                            // Definitely an error (stop yourself, for your own sake)
                            self.context.borrow_mut().reporter.report_error(init, format_args!("'init' has more initializer values than can be represented by a machine-size integer"));
                            None
                        } else if *is_init_sized {
                            // Match type count with init size
                            Some(std::iter::once(element_type).cycle().take(exprs.len()))
                        } else {
                            // Build type iter on array count sizes
                            Some(std::iter::once(element_type).cycle().take(elem_count))
                        }
                    }
                    _ => {
                        // Not the requested type
                        None
                    }
                }
            } else {
                // Nope!
                None
            };

            // If a none, errors are already produced
            if let Some(field_types) = field_types.as_mut() {
                // Iterate over the init types
                let mut init_types = exprs.iter().map(|e| (e, e.get_eval_type()));
                let mut has_fields_remaining = false;

                for field_type in field_types {
                    let init_field = init_types.next();

                    if init_field.is_none() {
                        has_fields_remaining = true;
                        // None left
                        break;
                    }

                    let (init_expr, init_type) = init_field.unwrap();

                    if !matches!(init_expr.kind, ExprKind::Error)
                        && !types::is_assignable_to(field_type, &init_type, &self.type_table)
                    {
                        // Wrong types (skipping over error expressions as those are produced by the parser)
                        // ???: Report field name for records?
                        self.context.borrow_mut().reporter.report_error(
                            init_expr.get_span(),
                            format_args!("Initializer value evaluates to the wrong type"),
                        );
                    }
                }

                // Check if there are any remaining
                let next_init = init_types.next();

                match next_init {
                    Some((next_expr, _)) if !has_fields_remaining => {
                        // Too many init fields
                        let report_at = next_expr.get_span();

                        self.context
                            .borrow_mut()
                            .reporter
                            .report_error(report_at, format_args!("Too many initializer values"));
                    }
                    None if has_fields_remaining => {
                        // Too few init
                        // If empty, report at init
                        let report_at = exprs.last().map_or(init, |expr| expr.get_span());

                        // ???: Report field name for records?
                        // ???: Report missing count for arrays?
                        self.context
                            .borrow_mut()
                            .reporter
                            .report_error(report_at, format_args!("Too few initializer values"));
                    }
                    _ => {}
                }
            }
        }
    }

    pub(super) fn resolve_decl_type(
        &mut self,
        ident: &mut Option<IdentRef>,
        resolved_type: &mut Option<TypeRef>,
        is_new_def: bool,
    ) {
        if ident.is_none() {
            // This is a dummy type declare, and only provides resolving access
            // Resolve the type, and return
            let dummy_ref = resolved_type.take().unwrap();
            let dummy_ref = self.resolve_type(dummy_ref, ResolveContext::CompileTime(false));
            resolved_type.replace(dummy_ref);

            return;
        }

        let ident = ident.as_mut().unwrap();
        let info = self.unit_scope.get_ident_info(&ident.id);

        if is_new_def {
            if resolved_type.is_some() {
                // Resolve the associated type (do not allow forward references)
                let ty_spec = info.type_spec;
                let ty_spec = self.resolve_type(ty_spec, ResolveContext::CompileTime(false));
                let info = self.unit_scope.get_ident_info_mut(&ident.id);
                info.type_spec = ty_spec;
            } else if let Some(Type::Forward { is_resolved: false }) =
                self.type_table.type_from_ref(&info.type_spec)
            {
                // Not resolved in the current unit
                self.context.borrow_mut().reporter.report_error(
                    &ident.location,
                    format_args!("'{}' is not resolved in the current unit", info.name),
                );
            }
        } else {
            // Use the identifier
            // Must be defined
            assert!(info.is_declared);

            if let Some(resolve_ref) = resolved_type {
                // This is a type resolution statement, update the associated type reference
                if let TypeRef::Named(replace_id) = info.type_spec {
                    // Make an alias to the resolved type
                    self.type_table
                        .replace_type(replace_id, Type::Alias { to: *resolve_ref });
                }

                // Resolve the rest of the type
                let ty_spec = info.type_spec;
                let ty_spec = self.resolve_type(ty_spec, ResolveContext::CompileTime(false));

                // Mutate identifier
                let info = self.unit_scope.get_ident_info_mut(&ident.id);
                info.type_spec = ty_spec;
            } else {
                // This is a redeclared forward, and is safe to ignore
            }
        }
    }

    // --- Stmt Resolvers --- //

    pub(super) fn resolve_stmt_assign(
        &mut self,
        var_ref: &mut Box<Expr>,
        op: Option<&mut BinaryOp>,
        value: &mut Box<Expr>,
    ) {
        self.visit_expr(var_ref);
        self.visit_expr(value);

        // Try to replace the operands with the folded values
        let ref_eval = self.eval_expr(var_ref).ok().flatten();
        let value_eval = self.eval_expr(value).ok().flatten();
        super::replace_with_folded(var_ref, ref_eval);
        super::replace_with_folded(value, value_eval);

        // Resolve types first
        let left_type = &self.dealias_resolve_type(var_ref.get_eval_type());
        let right_type = &self.dealias_resolve_type(value.get_eval_type());

        // Validate that the types are assignable for the given operation
        if types::is_error(left_type) || types::is_error(right_type) {
            // Silently drop propogated TypeErrors
            return;
        }

        // Check the reference expression
        if !self.can_assign_to_ref_expr(&var_ref, &self.type_table) {
            // Not a var ref
            self.context.borrow_mut().reporter.report_error(var_ref.get_span(), format_args!("Left side of assignment does not reference a variable and cannot be assigned to"));
            return;
        }

        debug_assert!(types::is_base_type(left_type, &self.type_table));
        debug_assert!(types::is_base_type(right_type, &self.type_table));

        let is_valid_assignment = if let Some(op) = op {
            let produce_type =
                expr::check_binary_operands(left_type, *op, right_type, &self.type_table);

            // Assignment is ok if a type is produced & compatible with the return value
            produce_type.is_ok()
                && types::is_assignable_to(left_type, &produce_type.unwrap(), &self.type_table)
        } else {
            // Assignment is only valid if types are compatible
            types::is_assignable_to(left_type, right_type, &self.type_table)
        };

        if !is_valid_assignment {
            // Value to assign is the wrong type
            if self.is_type_reference(value) {
                self.context.borrow_mut().reporter.report_error(
                    &value.get_span(),
                    format_args!("Expression is a type reference, and cannot be used here"),
                );
            } else {
                self.context.borrow_mut().reporter.report_error(
                    &value.get_span(),
                    format_args!("Assignment value is the wrong type"),
                );
            }
        }
    }

    pub(super) fn resolve_stmt_block(&mut self, block: &mut stmt::Block) {
        // Report unused identifiers
        self.report_unused_identifiers(&block.block);
        // Report redeclared identifiers
        self.report_redeclared_identifiers(&block.block);

        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
    }

    pub(super) fn resolve_stmt_if(
        &mut self,
        condition: &mut Box<Expr>,
        true_branch: &mut Box<Stmt>,
        false_branch: &mut Option<Box<Stmt>>,
    ) {
        // Visit conditional
        self.visit_expr(condition);

        // Optionally fold
        let condition_val = self.eval_expr(condition).ok().flatten();
        super::replace_with_folded(condition, condition_val);

        // Then `true_branch`
        self.visit_stmt(true_branch);

        // Then `false_branch`
        if let Some(block) = false_branch.as_mut() {
            self.visit_stmt(block);
        }

        // Typecheck the condition
        let dealias_eval = types::dealias_ref(&condition.get_eval_type(), &self.type_table);
        if !types::is_boolean(&dealias_eval) {
            self.context.borrow_mut().reporter.report_error(
                &condition.get_span(),
                format_args!("If condition expression is not the right type"),
            );
        }
    }

    /// Checks if the given `ref_expr` references a variable or a mutable reference.
    /// Assumes the `ref_expr` has already had the types propogated.
    fn can_assign_to_ref_expr(&self, ref_expr: &Expr, type_table: &TypeTable) -> bool {
        match &ref_expr.kind {
            ExprKind::Reference { ident, .. } => {
                let info = self.unit_scope.get_ident_info(&ident.id);
                // Can only assign to a variable reference
                info.ref_kind == RefKind::Var
            }
            ExprKind::Dot {
                field: (field, _), ..
            } => {
                // For now, we don't have compound types (with or without type reference), so assume they are not var ref exprs
                assert_ne!(
                    field.ref_kind,
                    RefKind::Type,
                    "No compound types with type refs exist yet"
                );
                false
            }
            ExprKind::Call { left, .. } => {
                match &left.kind {
                    ExprKind::Reference { ident, .. } => {
                        let info = self.unit_scope.get_ident_info(&ident.id);
                        let dealiased_ref = types::dealias_ref(&info.type_spec, type_table);
                        let type_info = type_table.type_from_ref(&dealiased_ref);

                        // Can only assign if the ref is to a var array variable
                        // Can't assign to a const ref or a type def
                        info.ref_kind == RefKind::Var
                            && matches!(type_info, Some(Type::Array { .. }))
                    }
                    ExprKind::Dot { field: _ident, .. } => {
                        // Same reasoning as beforehand
                        false
                    }
                    _ => self.can_assign_to_ref_expr(&left, type_table), // Go further down the chain
                }
            }
            ExprKind::UnaryOp { op, .. } => {
                // Only assignable if the expression is a deref, and the eval type isn't an error
                matches!(op.0, UnaryOp::Deref) && !types::is_error(&ref_expr.get_eval_type())
            }
            ExprKind::Indirect { .. } => true, // Can always assign to an indirect expression
            _ => false,                        // Not one of the above, likely unable to assign to
        }
    }
}
