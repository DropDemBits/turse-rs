//! Validator fragment, resolves all statements and declarations
use super::expr_resolve;
use super::{ResolveContext, ScopeInfo, Validator};

use crate::compiler::ast::{Expr, Identifier, Stmt, VisitorMut};
use crate::compiler::block::CodeBlock;
use crate::compiler::frontend::token::TokenType;
use crate::compiler::types::{self, PrimitiveType, Type, TypeRef, TypeTable};
use crate::compiler::value::Value;
use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

impl Validator {
    // --- Decl Resolvers --- //

    pub(super) fn resolve_decl_var(
        &mut self,
        idents: &mut Vec<Identifier>,
        type_spec: &mut TypeRef,
        value: &mut Option<Box<Expr>>,
        is_const: bool,
    ) {
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
        }

        // Visit the expression to update the eval type
        if value.is_some() {
            let expr = value.as_mut().unwrap();
            let init_eval = self.visit_expr(expr);

            // Try to replace the initializer value with the folded value
            if init_eval.is_some() {
                let span = expr.get_span().clone();
                *expr = Box::new(Expr::try_from(init_eval.unwrap()).unwrap());
                expr.set_span(span);
            }

            is_compile_eval = expr.is_compile_eval();
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
            if let Some(Type::Range { start, end, .. }) = self.type_table.type_from_ref(&type_spec)
            {
                // No guarrantess that end is a Some
                if end.is_none() {
                    // Error should be reported by parser
                    *type_spec = TypeRef::TypeError;
                } else if !super::type_resolve::validate_range_size(
                    &start,
                    &end.as_ref().unwrap(),
                    &self.type_table,
                    false,
                ) {
                    // Zero sized ranges aren't allowed in variable/constant range types
                    let range_span = start.get_span().span_to(end.as_ref().unwrap().get_span());
                    self.reporter.report_error(
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
                    self.reporter.report_error(
                        &idents.last().as_ref().unwrap().token.location,
                        format_args!("Initialization value is the wrong type"),
                    );
                } else {
                    // Update compile-time evaluability status
                    is_compile_eval = value.as_ref().unwrap().is_compile_eval();
                }
            }
        }
        // Variable declarations with no assignment value will have the type already given

        // Grab the compile-time value
        let const_val = if is_compile_eval && is_const {
            // Create a value to clone from
            let value = Value::from_expr(*value.as_ref().unwrap().clone(), &self.type_table)
                .expect(&format!(
                    "Initializer value '{:?}' is not a compile-time expression",
                    value
                ));
            Some(value)
        } else {
            // No compile-time value is produced
            None
        };

        // Update the identifiers to the new identifier type
        for ident in idents.iter_mut() {
            ident.type_spec = *type_spec;
            // Only compile-time evaluable if the identifier referencences a constant
            ident.is_compile_eval = is_compile_eval && ident.is_const;
            self.active_block
                .as_ref()
                .unwrap()
                .upgrade()
                .unwrap()
                .borrow_mut()
                .scope
                .resolve_ident(&ident.name, &ident);

            // Add identifier to the scope info (including the compile-time value)
            if self
                .scope_infos
                .last_mut()
                .unwrap()
                .decl_ident_with(ident.clone(), const_val.clone())
            {
                // Report the error
                self.reporter.report_error(
                    &ident.token.location,
                    format_args!("'{}' has already been declared", ident.name),
                );
            }
        }
    }

    pub(super) fn resolve_decl_type(
        &mut self,
        ident: &mut Identifier,
        resolved_type: &mut Option<TypeRef>,
        is_new_def: bool,
    ) {
        if is_new_def {
            if resolved_type.is_some() {
                // Resolve the associated type (do not allow forward references)
                ident.type_spec =
                    self.resolve_type(ident.type_spec, ResolveContext::CompileTime(false));
            }

            // Declare the identifier and check for redeclaration errors
            if self
                .scope_infos
                .last_mut()
                .unwrap()
                .decl_ident(ident.clone())
            {
                self.reporter.report_error(
                    &ident.token.location,
                    format_args!("'{}' has already been declared", ident.name),
                );
            }
        } else {
            // Use the identifier
            // Must be defined
            let is_defined = self.scope_infos.last_mut().unwrap().use_ident(&ident).1;
            assert!(is_defined);

            if let Some(resolve_ref) = resolved_type {
                // This is a type resolution statement, update the associated type reference
                if let TypeRef::Named(replace_id) = ident.type_spec {
                    // Make an alias to the resolved type
                    self.type_table
                        .replace_type(replace_id, Type::Alias { to: *resolve_ref });
                }

                // Resolve the rest of the type
                ident.type_spec =
                    self.resolve_type(ident.type_spec, ResolveContext::CompileTime(false));
            } else {
                // This is a redeclared forward, and is safe to ignore
            }
        }
    }

    // --- Stmt Resolvers --- //

    pub(super) fn resolve_stmt_assign(
        &mut self,
        var_ref: &mut Box<Expr>,
        op: &TokenType,
        value: &mut Box<Expr>,
    ) {
        let ref_eval = self.visit_expr(var_ref);
        let value_eval = self.visit_expr(value);

        // Try to replace the operands with the folded values
        if ref_eval.is_some() {
            let span = var_ref.get_span().clone();
            *var_ref = Box::new(Expr::try_from(ref_eval.unwrap()).unwrap());
            var_ref.set_span(span);
        }
        if value_eval.is_some() {
            let span = value.get_span().clone();
            *value = Box::new(Expr::try_from(value_eval.unwrap()).unwrap());
            value.set_span(span);
        }

        // Check the reference expression
        if !can_assign_to_ref_expr(&var_ref, &self.type_table) {
            // Not a var ref
            self.reporter.report_error(var_ref.get_span(), format_args!("Left side of assignment does not reference a variable and cannot be assigned to"));
            return;
        }

        let left_type = &self.dealias_resolve_type(var_ref.get_eval_type());
        let right_type = &self.dealias_resolve_type(value.get_eval_type());

        // Validate that the types are assignable for the given operation
        if types::is_error(left_type) || types::is_error(right_type) {
            // Silently drop propogated TypeErrors
            return;
        }

        debug_assert!(types::is_base_type(left_type, &self.type_table));
        debug_assert!(types::is_base_type(right_type, &self.type_table));

        if *op == TokenType::Assign {
            if !types::is_assignable_to(left_type, right_type, &self.type_table) {
                // Value to assign is the wrong type
                self.reporter.report_error(
                    &value.get_span(),
                    format_args!("Assignment value is the wrong type"),
                );
            }
        } else {
            let produce_type =
                expr_resolve::check_binary_operands(left_type, op, right_type, &self.type_table);
            if produce_type.is_err()
                || !types::is_assignable_to(left_type, &produce_type.unwrap(), &self.type_table)
            {
                // Value to assign is the wrong type
                self.reporter.report_error(
                    &value.get_span(),
                    format_args!("Assignment value is the wrong type"),
                );
            }
        }
    }

    pub(super) fn resolve_stmt_block(
        &mut self,
        block: &Rc<RefCell<CodeBlock>>,
        stmts: &mut Vec<Stmt>,
    ) {
        let mut scope_info = ScopeInfo::new();

        // Import all of the identifiers from above scopes
        // Don't need to worry about the "pervasive" import attribute,
        // as that is handled by the parser
        // An identifier is only in the import table if and only if it
        // has been used
        {
            // Drop the scope ref after importing everything
            let scope = &block.borrow().scope;

            for import in scope.import_table() {
                // Fetch ident from the new scope
                let ident = scope
                    .get_ident_instance(&import.name, 0)
                    .expect("Import does not have a corresponding identifier entry");

                // Build the imported identifier
                let mut imported_ident = ident.clone();
                imported_ident.instance = import.instance;

                // Get info from the imported scope info
                let (compile_value, is_declared) =
                    self.scope_infos[import.downscopes].use_ident(&imported_ident);
                assert!(is_declared, "Imported identifier was never declared");

                // Import into the new scope info
                assert!(
                    !scope_info.decl_ident_with(ident.clone(), compile_value),
                    "Duplicate import identifier?"
                );
            }
        }

        // Change the active block and push the new scope info
        let previous_scope = self.active_block.replace(Rc::downgrade(block));
        self.scope_infos.push(scope_info);

        for stmt in stmts.iter_mut() {
            self.visit_stmt(stmt);
        }

        // Revert to previous scope and pop the last scope info
        self.active_block.replace(previous_scope.unwrap());
        self.scope_infos.pop();
    }
}

/// Checks if the given `ref_expr` references a variable or a mutable reference.
/// Assumes the `ref_expr` has already had the types propogated.
fn can_assign_to_ref_expr(ref_expr: &Expr, type_table: &TypeTable) -> bool {
    match ref_expr {
        Expr::Reference { ident } | Expr::Dot { field: ident, .. } => {
            // Can only assign to a variable reference
            !ident.is_const && !ident.is_typedef
        }
        Expr::Call { left, .. } => {
            match &**left {
                Expr::Reference { ident } | Expr::Dot { field: ident, .. } => {
                    let dealiased_ref = types::dealias_ref(&ident.type_spec, type_table);
                    let type_info = type_table.type_from_ref(&dealiased_ref);

                    // Can only assign if the ref is to an array variable
                    // Can't assign to a const ref or a type def
                    !ident.is_const
                        && !ident.is_typedef
                        && matches!(type_info, Some(Type::Array { .. }))
                }
                _ => can_assign_to_ref_expr(&left, type_table), // Go further down the chain
            }
        }
        Expr::UnaryOp { op, eval_type, .. } => {
            // Only assignable if the expression is a deref, and the eval type isn't an error
            matches!(op.token_type, TokenType::Caret) && !types::is_error(eval_type)
        }
        _ => false, // Not one of the above, likely unable to assign to
    }
}