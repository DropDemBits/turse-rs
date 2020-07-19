//! Type validator upon the AST tree
//! Performs the majority of the semantic validation pass
//! - Propogates and checks expressions for type correctness
//! - Resolves identifiers into their final types
//! - Validates and resolves types into their final forms
//! - Checks and evaluates compile-time expressions
//!
//! Types are resolved before the expression that use them are visited by only
//! resolving types in declaration statements
use crate::compiler::ast::{ASTVisitorMut, Identifier, Expr, Stmt};
use crate::compiler::block::CodeBlock;
use crate::compiler::token::TokenType;
use crate::compiler::types::{self, PrimitiveType, Type, TypeRef, TypeTable};
use crate::compiler::value::{self, Value};
use crate::status_reporter::StatusReporter;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::convert::TryFrom;
use std::collections::HashMap;

// An identifier info entry associated with one instance of an identifier
struct IdentInfo {
    /// The identifier associated with this info
    ident: Identifier,
    /// How many uses (for the current instance) of the identfier
    uses: usize,
    /// The associated compile-time value of the identifier, if it
    /// is compile-time evaluable
    compile_value: Option<Value>,
}

/// Info for a scope
struct ScopeInfo {
    /// All identifiers used within the scope,
    /// with all identifier name conflicts tracked
    local_idents: HashMap<String, Vec<IdentInfo>>,
}

impl ScopeInfo {
    pub fn new() -> Self {
        Self {
            local_idents: HashMap::new()
        }
    }

    /// Uses an identifier declared in the current scope.
    /// Any imported identifiers in the current scope must refer to the ScopeInfo
    /// where the imported identifier is declared in.
    /// 
    /// `Option<Value>`     Associated compile-time value
    /// `bool`              True if the identifier was never declared
    pub fn use_ident(&mut self, ident: &Identifier) -> (Option<Value>, bool) {
        let mut is_never_declared = false;

        // Grab the compile-time value
        let compile_value = self.local_idents.entry(ident.name.clone()).and_modify(|all_idents| {
            // Increment use of the associated identifier
            // The given instance must exist already, otherwise a declaration
            // was skipped or the instance counter is not being incremented properly
            let entry = &mut all_idents[ident.instance as usize];
            entry.uses = entry.uses.saturating_add(1);
        }).or_insert_with(|| {
            // Notify of the identifer never being declared
            is_never_declared = true;

            // Make sure this is the first time we're seeing this identifer
            assert_eq!(ident.instance, 0, "Not the first time seeing the identifier");

            // Create a new identifier info group
            vec![IdentInfo {
                ident: ident.clone(),
                uses: 1,
                compile_value: None,
            }]
        })[ident.instance as usize].compile_value.clone();

        (compile_value, is_never_declared)
    }

    /// Declares an identifier, creating the associated IdentInfo entry.
    ///
    /// `bool`  If the current declaration is redeclaring an identifier
    pub fn decl_ident(&mut self, ident: Identifier) -> bool {
        self.decl_ident_with(ident, None)
    }

    /// Declares an identifier, creating the associated IdentInfo entry and
    /// associating a compile-time value with the identifier.
    ///
    /// `bool`  If the current declaration is redeclaring an identifier
    pub fn decl_ident_with(&mut self, ident: Identifier, compile_value: Option<Value>)  -> bool {
        let mut is_redeclare = false;

        self.local_idents.entry(ident.name.clone()).and_modify(|_| {
            // Notify of redeclare
            is_redeclare = true
        }).or_insert_with(|| {
            if ident.instance == 0 {
                // Give back an empty vec, populated on next insert
                vec![]
            } else {
                // Ensure that this usage is the first of the instance
                assert_eq!(ident.instance, 1);

                // Insert a dummy info entry into the returned vec
                let mut dummy_ident = ident.clone();
                dummy_ident.name = String::from("<not a real entry>");
                dummy_ident.is_declared = false;
                dummy_ident.type_spec = TypeRef::TypeError;
                dummy_ident.instance = 0;

                vec![IdentInfo {
                    ident: dummy_ident,
                    uses: 0,
                    compile_value: None,
                }]
            }
        }).push(IdentInfo {
            ident: ident.clone(),
            uses: 0,
            compile_value,
        });

        is_redeclare
    }
}

/// Type resolving context
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum ResolveContext {
    /// Any resolution phase (runtime/compile-time) is valid
    Any,
    /// Everything must be resolved at compile time
    /// `bool` is for whether 'forward' unnamed types is allowed
    CompileTime(bool),
}

/// Validator Instance
pub struct Validator<'a> {
    /// Status reporter for the type validator
    reporter: StatusReporter,
    /// Type table to use
    type_table: &'a mut TypeTable,
    /// Actively parsed scope
    active_scope: Weak<RefCell<CodeBlock>>,
    /// Associated scope info
    scope_infos: Vec<ScopeInfo>,
}

impl<'a> Validator<'a> {
    pub fn new(root_block: &Rc<RefCell<CodeBlock>>, type_table: &'a mut TypeTable) -> Self {
        Self {
            reporter: StatusReporter::new(),
            type_table,
            active_scope: Rc::downgrade(root_block),
            scope_infos: vec![ScopeInfo::new()]
        }
    }

    /// Resolves the given type, validating that the type is a valid type
    /// Returns the resolved typedef
    fn resolve_type(&mut self, base_ref: TypeRef, resolving_context: ResolveContext) -> TypeRef {
        if !types::is_named(&base_ref) {
            // Not a named ref, no resolving needs to be done
            return base_ref;
        }

        let type_id = if let TypeRef::Named(id) = base_ref {
            id
        } else {
            unreachable!()
        };

        // Clone is used to appease the borrow checker so that `self` and
        // `self.type_table` aren't borrowed, allowing nested exprs
        let mut type_info = self.type_table.get_type(type_id).clone();

        match &mut type_info {
            Type::Alias { to } => {
                if types::is_error(to) {
                    // Apply type error to `to`
                    *to = TypeRef::TypeError;
                } else {
                    // Resolve the `to`
                    *to = self.resolve_type(*to, resolving_context);
                }
            }
            Type::Array { ranges, element_type, ..} => {
                // Resolve the ranges
                for range in ranges.iter_mut() {
                    // Not required to be compile-time, unless we are in a compile-time context
                    *range = self.resolve_type(*range, resolving_context);
                }

                // Resolve the element type
                // Required to be compile-time as the element size must be known
                *element_type = self.resolve_type(*element_type, ResolveContext::CompileTime(false));
            }
            Type::Forward { is_resolved } => {
                if !*is_resolved {
                    // Type has not been resolved in the unit
                    // Replace with a type error
                    return TypeRef::TypeError;
                }

                // Type has been resolved in the unit, but will be replaced with the real type later
            }
            Type::Function { params, result } => {
                // Resolve each of the parameters
                if params.is_some() {
                    for param in params.as_mut().unwrap().iter_mut() {
                        param.type_spec = self.resolve_type(param.type_spec, ResolveContext::CompileTime(false));
                    }
                }

                // Resolve the result type
                if result.is_some() {
                    *result = Some(self.resolve_type(result.unwrap(), ResolveContext::CompileTime(false)));
                }
            }
            Type::Pointer { to } => {
                // Resolve the 'to' type (allow forward references)
                *to = self.resolve_type(*to, ResolveContext::CompileTime(true));
            }
            Type::Range { start, end, base_type } => {
                // Visit & fold the bound expressions
                if let Some(value) = self.visit_expr(start) {
                    *start = Expr::try_from(value).expect("Cannot convert start bound into a value");
                }

                if end.is_some() {
                    if let Some(value) = self.visit_expr(end.as_mut().unwrap()) {
                        *end = Some(Expr::try_from(value).expect("Cannot convert end bound into a value"));
                    }
                }

                if !start.is_compile_eval() {
                    // The start range must be a compile-time expression
                    // Span over the start bound
                    self.reporter.report_error(start.get_span(), format_args!("Start bound must be a compile-time expression"));

                    // Produce a type error as this is not a valid expression
                    return TypeRef::TypeError;
                }

                if matches!(resolving_context, ResolveContext::CompileTime(_)) {
                    // All type info must be known at compile-time

                    // Validate that the range type ref references a range that
                    // has the end bound as a compile-time expression
                    // Don't need to worry about checking * (checked by the parser)
                    if let Some(end) = end {
                        if !end.is_compile_eval() {
                            // Right-hand side is not a compile-time expression
                            // Span over the end bound
                            self.reporter.report_error(end.get_span(), format_args!("End bound must be a compile-time expression"));

                            // Range is not a valid type
                            return TypeRef::TypeError;
                        }
                    }
                }

                // Try to derive a base copy from the given types
                let start_type = start.get_eval_type();
                let end_type = if end.is_some() {
                    end.as_ref().unwrap().get_eval_type()
                } else {
                    // No specified end range, use the start type
                    start_type
                };

                if !types::is_equivalent_to(&start_type, &end_type, &self.type_table) {
                    // Range eval types do not match
                    // Span the entire range
                    let span = if end.is_some() {
                        start.get_span().span_to(end.as_ref().unwrap().get_span())
                    } else {
                        start.get_span().clone()
                    };

                    self.reporter.report_error(&span, format_args!("Range bounds must be both integers, characters, booleans, or elements from the same enumeration"));

                    return TypeRef::TypeError;
                }

                // Update the base type
                // Either `start_type` or `end_type` could be used
                *base_type = self.dealias_resolve_type(start_type);
            }
            Type::Reference { expr } => {
                // Reference will produce a reference to the associated type_spec
                // If there is no reference to a type, a TypeError is produced

                // Evaluate expression
                self.visit_expr(expr);

                // Error reporting purposes
                let reference_locate;
                
                // Ensure that the top-most expression resolves to a type
                match &**expr {
                    Expr::Dot { field: (token, name, is_typedef), .. } => {
                        if !is_typedef {
                            self.reporter.report_error(&token.location, format_args!("Field '{}' is not a reference to a type", name));

                            // Produce a type error
                            return TypeRef::TypeError;
                        }

                        reference_locate = token.location.clone();
                    }
                    Expr::Reference { ident } => {
                        if !ident.is_typedef {
                            self.reporter.report_error(&ident.token.location, format_args!("'{}' is not a reference to a type", ident.name));

                            // Produce a type error
                            return TypeRef::TypeError;
                        }

                        reference_locate = ident.token.location.clone();
                    }
                    _ => unreachable!(), // No other expressions allowed, handled by the parser
                }

                // Check if the eval type de-aliases to a forward
                let type_ref = self.dealias_resolve_type(expr.get_eval_type());

                if let Some(Type::Forward { is_resolved }) = self.type_table.type_from_ref(&type_ref) {
                    if !*is_resolved {
                        // The type is not resolved at all, replace with TypeError
                        self.reporter.report_error(&reference_locate, format_args!("Type reference is not resolved in the current unit"));
                        return TypeRef::TypeError;
                    } else if matches!(resolving_context, ResolveContext::CompileTime(false)) {
                        // The type ref is required to be resolved at this point, replace with TypeError
                        self.reporter.report_error(&reference_locate, format_args!("Type reference is required to be resolved at this point"));
                        return TypeRef::TypeError;
                    }
                }

                // Produce the resolved type
                return type_ref;
            }
            Type::Set { range: index } => {
                // Keep track of the old type ref for error reporting
                let old_index_ref = *index;

                // Doesn't matter if the range is a type error or not, will be
                // ignored during equivalence checking
                *index = self.resolve_type(*index, ResolveContext::CompileTime(false));

                if types::is_named(&index) {
                    // Check that the index reference is actually an index type and not a reference to a non-index type
                    // Other cases are handled by the parser
                    let real_index = self.dealias_resolve_type(*index);

                    if !types::is_index_type(&real_index, self.type_table) {
                        // Not a real index type, change it to point to a type error
                        *index = TypeRef::TypeError;

                        // Report the error based on the reference location
                        if let Some(Type::Reference { expr }) = self.type_table.type_from_ref(&old_index_ref) {
                            self.reporter.report_error(expr.get_span(), format_args!("Set index is not a range, char, boolean, or enumerated type"));
                        } else {
                            // Other cases should be reported by the parser (e.g. wrong primitive type)
                        }
                    }
                } else if types::is_primitive(&index) && types::is_index_type(&index, self.type_table) {
                    // Is a primitive, either a 'char' or 'boolean'
                    // Keep as is
                } else {
                    // Ensure that the range is really a type error
                    // Don't need to report, as it is covered by a previous error
                    *index = TypeRef::TypeError;
                }
            }
        }

        // Replace the type with the updated type
        self.type_table.replace_type(type_id, type_info);
        return base_ref;
    }

    /// De-aliases a type ref, following through Alias types and resolving Reference types.
    fn dealias_resolve_type(&mut self, type_ref: TypeRef) -> TypeRef {
        let type_id = if let Some(id) = types::get_type_id(&type_ref) {
            id
        } else {
            // Non-named types don't need to be dealiased (already at the base type)
            return type_ref;
        };

        if !self.type_table.is_indirect_alias(type_id) {
            // Any compound types that aren't Alias or Reference do not need to be
            // dealiased (already pointing to the base types)
            return type_ref;
        }

        // Type is either an alias, or a reference (to resolve)
        // Walk the alias tree
        let mut current_ref = type_ref;

        if matches!(self.type_table.get_type(type_id), Type::Reference { .. }) {
            // Resolve an immediate reference
            current_ref = self.resolve_type(current_ref, ResolveContext::CompileTime(false));
        }

        // Walk the aliasing list
        //
        // We do not have to worry about a cyclic chain of aliases as the
        // parser should not produce such a alias cyclic chain, and when using
        // external libraries, the type references should be validated to not
        // produce a cyclic reference chain.
        loop {
            let current_id  = if let Some(type_id) = types::get_type_id(&current_ref) {
                type_id
            } else {
                // Most likely at the end of the chain, so break
                break; 
            };

            // Reference types should already be resolved
            let mut type_info = self.type_table.get_type(current_id).clone();
            debug_assert!(!matches!(type_info, Type::Reference { .. }));

            match &mut type_info {
                Type::Alias { to } => {
                    if let Some(Type::Reference { .. }) = self.type_table.type_from_ref(&to) {
                        *to = self.resolve_type(*to, ResolveContext::CompileTime(false));
                    }

                    // Walk to the next id
                    current_ref = *to;
                }
                Type::Reference { .. } => panic!("Unresolved reference type"),
                _ => break, // Not either of the above, can stop
            }

            // Update the alias reference
            self.type_table.replace_type(current_id, type_info);
        }

        // At the end of the aliasing chain
        return current_ref;
    }
}

impl ASTVisitorMut<(), Option<Value>> for Validator<'_> {
    fn visit_stmt(&mut self, visit_stmt: &mut Stmt) {
        match visit_stmt {
            Stmt::VarDecl {
                idents,
                type_spec,
                value,
                is_const
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
                }

                // Visit the expression to update the eval type
                if value.is_some() {
                    let expr = value.as_mut().unwrap();
                    let init_eval = self.visit_expr(expr);

                    // Try to replace the initializer value with the folded value
                    if init_eval.is_some() { *expr = Box::new(Expr::try_from(init_eval.unwrap()).unwrap()); }

                    is_compile_eval = expr.is_compile_eval();
                }

                // Resolve the identifier type spec, if possible
                if types::is_named(type_spec) {
                    // Only required to be compile-time if the decl is a const decl, or if the type spec
                    // is not directly an array
                    let resolving_context = if *is_const || !matches!(self.type_table.type_from_ref(type_spec), Some(Type::Array{ .. })) {
                        ResolveContext::CompileTime(false)
                    } else {
                        ResolveContext::Any
                    };

                    *type_spec = self.resolve_type(*type_spec, resolving_context);
                }
                
                // Handle the type spec propogation
                if *type_spec == TypeRef::Unknown {
                    // Unknown type, use the type of the expr
                    // Safe to unwrap as if no expr was provided, the type_spec would be TypeError    

                    let expr = value.as_ref().unwrap();
                    *type_spec = expr.get_eval_type();
                } else if value.is_some() {
                    // Type of the identifier is known, validate that the types are assignable
                    let expr = value.as_ref().unwrap();

                    let left_type = &self.dealias_resolve_type(*type_spec);
                    let right_type = &self.dealias_resolve_type(expr.get_eval_type());

                    // If both of the types are not an error, check for assignability
                    if !types::is_error(left_type) && !types::is_error(right_type) {
                        debug_assert!(types::is_base_type(left_type, &self.type_table), "Of type {:?}", left_type);
                        debug_assert!(types::is_base_type(right_type, &self.type_table), "Of type {:?}", right_type);

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
                let const_val = if is_compile_eval && *is_const {
                    // Create a value to clone from
                    Some(Value::try_from(*value.as_ref().unwrap().clone()).expect("Initializer value is not a compile-time expression"))
                } else {
                    // No compile-time value is produced
                    None
                };

                // Update the identifiers to the new identifier type
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

                    // Add identifier to the scope info (including the compile-time value)
                    if self.scope_infos.last_mut().unwrap().decl_ident_with(ident.clone(), const_val.clone()) {
                        // Report the error
                        self.reporter.report_error(&ident.token.location, format_args!("'{}' has already been declared", ident.name));
                    }
                }
            }
            Stmt::TypeDecl { ident, resolved_type, is_new_def } => {
                if *is_new_def {
                    if resolved_type.is_some() {
                        // Resolve the associated type (do not allow forward references)
                        ident.type_spec = self.resolve_type(ident.type_spec, ResolveContext::CompileTime(false));
                    }

                    // Declare the identifier and check for redeclaration errors
                    if self.scope_infos.last_mut().unwrap().decl_ident(ident.clone()) {
                        self.reporter.report_error(&ident.token.location, format_args!("'{}' has already been declared", ident.name));
                    }
                } else {
                    // Use the identifier
                    assert!(!self.scope_infos.last_mut().unwrap().use_ident(&ident).1);

                    if let Some(resolve_ref) = resolved_type {
                        // This is a type resolution statement, update the associated type reference
                        if let TypeRef::Named(replace_id) = ident.type_spec{
                            // Make an alias to the resolved type
                            self.type_table.replace_type(replace_id, Type::Alias{
                                to: *resolve_ref,
                            });
                        }
                        
                        // Resolve the rest of the type
                        ident.type_spec = self.resolve_type(ident.type_spec, ResolveContext::CompileTime(false));
                    } else {
                        // This is a redeclared forward, and is safe to ignore
                    }
                }
            }
            Stmt::Assign {
                var_ref,
                op,
                value,
            } => {
                let ref_eval = self.visit_expr(var_ref);
                let value_eval = self.visit_expr(value);

                // Try to replace the operands with the folded values
                if ref_eval.is_some() { *var_ref = Box::new(Expr::try_from(ref_eval.unwrap()).unwrap()); }
                if value_eval.is_some() { *value = Box::new(Expr::try_from(value_eval.unwrap()).unwrap()); }

                let left_type = &self.dealias_resolve_type(var_ref.get_eval_type());
                let right_type = &self.dealias_resolve_type(value.get_eval_type());

                // Validate that the types are assignable for the given operation
                if types::is_error(left_type) || types::is_error(right_type) {
                    // Silently drop propogated TypeErrors
                    return;
                }

                debug_assert!(types::is_base_type(left_type, &self.type_table));
                debug_assert!(types::is_base_type(right_type, &self.type_table));
                
                if op.token_type == TokenType::Assign {
                    if !types::is_assignable_to(left_type, right_type, &self.type_table) {
                        // Value to assign is the wrong type
                        self.reporter.report_error(
                            &op.location,
                            format_args!("Assignment value is the wrong type"),
                        );
                    }
                } else {
                    let produce_type = check_binary_operands(left_type, &op.token_type, right_type, &self.type_table);
                    if produce_type.is_err() || !types::is_assignable_to(left_type, &produce_type.unwrap(), &self.type_table) {
                        // Value to assign is the wrong type
                        self.reporter.report_error(
                            &op.location,
                            format_args!("Assignment value is the wrong type"),
                        );
                    }
                }
            }
            Stmt::ProcedureCall { proc_ref } => { let _ = self.visit_expr(proc_ref); },
            Stmt::Block { block } => {
                // TODO: Change our active scope and push a new scope info
                for stmt in block.borrow_mut().stmts.iter_mut() {
                    self.visit_stmt(stmt);
                }
                // TODO: Revert to previous scope and pop the last scope info
            }
        }
    }

    // Note: If the eval_type is still TypeRef::Unknown, propagate the type error
    fn visit_expr(&mut self, visit_expr: &mut Expr) -> Option<Value> {
        match visit_expr {
            Expr::Grouping { expr, eval_type, is_compile_eval, .. } => {
                let eval = self.visit_expr(expr);

                // Try to replace the inner expression with the folded value
                if eval.is_some() { *expr = Box::new(Expr::try_from(eval.clone().unwrap()).unwrap()); }

                *eval_type = expr.get_eval_type();
                *is_compile_eval = expr.is_compile_eval();

                // Propogate the folded value
                return eval;
            }
            Expr::BinaryOp {
                left,
                op,
                right,
                eval_type,
                is_compile_eval,
                ..
            } => {
                let left_eval = self.visit_expr(left);
                let right_eval = self.visit_expr(right);

                // Try to replace the operands with the folded values
                if left_eval.is_some() { *left = Box::new(Expr::try_from(left_eval.unwrap()).unwrap()); }
                if right_eval.is_some() { *right = Box::new(Expr::try_from(right_eval.unwrap()).unwrap()); }

                // Validate that the types are assignable with the given operation
                // eval_type is the type of the expr result

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
                            let lvalue = Value::try_from(*left.clone()).expect(&format!("Left operand is not a compile-time value {:?}", left));
                            let rvalue = Value::try_from(*right.clone()).expect(&format!("Right operand is not a compile-time value {:?}", right));

                            let result = value::apply_binary(lvalue, op, rvalue);
                            
                            return match result {
                                Ok(v) => {
                                    eprintln!("Folded {:?} into {:?}", visit_expr, v);
                                    Some(v)
                                },
                                Err(msg) => {
                                    // Report the error message!
                                    // TODO: Produce an appropriate error message for the current operand
                                    self.reporter.report_error(&loc, format_args!("Error in folding compile-time expression: {:?}", msg));

                                    // Remove the compile-time evaluability status
                                    *is_compile_eval = false;
                                    None
                                }
                            }
                        } else {
                            // Can't fold the current expression
                            return None;
                        }
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
                            TokenType::And | TokenType::Or | TokenType::Xor =>
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat) or booleans", op)),
                            TokenType::Shl | TokenType::Shr => 
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be integers (int, or nat)", op)),
                            TokenType::Less | TokenType::LessEqu | TokenType::Greater | TokenType::GreaterEqu =>
                                if !types::is_equivalent_to(left_type, right_type, self.type_table) {
                                    self.reporter.report_error(loc, format_args!("Operands of '{}' must be the same type", op))
                                } else {
                                    self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat), sets, enumerations, strings, or object classes", op))
                                },
                            TokenType::NotEq | TokenType::Equ => if !types::is_equivalent_to(left_type, right_type, self.type_table) {
                                    self.reporter.report_error(loc, format_args!("Operands of '{}' must be the same type", op));
                                } else {
                                    self.reporter.report_error(loc, format_args!("Operands of '{}' must both be booleans, scalars (int, real, or nat), sets, enumerations, strings, object classes, or pointers of equivalent types", op));
                                },
                            TokenType::In | TokenType::NotIn => if !types::is_set(right_type, self.type_table) {
                                self.reporter.report_error(loc, format_args!("Right operand of '{}' must be a set type", op));
                                } else {
                                        self.reporter.report_error(loc, format_args!("Left operand of '{}' must be compatible with the set's index type", op));
                                },
                            TokenType::Imply =>
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be booleans", op)),
                            _ => todo!(),
                        }

                        // Produce no value
                        return None;
                    }
                }
            }
            Expr::UnaryOp {
                op,
                right,
                eval_type,
                is_compile_eval,
                ..
            } => {
                let right_eval = self.visit_expr(right);

                // Try to replace operand with the folded value
                if right_eval.is_some() { *right = Box::new(Expr::try_from(right_eval.unwrap()).unwrap()); }

                // Validate that the unary operator can be applied to the rhs
                // eval_type is the result of the operation (usually the same
                // as rhs)

                let loc = &op.location;
                let op = &op.token_type;

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
                        // Forced to false as we currently don't fold unary expressions
                        *is_compile_eval = right.is_compile_eval() && false;

                        if *op == TokenType::Caret {
                            // Pointers are never compile-time evaluable
                            *is_compile_eval = false;
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
            Expr::Call {
                left,
                op: _,
                arg_list,
                eval_type: _,
                is_compile_eval,
                ..
            } => {
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
                ..
            } => {
                self.visit_expr(left);

                // Validate that the field exists in the given type
                // eval_type is the field type

                // For now, dot expressions default to runtime-time only
                *is_compile_eval = false;
            }
            Expr::Reference { ident } => {
                // TODO: Check if the reference is imported, and grab info from the correct scope info
                // Use the identifier and grab the associated value
                let (compile_value, is_never_declared) = self.scope_infos.last_mut().unwrap().use_ident(&ident);

                if is_never_declared {
                    // Identifier has not been declared at all before this point, report it
                    // Only reported once everytime something is declared
                    self.reporter.report_error(&ident.token.location, format_args!("'{}' has not been declared yet", ident.name));
                }

                if ident.is_declared {
                    // Should either have a valid type, or TypeRef::Unknown
                    assert_ne!(ident.type_spec, TypeRef::TypeError);

                    // Grab the correct identifier information (including the
                    // type_spec) in the scope table
                    let new_ident = self
                        .active_scope
                        .upgrade()
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .scope
                        .get_ident_instance(&ident.name, ident.instance)
                        .unwrap()
                        .clone();
                    *ident = new_ident;

                    // An identifier is compile-time evaluable if and only if there is an associated expression
                    ident.is_compile_eval = compile_value.is_some();
                } else {
                    // Not declared, don't touch the `type_spec` (preserves error checking "correctness")
                }

                // Return the reference's associated compile-time value
                return compile_value;
            }
            Expr::Literal { .. } => {
                // Literal values already have the type resolved
                // No need to produce a value as the current literal can produce the required value
                return None;
            }
        }

        None
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
/// they are the base types) \
/// `left`                  The base type of the right operand \
/// `op`                    The operator to check for \
/// `right`                 The base type of the left operand \
/// `type_table`            Type table to resolve named types \
/// `check_compile_eval`    Whether to check for compile-time evaluability for
///                         certain operations (e.g. shl, shr)
fn check_binary_operands(
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
            } else if types::is_set(left_type, type_table) && types::is_set(right_type, type_table)
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
            } else if types::is_set(left_type, type_table) && types::is_set(right_type, type_table)
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
            // Valid, but not checked:
            // - Both types are enums (not necessarily equivalent)
            // - Both types are (object) classes (not necessarily equivalent)
            // Otherwise, Boolean (as an error) is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number compare, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if (types::is_char_seq_type(left_type) || types::is_char(left_type))
                    && (types::is_char_seq_type(right_type) || types::is_char(right_type)) {
                // String/char seq or char class type, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_set(left_type, type_table) && types::is_set(right_type, type_table) {
                // Set comparision, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            }
            // TODO: Check remaining types (object class, enums)
        }
        TokenType::Equ | TokenType::NotEq => {
            // Valid conditions:
            // - Both types are numerics (real, int, nat, etc)
            // - Both types are char or strings class types (string, string(n), char, char(n))
            // - Both types are sets (not necessarily equivalent)
            // - Both types are booleans
            // Valid, but not checked:
            // - Both types are enums (not necessarily equivalent)
            // - Both types are (object) classes (not necessarily equivalent)
            // - Both types are pointers (with equivalent types)
            // - Both types are class pointers (not necessarily equivalent)
            // Otherwise, Boolean (as an error) is produced
            if types::is_number_type(left_type) && types::is_number_type(right_type) {
                // Number equality, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_boolean(left_type) && types::is_boolean(right_type) {
                // Boolean equality, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if (types::is_char_seq_type(left_type) || types::is_char(left_type))
                    && (types::is_char_seq_type(right_type) || types::is_char(right_type)) {
                // String/char seq or char class type, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            } else if types::is_set(left_type, type_table) && types::is_set(right_type, type_table) {
                // Set equality, produce boolean
                return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
            }
            // TODO: Check remaining types (pointer, object class, enums)
        }
        TokenType::In | TokenType::NotIn => {
            // Valid conditions:
            // - Left type matches the set's index type
            // - Right type is a set
            // Otherwise, Boolean (as error) is produced
            if let Some(Type::Set { range }) = type_table.type_from_ref(right_type) {
                // ???: Use 'assignable to' to reuse range check?
                if let Some(Type::Range { base_type, .. }) = type_table.type_from_ref(range) {
                    if types::is_equivalent_to(left_type, base_type, type_table) {
                        // Element test with equivalent/compatible types produces boolean
                        return Ok(TypeRef::Primitive(PrimitiveType::Boolean));
                    }
                } else if types::is_equivalent_to(left_type, range, type_table) {
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
        _ => todo!()
    }

    Err(binary_default(op))
}

fn check_unary_operand(op: &TokenType, right_type: &TypeRef, type_table: &TypeTable) -> Result<TypeRef, TypeRef> {
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
            if let Some(Type::Pointer { to }) = type_table.type_from_ref(right_type) {
                // Produce the type pointed to by the type ref
                return Ok(*to);
            }
        }
        _ => unreachable!(),
    }

    Err(unary_default(op))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::block::CodeUnit;
    use crate::compiler::scanner::Scanner;
    use crate::compiler::parser::Parser;
    use rand::prelude::*;

    /// Makes and runs a validator
    /// Parsing & scanning must complete successfully
    /// Returns true if the AST is valid, and the validated code unit
    fn make_validator(source: &str) -> (bool, CodeUnit) {
        // Taken from main.rs
        // Build the main unit
        let code_unit = CodeUnit::new(true);
        
        let mut scanner = Scanner::new(&source);
        assert!(scanner.scan_tokens(), "Scanner failed to scan the source");
        
        let mut parser = Parser::new(scanner.tokens, &source, code_unit);
        assert!(parser.parse(), "Parser failed to parse the source");
        
        // Take the unit back from the parser
        let mut code_unit = parser.take_unit();
        let mut type_table = code_unit.take_types();
        
        // Validate AST
        let mut validator = Validator::new(code_unit.root_block(), &mut type_table);
        code_unit.visit_ast_mut(&mut validator);
        let successful_validate = !validator.reporter.has_error();

        code_unit.put_types(type_table);
        
        (successful_validate, code_unit)
    }

    /// Runs the validator on the given source
    /// Parsing & scanning must complete successfully
    /// Returns true if the AST is valid
    fn run_validator(source: &str) -> bool {
        make_validator(source).0
    }

    #[test]
    fn test_empty_file() {
        assert!(run_validator(""));
    }

    #[test]
    fn test_simple_asn_typecheck() {
        // Also tests type compatibility
        // Basic, unsized types
        assert_eq!(true, run_validator("var a : int  := 1"));
        assert_eq!(true, run_validator("var a : nat  := 1"));
        assert_eq!(true, run_validator("var a : real := 1.0"));
        assert_eq!(true, run_validator("var a : string := \"Some text\""));

        // Compatibility between char and char(1)
        assert_eq!(true, run_validator("var a : char := 'a'"));
        assert_eq!(true, run_validator("var a : char(1) := 'a'"));
        assert_eq!(true, run_validator("var c : char := 'c'\nvar a : char(1) := c"));
        assert_eq!(true, run_validator("var c : char(1) := 'c'\nvar a : char := c"));

        // Compatibility between char and string(1)
        assert_eq!(true, run_validator("var a : char := 'a'"));
        assert_eq!(true, run_validator("var a : string(1) := 'a'"));
        assert_eq!(true, run_validator("var c : char := 'c'\nvar a : string(1) := c"));
        assert_eq!(true, run_validator("var c : string(1) := 'c'\nvar a : char := c"));

        // Compatibility between char and string
        assert_eq!(true, run_validator("var a : char := \"a\""));
        assert_eq!(true, run_validator("var a : string := 'a'"));
        assert_eq!(true, run_validator("var c : char := 'c'\nvar a : string := c"));
        assert_eq!(true, run_validator("var c : string := 'c'\nvar a : char := c"));

        // Incompatibility between char with char(n) and string(n)
        assert_eq!(false, run_validator("var c : char(2) := 'ce'\nvar a : char := c"));
        assert_eq!(false, run_validator("var c : string(2) := 'ce'\nvar a : char := c"));

        // Compatibility with char into char(n) and string(n) 
        assert_eq!(true, run_validator("var c : char := 'c'\nvar a : char(6) := c"));
        assert_eq!(true, run_validator("var c : char := 'c'\nvar a : string(6) := c"));

        // Compatibility between char(n) and string
        // char(n) <- string is only checked at runtime
        assert_eq!(true, run_validator("var s : string := \"abcd\"\nvar a : char(6) := s"));
        assert_eq!(true, run_validator("var c : char(6) := 'abcd'\nvar a : string := c"));

        // (In)compatibility between char(n) of same or different size
        assert_eq!(true, run_validator("var a : char(6) := 'abcd'"));
        assert_eq!(true, run_validator("var a : char(6) := 'abcdaa'"));
        assert_eq!(false, run_validator("var a : char(6) := 'abcdaaa'"));

        // (In)compatibility between string(n) of same or different size
        assert_eq!(true, run_validator("var s : string(4) := 'abcd'    \nvar a : string(6) := s"));
        assert_eq!(true, run_validator("var s : string(6) := 'abcdaa'  \nvar a : string(6) := s"));
        assert_eq!(false, run_validator("var s : string(7) := 'abcdaaa'\nvar a : string(6) := s"));

        // Compatibility between real and number types
        assert_eq!(true, run_validator("var i : int := 1   \nvar a : real := i"));
        assert_eq!(true, run_validator("var i : nat := 1   \nvar a : real := i"));
        assert_eq!(true, run_validator("var i : real := 1.0\nvar a : real := i"));

        // Incompatibility between real and integers
        assert_eq!(false, run_validator("var i : int := 1.0"));
        assert_eq!(false, run_validator("var i : nat := 1.0"));

        // Incompatibility between numbers and strings
        assert_eq!(false, run_validator("var i : int  := \"text\""));
        assert_eq!(false, run_validator("var i : nat  := \"text\""));
        assert_eq!(false, run_validator("var i : real := \"text\""));
        assert_eq!(false, run_validator("var i : int  := 'text'"));
        assert_eq!(false, run_validator("var i : nat  := 'text'"));
        assert_eq!(false, run_validator("var i : real := 'text'"));
        assert_eq!(false, run_validator("var i : int  := 't'"));
        assert_eq!(false, run_validator("var i : nat  := 't'"));
        assert_eq!(false, run_validator("var i : real := 't'"));

        // Incompatibility between numbers and booleans
        assert_eq!(false, run_validator("var i : int  := false"));
        assert_eq!(false, run_validator("var i : nat  := false"));
        assert_eq!(false, run_validator("var i : real := false"));

        // Compatibility between booleans
        assert_eq!(true, run_validator("var i : boolean := false"));

        // Incompatibility between booleans and non-booleans
        assert_eq!(false, run_validator("var i : boolean := 1"));
        assert_eq!(false, run_validator("var i : boolean := 1.0"));
        assert_eq!(false, run_validator("var i : boolean := \"h\""));
        assert_eq!(false, run_validator("var i : boolean := 'ha'"));
        assert_eq!(false, run_validator("var i : boolean := 'h'"));
    }

    #[test]
    fn test_add_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 + 1  \na +=   1 + 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 + 1  \na +=   1 + 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 + 1  \na +=   1 + 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 + 1  \na += 1.0 + 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 + 1.0\na +=   1 + 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 + 1.0\na += 1.0 + 1.0"));
        assert_eq!(true, run_validator("var a : string := \"Hello, \" + \"World!\"\na += \"Hello, \" + \"World!\""));
        assert_eq!(true, run_validator("type s : set of 1 .. 3\nvar a : s\nvar b : s\nvar c : s := a + b\nc += a"));
        assert_eq!(true, run_validator("type s : set of char\nvar a : s\nvar b : s\nvar c : s := a + b\nc += a"));
        assert_eq!(true, run_validator("type s : set of boolean\nvar a : s\nvar b : s\nvar c : s := a + b\nc += a"));

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 + 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na += 1.0"));

        // string cannot be assigned into number
        assert_eq!(false, run_validator("var a : int := \"str\" + \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na += \"str\""));

        // number cannot be assigned into string
        assert_eq!(false, run_validator("var a : string := 1 + 1.0"));
        assert_eq!(false, run_validator("var a : string := \"str\"\na += 1.0"));

        // Set union not applicable to non-equivalent ranges / indexes
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar c : t\nc += a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar c : t\nc += a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar c : t\nc += a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar c : t\nc += a"));
    }

    #[test]
    fn test_sub_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 - 1  \na -=   1 - 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 - 1  \na -=   1 - 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 - 1  \na -=   1 - 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 - 1  \na -= 1.0 - 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 - 1.0\na -=   1 - 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 - 1.0\na -= 1.0 - 1.0"));
        assert_eq!(true, run_validator("type s : set of 1 .. 3\nvar a : s\nvar b : s\nvar c : s := a - b\nc -= a"));
        assert_eq!(true, run_validator("type s : set of char\nvar a : s\nvar b : s\nvar c : s := a - b\nc -= a"));
        assert_eq!(true, run_validator("type s : set of boolean\nvar a : s\nvar b : s\nvar c : s := a - b\nc -= a"));

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 - 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na -= 1.0"));

        // Set difference not applicable to non-equivalent ranges / indexes
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar c : t\nc -= a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar c : t\nc -= a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar c : t\nc -= a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar c : t\nc -= a"));

        // Not scalars or sets
        assert_eq!(false, run_validator("var a : int := \"str\" - \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    -= \"str\""));
    }

    #[test]
    fn test_mul_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 * 1  \na *=   1 * 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 * 1  \na *=   1 * 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 * 1  \na *=   1 * 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 * 1  \na *= 1.0 * 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 * 1.0\na *=   1 * 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 * 1.0\na *= 1.0 * 1.0"));
        assert_eq!(true, run_validator("type s : set of 1 .. 3\nvar a : s\nvar b : s\nvar c : s := a * b\nc *= a"));
        assert_eq!(true, run_validator("type s : set of char\nvar a : s\nvar b : s\nvar c : s := a * b\nc *= a"));
        assert_eq!(true, run_validator("type s : set of boolean\nvar a : s\nvar b : s\nvar c : s := a * b\nc *= a"));

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 * 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na *= 1.0"));

        // Set intersection not applicable to non-equivalent ranges / indexes
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar c : t\nc *= a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar c : t\nc *= a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar c : t\nc *= a"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar c : t\nc *= a"));

        // Not scalars or sets
        assert_eq!(false, run_validator("var a : int := \"str\" * \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    *= \"str\""));
    }

    #[test]
    fn test_idiv_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 div 1  \na div=   1 div 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 div 1  \na div=   1 div 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 div 1  \na div=   1 div 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 div 1  \na div= 1.0 div 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 div 1.0\na div=   1 div 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 div 1.0\na div= 1.0 div 1.0"));

        // Result of idiv can be assigned to an int
        assert_eq!(true, run_validator("var a : int := 1 div 1.0"));
        assert_eq!(true, run_validator("var a : int := 1\na div= 1.0"));

        // Not scalars
        assert_eq!(false, run_validator("var a : int := \"str\" div \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    div= \"str\""));
    }

    #[test]
    fn test_rdiv_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : real :=   1 / 1  \na /=   1 / 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 / 1  \na /= 1.0 / 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 / 1.0\na /=   1 / 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 / 1.0\na /= 1.0 / 1.0"));

        // Result of rdiv cannot be assigned to an int
        assert_eq!(false, run_validator("var a : int := 1 / 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na /= 1.0"));

        // Not scalars
        assert_eq!(false, run_validator("var a : int := \"str\" / \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    /= \"str\""));
    }

    #[test]
    fn test_mod_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 mod 1  \na mod=   1 mod 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 mod 1  \na mod=   1 mod 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 mod 1  \na mod=   1 mod 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 mod 1  \na mod= 1.0 mod 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 mod 1.0\na mod=   1 mod 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 mod 1.0\na mod= 1.0 mod 1.0"));

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1    mod 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na mod= 1.0"));

        // Not scalars
        assert_eq!(false, run_validator("var a : int := \"str\" mod \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    mod= \"str\""));
    }

    #[test]
    fn test_rem_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 rem 1  \na rem=   1 rem 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 rem 1  \na rem=   1 rem 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 rem 1  \na rem=   1 rem 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 rem 1  \na rem= 1.0 rem 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 rem 1.0\na rem=   1 rem 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 rem 1.0\na rem= 1.0 rem 1.0"));

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1    rem 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na rem= 1.0"));
    }

    #[test]
    fn test_exp_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=   1 ** 1  \na **=   1 ** 1  "));
        assert_eq!(true, run_validator("var a : nat  :=   1 ** 1  \na **=   1 ** 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 ** 1  \na **=   1 ** 1  "));
        assert_eq!(true, run_validator("var a : real := 1.0 ** 1  \na **= 1.0 ** 1  "));
        assert_eq!(true, run_validator("var a : real :=   1 ** 1.0\na **=   1 ** 1.0"));
        assert_eq!(true, run_validator("var a : real := 1.0 ** 1.0\na **= 1.0 ** 1.0"));

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1    ** 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na **= 1.0"));

        // Not scalars
        assert_eq!(false, run_validator("var a : int := \"str\" ** \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    **= \"str\""));
    }

    #[test]
    fn test_and_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=        1 and 1       \na and=    1 and 1   "));
        assert_eq!(true, run_validator("var a : nat  :=        1 and 1       \na and=    1 and 1   "));
        assert_eq!(true, run_validator("var a : boolean := false and true    \na and= true and true"));

        // Not integers
        assert_eq!(false, run_validator("var a : int := \"str\" and \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    and= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      and  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   and= 1.0"));

        // Not matching types
        assert_eq!(false, run_validator("var a : boolean := 1       and  1"));
        assert_eq!(false, run_validator("var a : boolean := true\na and= 1    "));
        assert_eq!(false, run_validator("var a : nat     := true    and  false"));
        assert_eq!(false, run_validator("var a : nat     := 1\na    and= true "));
    }

    #[test]
    fn test_or_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=        1 or 1       \na or=    1 or 1   "));
        assert_eq!(true, run_validator("var a : nat  :=        1 or 1       \na or=    1 or 1   "));
        assert_eq!(true, run_validator("var a : boolean := false or true    \na or= true or true"));

        // Not integers
        assert_eq!(false, run_validator("var a : int := \"str\" or \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    or= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      or  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   or= 1.0"));

        // Not matching types
        assert_eq!(false, run_validator("var a : boolean := 1       or  1    "));
        assert_eq!(false, run_validator("var a : boolean := true\na or= 1    "));
        assert_eq!(false, run_validator("var a : nat     := true    or  false"));
        assert_eq!(false, run_validator("var a : nat     := 1\na    or= true "));
    }

    #[test]
    fn test_xor_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=        1 xor 1       \na xor=    1 xor 1   "));
        assert_eq!(true, run_validator("var a : nat  :=        1 xor 1       \na xor=    1 xor 1   "));
        assert_eq!(true, run_validator("var a : boolean := false xor true    \na xor= true xor true"));

        // Not integers
        assert_eq!(false, run_validator("var a : int := \"str\" xor \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    xor= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      xor  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   xor= 1.0"));

        // Not matching types
        assert_eq!(false, run_validator("var a : boolean := 1       xor  1    "));
        assert_eq!(false, run_validator("var a : boolean := true\na xor= 1    "));
        assert_eq!(false, run_validator("var a : nat     := true    xor  false"));
        assert_eq!(false, run_validator("var a : nat     := 1\na    xor= true "));
    }

    #[test]
    fn test_shl_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=        1 shl 1       \na shl=    1 shl 1   "));
        assert_eq!(true, run_validator("var a : nat  :=        1 shl 1       \na shl=    1 shl 1   "));

        // Not integers
        assert_eq!(false, run_validator("var a : int    := \"str\"  shl \"str\""));
        assert_eq!(false, run_validator("var a : int    := 1\na     shl= \"str\""));

        assert_eq!(false, run_validator("var a : real    := 1       shl  1.0"));
        assert_eq!(false, run_validator("var a : real    := 1\na    shl= 1.0"));

        assert_eq!(false, run_validator("var a : boolean := true    shl  true"));
        assert_eq!(false, run_validator("var a : boolean := true\na shl= true"));
    }

    #[test]
    fn test_shr_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : int  :=        1 shr 1       \na shr=    1 shr 1   "));
        assert_eq!(true, run_validator("var a : nat  :=        1 shr 1       \na shr=    1 shr 1   "));

        // Not integers
        assert_eq!(false, run_validator("var a : int    := \"str\"  shr \"str\""));
        assert_eq!(false, run_validator("var a : int    := 1\na     shr= \"str\""));

        assert_eq!(false, run_validator("var a : real    := 1       shr  1.0"));
        assert_eq!(false, run_validator("var a : real    := 1\na    shr= 1.0"));

        assert_eq!(false, run_validator("var a : boolean := true    shr  true"));
        assert_eq!(false, run_validator("var a : boolean := true\na shr= true"));
    }

    fn test_compare_operator_typecheck(compare_op: &str) {
        // Tests typechecking for the binary operator
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := 1.0 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1.0", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := 1.0 {} 1.0", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := \"Hello, \" {} \"World!\"", compare_op)));
        assert_eq!(true, run_validator(&format!("type s0 : set of 1 .. 3\ntype s1 : set of 1 .. 3\nvar a : s0\nvar b : s1\nvar c : boolean := a {} b", compare_op)));
        // Missing: enum & objectclass compares

        // Comparison operands must be the same type (class)
        // bool is whether to always reject
        let type_variants = [
            (false, vec![ "int := 1", "nat := 1", "real := 1.0", "real := 1" ]),
            (false, vec![ "string := \"Hello!\"", "char := 'c'", "char(3) := 'cd'", "string(5) := 'cdefg'" ]),
            (false, vec![ "s0", "s1", "s2", "s3" ]),
            (true,  vec![ "boolean := true", "boolean := false", "boolean := true and false" ]),
            (true,  vec![ "alt" ]),
        ];

        let mut rng = thread_rng();

        for _ in 0 .. 150 {
            // stuff with randomness
            let left_variant_class = rng.gen_range(0, type_variants.len());
            let right_variant_class = rng.gen_range(0, type_variants.len());
            let accept = left_variant_class == right_variant_class && !type_variants[left_variant_class].0 && !type_variants[right_variant_class].0;
            let test_code = format!(
                "type alt : proc _ ()
                type s0 : set of 1 .. 3
                type s1 : set of 1 .. 3
                type s2 : set of char
                type s3 : set of boolean
                var a : {}
                var b : {}
                var c : boolean := a {} b",
                type_variants[left_variant_class].1.iter().choose(&mut rng).unwrap(),
                type_variants[right_variant_class].1.iter().choose(&mut rng).unwrap(),
                compare_op
            );

            assert_eq!(accept, run_validator(&test_code), "Failed on generated test '\n{}'", test_code);
        }
    }

    fn test_equality_operator_typecheck(compare_op: &str) {
        // Tests typechecking for the binary operator
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := 1.0 {} 1  ", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean :=   1 {} 1.0", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := 1.0 {} 1.0", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := \"Hello, \" {} \"World!\"", compare_op)));
        assert_eq!(true, run_validator(&format!("type s0 : set of 1 .. 3\ntype s1 : set of 1 .. 3\nvar a : s0\nvar b : s1\nvar c : boolean := a {} b", compare_op)));
        assert_eq!(true, run_validator(&format!("var a : boolean := true {} true", compare_op)));
        // Missing: enum, objectclass & ptr compares

        // Equality operands must be the same type (class)
        // bool is whether to always reject
        let type_variants = [
            (false, vec![ "int := 1", "nat := 1", "real := 1.0", "real := 1" ]),
            (false, vec![ "string := \"Hello!\"", "char := 'c'", "char(3) := 'cd'", "string(5) := 'cdefg'" ]),
            (false, vec![ "s0", "s1", "s2", "s3" ]),
            (false, vec![ "boolean := true", "boolean := false", "boolean := true and false" ]),
            (true,  vec![ "alt" ]),
        ];

        let mut rng = thread_rng();

        for _ in 0 .. 150 {
            // stuff with randomness
            let left_variant_class = rng.gen_range(0, type_variants.len());
            let right_variant_class = rng.gen_range(0, type_variants.len());
            let accept = left_variant_class == right_variant_class && !type_variants[left_variant_class].0 && !type_variants[right_variant_class].0;
            let test_code = format!(
                "type alt : proc _ ()
                type s0 : set of 1 .. 3
                type s1 : set of 1 .. 3
                type s2 : set of char
                type s3 : set of boolean
                var a : {}
                var b : {}
                var c : boolean := a {} b",
                type_variants[left_variant_class].1.iter().choose(&mut rng).unwrap(),
                type_variants[right_variant_class].1.iter().choose(&mut rng).unwrap(),
                compare_op
            );

            assert_eq!(accept, run_validator(&test_code), "Failed on generated test '\n{}'", test_code);
        }
    }

    #[test]
    fn test_lt_typecheck() {
        test_compare_operator_typecheck("<");
    }

    #[test]
    fn test_gt_typecheck() {
        test_compare_operator_typecheck(">");
    }

    #[test]
    fn test_le_typecheck() {
        test_compare_operator_typecheck("<=");
    }

    #[test]
    fn test_ge_typecheck() {
        test_compare_operator_typecheck(">=");
    }

    #[test]
    fn test_eq_typecheck() {
        test_equality_operator_typecheck("=");
    }

    #[test]
    fn test_ne_token_typecheck() {
        test_equality_operator_typecheck("not=");
    }

    #[test]
    fn test_ne_token_spacing_typecheck() {
        test_equality_operator_typecheck("not =");
    }

    #[test]
    fn test_ne_tilde_typecheck() {
        test_equality_operator_typecheck("~=");
    }

    #[test]
    fn test_ne_tilde_spacing_typecheck() {
        test_equality_operator_typecheck("~ =");
    }

    fn test_set_in_typecheck(variant: &str) {
        // Tests typechecking for the binary operatory
        // TODO: add remaining enum type (and range)
        assert_eq!(true, run_validator(&format!("type s : set of 1 .. 3 \nvar a : s\nvar b := 1 {} a", variant)));
        assert_eq!(true, run_validator(&format!("type s : set of char   \nvar a : s\nvar b := 'a' {} a", variant)));
        assert_eq!(true, run_validator(&format!("type s : set of char   \nvar a : s\nvar c : char := 'c'\nvar b := c {} a", variant)));
        assert_eq!(true, run_validator(&format!("type s : set of boolean\nvar a : s\nvar b := true {} a", variant)));

        // Right operand must be a set
        assert_eq!(false, run_validator(&format!("type s : set of 1 .. 3 \nvar a : int\nvar b := 1 {} a", variant)));
        assert_eq!(false, run_validator(&format!("type s : set of char   \nvar a : string\nvar b := 'a' {} a", variant)));
        assert_eq!(false, run_validator(&format!("type s : set of boolean\nvar a : char\nvar b := true {} a", variant)));

        // Left operand must be compatible with the set index
        assert_eq!(false, run_validator(&format!("type s : set of 1 .. 3 \nvar a : s\nvar b := true {} a", variant)));
        assert_eq!(false, run_validator(&format!("type s : set of char   \nvar a : s\nvar b := 1 {} a", variant)));
        assert_eq!(false, run_validator(&format!("type s : set of boolean\nvar a : s\nvar b := 'a' {} a", variant)));
    }

    #[test]
    fn test_in_typecheck() {
        // Tests typechecking for the binary operatory
        test_set_in_typecheck("in");
    }

    #[test]
    fn test_tilde_in_typecheck() {
        // Tests typechecking for the binary operatory
        test_set_in_typecheck("~in");
    }

    #[test]
    fn test_tilde_space_in_typecheck() {
        // Tests typechecking for the binary operatory
        test_set_in_typecheck("~ in");
    }

    #[test]
    fn test_not_in_typecheck() {
        // Tests typechecking for the binary operatory
        test_set_in_typecheck("not in");
    }

    #[test]
    fn test_imply_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(true, run_validator("var a : boolean := false => true    \na =>= true => true"));

        // Not booleans
        assert_eq!(false, run_validator("var a : int := \"str\" => \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    =>= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      =>  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   =>= 1.0"));

        assert_eq!(false, run_validator("var a : nat     := true    =>  false"));
        assert_eq!(false, run_validator("var a : nat     := 1\na    =>= true "));
    }

    #[test]
    fn test_unary_plus_typecheck() {
        // Tests typechecking for the unary operator
        assert_eq!(true, run_validator("var a : int  := +1"));
        assert_eq!(true, run_validator("var a : nat  := +1"));
        assert_eq!(true, run_validator("var a : real := +1"));
        assert_eq!(true, run_validator("var a : real := +1.0"));

        // Arbitrary applications of unary plus
        assert_eq!(true, run_validator("var a : real := +++++++++++++++1"));

        // real cannot be assigned into integers
        assert_eq!(false, run_validator("var a : int  := +1.0"));
        assert_eq!(false, run_validator("var a : nat  := +1.0"));

        // strings and chars cannot be applied to the unary plus
        assert_eq!(false, run_validator("var a : string  := +\"a\""));
        assert_eq!(false, run_validator("var a : string  := +'aa'"));
        assert_eq!(false, run_validator("var a : string  := +'a'"));

        // boolean cannot be applied to the unary plus
        assert_eq!(false, run_validator("var a : boolean  := +false"));
    }

    #[test]
    fn test_unary_minus_typecheck() {
        // Tests typechecking for the unary operator
        assert_eq!(true, run_validator("var a : int  := -1"));
        assert_eq!(true, run_validator("var a : nat  := -1")); // Invalid, checked at runtime
        assert_eq!(true, run_validator("var a : real := -1"));
        assert_eq!(true, run_validator("var a : real := -1.0"));

        // Arbitrary applications of unary minus
        assert_eq!(true, run_validator("var a : real := ---------------1"));

        // real cannot be assigned into integers
        assert_eq!(false, run_validator("var a : int  := -1.0"));
        assert_eq!(false, run_validator("var a : nat  := -1.0"));

        // strings and chars cannot be applied to the unary minus
        assert_eq!(false, run_validator("var a : string  := -\"a\""));
        assert_eq!(false, run_validator("var a : string  := -'aa'"));
        assert_eq!(false, run_validator("var a : string  := -'a'"));

        // boolean cannot be applied to the unary minus
        assert_eq!(false, run_validator("var a : boolean  := -false"));
    }

    #[test]
    fn test_not_typecheck() {
        // Tests typechecking for the unary operator
        assert_eq!(true, run_validator("var a : boolean := not true"));
        assert_eq!(true, run_validator("var a : int     := not 1"));
        assert_eq!(true, run_validator("var a : nat     := not 1"));

        // Arbitrary applications of not
        assert_eq!(true, run_validator("var a : boolean := ~~~~~~~~~~~~~true"));

        // reals cannot be applied to 'not'
        assert_eq!(false, run_validator("var a : real  := not 1.0"));

        // strings and chars cannot be applied to 'not'
        assert_eq!(false, run_validator("var a : string  := not\"a\""));
        assert_eq!(false, run_validator("var a : string  := not'aa'"));
        assert_eq!(false, run_validator("var a : string  := not'a'"));
    }

    #[test]
    fn test_deref_typecheck() {
        // Tests typechecking for the unary operator
        assert_eq!(true, run_validator("var a : ^int\nvar b : int := ^a"));

        // Arbitrary applications of deref
        assert_eq!(true, run_validator("var a : ^^^^^^^^^^^^^int\nvar b : int := ^^^^^^^^^^^^^a"));

        // Deref propogates the pointed to type
        assert_eq!(false, run_validator("var a : ^^int\nvar b : int := ^a"));
        assert_eq!(false, run_validator("var a : ^^string\nvar b : int := ^^a"));

        // Deref cannot be applied to non-pointers
        assert_eq!(false, run_validator("var a : boolean := ^true"));
        assert_eq!(false, run_validator("var a : int     := ^1"));
        assert_eq!(false, run_validator("var a : nat     := ^1"));
        assert_eq!(false, run_validator("var a : real    := ^1.0"));

        // strings and chars cannot be applied to deref
        assert_eq!(false, run_validator("var a : string  := ^\"a\""));
        assert_eq!(false, run_validator("var a : string  := ^'aa'"));
        assert_eq!(false, run_validator("var a : string  := ^'a'"));
    }

    #[test]
    fn test_poundcheat_typecheck() {
        // Tests typechecking for the unary operator
        // nat cheat can be applied to anything var/const reference or literal,
        // as long as the destination operand is a nat
        assert_eq!(true, run_validator("var a : nat := #true"));
        assert_eq!(true, run_validator("var a : nat := #1"));
        assert_eq!(true, run_validator("var a : nat := #1"));
        assert_eq!(true, run_validator("var a : nat := #1.0"));
        assert_eq!(true, run_validator("var a : nat := #\"a\""));
        assert_eq!(true, run_validator("var a : nat := #'aa'"));
        assert_eq!(true, run_validator("var a : nat := #'a'"));
        assert_eq!(true, run_validator("var a : function a() : int\nvar b : nat := #a"));

        // nat cheat cannot be applied to direct typedefs (not checked yet)
        //assert_eq!(false, run_validator("type a : function a() : int\nvar b : nat := #a"));
        // nat cheat cannot be applied to typedefs hidden behind '.'s (not checked yet)
        // TODO: flesh this out once types with fields are parsed

        // Arbitrary applications of nat cheat
        assert_eq!(true, run_validator("var a : int := ###############'kemp'"));
    }

    #[test]
    fn test_constant_folder() {
        // Folds should chain together
        let (success, unit) = make_validator("var a : int := 1 - 1 - 1 - 1 - 1");
        assert_eq!(true, success);
        if let Stmt::VarDecl { value: Some(expr), .. } = &unit.stmts()[0] {
            assert_eq!(Value::try_from(*expr.clone()).unwrap(), Value::IntValue(-3));
        } else {
            panic!("Fold failed");
        }

        // Stop folding in an error
        assert_eq!(false, run_validator("var a : int := 1 - 1 - \"bad\" - 1 - 1"));
        assert_eq!(false, run_validator("var a : int := 1 - 1 ** (0 - 1) - 1 - 1"));
        assert_eq!(false, run_validator("var a : real := 10.0 ** (300 + 7) * 100"));
        assert_eq!(false, run_validator("var a : real := 10.0 ** (300 + 10)"));
        // Preserve types
        assert_eq!(false, run_validator("var a : int := 1 + 0.1 - 1 - 0.1 - 1 - 1"));

        // Ensure that the constant folder preserves assignment semantics
        assert_eq!(false, run_validator("var a : char(6) := 'abcd' + 'aaa'"));

        // Valid type check, checked at runtime
        assert_eq!(true, run_validator("var a : nat := (0 - 1)"));
    }

    #[test]
    fn test_constant_prop() {
        // Test constant propogation

        // Fold constants together
        let (success, unit) = make_validator("
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a    % (4 + 1) + 1 + 4
const d := a + b + c    % 4*4 + 1 + 1 + 1
        ");
        assert_eq!(true, success);
        if let Stmt::VarDecl { value: Some(expr), .. } = &unit.stmts().last().unwrap() {
            assert_eq!(Value::try_from(*expr.clone()).unwrap(), Value::NatValue(19));
        } else {
            panic!("Fold failed");
        }

        // Stop folding constants in the event of an error
        let (success, unit) = make_validator("
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a + \"beep beep\"
const d := a + b + c    % 4*4 + 1 + 1 + 1
        ");
        assert_eq!(false, success);
        if let Stmt::VarDecl { value: Some(expr), .. } = &unit.stmts().last().unwrap() {
            if let Expr::BinaryOp { left, right, ..} = *expr.clone() {
                // Check that (a + b) was folded, but not the + c
                assert!(!matches!(*left.clone(), Expr::BinaryOp { .. }));
                assert!(matches!(*right.clone(), Expr::Reference { .. }));
            } else {
                panic!("Something wrong happened! (folding did weird things!)");
            }
        } else {
            panic!("Something wrong happened! (not a var_decl!)");
        }

        // TODO: Check that constant propogation still works in the case of
        // nested inner scopes
    }

    #[test]
    fn test_ident_resolution() {
        // All errors reported here, including undeclared uses
        // v decl use
        let (success, unit) = make_validator("var a : int := 1\na += 1");
        assert_eq!(true, success);
        assert_eq!(true, unit.root_block().borrow().scope.get_ident("a").unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::Primitive(PrimitiveType::Int));

        // x use decl
        let (success, unit) = make_validator("a += 1\nvar b : int := 1");
        assert_eq!(false, success);
        assert_eq!(false, unit.root_block().borrow().scope.get_ident("a").unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::TypeError);

        // x use use decl (only 1 error produced)
        let (success, unit) = make_validator("a += 1\na += 1\nvar b : int := 1");
        assert_eq!(false, success);
        assert_eq!(false, unit.root_block().borrow().scope.get_ident("a").unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::TypeError);

        // x use
        let (success, unit) = make_validator("a += 1\n");
        assert_eq!(false, success);
        assert_eq!(false, unit.root_block().borrow().scope.get_ident("a").unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::TypeError);

        // x use decl decl
        let (success, unit) = make_validator("a := a + 1\nvar a : int\nvar a : string % final type\n");
        assert_eq!(false, success);
        assert_eq!(false, unit.root_block().borrow().scope.get_ident_instance("a", 0).unwrap().is_declared);
        assert_eq!(true, unit.root_block().borrow().scope.get_ident_instance("a", 1).unwrap().is_declared);
        assert_eq!(true, unit.root_block().borrow().scope.get_ident_instance("a", 2).unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 0).unwrap().type_spec, TypeRef::TypeError);
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 1).unwrap().type_spec, TypeRef::Primitive(PrimitiveType::Int));
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 2).unwrap().type_spec, TypeRef::Primitive(PrimitiveType::String_));
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::Primitive(PrimitiveType::String_)); // Final type

        // x decl decl
        let (success, unit) = make_validator("var a : string\nvar a : real8 % final type");
        assert_eq!(false, success);
        assert_eq!(true, unit.root_block().borrow().scope.get_ident_instance("a", 1).unwrap().is_declared);
        assert_eq!(true, unit.root_block().borrow().scope.get_ident_instance("a", 2).unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 1).unwrap().type_spec, TypeRef::Primitive(PrimitiveType::String_));
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 2).unwrap().type_spec, TypeRef::Primitive(PrimitiveType::Real8));
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::Primitive(PrimitiveType::Real8)); // Final type

        // x use-in-init decl
        let (success, unit) = make_validator("var a : string := a + \"oops\"");
        eprintln!("??: {:#?}", unit);
        assert_eq!(false, success);
        assert_eq!(false, unit.root_block().borrow().scope.get_ident_instance("a", 0).unwrap().is_declared);
        assert_eq!(true, unit.root_block().borrow().scope.get_ident_instance("a", 1).unwrap().is_declared);
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 0).unwrap().type_spec, TypeRef::TypeError);
        assert_eq!(unit.root_block().borrow().scope.get_ident_instance("a", 1).unwrap().type_spec, TypeRef::Primitive(PrimitiveType::String_));
        assert_eq!(unit.root_block().borrow().scope.get_ident("a").unwrap().type_spec, TypeRef::Primitive(PrimitiveType::String_)); // Final type
    }

    #[test]
    fn test_type_resolution() {
        // Aliases to aliases are allowed
        assert_eq!(true, run_validator("type a : int\ntype b : a\ntype c : b"));

        // Aliases are equivalent to their base types
        assert_eq!(true, run_validator("type a : 1 .. 5\ntype b : set of a"));
        assert_eq!(true, run_validator("type a : int\ntype b : a\nvar c : int := 1\nvar d : b := c"));

        // Aliases of resolved forward types are equivalent to their base types
        assert_eq!(true, run_validator("type a : forward\ntype a : int\ntype b : a\nvar c : a := 2"));

        // Forward refs are only allowed in pointer type definitions
        assert_eq!(true, run_validator("type a : forward\nvar k : ^a\ntype a : int"));
        assert_eq!(true, run_validator("type a : forward\ntype k : ^a\ntype a : int"));
        assert_eq!(false, run_validator("type a : forward\ntype k : a\ntype a : int"));
        assert_eq!(false, run_validator("type a : forward\ntype k : set of a\ntype a : int"));

        // Range bounds types do not match
        assert_eq!(false, run_validator("type a : true .. 'c'"));
        assert_eq!(false, run_validator("type a : 1 .. 'c'"));
        assert_eq!(false, run_validator("type a : 'c' .. true"));

        // Identifier is not a reference to a type
        // TODO: Test dot references once those are valid & resolvable
        assert_eq!(false, run_validator("var a : int := 1\ntype b : a"));
        assert_eq!(false, run_validator("var a : int := 1\nvar b : a := 2"));

        // Range end bound must be a compile-time expression (in theses contexts)
        assert_eq!(true, run_validator("type b : set of 1 .. (8 + 20 - 3)"));
        assert_eq!(true, run_validator("type b : 1 .. (8 + 20 - 3)"));
        assert_eq!(true, run_validator(" var b : 1 .. (8 + 20 - 3)"));

        assert_eq!(false, run_validator("var a : int := 1\ntype b : set of 1 .. a"));
        assert_eq!(false, run_validator("var a : int := 1\ntype b : 1 .. a"));
        assert_eq!(false, run_validator("var a : int := 1\n var b : 1 .. a"));
        assert_eq!(false, run_validator("var a : int := 1\ntype b : array 1 .. a of int"));

        // Range end bound is allowed to be a runtime expression (in this context)
        assert_eq!(true, run_validator("var a : int := 1\n var b : array 1 .. a of int"));
    }
}