//! Type validator/resolver for the AST tree
//! Performs the majority of the semantic validation pass
//! - Propogates and checks expressions for type correctness
//! - Resolves identifiers into their final types
//! - Validates and resolves types into their final forms
//! - Checks and evaluates compile-time expressions
//!
//! Types are resolved before the expression that use them are visited by only
//! resolving types in declaration statements

// Validator fragments
mod expr;
mod stmt;
mod types;

use crate::context::CompileContext;
use toc_ast::ast::expr::{Expr, ExprKind, Literal};
use toc_ast::ast::ident::{IdentId, RefKind};
use toc_ast::ast::stmt::{Stmt, StmtKind};
use toc_ast::ast::VisitorMut;
use toc_ast::scope::{ScopeBlock, UnitScope};
use toc_ast::types as ty; // Validator submodule is named `types`, but not used here
use toc_ast::types::{Type, TypeRef, TypeTable};
use toc_ast::value::{self, Value, ValueApplyError};
use toc_core::Location;

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

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
pub struct Validator {
    /// Compile Context
    context: Rc<RefCell<CompileContext>>,
    /// Type table to use
    type_table: TypeTable,
    /// Unit scope to take identifiers from
    unit_scope: UnitScope,
    /// Mapping of all compile-time values
    compile_values: HashMap<IdentId, Value>,
}

type ResolveResult = Option<TypeRef>;

impl Validator {
    pub fn new(
        unit_scope: UnitScope,
        type_table: TypeTable,
        context: Rc<RefCell<CompileContext>>,
    ) -> Self {
        Self {
            context,
            type_table,
            unit_scope,
            compile_values: HashMap::new(),
        }
    }

    /// Takes the `type_table` and `unit_scope` from the validator
    pub fn take_code_unit_parts(&mut self) -> (TypeTable, UnitScope) {
        let type_table = std::mem::replace(&mut self.type_table, TypeTable::new());
        let unit_scope = std::mem::replace(&mut self.unit_scope, UnitScope::new());
        (type_table, unit_scope)
    }

    /// De-aliases a type ref, following through `Type::Alias`'s and resolving `Type::Reference`s.
    fn dealias_resolve_type(&mut self, type_ref: TypeRef) -> TypeRef {
        let type_id = if let Some(id) = ty::get_type_id(&type_ref) {
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
        while let Some(current_id) = ty::get_type_id(&current_ref) {
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
        current_ref
    }

    /// Reports unused identifiers in the given scope
    fn report_unused_identifiers(&self, block: &ScopeBlock) {
        let mut unique_undeclared = std::collections::HashSet::new();
        let idents = block.declared_idents().copied();
        let idents = block
            .shadowed_idents()
            .map(|(_, id)| *id)
            .chain(idents)
            .filter(|id| {
                let info = self.unit_scope.get_ident_info(&id);
                // Select all declared & unused identifiers
                info.is_declared && info.usages == 0
            });
        idents.for_each(|id| {
            unique_undeclared.insert(id);
        });

        // Report all undeclared identifiers
        for id in unique_undeclared {
            let ident = self.unit_scope.get_ident_info(&id);
            self.context.borrow_mut().reporter.report_warning(
                &ident.location,
                format_args!("This declaration of '{}' is never used", ident.name),
            );
        }
    }

    fn report_redeclared_identifiers(&self, block: &ScopeBlock) {
        for (_, new_id) in block.shadowed_idents() {
            let info = self.unit_scope.get_ident_info(&new_id);

            self.context.borrow_mut().reporter.report_error(
                &info.location,
                format_args!("'{}' has already been declared", &info.name),
            );
        }
    }

    // --- Associated Helpers --- //

    /// Gets the reference info, if there is some
    ///
    /// Returns (in order):
    /// - `ref_name`
    /// - `ref_type_spec`
    /// - `ref_kind`
    /// - `use_location`
    fn get_reference_ident<'a, 'b: 'a>(
        &'a self,
        ref_expr: &'b Expr,
    ) -> Option<(&'a String, &'a TypeRef, &'a RefKind, &'a Location)> {
        match &ref_expr.kind {
            ExprKind::Parens { inner } => self.get_reference_ident(inner),
            ExprKind::Reference { ident, .. } => {
                let info = self.unit_scope.get_ident_info(&ident.id);
                Some((&info.name, &info.type_spec, &info.ref_kind, &ident.location))
            }
            ExprKind::Dot {
                field: (field, location),
                ..
            } => Some((&field.name, &field.type_spec, &field.ref_kind, &location)),
            _ => None,
        }
    }

    /// Checks if the expression evaluates to a type reference
    fn is_type_reference(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Parens { inner } => self.is_type_reference(inner),
            ExprKind::Reference { ident, .. } => {
                // It's a type reference based on the identifier
                let info = self.unit_scope.get_ident_info(&ident.id);
                info.ref_kind == RefKind::Type
            }
            ExprKind::Dot {
                left,
                field: (field_def, _),
                ..
            } => {
                let base_type = left.get_eval_type();
                let field_type = field_def.type_spec;

                if ty::is_enum_type(&base_type, &self.type_table)
                    || ty::is_enum_type(&field_type, &self.type_table)
                {
                    // Enum fields are definitely not types
                    false
                } else {
                    // Right now, we don't have compound types that contain type references (e.g. in modules)
                    // If we did, we'd refer to the `var_ref`'s eval_type (i.e. the container type) and look
                    // at the field definition

                    // TODO(compound_types): Fill this out once compound types with typedefs are added
                    assert_ne!(
                        field_def.ref_kind,
                        RefKind::Type,
                        "Don't have compound types with type defs yet"
                    );
                    false
                }
            }
            _ => false, // Most likely not a type reference
        }
    }

    // Evaluates expression.
    // Expected to be called after visiting the expression root.
    //
    // If an expresssion is not compile-time evaluable, `Ok(None)` is produced
    // If an error occurred during evaluation, an error is reported and the specific
    // error is produced.
    fn eval_expr(&self, expr: &Expr) -> Result<Option<Value>, ValueApplyError> {
        // Only used here, and may be moved later on
        use toc_ast::ast::expr::BinaryOp;

        if !expr.is_compile_eval {
            // Will never produce a value
            return Ok(None);
        }

        match &expr.kind {
            ExprKind::Parens { inner } => self.eval_expr(inner), // Evaluate inner
            ExprKind::BinaryOp {
                left,
                op: (op, location),
                right,
            } => {
                // Evaluate operands
                let lvalue = if let Some(value) = self.eval_expr(left)? {
                    value
                } else {
                    return Ok(None);
                };

                let rvalue = if let Some(value) = self.eval_expr(right)? {
                    value
                } else {
                    return Ok(None);
                };

                let result = value::apply_binary(
                    lvalue,
                    *op,
                    rvalue,
                    value::EvalConstraints { as_64_bit: false },
                );

                if let Err(msg) = result {
                    // Report the error message!
                    let context = self.context.borrow_mut();

                    match &msg {
                        ValueApplyError::InvalidOperand
                            if matches!(op, BinaryOp::Shl | BinaryOp::Shr) =>
                        {
                            context.reporter.report_error(
                                right.get_span(),
                                format_args!(
                                    "Negative shift amount in compile-time '{}' expression",
                                    op
                                ),
                            );
                        }
                        ValueApplyError::InvalidOperand => {
                            context.reporter.report_error(
                                right.get_span(),
                                format_args!("Invalid operand in compile-time expression"),
                            );
                        }
                        ValueApplyError::DivisionByZero => {
                            // Recoverable
                            context.reporter.report_warning(
                                &location,
                                format_args!("Compile-time '{}' by zero", op),
                            );
                        }
                        other => {
                            // Other, non specialized message
                            context
                                .reporter
                                .report_error(&location, format_args!("{}", other));
                        }
                    }

                    Err(msg)
                } else {
                    Ok(result.ok())
                }
            }
            ExprKind::UnaryOp {
                op: (op, location),
                right,
            } => {
                let rvalue = if let Some(value) = self.eval_expr(right)? {
                    value
                } else {
                    return Ok(None);
                };

                let result =
                    value::apply_unary(*op, rvalue, value::EvalConstraints { as_64_bit: false });

                if let Err(msg) = result {
                    let context = self.context.borrow_mut();

                    match &msg {
                        ValueApplyError::Overflow => {
                            context.reporter.report_error(
                                &right.get_span(),
                                format_args!("Overflow in compile-time expression"),
                            );
                        }
                        other => {
                            context
                                .reporter
                                .report_error(&location, format_args!("{}", other));
                        }
                    }

                    Err(msg)
                } else {
                    Ok(result.ok())
                }
            }
            ExprKind::Dot { left, field } => {
                // Enum, or known const

                // Dealias left type
                let left_ref = ty::dealias_ref(&left.get_eval_type(), &self.type_table);
                let field = &field.0;

                if ty::is_error(&left_ref) {
                    // Is a type error, no value
                    return Ok(None);
                }

                if let Some(type_info) = self.type_table.type_from_ref(&left_ref) {
                    // Match based on the type info
                    match type_info {
                        Type::Enum { fields, .. } => {
                            let enum_field = fields.get(&field.name);
                            // Check if the field is a part of the compound type
                            if let Some(field_ref) = enum_field {
                                // Build Enum value
                                let enum_id = ty::get_type_id(&left_ref).unwrap();
                                let field_id = ty::get_type_id(&field_ref).unwrap();
                                let ordinal = if let Type::EnumField { ordinal, .. } =
                                    self.type_table.get_type(field_id)
                                {
                                    *ordinal
                                } else {
                                    0
                                };

                                Ok(Some(Value::EnumValue(field_id, enum_id, ordinal)))
                            } else {
                                unreachable!("Validated to be an enum field")
                            }
                        }
                        _ => Ok(None),
                    }
                } else {
                    unreachable!("Is compile eval but no associated dot/arrow conversion")
                }
            }
            ExprKind::Reference { ident } => {
                // From `compile_values map`
                Ok(self.compile_values.get(&ident.id).cloned())
            }
            ExprKind::Literal { value } => {
                // Known literal
                if matches!(value, Literal::Nil) {
                    // Don't produce a compile-time value for 'nil';
                    Ok(None)
                } else {
                    // Produce the corresponding literal value
                    let v = Value::from_literal(value.clone()).unwrap_or_else(|msg| {
                        panic!(
                            "Literal '{:?}' cannot be converted into a compile-time value ({})",
                            value, msg
                        );
                    });

                    Ok(Some(v))
                }
            }
            ExprKind::Call { .. } => {
                // Only builtins & known predefs would produce a value
                // TODO: Add compile-time evals for known built-ins
                Ok(None)
            }
            _ => {
                // No Compile-time value produced
                Ok(None)
            }
        }
    }
}

impl VisitorMut<(), ()> for Validator {
    fn visit_stmt(&mut self, visit_stmt: &mut Stmt) {
        match &mut visit_stmt.kind {
            StmtKind::Nop | StmtKind::Error => {}
            StmtKind::VarDecl {
                idents,
                type_spec,
                value,
                is_const,
            } => self.resolve_decl_var(idents, type_spec, value, *is_const),
            StmtKind::TypeDecl {
                ident,
                resolved_type,
                is_new_def,
            } => self.resolve_decl_type(ident, resolved_type, *is_new_def),
            StmtKind::Assign { var_ref, op, value } => {
                self.resolve_stmt_assign(var_ref, op.as_mut(), value)
            }
            StmtKind::ProcedureCall { proc_ref } => {
                if let ExprKind::Call {
                    left,
                    paren_at,
                    arg_list,
                    ..
                } = &mut proc_ref.kind
                {
                    // Defer to expression resolution
                    self.resolve_expr_call(
                        left,
                        paren_at,
                        arg_list,
                        &mut proc_ref.eval_type,
                        &mut proc_ref.is_compile_eval,
                        true,
                    );
                } else {
                    unreachable!();
                }
            }
            StmtKind::Block { block } => self.resolve_stmt_block(block),
            StmtKind::If {
                condition,
                true_branch,
                false_branch,
            } => self.resolve_stmt_if(condition, true_branch, false_branch),
        }
    }

    // Note: If the eval_type is still TypeRef::Unknown, propagate the type error
    fn visit_expr(&mut self, visit_expr: &mut Expr) {
        match &mut visit_expr.kind {
            ExprKind::Error => {}
            ExprKind::Parens { inner } => self.resolve_expr_parens(
                inner,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
            ),
            ExprKind::Init { exprs, .. } => self.resolve_expr_init(exprs),
            ExprKind::Indirect {
                reference, addr, ..
            } => self.resolve_expr_indirect(reference, addr, &mut visit_expr.eval_type),
            ExprKind::BinaryOp {
                left, op, right, ..
            } => self.resolve_expr_binary(
                left,
                op,
                right,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
            ),
            ExprKind::UnaryOp { op, right, .. } => self.resolve_expr_unary(
                op,
                right,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
            ),
            ExprKind::Call {
                left,
                paren_at,
                arg_list,
                ..
            } => self.resolve_expr_call(
                left,
                paren_at,
                arg_list,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
                false,
            ),
            ExprKind::Dot { left, field, .. } => self.resolve_expr_dot(
                left,
                field,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
            ),
            ExprKind::Arrow { left, field, .. } => self.resolve_expr_arrow(
                left,
                field,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
            ),
            ExprKind::Reference { ident } => self.resolve_expr_reference(
                ident,
                &mut visit_expr.eval_type,
                &mut visit_expr.is_compile_eval,
            ),
            ExprKind::Literal { value, .. } => {
                self.resolve_expr_literal(value, &mut visit_expr.eval_type)
            }
        }
    }
}

// --- Helpers --- //

/// Replaces an expression with the folded version of the value
///
/// Appropriately carries over the associated expr's span
fn replace_with_folded(expr: &mut Expr, folded_expr: Option<Value>) {
    if let Some(value) = folded_expr {
        // Conversion is infalliable (should be using either Into or From)
        let value_expr = Expr::try_from(value).unwrap();
        expr.kind = value_expr.kind;
        expr.eval_type = value_expr.eval_type;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::context::CompileContext;
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use rand::prelude::*;
    use std::{cell::RefCell, rc::Rc};
    use toc_ast::ast::ident::Identifier;
    use toc_ast::block::CodeUnit;
    use toc_ast::types::{PrimitiveType, SequenceSize};

    /// Makes and runs a validator
    /// Parsing & scanning must complete successfully
    /// Returns true if the AST is valid, and the validated code unit
    fn make_validator(source: &str) -> (bool, CodeUnit) {
        // Build the main unit
        let context = Rc::new(RefCell::new(CompileContext::new()));
        let scanner = Scanner::scan_source(&source, context.clone());

        // Ignore the parser status, as the validator needs to handle
        // invalid parser ASTs
        let mut parser = Parser::new(scanner, &source, true, context);
        let successful_parse = parser.parse();

        // Take the unit back from the parser
        let mut code_unit = parser.take_unit();
        let type_table = code_unit.take_types();
        let unit_scope = code_unit.take_unit_scope();

        // Validate AST
        let validator_context = Rc::new(RefCell::new(CompileContext::new()));
        let mut validator = Validator::new(unit_scope, type_table, validator_context.clone());
        code_unit.visit_ast_mut(&mut validator);

        let (type_table, unit_scope) = validator.take_code_unit_parts();
        code_unit.put_types(type_table);
        code_unit.put_unit_scope(unit_scope);

        // Ok for now until we start doing funky stuff with contexts
        // TODO: Use a unified context when using file-based test harness
        let successful_validate = !validator_context.borrow().reporter.has_error();

        (successful_validate && successful_parse, code_unit)
    }

    /// Runs the validator on the given source
    /// Parsing & scanning must complete successfully
    /// Returns true if the AST is valid
    fn run_validator(source: &str) -> bool {
        make_validator(source).0
    }

    fn get_ident_for_unit<'u>(unit: &'u CodeUnit, name: &str) -> Option<&'u Identifier> {
        let id = unit.root_block().block.get_ident_id(name)?;
        let info = unit.unit_scope().get_ident_info(&id);
        Some(&info)
    }

    fn get_info_for_unit(unit: &'_ CodeUnit, id: IdentId) -> &'_ Identifier {
        unit.unit_scope().get_ident_info(&id)
    }

    #[test]
    fn test_empty_file() {
        assert!(run_validator(""));
    }

    #[test]
    fn test_simple_asn_typecheck() {
        // Const refs & type defs aren't assignable (const only assignable at init)
        assert_eq!(false, run_validator("const a : int := 1\na := 2"));
        assert_eq!(false, run_validator("type a : enum(a, b, c)\na.b := 2"));
        assert_eq!(false, run_validator("type a : enum(a, b, c)\na := 2"));

        // Pointers derefs are asssignable if the pointer ref is a var or const ref
        assert_eq!(true, run_validator("var a : ^int\n^a := 2"));
        assert_eq!(true, run_validator("var a : ^^int\n^^a := 2"));
        assert_eq!(
            true,
            run_validator("var src : ^int\nvar a : ^^int\n^a := src")
        );
        assert_eq!(
            true,
            run_validator("var src : ^int\nconst a : ^int := src\n^a := 2")
        );
        assert_eq!(
            true,
            run_validator("var src : ^^int\nconst a : ^^int := src\n^^a := 2")
        );
        assert_eq!(
            true,
            run_validator("var src1 : ^int\nvar src2 : ^^int\nconst a : ^^int := src2\n^a := src1")
        );

        assert_eq!(false, run_validator("type a : ^int\n^a := 2"));

        // TODO: Check arrow operator using records

        // Array subscripts are only assignable if they are a variable
        // TODO: above

        // Call exprs aren't directly assignable
        // TODO: above

        // Also tests type compatibility
        // Basic, unsized types
        assert_eq!(true, run_validator("var a : int  := 1"));
        assert_eq!(true, run_validator("var a : nat  := 1"));
        assert_eq!(true, run_validator("var a : real := 1.0"));
        assert_eq!(true, run_validator("var a : string := \"Some text\""));

        // Compatibility between char and char(1)
        assert_eq!(true, run_validator("var a : char := 'a'"));
        assert_eq!(true, run_validator("var a : char(1) := 'a'"));
        assert_eq!(
            true,
            run_validator("var c : char := 'c'\nvar a : char(1) := c")
        );
        assert_eq!(
            true,
            run_validator("var c : char(1) := 'c'\nvar a : char := c")
        );

        // Compatibility between char and string(1)
        assert_eq!(true, run_validator("var a : char := 'a'"));
        assert_eq!(true, run_validator("var a : string(1) := 'a'"));
        assert_eq!(
            true,
            run_validator("var c : char := 'c'\nvar a : string(1) := c")
        );
        assert_eq!(
            true,
            run_validator("var c : string(1) := 'c'\nvar a : char := c")
        );

        // Compatibility between char and string
        assert_eq!(true, run_validator("var a : char := \"a\""));
        assert_eq!(true, run_validator("var a : string := 'a'"));
        assert_eq!(
            true,
            run_validator("var c : char := 'c'\nvar a : string := c")
        );
        assert_eq!(
            true,
            run_validator("var c : string := 'c'\nvar a : char := c")
        );

        // Incompatibility between char with char(n) and string(n)
        assert_eq!(
            false,
            run_validator("var c : char(2) := 'ce'\nvar a : char := c")
        );
        assert_eq!(
            false,
            run_validator("var c : string(2) := 'ce'\nvar a : char := c")
        );
        assert_eq!(
            false,
            run_validator("var c : char(2) := 'ce'\nvar a : char\na := c")
        );
        assert_eq!(
            false,
            run_validator("var c : string(2) := 'ce'\nvar a : char\na := c")
        );

        // Compatibility with char into char(n) and string(n)
        assert_eq!(
            true,
            run_validator("var c : char := 'c'\nvar a : char(6) := c")
        );
        assert_eq!(
            true,
            run_validator("var c : char := 'c'\nvar a : string(6) := c")
        );

        // Compatibility between char(n) and string
        // char(n) <- string is only checked at runtime
        assert_eq!(
            true,
            run_validator("var s : string := \"abcd\"\nvar a : char(6) := s")
        );
        assert_eq!(
            true,
            run_validator("var c : char(6) := 'abcd'\nvar a : string := c")
        );

        // (In)compatibility between char(n) of same or different size
        assert_eq!(true, run_validator("var a : char(6) := 'abcd'"));
        assert_eq!(true, run_validator("var a : char(6) := 'abcdaa'"));
        assert_eq!(false, run_validator("var a : char(6) := 'abcdaaa'"));
        assert_eq!(true, run_validator("var a : char(6)\na := 'abcd'"));
        assert_eq!(true, run_validator("var a : char(6)\na := 'abcdaa'"));
        assert_eq!(false, run_validator("var a : char(6)\na := 'abcdaaa'"));

        // (In)compatibility between string(n) of same or different size
        assert_eq!(
            true,
            run_validator("var s : string(4) := 'abcd'    \nvar a : string(6) := s")
        );
        assert_eq!(
            true,
            run_validator("var s : string(6) := 'abcdaa'  \nvar a : string(6) := s")
        );
        assert_eq!(
            false,
            run_validator("var s : string(7) := 'abcdaaa'\nvar a : string(6) := s")
        );
        assert_eq!(
            true,
            run_validator("var s : string(4) := 'abcd'    \nvar a : string(6)\na := s")
        );
        assert_eq!(
            true,
            run_validator("var s : string(6) := 'abcdaa'  \nvar a : string(6)\na := s")
        );
        assert_eq!(
            false,
            run_validator("var s : string(7) := 'abcdaaa'\nvar a : string(6)\na := s")
        );

        // Compatibility between real and number types
        assert_eq!(
            true,
            run_validator("var i : int := 1   \nvar a : real := i")
        );
        assert_eq!(
            true,
            run_validator("var i : nat := 1   \nvar a : real := i")
        );
        assert_eq!(
            true,
            run_validator("var i : real := 1.0\nvar a : real := i")
        );

        // Incompatibility between real and integers
        assert_eq!(false, run_validator("var i : int := 1.0"));
        assert_eq!(false, run_validator("var i : nat := 1.0"));
        assert_eq!(false, run_validator("var i : int\ni := 1.0"));
        assert_eq!(false, run_validator("var i : nat\ni := 1.0"));

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
        assert_eq!(false, run_validator("var i : int \ni := \"text\""));
        assert_eq!(false, run_validator("var i : nat \ni := \"text\""));
        assert_eq!(false, run_validator("var i : real\ni := \"text\""));
        assert_eq!(false, run_validator("var i : int \ni := 'text'"));
        assert_eq!(false, run_validator("var i : nat \ni := 'text'"));
        assert_eq!(false, run_validator("var i : real\ni := 'text'"));
        assert_eq!(false, run_validator("var i : int \ni := 't'"));
        assert_eq!(false, run_validator("var i : nat \ni := 't'"));
        assert_eq!(false, run_validator("var i : real\ni := 't'"));

        // Incompatibility between numbers and booleans
        assert_eq!(false, run_validator("var i : int  := false"));
        assert_eq!(false, run_validator("var i : nat  := false"));
        assert_eq!(false, run_validator("var i : real := false"));
        assert_eq!(false, run_validator("var i : int \ni := false"));
        assert_eq!(false, run_validator("var i : nat \ni := false"));
        assert_eq!(false, run_validator("var i : real\ni := false"));

        // Compatibility between booleans
        assert_eq!(true, run_validator("var i : boolean := false"));

        // Incompatibility between booleans and non-booleans
        assert_eq!(false, run_validator("var i : boolean := 1"));
        assert_eq!(false, run_validator("var i : boolean := 1.0"));
        assert_eq!(false, run_validator("var i : boolean := \"h\""));
        assert_eq!(false, run_validator("var i : boolean := 'ha'"));
        assert_eq!(false, run_validator("var i : boolean := 'h'"));
        assert_eq!(false, run_validator("var i : boolean\ni := 1"));
        assert_eq!(false, run_validator("var i : boolean\ni := 1.0"));
        assert_eq!(false, run_validator("var i : boolean\ni := \"h\""));
        assert_eq!(false, run_validator("var i : boolean\ni := 'ha'"));
        assert_eq!(false, run_validator("var i : boolean\ni := 'h'"));

        // Compatibility with ranges
        assert_eq!(true, run_validator("var i : false .. true := true"));
        assert_eq!(true, run_validator("var i : 0 .. 8 := 5"));
        assert_eq!(true, run_validator("var i : 'd' .. 'g' := 'f'"));
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\nvar i : e0.a .. e0.b := e0.a")
        );

        // Comptibility with equivalent sets
        assert_eq!(
            true,
            run_validator(
                "type s0 : set of 1 .. 3 \ntype s1 : set of 1 .. 3\nvar a : s0\nvar b : s1 := a"
            )
        );
        assert_eq!(true,  run_validator("type e0 : enum(a, b, c, d)\ntype s0 : set of e0\ntype s1 : set of e0\nvar a : s0\nvar b : s1 := a"));

        // Incompatibilty with sets of incompatible ranges
        assert_eq!(
            false,
            run_validator(
                "type s0 : set of 1 .. 4 \ntype s1 : set of 1 .. 3\nvar a : s0\nvar b : s1 := a"
            )
        );
        assert_eq!(
            false,
            run_validator(
                "type s0 : set of 1 .. 3 \ntype s1 : set of 2 .. 3\nvar a : s0\nvar b : s1 := a"
            )
        );
        assert_eq!(
            false,
            run_validator(
                "type s0 : set of 1 .. 3 \ntype s1 : set of char  \nvar a : s0\nvar b : s1 := a"
            )
        );
        assert_eq!(
            false,
            run_validator(
                "type s0 : set of boolean\ntype s1 : set of char  \nvar a : s0\nvar b : s1 := a"
            )
        );

        assert_eq!(false, run_validator("type e0 : enum(a, b, c, d)\ntype s0 : set of e0.a .. e0.c\ntype s1 : set of e0     \nvar a : s0\nvar b : s1 := a"));
        assert_eq!(false, run_validator("type e0 : enum(a, b, c, d)\ntype s0 : set of e0.a .. e0.c\ntype s1 : set of boolean\nvar a : s0\nvar b : s1 := a"));
        assert_eq!(false, run_validator("type e0 : enum(a, b, c, d)\ntype s0 : set of e0.a .. e0.c\ntype s1 : set of char   \nvar a : s0\nvar b : s1 := a"));
        assert_eq!(false, run_validator("type e0 : enum(a)\ntype e1 : enum(a)\ntype s0 : set of e0.a .. e0.a\ntype s1 : set of e1.a .. e1.a\nvar a : s0\nvar b : s1 := a"));
        assert_eq!(false, run_validator("type e0 : enum(a)\ntype e1 : enum(a)\ntype s0 : set of e0.a .. e0.a\ntype s1 : set of e1          \nvar a : s0\nvar b : s1 := a"));
        assert_eq!(false, run_validator("type e0 : enum(a)\ntype e1 : enum(a)\ntype s0 : set of e0          \ntype s1 : set of e1.a .. e1.a\nvar a : s0\nvar b : s1 := a"));
        assert_eq!(false, run_validator("type e0 : enum(a)\ntype e1 : enum(a)\ntype s0 : set of e0          \ntype s1 : set of e1          \nvar a : s0\nvar b : s1 := a"));

        // Compatibility between enums of same root type declaration
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\nvar a : e0\nvar b : e0 := a")
        );
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : e0\nvar a : e0\nvar b : e1 := a")
        );
        assert_eq!(
            true,
            run_validator(
                "type e0 : enum(a, b, c)\ntype e1 : e0\nvar a : e0 := e1.a\nvar b : e1 := a"
            )
        );

        // Incompatibility between enums of different root type declaration (even with same field names and ordering)
        assert_eq!(
            false,
            run_validator(
                "type e0 : enum(a, b, c)\ntype e1 : enum(a, b, c)\nvar a : e0\nvar b : e1 := a"
            )
        );
        assert_eq!(
            false,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : enum(a, b, c)\nvar a : e0 := e1.a")
        );
        assert_eq!(
            false,
            run_validator(
                "type e0 : enum(a, b, c)\ntype e1 : enum(a, b, c)\nvar a : e0\nvar b : e1\nb := a"
            )
        );
        assert_eq!(
            false,
            run_validator(
                "type e0 : enum(a, b, c)\ntype e1 : enum(a, b, c)\nvar a : e0\na := e1.a"
            )
        );

        // Compatibility between enum and fields of same root type declaration
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\nvar a : e0 := e0.a")
        );
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : e0\nvar a : e0 := e0.a")
        );
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : e0\nvar a : e0 := e1.a")
        );
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : e0\nvar a : e1 := e0.a")
        );
        assert_eq!(
            true,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : e0\nvar a : e1 := e1.a")
        );

        // Incompatibility between enum and fields of different root type declaration
        assert_eq!(
            false,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : enum(a, b, c)\nvar a : e0 := e1.a")
        );
        assert_eq!(
            false,
            run_validator("type e0 : enum(a, b, c)\ntype e1 : enum(a, b, c)\nvar a : e1 := e0.a")
        );

        // Range types are assignable into compatible base type
        assert_eq!(true, run_validator("var a : 1 .. 3\nvar b : int := a"));
        assert_eq!(true, run_validator("var a : int\nvar b : 1 .. 3 := a"));
        assert_eq!(true, run_validator("var a : 1 .. 3\nvar b : real := a"));
        assert_eq!(false, run_validator("var a : real\nvar b : 1 .. 3 := a"));

        // Type refs are never assignable or used as an assignment value
        assert_eq!(false, run_validator("type a : int\na := 1"));
        assert_eq!(false, run_validator("type a : int\nvar b : int := a"));
    }

    #[test]
    fn test_binary_no_typerefs() {
        // Check for invalid use of type refs in binary exprs
        assert_eq!(
            false,
            run_validator("type a : int\nvar b : int\nb := a + b")
        );
        assert_eq!(
            false,
            run_validator("type a : int\nvar b : int\nb := b + a")
        );
    }

    #[test]
    fn test_add_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 + 1  \na +=   1 + 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 + 1  \na +=   1 + 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 + 1  \na +=   1 + 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 + 1  \na += 1.0 + 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 + 1.0\na +=   1 + 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 + 1.0\na += 1.0 + 1.0")
        );
        assert_eq!(
            true,
            run_validator(
                "var a : string := \"Hello, \" + \"World!\"\na += \"Hello, \" + \"World!\""
            )
        );
        assert_eq!(
            true,
            run_validator(
                "type s : set of 1 .. 3\nvar a : s\nvar b : s\nvar c : s := a + b\nc += a"
            )
        );
        assert_eq!(
            true,
            run_validator("type s : set of char\nvar a : s\nvar b : s\nvar c : s := a + b\nc += a")
        );
        assert_eq!(
            true,
            run_validator(
                "type s : set of boolean\nvar a : s\nvar b : s\nvar c : s := a + b\nc += a"
            )
        );

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
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar c : t\nc += a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar c : t\nc += a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar c : t\nc += a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar b : t\nvar c := a + b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar c : t\nc += a"
            )
        );
    }

    #[test]
    fn test_sub_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 - 1  \na -=   1 - 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 - 1  \na -=   1 - 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 - 1  \na -=   1 - 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 - 1  \na -= 1.0 - 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 - 1.0\na -=   1 - 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 - 1.0\na -= 1.0 - 1.0")
        );
        assert_eq!(
            true,
            run_validator(
                "type s : set of 1 .. 3\nvar a : s\nvar b : s\nvar c : s := a - b\nc -= a"
            )
        );
        assert_eq!(
            true,
            run_validator("type s : set of char\nvar a : s\nvar b : s\nvar c : s := a - b\nc -= a")
        );
        assert_eq!(
            true,
            run_validator(
                "type s : set of boolean\nvar a : s\nvar b : s\nvar c : s := a - b\nc -= a"
            )
        );

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 - 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na -= 1.0"));

        // Set difference not applicable to non-equivalent ranges / indexes
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar c : t\nc -= a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar c : t\nc -= a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar c : t\nc -= a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar b : t\nvar c := a - b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar c : t\nc -= a"
            )
        );

        // Not scalars or sets
        assert_eq!(false, run_validator("var a : int := \"str\" - \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    -= \"str\""));
    }

    #[test]
    fn test_mul_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 * 1  \na *=   1 * 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 * 1  \na *=   1 * 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 * 1  \na *=   1 * 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 * 1  \na *= 1.0 * 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 * 1.0\na *=   1 * 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 * 1.0\na *= 1.0 * 1.0")
        );
        assert_eq!(
            true,
            run_validator(
                "type s : set of 1 .. 3\nvar a : s\nvar b : s\nvar c : s := a * b\nc *= a"
            )
        );
        assert_eq!(
            true,
            run_validator("type s : set of char\nvar a : s\nvar b : s\nvar c : s := a * b\nc *= a")
        );
        assert_eq!(
            true,
            run_validator(
                "type s : set of boolean\nvar a : s\nvar b : s\nvar c : s := a * b\nc *= a"
            )
        );

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 * 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na *= 1.0"));

        // Set intersection not applicable to non-equivalent ranges / indexes
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of 1 .. 4\nvar a : s\nvar c : t\nc *= a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of 0 .. 3\nvar a : s\nvar c : t\nc *= a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of char\nvar a : s\nvar c : t\nc *= a"
            )
        );
        assert_eq!(false, run_validator("type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar b : t\nvar c := a * b"));
        assert_eq!(
            false,
            run_validator(
                "type s : set of 1 .. 3\ntype t : set of boolean\nvar a : s\nvar c : t\nc *= a"
            )
        );

        // Not scalars or sets
        assert_eq!(false, run_validator("var a : int := \"str\" * \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    *= \"str\""));
    }

    #[test]
    fn test_idiv_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 div 1  \na div=   1 div 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 div 1  \na div=   1 div 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 div 1  \na div=   1 div 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 div 1  \na div= 1.0 div 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 div 1.0\na div=   1 div 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 div 1.0\na div= 1.0 div 1.0")
        );

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
        assert_eq!(
            true,
            run_validator("var a : real :=   1 / 1  \na /=   1 / 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 / 1  \na /= 1.0 / 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 / 1.0\na /=   1 / 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 / 1.0\na /= 1.0 / 1.0")
        );

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
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 mod 1  \na mod=   1 mod 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 mod 1  \na mod=   1 mod 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 mod 1  \na mod=   1 mod 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 mod 1  \na mod= 1.0 mod 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 mod 1.0\na mod=   1 mod 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 mod 1.0\na mod= 1.0 mod 1.0")
        );

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
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 rem 1  \na rem=   1 rem 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 rem 1  \na rem=   1 rem 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 rem 1  \na rem=   1 rem 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 rem 1  \na rem= 1.0 rem 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 rem 1.0\na rem=   1 rem 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 rem 1.0\na rem= 1.0 rem 1.0")
        );

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1    rem 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na rem= 1.0"));
    }

    #[test]
    fn test_exp_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=   1 ** 1  \na **=   1 ** 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=   1 ** 1  \na **=   1 ** 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 ** 1  \na **=   1 ** 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 ** 1  \na **= 1.0 ** 1  ")
        );
        assert_eq!(
            true,
            run_validator("var a : real :=   1 ** 1.0\na **=   1 ** 1.0")
        );
        assert_eq!(
            true,
            run_validator("var a : real := 1.0 ** 1.0\na **= 1.0 ** 1.0")
        );

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
        assert_eq!(
            true,
            run_validator("var a : int  :=        1 and 1       \na and=    1 and 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=        1 and 1       \na and=    1 and 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : boolean := false and true    \na and= true and true")
        );

        // Not integers
        assert_eq!(false, run_validator("var a : int := \"str\" and \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    and= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      and  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   and= 1.0"));

        // Not matching types
        assert_eq!(false, run_validator("var a : boolean := 1       and  1"));
        assert_eq!(
            false,
            run_validator("var a : boolean := true\na and= 1    ")
        );
        assert_eq!(
            false,
            run_validator("var a : nat     := true    and  false")
        );
        assert_eq!(
            false,
            run_validator("var a : nat     := 1\na    and= true ")
        );
    }

    #[test]
    fn test_or_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=        1 or 1       \na or=    1 or 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=        1 or 1       \na or=    1 or 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : boolean := false or true    \na or= true or true")
        );

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
        assert_eq!(
            true,
            run_validator("var a : int  :=        1 xor 1       \na xor=    1 xor 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=        1 xor 1       \na xor=    1 xor 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : boolean := false xor true    \na xor= true xor true")
        );

        // Not integers
        assert_eq!(false, run_validator("var a : int := \"str\" xor \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    xor= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      xor  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   xor= 1.0"));

        // Not matching types
        assert_eq!(
            false,
            run_validator("var a : boolean := 1       xor  1    ")
        );
        assert_eq!(
            false,
            run_validator("var a : boolean := true\na xor= 1    ")
        );
        assert_eq!(
            false,
            run_validator("var a : nat     := true    xor  false")
        );
        assert_eq!(
            false,
            run_validator("var a : nat     := 1\na    xor= true ")
        );
    }

    #[test]
    fn test_shl_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=        1 shl 1       \na shl=    1 shl 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=        1 shl 1       \na shl=    1 shl 1   ")
        );

        // Not integers
        assert_eq!(
            false,
            run_validator("var a : int    := \"str\"  shl \"str\"")
        );
        assert_eq!(
            false,
            run_validator("var a : int    := 1\na     shl= \"str\"")
        );

        assert_eq!(false, run_validator("var a : real    := 1       shl  1.0"));
        assert_eq!(false, run_validator("var a : real    := 1\na    shl= 1.0"));

        assert_eq!(false, run_validator("var a : boolean := true    shl  true"));
        assert_eq!(false, run_validator("var a : boolean := true\na shl= true"));
    }

    #[test]
    fn test_shr_typecheck() {
        // Tests typechecking for both the binary operator and the combined assignment
        assert_eq!(
            true,
            run_validator("var a : int  :=        1 shr 1       \na shr=    1 shr 1   ")
        );
        assert_eq!(
            true,
            run_validator("var a : nat  :=        1 shr 1       \na shr=    1 shr 1   ")
        );

        // Not integers
        assert_eq!(
            false,
            run_validator("var a : int    := \"str\"  shr \"str\"")
        );
        assert_eq!(
            false,
            run_validator("var a : int    := 1\na     shr= \"str\"")
        );

        assert_eq!(false, run_validator("var a : real    := 1       shr  1.0"));
        assert_eq!(false, run_validator("var a : real    := 1\na    shr= 1.0"));

        assert_eq!(false, run_validator("var a : boolean := true    shr  true"));
        assert_eq!(false, run_validator("var a : boolean := true\na shr= true"));
    }

    fn test_compare_operator_typecheck(compare_op: &str) {
        // Tests typechecking for the binary operator
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean := 1.0 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1.0", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean := 1.0 {} 1.0", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "var a : boolean := \"Hello, \" {} \"World!\"",
                compare_op
            ))
        );
        assert_eq!(true, run_validator(&format!("type s0 : set of 1 .. 3\ntype s1 : set of 1 .. 3\nvar a : s0\nvar b : s1\nvar c : boolean := a {} b", compare_op)));
        // Missing: objectclass compares

        // Comparison operands are applicable (and foldable) to enum fields behind constants
        assert_eq!(
            true,
            run_validator(&format!(
                "type e0 : enum(a, b)\nconst a : e0 := e0.b\nvar c : boolean := a {} e0.a",
                compare_op
            ))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "type e0 : enum(a, b)\nconst a : e0 := e0.b\nvar c : boolean := e0.a {} a",
                compare_op
            ))
        );

        // Comparison operands must be the same type (class)
        // bool is whether to always reject
        let type_variants = [
            (
                false,
                vec!["int := 1", "nat := 1", "real := 1.0", "real := 1"],
            ),
            (
                false,
                vec![
                    "string := \"Hello!\"",
                    "char := 'c'",
                    "char(3) := 'cd'",
                    "string(5) := 'cdefg'",
                ],
            ),
            (false, vec!["s0", "s1", "s2", "s3"]),
            (
                true,
                vec![
                    "boolean := true",
                    "boolean := false",
                    "boolean := true and false",
                ],
            ),
            (
                false,
                vec![
                    "e0 := e0.a",
                    "e0 := e0.b",
                    "e0 := e0.c",
                    "e0 := e0.d",
                    "e1 := e1.e",
                    "e1 := e1.f",
                    "e1 := e1.g",
                    "e1 := e1.h",
                ],
            ),
            (true, vec!["alt"]),
        ];

        let mut rng = thread_rng();

        for _ in 0..150 {
            // stuff with randomness
            let left_variant_class = rng.gen_range(0, type_variants.len());
            let right_variant_class = rng.gen_range(0, type_variants.len());
            let accept = left_variant_class == right_variant_class
                && !type_variants[left_variant_class].0
                && !type_variants[right_variant_class].0;
            let test_code = format!(
                "type alt : proc _ ()
                type s0 : set of 1 .. 3
                type s1 : set of 1 .. 3
                type s2 : set of char
                type s3 : set of boolean
                type e0 : enum(a, b, c, d)
                type e1 : enum(e, f, g, h)
                var a : {}
                var b : {}
                var c : boolean := a {} b",
                type_variants[left_variant_class]
                    .1
                    .iter()
                    .choose(&mut rng)
                    .unwrap(),
                type_variants[right_variant_class]
                    .1
                    .iter()
                    .choose(&mut rng)
                    .unwrap(),
                compare_op
            );

            assert_eq!(
                accept,
                run_validator(&test_code),
                "Failed on generated test '\n{}'",
                test_code
            );
        }
    }

    fn test_equality_operator_typecheck(compare_op: &str) {
        // Tests typechecking for the binary operator
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean := 1.0 {} 1  ", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean :=   1 {} 1.0", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean := 1.0 {} 1.0", compare_op))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "var a : boolean := \"Hello, \" {} \"World!\"",
                compare_op
            ))
        );
        assert_eq!(true, run_validator(&format!("type s0 : set of 1 .. 3\ntype s1 : set of 1 .. 3\nvar a : s0\nvar b : s1\nvar c : boolean := a {} b", compare_op)));
        assert_eq!(
            true,
            run_validator(&format!("var a : boolean := true {} true", compare_op))
        );
        // Missing: enum, objectclass & ptr compares

        // Equality operands must be the same type (class)
        // bool is whether to always reject
        let type_variants = [
            (
                false,
                vec!["int := 1", "nat := 1", "real := 1.0", "real := 1"],
            ),
            (
                false,
                vec![
                    "string := \"Hello!\"",
                    "char := 'c'",
                    "char(3) := 'cd'",
                    "string(5) := 'cdefg'",
                ],
            ),
            (false, vec!["s0", "s1", "s2", "s3"]),
            (
                false,
                vec![
                    "boolean := true",
                    "boolean := false",
                    "boolean := true and false",
                ],
            ),
            (
                false,
                vec![
                    "^int",
                    "^nat",
                    "pointer to string",
                    "pointer to int",
                    "pointer to nat",
                ],
            ),
            (
                false,
                vec![
                    "unchecked ^int",
                    "unchecked ^nat",
                    "unchecked pointer to string",
                    "unchecked pointer to int",
                    "unchecked pointer to nat",
                ],
            ),
            (
                false,
                vec![
                    "e0 := e0.a",
                    "e0 := e0.b",
                    "e0 := e0.c",
                    "e0 := e0.d",
                    "e1 := e1.e",
                    "e1 := e1.f",
                    "e1 := e1.g",
                    "e1 := e1.h",
                ],
            ),
            (true, vec!["alt"]),
        ];

        let mut rng = thread_rng();

        for _ in 0..150 {
            // stuff with randomness
            let left_variant_class = rng.gen_range(0, type_variants.len());
            let right_variant_class = rng.gen_range(0, type_variants.len());
            let accept = left_variant_class == right_variant_class
                && !type_variants[left_variant_class].0
                && !type_variants[right_variant_class].0;
            let test_code = format!(
                "type alt : proc _ ()
                type s0 : set of 1 .. 3
                type s1 : set of 1 .. 3
                type s2 : set of char
                type s3 : set of boolean
                type e0 : enum(a, b, c, d)
                type e1 : enum(e, f, g, h)
                var a : {}
                var b : {}
                var c : boolean := a {} b",
                type_variants[left_variant_class]
                    .1
                    .iter()
                    .choose(&mut rng)
                    .unwrap(),
                type_variants[right_variant_class]
                    .1
                    .iter()
                    .choose(&mut rng)
                    .unwrap(),
                compare_op
            );

            assert_eq!(
                accept,
                run_validator(&test_code),
                "Failed on generated test '\n{}'",
                test_code
            );
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
        assert_eq!(
            true,
            run_validator(&format!(
                "type s : set of 1 .. 3 \nvar a : s\nvar b := 1 {} a",
                variant
            ))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "type s : set of char   \nvar a : s\nvar b := 'a' {} a",
                variant
            ))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "type s : set of char   \nvar a : s\nvar c : char := 'c'\nvar b := c {} a",
                variant
            ))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "type s : set of boolean\nvar a : s\nvar b := true {} a",
                variant
            ))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "type e0 : enum(a, b)\ntype s : set of e0\nvar a : s\nvar b := e0.a {} a",
                variant
            ))
        );
        assert_eq!(
            true,
            run_validator(&format!(
                "type e0 : enum(a, b)\ntype s : set of e0.a .. e0.b\nvar a : s\nvar b := e0.a {} a",
                variant
            ))
        );
        assert_eq!(true, run_validator(&format!(
            "type e0 : enum(a, b)\nconst c : e0 := e0.b\ntype s : set of e0.a .. c\nvar a : s\nvar b := c {} a",
            variant
        )));
        assert_eq!(true, run_validator(&format!(
            "type e0 : enum(a, b)\nconst c := e0.b\ntype s : set of e0.a .. c\nvar a : s\nvar b := c {} a",
            variant
        )));
        assert_eq!(true, run_validator(&format!(
            "type e0 : enum(a, b)\nconst c : e0 := e0.b\ntype s : set of e0\nvar a : s\nvar b := c {} a",
            variant
        )));
        assert_eq!(
            true,
            run_validator(&format!(
            "type e0 : enum(a, b)\nconst c := e0.b\ntype s : set of e0\nvar a : s\nvar b := c {} a",
            variant
        ))
        );

        // Right operand must be a set
        assert_eq!(
            false,
            run_validator(&format!("var a : int\nvar b := 1 {} a", variant))
        );
        assert_eq!(
            false,
            run_validator(&format!("var a : string\nvar b := 'a' {} a", variant))
        );
        assert_eq!(
            false,
            run_validator(&format!("var a : char\nvar b := true {} a", variant))
        );
        assert_eq!(
            false,
            run_validator(&format!(
                "type e0 : enum(a, b)\nvar a : e0\nvar b := e0.a {} a",
                variant
            ))
        );

        // Left operand must be compatible with the set index
        assert_eq!(
            false,
            run_validator(&format!(
                "type s : set of 1 .. 3 \nvar a : s\nvar b := true {} a",
                variant
            ))
        );
        assert_eq!(
            false,
            run_validator(&format!(
                "type s : set of char   \nvar a : s\nvar b := 1 {} a",
                variant
            ))
        );
        assert_eq!(
            false,
            run_validator(&format!(
                "type s : set of boolean\nvar a : s\nvar b := 'a' {} a",
                variant
            ))
        );
        assert_eq!(
            false,
            run_validator(&format!(
                "type e0 : enum(a, b)\ntype s : set of e0\nvar a : s\nvar b := 'c' {} a",
                variant
            ))
        );
        assert_eq!(false, run_validator(&format!("type e0 : enum(a, b)\ntype s : set of e0.a .. e0.b\nvar a : s\nvar b := false {} a", variant)));
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
        assert_eq!(
            true,
            run_validator("var a : boolean := false => true    \na =>= true => true")
        );

        // Not booleans
        assert_eq!(false, run_validator("var a : int := \"str\" => \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na    =>= \"str\""));

        assert_eq!(false, run_validator("var a : real := 1      =>  1.0"));
        assert_eq!(false, run_validator("var a : real := 1\na   =>= 1.0"));

        assert_eq!(false, run_validator("var a : nat     := true    =>  false"));
        assert_eq!(false, run_validator("var a : nat     := 1\na    =>= true "));
    }

    #[test]
    fn test_indirect_typecheck() {
        // Parser checks all primitive eval types
        assert_eq!(
            true,
            run_validator("type into : int\nvar adr : addressint\nvar k := into @ (adr)")
        );
        assert_eq!(
            true,
            run_validator("var adr : addressint\nvar k := int @ (adr)")
        );
        // Accept constant addresses
        assert_eq!(true, run_validator("type into : int\nvar k := into @ (1)"));
        assert_eq!(true, run_validator("var k := int @ (1)"));
        assert_eq!(true, run_validator("var k := char(3) @ (1)"));

        // Reference must refer to a type
        assert_eq!(false, run_validator("var into : int\nvar k := into @ (1)"));
        assert_eq!(
            false,
            run_validator("const into : int := 0\nvar k := into @ (1)")
        );
        assert_eq!(
            false,
            run_validator(
                "type into_enum : enum(a)\nvar adr : addressint\nvar k := into_enum.a @ (adr)"
            )
        );

        // address must not be a type reference
        assert_eq!(false, run_validator("type at : int\nvar k := int @ (at)"));

        // address must be a `nat` or `int` type
        assert_eq!(false, run_validator("var at : real\nvar k := int @ (at)"));
        assert_eq!(false, run_validator("var at : string\nvar k := int @ (at)"));
        assert_eq!(
            false,
            run_validator("var at : string(5)\nvar k := int @ (at)")
        );
        assert_eq!(false, run_validator("var at : char\nvar k := int @ (at)"));
        assert_eq!(
            false,
            run_validator("var at : char(5)\nvar k := int @ (at)")
        );

        // Range types with integer base types are allowed
        assert_eq!(true, run_validator("var at : 1 .. 3\nvar ka := int @ (at)"));

        // Allowed to be used in reference position
        assert_eq!(true, run_validator("int @ (0) := 1"));
        assert_eq!(true, run_validator("type bambam : int\nbambam @ (0) := 1"));
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
        assert_eq!(
            true,
            run_validator("var a : ^^^^^^^^^^^^^int\nvar b : int := ^^^^^^^^^^^^^a")
        );

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

        // Cannot apply deref to type references
        assert_eq!(false, run_validator("type a : ^int\nvar b : int := ^a"));

        // Gracefully handle empty expressions
        assert_eq!(false, run_validator("^to"));
    }

    #[test]
    fn test_poundcheat_typecheck() {
        // Tests typechecking for the unary operator
        // nat cheat can be applied to anything var/const reference or literal,
        // as long as the destination operand is a 'nat'
        assert_eq!(true, run_validator("var a : nat := #true"));
        assert_eq!(true, run_validator("var a : nat := #1"));
        assert_eq!(true, run_validator("var a : nat := #1"));
        assert_eq!(true, run_validator("var a : nat := #1.0"));
        assert_eq!(true, run_validator("var a : nat := #\"a\""));
        assert_eq!(true, run_validator("var a : nat := #'aa'"));
        assert_eq!(true, run_validator("var a : nat := #'a'"));
        assert_eq!(
            true,
            run_validator("var a : function a() : int\nvar b : nat := #a")
        );
        assert_eq!(
            true,
            run_validator("var a : proc a(a : int, b, c : nat)\nvar b : nat := #a")
        );
        assert_eq!(
            true,
            run_validator("type e0 : enum (a)\nvar a : nat := #e0.a")
        );

        // nat cheat cannot be applied to direct typedefs
        assert_eq!(
            false,
            run_validator("type a : function a() : int\nvar b : nat := #a")
        );
        assert_eq!(
            false,
            run_validator("type a : function a() : int\nvar b : nat := #(((a)))")
        );
        // nat cheat cannot be applied to typedefs hidden behind '.'s (not checked yet)
        // TODO: flesh this out once modules are parsed

        // Arbitrary applications of nat cheat
        assert_eq!(true, run_validator("var a : int := ###############'kemp'"));
    }

    #[test]
    fn test_dot_typecheck() {
        // Normal cases
        assert_eq!(
            true,
            run_validator("type e0 : enum (a, b, c)\nvar a := e0.a")
        );

        // Field is not a part of the compound type
        assert_eq!(
            false,
            run_validator("type e0 : enum (a, b, c)\nvar a := e0.d")
        );

        // Reference is not a compound type
        assert_eq!(false, run_validator("var a : int\na.b"));
        assert_eq!(false, run_validator("var a : array 1 .. 2 of int\na.b"));
        assert_eq!(false, run_validator("var a : ^int\na->b"));

        // Reference is behind a pointer (gives a special error message)
        assert_eq!(false, run_validator("var a : ^int\na.b"));

        // TODO: Handle cases for union & Record fields
        // Class, Module, and Monitor qualified exported types are checked in test_type_resolution
    }

    #[test]
    fn test_range_size_checking() {
        // Ranges in Turing are inclusive on both bounds
        assert_eq!(true, run_validator("var a : 1 .. 16"));
        assert_eq!(
            true,
            run_validator(
                "const s : int := 0
                const e : int := 10
                var a : s .. e"
            )
        );

        // 1 sized ranges are valid
        assert_eq!(true, run_validator("var a : 'a' .. 'a'"));
        assert_eq!(true, run_validator("var a : 1 .. 1"));
        assert_eq!(true, run_validator("var a : true .. true"));
        assert_eq!(true, run_validator("var a : false .. false"));
        assert_eq!(
            true,
            run_validator("type e : enum(a, b)\nvar a : e.a .. e.a")
        );
        assert_eq!(
            true,
            run_validator("type e : enum(a, b)\nconst c : e := e.a\nvar a : c .. c")
        );
        assert_eq!(
            true,
            run_validator(
                "const s : int := 0
                const e : int := 0
                var a : s .. e"
            )
        );

        // End range overflows constitute a valid range (but emits a warning)
        assert_eq!(true, run_validator("var a : -8000 .. 16#8000000000000000"));
        assert_eq!(true, run_validator("var a : -1 .. 16#ffffffffffffffff"));
        assert_eq!(true, run_validator("var a : 0 .. 16#ffffffffffffffff"));
        assert_eq!(true, run_validator("var a : 1 .. 16#ffffffffffffffff"));
        assert_eq!(true, run_validator("var a : 0-2+2 .. 16#8000000000000000"));
        assert_eq!(true, run_validator("var a : 1-2+2 .. 16#8000000000000000"));
        assert_eq!(
            true,
            run_validator("const s : int := 1\nvar a : s .. 16#ffffffffffffffff")
        );
        assert_eq!(
            true,
            run_validator("const s : int := 0\nvar a : s .. 16#fffffffffffffffe")
        );
        assert_eq!(
            true,
            run_validator("const s : int := 0\nvar a : s .. 16#ffffffffffffffff")
        );
        assert_eq!(
            true,
            run_validator("var a : -16#7fffffffffffffff - 1 .. 16#ffffffffffffffff")
        );

        // Start overflows constitute an invalid range
        assert_eq!(false, run_validator("var a : 16#ffffffffffffffff .. -3"));
        assert_eq!(
            false,
            run_validator("var a : 16#ffffffffffffffff .. 16#7fffffffffffffff")
        );

        // 0 sized ranges are valid in some contexts (e.g. flexible arrays)
        assert_eq!(
            true,
            run_validator(
                "var a : flexible array 16#8000000000000000 .. 16#7fffffffffffffff of int"
            )
        );
        assert_eq!(
            true,
            run_validator("var a : flexible array true .. false of int")
        );
        assert_eq!(
            true,
            run_validator("var a : flexible array 'D' .. 'C' of int")
        );
        assert_eq!(
            true,
            run_validator("type e : enum(a, b)\nvar a : flexible array e.b .. e.a of int")
        );
        assert_eq!(true, run_validator("type e : enum(a, b)\nconst c : e := e.b\nconst d : e := e.a\nvar a : flexible array c .. d of int"));
        assert_eq!(
            true,
            run_validator(
                "const s : int := 1
                const e : int := 0
                var a : flexible array s .. e of int"
            )
        );

        // 0 sized ranges aren't valid anywhere else
        assert_eq!(false, run_validator("var a : array 1 .. 0 of int"));
        assert_eq!(
            false,
            run_validator("var a : array 16#80000000 .. 16#7fffffff of int")
        );
        assert_eq!(false, run_validator("var a : array true .. false of int"));
        assert_eq!(false, run_validator("var a : array 'D' .. 'C' of int"));
        assert_eq!(false, run_validator("var a : 16#80000000 .. 16#7fffffff"));
        assert_eq!(false, run_validator("var a : true .. false"));
        assert_eq!(false, run_validator("var a : 'D' .. 'C'"));
        assert_eq!(
            false,
            run_validator("type a : set of 16#80000000 .. 16#7fffffff")
        );
        assert_eq!(false, run_validator("type a : set of true .. false"));
        assert_eq!(false, run_validator("type a : set of 'D' .. 'C'"));
        assert_eq!(
            false,
            run_validator("type e : enum(a, b)\nvar a : e.b .. e.a")
        );
        assert_eq!(
            false,
            run_validator(
                "type e : enum(a, b)\nconst c : e := e.b\nconst d : e := e.a\nvar a : c .. d"
            )
        );
        assert_eq!(
            false,
            run_validator(
                "const s : int := 1
                const e : int := 0
                var a : array s .. e of int"
            )
        );

        // 0 sized ranges can't hide behind aliases
        assert_eq!(
            false,
            run_validator("type a : 16#80000000 .. 16#7fffffff\nvar b : a")
        );
        assert_eq!(
            false,
            run_validator("type a : true .. false             \nvar b : a")
        );
        assert_eq!(
            false,
            run_validator("type a : 'D' .. 'C'                \nvar b : a")
        );
        assert_eq!(
            false,
            run_validator("type a : 16#80000000 .. 16#7fffffff\ntype b : set of a")
        );
        assert_eq!(
            false,
            run_validator("type a : true .. false             \ntype b : set of a")
        );
        assert_eq!(
            false,
            run_validator("type a : 'D' .. 'C'                \ntype b : set of a")
        );
        assert_eq!(
            false,
            run_validator("type a : -1+2 .. 1-1               \ntype b : set of a")
        );
        assert_eq!(
            false,
            run_validator("type a : 'D' .. 'C'                \ntype b : array a of int")
        );

        // 0 sized ranges can't hide behind double aliases
        assert_eq!(
            false,
            run_validator("type a : 'D' .. 'C'\ntype b : a    \nvar c : a")
        );
        assert_eq!(
            false,
            run_validator("type a : 'D' .. 'C'\ntype b : a    \ntype c : set of a")
        );
        assert_eq!(
            false,
            run_validator("type a : 'D' .. 'C'\ntype b : a    \ntype c : array a of int")
        );

        // `init`-sized ranges can't hide behind aliases
        assert_eq!(
            false,
            run_validator("type a : 1 .. *\nvar b : array a of int := init(1)")
        );
        assert_eq!(false, run_validator("type a : 1 .. *\ntype b : set of a"));
        assert_eq!(false, run_validator("type a : 1 .. *\nvar b : set of a"));

        // `init`-sized ranges can't hide behind double aliases
        assert_eq!(
            false,
            run_validator("type a : 1 .. *\ntype b : a\nvar c : array b of int")
        );
        assert_eq!(
            false,
            run_validator("type a : 1 .. *\ntype b : a\ntype c : set of b")
        );
        assert_eq!(
            false,
            run_validator("type a : 1 .. *\ntype b : a\nvar c : b")
        );

        // Negative size ranges are invalid
        assert_eq!(false, run_validator("var a : 16#80000000 .. 16#7ffffffe"));
        assert_eq!(
            false,
            run_validator("var a : 16#ffffffffffffffff .. 16#fffffffffffffffd")
        );
        assert_eq!(false, run_validator("var a : 'D' .. 'B'"));
        assert_eq!(
            false,
            run_validator("type e : enum(a, b, c)\nvar a : e.c .. e.a")
        );
        assert_eq!(
            false,
            run_validator(
                "type e : enum(a, b, c)\nconst c : e := e.c\nconst d : e := e.a\nvar a : c .. d"
            )
        );
        assert_eq!(false, run_validator("var a : -1+3 .. 0"));

        // Range bounds can't be reals, or any other types
        assert_eq!(false, run_validator("type e : 0.0 .. 0.0"));
        assert_eq!(false, run_validator("type e : 0.0 + 0.0 .. 0.0 + 0.0"));

        // Range bounds must be compile-time expressions
        assert_eq!(false, run_validator("var a : int\ntype e : set of a..0"));
        assert_eq!(false, run_validator("var a : int\ntype e : set of 0..a"));
        assert_eq!(false, run_validator("var a : int\ntype e : set of a..a"));

        // Still ba safe when handling empty expressions
        assert_eq!(false, run_validator("type e set of(0.."));
        assert_eq!(false, run_validator("type e.."));
        assert_eq!(false, run_validator("type e:..0*0"));
        assert_eq!(false, run_validator("var a: array of int"));
    }

    #[test]
    fn test_constant_folder() {
        // Folds should chain together
        let (success, unit) = make_validator("var a : int := 1 - 1 - 1 - 1 - 1");
        assert_eq!(true, success);
        if let StmtKind::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[0].kind
        {
            assert_eq!(
                Value::from_expr(*expr.clone(), unit.types()).unwrap(),
                Value::IntValue(-3)
            );
        } else {
            panic!("Fold failed");
        }

        // 'nil' should never be folded
        assert_eq!(true, run_validator("var k : nat := #nil"));

        // Stop folding in an error
        assert_eq!(
            false,
            run_validator("var a : int := 1 - 1 - \"bad\" - 1 - 1")
        );
        assert_eq!(
            false,
            run_validator("var a : int := 1 - 1 ** (0 - 1) - 1 - 1")
        );
        assert_eq!(
            false,
            run_validator("var a : real := 10.0 ** (300 + 7) * 100")
        );
        assert_eq!(false, run_validator("var a : real := 10.0 ** (300 + 10)"));
        assert_eq!(false, run_validator("var a := false=0=-#-3"));
        assert_eq!(false, run_validator("var a := -#-3"));
        assert_eq!(false, run_validator("var a := --#-3"));
        // Preserve types
        assert_eq!(
            false,
            run_validator("var a : int := 1 + 0.1 - 1 - 0.1 - 1 - 1")
        );

        // Ensure that the constant folder preserves assignment semantics
        assert_eq!(false, run_validator("var a : char(6) := 'abcd' + 'aaa'"));

        // Valid type check, checked at runtime
        assert_eq!(true, run_validator("var a : nat := (0 - 1)"));

        // Folding should stop at runtime evaluations
        assert_eq!(
            true,
            run_validator("var a : nat := 1\nvar b := a + (1 + 1)")
        );

        // Folding should be able to fold enum comparisons
        let (success, unit) = make_validator("type e0 : enum (a, b, c)\nvar a := e0.a < e0.c");
        assert_eq!(true, success);
        if let StmtKind::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[1].kind
        {
            assert_eq!(
                Value::from_expr(*expr.clone(), unit.types()).unwrap(),
                Value::BooleanValue(true)
            );
        } else {
            panic!("Fold failed");
        }

        let (success, unit) =
            make_validator("type e0 : enum (a, b, c)\nconst c : e0 := e0.c\nvar a := e0.a < c");
        assert_eq!(true, success);
        if let StmtKind::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[2].kind
        {
            assert_eq!(
                Value::from_expr(*expr.clone(), unit.types()).unwrap(),
                Value::BooleanValue(true)
            );
        } else {
            panic!("Fold failed");
        }

        let (success, unit) =
            make_validator("type e0 : enum (a, b, c)\nconst c := e0.c\nvar a := e0.a < c");
        assert_eq!(true, success);
        if let StmtKind::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[2].kind
        {
            assert_eq!(
                Value::from_expr(*expr.clone(), unit.types()).unwrap(),
                Value::BooleanValue(true)
            );
        } else {
            panic!("Fold failed");
        }

        // Pull literal expressions from inside grouping expressions
        assert_eq!(true, run_validator("const c := (0)"));
    }

    #[test]
    fn test_folding_reporting() {
        // All of these should produce errors
        assert_eq!(false, run_validator("var beebee := 1 shl -1"));
        assert_eq!(false, run_validator("beebee := 1 shr amt"));
        assert_eq!(false, run_validator("beebee := 1 div 0"));
        assert_eq!(false, run_validator("beebee := 1 / 0"));
        assert_eq!(false, run_validator("beebee := 1 rem 0"));
        assert_eq!(false, run_validator("beebee := 1 mod 0"));
    }

    #[test]
    fn test_constant_prop() {
        // Test constant propogation

        // Fold constants together
        let (success, unit) = make_validator(
            "
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a    % (4 + 1) + 1 + 4
const d := a + b + c    % 4*4 + 1 + 1 + 1
        ",
        );
        assert_eq!(true, success);
        if let StmtKind::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts().last().unwrap().kind
        {
            assert_eq!(
                Value::from_expr(*expr.clone(), unit.types()).unwrap(),
                Value::NatValue(19)
            );
        } else {
            panic!("Fold failed");
        }

        // Don't fold binary & unary expressions in the event of an error
        let (success, unit) = make_validator(
            "
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a + \"beep beep\"
const d := a + b + c    % 4*4 + 1 + 1 + 1
        ",
        );
        assert_eq!(false, success);
        if let StmtKind::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts().last().unwrap().kind
        {
            if let ExprKind::BinaryOp { left, right, .. } = expr.clone().kind {
                // Check that (a + b) and c was not folded
                assert!(matches!(left.kind, ExprKind::BinaryOp { .. }));
                assert!(matches!(right.kind, ExprKind::Reference { .. }));
            } else {
                panic!("Something wrong happened! (folding should not have occurred!)");
            }
        } else {
            unreachable!("Something wrong happened! (not a var_decl!)");
        }

        // Constant folding tests with inner scopes are tested in the dedicated block stmt test

        // Even though positive literals are NatValues, the transferred type should still
        // allow implicit type variables to be assigned negative integers
        let (success, unit) = make_validator("var a := 1 + 1\na := -1");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        let (success, unit) = make_validator("var a := 1\na := -1");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // Constont folding allows compile-time expressions in length specifiers
        let (success, unit) = make_validator("const sz := 3\nconst a : string(sz) := 'aaa'");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::StringN(SequenceSize::Size(3)))
        );

        let (success, unit) = make_validator("const sz := 3\nconst a : char(sz + 10) := 'aaa'");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::CharN(SequenceSize::Size(13)))
        );

        // Constant folding length specifiers should still preserve parser validation semantics
        // On error, should resovle into the base types

        // Case: Zero Size
        let (success, unit) = make_validator("const sz := 0\nconst a : string(sz) := 'aaa'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let (success, unit) = make_validator("const sz := 0\nconst a : char(sz + 1 - 1) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Case: Negative size
        let (success, unit) = make_validator("const sz := -2\nconst a : string(sz) := 'aaa'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let (success, unit) = make_validator("const a : char(5 - 10) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Case: Too large
        let (success, unit) =
            make_validator("const sz := 16#10000 - 5 + 5\nconst a : string(sz) := 'aaa'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let (success, unit) = make_validator("const a : char(65530 + 10) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Case: Wrong compile-time type
        let (success, unit) = make_validator("const a : char(65530 + 0.0) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        let (success, unit) = make_validator("const a : string('noop' + 'boop') := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        // Case: Not actually a compile-time expression
        let (success, unit) = make_validator("var depend := 65530\nconst a : char(depend) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        let (success, unit) =
            make_validator("var depend := 65530\nconst a : string(1 + depend + 1) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        // Constant propogation should allow enum fields to be hidden behind constant vars
        assert_eq!(
            true,
            run_validator(
                "
        type e0 : enum(a, b, c)
        const a : e0 := e0.a
        var b : a .. e0.b := e0.c
        "
            )
        );

        assert_eq!(
            true,
            run_validator(
                "
        type e0 : enum(a, b, c)
        const a := e0.a
        var b : a .. e0.b := e0.c
        "
            )
        );
    }

    #[test]
    fn test_ident_resolution() {
        // All errors reported here, including undeclared uses
        // v decl use
        let (success, unit) = make_validator("var a : int := 1\na += 1");
        assert_eq!(true, success);
        assert_eq!(true, get_ident_for_unit(&unit, "a").unwrap().is_declared);
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // x use decl
        let (success, unit) = make_validator("a += 1\nvar b : int := 1");
        assert_eq!(false, success);
        assert_eq!(false, get_ident_for_unit(&unit, "a").unwrap().is_declared);
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::TypeError
        );

        // x use decl behind scope
        let (success, unit) = make_validator("a += 1\nbegin var a : int := 1 end");
        assert_eq!(false, success);

        assert_eq!(false, get_info_for_unit(&unit, IdentId(0)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(0)).type_spec,
            TypeRef::TypeError
        );

        assert_eq!(true, get_info_for_unit(&unit, IdentId(1)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(1)).type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        assert_eq!(false, get_ident_for_unit(&unit, "a").unwrap().is_declared);
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::TypeError
        );

        // x use use decl (only 1 error produced)
        let (success, unit) = make_validator("a += 1\na += 1\nvar b : int := 1");
        assert_eq!(false, success);
        assert_eq!(false, get_ident_for_unit(&unit, "a").unwrap().is_declared);
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::TypeError
        );

        // x use
        let (success, unit) = make_validator("a += 1\n");
        assert_eq!(false, success);
        assert_eq!(false, get_ident_for_unit(&unit, "a").unwrap().is_declared);
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::TypeError
        );

        // x use decl decl
        let (success, unit) =
            make_validator("a := a + 1\nvar a : int\nvar a : string % final type\n");
        assert_eq!(false, success);
        assert_eq!(false, get_info_for_unit(&unit, IdentId(0)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(0)).type_spec,
            TypeRef::TypeError
        );

        assert_eq!(true, get_info_for_unit(&unit, IdentId(1)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(1)).type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        assert_eq!(true, get_info_for_unit(&unit, IdentId(2)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(2)).type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        ); // Final type

        // x decl decl
        let (success, unit) = make_validator("var a : string\nvar a : real8 % final type");
        assert_eq!(false, success);
        assert_eq!(true, get_info_for_unit(&unit, IdentId(0)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(0)).type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        assert_eq!(true, get_info_for_unit(&unit, IdentId(1)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(1)).type_spec,
            TypeRef::Primitive(PrimitiveType::Real8)
        );
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Real8)
        ); // Final type

        // x use-in-init decl
        let (success, unit) = make_validator("var a : string := a + \"oops\"");
        assert_eq!(false, success);
        assert_eq!(false, get_info_for_unit(&unit, IdentId(0)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(0)).type_spec,
            TypeRef::TypeError
        );

        assert_eq!(true, get_info_for_unit(&unit, IdentId(1)).is_declared);
        assert_eq!(
            get_info_for_unit(&unit, IdentId(1)).type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_eq!(
            get_ident_for_unit(&unit, "a").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        ); // Final type

        // None of these should panic
        assert_eq!(run_validator("var a:0\na"), false);
        assert_eq!(run_validator("var a\na"), false);
        assert_eq!(run_validator("a=begin a end"), false);
        assert_eq!(run_validator("k().c"), false);
        assert_eq!(run_validator("a(begin a"), false);
        assert_eq!(run_validator("a.begin a"), false);
        assert_eq!(run_validator("var ye0\nvar y : ye0\nbegin i=y"), false);
        assert_eq!(run_validator("var ye0\nvar ye0 : ye0\nbegin ye0="), false);

        // Type parsing should preserve used identifiers
        assert_eq!(run_validator("var k : set a\nbegin a end"), false);
    }

    #[test]
    fn test_type_resolution() {
        // Aliases to aliases are allowed
        assert_eq!(true, run_validator("type a : int\ntype b : a\ntype c : b"));

        // Aliases are equivalent to their base types
        assert_eq!(true, run_validator("type a : 1 .. 5\ntype b : set of a"));
        assert_eq!(
            true,
            run_validator("type a : int\ntype b : a\nvar c : int := 1\nvar d : b := c")
        );
        assert_eq!(true, run_validator("type a : boolean\ntype b : set of a"));
        assert_eq!(true, run_validator("type a : char\ntype b : set of a"));
        assert_eq!(false, run_validator("type a : real\ntype b : set of a"));
        assert_eq!(false, run_validator("type a : string\ntype b : set of a"));
        // TODO: include cases of record and union

        // Aliases of resolved forward types are equivalent to their base types
        assert_eq!(
            true,
            run_validator("type a : forward\ntype a : int\ntype b : a\nvar c : a := 2")
        );

        // Forward refs are only allowed in pointer type definitions
        assert_eq!(
            true,
            run_validator("type a : forward\nvar k : ^a\ntype a : int")
        );
        assert_eq!(
            true,
            run_validator("type a : forward\ntype k : ^a\ntype a : int")
        );
        assert_eq!(
            false,
            run_validator("type a : forward\ntype k : a\ntype a : int")
        );
        assert_eq!(
            false,
            run_validator("type a : forward\ntype k : set of a\ntype a : int")
        );

        // Forward refs must be resolved in the current unit
        assert_eq!(false, run_validator("type a : forward"));

        // Range bounds types do not match
        assert_eq!(false, run_validator("type a : true .. 'c'"));
        assert_eq!(false, run_validator("type a : 1 .. 'c'"));
        assert_eq!(false, run_validator("type a : 'c' .. true"));
        assert_eq!(false, run_validator("type a : 'c' .. 'aa'"));
        assert_eq!(false, run_validator("type a : 'cb' .. 'aa'"));

        // Identifier is not a reference to a type
        // TODO: Test dot references for records, unions, monitors, and modules once those are valid & resolvable
        assert_eq!(false, run_validator("type a : enum (c)\ntype b : a.c"));
        assert_eq!(false, run_validator("type a : enum (c)\nvar b : a.c"));
        assert_eq!(false, run_validator("var a : int := 1\ntype b : a"));
        assert_eq!(false, run_validator("var a : int := 1\nvar b : a := 2"));

        // Range end bound must be a compile-time expression (in theses contexts)
        assert_eq!(true, run_validator("type b : set of 1 .. (8 + 20 - 3)"));
        assert_eq!(true, run_validator("type b : 1 .. (8 + 20 - 3)"));
        assert_eq!(true, run_validator(" var b : 1 .. (8 + 20 - 3)"));

        assert_eq!(
            false,
            run_validator("var a : int := 1\ntype b : set of 1 .. a")
        );
        assert_eq!(false, run_validator("var a : int := 1\ntype b : 1 .. a"));
        assert_eq!(false, run_validator("var a : int := 1\n var b : 1 .. a"));
        assert_eq!(
            false,
            run_validator("var a : int := 1\ntype b : array 1 .. a of int")
        );

        // Range end bound is allowed to be a runtime expression (in this context)
        assert_eq!(
            true,
            run_validator("var a : int := 1\n var b : array 1 .. a of int")
        );
    }

    #[test]
    fn test_default_types() {
        // Test that the correct inferred types are being used
        let (success, unit) = make_validator("var a := 16#7FFFFFFF");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        let (success, unit) = make_validator("var a := 16#80000000");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::Nat)
        );

        let (success, unit) = make_validator("var a := 16#100000000");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::LongInt)
        );

        let (success, unit) = make_validator("var a := 16#100000000 + 16#100000000");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::LongInt)
        );

        let (success, unit) = make_validator("var a := 16#8000000000000000");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::LongNat)
        );

        let (success, unit) = make_validator("var a := 16#8000000000000000 + 1");
        assert_eq!(true, success);
        assert_eq!(
            get_ident_for_unit(&unit, "a").as_ref().unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::LongNat)
        );
    }

    #[test]
    fn test_resolve_init_expr() {
        // All expressions in 'init' must be compile-time expressions
        assert_eq!(
            true,
            run_validator("var a : array 1 .. * of int := init(1, 2, 3)")
        );

        assert_eq!(
            true,
            run_validator("var a : array 1 .. * of int := init(1, 2 + 3 - 5 * 10, 3)")
        );

        assert_eq!(
            true,
            run_validator("const c := 5\nvar a : array 1 .. * of int := init(1, c - 5, 3)")
        );

        assert_eq!(
            false,
            run_validator("var c := 5\nvar a : array 1 .. * of int := init(1, c, 3)")
        );

        assert_eq!(
            false,
            run_validator("type c : int\nvar a : array 1 .. * of int := init(1, c, 3)")
        );

        // Should be as many elements as specified by the array ranges
        assert_eq!(
            true,
            run_validator("var a : array 1 .. 3 of int := init(1, 2, 3)")
        );

        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init(1, 2, 3, 4, 5)")
        );

        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init(1, 2)")
        );

        // Should not panic
        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init(1, 2, 3, or)")
        );

        // Reported by parser
        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init()")
        );

        // Should also apply to other types
        assert_eq!(
            true,
            run_validator("var a : array boolean of int := init(1, 2)")
        );

        assert_eq!(
            false,
            run_validator("var a : array boolean of int := init(1)")
        );

        assert_eq!(
            false,
            run_validator("var a : array boolean of int := init(1, 2, 3)")
        );

        // Range types should compound together
        assert_eq!(
            true,
            run_validator("var a : array boolean, boolean of int := init(1, 2, 3, 4)")
        );

        assert_eq!(
            false,
            run_validator("var a : array boolean, boolean of int := init(1, 2, 3, 4, 5, 6)")
        );

        // Range types should compound together
        assert_eq!(
            false,
            run_validator("var a : array boolean, boolean of int := init(1, 2, 3)")
        );

        assert_eq!(
            false,
            run_validator("var a : array boolean, boolean of int := init(1, 2)")
        );

        // Types should match field/element type (and not be a type reference)
        // TODO: add tests for union & record fields
        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init(1, 2.0, 3)")
        );

        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init(1, 2, \"ee\")")
        );

        assert_eq!(
            false,
            run_validator("var a : array 1 .. 3 of int := init('c', 2, 3)")
        );

        assert_eq!(
            false,
            run_validator("type k : int\nvar a : array 1 .. 3 of int := init(k, 2, 3)")
        );

        // "init" initializers are not allowed for dynamic or flexible arrays
        assert_eq!(
            false,
            run_validator("var a : flexible array 1 .. 3 of int := init(1, 2, 3)")
        );

        assert_eq!(
            false,
            run_validator("var len := 3\nvar a : array 1 .. len of int := init(1, 2, 3)")
        );
    }

    #[test]
    fn test_resolve_var_decl() {
        assert_eq!(true, run_validator("var a : 1 .. 3 := 2"));
        assert_eq!(true, run_validator("const a : 1 .. 3 := 2"));

        // Init-sized type specs are not allowed in const/var decls
        assert_eq!(false, run_validator("var a : 1 .. *"));

        assert_eq!(false, run_validator("const a : 1 .. *"));
    }

    #[test]
    fn test_resolve_type_decl() {
        assert_eq!(true, run_validator("type a : 1 .. 3"));

        // Should report redecl errors
        assert_eq!(false, run_validator("var a : int\ntype a : 1 .. 3"));
    }

    #[test]
    fn test_resolve_block_stmt() {
        // Local scope identifiers don't leak out of scope boundaries
        assert_eq!(
            true,
            run_validator(
                "begin
                const cant_see_mee : int := 3
            end
            const cant_see_mee : string := 'heehee'"
            )
        );

        // Importing consts should work
        assert_eq!(
            true,
            run_validator(
                "const outer_see : int := 5
            begin
                const middle_see : int := 262
                begin
                    var inner_see : outer_see .. middle_see + outer_see
                end
                begin
                    var and_here_too : int := middle_see + outer_see
                end
            end"
            )
        );

        // Inner scopes can't shadow an outer scope's variables
        assert_eq!(
            false,
            run_validator(
                "var cant_shadow := 'eep'
        begin
            var cant_shadow := 'eep'
        end"
            )
        );

        assert_eq!(
            false,
            run_validator(
                "begin
                var cant_shadow := 'eep'
                begin
                    var cant_shadow := 'eep'
                end
            end"
            )
        );

        // Inner scope can't access identifiers declared after it
        assert_eq!(
            false,
            run_validator(
                "begin
                var fail_to_use := cant_see_me
            end
            var cant_see_me := 'eep'"
            )
        );

        assert_eq!(
            false,
            run_validator(
                "begin
                begin
                    var fail_to_use := cant_see_me
                end
                var cant_see_me := 'eep'
            end"
            )
        );

        // Undeclared identifier should be captured
        assert_eq!(
            false,
            run_validator(
                "begin
                    a
                end
                "
            )
        );
    }

    #[test]
    fn test_resolve_if_stmt() {
        assert_eq!(true, run_validator(r#"if true then var k := 1 end if"#));
        assert_eq!(
            true,
            run_validator(r#"if true then var k := 1 elsif true then var k := 2 end if"#)
        );
        assert_eq!(
            true,
            run_validator(
                r#"if true then var k := 1 elsif true then var k := 2 else var k := 3 end if"#
            )
        );

        // false_branch should be visited
        assert_eq!(
            false,
            run_validator(r#"if true then elsif true then a end if"#)
        );

        assert_eq!(false, run_validator(r#"if true then else a end if"#));

        // Only boolean expressions are allowed in the conditional
        assert_eq!(false, run_validator(r#"if 1 then var k := 2 end if"#));
        assert_eq!(false, run_validator(r#"if 1.0 then var k := 2 end if"#));
        assert_eq!(false, run_validator(r#"if 'yee' then var k := 2 end if"#));
        assert_eq!(false, run_validator(r#"if nil then var k := 2 end if"#));

        assert_eq!(
            false,
            run_validator(r#"if true then elsif 1 then var k := 2 end if"#)
        );
        assert_eq!(
            false,
            run_validator(r#"if true then elsif 1.0 then var k := 2 end if"#)
        );
        assert_eq!(
            false,
            run_validator(r#"if true then elsif 'yee' then var k := 2 end if"#)
        );
        assert_eq!(
            false,
            run_validator(r#"if true then elsif nil then var k := 2 end if"#)
        );

        // Conditional expression should be folded
        let (success, code_unit) = make_validator(r#"if true & true then var k := 2 end if"#);
        assert_eq!(true, success);

        if let StmtKind::If { condition, .. } = &code_unit.stmts()[0].kind {
            assert_eq!(
                &condition.kind,
                &ExprKind::Literal {
                    value: Literal::Bool(true)
                }
            );
        } else {
            unreachable!();
        }

        let (success, code_unit) = make_validator(
            r#"if false & false then var k := 2 elsif true & true then var c := 3 end if"#,
        );
        assert_eq!(true, success);

        if let StmtKind::If {
            false_branch: Some(false_block),
            ..
        } = &code_unit.stmts()[0].kind
        {
            // peek into false branch
            if let StmtKind::If { condition, .. } = &false_block.kind {
                assert_eq!(
                    &condition.kind,
                    &ExprKind::Literal {
                        value: Literal::Bool(true)
                    }
                );
            } else {
                unreachable!()
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_type_spec_access() {
        // From fuzzing, should not panic
        assert_eq!(false, run_validator("type : a\nbegin a end"));
        assert_eq!(false, run_validator("var k\nvar l : k\nbegin l end"));
        assert_eq!(false, run_validator("const i:char(Qbegin v"));
        assert_eq!(false, run_validator("const i:char(egin begin+egin"));
    }

    #[test]
    fn test_error_propogate() {
        // Type errors should propogate downards and be handled correctly (i.e. not panic)
        assert_eq!(run_validator("var j : int\nvar k := (j.k) + 1"), false);
        assert_eq!(run_validator("var j : int\nvar k := # (j.k)"), false);
        assert_eq!(run_validator("var j : int\nvar k := # (j->k)"), false);
    }
}
