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
mod type_resolve;

use crate::compiler::ast::{Expr, Identifier, Stmt, VisitorMut};
use crate::compiler::block::CodeBlock;
use crate::compiler::types::{self, Type, TypeRef, TypeTable};
use crate::compiler::value::Value;
use crate::status_reporter::StatusReporter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::{Rc, Weak};

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
            local_idents: HashMap::new(),
        }
    }

    /// Uses an identifier declared in the current scope.
    /// Any imported identifiers in the current scope must refer to the ScopeInfo
    /// where the imported identifier is declared in.
    ///
    /// # Return Values
    /// `Option<Value>`     Associated compile-time value. \
    /// `bool`              True if the identifier has been declared before.
    pub fn use_ident(&mut self, ident: &Identifier) -> (Option<Value>, bool) {
        let mut is_already_declared = true;

        // Grab the compile-time value
        let compile_value = self
            .local_idents
            .entry(ident.name.clone())
            .and_modify(|all_idents| {
                // Increment use of the associated identifier
                // The given instance must exist already, otherwise a declaration
                // was skipped or the instance counter is not being incremented properly
                let entry = &mut all_idents[ident.instance as usize];
                entry.uses = entry.uses.saturating_add(1);
            })
            .or_insert_with(|| {
                // Notify of the identifer never being declared
                is_already_declared = false;

                // Make sure this is the first time we're seeing this identifer
                assert_eq!(
                    ident.instance, 0,
                    "Not the first time seeing the identifier"
                );

                // Create a new identifier info group
                vec![IdentInfo {
                    ident: ident.clone(),
                    uses: 1,
                    compile_value: None,
                }]
            })[ident.instance as usize]
            .compile_value
            .clone();

        (compile_value, is_already_declared)
    }

    /// Declares an identifier, creating the associated IdentInfo entry.
    ///
    /// # Return Values
    /// `bool`  If the current declaration is redeclaring an identifier (i.e.
    /// the identifier has already been declared)
    pub fn decl_ident(&mut self, ident: Identifier) -> bool {
        self.decl_ident_with(ident, None)
    }

    /// Declares an identifier, creating the associated IdentInfo entry and
    /// associating a compile-time value with the identifier.
    ///
    /// # Return Values
    /// `bool`  If the current declaration is redeclaring an identifier (i.e.
    /// the identifier has already been declared)
    pub fn decl_ident_with(&mut self, ident: Identifier, compile_value: Option<Value>) -> bool {
        let mut is_redeclare = false;

        self.local_idents
            .entry(ident.name.clone())
            .and_modify(|_| {
                // Notify of redeclare
                is_redeclare = true
            })
            .or_insert_with(|| {
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
            })
            .push(IdentInfo {
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
pub struct Validator {
    /// Status reporter for the type validator
    pub reporter: StatusReporter,
    /// Type table to use
    type_table: TypeTable,
    /// Actively parsed scope
    active_block: Option<Weak<RefCell<CodeBlock>>>,
    /// Associated scope info
    scope_infos: Vec<ScopeInfo>,
}

type ResolveResult = Option<TypeRef>;

impl Validator {
    pub fn new(root_block: &Rc<RefCell<CodeBlock>>, type_table: TypeTable) -> Self {
        Self {
            reporter: StatusReporter::new(),
            type_table,
            active_block: Some(Rc::downgrade(root_block)),
            scope_infos: vec![ScopeInfo::new()],
        }
    }

    /// Takes the type_table from the validator
    pub fn take_types(&mut self) -> TypeTable {
        std::mem::replace(&mut self.type_table, TypeTable::new())
    }

    /// De-aliases a type ref, following through `Type::Alias`'s and resolving `Type::Reference`s.
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
            let current_id = if let Some(type_id) = types::get_type_id(&current_ref) {
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

impl VisitorMut<(), Option<Value>> for Validator {
    fn visit_stmt(&mut self, visit_stmt: &mut Stmt) {
        match visit_stmt {
            Stmt::VarDecl {
                idents,
                type_spec,
                value,
                is_const,
            } => self.visit_decl_var(idents, type_spec, value, *is_const),
            Stmt::TypeDecl {
                ident,
                resolved_type,
                is_new_def,
            } => self.visit_decl_type(ident, resolved_type, *is_new_def),
            Stmt::Assign { var_ref, op, value } => {
                self.visit_stmt_assign(var_ref, &op.token_type, value)
            }
            Stmt::ProcedureCall { proc_ref } => {
                let _ = self.visit_expr(proc_ref);
            }
            Stmt::Block { block, stmts } => self.visit_stmt_block(block, stmts),
        }
    }

    // Note: If the eval_type is still TypeRef::Unknown, propagate the type error
    fn visit_expr(&mut self, visit_expr: &mut Expr) -> Option<Value> {
        match visit_expr {
            Expr::Grouping {
                expr,
                eval_type,
                is_compile_eval,
                ..
            } => self.visit_expr_grouping(expr, eval_type, is_compile_eval),
            Expr::BinaryOp {
                left,
                op,
                right,
                eval_type,
                is_compile_eval,
                ..
            } => self.visit_expr_binary(left, op, right, eval_type, is_compile_eval),
            Expr::UnaryOp {
                op,
                right,
                eval_type,
                is_compile_eval,
                ..
            } => self.visit_expr_unary(op, right, eval_type, is_compile_eval),
            Expr::Call {
                left,
                op: _,
                arg_list,
                eval_type,
                is_compile_eval,
                ..
            } => self.visit_expr_call(left, arg_list, eval_type, is_compile_eval),
            Expr::Dot {
                left,
                field,
                eval_type,
                is_compile_eval,
                ..
            } => self.visit_expr_dot(left, field, eval_type, is_compile_eval),
            Expr::Reference { ident } => self.visit_expr_reference(ident),
            Expr::Literal { eval_type, .. } => self.visit_expr_literal(eval_type),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::block::CodeUnit;
    use crate::compiler::frontend::parser::Parser;
    use crate::compiler::frontend::scanner::Scanner;
    use crate::compiler::types::{PrimitiveType, SequenceSize};
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
        let type_table = code_unit.take_types();

        // Validate AST
        let mut validator = Validator::new(code_unit.root_block(), type_table);
        code_unit.visit_ast_mut(&mut validator);
        code_unit.put_types(validator.take_types());

        let successful_validate = !validator.reporter.has_error();

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
        // Missing: enum (direct field & behind const) & objectclass compares

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
    }

    #[test]
    fn test_range_size_checking() {
        // Ranges in Turing are inclusive on both bounds
        assert_eq!(true, run_validator("var a : 1 .. 16"));

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

        // End range overflows constitute a valid range
        assert_eq!(true, run_validator("var a : -8000 .. 16#8000000000000000"));
        assert_eq!(true, run_validator("var a : -1 .. 16#ffffffffffffffff"));
        assert_eq!(true, run_validator("var a : 0 .. 16#ffffffffffffffff"));
        assert_eq!(true, run_validator("var a : 1 .. 16#ffffffffffffffff"));
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

        // 0 sized ranges aren't valid anywhere else
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
    }

    #[test]
    fn test_constant_folder() {
        // Folds should chain together
        let (success, unit) = make_validator("var a : int := 1 - 1 - 1 - 1 - 1");
        assert_eq!(true, success);
        if let Stmt::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[0]
        {
            assert_eq!(Value::try_from(*expr.clone()).unwrap(), Value::IntValue(-3));
        } else {
            panic!("Fold failed");
        }

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
        if let Stmt::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[1]
        {
            assert_eq!(
                Value::try_from(*expr.clone()).unwrap(),
                Value::BooleanValue(true)
            );
        } else {
            panic!("Fold failed");
        }

        let (success, unit) =
            make_validator("type e0 : enum (a, b, c)\nconst c : e0 := e0.c\nvar a := e0.a < c");
        assert_eq!(true, success);
        if let Stmt::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[2]
        {
            assert_eq!(
                Value::try_from(*expr.clone()).unwrap(),
                Value::BooleanValue(true)
            );
        } else {
            panic!("Fold failed");
        }

        let (success, unit) =
            make_validator("type e0 : enum (a, b, c)\nconst c := e0.c\nvar a := e0.a < c");
        assert_eq!(true, success);
        if let Stmt::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts()[2]
        {
            assert_eq!(
                Value::try_from(*expr.clone()).unwrap(),
                Value::BooleanValue(true)
            );
        } else {
            panic!("Fold failed");
        }
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
        if let Stmt::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts().last().unwrap()
        {
            assert_eq!(Value::try_from(*expr.clone()).unwrap(), Value::NatValue(19));
        } else {
            panic!("Fold failed");
        }

        // Stop folding constants in the event of an error
        let (success, unit) = make_validator(
            "
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a + \"beep beep\"
const d := a + b + c    % 4*4 + 1 + 1 + 1
        ",
        );
        assert_eq!(false, success);
        if let Stmt::VarDecl {
            value: Some(expr), ..
        } = &unit.stmts().last().unwrap()
        {
            if let Expr::BinaryOp { left, right, .. } = *expr.clone() {
                // Check that (a + b) was folded, but not the + c
                assert!(!matches!(*left.clone(), Expr::BinaryOp { .. }));
                assert!(matches!(*right.clone(), Expr::Reference { .. }));
            } else {
                panic!("Something wrong happened! (folding did weird things!)");
            }
        } else {
            panic!("Something wrong happened! (not a var_decl!)");
        }

        // Constant folding tests with inner scopes are tested in the dedicated block stmt test

        // Even though positive literals are NatValues, the transferred type should still
        // allow implicit type variables to be assigned negative integers
        let (success, unit) = make_validator("var a := 1 + 1\na := -1");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        let (success, unit) = make_validator("var a := 1\na := -1");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // Constont folding allows compile-time expressions in length specifiers
        let (success, unit) = make_validator("const sz := 3\nconst a : string(sz) := 'aaa'");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::StringN(SequenceSize::Size(3)))
        );

        let (success, unit) = make_validator("const sz := 3\nconst a : char(sz + 10) := 'aaa'");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::CharN(SequenceSize::Size(13)))
        );

        // Constant folding length specifiers should still preserve parser validation semantics
        // On error, should resovle into the base types

        // Case: Zero Size
        let (success, unit) = make_validator("const sz := 0\nconst a : string(sz) := 'aaa'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let (success, unit) = make_validator("const sz := 0\nconst a : char(sz + 1 - 1) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Case: Negative size
        let (success, unit) = make_validator("const sz := -2\nconst a : string(sz) := 'aaa'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let (success, unit) = make_validator("const a : char(5 - 10) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Case: Too large
        let (success, unit) =
            make_validator("const sz := 16#10000 - 5 + 5\nconst a : string(sz) := 'aaa'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let (success, unit) = make_validator("const a : char(65530 + 10) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Case: Wrong compile-time type
        let (success, unit) = make_validator("const a : char(65530 + 0.0) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
        );

        let (success, unit) = make_validator("const a : string('noop' + 'boop') := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        // Case: Not actually a compile-time expression
        let (success, unit) = make_validator("var depend := 65530\nconst a : char(depend) := 'a'");
        assert_eq!(false, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Char)
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
        assert_eq!(
            true,
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // x use decl
        let (success, unit) = make_validator("a += 1\nvar b : int := 1");
        assert_eq!(false, success);
        assert_eq!(
            false,
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::TypeError
        );

        // x use use decl (only 1 error produced)
        let (success, unit) = make_validator("a += 1\na += 1\nvar b : int := 1");
        assert_eq!(false, success);
        assert_eq!(
            false,
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::TypeError
        );

        // x use
        let (success, unit) = make_validator("a += 1\n");
        assert_eq!(false, success);
        assert_eq!(
            false,
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::TypeError
        );

        // x use decl decl
        let (success, unit) =
            make_validator("a := a + 1\nvar a : int\nvar a : string % final type\n");
        assert_eq!(false, success);
        assert_eq!(
            false,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 0)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            true,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 1)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            true,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 2)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 0)
                .unwrap()
                .type_spec,
            TypeRef::TypeError
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 1)
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 2)
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        ); // Final type

        // x decl decl
        let (success, unit) = make_validator("var a : string\nvar a : real8 % final type");
        assert_eq!(false, success);
        assert_eq!(
            true,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 1)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            true,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 2)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 1)
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 2)
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Real8)
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Real8)
        ); // Final type

        // x use-in-init decl
        let (success, unit) = make_validator("var a : string := a + \"oops\"");
        assert_eq!(false, success);
        assert_eq!(
            false,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 0)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            true,
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 1)
                .unwrap()
                .is_declared
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 0)
                .unwrap()
                .type_spec,
            TypeRef::TypeError
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident_instance("a", 1)
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        ); // Final type
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
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Int)
        );

        let (success, unit) = make_validator("var a := 16#80000000");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::Nat)
        );

        let (success, unit) = make_validator("var a := 16#100000000");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::LongInt)
        );

        let (success, unit) = make_validator("var a := 16#100000000 + 16#100000000");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::LongInt)
        );

        let (success, unit) = make_validator("var a := 16#8000000000000000");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::LongNat)
        );

        let (success, unit) = make_validator("var a := 16#8000000000000000 + 1");
        assert_eq!(true, success);
        assert_eq!(
            unit.root_block()
                .borrow()
                .scope
                .get_ident("a")
                .as_ref()
                .unwrap()
                .type_spec,
            TypeRef::Primitive(PrimitiveType::LongNat)
        );
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
    }
}
