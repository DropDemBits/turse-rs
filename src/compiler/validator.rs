//! Type validator upon the AST tree
//! Performs the majority of the semantic validation pass
//! - Propogates and checks expressions for type correctness
//! - Resolves identifiers into their final types
//! - Checks and evaluates compile-time expressions
use crate::compiler::ast::{ASTVisitorMut, Expr, Stmt};
use crate::compiler::block::CodeBlock;
use crate::compiler::token::TokenType;
use crate::compiler::types::{self, PrimitiveType, TypeRef, TypeTable};
use crate::status_reporter::StatusReporter;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct Validator<'a> {
    /// Status reporter for the type validator
    reporter: StatusReporter,
    type_table: &'a mut TypeTable,
    active_scope: Weak<RefCell<CodeBlock>>,
}

impl<'a> Validator<'a> {
    pub fn new(root_block: &Rc<RefCell<CodeBlock>>, type_table: &'a mut TypeTable) -> Self {
        Self {
            reporter: StatusReporter::new(),
            type_table,
            active_scope: Rc::downgrade(root_block),
        }
    }
}

impl ASTVisitorMut<(), ()> for Validator<'_> {
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
                }

                // Visit the expression to update the eval type
                if value.is_some() || *type_spec == TypeRef::Unknown {
                    let expr = value.as_mut().unwrap();
                    self.visit_expr(expr);
                    is_compile_eval = expr.is_compile_eval();
                }

                
                if *type_spec == TypeRef::Unknown {
                    // Unknown type, use the type of the expr
                    // Safe to unwrap as if no expr was provided, the type_spec would be TypeError    

                    let expr = value.as_ref().unwrap();
                    *type_spec = expr.get_eval_type();
                } else if value.is_some() {
                    // Validate that the types are assignable
                    let expr = value.as_ref().unwrap();

                    let left_type = &type_spec;
                    let right_type = &expr.get_eval_type();

                    if !types::is_error(right_type) {
                        // TODO: Resolve & De-alias type refs
                        debug_assert!(types::is_base_type(left_type, &self.type_table), "Of type {:?}", left_type);
                        debug_assert!(types::is_base_type(right_type, &self.type_table), "Of type {:?}", right_type);

                        // Validate that the types are assignable
                        if !types::is_assignable_to(&left_type, &right_type, &self.type_table) {
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

                let left_type = &var_ref.get_eval_type();
                let right_type = &value.get_eval_type();

                // Validate that the types are assignable for the given operation
                if types::is_error(right_type) {
                    // Silently drop propogated TypeErrors
                    return;
                }

                // TODO: Resolve & De-alias type refs
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
                    let produce_type = check_binary_operands((&var_ref, left_type), &op.token_type, (&value, right_type), &self.type_table);
                    if produce_type.is_err() || !types::is_assignable_to(left_type, &produce_type.unwrap(), &self.type_table) {
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
                // TODO: Change our active scope
                for stmt in block.borrow_mut().stmts.iter_mut() {
                    self.visit_stmt(stmt);
                }
                // TODO: Revert to previous scope
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
                            TokenType::And | TokenType::Or | TokenType::Xor =>
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be scalars (int, real, or nat) or booleans", op)),
                            TokenType::Shl | TokenType::Shr => 
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be integers (int, or nat)", op)),
                            TokenType::Imply =>
                                self.reporter.report_error(loc, format_args!("Operands of '{}' must both be booleans", op)),
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::block::CodeUnit;
    use crate::compiler::scanner::Scanner;
    use crate::compiler::parser::Parser;

    /// Runs the validator on the given source
    /// Parsing & scanning must complete successfully
    /// Returns true if the AST is valid
    fn run_validator(source: &str) -> bool {
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
        
        successful_validate
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
        eprintln!("here:");
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

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 + 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na += 1.0"));

        // string cannot be assigned into number
        assert_eq!(false, run_validator("var a : int := \"str\" + \"str\""));
        assert_eq!(false, run_validator("var a : int := 1\na += \"str\""));

        // number cannot be assigned into string
        assert_eq!(false, run_validator("var a : string := 1 + 1.0"));
        assert_eq!(false, run_validator("var a : string := \"str\"\na += 1.0"));

        // TODO: Remaining cases for set types
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

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 - 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na -= 1.0"));

        // TODO: Remaining cases for set types

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

        // real cannot be assigned into int
        assert_eq!(false, run_validator("var a : int := 1 * 1.0"));
        assert_eq!(false, run_validator("var a : int := 1\na *= 1.0"));

        // TODO: Remaining cases for set types

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
}