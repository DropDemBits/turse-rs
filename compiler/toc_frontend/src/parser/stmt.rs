//! Parser fragment, parsing all statements and declarations
use super::{Parser, ParsingStatus};
use crate::token::TokenType;
use toc_ast::ast::{Expr, Identifier, Stmt};
use toc_ast::block::BlockKind;
use toc_ast::types::{Type, TypeRef};
use toc_core::Location;

impl<'s> Parser<'s> {
    // --- Decl Parsing --- //

    pub(super) fn decl(&mut self) -> Result<Stmt, ParsingStatus> {
        // Update nesting
        self.stmt_nesting = self.stmt_nesting.saturating_add(1);

        if self.stmt_nesting > super::MAX_NESTING_DEPTH {
            // Over nesting depth
            self.context.borrow_mut().reporter.report_error(
                &self.current().location,
                format_args!("Implementation limit - Statement is nested too deeply"),
            );

            self.stmt_nesting = self
                .stmt_nesting
                .checked_sub(1)
                .expect("Mismatched nesting counts");

            // nom the token!
            self.next_token();
            return Err(ParsingStatus::Error);
        }

        let nom = self.current();
        let decl = match nom.token_type {
            TokenType::Var => self.decl_var(false),
            TokenType::Const => self.decl_var(true),
            TokenType::Type => self.decl_type(),
            _ => self.stmt(),
        };

        self.stmt_nesting = self
            .stmt_nesting
            .checked_sub(1)
            .expect("Mismatched nesting counts");
        decl
    }

    fn decl_var(&mut self, is_const: bool) -> Result<Stmt, ParsingStatus> {
        let decl_name = if is_const { "const" } else { "var" };

        // Consume decl_tok
        let decl_tok = self.next_token();

        // Grab identifier token
        let ident_tokens = {
            let mut idents = vec![self.expects(
                TokenType::Identifier,
                format_args!("Expected an identifier for the declared {}", decl_name),
            )?];

            while self.current().token_type == TokenType::Comma {
                // Consume comma
                self.next_token();

                // Identifier expected, but can break out of loop
                let token = self.expects(
                    TokenType::Identifier,
                    format_args!("Expected an identifier after the comma"),
                );

                if token.is_err() {
                    break;
                }

                // Add to identifier list
                idents.push(token.unwrap());
            }

            idents
        };

        // Grab typespec
        let mut type_spec = if self.current().token_type == TokenType::Colon {
            // Consume colon
            self.next_token();

            // Parse the type spec
            // If type parsing fails (i.e. TypeRef::TypeError is produced), the
            // assignment value is automagically skipped
            self.parse_type(&decl_tok.token_type)
        } else {
            // Will be resolved in the validation stage
            TypeRef::Unknown
        };

        // Grab assign value
        let has_init_expr;
        let assign_expr = if self.is_simple_assignment() {
            if self.current().token_type == TokenType::Equ {
                // Warn of mistake
                self.warn_equ_as_assign(self.current().location);
            }

            // Consume assign
            self.next_token();

            has_init_expr = matches!(self.current().token_type, TokenType::Init);

            // Get the assign expression
            let asn_expr = if has_init_expr {
                // Parse the init expr
                self.next_token();
                self.expr_init()
            } else {
                // Parse a normal expr
                self.expr()
            };

            asn_expr
                .map(|expr| Some(Box::new(expr)))
                .unwrap_or_else(|_| None)
        } else {
            has_init_expr = false;
            None
        };

        // Check if the init expression was required to be present or absent
        // Required to be present when the type_spec is an array, and is init-sized
        // Required to be absent in the case of a missing type spec
        if let Some(Type::Array { is_init_sized, .. }) = self
            .unit
            .as_ref()
            .unwrap()
            .types()
            .type_from_ref(&type_spec)
        {
            if *is_init_sized && !has_init_expr {
                // Error: Requires to have init, but has no init
                self.context.borrow_mut().reporter.report_error(
                    &self.current().location,
                    format_args!("Arrays with '*' as an end bound require an 'init' initializer"),
                );
            }
        } else if type_spec == TypeRef::Unknown && has_init_expr {
            // Error: Requires to not have init, but 'init' present
            // Force into a TypeError
            self.context.borrow_mut().reporter.report_error(
                &self.current().location,
                format_args!("Cannot infer a type from an 'init' initializer"),
            );
            type_spec = TypeRef::TypeError;
        }

        // Validate if the other declaration requirements have been met
        // Otherwise, produce an error and a TypeError
        if is_const && assign_expr.is_none() {
            // const declares require the assignment expression
            // Recoverable error
            // If the type is still unknown, just use TypeError as the type_spec
            self.context.borrow_mut().reporter.report_error(
                &decl_tok.location,
                format_args!("const declaration requires an initial value"),
            );

            if matches!(type_spec, TypeRef::Unknown) {
                type_spec = TypeRef::TypeError;
            }
        } else if type_spec == TypeRef::Unknown && assign_expr.is_none() {
            // No type inferrable
            // Recoverable error, just use TypeError as the type_spec
            self.context.borrow_mut().reporter.report_error(
                &decl_tok.location,
                format_args!("Cannot infer type for given {} declaration (no type specification or initial value given)", decl_name)
            );

            type_spec = TypeRef::TypeError;
        }

        // Declare the identifiers
        let idents: Vec<Identifier> = ident_tokens
            .into_iter()
            .map(|token| {
                self.declare_ident(token, type_spec, is_const, false)
                    .unwrap_or_else(|err| err.into())
            })
            .collect();

        Ok(Stmt::VarDecl {
            idents,
            type_spec,
            value: assign_expr,
            is_const,
        })
    }

    fn decl_type(&mut self) -> Result<Stmt, ParsingStatus> {
        // Nom "type"
        let type_tok = self.next_token();

        // Expect identifier (continue parsing after the identifier)
        let ident_tok = self
            .expects(
                TokenType::Identifier,
                format_args!("Expected identifier after 'type'"),
            )
            .ok();

        if ident_tok.is_some() {
            // Only require a colon if there was an identifier
            let _ = self.expects(
                TokenType::Colon,
                format_args!("Expected ':' after identifier"),
            );
        } else {
            // If there is a ':', consume it
            // Otherwise, continue onwards
            let _ = self.optional(TokenType::Colon);
        }

        // Get either the type spec, or the forward keyword
        let type_spec = if self.optional(TokenType::Forward) {
            None
        } else {
            // Get the type spec (in the type declaration context)
            Some(self.parse_type(&type_tok.token_type))
        };

        if ident_tok.is_none() {
            // If None, give an err (cannot declare a forward named type without an identifier)
            // Else, create a dummy type decl (provide validator access to any refs inside the type)
            return match type_spec {
                Some(type_spec) => {
                    // Can take a token from the previous, as the location doesn't matter
                    let dummy_ident = Identifier::new(
                        Location::new(),
                        TypeRef::TypeError,
                        "<dummy>".to_string(),
                        false,
                        false,
                        false,
                        0, // Not imported, just a dummy
                    );

                    Ok(Stmt::TypeDecl {
                        ident: dummy_ident,
                        resolved_type: Some(type_spec),
                        is_new_def: false, // Doesn't really matter
                    })
                }
                None => Err(ParsingStatus::Error),
            };
        }

        // Declare the actual type
        let ident_tok = ident_tok.unwrap();
        let old_ident = self.get_ident(self.get_token_lexeme(&ident_tok));

        if let Some(Type::Forward { is_resolved: false }) = old_ident.as_ref().and_then(|ident| {
            self.unit
                .as_ref()
                .unwrap()
                .types()
                .type_from_ref(&ident.type_spec)
        }) {
            // Resolve forwards (otherwise `is_resolved` would be true)

            // We known that the old ident is valid (from above condtion)
            let old_ident = old_ident.unwrap();

            match type_spec {
                Some(resolve_type) => {
                    // Resolving forward, update old resolving type
                    self.replace_type(&old_ident.type_spec, Type::Forward { is_resolved: true });

                    // Use the resolved type in the type decl
                    let mut ident = old_ident;
                    ident.location = ident_tok.location;

                    Ok(Stmt::TypeDecl {
                        ident,
                        resolved_type: Some(resolve_type),
                        is_new_def: false,
                    })
                }
                None => {
                    // Redeclaring forward, keep the same type
                    self.context.borrow_mut().reporter.report_error(
                        &ident_tok.location,
                        format_args!("Duplicate forward type declaration"),
                    );

                    let mut ident = old_ident;
                    ident.location = ident_tok.location;

                    Ok(Stmt::TypeDecl {
                        ident,
                        resolved_type: None,
                        is_new_def: false,
                    })
                }
            }
        } else {
            // Normal declaration
            match type_spec {
                Some(type_spec) => {
                    let alias_type = self.declare_type(Type::Alias { to: type_spec });

                    // Normal declare
                    Ok(Stmt::TypeDecl {
                        ident: self
                            .declare_ident(ident_tok, alias_type, true, true)
                            .unwrap_or_else(|err| err.into()),
                        resolved_type: Some(alias_type),
                        is_new_def: true,
                    })
                }
                None => {
                    let forward_type = self.declare_type(Type::Forward { is_resolved: false });

                    // Forward declare
                    Ok(Stmt::TypeDecl {
                        ident: self
                            .declare_ident(ident_tok, forward_type, true, true)
                            .unwrap_or_else(|err| err.into()),
                        resolved_type: None,
                        is_new_def: true,
                    })
                }
            }
        }
    }

    // --- Stmt Parsing --- //

    fn stmt(&mut self) -> Result<Stmt, ParsingStatus> {
        match self.current().token_type {
            TokenType::Identifier | TokenType::Caret => self.stmt_reference(),
            // TODO: Looks dirty, please merge with above
            TokenType::Addressint
            | TokenType::Int
            | TokenType::Int1
            | TokenType::Int2
            | TokenType::Int4
            | TokenType::Nat
            | TokenType::Nat1
            | TokenType::Nat2
            | TokenType::Nat4
            | TokenType::Real
            | TokenType::Real4
            | TokenType::Real8
            | TokenType::Boolean
            | TokenType::Char
            | TokenType::String_
                if matches!(self.peek().token_type, TokenType::At) =>
            {
                // Part of an indirect reference
                self.stmt_reference()
            }
            TokenType::Begin => self.stmt_block(),
            TokenType::Semicolon => {
                // Consume extra semicolon
                self.next_token();

                // Nothing produced
                Err(ParsingStatus::Skip)
            }
            _ => {
                // Nom as token isn't consumed by anything else
                self.next_token();

                self.context.borrow_mut().reporter.report_error(
                    &self.previous().location,
                    format_args!(
                        "'{}' does not begin a statement or declaration",
                        self.get_token_lexeme(self.previous())
                    ),
                );

                // Cause an error
                Err(ParsingStatus::Error)
            }
        }
    }

    fn stmt_reference(&mut self) -> Result<Stmt, ParsingStatus> {
        // Identifiers & References can begin either an assignment or a procedure call
        // Both take references as the primary expression

        // If the reference expr can't be parsed, replace it with an empty expression
        // instead of bailing out
        //
        // Referring to an identifier still has side effects in an invalid parse, as
        // it may use an undefined identifier
        let reference = self.expr_reference().unwrap_or(Expr::Empty);
        let is_compound_assign = self.is_compound_assignment();

        if is_compound_assign || self.is_simple_assignment() {
            // Is a (compound) assignment or '='
            // '=' is checked for as it's a common mistake to have '=' instead of ':='
            let assign_tok = self.next_token();

            let assign_op = if is_compound_assign {
                // Nom the other equ in the compound assignment
                self.next_token();
                Some(super::try_into_binary(assign_tok.token_type).expect("Not a binary operator"))
            } else if assign_tok.token_type != TokenType::Assign {
                // Current assignment op is '=', not ':='
                // Warn of mistake, convert into ':='
                let locate = self.previous().location;
                self.warn_equ_as_assign(locate);

                None
            } else {
                None
            };

            // If the assign value expr can't be parsed, replace it with an
            // Expr::Empty.
            // Assignment after variable declaration is really important
            // in the case where the identifier isn't declared before this
            // assignment, as this assignment makes the unknown identifier
            // known.
            let value = if matches!(self.current().token_type, TokenType::Init) {
                // Init expressions invalid in normal assign contexts
                self.context.borrow_mut().reporter.report_error(
                    &self.current().location,
                    format_args!(
                        "'init' assignments are only valid in constant and variable declarations"
                    ),
                );

                // Nom the expr anyways
                self.next_token();
                // Parse the init expression as it may use undefined identifiers
                self.expr_init().unwrap_or(Expr::Empty)
            } else {
                // Parse a regular expr
                self.expr().unwrap_or(Expr::Empty)
            };

            Ok(Stmt::Assign {
                var_ref: Box::new(reference),
                op: assign_op,
                value: Box::new(value),
            })
        } else {
            // Is a procedure call
            let proc_ref = if matches!(reference, Expr::Call { .. }) {
                reference
            } else {
                // Reference is not a call expression, wrap it in one
                Expr::Call {
                    arg_list: vec![],
                    span: *reference.get_span(),
                    eval_type: TypeRef::Unknown,
                    is_compile_eval: false,
                    paren_at: self.previous().location,
                    left: Box::new(reference),
                }
            };

            Ok(Stmt::ProcedureCall {
                proc_ref: Box::new(proc_ref),
            })
        }
    }

    fn stmt_block(&mut self) -> Result<Stmt, ParsingStatus> {
        // Nom begin
        let begin_loc = self.next_token().location;

        self.push_block(BlockKind::InnerBlock);

        let mut stmts = vec![];
        while !matches!(self.current().token_type, TokenType::End | TokenType::Eof) {
            let stmt = self.decl();

            // Only add the Stmt if it was parsed successfully
            if let Ok(stmt) = stmt {
                stmts.push(stmt);
            }
        }

        if matches!(self.current().token_type, TokenType::Eof) {
            // If at the end of file, do nothing
            // All of the statements have been absolved into this block

            self.context.borrow_mut().reporter.report_error(
                &begin_loc,
                format_args!("'begin' block does not have a matching 'end'"),
            );
        } else {
            let _ = self.expects(
                TokenType::End,
                format_args!("Expected 'end' to close off 'begin' block"),
            );
        }

        // Close the block
        let block = self.pop_block();

        Ok(Stmt::Block { block, stmts })
    }

    // --- Helpers --- //

    /// Checks if the current tokens form a compound assignment (operator '=')
    fn is_compound_assignment(&self) -> bool {
        if self.peek().token_type == TokenType::Equ {
            // Look ahead token is a '=', check if current is one of the valid compound assign operators
            matches!(
                &self.current().token_type,
                TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Star
                    | TokenType::Div
                    | TokenType::Slash
                    | TokenType::Rem
                    | TokenType::Mod
                    | TokenType::Exp
                    | TokenType::And
                    | TokenType::Or
                    | TokenType::Xor
                    | TokenType::Shl
                    | TokenType::Shr
                    | TokenType::Imply
            )
        } else {
            false
        }
    }

    /// Checks if the current token is a simple assignment (':=' or '=')
    fn is_simple_assignment(&self) -> bool {
        matches!(
            &self.current().token_type,
            TokenType::Assign | TokenType::Equ
        )
    }
}
