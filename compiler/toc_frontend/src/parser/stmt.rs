//! Parser fragment, parsing all statements and declarations
use super::{ParseResult, Parser};
use crate::token::TokenType;
use toc_ast::ast::expr::{Expr, ExprKind};
use toc_ast::ast::ident::{IdentRef, RefKind};
use toc_ast::ast::stmt::{self, Stmt, StmtKind};
use toc_ast::block::BlockKind;
use toc_ast::types::{Type, TypeRef};

impl<'s> Parser<'s> {
    // --- Decl Parsing --- //

    /// Performs a parse, updating the stmt nesting count
    fn with_stmt_nesting_tracking<F>(&mut self, mut f: F) -> ParseResult<Stmt>
    where
        F: FnMut(&mut Parser) -> ParseResult<Stmt>,
    {
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

            let err_stmt = Stmt {
                kind: StmtKind::Error,
                span: self.current().location,
            };

            return err_stmt;
        }

        // Call it (give ref to self)
        let result = f(self);

        self.stmt_nesting = self
            .stmt_nesting
            .checked_sub(1)
            .expect("Mismatched nesting counts");

        result
    }

    pub(super) fn decl(&mut self) -> ParseResult<Stmt> {
        // Update nesting
        let decl = self.with_stmt_nesting_tracking(|self_| {
            let nom = self_.current();

            match nom.token_type {
                TokenType::Var => self_.decl_var(false),
                TokenType::Const => self_.decl_var(true),
                TokenType::Type => self_.decl_type(),
                _ => self_.stmt(),
            }
        });

        if let StmtKind::Error = &decl.kind {
            // Skip to safe point
            self.skip_to_safe_point(|_| false);
        }

        decl
    }

    fn decl_var(&mut self, is_const: bool) -> ParseResult<Stmt> {
        let decl_name = if is_const { "const" } else { "var" };

        // Consume decl_tok
        let decl_tok = self.next_token();
        let span = decl_tok.location;

        // Grab identifier token
        let ident_tokens = {
            let mut idents = vec![];

            loop {
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

                if !self.optional(&TokenType::Comma) {
                    break;
                }
            }

            if idents.is_empty() {
                None
            } else {
                Some(idents)
            }
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
                self.warn_found_as_something_else("=", ":=", &self.current().location);
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

            Some(Box::new(asn_expr))
        } else {
            has_init_expr = false;
            None
        };

        // Check if the init expression was required to be present or absent
        // Required to be present when the type_spec is an array, and is init-sized
        // Required to be absent in the case of a missing type spec
        if let Some(Type::Array { is_init_sized, .. }) = self.type_table.type_from_ref(&type_spec) {
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

        let ref_kind = if is_const {
            RefKind::Const
        } else {
            RefKind::Var
        };

        // Declare the identifiers
        let idents: Option<Vec<IdentRef>> = ident_tokens.map(|toks| {
            toks.into_iter()
                .map(|token| {
                    let location = token.location;
                    let use_id = self.declare_ident(&token, type_spec, ref_kind, false);

                    IdentRef::new(use_id, location)
                })
                .collect()
        });

        // Make the span
        let span = span.span_to(&self.previous().location);

        Stmt {
            kind: StmtKind::VarDecl {
                idents,
                type_spec,
                value: assign_expr,
                is_const,
            },
            span,
        }
    }

    fn decl_type(&mut self) -> ParseResult<Stmt> {
        // Nom "type"
        let type_tok = self.next_token();
        let span = type_tok.location;

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
            let _ = self.optional(&TokenType::Colon);
        }

        // Get either the type spec, or the forward keyword
        let type_spec = if self.optional(&TokenType::Forward) {
            None
        } else {
            // Get the type spec (in the type declaration context)
            Some(self.parse_type(&type_tok.token_type))
        };

        let span = span.span_to(&self.previous().location);

        if ident_tok.is_none() {
            // If None, give an err (cannot declare a forward named type without an identifier)
            // Else, create a dummy type decl (provide validator access to any refs inside the type)
            let kind = match type_spec {
                Some(type_spec) => {
                    StmtKind::TypeDecl {
                        ident: None,
                        resolved_type: Some(type_spec),
                        is_new_def: false, // Doesn't really matter
                    }
                }
                None => StmtKind::Error,
            };

            return Stmt { kind, span };
        }

        // Declare the actual type
        let ident_tok = ident_tok.unwrap();
        let old_ident = self.get_ident(self.get_token_lexeme(&ident_tok));

        let kind = if let Some(Type::Forward { is_resolved: false }) =
            old_ident.as_ref().and_then(|id| {
                let ident = self.get_ident_info(id);

                self.type_table.type_from_ref(&ident.type_spec)
            }) {
            // Resolve forwards (otherwise `is_resolved` would be true)

            // We known that the old ident is valid (from above condtion)
            let old_ident_id = old_ident.unwrap();
            let old_ident_tyspec = self.get_ident_info(&old_ident_id).type_spec;

            let resolved_type = if let Some(resolve_type) = type_spec {
                // Resolving forward, update old resolving type
                self.replace_type(&old_ident_tyspec, Type::Forward { is_resolved: true });

                Some(resolve_type)
            } else {
                // Redeclaring forward, keep the same type
                self.context.borrow_mut().reporter.report_error(
                    &ident_tok.location,
                    format_args!("Duplicate forward type declaration"),
                );

                None
            };

            StmtKind::TypeDecl {
                ident: Some(IdentRef::new(old_ident_id, ident_tok.location)),
                resolved_type,
                is_new_def: false,
            }
        } else {
            // New declaration
            let (ident, resolved_type) = if let Some(type_spec) = type_spec {
                let alias_type = self.declare_type(Type::Alias { to: type_spec });
                let location = ident_tok.location;
                let new_id = self.declare_ident(&ident_tok, alias_type, RefKind::Type, false);

                // Normal declare
                ((IdentRef::new(new_id, location)), Some(alias_type))
            } else {
                let forward_type = self.declare_type(Type::Forward { is_resolved: false });
                let location = ident_tok.location;
                let new_id = self.declare_ident(&ident_tok, forward_type, RefKind::Type, false);

                // Forward declare
                ((IdentRef::new(new_id, location)), None)
            };

            StmtKind::TypeDecl {
                ident: Some(ident),
                resolved_type,
                is_new_def: true,
            }
        };

        Stmt { kind, span }
    }

    // --- Stmt Parsing --- //

    fn stmt(&mut self) -> ParseResult<Stmt> {
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
            TokenType::If => self.stmt_if(false),
            TokenType::Elseif | TokenType::Elsif | TokenType::Elif => self.stmt_err_elseif(),
            TokenType::Else => self.stmt_err_else(),
            TokenType::Semicolon => {
                // Consume extra semicolon
                self.next_token();

                // Nothing produced
                Stmt {
                    kind: StmtKind::Nop,
                    span: self.previous().location,
                }
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

                // Nothing produced
                Stmt {
                    kind: StmtKind::Error,
                    span: self.previous().location,
                }
            }
        }
    }

    fn stmt_reference(&mut self) -> ParseResult<Stmt> {
        // Identifiers & References can begin either an assignment or a procedure call
        // Both take references as the primary expression

        let span = self.current().location;

        // Parse the reference expr
        let reference = self.expr_reference();
        let is_compound_assign = self.is_compound_assignment();

        let kind = if is_compound_assign || self.is_simple_assignment() {
            // Is a (compound) assignment or '='
            // '=' is checked for as it's a common mistake to have '=' instead of ':='
            let assign_tok = self.next_token();

            let assign_op = if is_compound_assign {
                // Nom the other equ in the compound assignment
                self.next_token();

                Some(super::try_into_binary(assign_tok.token_type).expect("Not a binary operator"))
            } else {
                if assign_tok.token_type == TokenType::Equ {
                    // Current assignment op is '=', not ':='
                    // Warn of mistake, convert into ':='
                    let locate = self.previous().location;
                    self.warn_found_as_something_else("=", ":=", &locate);
                }

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
                self.expr_init()
            } else {
                // Parse a regular expr
                self.expr()
            };

            StmtKind::Assign {
                var_ref: Box::new(reference),
                op: assign_op,
                value: Box::new(value),
            }
        } else {
            // Is a procedure call
            let proc_ref = if matches!(reference.kind, ExprKind::Call { .. }) {
                reference
            } else {
                // Reference is not a call expression, wrap it in one
                Expr {
                    span: *reference.get_span(),
                    eval_type: TypeRef::Unknown,
                    is_compile_eval: false,
                    kind: ExprKind::Call {
                        left: Box::new(reference),
                        paren_at: self.previous().location,
                        arg_list: vec![],
                    },
                }
            };

            StmtKind::ProcedureCall {
                proc_ref: Box::new(proc_ref),
            }
        };

        // Make span
        let span = span.span_to(&self.previous().location);

        Stmt { kind, span }
    }

    fn stmt_block(&mut self) -> ParseResult<Stmt> {
        // Nom begin
        let begin_loc = self.next_token().location;
        let block = self.parse_block(BlockKind::InnerBlock, |tok_type| {
            !matches!(tok_type, TokenType::End)
        });

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

        let span = begin_loc.span_to(&self.previous().location);

        Stmt {
            kind: StmtKind::Block { block },
            span,
        }
    }

    fn stmt_if(&mut self, as_elsif: bool) -> ParseResult<Stmt> {
        // Nom 'if'
        let if_loc = self.next_token().location;

        if as_elsif {
            debug_assert!(matches!(
                self.previous().token_type,
                TokenType::Elsif | TokenType::Elif | TokenType::Elseif
            ));

            // Canonical version is `elsif`, warn about the alternates
            match &self.previous().token_type {
                TokenType::Elseif => {
                    self.warn_found_as_something_else("elseif", "elsif", &self.current().location)
                }
                TokenType::Elif => {
                    self.warn_found_as_something_else("elif", "elsif", &self.current().location)
                }
                _ => {}
            }
        } else {
            debug_assert!(matches!(self.previous().token_type, TokenType::If));
        }

        // Parse conditional
        let condition = self.expr();
        let condition = Box::new(condition);

        let _ = self.expects(
            TokenType::Then,
            format_args!("Expected 'then' after boolean expression"),
        );

        // Parse true branch
        let true_branch = self.parse_block(BlockKind::InnerBlock, |tok_type| {
            !matches!(
                tok_type,
                TokenType::End
                    | TokenType::EndIf
                    | TokenType::Elsif
                    | TokenType::Elif
                    | TokenType::Elseif
                    | TokenType::Else
            )
        });

        // Make a span to the end of the first if block
        let top_span = if_loc.span_to(&self.previous().location);

        // Nom either elsif or else
        let false_branch = if matches!(
            &self.current().token_type,
            TokenType::Elsif | TokenType::Elif | TokenType::Elseif
        ) {
            // Update nesting count
            let stmt = self.with_stmt_nesting_tracking(|self_| self_.stmt_if(true));

            if let StmtKind::Error = &stmt.kind {
                // Skip to known end point
                self.skip_to(|maybe_safe| matches!(maybe_safe, TokenType::End | TokenType::EndIf));
                self.nom_endif_or_end_if();
            }

            Some(stmt)
        } else {
            let branch = if matches!(self.current().token_type, TokenType::Else) {
                // Nom optional else block
                Some(self.parse_else_block())
            } else {
                None
            };

            // Either nom `end` `if` or `endif`
            self.nom_endif_or_end_if();
            branch
        };

        let true_branch = Box::new(Stmt {
            kind: StmtKind::Block { block: true_branch },
            span: top_span,
        });
        let false_branch = false_branch.map(Box::new);

        // Give back top level if statement
        Stmt {
            kind: StmtKind::If {
                condition,
                true_branch,
                false_branch,
            },
            span: top_span,
        }
    }

    fn parse_else_block(&mut self) -> ParseResult<Stmt> {
        // Nom 'else'
        let else_loc = self.next_token().location;
        debug_assert_eq!(&self.previous().token_type, &TokenType::Else);

        let block = self.parse_block(BlockKind::InnerBlock, |tok_type| {
            !matches!(tok_type, TokenType::End | TokenType::EndIf)
        });

        let span = else_loc.span_to(&self.previous().location);

        Stmt {
            kind: StmtKind::Block { block },
            span,
        }
    }

    /// Error recovery for else on a top level block
    fn stmt_err_else(&mut self) -> ParseResult<Stmt> {
        self.context.borrow_mut().reporter.report_error(
            &self.current().location,
            format_args!("'else' without matching 'if'"),
        );

        let else_stmt = self.parse_else_block();
        self.nom_endif_or_end_if();
        else_stmt
    }

    // Error recovery for elseif on a top level block
    fn stmt_err_elseif(&mut self) -> ParseResult<Stmt> {
        self.context.borrow_mut().reporter.report_error(
            &self.current().location,
            format_args!("'{}' without matching 'if'", &self.current().token_type),
        );

        // Parse as a regular elsif
        self.stmt_if(true)
    }

    // --- Helpers --- //

    fn nom_endif_or_end_if(&mut self) {
        if self.optional(&TokenType::EndIf) {
            // report warning!
            self.warn_found_as_something_else("endif", "end if", &self.previous().location);
        } else {
            // Nom `end` `if`
            let _ = self.expects(
                TokenType::End,
                format_args!("Expected 'end' at the end of the if statement"),
            );
            let _ = self.expects(TokenType::If, format_args!("Expected 'if' after 'end'"));
        }
    }

    /// Parses a block of statements.
    ///
    /// # Parameters:
    /// - `block_kind`: The block kind the statements exist in.
    /// - `end_predicate`: A function that, if evaluates to false, will stop parsing statements.
    ///
    /// # Returns
    /// Returns an `ast::Block` containing the parsed statements and associated scope block.
    fn parse_block<T>(&mut self, block_kind: BlockKind, end_predicate: T) -> stmt::Block
    where
        T: Fn(&TokenType) -> bool,
    {
        self.push_block(block_kind);

        let mut stmts = vec![];
        while !matches!(&self.current().token_type, &TokenType::Eof)
            && end_predicate(&self.current().token_type)
        {
            let stmt = self.decl();

            // Only add the Stmt if it was parsed successfully
            if !matches!(stmt.kind, StmtKind::Nop | StmtKind::Error) {
                stmts.push(stmt);
            } else if matches!(stmt.kind, StmtKind::Error) {
                // Skip to safe point
                self.skip_to_safe_point(|_| false);
            }
        }

        // Close the block
        let block = self.pop_block();

        stmt::Block { block, stmts }
    }

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
