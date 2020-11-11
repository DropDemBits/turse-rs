//! Main parser for tokens to build the AST
// Parser fragments
mod expr;
mod stmt;
mod types;

use crate::context::CompileContext;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use toc_ast::ast::expr::{BinaryOp, Expr, ExprKind, UnaryOp};
use toc_ast::ast::ident::{IdentId, Identifier, RefKind};
use toc_ast::ast::stmt::{Block, BlockKind, Stmt, StmtKind};
use toc_ast::scope;
use toc_ast::types::{Type, TypeRef, TypeTable};
use toc_ast::unit::CodeUnit;
use toc_core::Location;

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Arguments;
use std::rc::Rc;

/// Maximum nesting depth during parsing
const MAX_NESTING_DEPTH: usize = 1024;

/// Parse Result Type
type ParseResult<T> = T;

/// Main parser
#[derive(Debug)]
pub struct Parser<'s> {
    /// Compile Context
    context: Rc<RefCell<CompileContext>>,
    /// File source used for getting lexemes for reporting
    source: &'s str,
    /// Scanner for scanning tokens
    scanner: Scanner<'s>,

    /// Previous token parsed
    previous: Token<'s>,
    /// Current token being parsed
    current: Token<'s>,
    /// Next token to parse
    peek: Token<'s>,

    /// Expression nesting depth
    expr_nesting: usize,
    /// Statement nesting depth
    stmt_nesting: usize,
    /// Type nesting depth
    type_nesting: usize,

    /// All identifiers being used as forward references
    forward_refs: HashSet<IdentId>,

    // Fragments of a code unit //
    /// If the parser is for the root unit
    is_main: bool,
    /// UnitScope handling identifier import semantics
    unit_scope: scope::UnitScope,
    /// Root Statement
    root_stmt: Option<Box<Stmt>>,
    /// TypeTable for types
    type_table: TypeTable,
}

impl<'s> Parser<'s> {
    pub fn new(
        mut scanner: Scanner<'s>,
        source: &'s str,
        is_main: bool,
        context: Rc<RefCell<CompileContext>>,
    ) -> Self {
        Self {
            context,
            source,
            previous: Token::new(TokenType::Error, Location::new()),
            current: scanner.next().unwrap_or_else(|| scanner.make_eof_here()),
            peek: scanner.next().unwrap_or_else(|| scanner.make_eof_here()),
            scanner,

            expr_nesting: 0,
            stmt_nesting: 0,
            type_nesting: 0,

            forward_refs: HashSet::new(),

            is_main,
            unit_scope: scope::UnitScope::new(),
            root_stmt: None,
            type_table: TypeTable::new(),
        }
    }

    /// Parses the token stream
    /// Returns if the parse has no errors
    pub fn parse(&mut self) -> bool {
        // TODO: Check if the root block is a unit block
        // Push root block
        self.unit_scope.push_block(BlockKind::Main);

        // Parse the statements
        let mut stmts = vec![];

        while !self.is_at_end() {
            let stmt = self.decl();

            if !matches!(stmt.kind, StmtKind::Nop | StmtKind::Error) {
                // Don't push No-ops
                stmts.push(stmt);
            } else if let StmtKind::Error = stmt.kind {
                // Skip to safe point
                self.skip_to_safe_point(|_| false);
            }
        }

        // Pop off root block
        let block = self.unit_scope.pop_block();
        let block = Block { block, stmts };

        // Make the root block
        let root_stmt = Stmt {
            kind: StmtKind::Block { block },
            span: Default::default(),
        };

        self.root_stmt = Some(Box::new(root_stmt));

        !self.context.borrow().reporter.has_error()
    }

    /// Takes the unit from the parser
    /// The parser cannot be used after this point
    pub fn take_unit(self) -> CodeUnit {
        // Move fragements into the CodeUnit
        CodeUnit::new(
            self.is_main,
            self.root_stmt.expect("needs parsing"),
            self.unit_scope,
            self.type_table,
        )
    }

    #[allow(dead_code)] // Only used by the tests
    fn stmts(&self) -> &Vec<Stmt> {
        if let StmtKind::Block { block } = &self.root_stmt.as_ref().expect("needs parsing").kind {
            &block.stmts
        } else {
            unreachable!("not a StmtKind::Block!!!")
        }
    }

    /// Gets the previous token in the stream
    fn previous(&self) -> &Token<'s> {
        &self.previous
    }

    /// Gets the current token in the stream
    fn current(&self) -> &Token<'s> {
        &self.current
    }

    /// Peeks at the next token in the stream
    /// `peek` is not affected by token stitching
    fn peek(&self) -> &Token<'s> {
        &self.peek
    }

    /// Advances `peek` without updating `current` and `previous`.
    ///
    /// Only used to support token stitching
    fn advance_peek(&mut self) {
        self.peek = self
            .scanner
            .next()
            .unwrap_or_else(|| self.scanner.make_eof_here());
    }

    /// Advances to the next token, returning the previous token
    fn next_token(&mut self) -> Token<'s> {
        fn stitch_into<'s>(
            token_type: TokenType,
            stitch_from: &Token<'s>,
            stitch_to: &Token<'s>,
        ) -> Token<'s> {
            let new_span = stitch_from.location.span_to(&stitch_to.location);
            Token::new(token_type, new_span)
        }

        std::mem::swap(&mut self.previous, &mut self.current);
        std::mem::swap(&mut self.current, &mut self.peek);
        self.advance_peek();

        // Try and do some token stitching of the current token
        if matches!(self.current.token_type, TokenType::Not | TokenType::Tilde) {
            // Stitch together with the next token
            match self.peek.token_type {
                TokenType::Equ => {
                    // Make `not =`
                    self.current = stitch_into(TokenType::NotEqu, &self.current, &self.peek);
                    self.advance_peek()
                }
                TokenType::In => {
                    // Make `not in`
                    self.current = stitch_into(TokenType::NotIn, &self.current, &self.peek);
                    self.advance_peek()
                }
                _ => {}
            }
        }

        self.previous().clone()
    }

    /// Checks if all of the tokens have been consumed yet
    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }

    /// Expects a certain token to be next in the stream. \
    /// If the current token matches the expected token, the current token is consumed.
    /// Otherwise an error message is reported.
    fn expects(&mut self, expected_type: TokenType, message: Arguments) -> Result<Token<'s>, ()> {
        if self.current().token_type == expected_type {
            Ok(self.next_token())
        } else {
            self.context
                .borrow_mut()
                .reporter
                .report_error(&self.current().location, message);

            Err(())
        }
    }

    /// Optionally expects a certain token to be next in the stream. \
    /// If the current token matches the expected token, the current token is
    /// consumed and true is returned.
    /// Otherwise, false is returned.
    fn optional(&mut self, optional_type: &TokenType) -> bool {
        if &self.current().token_type == optional_type {
            self.next_token();
            true
        } else {
            false
        }
    }

    /// Parse `var` attribute
    ///
    /// # Returns
    /// Returns true if the `var` attribute was parsed
    fn attrib_var(&mut self) -> bool {
        self.optional(&TokenType::Var)
    }

    /// Parse `register` attribute
    ///
    /// # Returns
    /// Returns true if the `register` attribute was parsed
    fn attrib_register(&mut self) -> bool {
        let has_attribute = self.optional(&TokenType::Register);

        has_attribute
    }

    /// Parse 'pervasive' attribute
    ///
    /// # Returns
    /// Returns true if the `pervasive` attribute (`pervasive` or `*`) was parsed
    fn attrib_pervasive(&mut self) -> bool {
        match self.current().token_type {
            TokenType::Pervasive | TokenType::Star => {
                // Nom token
                self.next_token();
                true
            }
            _ => false,
        }
    }

    fn warn_found_as_something_else(&self, found: &str, as_something: &str, at: &Location) {
        self.context.borrow_mut().reporter.report_warning(
            at,
            format_args!("'{}' found, assumed it to be '{}'", found, as_something),
        );
    }

    /// Gets the corresponding lexeme for the given Token.
    ///
    /// If the given token is of `TokenType::Eof`, the given lexeme is `"<end of file>"`.
    fn get_token_lexeme(&self, token: &Token) -> &str {
        let token_content = token.location.get_lexeme(self.source);

        if token_content.is_empty() {
            "<end of file>"
        } else {
            token_content
        }
    }

    /// Skips all tokens until a safe parsing point is reached.
    ///
    /// This should only be called in the event of a fatal error, as this will
    /// skip any valid expressions and statements caught up in the recovery
    /// process.
    ///
    /// # Parameters
    /// - `exclude` Tokens to exclude from the default safe list
    fn skip_to_safe_point<F>(&mut self, exclude: F)
    where
        F: Fn(&TokenType) -> bool,
    {
        loop {
            match self.current().token_type {
                TokenType::Var
                | TokenType::Const
                | TokenType::Type
                | TokenType::Bind
                | TokenType::Procedure
                | TokenType::Function
                | TokenType::Module
                | TokenType::Class
                | TokenType::Process
                | TokenType::Monitor
                | TokenType::Open
                | TokenType::Close
                | TokenType::Put
                | TokenType::Get
                | TokenType::Read
                | TokenType::Write
                | TokenType::Seek
                | TokenType::Tell
                | TokenType::For
                | TokenType::Loop
                | TokenType::Exit
                | TokenType::If
                | TokenType::Elif
                | TokenType::Elsif
                | TokenType::Elseif
                | TokenType::Else
                | TokenType::Case
                | TokenType::Assert
                | TokenType::Begin
                | TokenType::End
                | TokenType::EndIf
                | TokenType::EndFor
                | TokenType::EndLoop
                | TokenType::EndCase
                | TokenType::Return
                | TokenType::Result_
                | TokenType::New
                | TokenType::Free
                | TokenType::Tag
                | TokenType::Fork
                | TokenType::Signal
                | TokenType::Wait
                | TokenType::Pause
                | TokenType::Quit
                | TokenType::Semicolon
                | TokenType::Eof => {
                    if exclude(&self.current().token_type) {
                        // In the exclusion list, skip over
                        self.next_token();
                    } else {
                        // Not in the exclusion list, continue parsing
                        break;
                    }
                }
                _ => {
                    let _ = self.next_token();
                }
            }
        }
    }

    /// Skips to the tokens specified in `end_predicate`
    fn skip_to<F>(&mut self, end_predicate: F)
    where
        F: Fn(&TokenType) -> bool,
    {
        while !end_predicate(&self.current().token_type) {
            self.next_token();
        }
    }

    /// Declares an identifer in the current scope.
    ///
    /// Replaces any previous declaration of an identifier, if any exists.
    fn declare_ident(
        &mut self,
        ident: &Token,
        type_spec: TypeRef,
        ref_kind: RefKind,
        is_pervasive: bool,
    ) -> IdentId {
        let decl_location = ident.location;
        let name = decl_location.get_lexeme(self.source).to_string();

        self.unit_scope
            .declare_ident(name, decl_location, type_spec, ref_kind, is_pervasive)
    }

    /// Uses an identifer.
    ///
    /// If an identifier is not declared, a new identifier is made.
    fn use_ident(&mut self, ident: &Token) -> IdentId {
        let name = ident.location.get_lexeme(self.source);
        self.unit_scope.use_ident(name, ident.location)
    }

    /// Gets the identifier id from the current scope by name.
    ///
    /// Note: The latest identifier declaration is fetched, instead of a specific declaration
    fn get_ident(&self, name: &str) -> Option<IdentId> {
        self.unit_scope.get_ident_id(name)
    }

    /// Gets the identifier info for the given id
    fn get_ident_info(&self, id: &IdentId) -> &Identifier {
        self.unit_scope.get_ident_info(id)
    }

    // -- Wrappers around the type table -- //

    fn declare_type(&mut self, type_info: Type) -> TypeRef {
        TypeRef::Named(self.type_table.declare_type(type_info))
    }

    fn replace_type(&mut self, type_ref: &TypeRef, new_info: Type) {
        if let TypeRef::Named(replace_id) = type_ref {
            self.type_table.replace_type(*replace_id, new_info);
        } else {
            panic!("Not a named type ref");
        }
    }

    // -- Block Helpers -- //

    /// Pushes a new block onto the block list
    fn push_block(&mut self, block_kind: BlockKind) {
        // Push scope block
        self.unit_scope.push_block(block_kind);
    }

    /// Pops a block off of the block list, returning the block
    fn pop_block(&mut self) -> scope::ScopeBlock {
        // Pop scope block
        self.unit_scope.pop_block()
    }
}

/// Tries to convert the given token type into the corresponding biary operator
fn try_into_binary(op: TokenType) -> Result<BinaryOp, ()> {
    match op {
        TokenType::Plus => Ok(BinaryOp::Add),
        TokenType::Minus => Ok(BinaryOp::Sub),
        TokenType::Star => Ok(BinaryOp::Mul),
        TokenType::Div => Ok(BinaryOp::Div),
        TokenType::Slash => Ok(BinaryOp::RealDiv),
        TokenType::Mod => Ok(BinaryOp::Mod),
        TokenType::Rem => Ok(BinaryOp::Rem),
        TokenType::Exp => Ok(BinaryOp::Exp),
        TokenType::And => Ok(BinaryOp::And),
        TokenType::Or => Ok(BinaryOp::Or),
        TokenType::Xor => Ok(BinaryOp::Xor),
        TokenType::Shl => Ok(BinaryOp::Shl),
        TokenType::Shr => Ok(BinaryOp::Shr),
        TokenType::Less => Ok(BinaryOp::Less),
        TokenType::LessEqu => Ok(BinaryOp::LessEq),
        TokenType::Greater => Ok(BinaryOp::Greater),
        TokenType::GreaterEqu => Ok(BinaryOp::GreaterEq),
        TokenType::Equ => Ok(BinaryOp::Equal),
        TokenType::NotEqu => Ok(BinaryOp::NotEqual),
        TokenType::In => Ok(BinaryOp::In),
        TokenType::NotIn => Ok(BinaryOp::NotIn),
        TokenType::Imply => Ok(BinaryOp::Imply),
        TokenType::Dot => Ok(BinaryOp::Dot),
        TokenType::Arrow => Ok(BinaryOp::Arrow),
        _ => Err(()),
    }
}

/// Tries to convert the given token type into the corresponding unary operator
fn try_into_unary(op: TokenType) -> Result<UnaryOp, ()> {
    match op {
        TokenType::Pound => Ok(UnaryOp::NatCheat),
        TokenType::Caret => Ok(UnaryOp::Deref),
        TokenType::Plus => Ok(UnaryOp::Identity),
        TokenType::Minus => Ok(UnaryOp::Negate),
        TokenType::Not => Ok(UnaryOp::Not),
        _ => Err(()),
    }
}

/// Makes an error expression at the given location
fn make_error_expr(error_at: Location) -> Expr {
    Expr {
        kind: ExprKind::Error,
        eval_type: TypeRef::TypeError,
        is_compile_eval: false,
        span: error_at,
    }
}
