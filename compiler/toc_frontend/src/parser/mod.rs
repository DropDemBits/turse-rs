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
use toc_ast::ast::stmt::{Block, Stmt, StmtKind};
use toc_ast::block::{BlockKind, CodeUnit};
use toc_ast::scope;
use toc_ast::types::{Type, TypeRef, TypeTable};
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::context::CompileContext;
    use crate::scanner::Scanner;
    use std::{cell::RefCell, rc::Rc};
    use toc_ast::ast::{expr::ExprKind, stmt::Stmt, stmt::StmtKind, types::TypeKind};
    use toc_ast::types::{PrimitiveType, Type, TypeRef};

    fn make_test_parser(source: &str) -> Parser {
        let context = Rc::new(RefCell::new(CompileContext::new()));
        let scanner = Scanner::scan_source(source, context.clone());

        Parser::new(scanner, source, true, context)
    }

    // Get the latest version of the identifier
    fn get_ident(parser: &Parser, name: &str) -> Option<Identifier> {
        parser
            .root_stmt
            .as_ref()
            .and_then(|block| {
                if let StmtKind::Block { block } = &block.kind {
                    block.block.get_ident_id(name)
                } else {
                    None
                }
            })
            .map(|id| parser.get_ident_info(&id).to_owned())
    }

    // Gets the identifier with the specified instance
    fn get_ident_instance(parser: &Parser, id: IdentId) -> Option<Identifier> {
        Some(parser.get_ident_info(&id).to_owned())
    }

    fn get_ident_type(parser: &Parser, name: &str) -> TypeRef {
        get_ident(parser, name).expect("No identifier").type_spec
    }

    #[test]
    fn test_var_decl() {
        let mut parser = make_test_parser(
            "
        % Valid forms
        var a : int := 1
        var b : int
        var c := 3 + 6 ** 2
        var d, e, f : string := \"hai\"
        var x, y, z : real := 42e10
        
        % Accepted forms
        var g : int = -5
        var h : int = -10 + 3 * 2
        var i, j, k : nat = 20 + 40 shl 5
        ",
        );
        assert!(parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(ty_var),
            ..
        } = &parser.stmts()[4].kind
        {
            assert!(matches!(
                ty_var.kind,
                TypeKind::Primitive(PrimitiveType::Real)
            ));
        } else {
            unreachable!()
        }

        if let StmtKind::VarDecl {
            type_spec: Some(ty_var),
            ..
        } = &parser.stmts()[3].kind
        {
            assert!(matches!(
                ty_var.kind,
                TypeKind::Primitive(PrimitiveType::String_)
            ));
        } else {
            unreachable!()
        }

        if let StmtKind::VarDecl {
            type_spec: Some(ty_var),
            ..
        } = &parser.stmts()[7].kind
        {
            assert!(matches!(
                ty_var.kind,
                TypeKind::Primitive(PrimitiveType::Nat)
            ));
        } else {
            unreachable!()
        }

        // Invalid forms - can't deduce type
        let mut parser = make_test_parser(
            "
        % Invalid forms
        var a
        var c
        var e, b, k",
        );
        assert!(!parser.parse());
        for stmt in parser.stmts().iter() {
            assert!(matches!(stmt.kind, StmtKind::VarDecl {
                type_spec: Some(_), ..
            }));
        }

        // Invalid forms - comma after last item
        let mut parser = make_test_parser(
            "
        % Invalid forms
        var a, b, c, : int := 5",
        );
        assert!(!parser.parse());
    }

    #[test]
    fn test_const_decl() {
        let mut parser = make_test_parser(
            "
        % Valid forms
        const a : int := 1
        const b := 5.0
        const c, d : int := 3
        const e, f := 3 + 6 ** 2
        
        % Accepted forms
        const g : int = -5
        const h : int = -10 + 3 * 2",
        );
        assert!(parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(ty_var),
            ..
        } = &parser.stmts()[2].kind
        {
            assert!(matches!(
                ty_var.kind,
                TypeKind::Primitive(PrimitiveType::Int)
            ));
        } else {
            unreachable!()
        }

        // Invalid forms
        let mut parser = make_test_parser(
            "
        % Invalid forms - No type or value
        const a
        const b",
        );
        assert!(!parser.parse());
        for stmt in parser.stmts().iter() {
            assert!(matches!(stmt.kind, StmtKind::VarDecl {
                type_spec: Some(_), ..
            }));
        }

        let mut parser = make_test_parser(
            "
        % Invalid forms - No value
        const a : int
        const b : int",
        );
        assert!(!parser.parse());
    }

    #[test]
    fn test_pointer_type_invalids() {
        // Pointer type expects "to"
        let mut parser = make_test_parser("var a : pointer int");
        assert!(!parser.parse());

        // Pointer type expects "to"
        let mut parser = make_test_parser("var a : unchecked pointer int");
        assert!(!parser.parse());

        // Pointer type expects type
        let mut parser = make_test_parser("var a : ^");
        assert!(!parser.parse());

        // Pointer type expects type
        let mut parser = make_test_parser("var a : pointer");
        assert!(!parser.parse());

        // Pointer type expects type
        let mut parser = make_test_parser("var a : unchecked ^");
        assert!(!parser.parse());

        // Pointer type expects type
        let mut parser = make_test_parser("var a : unchecked pointer");
        assert!(!parser.parse());
    }

    #[test]
    fn test_subprogram_type_invalids() {
        // Function expects ':' before result type
        let mut parser = make_test_parser("var a : function a int");
        assert!(!parser.parse());

        // Function expects type after ':'
        let mut parser = make_test_parser("var a : function a :");
        assert!(!parser.parse());

        // Function type declaration expects '()' if there are no parameters (only as a warning)
        let mut parser = make_test_parser("var a : function amphy : int");
        assert!(parser.parse());

        // Function / procedure expects identifier after keyword (this can be made optional in the future)
        let mut parser = make_test_parser("var a : procedure");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var a : function : int");
        assert!(!parser.parse());
    }

    #[test]
    fn test_range_type_invalids() {
        // Inferred range end is only valid in array range contexts
        let mut parser = make_test_parser("var a : 1 .. *");
        assert!(!parser.parse());

        // No range end
        let mut parser = make_test_parser("var a : 1 .. ");
        assert!(!parser.parse());

        // No range end in function parameter
        let mut parser = make_test_parser("var a : function _ (a : array 1 .. )");
        assert!(!parser.parse());
    }

    #[test]
    fn test_set_type_invalids() {
        // Set type declarations are only valid in type statements
        let mut parser = make_test_parser("var a : set of 1 .. 3");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Var);

        // Set type declarations expect 'of'
        let mut parser = make_test_parser("type a : set 1 .. 3");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Type);

        let mut parser = make_test_parser("type a : set");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Type);

        // Set type declarations expect a range
        let mut parser = make_test_parser("type a : set of ");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Type);
    }

    #[test]
    fn test_array_type_invalids() {
        // Flexible array cannot have an implicit range
        let mut parser = make_test_parser("var inv : flexible array 1 .. * of real");
        assert!(!parser.parse());

        // Array cannot have a flexible array as an element type
        let mut parser =
            make_test_parser("var inv : flexible array 1 .. 2 of flexible array 1 .. 2 of real");
        assert!(!parser.parse());

        let mut parser =
            make_test_parser("var inv : array 1 .. 2 of flexible array 1 .. 2 of real");
        assert!(!parser.parse());

        let mut parser =
            make_test_parser("var inv : array 1 .. * of flexible array 1 .. 2 of real");
        assert!(!parser.parse());

        // Array cannot have an implicit size array as an element type
        let mut parser =
            make_test_parser("var inv : flexible array 1 .. 2 array of 1 .. * of real");
        assert!(!parser.parse());

        let mut parser =
            make_test_parser("var inv : flexible array 1 .. 2 of array 1 .. * of real");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var inv : array 1 .. 2 of array 1 .. * of real");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var inv : array 1 .. * of array 1 .. * of real");
        assert!(!parser.parse());

        // Implicit size array cannot have more than one range specifier
        let mut parser = make_test_parser("var inv : array 1 .. *, char of real");
        assert!(!parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(ty_array),
            ..
        } = &parser.stmts()[0].kind
        {
            if let TypeKind::Array { ranges, .. } = &ty_array.kind {
                assert_eq!(ranges.len(), 1);
            } else {
                panic!("Not an array");
            }
        } else {
            unreachable!()
        }

        let mut parser = make_test_parser("var inv : array 1 .. *, 1 .. *, char of real");
        assert!(!parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(ty_array),
            ..
        } = &parser.stmts()[0].kind
        {
            if let TypeKind::Array { ranges, .. } = &ty_array.kind {
                assert_eq!(ranges.len(), 1);
            } else {
                panic!("Not an array");
            }
        } else {
            unreachable!()
        }

        // Implicit size range is only allowed for the first range specifier
        let mut parser = make_test_parser("var inv : array 1 .. 2, 1 .. *, char of real");
        assert!(!parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(ty_array),
            ..
        } = &parser.stmts()[0].kind
        {
            if let TypeKind::Array { ranges, .. } = &ty_array.kind {
                assert_eq!(ranges.len(), 3);
            } else {
                panic!("Not an array");
            }
        } else {
            unreachable!()
        }
    }

    #[test]
    fn test_ident_ref_invalids() {
        // Missing identifier after '.'
        let mut parser = make_test_parser("var inv : an.ident.list.");
        assert!(!parser.parse());

        // Expression does not contain only field refs
        let mut parser = make_test_parser("var inv : an.ident.list.of(1, 2, 3)");
        assert!(!parser.parse());
    }

    #[test]
    fn test_enum_invalids() {
        // Enums can have 1 or more fields
        let mut parser = make_test_parser("type a : enum (a)");
        assert!(parser.parse());

        let mut parser = make_test_parser("type a : enum (a, b, c)");
        assert!(parser.parse());

        // At least one field must be specified
        let mut parser = make_test_parser("type a : enum ()\nvar b : int");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);
        // Ensure this gets parsed
        assert!(parser.stmts().get(1).is_some());

        let mut parser = make_test_parser("type a : enum )\nvar b : int");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);
        // Ensure this gets parsed
        assert!(parser.stmts().get(1).is_some());

        let mut parser = make_test_parser("type a : enum \nvar b : int");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);
        // Ensure this gets parsed
        assert!(parser.stmts().get(1).is_some());

        // Right paren is required, but should not create an error type
        let mut parser = make_test_parser("type a : enum (a");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);

        let mut parser = make_test_parser("type a : enum (a, b, c");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);

        // Field identifiers must be separated by comma delimiters (ends the list otherwise)
        let mut parser = make_test_parser("var c := 3\ntype a : enum (a, b c += 1");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);

        // Non-identifiers terminate the list
        let mut parser = make_test_parser("type a : enum (a, to\nvar b : int");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);
        assert!(parser.stmts().get(1).is_some());

        // Enums not in top-level type contexts are rejected
        // (i.e. anonymous enums are not allowed), but still produce
        // an enum type
        let mut parser = make_test_parser("var a : enum (a, b, c)");
        assert!(!parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(type_spec),
            ..
        } = &parser.stmts()[0].kind
        {
            assert!(matches!(type_spec.kind, TypeKind::Enum { .. }))
        } else {
            unreachable!()
        }

        let mut parser = make_test_parser("const a : enum (a, b, c) := 2");
        assert!(!parser.parse());
        if let StmtKind::VarDecl {
            type_spec: Some(type_spec),
            ..
        } = &parser.stmts()[0].kind
        {
            assert!(matches!(type_spec.kind, TypeKind::Enum { .. }))
        } else {
            unreachable!()
        }

        let mut parser = make_test_parser("type a : set of enum (a, b, c)");
        assert!(!parser.parse());
    }

    #[test]
    fn test_type_decl() {
        let mut parser = make_test_parser("type a : int");
        assert!(parser.parse());

        // Requires identifer, will consume the type and colon
        let mut parser = make_test_parser("type : a := 1");
        assert!(!parser.parse());
        // The 'type' stmt should still produce a statement. providing validator access to 'a'
        // The a := 1 should not produce a statement (the a should be consumed by "type")
        assert_eq!(parser.stmts().len(), 1);

        // Requires colon, will parse the rest and produce a declaration
        let mut parser = make_test_parser("var a : string\ntype a");
        assert!(!parser.parse());
        assert!(get_ident(&parser, "a").is_some());
        let type_ref = get_ident(&parser, "a").unwrap().type_spec;
        assert!(
            matches!(parser.type_table.type_from_ref(&type_ref), Some(_)),
            "From ref {:?} to {:?}",
            type_ref,
            parser.type_table.type_from_ref(&type_ref)
        );
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Type);

        let mut parser = make_test_parser("var a : string\ntype a int");
        assert!(!parser.parse());
        assert!(get_ident(&parser, "a").is_some());
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Type);
        assert_eq!(
            true,
            matches!(
                parser
                    .type_table
                    .type_from_ref(&get_ident(&parser, "a").unwrap().type_spec),
                Some(Type::Forward { .. }) // Not resolved until the validator stage
            )
        );
        assert_eq!(get_ident(&parser, "a").unwrap().ref_kind, RefKind::Type);

        // Check that the forward reference is updated
        let mut parser = make_test_parser("type a : forward");
        assert!(parser.parse()); // Checked by the validator
        assert_eq!(
            true,
            matches!(
                parser
                    .type_table
                    .type_from_ref(&get_ident(&parser, "a").unwrap().type_spec),
                Some(Type::Forward { is_resolved: false })
            )
        );

        // Check that the forward reference is updated
        let mut parser = make_test_parser("type a : forward\ntype a : int");
        assert!(parser.parse()); // Checked by the validator
        assert_eq!(
            true,
            matches!(
                parser
                    .type_table
                    .type_from_ref(&get_ident(&parser, "a").unwrap().type_spec),
                Some(Type::Forward { is_resolved: true })
            )
        );

        // Forward refs after resolves create a new type
        let mut parser = make_test_parser("type a : forward\ntype a : int\ntype a : forward");
        assert!(parser.parse()); // Checked at validator time
        assert_eq!(true, get_ident_instance(&parser, IdentId(1)).is_some());

        // Duplicate forward refs should not affect resolved state (and should share IdentId)
        let mut parser = make_test_parser("type a : forward\ntype a : forward\ntype a : int");
        assert!(!parser.parse());
        assert_eq!(
            true,
            matches!(
                parser
                    .type_table
                    .type_from_ref(&get_ident_instance(&parser, IdentId(0)).unwrap().type_spec),
                Some(Type::Forward { is_resolved: true })
            )
        );
    }

    #[test]
    fn test_init_expr() {
        fn nab_init_len(stmt: &Stmt) -> Option<usize> {
            if let StmtKind::VarDecl {
                value: Some(init_expr),
                ..
            } = &stmt.kind
            {
                if let ExprKind::Init { exprs, .. } = &init_expr.kind {
                    Some(exprs.len())
                } else {
                    None
                }
            } else {
                None
            }
        }

        // Size checking & compile-time checking is performed by the validator
        let mut parser = make_test_parser("var a : array 1 .. 3 of int := init(1, 2, 3)");
        assert!(parser.parse());
        assert_eq!(Some(3), nab_init_len(&parser.stmts()[0]));

        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1)");
        assert!(parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts()[0]));

        // Expect at least one expression
        let mut parser = make_test_parser("var a : array 1 .. * of int := init() begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts()[0]));

        // Expect closing paren
        let mut parser = make_test_parser("var a : array 1 .. * of int := init( begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts()[0]));

        // Expect starting paren
        let mut parser = make_test_parser("var a : array 1 .. * of int := init) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts()[0]));

        // Expect parens
        let mut parser = make_test_parser("var a : array 1 .. * of int := init begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts()[0]));

        // Expect expr after comma (length 2)
        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1,) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(2), nab_init_len(&parser.stmts()[0]));

        // Expect expr after comma (length 3)
        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1,,) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(3), nab_init_len(&parser.stmts()[0]));

        // Bad exprs should still contribute to length
        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1,+,+,4) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(4), nab_init_len(&parser.stmts()[0]));

        // Can only be used in initalization of const's & var's
        let mut parser = make_test_parser("var a : array 1 .. 3 of int\n a := init(1,2,3)");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var a : array 1 .. 3 of int\n a := +init(1,2,3)");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var a : array 1 .. 3 of int\n a := -init(1,2,3)");
        assert!(!parser.parse());

        let mut parser =
            make_test_parser("var a : array 1 .. 3 of int\n a := init(1,2,3)+init(1,2,3)");
        assert!(!parser.parse());

        // Arrays require init initializers
        let mut parser = make_test_parser("var a : array 1 .. * of int");
        assert!(!parser.parse());

        // Can't infer type from init
        let mut parser = make_test_parser("var a := init(1, 2, 3)");
        assert!(!parser.parse());

        // Can't infer type from init
        let mut parser = make_test_parser("const a := init(1, 2, 3)");
        assert!(!parser.parse());
    }

    #[test]
    fn test_indirect_expr() {
        // all valid types
        let mut parser = make_test_parser(
            "
            const c := 1 + 2 - 3
            type tyref : int
            var a00 := addressint @ (0)
            var a01 := char       @ (0)
            var a02 := char(c)    @ (0)
            var a03 := string     @ (0)
            var a04 := string(c)  @ (0)
            var a05 := boolean    @ (0)
            var a06 := int        @ (0)
            var a07 := int1       @ (0)
            var a08 := int2       @ (0)
            var a09 := int4       @ (0)
            var a10 := nat        @ (0)
            var a11 := nat1       @ (0)
            var a12 := nat2       @ (0)
            var a13 := nat4       @ (0)
            var a14 := real       @ (0)
            var a15 := real4      @ (0)
            var a16 := real8      @ (0)
            var a17 := tyref      @ (0)
            ",
        );
        assert_eq!(parser.parse(), true);

        // Allowed to be used in reference position
        let mut parser = make_test_parser("int @ (0) := 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("bambam @ (0) := 1");
        assert_eq!(parser.parse(), true);

        // Indirect expressions must be produced in error case
        let mut parser = make_test_parser(
            "
            var a00 := addressint
            var a01 := char      
            var a02 := char(c)   
            var a03 := string    
            var a04 := string(c) 
            var a05 := boolean   
            var a06 := int       
            var a07 := int1      
            var a08 := int2      
            var a09 := int4      
            var a10 := nat       
            var a11 := nat1      
            var a12 := nat2      
            var a13 := nat4      
            var a14 := real      
            var a15 := real4     
            var a16 := real8     
            ",
        );
        assert_eq!(parser.parse(), false);
        for stmt in parser.stmts() {
            if let StmtKind::VarDecl {
                value: Some(some_val),
                ..
            } = &stmt.kind
            {
                assert!(matches!(some_val.kind, ExprKind::Indirect { .. }));
            }
        }

        // Expect '(' after '@'
        let mut parser = make_test_parser("var a := int @ 0)");
        assert_eq!(parser.parse(), false);

        // Expect '(' after expr
        let mut parser = make_test_parser("var a := int @ 0");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("var a := int @ (0");
        assert_eq!(parser.parse(), false);

        // Address expression should be empty in these cases
        let mut parser = make_test_parser("var a := int @ ()");
        assert_eq!(parser.parse(), false);
        if let StmtKind::VarDecl {
            value: Some(indirect),
            ..
        } = &parser.stmts()[0].kind
        {
            if let ExprKind::Indirect { addr, .. } = &indirect.kind {
                assert!(matches!(addr.kind, ExprKind::Error), "Is {:?}", addr);
            }
        }

        let mut parser = make_test_parser("var a := int @ ");
        assert_eq!(parser.parse(), false);
        if let StmtKind::VarDecl {
            value: Some(indirect),
            ..
        } = &parser.stmts()[0].kind
        {
            if let ExprKind::Indirect { addr, .. } = &indirect.kind {
                assert!(matches!(addr.kind, ExprKind::Error), "Is {:?}", addr);
            }
        }

        // Should not be a bare empty
        let mut parser = make_test_parser("var a := int @ (+)");
        assert_eq!(parser.parse(), false);
        if let StmtKind::VarDecl {
            value: Some(indirect),
            ..
        } = &parser.stmts()[0].kind
        {
            if let ExprKind::Indirect { addr, .. } = &indirect.kind {
                assert!(!matches!(addr.kind, ExprKind::Error), "Is {:?}", addr);
            }
        }
    }

    #[test]
    fn test_call_stmt() {
        let mut parser = make_test_parser("var p : proc _\np");
        assert_eq!(parser.parse(), true);
        if let StmtKind::ProcedureCall {
            proc_ref: proc_call,
        } = &parser.stmts()[1].kind
        {
            assert!(
                matches!(proc_call.kind, ExprKind::Call { .. }),
                "Is {:#?}",
                proc_call
            );
        } else {
            unreachable!();
        }

        let mut parser = make_test_parser("var p : proc _\np()");
        assert_eq!(parser.parse(), true);
        if let StmtKind::ProcedureCall {
            proc_ref: proc_call,
        } = &parser.stmts()[1].kind
        {
            assert!(
                matches!(proc_call.kind, ExprKind::Call { .. }),
                "Is {:#?}",
                proc_call
            );
        } else {
            unreachable!();
        }

        let mut parser = make_test_parser("var p : fcn _ (__ : int) : int\np(1)");
        assert_eq!(parser.parse(), true);
        if let StmtKind::ProcedureCall {
            proc_ref: proc_call,
        } = &parser.stmts()[1].kind
        {
            assert!(
                matches!(proc_call.kind, ExprKind::Call { .. }),
                "Is {:#?}",
                proc_call
            );
        } else {
            unreachable!();
        }

        // TODO: Check for calls with reference behind dot, arrow, and deref exprs
    }
}
