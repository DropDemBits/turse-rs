//! Main parser for tokens to build the AST
// Parser fragments
mod expr;
mod stmt;
mod types;

use crate::context::CompileContext;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use toc_ast::ast::{BinaryOp, Expr, ExprKind, IdentId, Identifier, Stmt, UnaryOp};
use toc_ast::block::{BlockKind, CodeUnit};
use toc_ast::scope;
use toc_ast::types::{Type, TypeRef, TypeTable};
use toc_core::Location;

use std::cell::RefCell;
use std::fmt::Arguments;
use std::rc::Rc;

/// Maximum nesting depth during parsing
const MAX_NESTING_DEPTH: usize = 256;

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

    // Fragments of a code unit //
    /// If the parser is for the root unit
    is_main: bool,
    /// UnitScope handling identifier import semantics
    unit_scope: scope::UnitScope,
    /// Parsed statements
    stmts: Vec<Stmt>,
    /// TypeTable for types
    type_table: TypeTable,
}

#[derive(Debug)]
enum ParsingStatus {
    /// Error during parsing
    Error,
    /// Skipping tokens during parsing
    Skip,
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
            current: scanner
                .next()
                .unwrap_or_else(|| Token::new(TokenType::Eof, Location::new())),
            peek: scanner
                .next()
                .unwrap_or_else(|| Token::new(TokenType::Eof, Location::new())),
            scanner,
            // Clone a ref to the root block
            expr_nesting: 0,
            stmt_nesting: 0,
            type_nesting: 0,

            is_main,
            unit_scope: scope::UnitScope::new(),
            stmts: vec![],
            type_table: TypeTable::new(),
        }
    }

    /// Parses the token stream
    /// Returns if the parse has no errors
    pub fn parse(&mut self) -> bool {
        // TODO: Check if the root block is a unit block
        // Parse the statements
        while !self.is_at_end() {
            if let Ok(stmt) = self.decl() {
                self.stmts.push(stmt);
            }
        }

        !self.context.borrow().reporter.has_error()
    }

    /// Takes the unit from the parser
    /// The parser cannot be used after this point
    pub fn take_unit(self) -> CodeUnit {
        // Move fragements into the CodeUnit
        CodeUnit::new(self.is_main, self.stmts, self.unit_scope, self.type_table)
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
            .unwrap_or_else(|| Token::new(TokenType::Eof, Location::new()));
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
    fn expects(
        &mut self,
        expected_type: TokenType,
        message: Arguments,
    ) -> Result<Token<'s>, ParsingStatus> {
        if self.current().token_type == expected_type {
            Ok(self.next_token())
        } else {
            self.context
                .borrow_mut()
                .reporter
                .report_error(&self.current().location, message);
            Err(ParsingStatus::Error)
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

    fn warn_equ_as_assign(&self, at: Location) {
        self.context
            .borrow_mut()
            .reporter
            .report_warning(&at, format_args!("'=' found, assumed it to be ':='"));
    }

    /// Gets the corresponding lexeme for the given Token.
    ///
    /// If the given token is of `TokenType::Eof`, the given lexeme is `"<end of file>"`.
    fn get_token_lexeme(&self, token: &Token) -> &str {
        let token_content = token.location.get_lexeme(self.source);

        if !token_content.is_empty() {
            token_content
        } else {
            "<end of file>"
        }
    }

    /// Skips all tokens until a safe parsing point is reached.
    ///
    /// This should only be called in the event of a fatal error, as this will
    /// skip any valid expressions and statements caught up in the recovery
    /// process.
    fn skip_to_safe_point(&mut self) {
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
                | TokenType::Eof => break,
                _ => {
                    let _ = self.next_token();
                }
            }
        }
    }

    /// Declares an identifer in the current scope.
    ///
    /// Replaces any previous declaration of an identifier, if any exists.
    fn declare_ident(
        &mut self,
        ident: &Token,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
        is_pervasive: bool,
    ) -> IdentId {
        let decl_location = ident.location;
        let name = decl_location.get_lexeme(self.source).to_string();

        self.unit_scope.declare_ident(
            name,
            decl_location,
            type_spec,
            is_const,
            is_typedef,
            is_pervasive,
        )
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
fn try_into_binary(op: TokenType) -> Result<BinaryOp, ParsingStatus> {
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
        _ => Err(ParsingStatus::Error),
    }
}

/// Tries to convert the given token type into the corresponding unary operator
fn try_into_unary(op: TokenType) -> Result<UnaryOp, ParsingStatus> {
    match op {
        TokenType::Pound => Ok(UnaryOp::NatCheat),
        TokenType::Caret => Ok(UnaryOp::Deref),
        TokenType::Plus => Ok(UnaryOp::Identity),
        TokenType::Minus => Ok(UnaryOp::Negate),
        TokenType::Not => Ok(UnaryOp::Not),
        _ => Err(ParsingStatus::Error),
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
    use toc_ast::ast::{ExprKind, Stmt, StmtKind};
    use toc_ast::types::{self, *};

    fn make_test_parser(source: &str) -> Parser {
        let context = Rc::new(RefCell::new(CompileContext::new()));
        let scanner = Scanner::scan_source(source, context.clone());

        Parser::new(scanner, source, true, context)
    }

    // Get the latest version of the identifier
    fn get_ident(parser: &Parser, name: &str) -> Option<Identifier> {
        parser
            .get_ident(name)
            .map(|id| parser.get_ident_info(&id).to_owned())
    }

    // Gets the identifier with the specified instance
    fn get_ident_instance(parser: &Parser, id: IdentId) -> Option<Identifier> {
        Some(parser.get_ident_info(&id).to_owned())
    }

    fn get_ident_type(parser: &Parser, name: &str) -> TypeRef {
        get_ident(parser, name).expect("No identifier").type_spec
    }

    #[track_caller]
    fn check_ident_expected_type(parser: &Parser, name: &str, expected: TypeRef) {
        assert_eq!(get_ident_type(parser, name), expected);
    }

    fn is_ident_type_equivalent_to(parser: &Parser, lhs: &str, rhs: &str) -> bool {
        types::is_equivalent_to(
            &get_ident_type(&parser, lhs),
            &get_ident_type(&parser, rhs),
            &parser.type_table,
        )
    }

    #[test]
    fn test_opt_semicolon() {
        let mut parser = make_test_parser(";;;;;\nvar a : int := 1;\n;;;;;var b : int := 1;");
        assert!(parser.parse());
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
        for name in ["x", "y", "z"].iter() {
            check_ident_expected_type(&parser, name, TypeRef::Primitive(PrimitiveType::Real));
        }

        for name in ["d", "e", "f"].iter() {
            check_ident_expected_type(&parser, name, TypeRef::Primitive(PrimitiveType::String_));
        }

        for name in ["i", "j", "k"].iter() {
            check_ident_expected_type(&parser, name, TypeRef::Primitive(PrimitiveType::Nat));
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
        for name in ["a", "c", "e", "b", "k"].iter() {
            check_ident_expected_type(&parser, name, TypeRef::TypeError);
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
        for name in ["c", "d"].iter() {
            check_ident_expected_type(&parser, name, TypeRef::Primitive(PrimitiveType::Int));
        }

        // Invalid forms
        let mut parser = make_test_parser(
            "
        % Invalid forms - No type or value
        const a
        const b",
        );
        assert!(!parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::TypeError);
        check_ident_expected_type(&parser, "b", TypeRef::TypeError);

        let mut parser = make_test_parser(
            "
        % Invalid forms - No value
        const a : int
        const b : int",
        );
        assert!(!parser.parse());
    }

    #[test]
    fn test_simple_assignment() {
        let mut parser = make_test_parser(
            "
        % Setup
        var a : int
        
        % Valid forms
        a := 1
        a := 3 + 5 + 7
        a := #9 * 2 ** 3 and 5 xor 6

        % Accepted forms
        a = 2
        a = 194812
        a = -6
        ",
        );
        assert!(parser.parse());

        // Invaild: Dropped value
        let mut parser = make_test_parser(
            "
        % Setup
        var a : int
        a := 
        ",
        );
        assert!(!parser.parse());

        let mut parser = make_test_parser(
            "
        % Setup
        var a : int
        a = 
        ",
        );
        assert!(!parser.parse());
    }

    #[test]
    fn test_compound_assignment() {
        // Main operators
        let mut parser = make_test_parser(
            "
        % Setup
        var a : int
        var r : real
        
        % Valid forms
        a := 3
        a += 5
        a -= 7
        a *= 9
        a div= 11
        r /= 12.0
        a rem= 3
        a mod= 5
        a **= 2
        a and= 3
        a or= 5
        a xor= 6
        a shl= 9
        a shr= 12

        % Accepted forms
        a = 2
        ",
        );
        assert!(parser.parse());
        let expected_ops = [
            None,
            Some(BinaryOp::Add),
            Some(BinaryOp::Sub),
            Some(BinaryOp::Mul),
            Some(BinaryOp::Div),
            Some(BinaryOp::RealDiv),
            Some(BinaryOp::Rem),
            Some(BinaryOp::Mod),
            Some(BinaryOp::Exp),
            Some(BinaryOp::And),
            Some(BinaryOp::Or),
            Some(BinaryOp::Xor),
            Some(BinaryOp::Shl),
            Some(BinaryOp::Shr),
            None,
        ];

        let root_stmts = parser.stmts;

        for test_stmt in root_stmts[2..].iter().zip(expected_ops.iter()) {
            if let StmtKind::Assign { op, .. } = &test_stmt.0.kind {
                if op.ne(test_stmt.1) {
                    panic!(
                        "Mismatch between expected {:?} and parsed {:?}",
                        test_stmt.1, op
                    );
                }
            }
        }

        // Boolean operator versions
        let mut parser = make_test_parser(
            "
        % Setup
        var b : boolean
        
        % Valid forms
        b =>= true
        b and= false
        b or= true
        % xor= only valid for integers (int, nat, long, ulong) & sets
        ",
        );
        assert!(parser.parse());
        let expected_ops = [
            Some(BinaryOp::Imply),
            Some(BinaryOp::And),
            Some(BinaryOp::Or),
        ];

        let root_stmts = parser.stmts;

        for test_stmt in root_stmts[1..].iter().zip(expected_ops.iter()) {
            if let StmtKind::Assign { op, .. } = &test_stmt.0.kind {
                if op.ne(test_stmt.1) {
                    panic!(
                        "Mismatch between expected {:?} and parsed {:?}",
                        test_stmt.1, op
                    );
                }
            }
        }

        // The forbidden not expression is invalid in `toc`
        let mut parser = make_test_parser("var a : boolean := true\na ~==~ a");
        assert!(!parser.parse());
        let mut parser = make_test_parser("var a : boolean := true\na not==not a");
        assert!(!parser.parse());
    }

    #[test]
    fn test_equ_equ_hint_reporting() {
        // Using == instead of = for equality comparison

        // Should report
        let mut parser = make_test_parser("var q := a == true");
        assert!(!parser.parse());
    }

    #[test]
    fn test_primitive_type_parser() {
        let mut parser = make_test_parser(
            "
        var a : boolean
        var b : int
        var c : int1
        var d : int2
        var e : int4
        var f : nat
        var g : nat1
        var h : nat2
        var i : nat4
        var j : real
        var k : real4
        var l : real8
        var m : string
        var n : string(300)
        var o : char
        var p : char(768)
        var q : addressint
        ",
        );
        assert!(parser.parse());

        let expected_types = [
            TypeRef::Primitive(PrimitiveType::Boolean),
            TypeRef::Primitive(PrimitiveType::Int),
            TypeRef::Primitive(PrimitiveType::Int1),
            TypeRef::Primitive(PrimitiveType::Int2),
            TypeRef::Primitive(PrimitiveType::Int4),
            TypeRef::Primitive(PrimitiveType::Nat),
            TypeRef::Primitive(PrimitiveType::Nat1),
            TypeRef::Primitive(PrimitiveType::Nat2),
            TypeRef::Primitive(PrimitiveType::Nat4),
            TypeRef::Primitive(PrimitiveType::Real),
            TypeRef::Primitive(PrimitiveType::Real4),
            TypeRef::Primitive(PrimitiveType::Real8),
            TypeRef::Primitive(PrimitiveType::String_),
            TypeRef::Primitive(PrimitiveType::StringN(SequenceSize::Size(300))),
            TypeRef::Primitive(PrimitiveType::Char),
            TypeRef::Primitive(PrimitiveType::CharN(SequenceSize::Size(768))),
            TypeRef::Primitive(PrimitiveType::AddressInt),
        ];

        let root_stmts = &parser.stmts;

        for test_stmt in root_stmts.iter().zip(expected_types.iter()) {
            if let StmtKind::VarDecl { idents, .. } = &test_stmt.0.kind {
                let ident = parser.get_ident_info(&idents[0].id);
                assert_eq!(&ident.type_spec, test_stmt.1);
            }
        }

        // Star lengths in subprogram parameters
        let mut parser = make_test_parser(
            "
        var a : proc _ (a : string(*))
        var b : proc _ (b : char(*))
        ",
        );
        assert!(parser.parse());

        // Expressions are allowed for string(n) and char(n), resolved at validator time
        // They don't parse into to the base type
        let mut parser = make_test_parser("var c : string(1 + 1 + 1 - 2 + 4 * 8 div 2)");
        assert!(parser.parse());
        assert_ne!(
            get_ident_type(&parser, "c"),
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let mut parser =
            make_test_parser("const c := 1 + 1 + 1 - 2 + 4 * 8 div 2\nvar d : string(c)");
        assert!(parser.parse());
        assert_ne!(
            get_ident_type(&parser, "d"),
            TypeRef::Primitive(PrimitiveType::String_)
        );

        let mut parser =
            make_test_parser("const c := 1 + 1 + 1 - 2 + 4 * 8 div 2\nvar d : char(c + 4)");
        assert!(parser.parse());
        assert_ne!(
            get_ident_type(&parser, "d"),
            TypeRef::Primitive(PrimitiveType::Char)
        );

        // Wrong types will be captured by the validator

        // Invalid: Bigger than the maximum size
        let mut parser = make_test_parser("var c : string(16#10000)");
        assert!(!parser.parse());
        // Tried to parse as a "string"
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::String_));

        let mut parser = make_test_parser("var c : string(16#10001)");
        assert!(!parser.parse());
        // Tried to parse as a "string"
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::String_));

        // Invalid: Zero length size expression
        let mut parser = make_test_parser("var c : char(16#0)");
        assert!(!parser.parse());
        // Tried to parse as a "char"
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::Char));

        let mut parser = make_test_parser("var c : string(16#0)");
        assert!(!parser.parse());
        // Tried to parse as a "string"
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::String_));

        // Invalid: Dropping the right paren
        let mut parser = make_test_parser("var c : char(16#0");
        assert!(!parser.parse());
        // Tried to parse as a "char"
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::Char));

        // Invalid: No length specification
        let mut parser = make_test_parser("var c : string(");
        assert!(!parser.parse());
        // Tried to parse as a "string"
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::String_));

        // Invalid: '*' specifiec is only valid in subprogram parameter declarations
        let mut parser = make_test_parser("var c : string(*)");
        assert!(!parser.parse());
        // Failed to parse, as string
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::String_));

        let mut parser = make_test_parser("var c : char(*)");
        assert!(!parser.parse());
        // Failed to parse, as char
        check_ident_expected_type(&parser, "c", TypeRef::Primitive(PrimitiveType::Char));

        // Invalid: Not a type specification (shouldn't parse the := "hee" nor the 'to' as it may cause
        // phantom errors)
        let mut parser = make_test_parser("var c : to := 'hee'");
        assert!(!parser.parse());
        // Failed to parse, as type error
        check_ident_expected_type(&parser, "c", TypeRef::TypeError);
    }

    #[test]
    fn test_compound_type_parser() {
        // Undeclared type identifiers don't produce an error until the validator stage
        let mut parser = make_test_parser(
            "
var a : pointer to int
var a_alt : unchecked pointer to int
var b : ^ string
var c : some_type
var d : procedure nps
var e : procedure np   ()
var f : procedure p1   (a : int)
var g : procedure p2   (a : int, b : string)
var h : procedure pisp (a : int, b : string, c : procedure _ ())
var j : function np   () : real
var k : function p1   (a : int) : string
var l : function p2   (a : int, b : string) : addressint
var m : function pisp (a : int, b : string, c : procedure _ ()) : boolean

% Pairs are to be equivalent
var n : function _ (a, b : int, c : real) : int
var o : function _ (a : int, b : int, c : real) : int
var p : function _ (var a, b : int, c : string) : int
var q : function _ (var a : int, var b : int, c : string) : int

% Other variations
var r : function _ (var a : cheat int, var register b : cheat int, proc c) : int
% Nesting fun!
% While not valid in TProlog, it should still be valid syntax as inner parameter names are ignored
var s : function _ (function a (function a : int ) : int, proc b (proc a (proc a( proc a))), proc c) : int

% Range parsing
var a_range : (1 - 3 shl 5) .. (2 * 50 - 8 * 4)

% Set parsing (only valid in type statements)
type some_set : set of 1 .. 5
type some_set_c : set of char
type some_set_b : set of boolean

% Array parsing setup
var start_range := 1
var end_range := 5

% Array parsing (enum ranges aren't parsed yet, but are equivalent to identifiers)
var t : array 1 .. 2 of int
% Multiple ranges
var u : array 1 .. 2, (-1 - 20) .. (2 + 3), (1 + 8) .. (2 + 16) of string
% Char ranges
var v : array 'a' .. 'f' of real
var w : array char of nat
% Boolean ranges
var x : array false .. true of char
var y : array boolean of boolean
% Other ranges
var z : array start_range .. end_range of real
var implicit_size : array 1 .. * of real := init (1, 2, 3, 4, 5)
var flexi : flexible array 1 .. 0 of real

var up_size := 5
var runtime_size : array 1 .. up_size of real

% Identifier reference (resolved at validation time)
var some_external_use : some.thing.with.these.given.fields := 3
var ranged_external : some.thing.with.start .. some.thing.with.end_thing := 5
var implicit_external : array 1 .. some.thing.with.end_thing of int

% Enum types
type enumeration : enum (a, b, c, d, e, f)
        ",
        );
        assert!(parser.parse());
        assert!(is_ident_type_equivalent_to(&parser, "n", "o"));
        assert!(is_ident_type_equivalent_to(&parser, "p", "q"));

        // Arbitrary expressions are not valid types
        let mut parser = make_test_parser("var a : 1");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var a : 1 ** 2");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var a : (1 * 6 - 1 + 4 = 1)");
        assert!(!parser.parse());

        let mut parser = make_test_parser("var a : false");
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
        assert_eq!(get_ident(&parser, "a").unwrap().is_typedef, false);

        // Set type declarations expect 'of'
        let mut parser = make_test_parser("type a : set 1 .. 3");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().is_typedef, true);

        let mut parser = make_test_parser("type a : set");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().is_typedef, true);

        // Set type declarations expect a range
        let mut parser = make_test_parser("type a : set of ");
        assert!(!parser.parse());
        assert_eq!(get_ident(&parser, "a").unwrap().is_declared, true);
        assert_eq!(get_ident(&parser, "a").unwrap().is_typedef, true);
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
        if let Some(Type::Array { ranges, .. }) = parser
            .type_table
            .type_from_ref(&get_ident_type(&parser, "inv"))
        {
            assert_eq!(ranges.len(), 1);
        } else {
            panic!("Not an array");
        }

        let mut parser = make_test_parser("var inv : array 1 .. *, 1 .. *, char of real");
        assert!(!parser.parse());
        if let Some(Type::Array { ranges, .. }) = parser
            .type_table
            .type_from_ref(&get_ident_type(&parser, "inv"))
        {
            assert_eq!(ranges.len(), 1);
        } else {
            panic!("Not an array");
        }

        // Implicit size range is only allowed for the first range specifier
        let mut parser = make_test_parser("var inv : array 1 .. 2, 1 .. *, char of real");
        assert!(!parser.parse());
        if let Some(Type::Array { ranges, .. }) = parser
            .type_table
            .type_from_ref(&get_ident_type(&parser, "inv"))
        {
            assert_eq!(ranges.len(), 3);
        } else {
            panic!("Not an array");
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
        assert_eq!(
            get_ident_type(&parser, "b"),
            TypeRef::Primitive(PrimitiveType::Int)
        );

        let mut parser = make_test_parser("type a : enum )\nvar b : int");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);
        // Ensure this gets parsed
        assert_eq!(
            get_ident_type(&parser, "b"),
            TypeRef::Primitive(PrimitiveType::Int)
        );

        let mut parser = make_test_parser("type a : enum \nvar b : int");
        assert!(!parser.parse());
        assert_ne!(get_ident_type(&parser, "a"), TypeRef::TypeError);
        // Ensure this gets parsed
        assert_eq!(
            get_ident_type(&parser, "b"),
            TypeRef::Primitive(PrimitiveType::Int)
        );

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
        assert_eq!(
            get_ident_type(&parser, "b"),
            TypeRef::Primitive(PrimitiveType::Int)
        );

        // Enums not in top-level type contexts are rejected
        // (i.e. anonymous enums are not allowed), but still produce
        // an enum type
        let mut parser = make_test_parser("var a : enum (a, b, c)");
        assert!(!parser.parse());
        let type_ref = get_ident_type(&parser, "a");
        let type_of = parser.type_table.type_from_ref(&type_ref);
        assert!(
            matches!(type_of, Some(Type::Enum { .. })),
            "Is of type {:?} from {:?}",
            type_of,
            type_ref
        );

        let mut parser = make_test_parser("const a : enum (a, b, c) := 2");
        assert!(!parser.parse());
        let type_ref = get_ident_type(&parser, "a");
        let type_of = parser.type_table.type_from_ref(&type_ref);
        assert!(
            matches!(type_of, Some(Type::Enum { .. })),
            "Is of type {:?} from {:?}",
            type_of,
            type_ref
        );

        let mut parser = make_test_parser("type a : set of enum (a, b, c)");
        assert!(!parser.parse());
    }

    #[test]
    fn test_block_stmt() {
        let mut parser = make_test_parser(
            "
        % Local declarations & importation
        begin
            var hey := 2
            begin
                var yay : real := 5 + hey
            end
            begin
                % Different scope!
                var yay : real := 5 + hey
            end
            var yay : int := 6 - hey
        end
        var yay : string := \"hello!\"
        ",
        );
        assert!(parser.parse());

        // Missing end
        let mut parser = make_test_parser(
            "
        begin
            var yay : int := 5
        var yay : string := \"hello!\"
        ",
        );
        assert!(!parser.parse());

        // Redeclaration of declared - global - inner
        let mut parser = make_test_parser(
            "
        var yay : string := \"hello!\"
        begin
            var yay : int := 5
        end
        ",
        );
        assert!(parser.parse()); // Checked at validator time

        // Validate the types
        if let StmtKind::Block { block, .. } = &parser.stmts[1].kind {
            // Inner scope is still int
            let id = block.get_ident_id("yay").expect("Ident not declared");

            assert_eq!(
                parser.unit_scope.get_ident_info(&id).type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        } else {
            unreachable!();
        }

        // Outer scope is still string
        assert_eq!(
            get_ident(&parser, "yay").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );

        // Redeclaration of declared - inner - inner
        let mut parser = make_test_parser(
            "
        begin
            var yay : int := 5
            begin
                var yay : int := 5
            end
        end
        var yay : string := \"hello!\"
        ",
        );
        assert!(parser.parse()); // Checked at validator time

        // Validate the types
        if let StmtKind::Block { block, .. } = &parser.stmts[0].kind {
            // Innermost scope is still int
            let id = block.get_ident_id("yay").expect("Ident not declared");

            assert_eq!(
                parser.unit_scope.get_ident_info(&id).type_spec,
                TypeRef::Primitive(PrimitiveType::Int)
            );
        } else {
            unreachable!();
        }

        // Outermost scope is still string
        assert_eq!(
            get_ident(&parser, "yay").unwrap().type_spec,
            TypeRef::Primitive(PrimitiveType::String_)
        );
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
        assert_eq!(parser.stmts.len(), 1);

        // Requires colon, will parse the rest and produce a declaration
        let mut parser = make_test_parser("var a : string\ntype a");
        assert!(!parser.parse());
        assert!(get_ident(&parser, "a").is_some());
        let type_ref = get_ident(&parser, "a").unwrap().type_spec;
        assert!(
            matches!(
                parser.type_table.type_from_ref(&type_ref),
                Some(Type::Alias {
                    to: TypeRef::TypeError,
                })
            ),
            "From ref {:?} to {:?}",
            type_ref,
            parser.type_table.type_from_ref(&type_ref)
        );
        assert_eq!(get_ident(&parser, "a").unwrap().is_typedef, true);

        let mut parser = make_test_parser("var a : string\ntype a int");
        assert!(!parser.parse());
        assert!(get_ident(&parser, "a").is_some());
        assert!(get_ident(&parser, "a").unwrap().is_typedef);
        assert_eq!(
            true,
            matches!(
                parser
                    .type_table
                    .type_from_ref(&get_ident(&parser, "a").unwrap().type_spec),
                Some(Type::Alias {
                    to: TypeRef::Primitive(PrimitiveType::Int),
                })
            )
        );
        assert_eq!(get_ident(&parser, "a").unwrap().is_typedef, true);

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
        assert_eq!(
            true,
            matches!(
                parser
                    .type_table
                    .type_from_ref(&get_ident_instance(&parser, IdentId(1)).unwrap().type_spec),
                Some(Type::Forward { is_resolved: false })
            )
        );

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
        assert_eq!(Some(3), nab_init_len(&parser.stmts[0]));

        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1)");
        assert!(parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts[0]));

        // Expect at least one expression
        let mut parser = make_test_parser("var a : array 1 .. * of int := init() begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts[0]));

        // Expect closing paren
        let mut parser = make_test_parser("var a : array 1 .. * of int := init( begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts[0]));

        // Expect starting paren
        let mut parser = make_test_parser("var a : array 1 .. * of int := init) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts[0]));

        // Expect parens
        let mut parser = make_test_parser("var a : array 1 .. * of int := init begin end");
        assert!(!parser.parse());
        assert_eq!(Some(1), nab_init_len(&parser.stmts[0]));

        // Expect expr after comma (length 2)
        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1,) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(2), nab_init_len(&parser.stmts[0]));

        // Expect expr after comma (length 3)
        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1,,) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(3), nab_init_len(&parser.stmts[0]));

        // Bad exprs should still contribute to length
        let mut parser = make_test_parser("var a : array 1 .. * of int := init(1,+,+,4) begin end");
        assert!(!parser.parse());
        assert_eq!(Some(4), nab_init_len(&parser.stmts[0]));

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
        assert_eq!(TypeRef::TypeError, get_ident_type(&parser, "a"));

        // Can't infer type from init
        let mut parser = make_test_parser("const a := init(1, 2, 3)");
        assert!(!parser.parse());
        assert_eq!(TypeRef::TypeError, get_ident_type(&parser, "a"));
    }

    #[test]
    fn test_infix_operators() {
        // Test all operators in infix positions
        // Should not crash

        // Types don't matter here, as that's checked in validator
        let mut parser = make_test_parser("const a := 1 + 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 - 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 * 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 div 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 shl 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 shr 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 and 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 or 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 & 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 | 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 xor 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 in 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 not in 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 ~ in 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 ~in 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 < 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 <= 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 > 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 >= 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 = 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 ~= 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 ~ = 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 not = 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 not= 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const a := 1 => 1");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const ba := 2\nconst a := ba.a");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const ba := 2\nconst a := ba->a");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const ba := 2\nconst a := ba()");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const ba := 2\nconst a := ba(1, 2, 3)");
        assert_eq!(parser.parse(), true);

        // Only prefix
        let mut parser = make_test_parser("const a := 1 ~ ");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 not ");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 # ");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 1.0 ");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 1 ");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 \"keke\"");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 'keke'");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 true");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 false");
        assert_eq!(parser.parse(), false);
        let mut parser = make_test_parser("const a := 1 nil");
        assert_eq!(parser.parse(), false);

        // Identifiers and ^ are okay as they are interpreted as a new statement
        let mut parser = make_test_parser("const ba := 2\nconst a := 1 ba");
        assert_eq!(parser.parse(), true);
        let mut parser = make_test_parser("const ba := 2\nconst a := 1 ^ba");
        assert_eq!(parser.parse(), true);
    }

    #[test]
    fn test_nesting_limit() {
        // Should not panic

        // Expr limit, unary
        let mut parser = make_test_parser("var k := ####################################################################################################################################################################################################################################################################################1");
        assert_eq!(parser.parse(), false);

        // Expr limit, binary
        let mut parser = make_test_parser("var k := 1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))");
        assert_eq!(parser.parse(), false);

        // Stmt limit
        let mut parser = make_test_parser("begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin begin ");
        assert_eq!(parser.parse(), false);

        // Type limit
        let mut parser = make_test_parser("type k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : proc a (k : int))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))");
        assert_eq!(parser.parse(), false);
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
        for stmt in parser.stmts {
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
        } = &parser.stmts[0].kind
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
        } = &parser.stmts[0].kind
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
        } = &parser.stmts[0].kind
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
        } = &parser.stmts[1].kind
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
        } = &parser.stmts[1].kind
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
        } = &parser.stmts[1].kind
        {
            assert!(
                matches!(proc_call.kind, ExprKind::Call { .. }),
                "Is {:#?}",
                proc_call
            );
        } else {
            unreachable!();
        }

        // TODO: Check for calls behind dot, arrow, and deref exprs
    }
}
