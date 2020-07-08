//! Main parser for tokens to build the AST
use crate::compiler::ast::{ASTVisitor, Expr, Identifier, Stmt};
use crate::compiler::block::{BlockKind, CodeBlock};
use crate::compiler::token::{Token, TokenType};
use crate::compiler::types::{self, PrimitiveType, TypeRef};
use crate::compiler::Location;
use crate::status_reporter::StatusReporter;
use std::cell::{Cell, RefCell};
use std::fmt::Arguments;
use std::rc::Rc;

/*
expr:
    primary
    funCall | setCons
    unary
    binary

primary:
    literal
    reference | enumValue

param_list:
    expr (',' expr)*

literal:
    IntLiteral
    RealLiteral
    StringLiteral
    CharLiteral

selector:
    '(' arg_list ')'  // arrays (params: Expr::List)
    '.' reference		// fields (Expr::BinaryOp, op: Dot)

reference:
    identifier
    array_reference
    property_reference


array_reference:
    reference '(' arg_list ')' // Call on "op_subscript" (Expr::Call { Expr, List })

property_reference:
    reference '.' identifier // (Expr::Get {Expr, Identifier} )

binary:
    expr binOp expr

unary:
    unOp expr

funCall:
    reference '(' arg_list? ')' // (Expr::Call {Expr, List})

setCons:
    reference '(' arg_list? ')' // Call on "set_cons" (Expr::Call { Expr, List })

enumValue:
    reference '.' identifier // Documented encoding
    reference 				 // Real encoding

*/

/// Operator precedence
/// All operators have left associativity, with the exceptions of ^ and #
#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
enum Precedence {
    /// No precedence for this level
    NoPrec,
    /// Imply Operator \ =>
    Imply,
    /// Or Operator \ or/'|'
    BitOr,
    /// And Operator \ and/'&'
    BitAnd,
    /// Not Operator \ not/'~'
    BitNot,
    /// Comparator Operators \ < > = <= >= not=/'~=' in not_in/'~in'
    Comparison,
    /// Additive Operators \ + - xor
    Sum,
    /// Product Operators \ * / div mod rem shl shr
    Product,
    /// Unary Operators \ + -
    Unary,
    /// Exponent Operator \ **
    Exponent,
    /// Conversion Operator \ #
    Conversion,
    /// Deref Operator \ ->
    Deref,
    /// Calling & Get Operators \ ( .
    Call,
    /// Pointer Follow Operator \ ^
    Follow,
    /// Primaries
    Primary,
}

impl Precedence {
    /// Gets the next precedence up
    pub fn up(&self) -> Self {
        use Precedence::*;

        match self {
            // Lowest & highest saturate
            NoPrec => NoPrec,
            Primary => Primary,

            Imply => BitOr,
            BitOr => BitAnd,
            BitAnd => BitNot,
            BitNot => Comparison,
            Comparison => Sum,
            Sum => Product,
            Product => Unary,
            Unary => Exponent,
            Exponent => Conversion,
            Conversion => Deref,
            Deref => Call,
            Call => Follow,
            Follow => Primary,
        }
    }
}

struct PrecedenceRule<'a> {
    precedence: Precedence,
    prefix_rule: Option<fn(&Parser<'a>) -> Result<Expr, ParsingStatus>>,
    infix_rule: Option<fn(&Parser<'a>, Expr) -> Result<Expr, ParsingStatus>>,
}

/// Main parser
#[derive(Debug)]
pub struct Parser<'a> {
    /// Status reporter
    reporter: StatusReporter,
    /// File source used for getting lexemes for reporting
    source: &'a str,
    /// Source for tokens
    tokens: Vec<Token>,
    /// Parsed main block statements
    stmts: Vec<Stmt>,
    /// Current code block list
    // Enclosing in a RefCell because mutability loops
    blocks: RefCell<Vec<Rc<RefCell<CodeBlock>>>>,
    /// Current token being parsed
    current: Cell<usize>,
}

#[derive(Debug)]
enum ParsingStatus {
    Error,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self {
            reporter: StatusReporter::new(),
            source,
            tokens,
            stmts: vec![],
            blocks: RefCell::new(vec![Rc::new(RefCell::new(CodeBlock::new(
                BlockKind::Main,
                &vec![],
            )))]),
            current: Cell::new(0),
        }
    }

    /// Parses the token stream
    /// Returns if the parse has no errors
    pub fn parse(&mut self) -> bool {
        // TODO: Check if the root block is a unit block

        while !self.is_at_end() {
            match self.decl() {
                Ok(expr) => self.stmts.push(expr),
                Err(_) => {}
            }

            if self.current().token_type == TokenType::Semicolon {
                // Nom the semicolon
                self.next_token();
            }
        }

        !self.reporter.has_error()
    }

    /// Visits the AST using the given ASTVisitor
    /// Allows mutable access to the AST
    pub fn visit_ast<T>(&mut self, visitor: &mut T)
    where
        T: ASTVisitor,
    {
        for stmt in self.stmts.iter_mut() {
            visitor.visit_stmt(stmt);
        }
    }

    /// Gets the previous token in the stream
    fn previous(&self) -> &Token {
        &self.tokens[self.current.get().saturating_sub(1)]
    }

    /// Gets the current token in the stream
    fn current(&self) -> &Token {
        &self.tokens[self.current.get()]
    }

    /// Peeks at the next token in the stream
    fn peek(&self) -> &Token {
        &self.tokens[self.current.get().saturating_add(1)]
    }

    /// Advances to the next token, returning the previous token
    fn next_token(&self) -> &Token {
        if !self.is_at_end() {
            // Advance cursor
            self.current.set(self.current.get().saturating_add(1));
        }

        self.previous()
    }

    /// Checks if all of the tokens have been consumed yet
    fn is_at_end(&self) -> bool {
        self.current().token_type == TokenType::Eof
    }

    /// Consumes the expected token, or returns a message indicating the error
    fn expects(
        &self,
        expected_type: TokenType,
        message: Arguments,
    ) -> Result<&Token, ParsingStatus> {
        if self.current().token_type == expected_type {
            Ok(self.next_token())
        } else {
            self.reporter
                .report_error(&self.current().location, message);
            Err(ParsingStatus::Error)
        }
    }

    fn warn_equ_as_assign(&self, at: Location) {
        self.reporter
            .report_warning(&at, format_args!("'=' found, assumed it to be ':='"));
    }

    // --- Decl Parsing --- //

    fn decl(&self) -> Result<Stmt, ParsingStatus> {
        let nom = self.current();
        match nom.token_type {
            TokenType::Var => self.decl_var(false),
            TokenType::Const => self.decl_var(true),
            _ => self.stmt(),
        }
    }

    fn decl_var(&self, is_const: bool) -> Result<Stmt, ParsingStatus> {
        let decl_name = if is_const { "const" } else { "var" };

        // Consume decl_tok
        let decl_tok = self.next_token();

        // TODO: parse definition of multiple identifiers
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
            self.parse_type()
        } else {
            // Will be resolved in the type analysis stage
            TypeRef::Unknown
        };

        // Grab assign value
        let assign_expr = if self.is_simple_assignment() {
            if &self.current().token_type == &TokenType::Equ {
                // Warn of mistake
                self.warn_equ_as_assign(self.current().location);
            }

            // Consume assign
            self.next_token();

            // Get the assign expression
            let asn_expr = self.expr();

            asn_expr
                .map(|expr| Some(Box::new(expr)))
                .unwrap_or_else(|_| None)
        } else {
            None
        };

        // Validate if the declaration requirements have been met
        // Otherwise, produce an error and a TypeError
        if is_const && assign_expr.is_none() {
            // const declares require the assignment expression
            // Recoverable error, just use TypeError as the type_spec
            self.reporter.report_error(
                &decl_tok.location,
                format_args!("const declaration requires an initial value"),
            );

            type_spec = TypeRef::TypeError;
        } else if type_spec == TypeRef::Unknown && assign_expr.is_none() {
            // No type inferrable
            // Recoverable error, just use TypeError as the type_spec
            self.reporter.report_error(
                &decl_tok.location,
                format_args!("Cannot infer type for given {} declaration (no type specification or initial value given)", decl_name)
            );

            type_spec = TypeRef::TypeError;
        }

        // Declare the identifiers
        let idents: Vec<Identifier> = ident_tokens
            .iter()
            .map(|token| self.declare_ident(token, type_spec, is_const, false))
            .collect();

        Ok(Stmt::VarDecl {
            idents,
            type_spec,
            value: assign_expr,
            is_const,
        })
    }

    // --- Stmt Parsing --- //

    fn stmt(&self) -> Result<Stmt, ParsingStatus> {
        let nom = self.current();
        match nom.token_type {
            TokenType::Identifier | TokenType::Caret => self.stmt_reference(),
            TokenType::Begin => self.stmt_block(),
            _ => {
                // Nom as token isn't consumed by anything else
                self.next_token();

                self.reporter.report_error(
                    &nom.location,
                    format_args!(
                        "'{}' does not begin a statement or declaration",
                        nom.location.get_lexeme(self.source)
                    ),
                );

                // Cause an error
                Err(ParsingStatus::Error)
            }
        }
    }

    fn stmt_reference(&self) -> Result<Stmt, ParsingStatus> {
        // Identifiers & References can begin either an assignment or a procedure call
        // Both take references as the primary expression

        // If the reference expr can't be parsed, bail out
        // Referring to the an identifier (aside from checking if it's been declared)
        // isn't important in type resolution
        let reference = self.expr_precedence(Precedence::Deref)?;
        let is_compound_assign = self.is_compound_assignment();

        if is_compound_assign || self.is_simple_assignment() {
            // Is a (compound) assignment or '='
            // '=' is checked for as it's a common mistake to have '=' instead of ':='
            let mut assign_op = self.next_token().clone();

            if is_compound_assign {
                // Nom the other equ in the compound assignment
                self.next_token();
            } else if assign_op.token_type != TokenType::Assign {
                // Current assignment op is '=', not ':='
                // Warn of mistake, convert into ':='
                let locate = self.previous().location;
                self.warn_equ_as_assign(locate);

                assign_op.token_type = TokenType::Assign;
            };

            // If the assign value expr can't be parsed, bail out
            // Assignment after variable declaration isn't really important
            // for type resolution
            let value = self.expr()?;

            Ok(Stmt::Assign {
                var_ref: Box::new(reference),
                op: assign_op,
                value: Box::new(value),
            })
        } else {
            // Is a procedure call
            Ok(Stmt::ProcedureCall {
                proc_ref: Box::new(reference),
            })
        }
    }

    fn stmt_block(&self) -> Result<Stmt, ParsingStatus> {
        // Nom begin
        let begin_tok = self.next_token();

        self.push_block(BlockKind::InnerBlock);

        let mut stmts = vec![];
        while !matches!(self.current().token_type, TokenType::End | TokenType::Eof) {
            let stmt = self.decl();

            // Only add the Stmt if it was parsed successfully
            if stmt.is_ok() {
                stmts.push(stmt.unwrap());
            }
        }

        if matches!(self.current().token_type, TokenType::Eof) {
            // If at the end of file, do nothing
            // All of the statements have been absolved into this block

            self.reporter.report_error(
                &begin_tok.location,
                format_args!("'begin' block does not have a matching 'end'"),
            );
        } else {
            let _ = self.expects(
                TokenType::End,
                format_args!("Expected 'end' to close off 'begin' block"),
            );
        }

        // Close the block
        let block = self.pop_block(&mut stmts);

        Ok(Stmt::Block { block })
    }

    // --- Expr Parsing --- //

    fn expr(&self) -> Result<Expr, ParsingStatus> {
        self.expr_precedence(Precedence::Imply)
    }

    fn expr_precedence(&self, min_precedence: Precedence) -> Result<Expr, ParsingStatus> {
        // Keep track of last token
        let before_op = self.previous();

        // Get prefix side
        let op = self.next_token();

        let prefix = self.get_rule(&op.token_type);
        let prefix_rule = prefix.prefix_rule.ok_or_else(|| {
            // Try to figure out if the typo was a reasonable one
            // ???: Rework this system to use a hashmap with key (token_type, token_type)?

            let hint = if before_op.location != op.location {
                if before_op.token_type == TokenType::Equ && op.token_type == TokenType::Equ {
                    "(Did you mean '=' instead of '=='?)"
                } else {
                    ""
                }
            } else {
                // No hints for looking back at the start or end of the file
                ""
            };

            self.reporter.report_error(
                &self.previous().location,
                format_args!(
                    "Expected expression before '{}' {}",
                    self.previous().location.get_lexeme(self.source),
                    hint
                ),
            );

            ParsingStatus::Error
        })?;

        let mut expr = prefix_rule(self)?;

        // Go over infix operators
        while !self.is_at_end()
            && min_precedence <= self.get_rule(&self.current().token_type).precedence
        {
            let op = self.current();
            let infix = self.get_rule(&op.token_type);

            if infix.precedence >= Precedence::Follow {
                // Is  a deref, identifier, or literal
                // Most likely end of expression, so return
                return Ok(expr);
            }

            let infix_rule = infix
                .infix_rule
                .expect(&format!("No infix function for given rule '{:?}'", op).to_string());

            // Consume token for infix rule
            self.next_token();

            // Produce the next expression
            expr = infix_rule(self, expr)?;
        }

        // Give back parsed expression
        Ok(expr)
    }

    fn expr_grouping(&self) -> Result<Expr, ParsingStatus> {
        let expr = self.expr()?;
        self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' to close off parenthetical grouping"),
        )?;

        Ok(Expr::Grouping {
            expr: Box::new(expr),
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_binary(&self, lhs: Expr) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let rule = self.get_rule(&op.token_type);
        // Get rhs
        let rhs = self.expr_precedence(rule.precedence.up())?;

        Ok(Expr::BinaryOp {
            left: Box::new(lhs),
            op: op.clone(),
            right: Box::new(rhs),
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_unary(&self) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let right = self.expr_precedence(Precedence::Unary)?;

        Ok(Expr::UnaryOp {
            op: op.clone(),
            right: Box::new(right),
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_unary_rule(&self) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let rule = self.get_rule(&op.token_type);
        let right = self.expr_precedence(rule.precedence)?;

        Ok(Expr::UnaryOp {
            op: op.clone(),
            right: Box::new(right),
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_call(&self, func_ref: Expr) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let arg_list = self.make_arg_list()?.unwrap();

        Ok(Expr::Call {
            left: Box::new(func_ref),
            op: op.clone(),
            arg_list,
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_dot(&self, var_ref: Expr) -> Result<Expr, ParsingStatus> {
        // Get the ident
        let ident = self.expects(
            TokenType::Identifier,
            format_args!("Missing identifier after '.'"),
        )?;

        // The actual identifier information will be resolved at type resolution time,
        // so we can just store the field name and location info

        Ok(Expr::Dot {
            left: Box::new(var_ref),
            field: (
                ident.clone(),
                ident.location.get_lexeme(self.source).to_string(),
            ),
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_deref(&self, var_ref: Expr) -> Result<Expr, ParsingStatus> {
        let op = self.previous();

        // Wrap the var_ref in a deref
        self.expr_dot(Expr::UnaryOp {
            op: Token {
                token_type: TokenType::Caret,
                location: op.location.clone(),
            },
            right: Box::new(var_ref),
            eval_type: TypeRef::Unknown,
        })
    }

    fn expr_primary(&self) -> Result<Expr, ParsingStatus> {
        let token = self.previous();

        match &token.token_type {
            TokenType::StringLiteral(s) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: TypeRef::Primitive(types::get_string_kind(&s)),
            }),
            TokenType::CharLiteral(s) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: TypeRef::Primitive(types::get_char_kind(&s)),
            }),
            TokenType::IntLiteral(_) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: TypeRef::Primitive(PrimitiveType::Int),
            }),
            TokenType::RealLiteral(_) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: TypeRef::Primitive(PrimitiveType::Real),
            }),
            TokenType::True => Ok(Expr::Literal {
                value: Token {
                    token_type: TokenType::BoolLiteral(true),
                    location: token.location.clone(),
                },
                eval_type: TypeRef::Primitive(PrimitiveType::Boolean),
            }),
            TokenType::False => Ok(Expr::Literal {
                value: Token {
                    token_type: TokenType::BoolLiteral(false),
                    location: token.location.clone(),
                },
                eval_type: TypeRef::Primitive(PrimitiveType::Boolean),
            }),
            TokenType::Nil => {
                // Consume optional collection / class id

                // TODO: validate that theses are the same as the pointer type (ie produce nil for a given type id)
                // For classes, both must have a common ancestor
                if &self.current().token_type == &TokenType::LeftParen {
                    // Consume optional identifier & parens
                    self.next_token(); // (

                    if &self.current().token_type == &TokenType::Identifier {
                        self.next_token(); // identifier
                    }

                    self.expects(
                        TokenType::RightParen,
                        format_args!("Expected ')' to close off parentheses for 'nil'"),
                    )?;
                }

                Ok(Expr::Literal {
                    value: token.clone(),
                    eval_type: TypeRef::Primitive(PrimitiveType::Nil),
                })
            }
            _ => {
                self.reporter.report_error(
                    &token.location,
                    format_args!(
                        "Unexpected token '{}'",
                        token.location.get_lexeme(self.source)
                    ),
                );
                Err(ParsingStatus::Error)
            }
        }
    }

    fn expr_ident(&self) -> Result<Expr, ParsingStatus> {
        let ident = self.previous();

        if let TokenType::Identifier = &ident.token_type {
            Ok(Expr::Reference {
                ident: self.use_ident(ident),
            })
        } else {
            panic!(
                "Identifier found but also not found (at {:?})",
                ident.location
            )
        }
    }

    // --- Type Parsing --- //
    /// Tries to parse the given type, returning TypeRef::TypeError if the parsing couldn't be salvaged
    /// If a TypeRef::TypeError is produced, the token that caused the error is not consumed
    fn parse_type(&self) -> TypeRef {
        let nom_ok = |prim_type| {
            self.next_token();
            TypeRef::Primitive(prim_type)
        };

        let get_size_specifier = || -> Result<usize, ()> {
            match self.current().token_type {
                TokenType::IntLiteral(size) if size > 0 => {
                    if size as usize >= types::MAX_STRING_SIZE {
                        self.reporter.report_error(
                            &self.current().location,
                            format_args!(
                                "'{}' is larger than or equal to the maximum string length of '{}' (after including the end byte)",
                                size,
                                types::MAX_STRING_SIZE
                            ),
                        );
                        Err(())
                    } else {
                        // Consume parsed type
                        self.next_token();
                        Ok(size as usize)
                    }
                }
                TokenType::Star => {
                    // Consume parsed type
                    self.next_token();
                    Ok(0 as usize)
                }
                _ => {
                    self.reporter.report_error(
                        &self.current().location,
                        format_args!(
                            "Length specifier is not a '*' or a non-zero compile time expression"
                        ),
                    );
                    Err(())
                }
            }
        };

        match &self.current().token_type {
            // Basic primitive types
            TokenType::Addressint => nom_ok(PrimitiveType::AddressInt),
            TokenType::Boolean => nom_ok(PrimitiveType::Boolean),
            TokenType::Int => nom_ok(PrimitiveType::Int),
            TokenType::Int1 => nom_ok(PrimitiveType::Int1),
            TokenType::Int2 => nom_ok(PrimitiveType::Int2),
            TokenType::Int4 => nom_ok(PrimitiveType::Int4),
            TokenType::Nat => nom_ok(PrimitiveType::Nat),
            TokenType::Nat1 => nom_ok(PrimitiveType::Nat1),
            TokenType::Nat2 => nom_ok(PrimitiveType::Nat2),
            TokenType::Nat4 => nom_ok(PrimitiveType::Nat4),
            TokenType::Real => nom_ok(PrimitiveType::Real),
            TokenType::Real4 => nom_ok(PrimitiveType::Real4),
            TokenType::Real8 => nom_ok(PrimitiveType::Real8),
            TokenType::String_ | TokenType::Char => {
                // Nom "string" / "char"
                let is_char_type = matches!(self.next_token().token_type, TokenType::Char);

                // If left paren, construct sized type
                if self.current().token_type == TokenType::LeftParen {
                    self.next_token();

                    // Try to get the size
                    let parsed_size = get_size_specifier();

                    // Missing ) is recoverable
                    let _ = self.expects(
                        TokenType::RightParen,
                        format_args!("Expected ')' after length specifier"),
                    );

                    if is_char_type {
                        match parsed_size {
                            Ok(size) => TypeRef::Primitive(PrimitiveType::CharN(size)),
                            // Try to return as a single char, for preserving type analysis semantic and preserving compatibility with Turing proper
                            Err(_) => TypeRef::Primitive(PrimitiveType::Char),
                        }
                    } else {
                        match parsed_size {
                            Ok(size) => TypeRef::Primitive(PrimitiveType::StringN(size)),
                            // Try to return as a normal string, for preserving type analysis semantic
                            Err(_) => TypeRef::Primitive(PrimitiveType::String_),
                        }
                    }
                } else {
                    // Produce bracketless versions
                    if is_char_type {
                        // Make single char type
                        TypeRef::Primitive(PrimitiveType::Char)
                    } else {
                        // Make varsized type
                        TypeRef::Primitive(PrimitiveType::String_)
                    }
                }
            }

            // Compound primitives (requires type/sym table)
            TokenType::Array => unimplemented!(),
            TokenType::Pointer | TokenType::Caret => unimplemented!(),
            TokenType::Set => unimplemented!(), // Only in "type" decls
            // subprogram_header
            TokenType::Function => unimplemented!(),
            TokenType::Procedure => unimplemented!(),
            TokenType::Enum => unimplemented!(),
            TokenType::Record => unimplemented!(),
            TokenType::Union => unimplemented!(),
            TokenType::Identifier => unimplemented!(),
            _ => {
                self.reporter.report_error(
                    &self.current().location,
                    format_args!(
                        "Unexpected '{}', expected a type specifier",
                        self.current().location.get_lexeme(self.source)
                    ),
                );

                // Return a type error
                TypeRef::TypeError
            }
        }
    }

    // -- Wrappers around the scope list -- //
    // See `Scope` for the documentation of these functions

    fn declare_ident(
        &self,
        ident: &Token,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
    ) -> Identifier {
        let (reference, err) = self
            .blocks
            .borrow()
            .last()
            .unwrap()
            .borrow_mut()
            .scope
            .declare_ident(
                ident,
                ident.location.get_lexeme(self.source).to_string(),
                type_spec,
                is_const,
                is_typedef,
            );

        if let Some(msg) = err {
            self.reporter
                .report_error(&ident.location, format_args!("{}", msg));
        }

        reference
    }

    #[allow(dead_code)]
    fn resolve_ident(
        &self,
        ident: &Token,
        type_spec: TypeRef,
        is_const: bool,
        is_typedef: bool,
    ) -> Identifier {
        self.blocks
            .borrow()
            .last()
            .unwrap()
            .borrow_mut()
            .scope
            .resolve_ident(
                ident.location.get_lexeme(self.source),
                type_spec,
                is_const,
                is_typedef,
            )
    }

    fn use_ident(&self, ident: &Token) -> Identifier {
        let (reference, err) = self
            .blocks
            .borrow()
            .last()
            .unwrap()
            .borrow_mut()
            .scope
            .use_ident(&ident, ident.location.get_lexeme(self.source));

        if let Some(msg) = err {
            self.reporter
                .report_error(&ident.location, format_args!("{}", msg));
        }

        reference
    }

    /// Pushes a new block onto the block list
    fn push_block(&self, block_kind: BlockKind) {
        let block = CodeBlock::new(block_kind, self.blocks.borrow().as_ref());

        // Add the block to the list
        self.blocks.borrow_mut().push(Rc::new(RefCell::new(block)));
    }

    /// Pops a block off of the block list, and moving the given statement list
    /// into the block
    fn pop_block(&self, stmts: &mut Vec<Stmt>) -> Rc<RefCell<CodeBlock>> {
        let block = self.blocks.borrow_mut().pop().unwrap();
        block.borrow_mut().stmts.append(stmts);

        block
    }

    // --- Helpers --- //

    /// Checks if the current tokens form a compound assignment (operator '=')
    fn is_compound_assignment(&self) -> bool {
        if &self.peek().token_type == &TokenType::Equ {
            // Look ahead token is a '=', check if current is one of the valid compound assign operators
            match &self.current().token_type {
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
                | TokenType::Imply => true,
                _ => false,
            }
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

    /// Builds an argument list for an expression
    fn make_arg_list(&self) -> Result<Option<Vec<Expr>>, ParsingStatus> {
        if self.previous().token_type != TokenType::LeftParen {
            // No arg_list to be found
            return Ok(None);
        }

        let mut arg_list = vec![];

        if self.current().token_type != TokenType::RightParen {
            loop {
                arg_list.push(self.expr()?);

                if self.current().token_type != TokenType::Comma {
                    break;
                }

                // Consume comma
                self.next_token();
            }
        }

        self.expects(
            TokenType::RightParen,
            format_args!("Missing ')' after parameter list"),
        )?;

        // Give back the arg list
        return Ok(Some(arg_list));
    }

    /// Gets the precedence rule for the given token
    fn get_rule(&self, token_type: &TokenType) -> &PrecedenceRule {
        match token_type {
            TokenType::LeftParen => &PrecedenceRule {
                precedence: Precedence::Call,
                prefix_rule: Some(Parser::expr_grouping),
                infix_rule: Some(Parser::expr_call),
            },
            TokenType::Imply => &PrecedenceRule {
                precedence: Precedence::Imply,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Or => &PrecedenceRule {
                precedence: Precedence::BitOr,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::And => &PrecedenceRule {
                precedence: Precedence::BitAnd,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Not => &PrecedenceRule {
                precedence: Precedence::BitNot,
                prefix_rule: Some(Parser::expr_unary_rule),
                infix_rule: None,
            },
            TokenType::Less
            | TokenType::Greater
            | TokenType::Equ
            | TokenType::LessEqu
            | TokenType::GreaterEqu
            | TokenType::NotEq
            | TokenType::In
            | TokenType::NotIn => &PrecedenceRule {
                precedence: Precedence::Comparison,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Plus | TokenType::Minus => &PrecedenceRule {
                precedence: Precedence::Sum,
                prefix_rule: Some(Parser::expr_unary),
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Xor => &PrecedenceRule {
                precedence: Precedence::Sum,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Star
            | TokenType::Slash
            | TokenType::Div
            | TokenType::Mod
            | TokenType::Rem
            | TokenType::Shl
            | TokenType::Shr => &PrecedenceRule {
                precedence: Precedence::Product,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Pound => &PrecedenceRule {
                precedence: Precedence::Conversion,
                prefix_rule: Some(Parser::expr_unary_rule),
                infix_rule: None,
            },
            TokenType::Caret => &PrecedenceRule {
                precedence: Precedence::Follow,
                prefix_rule: Some(Parser::expr_unary_rule),
                infix_rule: None,
            },
            TokenType::Deref => &PrecedenceRule {
                precedence: Precedence::Deref,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_deref),
            },
            TokenType::Exp => &PrecedenceRule {
                precedence: Precedence::Exponent,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Dot => &PrecedenceRule {
                precedence: Precedence::Call,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_dot),
            },
            TokenType::IntLiteral(_)
            | TokenType::RealLiteral(_)
            | TokenType::CharLiteral(_)
            | TokenType::StringLiteral(_)
            | TokenType::True
            | TokenType::False
            | TokenType::Nil => &PrecedenceRule {
                precedence: Precedence::Primary,
                prefix_rule: Some(Parser::expr_primary),
                infix_rule: None,
            },
            TokenType::Identifier => &PrecedenceRule {
                precedence: Precedence::Primary,
                prefix_rule: Some(Parser::expr_ident),
                infix_rule: None,
            },
            _ => &PrecedenceRule {
                precedence: Precedence::NoPrec,
                prefix_rule: None,
                infix_rule: None,
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::scanner::Scanner;

    fn make_test_parser(source: &str) -> Parser {
        let mut scanner = Scanner::new(source);
        scanner.scan_tokens();

        Parser::new(scanner.tokens, source)
    }

    fn check_ident_expected_type(parser: &Parser, name: &str, expected: TypeRef) {
        assert_eq!(
            parser
                .blocks
                .borrow()
                .last()
                .unwrap()
                .borrow()
                .scope
                .get_ident(name)
                .unwrap()
                .type_spec,
            expected
        );
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
        parser.parse();
        assert!(!parser.reporter.has_error());

        // Invaild: Dropped value
        let mut parser = make_test_parser(
            "
        % Setup
        var a : int
        a := 
        ",
        );
        parser.parse();
        assert!(parser.reporter.has_error());

        let mut parser = make_test_parser(
            "
        % Setup
        var a : int
        a = 
        ",
        );
        parser.parse();
        assert!(parser.reporter.has_error());
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
        parser.parse();
        assert!(!parser.reporter.has_error());
        let expected_ops = [
            TokenType::Assign,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Div,
            TokenType::Slash,
            TokenType::Rem,
            TokenType::Mod,
            TokenType::Exp,
            TokenType::And,
            TokenType::Or,
            TokenType::Xor,
            TokenType::Shl,
            TokenType::Shr,
            TokenType::Assign,
        ];

        for test_stmt in parser.stmts[2..].iter().zip(expected_ops.iter()) {
            if let Stmt::Assign {
                op: Token { ref token_type, .. },
                ..
            } = test_stmt.0
            {
                if token_type != test_stmt.1 {
                    panic!(
                        "Mismatch between expected {:?} and parsed {:?}",
                        test_stmt.1, token_type
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
        parser.parse();
        assert!(!parser.reporter.has_error());
        let expected_ops = [TokenType::Imply, TokenType::And, TokenType::Or];

        for test_stmt in parser.stmts[1..].iter().zip(expected_ops.iter()) {
            if let Stmt::Assign {
                op: Token { ref token_type, .. },
                ..
            } = test_stmt.0
            {
                if token_type != test_stmt.1 {
                    panic!(
                        "Mismatch between expected {:?} and parsed {:?}",
                        test_stmt.1, token_type
                    );
                }
            }
        }
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
        var o : string(*)       % repr as size == 0
        var p : char
        var q : char(768)
        var r : char(*)         % repr as size == 0
        var s : addressint
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
            TypeRef::Primitive(PrimitiveType::StringN(300)),
            TypeRef::Primitive(PrimitiveType::StringN(0)),
            TypeRef::Primitive(PrimitiveType::Char),
            TypeRef::Primitive(PrimitiveType::CharN(768)),
            TypeRef::Primitive(PrimitiveType::CharN(0)),
            TypeRef::Primitive(PrimitiveType::AddressInt),
        ];

        for test_stmt in parser.stmts.iter().zip(expected_types.iter()) {
            if let Stmt::VarDecl { ref idents, .. } = test_stmt.0 {
                assert_eq!(&idents[0].type_spec, test_stmt.1);
            }
        }

        // Compile time expr (we don't evaluate them yet)
        /*
        let mut parser = make_test_parser(
            "
        const c := 5 + 25 * 2 % 55
        var str : string(c)
        ",
        );
        assert!(parser.parse());
        assert_eq!(
            match parser.stmts[1] {
                Stmt::VarDecl {
                    ident: Identifier { type_spec, .. },
                    ..
                } => type_spec,
                _ => unreachable!(),
            },
            TypeRef::Primitive(PrimitiveType::StringN(55))
        );
        */

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

        // Invalid: Not a type specification (shouldn't parse the := "hee" nor the 'to' as it may cause
        // phantom errors)
        let mut parser = make_test_parser("var c : to := 'hee'");
        assert!(!parser.parse());
        // Failed to parse, as type error
        check_ident_expected_type(&parser, "c", TypeRef::TypeError);
    }

    #[test]
    fn test_identifier_resolution() {
        // v decl usage
        let mut parser = make_test_parser(
            "
        var a : int
        a := a + 1
        ",
        );
        assert!(parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::Primitive(PrimitiveType::Int));

        // v decl usage usage
        let mut parser = make_test_parser(
            "
        var a : int
        a := a + 1
        var b := a + 1
        ",
        );
        assert!(parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::Primitive(PrimitiveType::Int));

        // x usage
        let mut parser = make_test_parser(
            "
        a := a + 1 % final type
        ",
        );
        assert!(!parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::TypeError);

        // x usage decl
        let mut parser = make_test_parser(
            "
        a := a + 1
        var a : int % final type
        ",
        );
        assert!(!parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::Primitive(PrimitiveType::Int));

        // x usage decl decl
        let mut parser = make_test_parser(
            "
        a := a + 1
        var a : int
        var a : string % final type
        ",
        );
        assert!(!parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::Primitive(PrimitiveType::String_));

        // x decl decl
        let mut parser = make_test_parser(
            "
        var a : string
        var a : real8 % final type
        ",
        );
        assert!(!parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::Primitive(PrimitiveType::Real8));

        // x decl usage-in-asn
        let mut parser = make_test_parser(
            "
        var a : string := a + \"oops\"
        ",
        );
        assert!(!parser.parse());
        check_ident_expected_type(&parser, "a", TypeRef::Primitive(PrimitiveType::String_));
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
        assert!(!parser.parse());

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
        assert!(!parser.parse());
    }
}
