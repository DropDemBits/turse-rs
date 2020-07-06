//! Main parser for tokens to build the AST
use crate::compiler::ast::{ASTVisitor, Expr, Identifier, Stmt};
use crate::compiler::token::{Token, TokenType};
use crate::compiler::types::{self, PrimitiveType, TypeRef};
use crate::compiler::Location;
use crate::status_reporter::StatusReporter;
use std::cell::Cell;
use std::fmt::Arguments;

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
    /// Parsed statements
    pub stmts: Vec<Stmt>,
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
            current: Cell::new(0),
        }
    }

    /// Parses the token stream
    /// Returns if the parse has no errors
    pub fn parse(&mut self) -> bool {
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

    fn report_error<T>(&self, at: Location, message: Arguments) -> Result<T, ParsingStatus> {
        self.reporter.report_error(&at, message);
        Err(ParsingStatus::Error)
    }

    fn report_warning(&self, at: Location, message: Arguments) {
        self.reporter.report_warning(&at, message);
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
            self.report_error(self.current().location, message)
        }
    }

    fn warn_equ_as_assign(&self, at: Location) {
        self.report_warning(at, format_args!("'=' found, assumed it to be ':='"));
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

        // Grab identifier
        let ident = self.expects(
            TokenType::Identifier,
            format_args!("Expected an identifier for the declared {}", decl_name),
        )?;

        // Grab typespec
        let type_spec = if self.current().token_type == TokenType::Colon {
            // Consume colon
            self.next_token();

            // Parse the type spec
            self.parse_type()?
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
            Some(Box::new(self.expr()?))
        } else if is_const {
            // const declares require the assignment expression
            self.report_error(
                decl_tok.location,
                format_args!("const declaration requires an initial value"),
            )?;

            unreachable!()
        } else {
            None
        };

        if let TypeRef::Unknown = type_spec {
            if assign_expr.is_none() {
                self.report_error(decl_tok.location, format_args!("Cannot infer type for given {} declaration (no type specification or initial value given)", decl_name))?;
            }
        }

        Ok(Stmt::VarDecl {
            ident: self.make_identifier(ident, type_spec),
            value: assign_expr,
            is_const,
        })
    }

    // --- Stmt Parsing --- //

    fn stmt(&self) -> Result<Stmt, ParsingStatus> {
        let nom = self.current();
        match nom.token_type {
            TokenType::Identifier | TokenType::Caret => self.handle_reference_stmt(),
            _ => {
                // Nom as token isn't consumed by anything else
                self.next_token();

                self.report_error(
                    nom.location,
                    format_args!(
                        "'{}' does not begin a statement or declaration",
                        nom.location.get_lexeme(self.source)
                    ),
                )
            }
        }
    }

    fn handle_reference_stmt(&self) -> Result<Stmt, ParsingStatus> {
        // Identifiers & References can begin either an assignment or a procedure call
        // Both take references as the primary expression
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

            let _ = self.report_error::<Expr>(
                self.previous().location,
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

        Ok(Expr::Dot {
            left: Box::new(var_ref),
            ident: self.make_identifier(ident, TypeRef::Unknown),
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

                // TODO: validate that theses are the same as the pointer type
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
            _ => self.report_error(
                token.location,
                format_args!(
                    "Unexpected token '{}'",
                    token.location.get_lexeme(self.source)
                ),
            ),
        }
    }

    fn expr_ident(&self) -> Result<Expr, ParsingStatus> {
        let token = self.previous();

        if let TokenType::Identifier = &token.token_type {
            Ok(Expr::Reference {
                ident: self.make_identifier(token, TypeRef::Unknown),
            })
        } else {
            panic!(
                "Identifier found but also not found (at {:?})",
                token.location
            )
        }
    }

    // --- Type Parsing --- //
    fn parse_type(&self) -> Result<TypeRef, ParsingStatus> {
        let nom_ok = |prim_type| {
            self.next_token();
            TypeRef::Primitive(prim_type)
        };

        let get_size_specifier = || {
            let size = match self.current().token_type {
                TokenType::IntLiteral(size) if size > 0 => {
                    if size as usize >= types::MAX_STRING_SIZE {
                        self.report_error(
                            self.current().location,
                            format_args!(
                                "'{}' is larger than or equal to the maximum string length of '{}' (after including the end byte)",
                                size,
                                types::MAX_STRING_SIZE
                            ),
                        )?;
                    }

                    size
                }
                TokenType::Star => 0,
                _ => self.report_error(
                    self.current().location,
                    format_args!(
                        "Length specifier is not a '*' or a non-zero compile time expression"
                    ),
                )?,
            } as usize;

            // Over parsed type
            self.next_token();

            Ok(size)
        };

        match &self.current().token_type {
            // Basic primitive types
            TokenType::Addressint => Ok(nom_ok(PrimitiveType::AddressInt)),
            TokenType::Boolean => Ok(nom_ok(PrimitiveType::Boolean)),
            TokenType::Int => Ok(nom_ok(PrimitiveType::Int)),
            TokenType::Int1 => Ok(nom_ok(PrimitiveType::Int1)),
            TokenType::Int2 => Ok(nom_ok(PrimitiveType::Int2)),
            TokenType::Int4 => Ok(nom_ok(PrimitiveType::Int4)),
            TokenType::Nat => Ok(nom_ok(PrimitiveType::Nat)),
            TokenType::Nat1 => Ok(nom_ok(PrimitiveType::Nat1)),
            TokenType::Nat2 => Ok(nom_ok(PrimitiveType::Nat2)),
            TokenType::Nat4 => Ok(nom_ok(PrimitiveType::Nat4)),
            TokenType::Real => Ok(nom_ok(PrimitiveType::Real)),
            TokenType::Real4 => Ok(nom_ok(PrimitiveType::Real4)),
            TokenType::Real8 => Ok(nom_ok(PrimitiveType::Real8)),
            TokenType::String_ => {
                // Nom string
                self.next_token();

                // If left paren, construct sized type
                if self.current().token_type == TokenType::LeftParen {
                    self.next_token();

                    let size = get_size_specifier()?;

                    self.expects(
                        TokenType::RightParen,
                        format_args!("Expected ')' after length specifier"),
                    )?;

                    Ok(TypeRef::Primitive(PrimitiveType::StringN(size)))
                } else {
                    // Make varsized type
                    Ok(TypeRef::Primitive(PrimitiveType::String_))
                }
            }
            TokenType::Char => {
                // Nom char
                self.next_token();

                // If left paren, construct sized type
                if self.current().token_type == TokenType::LeftParen {
                    self.next_token();

                    let size = get_size_specifier()?;

                    self.expects(
                        TokenType::RightParen,
                        format_args!("Expected ')' after length specifier"),
                    )?;

                    Ok(TypeRef::Primitive(PrimitiveType::CharN(size)))
                } else {
                    // Make single char type
                    Ok(TypeRef::Primitive(PrimitiveType::Char))
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
            _ => self.report_error(
                self.current().location,
                format_args!(
                    "Unexpected '{}', expected a type specifier",
                    self.current().location.get_lexeme(self.source)
                ),
            ),
        }
    }

    // --- Helpers --- //

    fn make_identifier(&self, token: &Token, type_spec: TypeRef) -> Identifier {
        Identifier::new(token, type_spec, self.source)
    }

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

    /// Gets the precedence and associativity
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

    #[test]
    fn test_var_decl() {
        let mut parser = make_test_parser(
            "
        % Valid forms
        var a : int := 1
        var b : int
        var c := 3 + 6 ** 2
        
        % Accepted forms
        var d : int = -5
        var e : int = -10 + 3 * 2",
        );
        parser.parse();
        assert!(!parser.reporter.has_error());

        let mut parser = make_test_parser(
            "
        % Invalid forms
        var a
        var c",
        );
        parser.parse();
        assert!(parser.reporter.has_error());
    }

    #[test]
    fn test_const_decl() {
        let mut parser = make_test_parser(
            "
        % Valid forms
        const a : int := 1
        const c := 3 + 6 ** 2
        
        % Accepted forms
        const d : int = -5
        const e : int = -10 + 3 * 2",
        );
        parser.parse();
        assert!(!parser.reporter.has_error());

        // Invalid forms
        let mut parser = make_test_parser(
            "
        % Invalid forms - No type or value
        const a
        const b",
        );
        parser.parse();
        assert!(parser.reporter.has_error());

        let mut parser = make_test_parser(
            "
        % Invalid forms - No value
        const a : int
        const b : int",
        );
        parser.parse();
        assert!(parser.reporter.has_error());
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
            if let Stmt::VarDecl {
                ident: Identifier { ref type_spec, .. },
                ..
            } = test_stmt.0
            {
                assert_eq!(type_spec, test_stmt.1);
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
        let mut parser = make_test_parser("const c : string(16#10000)");
        assert!(!parser.parse());

        let mut parser = make_test_parser("const c : string(16#10001)");
        assert!(!parser.parse());

        // Invalid: Zero length size expression
        let mut parser = make_test_parser("const c : char(16#0)");
        assert!(!parser.parse());

        // Invalid: Dropping the right paren
        let mut parser = make_test_parser("const c : char(16#0");
        assert!(!parser.parse());

        // Invalid: No length specification
        let mut parser = make_test_parser("const c : string(");
        assert!(!parser.parse());

        // Invalid: Not a type specification (should parse the := "hee", but parser needs a bit of rework)
        let mut parser = make_test_parser("const c : to := 'hee'");
        assert!(!parser.parse());
    }
}
