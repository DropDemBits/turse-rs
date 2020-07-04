//! Main parser for tokens to build the AST
use crate::compiler::ast::{Expr, Stmt};
use crate::compiler::token::{Token, TokenType};
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

    pub fn parse(&mut self) {
        while !self.is_at_end() {
            match self.stmt() {
                Ok(expr) => self.stmts.push(expr),
                Err(_) => {}
            }

            if self.current().token_type == TokenType::Semicolon {
                // Nom the semicolon
                self.next_token();
            }
        }
    }

    fn report_error<T>(&self, at: Location, message: Arguments) -> Result<T, ParsingStatus> {
        self.reporter.report_error(&at, message);
        Err(ParsingStatus::Error)
    }

    #[allow(dead_code)]
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

    // --- Decl Parsing --- //

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

        if is_compound_assign
            || matches!(
                self.current().token_type,
                TokenType::Equ | TokenType::Assign
            )
        {
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
                self.report_warning(locate, format_args!("'=' found, assumed it to be ':='"));

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
            eval_type: 0,
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
            eval_type: 0,
        })
    }

    fn expr_unary(&self) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let right = self.expr_precedence(Precedence::Unary)?;

        Ok(Expr::UnaryOp {
            op: op.clone(),
            right: Box::new(right),
            eval_type: 0,
        })
    }

    fn expr_unary_rule(&self) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let rule = self.get_rule(&op.token_type);
        let right = self.expr_precedence(rule.precedence)?;

        Ok(Expr::UnaryOp {
            op: op.clone(),
            right: Box::new(right),
            eval_type: 0,
        })
    }

    fn expr_call(&self, func_ref: Expr) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let arg_list = self.make_arg_list()?.unwrap();

        Ok(Expr::Call {
            left: Box::new(func_ref),
            op: op.clone(),
            arg_list,
            eval_type: 0,
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
            ident: ident.clone(),
            name: ident.location.get_lexeme(self.source).to_string(),
            eval_type: 0,
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
            eval_type: 0,
        })
    }

    fn expr_primary(&self) -> Result<Expr, ParsingStatus> {
        let token = self.previous();

        match token.token_type {
            TokenType::StringLiteral(_) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: 0,
            }),
            TokenType::CharLiteral(_) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: 0,
            }),
            TokenType::IntLiteral(_) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: 0,
            }),
            TokenType::RealLiteral(_) => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: 0,
            }),
            TokenType::True => Ok(Expr::Literal {
                value: Token {
                    token_type: TokenType::BoolLiteral(true),
                    location: token.location.clone(),
                },
                eval_type: 0,
            }),
            TokenType::False => Ok(Expr::Literal {
                value: Token {
                    token_type: TokenType::BoolLiteral(false),
                    location: token.location.clone(),
                },
                eval_type: 0,
            }),
            TokenType::Nil => Ok(Expr::Literal {
                value: token.clone(),
                eval_type: 0,
            }),
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
                ident: token.clone(),
                name: token.location.get_lexeme(self.source).to_string(),
                eval_type: 0,
            })
        } else {
            panic!(
                "Identifier found but also not found (at {:?})",
                token.location
            )
        }
    }

    /// --- Helpers ---
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
