//! Main parser for tokens to build the AST
use crate::compiler::ast::Expr;
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
    '(' param_list ')'  // arrays
    '.' reference		// fields

reference:
    identifier selector?

binary:
    expr binOp expr

unary:
    unOp expr

unOp:
    '+'
    '-'
    '^'
    '#'

funCall:
    reference '(' param_list? ')'

setCons:
    reference '(' param_list? ')' // Encoded as a "call" to the constructor

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
    /// Conversion Operators \ # ^
    Conversion,
    /// Exponent Operator \ **
    Exponent,
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
            Unary => Conversion,
            Conversion => Exponent,
            Exponent => Primary,
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
    /// Parsed expressions
    pub exprs: Vec<Expr>,
    /// Current token being parsed
    current: Cell<usize>,
}

#[derive(Debug)]
enum ParsingStatus {
    Error,
    Warn,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self {
            reporter: StatusReporter::new(),
            source,
            tokens,
            exprs: vec![],
            current: Cell::new(0),
        }
    }

    pub fn parse(&mut self) {
        while !self.is_at_end() {
            match self.stmt() {
                Ok(expr) => self.exprs.push(expr),
                Err(_) => {}
            }
        }
    }

    fn report_error<T>(&self, at: Location, message: Arguments) -> Result<T, ParsingStatus> {
        self.reporter.report_error(&at, message);
        Err(ParsingStatus::Error)
    }

    #[allow(dead_code)]
    fn report_warning<T>(&self, at: Location, message: Arguments) {
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

    fn stmt(&self) -> Result<Expr, ParsingStatus> {
        let expr = self.expr()?;

        self.expects(
            TokenType::Semicolon,
            format_args!("Expected semicolon after expression"),
        )?;

        Ok(expr)
    }

    // --- Expr Parsing ---

    fn expr(&self) -> Result<Expr, ParsingStatus> {
        self.expr_precedence(Precedence::Imply)
    }

    fn expr_precedence(&self, min_precedence: Precedence) -> Result<Expr, ParsingStatus> {
        // Keep track of last token
        let before_op = self.previous();

        // Get prefix side
        let op = self.next_token();

        let prefix_rule = self.get_rule(&op.token_type).prefix_rule.ok_or_else(|| {
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

        // Go over infix operators
        let mut expr = prefix_rule(self)?;

        while !self.is_at_end()
            && min_precedence <= self.get_rule(&self.current().token_type).precedence
        {
            let op = self.next_token();

            let infix_rule = self
                .get_rule(&op.token_type)
                .infix_rule
                .expect("No infix function for given rule");

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

    fn expr_unary(&self) -> Result<Expr, ParsingStatus> {
        let op = self.previous();
        let right = self.expr_precedence(Precedence::Unary)?;

        Ok(Expr::UnaryOp {
            op: op.clone(),
            right: Box::new(right),
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

    /// Gets the precedence and associativity
    fn get_rule(&self, token_type: &TokenType) -> &PrecedenceRule {
        match token_type {
            TokenType::LeftParen => &PrecedenceRule {
                precedence: Precedence::NoPrec,
                prefix_rule: Some(Parser::expr_grouping),
                infix_rule: None,
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
                prefix_rule: Some(Parser::expr_unary),
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
            TokenType::Pound | TokenType::Caret => &PrecedenceRule {
                precedence: Precedence::Conversion,
                prefix_rule: Some(Parser::expr_unary),
                infix_rule: None,
            },
            TokenType::Exp => &PrecedenceRule {
                precedence: Precedence::Exponent,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
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
            _ => &PrecedenceRule {
                precedence: Precedence::NoPrec,
                prefix_rule: None,
                infix_rule: None,
            },
        }
    }
}
