//! Main parser for tokens to build the AST
use crate::compiler::ast::Expr;
use crate::compiler::token::{Token, TokenType};
use crate::compiler::Location;
use crate::status_reporter::StatusReporter;
use std::cell::Cell;

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

struct PrecedenceRule {
    precedence: Precedence,
    prefix_rule: Option<fn(&Parser) -> Result<Expr, ParsingStatus>>,
    infix_rule: Option<fn(&Parser, Expr) -> Result<Expr, ParsingStatus>>,
}

/// Main parser
#[derive(Debug)]
pub struct Parser {
    /// Status reporter
    reporter: StatusReporter,
    /// Source for tokens
    tokens: Vec<Token>,
    /// Parsed expressions
    pub exprs: Vec<Expr>,
    /// Current token being parsed
    current: Cell<usize>,
}

#[derive(Debug)]
enum ParsingStatus {
    Error(Location, String),
    Warn(Location, String),
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            reporter: StatusReporter::new(),
            tokens,
            exprs: vec![],
            current: Cell::new(0),
        }
    }

    pub fn parse(&mut self) {
        match self.expr() {
            Ok(expr) => self.exprs.push(expr),
            Err(status) => match status {
                ParsingStatus::Error(loc, msg) => {
                    self.reporter.report_error(&loc, format_args!("{}", msg))
                }
                ParsingStatus::Warn(loc, msg) => {
                    self.reporter.report_warning(&loc, format_args!("{}", msg))
                }
            },
        }
    }

    /// Gets the previous token in the stream
    fn previous(&self) -> &Token {
        &self.tokens[self.current.get().saturating_sub(1)]
    }

    /// Peeks at the next token in the stream
    fn peek(&self) -> &Token {
        &self.tokens[self.current.get()]
    }

    /// Peeks at the next next token in the stream
    fn peek_ahead(&self) -> &Token {
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
        self.current.get() >= self.tokens.len() || self.peek().token_type == TokenType::Eof
    }

    /// Consumes the expected token, or returns a message indicating the error
    fn expects(&self, expected_type: TokenType, message: String) -> Result<&Token, ParsingStatus> {
        if self.peek().token_type == expected_type {
            Ok(self.next_token())
        } else {
            Err(ParsingStatus::Error(self.peek().location, message))
        }
    }

    // --- Expr Parsing ---

    fn expr(&self) -> Result<Expr, ParsingStatus> {
        self.expr_precedence(Precedence::Imply)
    }

    fn expr_precedence(&self, min_precedence: Precedence) -> Result<Expr, ParsingStatus> {
        // Get prefix side
        let op = self.next_token();
        let prefix_rule =
            self.get_precedence(&op.token_type)
                .prefix_rule
                .ok_or(ParsingStatus::Error(
                    self.previous().location,
                    "Expected expression".to_string(),
                ))?;

        // Go over infix operators
        let mut expr = prefix_rule(self)?;

        while !self.is_at_end()
            && min_precedence <= self.get_precedence(&self.peek().token_type).precedence
        {
            let op = self.next_token();

            let infix_rule = self
                .get_precedence(&op.token_type)
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
            "Expected ')' to close off parenthetical grouping".to_string(),
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
        let rule = self.get_precedence(&op.token_type);
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
            _ => Err(ParsingStatus::Error(
                token.location,
                "Unexpected token".to_string(),
            )),
        }
    }

    /// Gets the precedence and associativity
    fn get_precedence(&self, token_type: &TokenType) -> &PrecedenceRule {
        match token_type {
            TokenType::LeftParen => &PrecedenceRule {
                precedence: Precedence::NoPrec,
                prefix_rule: Some(Parser::expr_grouping),
                infix_rule: None,
            },
            TokenType::Plus | TokenType::Minus => &PrecedenceRule {
                precedence: Precedence::Sum,
                prefix_rule: Some(Parser::expr_unary),
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::Star | TokenType::Slash | TokenType::Div => &PrecedenceRule {
                precedence: Precedence::Product,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_binary),
            },
            TokenType::IntLiteral(_) | TokenType::RealLiteral(_) => &PrecedenceRule {
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
