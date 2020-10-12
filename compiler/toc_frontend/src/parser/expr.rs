//! Parser fragment, parsing all expressions
use super::{Parser, ParsingStatus};
use crate::token::TokenType;
use toc_ast::ast::{Expr, Identifier, Literal};
use toc_ast::types::{self, PrimitiveType, TypeRef};
use toc_core::Location;

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
    NatLiteral
    RealLiteral
    StringLiteral
    CharLiteral

selector:
    '(' arg_list ')'  // arrays (params: Expr::List)
    '.' reference        // fields (Expr::BinaryOp, op: Dot)

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
    reference                  // Real encoding

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
    /// Arrow Operator \ ->
    Arrow,
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
            Conversion => Arrow,
            Arrow => Call,
            Call => Follow,
            Follow => Primary,
        }
    }
}

// The contents of the Parser are valid for the entire lifetime of the parser,
// but the mutable reference is only bound to the lifetime of the
// PrecedenceRule reference
type PrefixRule<'s, 'local> = fn(&'local mut Parser<'s>) -> Result<Expr, ParsingStatus>;
type InfixRule<'s, 'local> = fn(&'local mut Parser<'s>, Expr) -> Result<Expr, ParsingStatus>;

struct PrecedenceRule<'s, 'local> {
    precedence: Precedence,
    prefix_rule: Option<PrefixRule<'s, 'local>>,
    infix_rule: Option<InfixRule<'s, 'local>>,
}

impl<'s> Parser<'s> {
    // --- Expr Parsing --- //

    pub(super) fn expr(&mut self) -> Result<Expr, ParsingStatus> {
        let last_nesting = self.expr_nesting;
        let expr = self.expr_precedence(Precedence::Imply);
        assert_eq!(self.expr_nesting, last_nesting);
        expr
    }

    pub(super) fn expr_reference(&mut self) -> Result<Expr, ParsingStatus> {
        let last_nesting = self.expr_nesting;
        let expr = self.expr_precedence(Precedence::Arrow);
        assert_eq!(self.expr_nesting, last_nesting);
        expr
    }

    fn expr_precedence(&mut self, min_precedence: Precedence) -> Result<Expr, ParsingStatus> {
        // Update the nesting depth
        self.expr_nesting = self.expr_nesting.saturating_add(1);

        if self.expr_nesting > super::MAX_NESTING_DEPTH {
            // Over the nesting limit
            self.reporter.report_error(
                &self.current().location,
                format_args!("Implementation limit - Expression is nested too deeply"),
            );

            self.expr_nesting = self
                .expr_nesting
                .checked_sub(1)
                .expect("Mismatched nesting counts");

            // nom the token!
            self.next_token();
            return Err(ParsingStatus::Error);
        }

        // Keep track of last token
        let before_op = self.previous();

        // Get prefix side
        let op = self.current();

        let prefix = Parser::get_rule(&op.token_type);
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

            // The token reference are relative to the next token, as the next
            // token is consumed unconditionally
            self.reporter.report_error(
                &self.current().location,
                format_args!(
                    "Expected expression before '{}' {}",
                    self.current().location.get_lexeme(self.source),
                    hint
                ),
            );

            ParsingStatus::Error
        });

        if prefix_rule.is_err() {
            // Reduce depth
            self.expr_nesting = self
                .expr_nesting
                .checked_sub(1)
                .expect("Mismatched nesting counts");

            return Err(ParsingStatus::Error);
        }
        let prefix_rule = prefix_rule.unwrap();

        // Consume the token
        self.next_token();

        let mut expr = prefix_rule(self)
            .map_err(|e| {
                self.expr_nesting = self
                    .expr_nesting
                    .checked_sub(1)
                    .expect("Mismatched nesting counts");
                e
            })
            .unwrap();

        // Go over infix operators
        while !self.is_at_end()
            && min_precedence <= Parser::get_rule(&self.current().token_type).precedence
        {
            let op = self.current();
            let infix = Parser::get_rule(&op.token_type);

            if infix.precedence >= Precedence::Follow
                && !matches!(op.token_type, TokenType::At | TokenType::Range)
            {
                // Is a deref, identifier, or literal
                // Most likely end of expression, so return

                // Reduce depth
                self.expr_nesting = self
                    .expr_nesting
                    .checked_sub(1)
                    .expect("Mismatched nesting counts");
                return Ok(expr);
            }

            let infix_rule = infix.infix_rule;

            if infix_rule.is_none() {
                // Not a valid infix rule, return whatever expression was parsed
                self.reporter.report_error(
                    &op.location,
                    format_args!("'{}' cannot be used as an infix operator", op.token_type),
                );

                // Reduce depth
                self.expr_nesting = self
                    .expr_nesting
                    .checked_sub(1)
                    .expect("Mismatched nesting counts");
                return Ok(expr);
            }

            // Consume token for infix rule
            self.next_token();

            // Produce the next expression
            let infix_rule = infix_rule.unwrap();
            expr = infix_rule(self, expr)
                .map_err(|e| {
                    self.expr_nesting = self
                        .expr_nesting
                        .checked_sub(1)
                        .expect("Mismatched nesting counts");
                    e
                })
                .unwrap();
        }

        // Reduce nesting
        self.expr_nesting = self
            .expr_nesting
            .checked_sub(1)
            .expect("Mismatched nesting counts");
        // Give back parsed expression
        Ok(expr)
    }

    fn expr_grouping(&mut self) -> Result<Expr, ParsingStatus> {
        let expr = match self.expr() {
            Ok(expr) => expr,
            Err(_) => {
                // Fatal error encountered
                // Skip all tokens until we reach a safe point
                self.skip_to_safe_point();
                return Ok(Expr::Empty);
            }
        };

        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' to close off parenthetical grouping"),
        );

        // Give back inner expr
        Ok(expr)
    }

    fn expr_binary(&mut self, lhs: Expr) -> Result<Expr, ParsingStatus> {
        let op = self.previous().clone();
        let precedence = Parser::get_rule(&op.token_type).precedence.up();
        // Get rhs
        let rhs = self.expr_precedence(precedence).ok();

        if rhs.is_none() {
            // Return back the lhs
            return Ok(lhs);
        }

        let rhs = rhs.unwrap();
        let span = lhs.get_span().span_to(&self.previous().location);

        Ok(Expr::BinaryOp {
            left: Box::new(lhs),
            op: (super::try_into_binary(op.token_type)?, op.location),
            right: Box::new(rhs),
            eval_type: TypeRef::Unknown,
            is_compile_eval: false,
            span,
        })
    }

    fn expr_unary(&mut self) -> Result<Expr, ParsingStatus> {
        let op = self.previous().clone();
        let right = self
            .expr_precedence(Precedence::Unary)
            .ok()
            .unwrap_or(Expr::Empty);
        let span = op.location.span_to(&self.previous().location);

        let location = op.location;

        Ok(Expr::UnaryOp {
            op: (super::try_into_unary(op.token_type)?, location),
            right: Box::new(right),
            eval_type: TypeRef::Unknown,
            is_compile_eval: false,
            span,
        })
    }

    fn expr_unary_rule(&mut self) -> Result<Expr, ParsingStatus> {
        let mut op = self.previous().clone();
        let precedence = Parser::get_rule(&op.token_type).precedence;
        let right = self.expr_precedence(precedence).ok().unwrap_or(Expr::Empty);
        let span = op.location.span_to(&self.previous().location);

        if op.token_type == TokenType::Tilde {
            // Convert '~'s into 'not's
            op.token_type = TokenType::Not;
        }

        Ok(Expr::UnaryOp {
            op: (super::try_into_unary(op.token_type)?, op.location),
            right: Box::new(right),
            eval_type: TypeRef::Unknown,
            is_compile_eval: false,
            span,
        })
    }

    fn expr_call(&mut self, func_ref: Expr) -> Result<Expr, ParsingStatus> {
        let op = self.previous().clone();
        let arg_list = self.make_arg_list().unwrap();
        let span = func_ref.get_span().span_to(&self.previous().location);

        Ok(Expr::Call {
            left: Box::new(func_ref),
            paren_at: op.location,
            arg_list,
            eval_type: TypeRef::Unknown,
            is_compile_eval: false,
            span,
        })
    }

    fn expr_dot(&mut self, var_ref: Expr) -> Result<Expr, ParsingStatus> {
        self.parse_dot(var_ref, false)
    }

    fn expr_arrow(&mut self, var_ref: Expr) -> Result<Expr, ParsingStatus> {
        // Arrow expression may desugar into either a pointer specialization
        // or deref-dot pair depending on the reference type
        self.parse_dot(var_ref, true)
    }

    /// Parses a dot, either producing an Expr::Dot or an Expr::Arrow
    fn parse_dot(&mut self, var_ref: Expr, as_arrow: bool) -> Result<Expr, ParsingStatus> {
        // Get the ident
        let ident = self.expects(
            TokenType::Identifier,
            format_args!("Missing identifier after '.'"),
        );

        // Return the var_ref on error
        if ident.is_err() {
            return Ok(var_ref);
        }

        let ident = ident.unwrap();

        // The actual identifier information will be resolved at validator time,
        // so we can just store the field name and location info

        let name = ident.location.get_lexeme(&self.source).to_string();
        let span = var_ref.get_span().span_to(&self.previous().location);
        // Field info will be updated to the correct type at validator time
        let field = Identifier::new(
            ident.location,
            TypeRef::Unknown,
            name,
            false,
            false,
            true,
            0,
        );

        if as_arrow {
            Ok(Expr::Arrow {
                left: Box::new(var_ref),
                field,
                eval_type: TypeRef::Unknown,
                is_compile_eval: false,
                span,
            })
        } else {
            Ok(Expr::Dot {
                left: Box::new(var_ref),
                field,
                eval_type: TypeRef::Unknown,
                is_compile_eval: false,
                span,
            })
        }
    }

    fn expr_primary(&mut self) -> Result<Expr, ParsingStatus> {
        let token = self.previous().clone();
        let token_type = token.token_type.clone();

        match token_type {
            TokenType::StringLiteral(s) => Ok(Expr::Literal {
                eval_type: TypeRef::Primitive(types::get_string_kind(&s)),
                value: Literal::StrSequence(s),
                span: token.location,
            }),
            TokenType::CharLiteral(s) => Ok(Expr::Literal {
                eval_type: TypeRef::Primitive(types::get_char_kind(&s)),
                value: Literal::CharSequence(s),
                span: token.location,
            }),
            TokenType::NatLiteral(v) => Ok(Expr::Literal {
                value: Literal::Nat(v),
                eval_type: TypeRef::Primitive(types::get_intnat_kind(v)),
                span: token.location,
            }),
            TokenType::RealLiteral(v) => Ok(Expr::Literal {
                value: Literal::Real(v),
                eval_type: TypeRef::Primitive(PrimitiveType::Real),
                span: token.location,
            }),
            TokenType::True => Ok(Expr::Literal {
                value: Literal::Bool(true),
                eval_type: TypeRef::Primitive(PrimitiveType::Boolean),
                span: token.location,
            }),
            TokenType::False => Ok(Expr::Literal {
                value: Literal::Bool(false),
                eval_type: TypeRef::Primitive(PrimitiveType::Boolean),
                span: token.location,
            }),
            TokenType::Nil => {
                // Consume optional collection / class id

                // TODO: validate that theses are the same as the pointer type (ie produce nil for a given type id)
                // For classes, both must have a common ancestor
                if self.optional(TokenType::LeftParen) {
                    // Consume optional identifier & parens
                    self.optional(TokenType::Identifier);

                    let _ = self.expects(
                        TokenType::RightParen,
                        format_args!("Expected ')' to close off parentheses for 'nil'"),
                    );
                }

                Ok(Expr::Literal {
                    value: Literal::Nil,
                    eval_type: TypeRef::Primitive(PrimitiveType::Nil),
                    span: token.location,
                })
            }
            _ => {
                // Unexpected token
                self.reporter.report_error(
                    &token.location,
                    format_args!("Unexpected token '{}'", self.get_token_lexeme(&token)),
                );
                // Give back an empty expression
                Ok(Expr::Empty)
            }
        }
    }

    fn expr_ident(&mut self) -> Result<Expr, ParsingStatus> {
        let ident_tok = self.previous().clone();

        if let TokenType::Identifier = &ident_tok.token_type {
            // Ignore the error right now, as the identifier may reference
            // something imported unqualified from another file. These
            // references will be resolved at validator time, and that
            // is where the error will be reported
            Ok(Expr::Reference {
                ident: self.use_ident(ident_tok).unwrap_or_else(|err| err.into()),
                eval_type: TypeRef::Unknown,
            })
        } else {
            panic!(
                "Identifier found but also not found (at {:?})",
                ident_tok.location
            )
        }
    }

    /// Parses an indirection expr, using a type reference expression
    fn expr_indirect_ref(&mut self, type_ref: Expr) -> Result<Expr, ParsingStatus> {
        self.expr_indirect(
            *type_ref.get_span(),
            Some(Box::new(type_ref)),
            TypeRef::Unknown,
        )
    }

    /// Parses an indirection expr, using a primitive type reference
    fn expr_indirect_type(&mut self) -> Result<Expr, ParsingStatus> {
        // Valid primitive types are:
        // - `addressint`
        // - `int`, `int1`, `int2`, `int4`
        // - `nat`, `nat1`, `nat2`, `nat4`
        // - `real`, `real4`, `real8`
        // - `boolean`
        // - `char`, `char(expr)`
        // - `string`, `string(expr)`

        let start_span = self.previous().location;
        let type_ref = self.parse_primitive_type(&TokenType::At);
        let at = self.expects(
            TokenType::At,
            format_args!("Expected '@' after primitive type (to form an indirection expression)"),
        );

        if at.is_ok() {
            self.expr_indirect(start_span, None, type_ref)
        } else {
            // Make a dummy expression
            Ok(Expr::Indirect {
                reference: None,
                addr: Box::new(Expr::Empty),
                eval_type: type_ref,
                span: start_span,
            })
        }
    }

    /// Parses the rest of an indirection expression
    fn expr_indirect(
        &mut self,
        span_from: Location,
        reference: Option<Box<Expr>>,
        eval_type: TypeRef,
    ) -> Result<Expr, ParsingStatus> {
        // ... '@' '(' expr ')'
        assert_eq!(self.previous().token_type, TokenType::At);

        let _ = self.expects(TokenType::LeftParen, format_args!("Expected '(' after '@'"));
        let addr = Box::new(self.expr().unwrap_or(Expr::Empty));
        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' after address expression"),
        );
        let span = span_from.span_to(&self.previous().location);

        Ok(Expr::Indirect {
            reference,
            addr,
            eval_type,
            span,
        })
    }

    /// Parses an init expression.
    /// Expects the "init" token to be consumed.
    ///
    /// While not a true expression, it is in expression position.
    ///
    /// `init` `(` expr (`,` expr)+ `)`
    pub(super) fn expr_init(&mut self) -> Result<Expr, ParsingStatus> {
        let init_token = self.previous().clone();

        let _ = self.expects(
            TokenType::LeftParen,
            format_args!("Expected '(' after 'init'"),
        );
        let mut exprs = vec![];

        loop {
            let next_expr = self.expr();

            // Always fill positions with something
            exprs.push(next_expr.unwrap_or(Expr::Empty));

            if !self.optional(TokenType::Comma) {
                // No more commas
                break;
            }
        }

        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' after the last expression"),
        );

        Ok(Expr::Init {
            span: init_token.location.span_to(&self.previous().location),
            init: init_token.location,
            exprs,
        })
    }

    // --- Helpers --- //

    /// Builds an argument list for an expression
    fn make_arg_list(&mut self) -> Option<Vec<Expr>> {
        if self.previous().token_type != TokenType::LeftParen {
            // No arg_list to be found
            return None;
        }

        let mut arg_list = vec![];

        if self.current().token_type != TokenType::RightParen {
            loop {
                // Fill arguments with something
                arg_list.push(self.expr().ok().unwrap_or(Expr::Empty));

                if self.current().token_type != TokenType::Comma {
                    break;
                }

                // Consume comma
                self.next_token();
            }
        }

        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Missing ')' after parameter list"),
        );

        // Give back the arg list
        Some(arg_list)
    }

    /// Gets the precedence rule for the given token
    fn get_rule<'a>(token_type: &'a TokenType) -> &PrecedenceRule<'s, 'a> {
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
            TokenType::Tilde | TokenType::Not => &PrecedenceRule {
                precedence: Precedence::BitNot,
                prefix_rule: Some(Parser::expr_unary_rule),
                infix_rule: None,
            },
            TokenType::Less
            | TokenType::Greater
            | TokenType::Equ
            | TokenType::LessEqu
            | TokenType::GreaterEqu
            | TokenType::NotEqu
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
            TokenType::Arrow => &PrecedenceRule {
                precedence: Precedence::Arrow,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_arrow),
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
            TokenType::NatLiteral(_)
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
            TokenType::At => &PrecedenceRule {
                precedence: Precedence::Primary,
                prefix_rule: None,
                infix_rule: Some(Parser::expr_indirect_ref),
            },
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
            | TokenType::String_ => &PrecedenceRule {
                precedence: Precedence::Primary,
                prefix_rule: Some(Parser::expr_indirect_type),
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
