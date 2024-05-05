//! Expression parsing
#[cfg(test)]
mod test;

use super::*;
use toc_syntax::{InfixOp, PrefixOp, SyntaxKind};

pub(super) fn expect_param_expr(p: &mut Parser) -> Option<CompletedMarker> {
    // cases:
    // AllArg: 'all'
    // ExprArg: Expr
    // RangeArg: '*' ( '-' Expr)?
    // RangeArg: RangeBound '..' RangeBound

    if p.at(TokenKind::All) {
        let m = p.start();
        p.bump();
        return Some(m.complete(p, SyntaxKind::AllItem));
    }

    let bound = expect_range_bound(p)?;
    let lhs = if p.at(TokenKind::Range) {
        let m = bound.marker().precede(p);
        p.bump();

        expect_range_bound(p);

        m.complete(p, SyntaxKind::RangeItem)
    } else {
        match bound {
            ParsedBound::Expr(m) => m,
            // Embed the bound in a range expr
            ParsedBound::Bound(m) => m.precede(p).complete(p, SyntaxKind::RangeItem),
        }
    };

    Some(lhs)
}

enum ParsedBound {
    Expr(CompletedMarker),
    Bound(CompletedMarker),
}

impl ParsedBound {
    fn marker(self) -> CompletedMarker {
        match self {
            ParsedBound::Expr(m) => m,
            ParsedBound::Bound(m) => m,
        }
    }
}

fn expect_range_bound(p: &mut Parser) -> Option<ParsedBound> {
    self::range_bound(p)
        .map(ParsedBound::Bound)
        .or_else(|| self::expr(p).map(ParsedBound::Expr))
        .or_else(|| {
            // not an appropriate primary expr or range bound
            p.error_unexpected()
                .with_category(Expected::Expression)
                .report();
            None
        })
}

fn range_bound(p: &mut Parser) -> Option<CompletedMarker> {
    // '*' ( '-' Expr)?
    if !p.at(TokenKind::Star) {
        return None;
    }

    let m = p.start();
    p.bump();

    if p.eat(TokenKind::Minus) {
        // Expect expr after
        self::expect_expr(p);
    }

    Some(m.complete(p, SyntaxKind::RelativeBound))
}

pub(super) fn expect_expr(p: &mut Parser) -> Option<CompletedMarker> {
    self::expr(p).or_else(|| {
        // not an appropriate primary expr
        p.error_unexpected()
            .with_category(Expected::Expression)
            .report();
        None
    })
}

/// Parses an expression
pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

pub(super) fn expr_list(p: &mut Parser) -> Option<CompletedMarker> {
    // Expr list
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        if expr::expect_expr(p).is_some() {
            while p.eat(TokenKind::Comma) {
                expr::expect_expr(p);
            }
        }
    });

    Some(m.complete(p, SyntaxKind::ExprList))
}

pub(super) fn expect_comptime_expr(p: &mut Parser) -> Option<CompletedMarker> {
    self::comptime_expr(p).or_else(|| {
        // not an appropriate primary expr
        p.error_unexpected()
            .with_category(Expected::Expression)
            .report();
        None
    })
}

pub(super) fn comptime_expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr(p).map(|it| it.precede(p).complete(p, SyntaxKind::CompTimeExpr))
}

pub(super) fn comptime_expr_list(p: &mut Parser) -> Option<CompletedMarker> {
    // CompTimeExpr list
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        if expr::expect_comptime_expr(p).is_some() {
            while p.eat(TokenKind::Comma) {
                expr::expect_comptime_expr(p);
            }
        }
    });

    Some(m.complete(p, SyntaxKind::CompTimeExprList))
}

/// Parses a reference
pub(super) fn reference(p: &mut Parser) -> Option<CompletedMarker> {
    // Exprs:

    // starting:
    // - name_expr
    // - deref_expr
    // - bits_expr
    // - objclass_expr
    // - cheat_expr

    // continuations:
    // - field_expr
    // - arrow_expr
    // - call_expr

    // terminators:
    // - indirect_expr
    expr_binding_power(p, toc_syntax::MIN_REF_BINDING_POWER)
}

fn expect_expr_binding_power(p: &mut Parser, min_binding_power: u8) -> Option<CompletedMarker> {
    expr_binding_power(p, min_binding_power).or_else(|| {
        // report missing expr
        p.error_unexpected()
            .with_category(Expected::Expression)
            .report();
        None
    })
}

fn expr_binding_power(p: &mut Parser, min_binding_power: u8) -> Option<CompletedMarker> {
    let only_primaries = min_binding_power >= toc_syntax::MIN_REF_BINDING_POWER;

    let mut lhs = lhs(p).or_else(|| prefix(p, only_primaries))?;

    loop {
        if p.at(TokenKind::At) {
            lhs = indirect_expr_tail(p, lhs);
        }

        let op = match infix_op(p, min_binding_power) {
            Ok(op) => op,
            Err(NoInfixOp::NotRecovery) => {
                // Tried to eat a not as an infix operator, treat it as if we did eat a valid operator
                InfixOp::NotEqual
            }
            Err(_) => {
                // Not an infix operator, so let the caller decide the outcome
                if p.at_hidden(TokenKind::At) {
                    // can't chain indirect expr tails
                    // outcome has been decided
                    p.error_unexpected()
                        .with_category(Expected::InfixOp)
                        .report();
                } else {
                    // It's probably the end of an expression, so dropped the acquired expected tokens
                    p.reset_expected_tokens();
                }

                break;
            }
        };

        let (left_bind_power, right_bind_power) = op.binding_power();

        if left_bind_power < min_binding_power {
            // stop
            break;
        }

        // nom on operator token
        match op {
            InfixOp::NotIn | InfixOp::NotEqual => {} // don't bump, node already constructed
            InfixOp::Call => {}                      // Will be consumed
            _ => p.bump(),
        }

        let found_rhs = match op {
            InfixOp::Call => {
                // call expr
                let m = lhs.precede(p);

                super::param_list(p);

                lhs = m.complete(p, SyntaxKind::CallExpr);

                // no rhs to miss
                true
            }
            InfixOp::Arrow | InfixOp::Dot => {
                // field or arrow expr
                let m = lhs.precede(p);

                // expect name_ref
                let found_rhs = super::name_ref(p).is_some();
                lhs = m.complete(
                    p,
                    if op == InfixOp::Dot {
                        SyntaxKind::FieldExpr
                    } else {
                        SyntaxKind::ArrowExpr
                    },
                );

                found_rhs
            }
            _ => {
                // wrap inside a binary expr
                let m = lhs.precede(p);
                let found_rhs = expect_expr_binding_power(p, right_bind_power).is_some();
                lhs = m.complete(p, SyntaxKind::BinaryExpr);

                found_rhs
            }
        };

        if !found_rhs {
            // did not find an rhs for the operator, bail out
            break;
        }
    }

    Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    match_token! {
        |p| match {
            TokenKind::Identifier => name_expr(p),
            TokenKind::Self_ => self_expr(p),
            TokenKind::Bits => bits_expr(p),
            TokenKind::ObjectClass => objclass_expr(p),
            TokenKind::Cheat => cheat_expr(p),
            TokenKind::SizeOf => sizeof_expr(p),
            _ => {
                // Try parsing an indirect expr primitive,
                // or otherwise a primary expression
                p.with_extra_recovery(&[TokenKind::At], |p| {
                    ty::ty_primitive(p)
                }).and_then(|cm| {
                    // Found a primitive, check if it's part of an indirect expression
                    if p.at(TokenKind::At) {
                        // `@` will be eaten in the infix parsing loop
                        // don't need to worry about removing it from the
                        // expected tokens list

                        // Give the original ty node
                        Some(cm)
                    } else {
                        // '@' needed to form an indirection expr
                        let m = cm.precede(p);
                        p.error_unexpected()
                            .with_marker(m)
                            .report();
                        None
                    }
                }).or_else(|| {
                    // Parse a primary expr

                    // Note: This does allow exprs like "1" to be "called",
                    // but those cases are reported as errors during typeck
                    primary(p)
                })
            }
        }
    }
}

fn primary(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        TokenKind::IntLiteral,
        TokenKind::RadixLiteral,
        TokenKind::RealLiteral,
        TokenKind::StringLiteral,
        TokenKind::CharLiteral,
        TokenKind::True,
        TokenKind::False => literal_expr(p),
        TokenKind::LeftParen => paren_expr(p),
        TokenKind::Init => init_expr(p),
        TokenKind::Nil => nil_expr(p),
        _ => None
    })
}

fn prefix(p: &mut Parser, only_primaries: bool) -> Option<CompletedMarker> {
    let op = prefix_op(p, only_primaries)?;

    // parse a prefix op
    let m = p.start();
    let ((), right_binding_power) = op.binding_power();

    // nom on operator token
    p.bump();

    expect_expr_binding_power(p, right_binding_power);

    let kind = match op {
        PrefixOp::Deref => SyntaxKind::DerefExpr,
        PrefixOp::NatCheat => SyntaxKind::NatCheatExpr,
        _ => SyntaxKind::UnaryExpr,
    };

    Some(m.complete(p, kind))
}

fn name_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Identifier));

    let m = p.start();
    super::name_ref(p);
    Some(m.complete(p, SyntaxKind::NameExpr))
}

fn self_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Self_));

    let m = p.start();
    p.bump(); // nom 'self'
    Some(m.complete(p, SyntaxKind::SelfExpr))
}

fn bits_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Bits));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::LeftParen) {
        // Eat a parameter list
        super::param_list(p);
    } else {
        p.error_unexpected().report();
    }

    Some(m.complete(p, SyntaxKind::BitsExpr))
}

fn objclass_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::ObjectClass));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::LeftParen) {
        // Eat a parameter list
        super::param_list(p);
    } else {
        p.error_unexpected().report();
    }

    Some(m.complete(p, SyntaxKind::ObjClassExpr))
}

fn cheat_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Cheat));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        p.expect_punct(TokenKind::LeftParen);

        p.with_extra_recovery(&[TokenKind::Comma], |p| {
            // parse a ty
            ty::ty(p);
        });
        p.expect_punct(TokenKind::Comma);

        p.with_extra_recovery(&[TokenKind::Colon], |p| {
            self::expect_expr(p);
        });

        ty::size_spec(p);
    });
    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::CheatExpr))
}

fn sizeof_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::SizeOf));

    let m = p.start();
    p.bump();

    p.expect_punct(TokenKind::LeftParen);
    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        ty::ty_structured(p).or_else(|| expect_expr(p))
    });
    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::SizeOfExpr))
}

fn paren_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::LeftParen));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::RightParen], expect_expr);

    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::ParenExpr))
}

fn literal_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(
        p.at(TokenKind::IntLiteral)
            || p.at(TokenKind::RadixLiteral)
            || p.at(TokenKind::RealLiteral)
            || p.at(TokenKind::StringLiteral)
            || p.at(TokenKind::CharLiteral)
            || p.at(TokenKind::True)
            || p.at(TokenKind::False)
    );

    let m = p.start();

    // bump token
    // Token gets automagically transformed into the correct type
    p.bump();

    Some(m.complete(p, SyntaxKind::LiteralExpr))
}

fn init_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Init));

    let m = p.start();

    p.bump();
    p.expect_punct(TokenKind::LeftParen);

    p.with_extra_recovery(&[TokenKind::RightParen, TokenKind::Comma], |p| {
        let m = p.start();

        if expr::expect_comptime_expr(p).is_some() {
            while p.eat(TokenKind::Comma) && !p.at(TokenKind::RightParen) {
                expr::expect_comptime_expr(p);
            }

            // Don't clog up expected tokens
            p.reset_expected_tokens();
        } else {
            // eat optional trailing comma
            p.eat(TokenKind::Comma);
        }

        m.complete(p, SyntaxKind::CompTimeExprList)
    });

    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::InitExpr))
}

fn nil_expr(p: &mut Parser) -> Option<CompletedMarker> {
    // nil ( '(' Reference ')' )?
    debug_assert!(p.at(TokenKind::Nil));

    let m = p.start();
    p.bump();

    if p.eat(TokenKind::LeftParen) {
        p.with_extra_recovery(&[TokenKind::RightParen], |p| {
            expr::expect_expr(p);
        });
        p.expect_punct(TokenKind::RightParen);
    }

    Some(m.complete(p, SyntaxKind::NilExpr))
}

fn indirect_expr_tail(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::At));

    // Postfix indirection tail
    // Only allowed for primary exprs
    let m = lhs.precede(p);
    p.bump();

    p.expect_punct(TokenKind::LeftParen);
    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        self::expr(p);
    });
    p.expect_punct(TokenKind::RightParen);

    m.complete(p, SyntaxKind::IndirectExpr)
}

fn prefix_op(p: &mut Parser, only_primaries: bool) -> Option<PrefixOp> {
    let ref_prefix_ops = match_token!(|p| match {
        TokenKind::Caret => Some(PrefixOp::Deref),
        TokenKind::Pound => Some(PrefixOp::NatCheat),
        _ => None,
    });

    if only_primaries {
        // Stop at the reference prefix ops,
        // since the others can't form reference expressions
        return ref_prefix_ops;
    }

    ref_prefix_ops.or_else(|| {
        Some(match_token!(|p| match {
            TokenKind::Tilde,
            TokenKind::Not => PrefixOp::Not,
            TokenKind::Plus => PrefixOp::Identity,
            TokenKind::Minus => PrefixOp::Negate,
            _ => return None,
        }))
    })
}

fn infix_op(p: &mut Parser, min_infix_power: u8) -> Result<InfixOp, NoInfixOp> {
    let ref_infix_ops = match_token!(|p| match {
        TokenKind::Dot => Ok(InfixOp::Dot),
        TokenKind::Arrow => Ok(InfixOp::Arrow),
        TokenKind::LeftParen => Ok(InfixOp::Call),
        _ => Err(NoInfixOp::LowerPrecedence)
    });

    if min_infix_power >= toc_syntax::MIN_REF_BINDING_POWER {
        // Stop at the reference infix ops,
        // since the others can't form reference expressions
        return ref_infix_ops;
    }

    ref_infix_ops.or_else(|_| {
        Ok(match_token! {
            |p| match {
                TokenKind::Imply => InfixOp::Imply,
                TokenKind::Pipe,
                TokenKind::Or => InfixOp::Or,
                TokenKind::Ampersand,
                TokenKind::And => InfixOp::And,
                TokenKind::Less => InfixOp::Less,
                TokenKind::Greater => InfixOp::Greater,
                TokenKind::Equ => InfixOp::Equal,
                TokenKind::LessEqu => InfixOp::LessEq,
                TokenKind::GreaterEqu => InfixOp::GreaterEq,
                TokenKind::In => InfixOp::In,
                TokenKind::Tilde,
                TokenKind::Not => maybe_composite_not_op(p, min_infix_power)?,
                TokenKind::Plus => InfixOp::Add,
                TokenKind::Minus => InfixOp::Sub,
                TokenKind::Xor => InfixOp::Xor,
                TokenKind::Star => InfixOp::Mul,
                TokenKind::Slash => InfixOp::RealDiv,
                TokenKind::Div => InfixOp::Div,
                TokenKind::Mod => InfixOp::Mod,
                TokenKind::Rem => InfixOp::Rem,
                TokenKind::Shl => InfixOp::Shl,
                TokenKind::Shr => InfixOp::Shr,
                TokenKind::Exp => InfixOp::Exp,
                _ => {
                    // Not an infix operator
                    return Err(NoInfixOp::NotInfixOp);
                }
            }
        })
    })
}

fn maybe_composite_not_op(p: &mut Parser, min_infix_power: u8) -> Result<InfixOp, NoInfixOp> {
    debug_assert!(p.at(TokenKind::Not) || p.at(TokenKind::Tilde));

    if InfixOp::NotIn.binding_power().0 < min_infix_power {
        // can't parse it right now, leave it for later
        return Err(NoInfixOp::LowerPrecedence);
    }

    let m = p.start();

    // bump to only leave "in" and "=" in expected token set
    p.bump(); // bump "not" or "~"

    match_token!(|p| match {
        TokenKind::In => {
            p.bump(); // consume "in"
            m.complete(p, SyntaxKind::NotIn); // make NotIn node
            Ok(InfixOp::NotIn)
        }
        TokenKind::Equ => {
            p.bump(); // consume "="
            m.complete(p, SyntaxKind::NotEq); // make NotEq node
            Ok(InfixOp::NotEqual)
        }
        _ => {
            // "not" / "~" is not allowed as an infix operator
            // don't eat the next token since it may be part of the rhs
            p.error_unexpected()
                .with_marker(m)
                .dont_eat()
                .report();

            Err(NoInfixOp::NotRecovery)
        }
    })
}

enum NoInfixOp {
    /// Encountered a lower precedence op
    LowerPrecedence,
    /// `~` or `not` as infix operator recovery
    NotRecovery,
    /// Not an infix op
    NotInfixOp,
}
