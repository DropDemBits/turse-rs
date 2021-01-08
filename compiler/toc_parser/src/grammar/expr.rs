//! Expression parsing
#[cfg(test)]
mod test;

use super::*;
use toc_syntax::{BinaryOp, SyntaxKind, UnaryOp};

pub(super) fn expect_expr_or_range_item(p: &mut Parser) -> Option<CompletedMarker> {
    let mut lhs = expect_range_bound(p)?;

    if p.at(TokenKind::Range) {
        let m = lhs.precede(p);
        p.bump();

        expect_range_bound(p);

        lhs = m.complete(p, SyntaxKind::RangeItem);
    }

    Some(lhs)
}

fn expect_range_bound(p: &mut Parser) -> Option<CompletedMarker> {
    self::range_bound(p).or_else(|| self::expr(p)).or_else(|| {
        // not an appropriate primary expr or range bound
        p.error(Expected::Expression);
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
        p.error(Expected::Expression);
        None
    })
}

/// Parses an expression
pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

pub(super) fn expr_list(p: &mut Parser) -> Option<CompletedMarker> {
    // Expr list (optional)
    let m = p.start();

    if let Some(..) = expr::expect_expr(p) {
        while p.eat(TokenKind::Comma) {
            expr::expect_expr(p);
        }
    }

    Some(m.complete(p, SyntaxKind::ExprList))
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

fn expr_binding_power(p: &mut Parser, min_binding_power: u8) -> Option<CompletedMarker> {
    let only_primaries = min_binding_power >= toc_syntax::MIN_REF_BINDING_POWER;

    let mut lhs = lhs(p).or_else(|| if !only_primaries { prefix(p) } else { None })?;

    loop {
        if p.at(TokenKind::At) {
            lhs = indirect_expr_tail(p, lhs);
            // can't chain indirect expr tails, so stop
            break;
        }

        let op = if let Some(op) = infix_op(p, only_primaries) {
            op
        } else {
            // Not an infix operator, so let the caller decide the outcome

            // It's probably the end of an expression, so dropped the acquired expected tokens
            p.reset_expected_tokens();
            break;
        };

        let (left_bind_power, right_bind_power) = op.binding_power();

        if left_bind_power < min_binding_power {
            // stop
            break;
        }

        // nom on operator token
        match op {
            BinaryOp::NotIn | BinaryOp::NotEqual => {} // don't bump, node already constructed
            BinaryOp::Call => {}                       // Will be consumed
            _ => p.bump(),
        }

        let found_rhs = match op {
            BinaryOp::Call => {
                // call expr
                let m = lhs.precede(p);

                super::param_list(p);

                lhs = m.complete(p, SyntaxKind::CallExpr);

                // no rhs to miss
                true
            }
            BinaryOp::Arrow | BinaryOp::Dot => {
                // field or arrow expr
                let m = lhs.precede(p);

                // expect name
                let found_rhs = super::name(p).is_some();
                lhs = m.complete(
                    p,
                    if op == BinaryOp::Dot {
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
                let found_rhs = expr_binding_power(p, right_bind_power)
                    .or_else(|| {
                        // report missing expr
                        p.error(Expected::Expression);
                        None
                    })
                    .is_some();
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
            TokenKind::Identifier => { name_expr(p) }
            TokenKind::Caret => { deref_expr(p) }
            TokenKind::Bits => { bits_expr(p) }
            TokenKind::ObjectClass => { objclass_expr(p) }
            TokenKind::Cheat => { cheat_expr(p) }
            _ => {
                p.with_extra_recovery(&[TokenKind::At], |p| {
                    ty::ty_primitive(p)
                }).and_then(|cm| {
                    if p.at(TokenKind::At) {
                        // Remove '@' from expected list
                        p.reset_expected_tokens();

                        // Give the original ty node
                        Some(cm)
                    } else {
                        // '@' needed to form an indirection expr
                        let m = cm.precede(p);
                        p.error_unexpected_at(m, None);
                        None
                    }
                }).or_else(|| {
                    // Parse a primary expr

                    // Note: This does allow exprs like "1" to be "called",
                    // but those cases should be reported as errors when validating the AST
                    // TODO: Report non-callable expressions as errors
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
        TokenKind::False => { literal_expr(p) }
        TokenKind::LeftParen => { paren_expr(p) }
        TokenKind::Init => { init_expr(p) }
        _ => None
    })
}

fn prefix(p: &mut Parser) -> Option<CompletedMarker> {
    let op = prefix_op(p)?;

    // parse a prefix op
    let m = p.start();
    let ((), right_binding_power) = op.binding_power();

    // nom on operator token
    p.bump();

    expr_binding_power(p, right_binding_power);

    Some(m.complete(p, SyntaxKind::UnaryExpr))
}

fn name_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Identifier));

    // nom name
    let m = p.start();
    super::name(p);
    Some(m.complete(p, SyntaxKind::NameExpr))
}

fn deref_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Caret));

    // parse rhs with the correct binding power
    let ((), right_binding_power) = UnaryOp::Deref.binding_power();

    let m = p.start();

    p.bump(); // nom caret
    expr_binding_power(p, right_binding_power);

    Some(m.complete(p, SyntaxKind::DerefExpr))
}

fn bits_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Bits));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::LeftParen) {
        // Eat a parameter list
        super::param_list(p);
    } else {
        p.error(None);
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
        p.error(None);
    }

    Some(m.complete(p, SyntaxKind::ObjClassExpr))
}

fn cheat_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Cheat));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        p.expect(TokenKind::LeftParen);

        p.with_extra_recovery(&[TokenKind::Comma], |p| {
            // parse a ty
            ty::ty(p);
        });
        p.expect(TokenKind::Comma);

        p.with_extra_recovery(&[TokenKind::Colon], |p| {
            self::expect_expr(p);
        });

        if p.at(TokenKind::Colon) {
            // Eat cheat size spec
            let m = p.start();
            p.bump();

            self::expect_expr(p);

            m.complete(p, SyntaxKind::SizeSpec);
        }
    });
    p.expect(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::CheatExpr))
}

fn paren_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::LeftParen));

    let m = p.start();
    p.bump();

    self::expect_expr(p);

    p.expect(TokenKind::RightParen);

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
    p.expect(TokenKind::LeftParen);

    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        self::expr_list(p);
    });

    p.expect(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::InitExpr))
}

fn indirect_expr_tail(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    debug_assert!(p.at(TokenKind::At));

    // Postfix indirection tail
    // Only allowed for primary exprs
    let m = lhs.precede(p);
    p.bump();

    p.expect(TokenKind::LeftParen);
    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        self::expr(p);
    });
    p.expect(TokenKind::RightParen);

    m.complete(p, SyntaxKind::IndirectExpr)
}

fn prefix_op(p: &mut Parser) -> Option<UnaryOp> {
    Some(match_token!(|p| match {
        TokenKind::Not => { UnaryOp::Not }
        TokenKind::Plus => { UnaryOp::Identity }
        TokenKind::Minus => { UnaryOp::Negate }
        TokenKind::Pound => { UnaryOp::NatCheat }
        _ => return None,
    }))
}

fn infix_op(p: &mut Parser, only_primaries: bool) -> Option<BinaryOp> {
    let ref_infix_ops = match_token!(|p| match {
        TokenKind::Dot => { Some(BinaryOp::Dot) },
        TokenKind::Arrow => { Some(BinaryOp::Arrow) },
        TokenKind::LeftParen => { Some(BinaryOp::Call) },
        _ => {
            None
        },
    });

    if only_primaries {
        // Stop at the reference infix ops,
        // since the others can't form reference expressions
        return ref_infix_ops;
    }

    ref_infix_ops.or_else(|| {
        Some(match_token! {
            |p| match {
                TokenKind::Imply => { BinaryOp::Imply },
                TokenKind::Pipe,
                TokenKind::Or => { BinaryOp::Or }
                TokenKind::Ampersand,
                TokenKind::And => { BinaryOp::And }
                TokenKind::Less => { BinaryOp::Less }
                TokenKind::Greater => { BinaryOp::Greater }
                TokenKind::Equ => { BinaryOp::Equal }
                TokenKind::LessEqu => { BinaryOp::LessEq }
                TokenKind::GreaterEqu => { BinaryOp::GreaterEq }
                TokenKind::In => { BinaryOp::In }
                TokenKind::Not => {
                    maybe_composite_not_op(p)?
                }
                TokenKind::Tilde => {
                    maybe_composite_not_op(p)?
                }
                TokenKind::Plus => { BinaryOp::Add }
                TokenKind::Minus => { BinaryOp::Sub }
                TokenKind::Xor => { BinaryOp::Xor }
                TokenKind::Star => { BinaryOp::Mul }
                TokenKind::Slash => { BinaryOp::RealDiv }
                TokenKind::Div => { BinaryOp::Div }
                TokenKind::Mod => { BinaryOp::Mod }
                TokenKind::Rem => { BinaryOp::Rem }
                TokenKind::Shl => { BinaryOp::Shl }
                TokenKind::Shr => { BinaryOp::Shr },
                TokenKind::Exp => { BinaryOp::Exp },
                _ => {
                    // Not an infix operator
                    return None;
                }
            }
        })
    })
}

pub(super) fn maybe_composite_not_op(p: &mut Parser) -> Option<BinaryOp> {
    debug_assert!(p.at(TokenKind::Not) || p.at(TokenKind::Tilde));

    let m = p.start();

    // bump to only leave "in" and "=" in expected token set
    p.bump(); // bump "not" or "~"

    Some(match_token!(|p| match {
        TokenKind::In => {
            p.bump(); // consume "in"
            m.complete(p, SyntaxKind::NotIn); // make NotIn node
            BinaryOp::NotIn
        },
        TokenKind::Equ => {
            p.bump(); // consume "="
            m.complete(p, SyntaxKind::NotEq); // make NotEq node
            BinaryOp::NotEqual
        },
        _ => {
            // "not" / "~" is not allowed as an infix operator
            p.error_unexpected_at(m, None);
            return None;
        }
    }))
}
