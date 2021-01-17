//! Expression parsing
#[cfg(test)]
mod test;

use super::*;
use toc_syntax::{BinaryOp, SyntaxKind, UnaryOp};

pub(super) fn expect_param_expr(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::All) {
        let m = p.start();
        p.bump();
        return Some(m.complete(p, SyntaxKind::AllItem));
    }

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
        if let Some(..) = expr::expect_expr(p) {
            while p.eat(TokenKind::Comma) {
                expr::expect_expr(p);
            }
        }
    });

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

        let op = if let Some(op) = infix_op(p, min_binding_power) {
            op
        } else {
            // Not an infix operator, so let the caller decide the outcome

            if p.at_hidden(TokenKind::At) {
                // can't chain indirect expr tails
                // outcome has been decided
                p.error_unexpected().report();
            } else {
                // It's probably the end of an expression, so dropped the acquired expected tokens
                p.reset_expected_tokens();
            }

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
            TokenKind::Identifier => { name_expr(p) }
            TokenKind::Self_ => { self_expr(p) }
            TokenKind::Bits => { bits_expr(p) }
            TokenKind::ObjectClass => { objclass_expr(p) }
            TokenKind::Cheat => { cheat_expr(p) }
            TokenKind::SizeOf => { sizeof_expr(p) }
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
                        p.error_unexpected()
                            .with_marker(m)
                            .report();
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
        TokenKind::Nil => { nil_expr(p) }
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
        UnaryOp::Deref => SyntaxKind::DerefExpr,
        UnaryOp::NatCheat => SyntaxKind::NatCheatExpr,
        _ => SyntaxKind::UnaryExpr,
    };

    Some(m.complete(p, kind))
}

fn name_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Identifier));

    let m = p.start();
    super::name(p);
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

        if p.at(TokenKind::Colon) {
            // Eat cheat size spec
            let m = p.start();
            p.bump();

            self::expect_expr(p);

            m.complete(p, SyntaxKind::SizeSpec);
        }
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

    self::expect_expr(p);

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

        if let Some(..) = expr::expect_expr(p) {
            while p.eat(TokenKind::Comma) && !p.at(TokenKind::RightParen) {
                expr::expect_expr(p);
            }

            // Don't clog up expected tokens
            p.reset_expected_tokens();
        } else {
            // eat optional trailing comma
            p.eat(TokenKind::Comma);
        }

        m.complete(p, SyntaxKind::ExprList)
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

fn prefix_op(p: &mut Parser, only_primaries: bool) -> Option<UnaryOp> {
    let ref_prefix_ops = match_token!(|p| match {
        TokenKind::Caret => { Some(UnaryOp::Deref) }
        TokenKind::Pound => { Some(UnaryOp::NatCheat) }
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
            TokenKind::Not => { UnaryOp::Not }
            TokenKind::Plus => { UnaryOp::Identity }
            TokenKind::Minus => { UnaryOp::Negate }
            TokenKind::Pound => { UnaryOp::NatCheat }
            _ => return None,
        }))
    })
}

fn infix_op(p: &mut Parser, min_infix_power: u8) -> Option<BinaryOp> {
    let ref_infix_ops = match_token!(|p| match {
        TokenKind::Dot => { Some(BinaryOp::Dot) },
        TokenKind::Arrow => { Some(BinaryOp::Arrow) },
        TokenKind::LeftParen => { Some(BinaryOp::Call) },
        _ => None,
    });

    if min_infix_power >= toc_syntax::MIN_REF_BINDING_POWER {
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
                TokenKind::Tilde,
                TokenKind::Not => {
                    maybe_composite_not_op(p, min_infix_power)?
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

pub(super) fn maybe_composite_not_op(p: &mut Parser, min_infix_power: u8) -> Option<BinaryOp> {
    debug_assert!(p.at(TokenKind::Not) || p.at(TokenKind::Tilde));

    if BinaryOp::NotIn.binding_power().0 < min_infix_power {
        // can't parse it anyways yet, leave it for later
        return None;
    }

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
            p.error_unexpected()
                .with_marker(m)
                .report();
            return None;
        }
    }))
}
