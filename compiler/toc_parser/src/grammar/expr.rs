//! Expression parsing
#[cfg(test)]
mod test;

use super::*;
use toc_syntax::{BinaryOp, SyntaxKind, UnaryOp};

/// Parses an expression
pub(super) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0, false)
}

/// Parses a reference
pub(super) fn reference(p: &mut Parser) -> Option<CompletedMarker> {
    // Exprs:

    // starting:
    // - name_expr
    // - deref_expr
    // x bits_expr

    // continuations:
    // x indirect_expr
    // - field_expr
    // - arrow_expr
    // x call_expr
    expr_binding_power(p, toc_syntax::MIN_REF_BINDING_POWER, true)
}

fn expr_binding_power(
    p: &mut Parser,
    min_binding_power: u8,
    only_references: bool,
) -> Option<CompletedMarker> {
    let mut lhs = lhs(p, only_references)?;

    loop {
        let op = if let Some(op) = infix_op(p, only_references) {
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
            _ => p.bump(),
        }

        let found_rhs = match op {
            BinaryOp::Call => {
                // call expr
                let m = lhs.precede(p);

                super::param_list(p);

                p.expect(TokenKind::RightParen);
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
                let found_rhs = expr_binding_power(p, right_bind_power, only_references).is_some();
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

fn lhs(p: &mut Parser, only_references: bool) -> Option<CompletedMarker> {
    match_token! {
        |p| match {
            TokenKind::Identifier => { name_expr(p) }
            TokenKind::Caret => { deref_expr(p) }
            TokenKind::Bits => { todo!() }
            // indirection stuff
            _ => {
                if !only_references {
                    primary(p)
                } else {
                    // only accepting references (safe to bail out)
                    None
                }
            }
        }
    }
}

fn primary(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        TokenKind::IntLiteral => { literal_expr(p) }
        TokenKind::RadixLiteral => { literal_expr(p) }
        TokenKind::RealLiteral => { literal_expr(p) }
        TokenKind::StringLiteral => { literal_expr(p) }
        TokenKind::CharLiteral => { literal_expr(p) }
        TokenKind::True => { literal_expr(p) }
        TokenKind::False => { literal_expr(p) }
        TokenKind::LeftParen => { paren_expr(p) }
        TokenKind::Init => { init_expr(p) }
        _ => {
            prefix(p).or_else(|| {
                // not an appropriate primary expr
                p.error(Expected::Expression);
                None
            })
        }
    })
}

fn prefix(p: &mut Parser) -> Option<CompletedMarker> {
    let op = prefix_op(p)?;

    // parse a prefix op
    let m = p.start();
    let ((), right_binding_power) = op.binding_power();

    // nom on operator token
    p.bump();

    expr_binding_power(p, right_binding_power, false);

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
    expr_binding_power(p, right_binding_power, false);

    Some(m.complete(p, SyntaxKind::DerefExpr))
}

fn paren_expr(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::LeftParen));

    let m = p.start();

    p.bump();
    expr_binding_power(p, 0, false);

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

    // TODO: Validate literal is actually valid
    // TODO: Report invalid literals

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
        expr(p);

        while p.eat(TokenKind::Comma) {
            expr(p);
        }
    });

    p.expect(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::InitExpr))
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

fn infix_op(p: &mut Parser, only_reference: bool) -> Option<BinaryOp> {
    let ref_infix_ops = match_token!(|p| match {
        TokenKind::Dot => { Some(BinaryOp::Dot) },
        TokenKind::Arrow => { Some(BinaryOp::Arrow) },
        TokenKind::LeftParen => { Some(BinaryOp::Call) },
        _ => {
            None
        },
    });

    if only_reference {
        // Stop at the reference infix ops
        return ref_infix_ops;
    }

    ref_infix_ops.or_else(|| {
        Some(match_token! {
            |p| match {
                TokenKind::Imply => { BinaryOp::Imply },
                TokenKind::Or => { BinaryOp::Or }
                TokenKind::Pipe => { BinaryOp::Or }
                TokenKind::And => { BinaryOp::And }
                TokenKind::Ampersand => { BinaryOp::And }
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
