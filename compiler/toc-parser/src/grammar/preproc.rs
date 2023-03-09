//! Preprocessor parsing
#[cfg(test)]
mod test;

use toc_syntax::{InfixOp, PrefixOp};

use super::*;

/// Preprocessor parsing, in stmt position
///
/// Only preprocessor conditionals and 'include' are allowed
pub(super) fn stmt_preproc(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        TokenKind::PreprocIf => if_preproc(p),
        TokenKind::PreprocElseIf,
        TokenKind::PreprocElsIf => elseif_preproc(p, true), // recovery
        TokenKind::PreprocElse => else_preproc(p, true), // recovery
        TokenKind::PreprocEnd,
        TokenKind::PreprocEndIf => endif_preproc(p), // recovery
        TokenKind::Include => include_preproc(p),
        _ => None
    })
    .map(|cm| cm.precede(p).complete(p, SyntaxKind::PreprocGlob))
}

fn include_preproc(p: &mut Parser) -> Option<CompletedMarker> {
    // 'include' LiteralExprz (only 'string_literal' is allowed)
    debug_assert!(p.at(TokenKind::Include));

    let m = p.start();
    p.bump();

    // Encapsulate path inside of a LiteralExpr (reuses string parsing code)
    if p.at(TokenKind::StringLiteral) {
        let m = p.start();
        p.bump();
        m.complete(p, SyntaxKind::LiteralExpr);
    } else {
        p.error_unexpected().report();
    }

    Some(m.complete(p, SyntaxKind::PPInclude))
}

fn if_preproc(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::PreprocIf));

    let m = p.start();
    p.bump();

    if_body(p);

    endif_preproc(p);

    Some(m.complete(p, SyntaxKind::PPIf))
}

fn elseif_preproc(p: &mut Parser, eat_tail: bool) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::PreprocElseIf) || p.at(TokenKind::PreprocElsIf));

    let m = p.start();
    if p.at(TokenKind::PreprocElseIf) {
        p.warn_alias("`#elsif`");
    }
    p.bump();

    if_body(p);

    if eat_tail {
        endif_preproc(p);
    }

    Some(m.complete(p, SyntaxKind::PPElseif))
}

fn if_body(p: &mut Parser) {
    // condition
    p.with_extra_recovery(&[TokenKind::Then], |p| {
        preproc_expr(p);
    });

    p.expect_punct(TokenKind::Then);

    stmt_glob(p, true);

    // Eat other bit
    match_token!(|p| match {
        TokenKind::PreprocElseIf,
        TokenKind::PreprocElsIf => elseif_preproc(p, false),
        TokenKind::PreprocElse => else_preproc(p, false),
        _ => { None } // done
    });
}

fn else_preproc(p: &mut Parser, eat_tail: bool) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::PreprocElse));

    let m = p.start();
    p.bump();

    stmt_glob(p, false);

    if eat_tail {
        endif_preproc(p);
    }

    Some(m.complete(p, SyntaxKind::PPElse))
}

fn endif_preproc(p: &mut Parser) -> Option<CompletedMarker> {
    if !p.at(TokenKind::PreprocEnd) && !p.at(TokenKind::PreprocEndIf) {
        // required portion
        p.error_unexpected().report();
        return None;
    }

    let m = p.start();
    if p.at(TokenKind::PreprocEndIf) {
        p.warn_alias("`#end if`");
        p.bump();
    } else {
        p.bump(); // nom '#end'
        p.expect(TokenKind::If);
    }

    Some(m.complete(p, SyntaxKind::PPEndIf))
}

fn stmt_glob(p: &mut Parser, continue_chain: bool) -> Option<CompletedMarker> {
    let stop_at: &[_] = if continue_chain {
        &[
            TokenKind::PreprocElseIf,
            TokenKind::PreprocElsIf,
            TokenKind::PreprocElse,
            TokenKind::PreprocEnd,
            TokenKind::PreprocEndIf,
        ]
    } else {
        &[TokenKind::PreprocEnd, TokenKind::PreprocEndIf]
    };

    let at_stopping_token = |p: &mut Parser| stop_at.iter().any(|tk| p.at_hidden(*tk));

    // nom all things
    let m = p.start();
    while !at_stopping_token(p) && !p.at_end() {
        stmt::stmt(p);
    }
    Some(m.complete(p, SyntaxKind::PPTokenBody))
}

fn preproc_expr(p: &mut Parser) -> Option<CompletedMarker> {
    preproc_expr_binding_power(p, 0)
}

/// already handles missing expr reporting
fn preproc_expr_binding_power(p: &mut Parser, min_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = lhs(p).or_else(|| {
        // report missing expr
        p.error_unexpected()
            .with_category(Expected::PreprocCondition)
            .report();
        None
    })?;

    loop {
        let Some(op) = infix_op(p) else {
            // probably the end of the expr
            p.reset_expected_tokens();
            break;
        };

        let (left_bp, right_bp) = op.binding_power();

        if left_bp < min_binding_power {
            // stop
            break;
        }

        // nom operator
        p.bump();

        let m = lhs.precede(p);
        let found_rhs = preproc_expr_binding_power(p, right_bp).is_some();
        lhs = m.complete(p, SyntaxKind::PPBinaryExpr);

        if !found_rhs {
            // no rhs for operator, bail
            break;
        }
    }

    Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        TokenKind::Identifier => {
            // wrap in name node
            let m = p.start();
            p.bump();
            let m = m.complete(p, SyntaxKind::Name).precede(p);
            Some(m.complete(p, SyntaxKind::PPNameExpr))
        }
        TokenKind::LeftParen => {
            // match paren expr
            let m = p.start();
            p.bump();

            p.with_extra_recovery(&[TokenKind::RightParen], |p| {
                preproc_expr(p);
            });

            p.expect_punct(TokenKind::RightParen);
            Some(m.complete(p, SyntaxKind::PPParenExpr))
        }
        _ => prefix(p),
    })
}

fn prefix(p: &mut Parser) -> Option<CompletedMarker> {
    let op = prefix_op(p)?;

    let m = p.start();
    let ((), right_bp) = op.binding_power();

    p.bump();

    preproc_expr_binding_power(p, right_bp);

    Some(m.complete(p, SyntaxKind::PPUnaryExpr))
}

fn prefix_op(p: &mut Parser) -> Option<PrefixOp> {
    match_token!(|p| match {
        TokenKind::Not,
        TokenKind::Tilde => Some(PrefixOp::Not),
        _ => None
    })
}

fn infix_op(p: &mut Parser) -> Option<InfixOp> {
    match_token!(|p| match {
        TokenKind::And,
        TokenKind::Ampersand => Some(InfixOp::And),
        TokenKind::Or,
        TokenKind::Pipe => Some(InfixOp::Or),
        _ => None
    })
}
