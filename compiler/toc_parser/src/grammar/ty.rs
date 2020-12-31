//! Type grammars
#[cfg(test)]
mod test;

use super::*;

pub(super) fn ty(p: &mut Parser) -> Option<CompletedMarker> {
    ty_primitive(p).or_else(|| {
        p.error(Expected::Type);
        None
    })
}

pub(super) fn ty_primitive(p: &mut Parser) -> Option<CompletedMarker> {
    // TODO: Add 64 bit types (int8, nat8, long int, long nat)
    match_token!(|p| match {
        TokenKind::Addressint,
        TokenKind::Boolean,
        TokenKind::Int, TokenKind::Int1, TokenKind::Int2, TokenKind::Int4,
        TokenKind::Nat, TokenKind::Nat1, TokenKind::Nat2, TokenKind::Nat4,
        TokenKind::Real, TokenKind::Real4, TokenKind::Real8 => { prim_type(p) }
        TokenKind::Char => { prim_charseq_type(p, TokenKind::Char) }
        TokenKind::String_ => { prim_charseq_type(p, TokenKind::String_) }
        _ => None // Not a primitive type
    })
}

fn prim_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(
        p.at(TokenKind::Addressint)
            || p.at(TokenKind::Boolean)
            || p.at(TokenKind::Int)
            || p.at(TokenKind::Int1)
            || p.at(TokenKind::Int2)
            || p.at(TokenKind::Int4)
            || p.at(TokenKind::Nat)
            || p.at(TokenKind::Nat1)
            || p.at(TokenKind::Nat2)
            || p.at(TokenKind::Nat4)
            || p.at(TokenKind::Real)
            || p.at(TokenKind::Real4)
            || p.at(TokenKind::Real8)
    );

    let m = p.start();
    p.bump();
    Some(m.complete(p, SyntaxKind::PrimType))
}

fn prim_charseq_type(p: &mut Parser, prim_kind: TokenKind) -> Option<CompletedMarker> {
    debug_assert!(p.at(prim_kind));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::LeftParen) {
        // make it sized!
        let kind = match prim_kind {
            TokenKind::Char => SyntaxKind::SizedCharType,
            TokenKind::String_ => SyntaxKind::SizedStringType,
            _ => unreachable!(),
        };

        p.bump();

        p.with_extra_recovery(&[TokenKind::RightParen], |p| {
            let m = p.start();

            if !p.eat(TokenKind::Star) {
                // if not dyn sized, parse an expr
                expr::expr(p);
            }

            m.complete(p, SyntaxKind::SeqLength);
        });

        p.expect(TokenKind::RightParen);

        Some(m.complete(p, kind))
    } else {
        // basic unsized type
        Some(m.complete(p, prim_kind.into()))
    }
}
