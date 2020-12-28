//! Statement grammars
#[cfg(test)]
mod test;

use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    match_token! {
        |p| match {
            TokenKind::Var => { const_var_decl(p) }
            TokenKind::Const => { const_var_decl(p) }
            _ => expr::expr(p)
        }
    }
}

fn const_var_decl(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Var) || p.at(TokenKind::Const));
    let _as_const = p.at(TokenKind::Const); // to check if initializer is required

    let m = p.start();
    p.bump(); // `var` or `const`

    p.expect(TokenKind::Identifier);
    p.expect(TokenKind::Assign);

    expr::expr(p);

    Some(m.complete(p, SyntaxKind::ConstVarDecl))
}
