//! Statement grammars
#[cfg(test)]
mod test;

use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    match_token! {
        |p| match {
            TokenKind::Var => { const_var_decl(p) }
            TokenKind::Const => { const_var_decl(p) }
            _ => expr::expr(p),
        }
    }
}

fn const_var_decl(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Var) || p.at(TokenKind::Const));
    let mut require_initializer = p.at(TokenKind::Const);

    let m = p.start();
    p.bump(); // `var` or `const`

    super::name_list(p);

    if p.eat(TokenKind::Colon) {
        // parse type
        p.with_extra_recovery(&[TokenKind::Assign], |p| {
            if ty::ty(p).is_none() {
                // not a type
                p.error();
            }
        });
    } else {
        // initializer is required if type is absent
        require_initializer = true;
    }

    if (require_initializer && p.expect(TokenKind::Assign)) || p.eat(TokenKind::Assign) {
        expr::expr(p);
    }

    Some(m.complete(p, SyntaxKind::ConstVarDecl))
}
