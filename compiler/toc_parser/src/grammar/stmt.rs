//! Statement grammars
#[cfg(test)]
mod test;

use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    match_token! {
        |p| match {
            TokenKind::Var => { const_var_decl(p) }
            TokenKind::Const => { const_var_decl(p) }
            _ => expr::reference(p).and_then(|m| {
                let m = m.precede(p);
                // check if there's an asn nearby
                if parse_asn_op(p).is_some() {
                    // parse an assign stmt
                    expr::expr(p);

                    Some(m.complete(p,SyntaxKind::AssignStmt))
                } else {
                    // plop as a call stmt
                    Some(m.complete(p,SyntaxKind::CallStmt))
                }
            }),
        }
    }
}

fn parse_asn_op(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    if p.eat(TokenKind::Equ) {
        // Simple assignment, but mistyped `=` instead of `:=`
        // TODO: Warn about mistakes like this
        return Some(m.complete(p, SyntaxKind::AsnOp));
    }

    // Reset expected tokens to drop `=`
    p.reset_expected_tokens();

    if p.eat(TokenKind::Assign) {
        // Simple assignment
    } else {
        // Compound assignment
        let valid_asn_op = match_token!(|p| match {
            TokenKind::Imply => { true }
            TokenKind::Or => { true }
            TokenKind::Pipe => { true }
            TokenKind::And => { true }
            TokenKind::Ampersand => { true }
            TokenKind::Plus => { true }
            TokenKind::Minus => { true }
            TokenKind::Xor => { true }
            TokenKind::Star => { true }
            TokenKind::Slash => { true }
            TokenKind::Div => { true }
            TokenKind::Mod => { true }
            TokenKind::Rem => { true }
            TokenKind::Shl => { true }
            TokenKind::Shr => { true }
            TokenKind::Exp => { true }
            _ => false,
        });

        if !valid_asn_op {
            // abandon parsing as compound op
            m.forget(p);
            return None;
        }

        // bump operator
        p.bump();

        if !p.at(TokenKind::Equ) {
            // not a valid asn op, missing equ
            // wrap inside of an error node
            p.error_unexpected_at(m);
            return None;
        }

        // bump equ
        p.bump();
    }

    Some(m.complete(p, SyntaxKind::AsnOp))
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
