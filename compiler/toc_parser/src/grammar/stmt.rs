//! Statement grammars
#[cfg(test)]
mod test;

use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    let m = match_token! {
        |p| match {
            TokenKind::Var => { const_var_decl(p) }
            TokenKind::Const => { const_var_decl(p) }
            TokenKind::Type => { type_decl(p) }
            TokenKind::Begin => { block_stmt(p) }
            _ => expr::reference(p).and_then(|m| {
                let m = m.precede(p);
                // check if there's an asn nearby
                if parse_asn_op(p).is_some() {
                    // parse an assign stmt
                    expr::expect_expr(p);

                    Some(m.complete(p,SyntaxKind::AssignStmt))
                } else {
                    // plop as a call stmt
                    Some(m.complete(p,SyntaxKind::CallStmt))
                }
            }).or_else(|| {
                // report as expecting a statement
                p.error(Expected::Statement);
                None
            }),
        }
    };

    // Eat optional semicolon (don't let it show up in the expected kinds)
    p.hidden_eat(TokenKind::Semicolon);

    m
}

fn parse_asn_op(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Equ) {
        // Simple assignment, but mistyped `=` instead of `:=`
        // TODO: Warn about mistakes like this

        let m = p.start();
        p.bump(); // bump `=`
        return Some(m.complete(p, SyntaxKind::AsnOp));
    }

    // Reset expected tokens to drop `=`
    p.reset_expected_tokens();

    if p.at(TokenKind::Assign) {
        // Simple assignment
        let m = p.start();
        p.bump(); // bump `:=`
        Some(m.complete(p, SyntaxKind::AsnOp))
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
            return None;
        }

        let m = p.start();

        // bump operator
        p.bump();

        if !p.at(TokenKind::Equ) {
            // not a valid asn op, missing equ
            // wrap inside of an error node
            p.error_unexpected_at(m, None);
            return None;
        }

        // bump equ
        p.bump();
        Some(m.complete(p, SyntaxKind::AsnOp))
    }
}

fn const_var_decl(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.at(TokenKind::Var) || p.at(TokenKind::Const));
    let mut require_initializer = p.at(TokenKind::Const);

    let m = p.start();
    p.bump(); // `var` or `const`

    // optional attributes
    attr_pervasive(p);
    // note when lowering: register attr not allowed in top-level blocks
    // (i.e. main, module, class, monitor, monitor class)
    attr_register(p);

    super::name_list(p);

    if p.eat(TokenKind::Colon) {
        // parse type
        p.with_extra_recovery(&[TokenKind::Assign], |p| {
            ty::ty(p);
        });
    } else {
        // initializer is required if type is absent
        require_initializer = true;
    }

    // note when validating: if array is init-sized, then it should require 'init'
    // if type is implied, then init is not allowed
    // refining error: for const, could say that initialzer is required
    if (require_initializer && p.expect(TokenKind::Assign)) || p.eat(TokenKind::Assign) {
        expr::expect_expr(p);
    }

    Some(m.complete(p, SyntaxKind::ConstVarDecl))
}

fn type_decl(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Type));

    let m = p.start();
    p.bump();

    attr_pervasive(p);

    super::name(p);

    p.with_extra_recovery(&[TokenKind::Forward], |p| {
        p.expect(TokenKind::Colon);
    });

    if !p.eat(TokenKind::Forward) {
        // parse a type (it's not a forward!)
        ty::ty(p);
    }

    Some(m.complete(p, SyntaxKind::TypeDecl))
}

fn block_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Begin));

    let m = p.start(); // BlockStmt
    p.bump();

    self::stmt_list(p);

    // end group
    let m_end = p.start();
    p.expect(TokenKind::End);
    m_end.complete(p, SyntaxKind::EndGroup);

    Some(m.complete(p, SyntaxKind::BlockStmt))
}

/// Parses stmts until the first `end` is reached
fn stmt_list(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::End], |p| {
        while !p.at(TokenKind::End) {
            stmt::stmt(p);
        }
    });

    Some(m.complete(p, SyntaxKind::StmtList))
}

fn attr_pervasive(p: &mut Parser) {
    match_token!(|p| match {
        TokenKind::Pervasive => { p.bump() }
        TokenKind::Star => { p.bump() }
        _ => {
            // dont clog up the expected tokens with the attributes
            p.reset_expected_tokens();
        }
    });
}

fn attr_register(p: &mut Parser) {
    if !p.eat(TokenKind::Register) {
        // dont clog up the expected tokens with the attributes
        p.reset_expected_tokens();
    }
}
