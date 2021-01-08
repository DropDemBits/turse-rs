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
            // bind_decl
            // procedure_decl
            // functionn_decl
            // forward_decl
            // deferred_decl
            // module_decl
            // class_decl
            // monitor_decl

            // include_stmt
            // open_stmt
            // close_stmt
            // put_stmt
            // get_stmt
            // read_stmt
            // write_stmt
            // seek_stmt
            // tell_stmt
            // for_stmt
            TokenKind::Loop =>{ loop_stmt(p) }
            TokenKind::Exit =>{ exit_stmt(p) }
            TokenKind::If => { if_stmt(p) }
            TokenKind::Elif,
            TokenKind::Elsif,
            TokenKind::Elseif => { elseif_stmt(p, true) } // recovery parse
            TokenKind::Else => { else_stmt(p, true) } // recovery parse
            // case_stmt
            TokenKind::Begin => { block_stmt(p) }
            TokenKind::Invariant => { stmt_with_expr(p, TokenKind::Invariant, SyntaxKind::InvariantStmt) }
            TokenKind::Assert => { stmt_with_expr(p, TokenKind::Assert, SyntaxKind::AssertStmt) }
            TokenKind::Return => { stmt_only_kw(p, TokenKind::Return, SyntaxKind::ReturnStmt) }
            TokenKind::Result_ => { stmt_with_expr(p, TokenKind::Result_, SyntaxKind::ResultStmt) }
            // new_stmt
            // free_stmt
            // tag_stmt
            // fork_stmt
            TokenKind::Signal => { stmt_with_expr(p, TokenKind::Signal, SyntaxKind::SignalStmt) }
            TokenKind::Pause => { stmt_with_expr(p, TokenKind::Pause, SyntaxKind::PauseStmt) }
            // quit_stmt
            TokenKind::Checked => { stmt_only_kw(p, TokenKind::Checked, SyntaxKind::CheckednessStmt) }
            TokenKind::Unchecked => { stmt_only_kw(p, TokenKind::Unchecked, SyntaxKind::CheckednessStmt) }
            // pre_stmt
            // init_stmt
            // post_stmt
            // handler_stmt
            // implement_stmt
            // implement_by_stmt
            // import_stmt
            // export_stmt
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

fn stmt_with_expr(
    p: &mut Parser,
    expect_at: TokenKind,
    make_kind: SyntaxKind,
) -> Option<CompletedMarker> {
    debug_assert!(p.at(expect_at));

    let m = p.start();
    p.bump();

    expr::expect_expr(p);

    Some(m.complete(p, make_kind))
}

fn stmt_only_kw(
    p: &mut Parser,
    expect_at: TokenKind,
    make_kind: SyntaxKind,
) -> Option<CompletedMarker> {
    debug_assert!(p.at(expect_at));

    let m = p.start();
    p.bump();

    Some(m.complete(p, make_kind))
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
    if p.at(TokenKind::Equ) {
        p.bump(); // bump `=`

        // TODO: Warn about mistake token
        expr::expect_expr(p);
    } else if (require_initializer && p.expect(TokenKind::Assign)) || p.eat(TokenKind::Assign) {
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

fn loop_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Loop));

    let m = p.start();
    p.bump();

    self::stmt_list(p, None);

    eat_end_group(p, TokenKind::Loop, Some(TokenKind::EndLoop));
    Some(m.complete(p, SyntaxKind::LoopStmt))
}

fn exit_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Exit));

    let m = p.start();
    p.bump();

    if p.eat(TokenKind::When) {
        expr::expect_expr(p);
    }

    Some(m.complete(p, SyntaxKind::ExitStmt))
}

fn if_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::If));

    let m = p.start();
    p.bump(); // bump `if`

    if_body(p);

    // Eat `end if` or `endif`
    eat_end_group(p, TokenKind::If, Some(TokenKind::EndIf));

    Some(m.complete(p, SyntaxKind::IfStmt))
}

fn if_body(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::EndIf, TokenKind::End, TokenKind::If], |p| {
        // condition
        p.with_extra_recovery(&[TokenKind::Then], |p| {
            expr::expect_expr(p);
            p.expect(TokenKind::Then);
        });

        // true_branch
        stmt_list(
            p,
            Some(&[
                TokenKind::Else,
                TokenKind::Elseif,
                TokenKind::Elsif,
                TokenKind::Elif,
            ]),
        );

        // false_branch
        match_token!(|p| match {
            TokenKind::Else => { else_stmt(p, false); }
            TokenKind::Elseif, TokenKind::Elsif, TokenKind::Elif => { elseif_stmt(p, false); }
            _ => { /* no false branch */ }
        });
    });

    Some(m.complete(p, SyntaxKind::IfBody))
}

fn elseif_stmt(p: &mut Parser, eat_tail: bool) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Elseif) || p.at(TokenKind::Elsif) || p.at(TokenKind::Elif));

    let m = p.start();

    if p.eat(TokenKind::Elseif) || p.eat(TokenKind::Elif) {
        // `elsif` is blessed version
        // TODO: Warn of mistake token
    } else {
        // nom `elsif`
        p.bump();
    }

    if_body(p);

    if eat_tail {
        // Eat `end if` or `endif`
        eat_end_group(p, TokenKind::If, Some(TokenKind::EndIf));
    }

    Some(m.complete(p, SyntaxKind::ElseifStmt))
}

fn else_stmt(p: &mut Parser, eat_tail: bool) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Else));

    let m = p.start();
    p.bump();

    let stop_on_kind = &[TokenKind::EndIf, TokenKind::End, TokenKind::If];

    p.with_extra_recovery(stop_on_kind, |p| self::stmt_list(p, Some(&[TokenKind::If])));

    if eat_tail {
        // Eat `end if` or `endif`
        eat_end_group(p, TokenKind::If, Some(TokenKind::EndIf));
    }

    Some(m.complete(p, SyntaxKind::ElseStmt))
}

fn block_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Begin));

    let m = p.start(); // BlockStmt
    p.bump();

    self::stmt_list(p, None);

    // end group
    let m_end = p.start();
    p.expect(TokenKind::End);
    m_end.complete(p, SyntaxKind::EndGroup);

    Some(m.complete(p, SyntaxKind::BlockStmt))
}

/// Parses stmts until the first `end` is reached, or until any `TokenKind` in
/// `excluding` is reached
fn stmt_list(p: &mut Parser, excluding: Option<&[TokenKind]>) -> Option<CompletedMarker> {
    let m = p.start();

    let at_excluded_set = |p: &mut Parser| {
        if let Some(excluded) = excluding {
            excluded.iter().any(|kind| p.at(*kind))
        } else {
            false
        }
    };

    p.with_extra_recovery(&[TokenKind::End], |p| {
        while !p.at_end() && !at_stmt_block_end(p) && !at_excluded_set(p) {
            stmt::stmt(p);
        }
    });

    Some(m.complete(p, SyntaxKind::StmtList))
}

/// Eats the end group with corresponding `tail` token, or eats the combined variant
fn eat_end_group(p: &mut Parser, tail: TokenKind, combined: Option<TokenKind>) {
    let m = p.start();

    if combined.map(|kind| p.eat(kind)).unwrap_or(false) {
        // TODO: Warn of mistake token
    } else {
        p.expect(TokenKind::End);
        p.expect(tail);
    }

    m.complete(p, SyntaxKind::EndGroup);
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

fn at_stmt_block_end(p: &mut Parser) -> bool {
    match_token!(|p| match {
        TokenKind::End,
        TokenKind::EndIf,
        TokenKind::EndCase,
        TokenKind::EndFor,
        TokenKind::EndLoop => {
            // hide the checks
            p.reset_expected_tokens();
            true
        }
        _ => { false }
    })
}
