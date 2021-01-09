//! Statement grammars
#[cfg(test)]
mod test;

use expr::{expect_expr, expr_list};

use super::*;

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    let m = match_token! {
        |p| match {
            TokenKind::Var => { const_var_decl(p) }
            TokenKind::Const => { const_var_decl(p) }
            TokenKind::Type => { type_decl(p) }
            TokenKind::Bind => { bind_decl(p) }
            TokenKind::Procedure => { procedure_decl(p) }
            TokenKind::Function => { function_decl(p) }
            // forward_decl
            // deferred_decl
            // body_decl
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
            TokenKind::For =>{ for_stmt(p) }
            TokenKind::Loop =>{ loop_stmt(p) }
            TokenKind::Exit =>{ exit_stmt(p) }
            TokenKind::If => { if_stmt(p) }
            TokenKind::Elif,
            TokenKind::Elsif,
            TokenKind::Elseif => { elseif_stmt(p, true) } // recovery parse
            TokenKind::Else => { else_stmt(p, true) } // recovery parse
            TokenKind::Case =>{ case_stmt(p) }
            TokenKind::Begin => { block_stmt(p) }
            TokenKind::Invariant => { stmt_with_expr(p, TokenKind::Invariant, SyntaxKind::InvariantStmt) }
            TokenKind::Assert => { stmt_with_expr(p, TokenKind::Assert, SyntaxKind::AssertStmt) }
            TokenKind::Return => { stmt_only_kw(p, TokenKind::Return, SyntaxKind::ReturnStmt) }
            TokenKind::Result_ => { stmt_with_expr(p, TokenKind::Result_, SyntaxKind::ResultStmt) }
            TokenKind::New => { heap_stmt(p, SyntaxKind::NewStmt) }
            TokenKind::Free => { heap_stmt(p, SyntaxKind::FreeStmt) }
            TokenKind::Tag => { tag_stmt(p) }
            TokenKind::Fork => { fork_stmt(p) }
            TokenKind::Signal => { stmt_with_expr(p, TokenKind::Signal, SyntaxKind::SignalStmt) }
            TokenKind::Pause => { stmt_with_expr(p, TokenKind::Pause, SyntaxKind::PauseStmt) }
            TokenKind::Quit => { quit_stmt(p) }
            TokenKind::Checked => { stmt_only_kw(p, TokenKind::Checked, SyntaxKind::CheckednessStmt) }
            TokenKind::Unchecked => { stmt_only_kw(p, TokenKind::Unchecked, SyntaxKind::CheckednessStmt) }
            TokenKind::Pre => { stmt_with_expr(p, TokenKind::Pre, SyntaxKind::PreStmt) }
            TokenKind::Init => { init_stmt(p) }
            TokenKind::Post => { stmt_with_expr(p, TokenKind::Post, SyntaxKind::PostStmt) }
            TokenKind::Handler => { handler_stmt(p) }
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
                p.error_unexpected()
                    .with_category(Expected::Statement)
                    .report();
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
    if p.at_hidden(TokenKind::Equ) {
        // Simple assignment, but mistyped `=` instead of `:=`
        p.warn_alias("’:=’");

        let m = p.start();
        p.bump(); // bump `=`
        return Some(m.complete(p, SyntaxKind::AsnOp));
    }

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
            p.error_unexpected().with_marker(m).report();
            return None;
        }

        // bump equ
        p.bump();
        Some(m.complete(p, SyntaxKind::AsnOp))
    }
}

// Decls //

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
    if p.at_hidden(TokenKind::Equ) {
        p.warn_alias("’:=’");
        p.bump(); // bump `=`

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

fn bind_decl(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Bind));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        bind_item(p);
        while p.eat(TokenKind::Comma) {
            bind_item(p);
        }
    });

    Some(m.complete(p, SyntaxKind::BindDecl))
}

fn bind_item(p: &mut Parser) -> Option<CompletedMarker> {
    // 'var'? 'register'? Name to Expr

    let m = p.start();
    // Optional attrs
    attr_var(p);
    attr_register(p);

    p.with_extra_recovery(&[TokenKind::To], |p| {
        super::name(p);
    });
    p.expect(TokenKind::To);

    expr::expect_expr(p);

    Some(m.complete(p, SyntaxKind::BindItem))
}

fn procedure_decl(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Procedure));
    let m = p.start();

    proc_header(p);
    stmt_list(p, None);
    eat_end_group(p, TokenKind::Identifier, None);

    Some(m.complete(p, SyntaxKind::ProcDecl))
}

fn function_decl(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Function));
    let m = p.start();

    fcn_header(p);
    stmt_list(p, None);
    eat_end_group(p, TokenKind::Identifier, None);

    Some(m.complete(p, SyntaxKind::FcnDecl))
}

fn proc_header(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Procedure));

    let m = p.start();
    p.bump();

    attr_pervasive(p);

    p.with_extra_recovery(&[TokenKind::LeftParen, TokenKind::Colon], |p| {
        super::name(p);

        if p.at(TokenKind::LeftParen) {
            super::param_spec(p);
        }
    });

    device_spec(p);

    Some(m.complete(p, SyntaxKind::ProcHeader))
}

fn fcn_header(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Function));

    let m = p.start();
    p.bump();

    attr_pervasive(p);

    p.with_extra_recovery(&[TokenKind::LeftParen, TokenKind::Colon], |p| {
        super::name(p);

        if p.at(TokenKind::LeftParen) {
            super::param_spec(p);
        }
    });

    fcn_result(p);

    Some(m.complete(p, SyntaxKind::FcnHeader))
}

fn device_spec(p: &mut Parser) -> Option<CompletedMarker> {
    if !p.at(TokenKind::Colon) {
        return None; // device_spec is completely optional
    }

    let m = p.start();
    p.bump();
    expr::expect_expr(p);

    Some(m.complete(p, SyntaxKind::DeviceSpec))
}

fn fcn_result(p: &mut Parser) -> Option<CompletedMarker> {
    if !p.at(TokenKind::Identifier) && !p.at(TokenKind::Colon) {
        // not optional
        p.error_unexpected().report();
        return None;
    }

    let m = p.start();

    if p.at(TokenKind::Identifier) {
        super::name(p);
    }

    p.expect(TokenKind::Colon);

    ty::ty(p);

    Some(m.complete(p, SyntaxKind::FcnResult))
}

// Stmts //

fn for_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::For));

    let m = p.start();
    p.bump();

    p.hidden_eat(TokenKind::Decreasing);

    p.with_extra_recovery(&[TokenKind::Colon], |p| {
        super::name(p);
    });

    // For-range
    // left bound
    p.with_extra_recovery(&[TokenKind::Range], |p| {
        p.expect(TokenKind::Colon);
        expr::expect_expr(p);
    });
    // right bound
    p.expect(TokenKind::Range);
    expr::expect_expr(p);

    // optional step by expr
    if p.at(TokenKind::By) {
        let m = p.start();
        p.bump();
        expr::expect_expr(p);
        m.complete(p, SyntaxKind::StepBy);
    }

    self::stmt_list(p, None);

    eat_end_group(p, TokenKind::For, Some(TokenKind::EndFor));
    Some(m.complete(p, SyntaxKind::ForStmt))
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

    if p.at(TokenKind::Elseif) || p.at(TokenKind::Elif) {
        // `elsif` is blessed version
        p.warn_alias("’elsif’");
        p.bump();
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

fn case_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Case));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::Label], |p| {
        p.with_extra_recovery(&[TokenKind::Of], |p| {
            expr::expect_expr(p);
        });
        p.expect(TokenKind::Of);
    });

    // Parse case arms
    loop {
        if case_arm(p).is_none() {
            break;
        }
    }

    eat_end_group(p, TokenKind::Case, Some(TokenKind::EndCase));
    Some(m.complete(p, SyntaxKind::CaseStmt))
}

fn case_arm(p: &mut Parser) -> Option<CompletedMarker> {
    if !p.at(TokenKind::Label) {
        return None;
    }

    let m = p.start();
    p.bump(); // nom `label`

    if !p.at(TokenKind::Colon) {
        p.with_extra_recovery(&[TokenKind::Colon], |p| {
            expr::expr_list(p);
        })
    }

    p.expect(TokenKind::Colon);

    // Nom on stmts
    self::stmt_list(p, Some(&[TokenKind::Label]));

    Some(m.complete(p, SyntaxKind::CaseArm))
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

fn heap_stmt(p: &mut Parser, syntax_kind: SyntaxKind) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::New) || p.at(TokenKind::Free));

    let m = p.start();
    p.bump();

    // Unbounded amount of entries for the heap stmts
    expr_list(p);

    Some(m.complete(p, syntax_kind))
}

fn tag_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    // 'tag' Reference ',' Expr
    debug_assert!(p.at(TokenKind::Tag));
    let m = p.start();
    p.bump();

    // union_ref
    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        expr::expect_expr(p);
    });
    p.expect(TokenKind::Comma);

    // tag_val
    expr::expect_expr(p);

    Some(m.complete(p, SyntaxKind::TagStmt))
}

fn fork_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    // 'fork' CallExpr ( ':' Reference )? ( ',' Expr )? ( ',' Reference )?
    debug_assert!(p.at(TokenKind::Fork));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::Colon], |p| {
        // process_id (contains the full call expr, including params)
        expr::expect_expr(p);
    });

    if p.eat(TokenKind::Colon) {
        p.with_extra_recovery(&[TokenKind::Comma], |p| {
            // status
            let m = p.start();
            expr::expect_expr(p);
            m.complete(p, SyntaxKind::ForkStatus);

            // stack_size
            if p.eat(TokenKind::Comma) {
                let m = p.start();
                expr::expect_expr(p);
                m.complete(p, SyntaxKind::StackSize);
            }

            // process_ref
            if p.eat(TokenKind::Comma) {
                let m = p.start();
                expr::expect_expr(p);
                m.complete(p, SyntaxKind::ProcessDesc);
            }
        });
    }

    Some(m.complete(p, SyntaxKind::ForkStmt))
}

fn quit_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    // 'quit' cause:( '>' | '<' )? code:( ':' Expr )?
    debug_assert!(p.at(TokenKind::Quit));

    let m = p.start();
    p.bump();

    // quit_cause
    match_token!(|p| match {
        TokenKind::Less,
        TokenKind::Greater => {
            let m = p.start();
            p.bump();
            m.complete(p, SyntaxKind::QuitCause);
        }
        _ => {}
    });

    if p.eat(TokenKind::Colon) {
        // quit_code
        expect_expr(p);
    }

    Some(m.complete(p, SyntaxKind::QuitStmt))
}

fn init_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    // 'init' InitVar (',' InitVar)*
    debug_assert!(p.at(TokenKind::Init));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        init_var(p);

        while p.eat(TokenKind::Comma) {
            init_var(p);
        }
    });

    Some(m.complete(p, SyntaxKind::InitStmt))
}

fn init_var(p: &mut Parser) -> Option<CompletedMarker> {
    // Name ':=' Expr
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Assign, TokenKind::Equ], |p| {
        super::name(p);
    });

    if p.at_hidden(TokenKind::Equ) {
        // blessed version is `:=`
        p.warn_alias(":=");
        p.bump();
    } else {
        p.expect(TokenKind::Assign);
    }

    expr::expect_expr(p);

    Some(m.complete(p, SyntaxKind::InitVar))
}

fn handler_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    // 'handler' '(' Name ')' StmtList 'end' 'handler'
    debug_assert!(p.at(TokenKind::Handler));

    let m = p.start();
    p.bump();

    p.with_extra_recovery(
        &[TokenKind::Identifier, TokenKind::RightParen, TokenKind::End],
        |p| {
            p.expect(TokenKind::LeftParen);
            super::name(p);
            p.expect(TokenKind::RightParen);
        },
    );

    stmt_list(p, None);

    eat_end_group(p, TokenKind::Handler, None);

    Some(m.complete(p, SyntaxKind::HandlerStmt))
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

    if combined.map(|kind| p.at(kind)).unwrap_or(false) {
        let tail_text = match tail {
            TokenKind::If => "if",
            TokenKind::Case => "case",
            TokenKind::For => "for",
            TokenKind::Loop => "loop",
            _ => unreachable!("not a combined end"),
        };

        p.warn_alias(&format!("’end {}’", tail_text));
        p.bump();
    } else {
        p.expect(TokenKind::End);
        p.expect(tail);
    }

    m.complete(p, SyntaxKind::EndGroup);
}

fn attr_pervasive(p: &mut Parser) {
    // ???: How to deal with pervasive attr variants?
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

fn attr_var(p: &mut Parser) {
    if !p.eat(TokenKind::Var) {
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
