//! Grammar parsing
#![allow(clippy::unnecessary_wraps)] // Simplifies top level grammar entry points
// both of these due to macro expansion
#![allow(clippy::needless_bool)]
#![allow(unused_braces)]

// All parsed items

// stmts:
// - const_decl
// - var_decl
// - type_decl
// - bind_decl
// - procedure_decl
// - function_decl
// - process_decl
// - external_decl
// - forward_decl
// - deferred_decl
// - body_decl
// - module_decl
// - class_decl
// - monitor_decl

// - include_glob
// - assign_stmt
// - open_stmt
// - close_stmt
// - put_stmt
// - get_stmt
// - read_stmt
// - write_stmt
// - seek_stmt
// - tell_stmt
// - for_stmt
// - loop_stmt
// - exit_stmt
// - if_stmt
// - case_stmt
// - block_stmt
// - invariant_stmt
// - assert_stmt
// - call_stmt
// - return_stmt
// - result_stmt
// - new_stmt
// - free_stmt
// - tag_stmt
// - fork_stmt
// - signal_stmt
// - pause_stmt
// - wait_stmt
// - quit_stmt
// - break_stmt
// - checkedness_stmt
// - pre_stmt
// - init_stmt
// - post_stmt
// - handler_stmt
// - inherit_stmt
// - implement_stmt
// - implement_by_stmt
// - import_stmt
// - export_stmt

// exprs:
// - include_glob
// - init_expr
// - literal_expr
// - call_expr
// - dot_expr
// - name_expr
// - deref_expr
// - binary_expr
// - unary_expr
// - paren_expr
// - indirect_expr
// - bits_expr
// - range_expr (used in string subscripting)
// - cheat_expr
// - objclass_expr
// - self_expr
// - nil_expr
// - sizeof_expr

// types
// - include_glob
// - primitive_type (including sized variants)
// - name_type
// - array_type
// - enum_type
// - pointer_type
// - range_type
// - set_type
// - function_type / subprog_type
// - record_type
// - union_type
// - collection_type
// - condition_type

/// Helper for matching tokens.
///
/// Prepending `[hidden]` before a token to match
/// (e.g. `[hidden] TokenKind::Elseif`) ensures that the token
/// is not added to the list of expected tokens.
macro_rules! match_token {
    (|$parser:ident| match { $($inner:tt)* }) => {
        { __match_token!(@init ($parser) { $($inner)* }) }
    };
}

macro_rules! __match_token {
    (@init ($p:ident) { $($other:tt)* }) => {
        __match_token!(@arm ($p) { $($other)* })
    };

    // Specialize over groups of
    // ...seven at the tail
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $tok3:expr => $action3:expr,
        $tok4:expr => $action4:expr,
        $tok5:expr => $action5:expr,
        $tok6:expr => $action6:expr,
        $tok7:expr => $action7:expr,
        _ => $otherwise:expr,
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else if __match_token!(@at ($p) $tok3) {
            $action3
        } else if __match_token!(@at ($p) $tok4) {
            $action4
        } else if __match_token!(@at ($p) $tok5) {
            $action5
        } else if __match_token!(@at ($p) $tok6) {
            $action6
        } else if __match_token!(@at ($p) $tok7) {
            $action7
        } else {
            $otherwise
        }
    };

    // ...five at the tail
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $tok3:expr => $action3:expr,
        $tok4:expr => $action4:expr,
        $tok5:expr => $action5:expr,
        _ => $otherwise:expr,
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else if __match_token!(@at ($p) $tok3) {
            $action3
        } else if __match_token!(@at ($p) $tok4) {
            $action4
        } else if __match_token!(@at ($p) $tok5) {
            $action5
        } else {
            $otherwise
        }
    };

    // ...three at the tail
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $tok3:expr => $action3:expr,
        _ => $otherwise:expr,
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else if __match_token!(@at ($p) $tok3) {
            $action3
        } else {
            $otherwise
        }
    };

    // ...two at the tail
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        _ => $otherwise:expr,
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else {
            $otherwise
        }
    };

    // ...seven
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $tok3:expr => $action3:expr,
        $tok4:expr => $action4:expr,
        $tok5:expr => $action5:expr,
        $tok6:expr => $action6:expr,
        $tok7:expr => $action7:expr,
        $($other:tt)*
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else if __match_token!(@at ($p) $tok3) {
            $action3
        } else if __match_token!(@at ($p) $tok4) {
            $action4
        } else if __match_token!(@at ($p) $tok5) {
            $action5
        } else if __match_token!(@at ($p) $tok6) {
            $action6
        } else if __match_token!(@at ($p) $tok7) {
            $action7
        } else {
            __match_token!(@arm ($p) { $($other)* })
        }
    };

    // ...five
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $tok3:expr => $action3:expr,
        $tok4:expr => $action4:expr,
        $tok5:expr => $action5:expr,
        $($other:tt)*
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else if __match_token!(@at ($p) $tok3) {
            $action3
        } else if __match_token!(@at ($p) $tok4) {
            $action4
        } else if __match_token!(@at ($p) $tok5) {
            $action5
        } else {
            __match_token!(@arm ($p) { $($other)* })
        }
    };

    // ...three
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $tok3:expr => $action3:expr,
        $($other:tt)*
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else if __match_token!(@at ($p) $tok3) {
            $action3
        } else {
            __match_token!(@arm ($p) { $($other)* })
        }
    };

    // ...two
    (@arm ($p:ident) {
        $tok1:expr => $action1:expr,
        $tok2:expr => $action2:expr,
        $($other:tt)*
    }) => {
        if __match_token!(@at ($p) $tok1) {
            $action1
        } else if __match_token!(@at ($p) $tok2) {
            $action2
        } else {
            __match_token!(@arm ($p) { $($other)* })
        }
    };

    // Final tail arm
    (@arm ($p:ident) { _ => $otherwise:expr $(,)? }) => {
        { $otherwise }
    };

    // Default fall-through arm
    (@arm ($p:ident) { $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [false]
            { $($other)* }
        )
    };

    // Carry through tail `=>`
    (@expand ($p:ident) [$current:expr] { $tok:expr => $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current || __match_token!(@at ($p) $tok)]
            { => $($other)* }
        )
    };
    (@expand ($p:ident) [$current:expr] { [hidden] $tok:expr => $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current || __match_token!(@at_hidden ($p) $tok)]
            { => $($other)* }
        )
    };
    // Or, expand token list

    // Match a trailing group of 3
    (@expand ($p:ident) [$current:expr] { $tok_a:expr, $tok_b:expr, $tok_c:expr => $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current
                || __match_token!(@at ($p) $tok_a)
                || __match_token!(@at ($p) $tok_b)
                || __match_token!(@at ($p) $tok_c)
            ]
            { => $($other)* }
        )
    };

    // Match groups of two
    (@expand ($p:ident) [$current:expr] { $tok_a:expr, $tok_b:expr, $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current || __match_token!(@at ($p) $tok_a) || __match_token!(@at ($p) $tok_b)]
            { $($other)* }
        )
    };

    // Single arms
    (@expand ($p:ident) [$current:expr] { $tok:expr, $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current || __match_token!(@at ($p) $tok)]
            { $($other)* }
        )
    };
    (@expand ($p:ident) [$current:expr] { [hidden] $tok:expr, $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current || __match_token!(@at_hidden ($p) $tok)]
            { $($other)* }
        )
    };

    // Emit action
    (@expand ($p:ident) [$match_toks:expr] { => $action:expr, $($other:tt)* }) => {
        if $match_toks {
            $action
        } else {
            __match_token!(@arm ($p) { $($other)* })
        }

    };
    (@expand ($p:ident) [$match_toks:expr] { => $action:block $($other:tt)* }) => {
        if $match_toks {
            $action
        } else {
            __match_token!(@arm ($p) { $($other)* })
        }

    };
    // Conditions
    (@at ($p:ident) $tok:expr) => {
        $p.at($tok)
    };
    (@at_hidden ($p:ident) $tok:expr) => {
        $p.at_hidden($tok)
    };
}

mod expr;
mod preproc;
mod stmt;
mod ty;

use crate::parser::{Expected, Parser, marker::CompletedMarker};
use toc_scanner::token::TokenKind;
use toc_syntax::SyntaxKind;

pub(crate) fn source(p: &mut Parser) -> CompletedMarker {
    let source = p.start();
    let is_unit = p.hidden_eat(TokenKind::Unit);

    if !is_unit && p.at_hidden(TokenKind::Import) {
        // embed inside source node
        stmt::import_stmt(p);
    }

    {
        let m = p.start();
        while !p.at_end() {
            stmt::stmt(p);
        }
        m.complete(p, SyntaxKind::StmtList);
    }

    source.complete(p, SyntaxKind::Source)
}

fn name(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Identifier) {
        let m = p.start();
        p.bump();
        Some(m.complete(p, SyntaxKind::Name))
    } else {
        // not found
        p.error_unexpected().report();
        None
    }
}

/// eager eating can only be done if it doesn't leak out to other statements,
/// even when there's missing chars
fn name_list_of(
    p: &mut Parser,
    list_kind: SyntaxKind,
    item_kind: SyntaxKind,
    eagerly_eat: bool,
) -> Option<CompletedMarker> {
    let mut eaten_anything = false;
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        let mut ate_comma = false;

        while p.at_hidden(TokenKind::Identifier) || p.at_hidden(TokenKind::Comma) {
            let m = p.start();
            let ate_name = name(p).is_some();
            ate_comma = p.eat(TokenKind::Comma);
            m.complete(p, item_kind);

            eaten_anything |= ate_name || ate_comma;

            if !eagerly_eat && !ate_comma {
                break;
            } else if !ate_comma && p.at_hidden(TokenKind::Identifier) {
                // missing separating comma
                p.error_unexpected().dont_eat().report();
            }
        }

        if !eaten_anything {
            // need to add identifer to the expeceted tokens list,
            // as we only passed through hidden eats
            p.at(TokenKind::Identifier);
            p.error_unexpected().report();
        } else if ate_comma && !p.at(TokenKind::Identifier) {
            p.error_unexpected().report();
        }
    });

    Some(m.complete(p, list_kind)).filter(|_| eaten_anything)
}

fn name_ref(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Identifier) {
        let m = p.start();
        p.bump();
        Some(m.complete(p, SyntaxKind::NameRef))
    } else {
        // not found
        p.error_unexpected().report();
        None
    }
}

/// ParamList ( `'(' Param ( ',' Param )* ')'` )
fn param_list(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::LeftParen));

    let m = p.start();
    p.bump();

    if !p.at(TokenKind::RightParen) {
        if let Some((_, true)) = param(p) {
            loop {
                match param(p) {
                    Some((_, true)) => {}      // parsed param, expecting more
                    Some((_, false)) => break, // parsed param, end of list
                    None => {
                        // missing next param
                        break;
                    }
                }
            }
        }
    }

    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::ParamList))
}

fn param(p: &mut Parser) -> Option<(CompletedMarker, bool)> {
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma, TokenKind::RightParen], |p| {
        expr::expect_param_expr(p);
    });

    // bump ',' onto param
    let found_comma = p.eat(TokenKind::Comma);

    Some((m.complete(p, SyntaxKind::Param), found_comma))
}

fn param_spec(p: &mut Parser) -> Option<CompletedMarker> {
    // ParamSpec: '(' ParamDecl ( ',' ParamDecl )* ')'
    let m = p.start();

    p.expect_punct(TokenKind::LeftParen);
    p.with_extra_recovery(&[TokenKind::RightParen, TokenKind::Comma], |p| {
        if !p.at(TokenKind::RightParen) && self::param_decl(p).is_some() {
            while p.eat(TokenKind::Comma) {
                self::param_decl(p);
            }
        }
    });
    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::ParamSpec))
}

fn param_decl(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        TokenKind::Function,
        TokenKind::Procedure => ty::subprog_type(p),
        TokenKind::Var,
        TokenKind::Register,
        [hidden] TokenKind::Colon, // For recovery when there's no leading names
        TokenKind::Identifier => ty::constvar_param(p),
        _ => {
            // not a thing
            None
        }
    })
}

fn attr_pervasive(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        [hidden] TokenKind::Pervasive,
        [hidden] TokenKind::Star => {
            let m = p.start();
            p.bump();
            Some(m.complete(p, SyntaxKind::PervasiveAttr))
        }
        _ => None
    })
}

fn attr_unqualified(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        [hidden] TokenKind::Unqualified => {
            let m = p.start();
            p.bump();
            Some(m.complete(p, SyntaxKind::UnqualifiedAttr))
        }
        [hidden] TokenKind::Tilde => {
            // '~' '.'
            let m = p.start();
            p.bump();

            if !p.eat(TokenKind::Dot) {
                p.error_unexpected().with_marker(m).report();
                return None;
            }

            Some(m.complete(p, SyntaxKind::UnqualifiedAttr))
        }
        _ => None
    })
}

macro_rules! make_single_attr {
    ($i:ident, $tk:expr, $kind:expr ) => {
        fn $i(p: &mut Parser) -> Option<CompletedMarker> {
            match_token!(|p| match {
                [hidden] $tk => {
                    let m = p.start();
                    p.bump();
                    Some(m.complete(p, $kind))
                }
                _ => None
            })
        }
    };
}

make_single_attr!(attr_register, TokenKind::Register, SyntaxKind::RegisterAttr);
make_single_attr!(attr_var, TokenKind::Var, SyntaxKind::VarAttr);
make_single_attr!(attr_const, TokenKind::Const, SyntaxKind::ConstAttr);
make_single_attr!(attr_forward, TokenKind::Forward, SyntaxKind::ForwardAttr);
make_single_attr!(attr_opaque, TokenKind::Opaque, SyntaxKind::OpaqueAttr);
make_single_attr!(attr_cheat, TokenKind::Cheat, SyntaxKind::CheatAttr);

fn at_stmt_block_end(p: &mut Parser) -> bool {
    match_token!(|p| match {
        [hidden] TokenKind::End,
        [hidden] TokenKind::EndIf,
        [hidden] TokenKind::EndCase,
        [hidden] TokenKind::EndFor,
        [hidden] TokenKind::EndLoop => true,
        _ => false,
    })
}

#[cfg(test)]
mod test {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn skip_trivia() {
        check(
            "var i := ??? 1 /* skipped */ % skipped too",
            expect![[r#"
                Source@0..42
                  StmtList@0..14
                    ConstVarDecl@0..14
                      KwVar@0..3 "var"
                      Whitespace@3..4 " "
                      ConstVarDeclNameList@4..5
                        ConstVarDeclName@4..5
                          Name@4..5
                            Identifier@4..5 "i"
                      Whitespace@5..6 " "
                      Assign@6..8 ":="
                      Whitespace@8..9 " "
                      Error@9..10 "?"
                      Error@10..11 "?"
                      Error@11..12 "?"
                      Whitespace@12..13 " "
                      LiteralExpr@13..14
                        IntLiteral@13..14 "1"
                  Whitespace@14..15 " "
                  Comment@15..28 "/* skipped */"
                  Whitespace@28..29 " "
                  Comment@29..42 "% skipped too"
                error at 9..10: invalid character
                | error for 9..10: here
                error at 10..11: invalid character
                | error for 10..11: here
                error at 11..12: invalid character
                | error for 11..12: here"#]],
        );
    }

    #[test]
    fn last_non_trivia_token() {
        check(
            "var i := /* some trivia */     % and more trivia",
            expect![[r#"
                Source@0..48
                  StmtList@0..8
                    ConstVarDecl@0..8
                      KwVar@0..3 "var"
                      Whitespace@3..4 " "
                      ConstVarDeclNameList@4..5
                        ConstVarDeclName@4..5
                          Name@4..5
                            Identifier@4..5 "i"
                      Whitespace@5..6 " "
                      Assign@6..8 ":="
                  Whitespace@8..9 " "
                  Comment@9..26 "/* some trivia */"
                  Whitespace@26..31 "     "
                  Comment@31..48 "% and more trivia"
                error at 6..8: unexpected end of file
                | error for 6..8: expected expression after here"#]],
        );
    }
}
