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
        __match_token!(@init ($parser) { $($inner)* })
    };
}

macro_rules! __match_token {
    (@init ($p:ident) { $($other:tt)* }) => {
        __match_token!(@expand ($p) [false] { $($other)* })
    };
    (@expand ($p:ident) [$_unused:expr] { _ => $otherwise:expr $(,)? }) => {
        { $otherwise }
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

    // Match groups of two
    (@expand ($p:ident) [$current:expr] { $tok_a:expr, $tok_b:expr, $($other:tt)* }) => {
        __match_token!(
            @expand ($p)
            [$current || __match_token!(@at ($p) $tok_a) || __match_token!(@at ($p) $tok_b)]
            { $($other)* }
        )
    };
    // Match group of 3, with terminating tail
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
    // Emit action
    (@expand ($p:ident) [$match_toks:expr] { => $action:expr, $($other:tt)* }) => {
        if $match_toks {
            $action
        } else {
            __match_token!(@expand ($p) [false] { $($other)* })
        }

    };
    (@expand ($p:ident) [$match_toks:expr] { => $action:block $($other:tt)* }) => {
        if $match_toks {
            $action
        } else {
            __match_token!(@expand ($p) [false] { $($other)* })
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

use crate::parser::marker::CompletedMarker;
use crate::parser::Expected;
use crate::parser::Parser;
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

pub(self) fn name(p: &mut Parser) -> Option<CompletedMarker> {
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

pub(self) fn name_list(p: &mut Parser) -> Option<CompletedMarker> {
    let mut parsed_any = false;
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        parsed_any |= self::name(p).is_some();

        while p.at(TokenKind::Comma) {
            // did parse something
            parsed_any = true;

            p.bump();

            self::name(p);
        }
    });

    Some(m.complete(p, SyntaxKind::NameList)).filter(|_| parsed_any)
}

/// ParamList ( `'(' Param ( ',' Param )* ')'` )
pub(self) fn param_list(p: &mut Parser) -> Option<CompletedMarker> {
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

pub(self) fn param(p: &mut Parser) -> Option<(CompletedMarker, bool)> {
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma, TokenKind::RightParen], |p| {
        expr::expect_param_expr(p);
    });

    // bump ',' onto param
    let found_comma = p.eat(TokenKind::Comma);

    Some((m.complete(p, SyntaxKind::Param), found_comma))
}

pub(self) fn param_spec(p: &mut Parser) -> Option<CompletedMarker> {
    // ParamSpec: '(' ParamDecl ( ',' ParamDecl )* ')'
    let m = p.start();

    p.expect_punct(TokenKind::LeftParen);
    p.with_extra_recovery(&[TokenKind::RightParen, TokenKind::Comma], |p| {
        if !p.at(TokenKind::RightParen) {
            if let Some(..) = self::param_decl(p) {
                while p.eat(TokenKind::Comma) {
                    self::param_decl(p);
                }
            }
        }
    });
    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::ParamSpec))
}

pub(self) fn param_decl(p: &mut Parser) -> Option<CompletedMarker> {
    match_token!(|p| match {
        TokenKind::Function,
        TokenKind::Procedure => ty::subprog_type(p),
        TokenKind::Var,
        TokenKind::Register,
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
                      NameList@4..5
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
                error in file FileId(1) at 9..10: invalid character
                | error in file FileId(1) for 9..10: here
                error in file FileId(1) at 10..11: invalid character
                | error in file FileId(1) for 10..11: here
                error in file FileId(1) at 11..12: invalid character
                | error in file FileId(1) for 11..12: here"#]],
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
                      NameList@4..5
                        Name@4..5
                          Identifier@4..5 "i"
                      Whitespace@5..6 " "
                      Assign@6..8 ":="
                  Whitespace@8..9 " "
                  Comment@9..26 "/* some trivia */"
                  Whitespace@26..31 "     "
                  Comment@31..48 "% and more trivia"
                error in file FileId(1) at 6..8: unexpected end of file
                | error in file FileId(1) for 6..8: expected expression after here"#]],
        );
    }
}
