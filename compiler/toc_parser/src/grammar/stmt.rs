//! Statement grammars
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

#[cfg(test)]
mod test {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_var_decl_inferred_ty() {
        check(
            "var a := 1",
            expect![[r#"
            Root@0..10
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                Identifier@4..5 "a"
                Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..9 " "
                LiteralExpr@9..10
                  IntLiteral@9..10 "1""#]],
        );
    }

    #[test]
    fn parse_const_decl_inferred_ty() {
        check(
            "const a := 1",
            expect![[r#"
            Root@0..12
              ConstVarDecl@0..12
                KwConst@0..5 "const"
                Whitespace@5..6 " "
                Identifier@6..7 "a"
                Whitespace@7..8 " "
                Assign@8..10 ":="
                Whitespace@10..11 " "
                LiteralExpr@11..12
                  IntLiteral@11..12 "1""#]],
        );
    }

    #[test]
    fn parse_several_stmts() {
        check(
            "const a := 1\na",
            expect![[r#"
                Root@0..14
                  ConstVarDecl@0..13
                    KwConst@0..5 "const"
                    Whitespace@5..6 " "
                    Identifier@6..7 "a"
                    Whitespace@7..8 " "
                    Assign@8..10 ":="
                    Whitespace@10..11 " "
                    LiteralExpr@11..13
                      IntLiteral@11..12 "1"
                      Whitespace@12..13 "\n"
                  RefExpr@13..14
                    Identifier@13..14 "a""#]],
        );
    }

    #[test]
    fn recover_on_var() {
        check(
            "const a := \nvar b := 1",
            expect![[r#"
                Root@0..22
                  ConstVarDecl@0..12
                    KwConst@0..5 "const"
                    Whitespace@5..6 " "
                    Identifier@6..7 "a"
                    Whitespace@7..8 " "
                    Assign@8..10 ":="
                    Whitespace@10..12 " \n"
                  ConstVarDecl@12..22
                    KwVar@12..15 "var"
                    Whitespace@15..16 " "
                    Identifier@16..17 "b"
                    Whitespace@17..18 " "
                    Assign@18..20 ":="
                    Whitespace@20..21 " "
                    LiteralExpr@21..22
                      IntLiteral@21..22 "1"
                error at 12..15: expected identifier, int literal, explicit int literal or real literal, but found ’var’"#]],
        );
    }

    #[test]
    fn recover_on_const() {
        check(
            "var a := \nconst b := 1",
            expect![[r#"
                Root@0..22
                  ConstVarDecl@0..10
                    KwVar@0..3 "var"
                    Whitespace@3..4 " "
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                    Assign@6..8 ":="
                    Whitespace@8..10 " \n"
                  ConstVarDecl@10..22
                    KwConst@10..15 "const"
                    Whitespace@15..16 " "
                    Identifier@16..17 "b"
                    Whitespace@17..18 " "
                    Assign@18..20 ":="
                    Whitespace@20..21 " "
                    LiteralExpr@21..22
                      IntLiteral@21..22 "1"
                error at 10..15: expected identifier, int literal, explicit int literal or real literal, but found ’const’"#]],
        );
    }
}
