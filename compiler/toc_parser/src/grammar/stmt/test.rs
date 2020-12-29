//! Tests for statements
use crate::check;
use expect_test::expect;

#[test]
fn report_not_a_stmt() {
    check(
        "pervasive",
        expect![[r##"
            Root@0..9
              Error@0..9
                KwPervasive@0..9 "pervasive"
            error at 0..9: expected ’var’, ’const’, identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’pervasive’"##]],
    );
}

#[test]
fn recover_just_assign() {
    check(
        ":=",
        expect![[r##"
        Root@0..2
          Error@0..2
            Assign@0..2 ":="
        error at 0..2: expected ’var’, ’const’, identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’:=’"##]],
    )
}

#[test]
fn recover_just_var() {
    check(
        "var",
        expect![[r#"
            Root@0..3
              ConstVarDecl@0..3
                KwVar@0..3 "var"
                NameList@3..3
            error at 0..3: expected identifier
            error at 0..3: expected ’,’, ’:’ or ’:=’"#]],
    )
}

#[test]
fn parse_var_decl() {
    check(
        "var a : int := 1",
        expect![[r#"
            Root@0..16
              ConstVarDecl@0..16
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Colon@6..7 ":"
                Whitespace@7..8 " "
                PrimType@8..12
                  KwInt@8..11 "int"
                  Whitespace@11..12 " "
                Assign@12..14 ":="
                Whitespace@14..15 " "
                LiteralExpr@15..16
                  IntLiteral@15..16 "1""#]],
    )
}

#[test]
fn parse_const_decl() {
    check(
        "const a : int := 1",
        expect![[r#"
            Root@0..18
              ConstVarDecl@0..18
                KwConst@0..5 "const"
                Whitespace@5..6 " "
                NameList@6..8
                  Name@6..8
                    Identifier@6..7 "a"
                    Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                PrimType@10..14
                  KwInt@10..13 "int"
                  Whitespace@13..14 " "
                Assign@14..16 ":="
                Whitespace@16..17 " "
                LiteralExpr@17..18
                  IntLiteral@17..18 "1""#]],
    )
}

#[test]
fn parse_var_decl_no_init() {
    check("var a : int", expect![[r#"
        Root@0..11
          ConstVarDecl@0..11
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "a"
                Whitespace@5..6 " "
            Colon@6..7 ":"
            Whitespace@7..8 " "
            PrimType@8..11
              KwInt@8..11 "int""#]])
}

#[test]
fn recover_const_decl_no_init() {
    check("const a : int", expect![[r#"
        Root@0..13
          ConstVarDecl@0..13
            KwConst@0..5 "const"
            Whitespace@5..6 " "
            NameList@6..8
              Name@6..8
                Identifier@6..7 "a"
                Whitespace@7..8 " "
            Colon@8..9 ":"
            Whitespace@9..10 " "
            PrimType@10..13
              KwInt@10..13 "int"
        error at 10..13: expected ’:=’"#]])
}

#[test]
fn parse_var_decl_multiple_names() {
    check("var a, b, c := 1", expect![[r#"
        Root@0..16
          ConstVarDecl@0..16
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..12
              Name@4..5
                Identifier@4..5 "a"
              Comma@5..6 ","
              Whitespace@6..7 " "
              Name@7..8
                Identifier@7..8 "b"
              Comma@8..9 ","
              Whitespace@9..10 " "
              Name@10..12
                Identifier@10..11 "c"
                Whitespace@11..12 " "
            Assign@12..14 ":="
            Whitespace@14..15 " "
            LiteralExpr@15..16
              IntLiteral@15..16 "1""#]]);
}

#[test]
fn recover_not_name_in_var_decl_multiple_names() {
    check("var a, to, c := 1", expect![[r#"
        Root@0..17
          ConstVarDecl@0..17
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..13
              Name@4..5
                Identifier@4..5 "a"
              Comma@5..6 ","
              Whitespace@6..7 " "
              Error@7..9
                KwTo@7..9 "to"
              Comma@9..10 ","
              Whitespace@10..11 " "
              Name@11..13
                Identifier@11..12 "c"
                Whitespace@12..13 " "
            Assign@13..15 ":="
            Whitespace@15..16 " "
            LiteralExpr@16..17
              IntLiteral@16..17 "1"
        error at 7..9: expected identifier, but found ’to’"#]]);
}

#[test]
fn recover_not_name_in_var_decl() {
    check("var to := 1", expect![[r#"
        Root@0..11
          ConstVarDecl@0..11
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..7
              Error@4..7
                KwTo@4..6 "to"
                Whitespace@6..7 " "
            Assign@7..9 ":="
            Whitespace@9..10 " "
            LiteralExpr@10..11
              IntLiteral@10..11 "1"
        error at 4..6: expected identifier, but found ’to’"#]]);
}

#[test]
fn recover_bare_var_decl() {
    check("var a", expect![[r#"
        Root@0..5
          ConstVarDecl@0..5
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..5
              Name@4..5
                Identifier@4..5 "a"
        error at 4..5: expected ’,’, ’:’ or ’:=’"#]])
}

#[test]
fn recover_bare_const_decl() {
    check("const a", expect![[r#"
        Root@0..7
          ConstVarDecl@0..7
            KwConst@0..5 "const"
            Whitespace@5..6 " "
            NameList@6..7
              Name@6..7
                Identifier@6..7 "a"
        error at 6..7: expected ’,’, ’:’ or ’:=’"#]])
}

#[test]
fn recover_var_decl_missing_ty() {
    check(
        "var a : := 1",
        expect![[r#"
            Root@0..12
              ConstVarDecl@0..12
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Colon@6..7 ":"
                Whitespace@7..8 " "
                Assign@8..10 ":="
                Whitespace@10..11 " "
                LiteralExpr@11..12
                  IntLiteral@11..12 "1"
            error at 8..10: expected ’addressint’, ’boolean’, ’int’, ’int1’, ’int2’, ’int4’, ’nat’, ’nat1’, ’nat2’, ’nat4’, ’real’, ’real4’, ’real8’, ’char’ or ’string’, but found ’:=’"#]],
    )
}

#[test]
fn recover_var_decl_not_a_ty() {
    check(
        "var a : to := 1",
        expect![[r#"
            Root@0..15
              ConstVarDecl@0..15
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Colon@6..7 ":"
                Whitespace@7..8 " "
                Error@8..11
                  KwTo@8..10 "to"
                  Whitespace@10..11 " "
                Assign@11..13 ":="
                Whitespace@13..14 " "
                LiteralExpr@14..15
                  IntLiteral@14..15 "1"
            error at 8..10: expected ’addressint’, ’boolean’, ’int’, ’int1’, ’int2’, ’int4’, ’nat’, ’nat1’, ’nat2’, ’nat4’, ’real’, ’real4’, ’real8’, ’char’ or ’string’, but found ’to’"#]],
    )
}

#[test]
fn parse_var_decl_inferred_ty() {
    check(
        "var a := 1",
        expect![[r#"
            Root@0..10
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
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
                NameList@6..8
                  Name@6..8
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
                NameList@6..8
                  Name@6..8
                    Identifier@6..7 "a"
                    Whitespace@7..8 " "
                Assign@8..10 ":="
                Whitespace@10..11 " "
                LiteralExpr@11..13
                  IntLiteral@11..12 "1"
                  Whitespace@12..13 "\n"
              NameExpr@13..14
                Name@13..14
                  Identifier@13..14 "a""#]],
    );
}

#[test]
fn recover_on_var() {
    check(
        "const a := \nvar b := 1",
        expect![[r##"
            Root@0..22
              ConstVarDecl@0..12
                KwConst@0..5 "const"
                Whitespace@5..6 " "
                NameList@6..8
                  Name@6..8
                    Identifier@6..7 "a"
                    Whitespace@7..8 " "
                Assign@8..10 ":="
                Whitespace@10..12 " \n"
              ConstVarDecl@12..22
                KwVar@12..15 "var"
                Whitespace@15..16 " "
                NameList@16..18
                  Name@16..18
                    Identifier@16..17 "b"
                    Whitespace@17..18 " "
                Assign@18..20 ":="
                Whitespace@20..21 " "
                LiteralExpr@21..22
                  IntLiteral@21..22 "1"
            error at 12..15: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’var’"##]],
    );
}

#[test]
fn recover_on_const() {
    check(
        "var a := \nconst b := 1",
        expect![[r##"
            Root@0..22
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              ConstVarDecl@10..22
                KwConst@10..15 "const"
                Whitespace@15..16 " "
                NameList@16..18
                  Name@16..18
                    Identifier@16..17 "b"
                    Whitespace@17..18 " "
                Assign@18..20 ":="
                Whitespace@20..21 " "
                LiteralExpr@21..22
                  IntLiteral@21..22 "1"
            error at 10..15: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’const’"##]],
    );
}
