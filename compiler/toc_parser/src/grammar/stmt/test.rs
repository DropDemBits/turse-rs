//! Tests for statements

// Required test for each statement:
// - Bare minimum example
// - Any optional part (1 test per optional part)
// - Empty variant
// - Recovery on starting token

use crate::check;
use expect_test::expect;

#[test]
fn parse_opt_semicolon() {
    check(
        "var i : int; type j : boolean;",
        expect![[r#"
            Source@0..30
              ConstVarDecl@0..11
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "i"
                    Whitespace@5..6 " "
                Colon@6..7 ":"
                Whitespace@7..8 " "
                PrimType@8..11
                  KwInt@8..11 "int"
              Semicolon@11..12 ";"
              Whitespace@12..13 " "
              TypeDecl@13..29
                KwType@13..17 "type"
                Whitespace@17..18 " "
                Name@18..20
                  Identifier@18..19 "j"
                  Whitespace@19..20 " "
                Colon@20..21 ":"
                Whitespace@21..22 " "
                PrimType@22..29
                  KwBoolean@22..29 "boolean"
              Semicolon@29..30 ";""#]],
    );
}

#[test]
fn report_not_a_stmt() {
    check(
        "pervasive",
        expect![[r#"
            Source@0..9
              Error@0..9
                KwPervasive@0..9 "pervasive"
            error at 0..9: expected statement, but found ’pervasive’"#]],
    );
}

#[test]
fn recover_just_assign() {
    check(
        ":=",
        expect![[r#"
            Source@0..2
              Error@0..2
                Assign@0..2 ":="
            error at 0..2: expected statement, but found ’:=’"#]],
    )
}

#[test]
fn recover_just_var() {
    check(
        "var",
        expect![[r#"
            Source@0..3
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
            Source@0..16
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
            Source@0..18
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
fn parse_var_decl_with_alt_eq() {
    check(
        "var a : int = 1",
        expect![[r#"
            Source@0..15
              ConstVarDecl@0..15
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
                Equ@12..13 "="
                Whitespace@13..14 " "
                LiteralExpr@14..15
                  IntLiteral@14..15 "1"
            warn at 12..13: ’=’ found, assuming it to be ’:=’"#]],
    )
}

#[test]
fn parse_const_decl_with_alt_eq() {
    check(
        "const a : int = 1",
        expect![[r#"
            Source@0..17
              ConstVarDecl@0..17
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
                Equ@14..15 "="
                Whitespace@15..16 " "
                LiteralExpr@16..17
                  IntLiteral@16..17 "1"
            warn at 14..15: ’=’ found, assuming it to be ’:=’"#]],
    )
}

#[test]
fn parse_var_decl_with_pervasive_attr() {
    check(
        "var pervasive a : int",
        expect![[r#"
            Source@0..21
              ConstVarDecl@0..21
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                KwPervasive@4..13 "pervasive"
                Whitespace@13..14 " "
                NameList@14..16
                  Name@14..16
                    Identifier@14..15 "a"
                    Whitespace@15..16 " "
                Colon@16..17 ":"
                Whitespace@17..18 " "
                PrimType@18..21
                  KwInt@18..21 "int""#]],
    );
}

#[test]
fn parse_var_decl_with_alt_pervasive_attr() {
    check(
        "var * a : int",
        expect![[r#"
            Source@0..13
              ConstVarDecl@0..13
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                Star@4..5 "*"
                Whitespace@5..6 " "
                NameList@6..8
                  Name@6..8
                    Identifier@6..7 "a"
                    Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                PrimType@10..13
                  KwInt@10..13 "int""#]],
    );
}

#[test]
fn parse_var_decl_with_register_attr() {
    check(
        "var register a : int",
        expect![[r#"
            Source@0..20
              ConstVarDecl@0..20
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                KwRegister@4..12 "register"
                Whitespace@12..13 " "
                NameList@13..15
                  Name@13..15
                    Identifier@13..14 "a"
                    Whitespace@14..15 " "
                Colon@15..16 ":"
                Whitespace@16..17 " "
                PrimType@17..20
                  KwInt@17..20 "int""#]],
    );
}

#[test]
fn parse_var_decl_with_all_attrs() {
    check(
        "var pervasive register a : int",
        expect![[r#"
            Source@0..30
              ConstVarDecl@0..30
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                KwPervasive@4..13 "pervasive"
                Whitespace@13..14 " "
                KwRegister@14..22 "register"
                Whitespace@22..23 " "
                NameList@23..25
                  Name@23..25
                    Identifier@23..24 "a"
                    Whitespace@24..25 " "
                Colon@25..26 ":"
                Whitespace@26..27 " "
                PrimType@27..30
                  KwInt@27..30 "int""#]],
    );
}

#[test]
fn parse_var_decl_no_init() {
    check(
        "var a : int",
        expect![[r#"
            Source@0..11
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
                  KwInt@8..11 "int""#]],
    )
}

#[test]
fn recover_const_decl_no_init() {
    check(
        "const a : int",
        expect![[r#"
            Source@0..13
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
            error at 10..13: expected ’:=’"#]],
    )
}

#[test]
fn parse_var_decl_multiple_names() {
    check(
        "var a, b, c := 1",
        expect![[r#"
            Source@0..16
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
                  IntLiteral@15..16 "1""#]],
    );
}

#[test]
fn recover_not_name_in_var_decl_multiple_names() {
    check(
        "var a, to, c := 1",
        expect![[r#"
            Source@0..17
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
            error at 7..9: expected identifier, but found ’to’"#]],
    );
}

#[test]
fn recover_not_name_in_var_decl() {
    check(
        "var to := 1",
        expect![[r#"
            Source@0..11
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
            error at 4..6: expected identifier, but found ’to’"#]],
    );
}

#[test]
fn recover_bare_var_decl() {
    check(
        "var a",
        expect![[r#"
            Source@0..5
              ConstVarDecl@0..5
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..5
                  Name@4..5
                    Identifier@4..5 "a"
            error at 4..5: expected ’,’, ’:’ or ’:=’"#]],
    )
}

#[test]
fn recover_bare_const_decl() {
    check(
        "const a",
        expect![[r#"
            Source@0..7
              ConstVarDecl@0..7
                KwConst@0..5 "const"
                Whitespace@5..6 " "
                NameList@6..7
                  Name@6..7
                    Identifier@6..7 "a"
            error at 6..7: expected ’,’, ’:’ or ’:=’"#]],
    )
}

#[test]
fn recover_var_decl_missing_ty() {
    check(
        "var a : := 1",
        expect![[r#"
            Source@0..12
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
            error at 8..10: expected type specifier, but found ’:=’"#]],
    )
}

#[test]
fn recover_var_decl_not_a_ty() {
    check(
        "var a : to := 1",
        expect![[r#"
            Source@0..15
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
            error at 8..10: expected type specifier, but found ’to’"#]],
    )
}

#[test]
fn parse_var_decl_inferred_ty() {
    check(
        "var a := 1",
        expect![[r#"
            Source@0..10
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
            Source@0..12
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
            Source@0..14
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
              CallStmt@13..14
                NameExpr@13..14
                  Name@13..14
                    Identifier@13..14 "a""#]],
    );
}

#[test]
fn recover_on_var() {
    check(
        "const a := \nvar b := 1",
        expect![[r#"
            Source@0..22
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
            error at 12..15: expected expression, but found ’var’"#]],
    );
}

#[test]
fn recover_on_const() {
    check(
        "var a := \nconst b := 1",
        expect![[r#"
            Source@0..22
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
            error at 10..15: expected expression, but found ’const’"#]],
    );
}

#[test]
fn parse_assign_stmt() {
    check(
        "a := b + 2 + c",
        expect![[r#"
            Source@0..14
              AssignStmt@0..14
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..5
                  Assign@2..4 ":="
                  Whitespace@4..5 " "
                BinaryExpr@5..14
                  BinaryExpr@5..11
                    NameExpr@5..7
                      Name@5..7
                        Identifier@5..6 "b"
                        Whitespace@6..7 " "
                    Plus@7..8 "+"
                    Whitespace@8..9 " "
                    LiteralExpr@9..11
                      IntLiteral@9..10 "2"
                      Whitespace@10..11 " "
                  Plus@11..12 "+"
                  Whitespace@12..13 " "
                  NameExpr@13..14
                    Name@13..14
                      Identifier@13..14 "c""#]],
    );
}

#[test]
fn parse_compound_assign_stmt() {
    check(
        "a => = 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  Imply@2..4 "=>"
                  Whitespace@4..5 " "
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
    check(
        "a |= 1",
        expect![[r#"
            Source@0..6
              AssignStmt@0..6
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..5
                  Pipe@2..3 "|"
                  Equ@3..4 "="
                  Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "1""#]],
    );
    check(
        "a or = 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  KwOr@2..4 "or"
                  Whitespace@4..5 " "
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
    check(
        "a & = 1",
        expect![[r#"
            Source@0..7
              AssignStmt@0..7
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..6
                  Ampersand@2..3 "&"
                  Whitespace@3..4 " "
                  Equ@4..5 "="
                  Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "1""#]],
    );
    check(
        "a and= 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  KwAnd@2..5 "and"
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
    check(
        "a + = 1",
        expect![[r#"
            Source@0..7
              AssignStmt@0..7
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..6
                  Plus@2..3 "+"
                  Whitespace@3..4 " "
                  Equ@4..5 "="
                  Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "1""#]],
    );
    check(
        "a -= 1",
        expect![[r#"
            Source@0..6
              AssignStmt@0..6
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..5
                  Minus@2..3 "-"
                  Equ@3..4 "="
                  Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "1""#]],
    );
    check(
        "a xor = 1",
        expect![[r#"
            Source@0..9
              AssignStmt@0..9
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..8
                  KwXor@2..5 "xor"
                  Whitespace@5..6 " "
                  Equ@6..7 "="
                  Whitespace@7..8 " "
                LiteralExpr@8..9
                  IntLiteral@8..9 "1""#]],
    );
    check(
        "a *= 1",
        expect![[r#"
            Source@0..6
              AssignStmt@0..6
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..5
                  Star@2..3 "*"
                  Equ@3..4 "="
                  Whitespace@4..5 " "
                LiteralExpr@5..6
                  IntLiteral@5..6 "1""#]],
    );
    check(
        "a / = 1",
        expect![[r#"
            Source@0..7
              AssignStmt@0..7
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..6
                  Slash@2..3 "/"
                  Whitespace@3..4 " "
                  Equ@4..5 "="
                  Whitespace@5..6 " "
                LiteralExpr@6..7
                  IntLiteral@6..7 "1""#]],
    );
    check(
        "a div= 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  KwDiv@2..5 "div"
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
    check(
        "a mod = 1",
        expect![[r#"
            Source@0..9
              AssignStmt@0..9
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..8
                  KwMod@2..5 "mod"
                  Whitespace@5..6 " "
                  Equ@6..7 "="
                  Whitespace@7..8 " "
                LiteralExpr@8..9
                  IntLiteral@8..9 "1""#]],
    );
    check(
        "a rem= 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  KwRem@2..5 "rem"
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
    check(
        "a shl = 1",
        expect![[r#"
            Source@0..9
              AssignStmt@0..9
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..8
                  KwShl@2..5 "shl"
                  Whitespace@5..6 " "
                  Equ@6..7 "="
                  Whitespace@7..8 " "
                LiteralExpr@8..9
                  IntLiteral@8..9 "1""#]],
    );
    check(
        "a shr= 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  KwShr@2..5 "shr"
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
    check(
        "a ** = 1",
        expect![[r#"
            Source@0..8
              AssignStmt@0..8
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..7
                  Exp@2..4 "**"
                  Whitespace@4..5 " "
                  Equ@5..6 "="
                  Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "1""#]],
    );
}

#[test]
fn recover_not_weird_asn_op() {
    // "not table" is not a valid asn op
    check(
        "a not==not 1",
        expect![[r#"
            Source@0..12
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..5
                KwNot@2..5 "not"
              Error@5..6
                Equ@5..6 "="
              Error@6..7
                Equ@6..7 "="
              Error@7..11
                KwNot@7..10 "not"
                Whitespace@10..11 " "
              CallStmt@11..12
                LiteralExpr@11..12
                  IntLiteral@11..12 "1"
            error at 2..5: expected statement, but found ’not’
            error at 5..6: expected statement, but found ’=’
            error at 6..7: expected statement, but found ’=’
            error at 7..10: expected statement, but found ’not’"#]],
    );
    check(
        "a ~==~ 1",
        expect![[r#"
            Source@0..8
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..3
                Tilde@2..3 "~"
              Error@3..4
                Equ@3..4 "="
              Error@4..5
                Equ@4..5 "="
              Error@5..7
                Tilde@5..6 "~"
                Whitespace@6..7 " "
              CallStmt@7..8
                LiteralExpr@7..8
                  IntLiteral@7..8 "1"
            error at 2..3: expected statement, but found ’~’
            error at 3..4: expected statement, but found ’=’
            error at 4..5: expected statement, but found ’=’
            error at 5..6: expected statement, but found ’~’"#]],
    );
}

#[test]
fn recover_not_a_compound_asn_op() {
    // these are not compound ops in Turing
    check(
        "a <= 1",
        expect![[r#"
            Source@0..6
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..5
                LessEqu@2..4 "<="
                Whitespace@4..5 " "
              CallStmt@5..6
                LiteralExpr@5..6
                  IntLiteral@5..6 "1"
            error at 2..4: expected statement, but found ’<=’"#]],
    );
    check(
        "a <== 1",
        expect![[r#"
            Source@0..7
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..4
                LessEqu@2..4 "<="
              Error@4..6
                Equ@4..5 "="
                Whitespace@5..6 " "
              CallStmt@6..7
                LiteralExpr@6..7
                  IntLiteral@6..7 "1"
            error at 2..4: expected statement, but found ’<=’
            error at 4..5: expected statement, but found ’=’"#]],
    );
    check(
        "a >= 1",
        expect![[r#"
            Source@0..6
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..5
                GreaterEqu@2..4 ">="
                Whitespace@4..5 " "
              CallStmt@5..6
                LiteralExpr@5..6
                  IntLiteral@5..6 "1"
            error at 2..4: expected statement, but found ’>=’"#]],
    );
    check(
        "a >== 1",
        expect![[r#"
            Source@0..7
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..4
                GreaterEqu@2..4 ">="
              Error@4..6
                Equ@4..5 "="
                Whitespace@5..6 " "
              CallStmt@6..7
                LiteralExpr@6..7
                  IntLiteral@6..7 "1"
            error at 2..4: expected statement, but found ’>=’
            error at 4..5: expected statement, but found ’=’"#]],
    );

    // these are not compound ops in `toc`
    check(
        "a ~== 1",
        expect![[r#"
            Source@0..7
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..3
                Tilde@2..3 "~"
              Error@3..4
                Equ@3..4 "="
              Error@4..6
                Equ@4..5 "="
                Whitespace@5..6 " "
              CallStmt@6..7
                LiteralExpr@6..7
                  IntLiteral@6..7 "1"
            error at 2..3: expected statement, but found ’~’
            error at 3..4: expected statement, but found ’=’
            error at 4..5: expected statement, but found ’=’"#]],
    );
    check(
        "a not== 1",
        expect![[r#"
            Source@0..9
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..5
                KwNot@2..5 "not"
              Error@5..6
                Equ@5..6 "="
              Error@6..8
                Equ@6..7 "="
                Whitespace@7..8 " "
              CallStmt@8..9
                LiteralExpr@8..9
                  IntLiteral@8..9 "1"
            error at 2..5: expected statement, but found ’not’
            error at 5..6: expected statement, but found ’=’
            error at 6..7: expected statement, but found ’=’"#]],
    );
    check(
        "a not in= 1",
        expect![[r#"
            Source@0..11
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..6
                KwNot@2..5 "not"
                Whitespace@5..6 " "
              Error@6..8
                KwIn@6..8 "in"
              Error@8..10
                Equ@8..9 "="
                Whitespace@9..10 " "
              CallStmt@10..11
                LiteralExpr@10..11
                  IntLiteral@10..11 "1"
            error at 2..5: expected statement, but found ’not’
            error at 6..8: expected statement, but found ’in’
            error at 8..9: expected statement, but found ’=’"#]],
    );
    check(
        "a ~in= 1",
        expect![[r#"
            Source@0..8
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..3
                Tilde@2..3 "~"
              Error@3..5
                KwIn@3..5 "in"
              Error@5..7
                Equ@5..6 "="
                Whitespace@6..7 " "
              CallStmt@7..8
                LiteralExpr@7..8
                  IntLiteral@7..8 "1"
            error at 2..3: expected statement, but found ’~’
            error at 3..5: expected statement, but found ’in’
            error at 5..6: expected statement, but found ’=’"#]],
    );
    check(
        "a in= 1",
        expect![[r#"
            Source@0..7
              CallStmt@0..2
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
              Error@2..4
                KwIn@2..4 "in"
              Error@4..6
                Equ@4..5 "="
                Whitespace@5..6 " "
              CallStmt@6..7
                LiteralExpr@6..7
                  IntLiteral@6..7 "1"
            error at 2..4: expected statement, but found ’in’
            error at 4..5: expected statement, but found ’=’"#]],
    );
    check(
        "a == 1",
        expect![[r#"
            Source@0..6
              AssignStmt@0..5
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..3
                  Equ@2..3 "="
                Error@3..5
                  Equ@3..4 "="
                  Whitespace@4..5 " "
              CallStmt@5..6
                LiteralExpr@5..6
                  IntLiteral@5..6 "1"
            warn at 2..3: ’=’ found, assuming it to be ’:=’
            error at 3..4: expected expression, but found ’=’"#]],
    );
}

#[test]
fn recover_missing_eq_in_asn_op() {
    // other 'a' should be untouched
    check(
        "a + 1\na",
        expect![[r#"
            Source@0..7
              CallStmt@0..6
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                Error@2..6
                  Plus@2..3 "+"
                  Whitespace@3..4 " "
                  IntLiteral@4..5 "1"
                  Whitespace@5..6 "\n"
              CallStmt@6..7
                NameExpr@6..7
                  Name@6..7
                    Identifier@6..7 "a"
            error at 4..5: expected ’=’, but found int literal"#]],
    );
}

#[test]
fn recover_eq_instead_of_asn() {
    check(
        "a = 1",
        expect![[r#"
            Source@0..5
              AssignStmt@0..5
                NameExpr@0..2
                  Name@0..2
                    Identifier@0..1 "a"
                    Whitespace@1..2 " "
                AsnOp@2..4
                  Equ@2..3 "="
                  Whitespace@3..4 " "
                LiteralExpr@4..5
                  IntLiteral@4..5 "1"
            warn at 2..3: ’=’ found, assuming it to be ’:=’"#]],
    );
}

#[test]
fn parse_type_decl() {
    check(
        "type a : int",
        expect![[r#"
            Source@0..12
              TypeDecl@0..12
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..7
                  Identifier@5..6 "a"
                  Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                PrimType@9..12
                  KwInt@9..12 "int""#]],
    );
}

#[test]
fn parse_type_decl_with_forward() {
    check(
        "type a : forward",
        expect![[r#"
            Source@0..16
              TypeDecl@0..16
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..7
                  Identifier@5..6 "a"
                  Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                KwForward@9..16 "forward""#]],
    );
}

#[test]
fn parse_type_decl_with_pervasive_attr() {
    check(
        "type pervasive a : int",
        expect![[r#"
            Source@0..22
              TypeDecl@0..22
                KwType@0..4 "type"
                Whitespace@4..5 " "
                KwPervasive@5..14 "pervasive"
                Whitespace@14..15 " "
                Name@15..17
                  Identifier@15..16 "a"
                  Whitespace@16..17 " "
                Colon@17..18 ":"
                Whitespace@18..19 " "
                PrimType@19..22
                  KwInt@19..22 "int""#]],
    );
}

#[test]
fn parse_type_decl_with_star_attr() {
    check(
        "type * a : int",
        expect![[r#"
            Source@0..14
              TypeDecl@0..14
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Star@5..6 "*"
                Whitespace@6..7 " "
                Name@7..9
                  Identifier@7..8 "a"
                  Whitespace@8..9 " "
                Colon@9..10 ":"
                Whitespace@10..11 " "
                PrimType@11..14
                  KwInt@11..14 "int""#]],
    );
}

#[test]
fn recover_type_decl_missing_type() {
    check(
        "type a : ",
        expect![[r#"
            Source@0..9
              TypeDecl@0..9
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..7
                  Identifier@5..6 "a"
                  Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
            error at 8..9: expected type specifier"#]],
    );
}

#[test]
fn recover_type_decl_missing_colon() {
    check(
        "type a forward",
        expect![[r#"
            Source@0..14
              TypeDecl@0..14
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..7
                  Identifier@5..6 "a"
                  Whitespace@6..7 " "
                KwForward@7..14 "forward"
            error at 7..14: expected ’:’, but found ’forward’"#]],
    );
}

#[test]
fn recover_type_decl_missing_colon_and_type() {
    check(
        "type a",
        expect![[r#"
            Source@0..6
              TypeDecl@0..6
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..6
                  Identifier@5..6 "a"
            error at 5..6: expected ’:’
            error at 5..6: expected type specifier"#]],
    );
}

#[test]
fn recover_just_type() {
    check(
        "type",
        expect![[r#"
            Source@0..4
              TypeDecl@0..4
                KwType@0..4 "type"
            error at 0..4: expected identifier
            error at 0..4: expected ’:’
            error at 0..4: expected type specifier"#]],
    );
}

#[test]
fn recover_on_type() {
    check(
        "var a := \ntype a : int",
        expect![[r#"
            Source@0..22
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              TypeDecl@10..22
                KwType@10..14 "type"
                Whitespace@14..15 " "
                Name@15..17
                  Identifier@15..16 "a"
                  Whitespace@16..17 " "
                Colon@17..18 ":"
                Whitespace@18..19 " "
                PrimType@19..22
                  KwInt@19..22 "int"
            error at 10..14: expected expression, but found ’type’"#]],
    );
}

#[test]
fn parse_block_stmt() {
    check(
        "begin var a := 1 end",
        expect![[r#"
            Source@0..20
              BlockStmt@0..20
                KwBegin@0..5 "begin"
                Whitespace@5..6 " "
                StmtList@6..17
                  ConstVarDecl@6..17
                    KwVar@6..9 "var"
                    Whitespace@9..10 " "
                    NameList@10..12
                      Name@10..12
                        Identifier@10..11 "a"
                        Whitespace@11..12 " "
                    Assign@12..14 ":="
                    Whitespace@14..15 " "
                    LiteralExpr@15..17
                      IntLiteral@15..16 "1"
                      Whitespace@16..17 " "
                EndGroup@17..20
                  KwEnd@17..20 "end""#]],
    );
}

#[test]
fn parse_empty_block_stmt() {
    check(
        "begin end",
        expect![[r#"
            Source@0..9
              BlockStmt@0..9
                KwBegin@0..5 "begin"
                Whitespace@5..6 " "
                StmtList@6..6
                EndGroup@6..9
                  KwEnd@6..9 "end""#]],
    );
}

#[test]
fn recover_on_block_stmt() {
    check(
        "var a := begin end",
        expect![[r#"
            Source@0..18
              ConstVarDecl@0..9
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..9 " "
              BlockStmt@9..18
                KwBegin@9..14 "begin"
                Whitespace@14..15 " "
                StmtList@15..15
                EndGroup@15..18
                  KwEnd@15..18 "end"
            error at 9..14: expected expression, but found ’begin’"#]],
    );
}

#[test]
fn parse_if_stmt() {
    check(
        "if true then var key : int end if",
        expect![[r#"
            Source@0..33
              IfStmt@0..33
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..27
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  Whitespace@12..13 " "
                  StmtList@13..27
                    ConstVarDecl@13..27
                      KwVar@13..16 "var"
                      Whitespace@16..17 " "
                      NameList@17..21
                        Name@17..21
                          Identifier@17..20 "key"
                          Whitespace@20..21 " "
                      Colon@21..22 ":"
                      Whitespace@22..23 " "
                      PrimType@23..27
                        KwInt@23..26 "int"
                        Whitespace@26..27 " "
                EndGroup@27..33
                  KwEnd@27..30 "end"
                  Whitespace@30..31 " "
                  KwIf@31..33 "if""#]],
    );
}

#[test]
fn parse_if_else() {
    check(
        "if true then else end if",
        expect![[r#"
            Source@0..24
              IfStmt@0..24
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..18
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  Whitespace@12..13 " "
                  StmtList@13..13
                  ElseStmt@13..18
                    KwElse@13..17 "else"
                    Whitespace@17..18 " "
                    StmtList@18..18
                EndGroup@18..24
                  KwEnd@18..21 "end"
                  Whitespace@21..22 " "
                  KwIf@22..24 "if""#]],
    );
}

#[test]
fn parse_if_elseif_else() {
    check(
        "if true then elsif true then else end if",
        expect![[r#"
            Source@0..40
              IfStmt@0..40
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..34
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  Whitespace@12..13 " "
                  StmtList@13..13
                  ElseifStmt@13..34
                    KwElsif@13..18 "elsif"
                    Whitespace@18..19 " "
                    IfBody@19..34
                      LiteralExpr@19..24
                        KwTrue@19..23 "true"
                        Whitespace@23..24 " "
                      KwThen@24..28 "then"
                      Whitespace@28..29 " "
                      StmtList@29..29
                      ElseStmt@29..34
                        KwElse@29..33 "else"
                        Whitespace@33..34 " "
                        StmtList@34..34
                EndGroup@34..40
                  KwEnd@34..37 "end"
                  Whitespace@37..38 " "
                  KwIf@38..40 "if""#]],
    );
}

#[test]
fn parse_if_chained_alternates() {
    check(
        "if true then elseif true then elif true then end if",
        expect![[r#"
            Source@0..51
              IfStmt@0..51
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..45
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  Whitespace@12..13 " "
                  StmtList@13..13
                  ElseifStmt@13..45
                    KwElseif@13..19 "elseif"
                    Whitespace@19..20 " "
                    IfBody@20..45
                      LiteralExpr@20..25
                        KwTrue@20..24 "true"
                        Whitespace@24..25 " "
                      KwThen@25..29 "then"
                      Whitespace@29..30 " "
                      StmtList@30..30
                      ElseifStmt@30..45
                        KwElif@30..34 "elif"
                        Whitespace@34..35 " "
                        IfBody@35..45
                          LiteralExpr@35..40
                            KwTrue@35..39 "true"
                            Whitespace@39..40 " "
                          KwThen@40..44 "then"
                          Whitespace@44..45 " "
                          StmtList@45..45
                EndGroup@45..51
                  KwEnd@45..48 "end"
                  Whitespace@48..49 " "
                  KwIf@49..51 "if"
            warn at 13..19: ’elseif’ found, assuming it to be ’elsif’
            warn at 30..34: ’elif’ found, assuming it to be ’elsif’"#]],
    );
}

#[test]
fn recover_if_stmt_missing_condition() {
    check(
        "if then end if",
        expect![[r#"
            Source@0..14
              IfStmt@0..14
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..8
                  KwThen@3..7 "then"
                  Whitespace@7..8 " "
                  StmtList@8..8
                EndGroup@8..14
                  KwEnd@8..11 "end"
                  Whitespace@11..12 " "
                  KwIf@12..14 "if"
            error at 3..7: expected expression, but found ’then’"#]],
    );
}

#[test]
fn recover_if_stmt_missing_then() {
    check(
        "if true end if",
        expect![[r#"
            Source@0..14
              IfStmt@0..14
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..8
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  StmtList@8..8
                EndGroup@8..14
                  KwEnd@8..11 "end"
                  Whitespace@11..12 " "
                  KwIf@12..14 "if"
            error at 8..11: expected ’then’, but found ’end’"#]],
    );
}

#[test]
fn recover_if_stmt_missing_end() {
    check(
        "if true then",
        expect![[r#"
            Source@0..12
              IfStmt@0..12
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..12
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  StmtList@12..12
                EndGroup@12..12
            error at 8..12: expected ’else’, ’elseif’, ’elsif’, ’elif’, ’endif’ or ’end’
            error at 8..12: expected ’if’"#]],
    );
}

#[test]
fn parse_if_alternate_end() {
    check(
        "if true then endif",
        expect![[r#"
            Source@0..18
              IfStmt@0..18
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..13
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  Whitespace@12..13 " "
                  StmtList@13..13
                EndGroup@13..18
                  KwEndIf@13..18 "endif"
            warn at 13..18: ’endif’ found, assuming it to be ’end if’"#]],
    );
}

#[test]
fn parse_elseif_stmt() {
    check(
        "elsif true then end if",
        expect![[r#"
            Source@0..22
              ElseifStmt@0..22
                KwElsif@0..5 "elsif"
                Whitespace@5..6 " "
                IfBody@6..16
                  LiteralExpr@6..11
                    KwTrue@6..10 "true"
                    Whitespace@10..11 " "
                  KwThen@11..15 "then"
                  Whitespace@15..16 " "
                  StmtList@16..16
                EndGroup@16..22
                  KwEnd@16..19 "end"
                  Whitespace@19..20 " "
                  KwIf@20..22 "if""#]],
    );
}

#[test]
fn parse_elseif_alternates_stmt() {
    check(
        "elseif true then end if",
        expect![[r#"
            Source@0..23
              ElseifStmt@0..23
                KwElseif@0..6 "elseif"
                Whitespace@6..7 " "
                IfBody@7..17
                  LiteralExpr@7..12
                    KwTrue@7..11 "true"
                    Whitespace@11..12 " "
                  KwThen@12..16 "then"
                  Whitespace@16..17 " "
                  StmtList@17..17
                EndGroup@17..23
                  KwEnd@17..20 "end"
                  Whitespace@20..21 " "
                  KwIf@21..23 "if"
            warn at 0..6: ’elseif’ found, assuming it to be ’elsif’"#]],
    );
    check(
        "elif true then end if",
        expect![[r#"
            Source@0..21
              ElseifStmt@0..21
                KwElif@0..4 "elif"
                Whitespace@4..5 " "
                IfBody@5..15
                  LiteralExpr@5..10
                    KwTrue@5..9 "true"
                    Whitespace@9..10 " "
                  KwThen@10..14 "then"
                  Whitespace@14..15 " "
                  StmtList@15..15
                EndGroup@15..21
                  KwEnd@15..18 "end"
                  Whitespace@18..19 " "
                  KwIf@19..21 "if"
            warn at 0..4: ’elif’ found, assuming it to be ’elsif’"#]],
    );
}

#[test]
fn parse_elseif_alternate_end() {
    check(
        "elsif true then endif",
        expect![[r#"
            Source@0..21
              ElseifStmt@0..21
                KwElsif@0..5 "elsif"
                Whitespace@5..6 " "
                IfBody@6..16
                  LiteralExpr@6..11
                    KwTrue@6..10 "true"
                    Whitespace@10..11 " "
                  KwThen@11..15 "then"
                  Whitespace@15..16 " "
                  StmtList@16..16
                EndGroup@16..21
                  KwEndIf@16..21 "endif"
            warn at 16..21: ’endif’ found, assuming it to be ’end if’"#]],
    );
}

#[test]
fn parse_chained_elseif_stmt() {
    check(
        r#"
    elsif true then
    elsif true then
    else
    end if"#,
        expect![[r#"
            Source@0..60
              Whitespace@0..5 "\n    "
              ElseifStmt@5..60
                KwElsif@5..10 "elsif"
                Whitespace@10..11 " "
                IfBody@11..54
                  LiteralExpr@11..16
                    KwTrue@11..15 "true"
                    Whitespace@15..16 " "
                  KwThen@16..20 "then"
                  Whitespace@20..25 "\n    "
                  StmtList@25..25
                  ElseifStmt@25..54
                    KwElsif@25..30 "elsif"
                    Whitespace@30..31 " "
                    IfBody@31..54
                      LiteralExpr@31..36
                        KwTrue@31..35 "true"
                        Whitespace@35..36 " "
                      KwThen@36..40 "then"
                      Whitespace@40..45 "\n    "
                      StmtList@45..45
                      ElseStmt@45..54
                        KwElse@45..49 "else"
                        Whitespace@49..54 "\n    "
                        StmtList@54..54
                EndGroup@54..60
                  KwEnd@54..57 "end"
                  Whitespace@57..58 " "
                  KwIf@58..60 "if""#]],
    );
}

#[test]
fn parse_else_stmt() {
    check(
        "else end if",
        expect![[r#"
            Source@0..11
              ElseStmt@0..11
                KwElse@0..4 "else"
                Whitespace@4..5 " "
                StmtList@5..5
                EndGroup@5..11
                  KwEnd@5..8 "end"
                  Whitespace@8..9 " "
                  KwIf@9..11 "if""#]],
    )
}

#[test]
fn recover_if_stmt_multiple_elses() {
    check(
        "if true then else else end if",
        expect![[r#"
            Source@0..29
              IfStmt@0..29
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..29
                  LiteralExpr@3..8
                    KwTrue@3..7 "true"
                    Whitespace@7..8 " "
                  KwThen@8..12 "then"
                  Whitespace@12..13 " "
                  StmtList@13..13
                  ElseStmt@13..29
                    KwElse@13..17 "else"
                    Whitespace@17..18 " "
                    StmtList@18..29
                      ElseStmt@18..29
                        KwElse@18..22 "else"
                        Whitespace@22..23 " "
                        StmtList@23..23
                        EndGroup@23..29
                          KwEnd@23..26 "end"
                          Whitespace@26..27 " "
                          KwIf@27..29 "if"
                EndGroup@29..29
            error at 27..29: expected ’endif’ or ’end’
            error at 27..29: expected ’if’"#]],
    );
}

#[test]
fn recover_on_if() {
    check(
        "var a := \nif true then end if",
        expect![[r#"
            Source@0..29
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              IfStmt@10..29
                KwIf@10..12 "if"
                Whitespace@12..13 " "
                IfBody@13..23
                  LiteralExpr@13..18
                    KwTrue@13..17 "true"
                    Whitespace@17..18 " "
                  KwThen@18..22 "then"
                  Whitespace@22..23 " "
                  StmtList@23..23
                EndGroup@23..29
                  KwEnd@23..26 "end"
                  Whitespace@26..27 " "
                  KwIf@27..29 "if"
            error at 10..12: expected expression, but found ’if’"#]],
    );
}

#[test]
fn recover_on_else() {
    check(
        "var a := \nelse end if",
        expect![[r#"
            Source@0..21
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              ElseStmt@10..21
                KwElse@10..14 "else"
                Whitespace@14..15 " "
                StmtList@15..15
                EndGroup@15..21
                  KwEnd@15..18 "end"
                  Whitespace@18..19 " "
                  KwIf@19..21 "if"
            error at 10..14: expected expression, but found ’else’"#]],
    );
}

#[test]
fn recover_on_elseif() {
    check(
        "var a := \nelseif true then end if",
        expect![[r#"
            Source@0..33
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              ElseifStmt@10..33
                KwElseif@10..16 "elseif"
                Whitespace@16..17 " "
                IfBody@17..27
                  LiteralExpr@17..22
                    KwTrue@17..21 "true"
                    Whitespace@21..22 " "
                  KwThen@22..26 "then"
                  Whitespace@26..27 " "
                  StmtList@27..27
                EndGroup@27..33
                  KwEnd@27..30 "end"
                  Whitespace@30..31 " "
                  KwIf@31..33 "if"
            error at 10..16: expected expression, but found ’elseif’
            warn at 10..16: ’elseif’ found, assuming it to be ’elsif’"#]],
    );
}

#[test]
fn recover_on_elsif() {
    check(
        "var a := \nelsif true then end if",
        expect![[r#"
            Source@0..32
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              ElseifStmt@10..32
                KwElsif@10..15 "elsif"
                Whitespace@15..16 " "
                IfBody@16..26
                  LiteralExpr@16..21
                    KwTrue@16..20 "true"
                    Whitespace@20..21 " "
                  KwThen@21..25 "then"
                  Whitespace@25..26 " "
                  StmtList@26..26
                EndGroup@26..32
                  KwEnd@26..29 "end"
                  Whitespace@29..30 " "
                  KwIf@30..32 "if"
            error at 10..15: expected expression, but found ’elsif’"#]],
    );
}

#[test]
fn recover_on_elif() {
    check(
        "var a := \nelif true then end if",
        expect![[r#"
            Source@0..31
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "a"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              ElseifStmt@10..31
                KwElif@10..14 "elif"
                Whitespace@14..15 " "
                IfBody@15..25
                  LiteralExpr@15..20
                    KwTrue@15..19 "true"
                    Whitespace@19..20 " "
                  KwThen@20..24 "then"
                  Whitespace@24..25 " "
                  StmtList@25..25
                EndGroup@25..31
                  KwEnd@25..28 "end"
                  Whitespace@28..29 " "
                  KwIf@29..31 "if"
            error at 10..14: expected expression, but found ’elif’
            warn at 10..14: ’elif’ found, assuming it to be ’elsif’"#]],
    );
}

#[test]
fn recover_begin_missing_end_in_if() {
    check(
        "if 0 then begin endif",
        expect![[r#"
            Source@0..21
              IfStmt@0..21
                KwIf@0..2 "if"
                Whitespace@2..3 " "
                IfBody@3..16
                  LiteralExpr@3..5
                    IntLiteral@3..4 "0"
                    Whitespace@4..5 " "
                  KwThen@5..9 "then"
                  Whitespace@9..10 " "
                  StmtList@10..16
                    BlockStmt@10..16
                      KwBegin@10..15 "begin"
                      Whitespace@15..16 " "
                      StmtList@16..16
                      EndGroup@16..16
                EndGroup@16..21
                  KwEndIf@16..21 "endif"
            error at 16..21: expected ’end’, but found ’endif’
            warn at 16..21: ’endif’ found, assuming it to be ’end if’"#]],
    );
}

#[test]
fn recover_begin_with_endloop() {
    check(
        "begin endloop",
        expect![[r#"
        Source@0..13
          BlockStmt@0..13
            KwBegin@0..5 "begin"
            Whitespace@5..6 " "
            StmtList@6..6
            EndGroup@6..13
              Error@6..13
                KwEndLoop@6..13 "endloop"
        error at 6..13: expected ’end’, but found ’endloop’"#]],
    );
}

#[test]
fn recover_begin_with_endfor() {
    check(
        "begin endfor",
        expect![[r#"
        Source@0..12
          BlockStmt@0..12
            KwBegin@0..5 "begin"
            Whitespace@5..6 " "
            StmtList@6..6
            EndGroup@6..12
              Error@6..12
                KwEndFor@6..12 "endfor"
        error at 6..12: expected ’end’, but found ’endfor’"#]],
    );
}

#[test]
fn recover_begin_with_endcase() {
    check(
        "begin endcase",
        expect![[r#"
        Source@0..13
          BlockStmt@0..13
            KwBegin@0..5 "begin"
            Whitespace@5..6 " "
            StmtList@6..6
            EndGroup@6..13
              Error@6..13
                KwEndCase@6..13 "endcase"
        error at 6..13: expected ’end’, but found ’endcase’"#]],
    );
}

#[test]
fn parse_invariant_stmt() {
    check(
        "invariant 1 + 1 = 2",
        expect![[r#"
        Source@0..19
          InvariantStmt@0..19
            KwInvariant@0..9 "invariant"
            Whitespace@9..10 " "
            BinaryExpr@10..19
              BinaryExpr@10..16
                LiteralExpr@10..12
                  IntLiteral@10..11 "1"
                  Whitespace@11..12 " "
                Plus@12..13 "+"
                Whitespace@13..14 " "
                LiteralExpr@14..16
                  IntLiteral@14..15 "1"
                  Whitespace@15..16 " "
              Equ@16..17 "="
              Whitespace@17..18 " "
              LiteralExpr@18..19
                IntLiteral@18..19 "2""#]],
    );
}

#[test]
fn recover_just_invariant() {
    check(
        "invariant",
        expect![[r#"
        Source@0..9
          InvariantStmt@0..9
            KwInvariant@0..9 "invariant"
        error at 0..9: expected expression"#]],
    );
}

#[test]
fn recover_on_invariant() {
    check(
        "var i := \ninvariant",
        expect![[r#"
        Source@0..19
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          InvariantStmt@10..19
            KwInvariant@10..19 "invariant"
        error at 10..19: expected expression, but found ’invariant’
        error at 10..19: expected expression"#]],
    );
}

#[test]
fn parse_assert_stmt() {
    check(
        "assert 1 + 1 = 2",
        expect![[r#"
        Source@0..16
          AssertStmt@0..16
            KwAssert@0..6 "assert"
            Whitespace@6..7 " "
            BinaryExpr@7..16
              BinaryExpr@7..13
                LiteralExpr@7..9
                  IntLiteral@7..8 "1"
                  Whitespace@8..9 " "
                Plus@9..10 "+"
                Whitespace@10..11 " "
                LiteralExpr@11..13
                  IntLiteral@11..12 "1"
                  Whitespace@12..13 " "
              Equ@13..14 "="
              Whitespace@14..15 " "
              LiteralExpr@15..16
                IntLiteral@15..16 "2""#]],
    );
}

#[test]
fn recover_just_assert() {
    check(
        "assert",
        expect![[r#"
        Source@0..6
          AssertStmt@0..6
            KwAssert@0..6 "assert"
        error at 0..6: expected expression"#]],
    );
}

#[test]
fn recover_on_assert() {
    check(
        "var i := \nassert true",
        expect![[r#"
        Source@0..21
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          AssertStmt@10..21
            KwAssert@10..16 "assert"
            Whitespace@16..17 " "
            LiteralExpr@17..21
              KwTrue@17..21 "true"
        error at 10..16: expected expression, but found ’assert’"#]],
    );
}

#[test]
fn parse_signal_stmt() {
    check(
        "signal a_sig",
        expect![[r#"
        Source@0..12
          SignalStmt@0..12
            KwSignal@0..6 "signal"
            Whitespace@6..7 " "
            NameExpr@7..12
              Name@7..12
                Identifier@7..12 "a_sig""#]],
    );
}

#[test]
fn parse_signal_stmt_not_ref() {
    // rejected during lowering
    check(
        "signal 1",
        expect![[r#"
        Source@0..8
          SignalStmt@0..8
            KwSignal@0..6 "signal"
            Whitespace@6..7 " "
            LiteralExpr@7..8
              IntLiteral@7..8 "1""#]],
    );
}

#[test]
fn recover_just_signal() {
    check(
        "signal",
        expect![[r#"
        Source@0..6
          SignalStmt@0..6
            KwSignal@0..6 "signal"
        error at 0..6: expected expression"#]],
    );
}

#[test]
fn recover_on_signal() {
    check(
        "var i := \nsignal a",
        expect![[r#"
        Source@0..18
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          SignalStmt@10..18
            KwSignal@10..16 "signal"
            Whitespace@16..17 " "
            NameExpr@17..18
              Name@17..18
                Identifier@17..18 "a"
        error at 10..16: expected expression, but found ’signal’"#]],
    );
}

#[test]
fn parse_pause_stmt() {
    check(
        "pause 1 + 1 = 2",
        expect![[r#"
        Source@0..15
          PauseStmt@0..15
            KwPause@0..5 "pause"
            Whitespace@5..6 " "
            BinaryExpr@6..15
              BinaryExpr@6..12
                LiteralExpr@6..8
                  IntLiteral@6..7 "1"
                  Whitespace@7..8 " "
                Plus@8..9 "+"
                Whitespace@9..10 " "
                LiteralExpr@10..12
                  IntLiteral@10..11 "1"
                  Whitespace@11..12 " "
              Equ@12..13 "="
              Whitespace@13..14 " "
              LiteralExpr@14..15
                IntLiteral@14..15 "2""#]],
    );
}

#[test]
fn recover_just_pause() {
    check(
        "pause",
        expect![[r#"
        Source@0..5
          PauseStmt@0..5
            KwPause@0..5 "pause"
        error at 0..5: expected expression"#]],
    );
}

#[test]
fn recover_on_pause() {
    check(
        "var i := \npause 3",
        expect![[r#"
            Source@0..17
              ConstVarDecl@0..10
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "i"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..10 " \n"
              PauseStmt@10..17
                KwPause@10..15 "pause"
                Whitespace@15..16 " "
                LiteralExpr@16..17
                  IntLiteral@16..17 "3"
            error at 10..15: expected expression, but found ’pause’"#]],
    );
}

#[test]
fn parse_result_stmt() {
    check(
        "result 2",
        expect![[r#"
            Source@0..8
              ResultStmt@0..8
                KwResult@0..6 "result"
                Whitespace@6..7 " "
                LiteralExpr@7..8
                  IntLiteral@7..8 "2""#]],
    );
}

#[test]
fn recover_just_result() {
    check(
        "result",
        expect![[r#"
        Source@0..6
          ResultStmt@0..6
            KwResult@0..6 "result"
        error at 0..6: expected expression"#]],
    );
}

#[test]
fn recover_on_result() {
    check(
        "var i := \nresult 2",
        expect![[r#"
        Source@0..18
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          ResultStmt@10..18
            KwResult@10..16 "result"
            Whitespace@16..17 " "
            LiteralExpr@17..18
              IntLiteral@17..18 "2"
        error at 10..16: expected expression, but found ’result’"#]],
    );
}

#[test]
fn parse_return() {
    check(
        "return",
        expect![[r#"
        Source@0..6
          ReturnStmt@0..6
            KwReturn@0..6 "return""#]],
    );
}

#[test]
fn recover_on_return() {
    check(
        "var i := \nreturn",
        expect![[r#"
        Source@0..16
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          ReturnStmt@10..16
            KwReturn@10..16 "return"
        error at 10..16: expected expression, but found ’return’"#]],
    );
}

#[test]
fn parse_checked() {
    check(
        "checked",
        expect![[r#"
        Source@0..7
          CheckednessStmt@0..7
            KwChecked@0..7 "checked""#]],
    );
}

#[test]
fn recover_on_checked() {
    check(
        "var i := \nchecked",
        expect![[r#"
        Source@0..17
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          CheckednessStmt@10..17
            KwChecked@10..17 "checked"
        error at 10..17: expected expression, but found ’checked’"#]],
    );
}

#[test]
fn parse_unchecked() {
    check(
        "unchecked",
        expect![[r#"
        Source@0..9
          CheckednessStmt@0..9
            KwUnchecked@0..9 "unchecked""#]],
    );
}

#[test]
fn recover_on_unchecked() {
    check(
        "var i := \nunchecked",
        expect![[r#"
        Source@0..19
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          CheckednessStmt@10..19
            KwUnchecked@10..19 "unchecked"
        error at 10..19: expected expression, but found ’unchecked’"#]],
    );
}

#[test]
fn parse_loop_stmt() {
    check(
        "loop begin loop end loop end end loop",
        expect![[r#"
        Source@0..37
          LoopStmt@0..37
            KwLoop@0..4 "loop"
            Whitespace@4..5 " "
            StmtList@5..29
              BlockStmt@5..29
                KwBegin@5..10 "begin"
                Whitespace@10..11 " "
                StmtList@11..25
                  LoopStmt@11..25
                    KwLoop@11..15 "loop"
                    Whitespace@15..16 " "
                    StmtList@16..16
                    EndGroup@16..25
                      KwEnd@16..19 "end"
                      Whitespace@19..20 " "
                      KwLoop@20..24 "loop"
                      Whitespace@24..25 " "
                EndGroup@25..29
                  KwEnd@25..28 "end"
                  Whitespace@28..29 " "
            EndGroup@29..37
              KwEnd@29..32 "end"
              Whitespace@32..33 " "
              KwLoop@33..37 "loop""#]],
    );
}

#[test]
fn parse_nested_loop_stmt() {
    check(
        "loop begin loop end loop end end loop",
        expect![[r#"
        Source@0..37
          LoopStmt@0..37
            KwLoop@0..4 "loop"
            Whitespace@4..5 " "
            StmtList@5..29
              BlockStmt@5..29
                KwBegin@5..10 "begin"
                Whitespace@10..11 " "
                StmtList@11..25
                  LoopStmt@11..25
                    KwLoop@11..15 "loop"
                    Whitespace@15..16 " "
                    StmtList@16..16
                    EndGroup@16..25
                      KwEnd@16..19 "end"
                      Whitespace@19..20 " "
                      KwLoop@20..24 "loop"
                      Whitespace@24..25 " "
                EndGroup@25..29
                  KwEnd@25..28 "end"
                  Whitespace@28..29 " "
            EndGroup@29..37
              KwEnd@29..32 "end"
              Whitespace@32..33 " "
              KwLoop@33..37 "loop""#]],
    );
}

#[test]
fn parse_loop_stmt_alt_end() {
    check(
        "loop endloop",
        expect![[r#"
            Source@0..12
              LoopStmt@0..12
                KwLoop@0..4 "loop"
                Whitespace@4..5 " "
                StmtList@5..5
                EndGroup@5..12
                  KwEndLoop@5..12 "endloop"
            warn at 5..12: ’endloop’ found, assuming it to be ’end loop’"#]],
    );
}

#[test]
fn recover_loop_stmt_missing_tail_loop() {
    check(
        "loop end begin end",
        expect![[r#"
        Source@0..18
          LoopStmt@0..9
            KwLoop@0..4 "loop"
            Whitespace@4..5 " "
            StmtList@5..5
            EndGroup@5..9
              KwEnd@5..8 "end"
              Whitespace@8..9 " "
          BlockStmt@9..18
            KwBegin@9..14 "begin"
            Whitespace@14..15 " "
            StmtList@15..15
            EndGroup@15..18
              KwEnd@15..18 "end"
        error at 9..14: expected ’loop’, but found ’begin’"#]],
    );
}

#[test]
fn recover_just_loop() {
    check(
        "loop begin end",
        expect![[r#"
        Source@0..14
          LoopStmt@0..14
            KwLoop@0..4 "loop"
            Whitespace@4..5 " "
            StmtList@5..14
              BlockStmt@5..14
                KwBegin@5..10 "begin"
                Whitespace@10..11 " "
                StmtList@11..11
                EndGroup@11..14
                  KwEnd@11..14 "end"
            EndGroup@14..14
        error at 11..14: expected ’endloop’ or ’end’
        error at 11..14: expected ’loop’"#]],
    );
}

#[test]
fn recover_on_loop() {
    check(
        "var i := \nloop end loop",
        expect![[r#"
        Source@0..23
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          LoopStmt@10..23
            KwLoop@10..14 "loop"
            Whitespace@14..15 " "
            StmtList@15..15
            EndGroup@15..23
              KwEnd@15..18 "end"
              Whitespace@18..19 " "
              KwLoop@19..23 "loop"
        error at 10..14: expected expression, but found ’loop’"#]],
    );
}

#[test]
fn parse_exit_stmt() {
    check(
        "exit",
        expect![[r#"
        Source@0..4
          ExitStmt@0..4
            KwExit@0..4 "exit""#]],
    );
}

#[test]
fn parse_exit_stmt_with_when() {
    check(
        "exit when false",
        expect![[r#"
        Source@0..15
          ExitStmt@0..15
            KwExit@0..4 "exit"
            Whitespace@4..5 " "
            KwWhen@5..9 "when"
            Whitespace@9..10 " "
            LiteralExpr@10..15
              KwFalse@10..15 "false""#]],
    );
}

#[test]
fn recover_exit_stmt_with_when_missing_expr() {
    check(
        "exit when",
        expect![[r#"
        Source@0..9
          ExitStmt@0..9
            KwExit@0..4 "exit"
            Whitespace@4..5 " "
            KwWhen@5..9 "when"
        error at 5..9: expected expression"#]],
    );
}

#[test]
fn recover_on_exit() {
    check(
        "var i := \nexit",
        expect![[r#"
        Source@0..14
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          ExitStmt@10..14
            KwExit@10..14 "exit"
        error at 10..14: expected expression, but found ’exit’"#]],
    );
}

#[test]
fn parse_for_loop() {
    check(
        r#"
    for i : 1 .. 3
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..59
              Whitespace@0..5 "\n    "
              ForStmt@5..59
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                Name@9..11
                  Identifier@9..10 "i"
                  Whitespace@10..11 " "
                Colon@11..12 ":"
                Whitespace@12..13 " "
                LiteralExpr@13..15
                  IntLiteral@13..14 "1"
                  Whitespace@14..15 " "
                Range@15..17 ".."
                Whitespace@17..18 " "
                LiteralExpr@18..28
                  IntLiteral@18..19 "3"
                  Whitespace@19..28 "\n        "
                StmtList@28..47
                  InvariantStmt@28..47
                    KwInvariant@28..37 "invariant"
                    Whitespace@37..38 " "
                    LiteralExpr@38..47
                      KwTrue@38..42 "true"
                      Whitespace@42..47 "\n    "
                EndGroup@47..59
                  KwEnd@47..50 "end"
                  Whitespace@50..51 " "
                  KwFor@51..54 "for"
                  Whitespace@54..59 "\n    ""#]],
    );
}

#[test]
fn parse_for_loop_alt_end() {
    check(
        r#"
    for i : 1 .. 3
        invariant true
    endfor
    "#,
        expect![[r#"
            Source@0..58
              Whitespace@0..5 "\n    "
              ForStmt@5..58
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                Name@9..11
                  Identifier@9..10 "i"
                  Whitespace@10..11 " "
                Colon@11..12 ":"
                Whitespace@12..13 " "
                LiteralExpr@13..15
                  IntLiteral@13..14 "1"
                  Whitespace@14..15 " "
                Range@15..17 ".."
                Whitespace@17..18 " "
                LiteralExpr@18..28
                  IntLiteral@18..19 "3"
                  Whitespace@19..28 "\n        "
                StmtList@28..47
                  InvariantStmt@28..47
                    KwInvariant@28..37 "invariant"
                    Whitespace@37..38 " "
                    LiteralExpr@38..47
                      KwTrue@38..42 "true"
                      Whitespace@42..47 "\n    "
                EndGroup@47..58
                  KwEndFor@47..53 "endfor"
                  Whitespace@53..58 "\n    "
            warn at 47..53: ’endfor’ found, assuming it to be ’end for’"#]],
    );
}

#[test]
fn parse_decreasing_for_loop() {
    check(
        r#"
    for decreasing i : 1 .. 3
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..70
              Whitespace@0..5 "\n    "
              ForStmt@5..70
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                KwDecreasing@9..19 "decreasing"
                Whitespace@19..20 " "
                Name@20..22
                  Identifier@20..21 "i"
                  Whitespace@21..22 " "
                Colon@22..23 ":"
                Whitespace@23..24 " "
                LiteralExpr@24..26
                  IntLiteral@24..25 "1"
                  Whitespace@25..26 " "
                Range@26..28 ".."
                Whitespace@28..29 " "
                LiteralExpr@29..39
                  IntLiteral@29..30 "3"
                  Whitespace@30..39 "\n        "
                StmtList@39..58
                  InvariantStmt@39..58
                    KwInvariant@39..48 "invariant"
                    Whitespace@48..49 " "
                    LiteralExpr@49..58
                      KwTrue@49..53 "true"
                      Whitespace@53..58 "\n    "
                EndGroup@58..70
                  KwEnd@58..61 "end"
                  Whitespace@61..62 " "
                  KwFor@62..65 "for"
                  Whitespace@65..70 "\n    ""#]],
    );
}

#[test]
fn parse_for_loop_opt_step_by() {
    check(
        r#"
    for decreasing i : 1 .. 8 by 3
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..75
              Whitespace@0..5 "\n    "
              ForStmt@5..75
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                KwDecreasing@9..19 "decreasing"
                Whitespace@19..20 " "
                Name@20..22
                  Identifier@20..21 "i"
                  Whitespace@21..22 " "
                Colon@22..23 ":"
                Whitespace@23..24 " "
                LiteralExpr@24..26
                  IntLiteral@24..25 "1"
                  Whitespace@25..26 " "
                Range@26..28 ".."
                Whitespace@28..29 " "
                LiteralExpr@29..31
                  IntLiteral@29..30 "8"
                  Whitespace@30..31 " "
                StepBy@31..44
                  KwBy@31..33 "by"
                  Whitespace@33..34 " "
                  LiteralExpr@34..44
                    IntLiteral@34..35 "3"
                    Whitespace@35..44 "\n        "
                StmtList@44..63
                  InvariantStmt@44..63
                    KwInvariant@44..53 "invariant"
                    Whitespace@53..54 " "
                    LiteralExpr@54..63
                      KwTrue@54..58 "true"
                      Whitespace@58..63 "\n    "
                EndGroup@63..75
                  KwEnd@63..66 "end"
                  Whitespace@66..67 " "
                  KwFor@67..70 "for"
                  Whitespace@70..75 "\n    ""#]],
    );
}

#[test]
fn recover_for_loop_missing_name() {
    check(
        r#"
    for : 1 .. 8
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..57
              Whitespace@0..5 "\n    "
              ForStmt@5..57
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                Colon@9..10 ":"
                Whitespace@10..11 " "
                LiteralExpr@11..13
                  IntLiteral@11..12 "1"
                  Whitespace@12..13 " "
                Range@13..15 ".."
                Whitespace@15..16 " "
                LiteralExpr@16..26
                  IntLiteral@16..17 "8"
                  Whitespace@17..26 "\n        "
                StmtList@26..45
                  InvariantStmt@26..45
                    KwInvariant@26..35 "invariant"
                    Whitespace@35..36 " "
                    LiteralExpr@36..45
                      KwTrue@36..40 "true"
                      Whitespace@40..45 "\n    "
                EndGroup@45..57
                  KwEnd@45..48 "end"
                  Whitespace@48..49 " "
                  KwFor@49..52 "for"
                  Whitespace@52..57 "\n    "
            error at 9..10: expected identifier, but found ’:’"#]],
    );
}

#[test]
fn recover_for_loop_missing_left_bound() {
    check(
        r#"
    for i : .. 8
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..57
              Whitespace@0..5 "\n    "
              ForStmt@5..57
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                Name@9..11
                  Identifier@9..10 "i"
                  Whitespace@10..11 " "
                Colon@11..12 ":"
                Whitespace@12..13 " "
                Range@13..15 ".."
                Whitespace@15..16 " "
                LiteralExpr@16..26
                  IntLiteral@16..17 "8"
                  Whitespace@17..26 "\n        "
                StmtList@26..45
                  InvariantStmt@26..45
                    KwInvariant@26..35 "invariant"
                    Whitespace@35..36 " "
                    LiteralExpr@36..45
                      KwTrue@36..40 "true"
                      Whitespace@40..45 "\n    "
                EndGroup@45..57
                  KwEnd@45..48 "end"
                  Whitespace@48..49 " "
                  KwFor@49..52 "for"
                  Whitespace@52..57 "\n    "
            error at 13..15: expected expression, but found ’..’"#]],
    );
}

#[test]
fn recover_for_loop_missing_range() {
    check(
        r#"
    for i : 1 8
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..56
              Whitespace@0..5 "\n    "
              ForStmt@5..56
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                Name@9..11
                  Identifier@9..10 "i"
                  Whitespace@10..11 " "
                Colon@11..12 ":"
                Whitespace@12..13 " "
                LiteralExpr@13..15
                  IntLiteral@13..14 "1"
                  Whitespace@14..15 " "
                Error@15..25
                  IntLiteral@15..16 "8"
                  Whitespace@16..25 "\n        "
                StmtList@25..44
                  InvariantStmt@25..44
                    KwInvariant@25..34 "invariant"
                    Whitespace@34..35 " "
                    LiteralExpr@35..44
                      KwTrue@35..39 "true"
                      Whitespace@39..44 "\n    "
                EndGroup@44..56
                  KwEnd@44..47 "end"
                  Whitespace@47..48 " "
                  KwFor@48..51 "for"
                  Whitespace@51..56 "\n    "
            error at 15..16: expected ’..’, but found int literal
            error at 25..34: expected expression, but found ’invariant’"#]],
    );
}

#[test]
fn recover_for_loop_missing_right_bound() {
    check(
        r#"
    for i : 1 ..
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..57
              Whitespace@0..5 "\n    "
              ForStmt@5..57
                KwFor@5..8 "for"
                Whitespace@8..9 " "
                Name@9..11
                  Identifier@9..10 "i"
                  Whitespace@10..11 " "
                Colon@11..12 ":"
                Whitespace@12..13 " "
                LiteralExpr@13..15
                  IntLiteral@13..14 "1"
                  Whitespace@14..15 " "
                Range@15..17 ".."
                Whitespace@17..26 "\n        "
                StmtList@26..45
                  InvariantStmt@26..45
                    KwInvariant@26..35 "invariant"
                    Whitespace@35..36 " "
                    LiteralExpr@36..45
                      KwTrue@36..40 "true"
                      Whitespace@40..45 "\n    "
                EndGroup@45..57
                  KwEnd@45..48 "end"
                  Whitespace@48..49 " "
                  KwFor@49..52 "for"
                  Whitespace@52..57 "\n    "
            error at 26..35: expected expression, but found ’invariant’"#]],
    );
}

#[test]
fn recover_on_for_loop() {
    check(
        r#"
    var i :=
    for i : 1 .. 8
        invariant true
    end for
    "#,
        expect![[r#"
            Source@0..72
              Whitespace@0..5 "\n    "
              ConstVarDecl@5..18
                KwVar@5..8 "var"
                Whitespace@8..9 " "
                NameList@9..11
                  Name@9..11
                    Identifier@9..10 "i"
                    Whitespace@10..11 " "
                Assign@11..13 ":="
                Whitespace@13..18 "\n    "
              ForStmt@18..72
                KwFor@18..21 "for"
                Whitespace@21..22 " "
                Name@22..24
                  Identifier@22..23 "i"
                  Whitespace@23..24 " "
                Colon@24..25 ":"
                Whitespace@25..26 " "
                LiteralExpr@26..28
                  IntLiteral@26..27 "1"
                  Whitespace@27..28 " "
                Range@28..30 ".."
                Whitespace@30..31 " "
                LiteralExpr@31..41
                  IntLiteral@31..32 "8"
                  Whitespace@32..41 "\n        "
                StmtList@41..60
                  InvariantStmt@41..60
                    KwInvariant@41..50 "invariant"
                    Whitespace@50..51 " "
                    LiteralExpr@51..60
                      KwTrue@51..55 "true"
                      Whitespace@55..60 "\n    "
                EndGroup@60..72
                  KwEnd@60..63 "end"
                  Whitespace@63..64 " "
                  KwFor@64..67 "for"
                  Whitespace@67..72 "\n    "
            error at 18..21: expected expression, but found ’for’"#]],
    );
}

#[test]
fn parse_case_stmt() {
    check(
        r#"
    case 1 of
        label 1, 2:
            assert false
        label 3:
        label :
            begin end % no fallthrough
    end case"#,
        expect![[r#"
            Source@0..144
              Whitespace@0..5 "\n    "
              CaseStmt@5..144
                KwCase@5..9 "case"
                Whitespace@9..10 " "
                LiteralExpr@10..12
                  IntLiteral@10..11 "1"
                  Whitespace@11..12 " "
                KwOf@12..14 "of"
                Whitespace@14..23 "\n        "
                CaseArm@23..68
                  KwLabel@23..28 "label"
                  Whitespace@28..29 " "
                  ExprList@29..33
                    LiteralExpr@29..30
                      IntLiteral@29..30 "1"
                    Comma@30..31 ","
                    Whitespace@31..32 " "
                    LiteralExpr@32..33
                      IntLiteral@32..33 "2"
                  Colon@33..34 ":"
                  Whitespace@34..47 "\n            "
                  StmtList@47..68
                    AssertStmt@47..68
                      KwAssert@47..53 "assert"
                      Whitespace@53..54 " "
                      LiteralExpr@54..68
                        KwFalse@54..59 "false"
                        Whitespace@59..68 "\n        "
                CaseArm@68..85
                  KwLabel@68..73 "label"
                  Whitespace@73..74 " "
                  ExprList@74..75
                    LiteralExpr@74..75
                      IntLiteral@74..75 "3"
                  Colon@75..76 ":"
                  Whitespace@76..85 "\n        "
                  StmtList@85..85
                CaseArm@85..136
                  KwLabel@85..90 "label"
                  Whitespace@90..91 " "
                  Colon@91..92 ":"
                  Whitespace@92..105 "\n            "
                  StmtList@105..136
                    BlockStmt@105..136
                      KwBegin@105..110 "begin"
                      Whitespace@110..111 " "
                      StmtList@111..111
                      EndGroup@111..136
                        KwEnd@111..114 "end"
                        Whitespace@114..115 " "
                        Comment@115..131 "% no fallthrough"
                        Whitespace@131..136 "\n    "
                EndGroup@136..144
                  KwEnd@136..139 "end"
                  Whitespace@139..140 " "
                  KwCase@140..144 "case""#]],
    );
}

#[test]
fn parse_case_stmt_alt_end() {
    check(
        r#"
    case 1 of
        label :
    endcase"#,
        expect![[r#"
            Source@0..42
              Whitespace@0..5 "\n    "
              CaseStmt@5..42
                KwCase@5..9 "case"
                Whitespace@9..10 " "
                LiteralExpr@10..12
                  IntLiteral@10..11 "1"
                  Whitespace@11..12 " "
                KwOf@12..14 "of"
                Whitespace@14..23 "\n        "
                CaseArm@23..35
                  KwLabel@23..28 "label"
                  Whitespace@28..29 " "
                  Colon@29..30 ":"
                  Whitespace@30..35 "\n    "
                  StmtList@35..35
                EndGroup@35..42
                  KwEndCase@35..42 "endcase"
            warn at 35..42: ’endcase’ found, assuming it to be ’end case’"#]],
    );
}

#[test]
fn parse_empty_case_stmt() {
    check(
        r#"
    case 1 of
    end case"#,
        expect![[r#"
            Source@0..27
              Whitespace@0..5 "\n    "
              CaseStmt@5..27
                KwCase@5..9 "case"
                Whitespace@9..10 " "
                LiteralExpr@10..12
                  IntLiteral@10..11 "1"
                  Whitespace@11..12 " "
                KwOf@12..14 "of"
                Whitespace@14..19 "\n    "
                EndGroup@19..27
                  KwEnd@19..22 "end"
                  Whitespace@22..23 " "
                  KwCase@23..27 "case""#]],
    );
}

#[test]
fn recover_case_stmt_missing_of() {
    check(
        r#"
    case 1
        label :
    end case"#,
        expect![[r#"
            Source@0..40
              Whitespace@0..5 "\n    "
              CaseStmt@5..40
                KwCase@5..9 "case"
                Whitespace@9..10 " "
                LiteralExpr@10..20
                  IntLiteral@10..11 "1"
                  Whitespace@11..20 "\n        "
                CaseArm@20..32
                  KwLabel@20..25 "label"
                  Whitespace@25..26 " "
                  Colon@26..27 ":"
                  Whitespace@27..32 "\n    "
                  StmtList@32..32
                EndGroup@32..40
                  KwEnd@32..35 "end"
                  Whitespace@35..36 " "
                  KwCase@36..40 "case"
            error at 20..25: expected ’of’, but found ’label’"#]],
    );
}

#[test]
fn recover_case_stmt_missing_expr() {
    check(
        r#"
    case of
        label :
    end case"#,
        expect![[r#"
            Source@0..41
              Whitespace@0..5 "\n    "
              CaseStmt@5..41
                KwCase@5..9 "case"
                Whitespace@9..10 " "
                KwOf@10..12 "of"
                Whitespace@12..21 "\n        "
                CaseArm@21..33
                  KwLabel@21..26 "label"
                  Whitespace@26..27 " "
                  Colon@27..28 ":"
                  Whitespace@28..33 "\n    "
                  StmtList@33..33
                EndGroup@33..41
                  KwEnd@33..36 "end"
                  Whitespace@36..37 " "
                  KwCase@37..41 "case"
            error at 10..12: expected expression, but found ’of’"#]],
    );
}

#[test]
fn recover_on_case_stmt() {
    check(
        r#"
    var i :=
    case 1 of
        label :
    end case"#,
        expect![[r#"
            Source@0..56
              Whitespace@0..5 "\n    "
              ConstVarDecl@5..18
                KwVar@5..8 "var"
                Whitespace@8..9 " "
                NameList@9..11
                  Name@9..11
                    Identifier@9..10 "i"
                    Whitespace@10..11 " "
                Assign@11..13 ":="
                Whitespace@13..18 "\n    "
              CaseStmt@18..56
                KwCase@18..22 "case"
                Whitespace@22..23 " "
                LiteralExpr@23..25
                  IntLiteral@23..24 "1"
                  Whitespace@24..25 " "
                KwOf@25..27 "of"
                Whitespace@27..36 "\n        "
                CaseArm@36..48
                  KwLabel@36..41 "label"
                  Whitespace@41..42 " "
                  Colon@42..43 ":"
                  Whitespace@43..48 "\n    "
                  StmtList@48..48
                EndGroup@48..56
                  KwEnd@48..51 "end"
                  Whitespace@51..52 " "
                  KwCase@52..56 "case"
            error at 18..22: expected expression, but found ’case’"#]],
    );
}

#[test]
fn parse_bind_decl() {
    check(
        "bind a to k.l.m",
        expect![[r#"
        Source@0..15
          BindDecl@0..15
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..15
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              KwTo@7..9 "to"
              Whitespace@9..10 " "
              FieldExpr@10..15
                FieldExpr@10..13
                  NameExpr@10..11
                    Name@10..11
                      Identifier@10..11 "k"
                  Dot@11..12 "."
                  Name@12..13
                    Identifier@12..13 "l"
                Dot@13..14 "."
                Name@14..15
                  Identifier@14..15 "m""#]],
    );
}

#[test]
fn parse_bind_decl_many_bindings() {
    check(
        "bind a to b, c to d, e to f",
        expect![[r#"
        Source@0..27
          BindDecl@0..27
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..11
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              KwTo@7..9 "to"
              Whitespace@9..10 " "
              NameExpr@10..11
                Name@10..11
                  Identifier@10..11 "b"
            Comma@11..12 ","
            Whitespace@12..13 " "
            BindItem@13..19
              Name@13..15
                Identifier@13..14 "c"
                Whitespace@14..15 " "
              KwTo@15..17 "to"
              Whitespace@17..18 " "
              NameExpr@18..19
                Name@18..19
                  Identifier@18..19 "d"
            Comma@19..20 ","
            Whitespace@20..21 " "
            BindItem@21..27
              Name@21..23
                Identifier@21..22 "e"
                Whitespace@22..23 " "
              KwTo@23..25 "to"
              Whitespace@25..26 " "
              NameExpr@26..27
                Name@26..27
                  Identifier@26..27 "f""#]],
    );
}

#[test]
fn parse_bind_decl_opt_var() {
    check(
        "bind var a to b",
        expect![[r#"
        Source@0..15
          BindDecl@0..15
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..15
              KwVar@5..8 "var"
              Whitespace@8..9 " "
              Name@9..11
                Identifier@9..10 "a"
                Whitespace@10..11 " "
              KwTo@11..13 "to"
              Whitespace@13..14 " "
              NameExpr@14..15
                Name@14..15
                  Identifier@14..15 "b""#]],
    );
}

#[test]
fn parse_bind_decl_opt_register() {
    check(
        "bind register a to b",
        expect![[r#"
        Source@0..20
          BindDecl@0..20
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..20
              KwRegister@5..13 "register"
              Whitespace@13..14 " "
              Name@14..16
                Identifier@14..15 "a"
                Whitespace@15..16 " "
              KwTo@16..18 "to"
              Whitespace@18..19 " "
              NameExpr@19..20
                Name@19..20
                  Identifier@19..20 "b""#]],
    );
}

#[test]
fn parse_bind_decl_opt_var_register() {
    check(
        "bind var register a to b",
        expect![[r#"
        Source@0..24
          BindDecl@0..24
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..24
              KwVar@5..8 "var"
              Whitespace@8..9 " "
              KwRegister@9..17 "register"
              Whitespace@17..18 " "
              Name@18..20
                Identifier@18..19 "a"
                Whitespace@19..20 " "
              KwTo@20..22 "to"
              Whitespace@22..23 " "
              NameExpr@23..24
                Name@23..24
                  Identifier@23..24 "b""#]],
    );
}

#[test]
fn parse_bind_decl_not_to_ref() {
    // rejected during lowering/validation
    check(
        "bind a to 1",
        expect![[r#"
        Source@0..11
          BindDecl@0..11
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..11
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              KwTo@7..9 "to"
              Whitespace@9..10 " "
              LiteralExpr@10..11
                IntLiteral@10..11 "1""#]],
    );
}

#[test]
fn recover_bind_decl_missing_name() {
    // rejected during lowering/validation
    check(
        "bind to b",
        expect![[r#"
        Source@0..9
          BindDecl@0..9
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..9
              KwTo@5..7 "to"
              Whitespace@7..8 " "
              NameExpr@8..9
                Name@8..9
                  Identifier@8..9 "b"
        error at 5..7: expected identifier, but found ’to’"#]],
    );
}

#[test]
fn recover_bind_decl_missing_to() {
    // rejected during lowering/validation
    check(
        "bind a b",
        expect![[r#"
        Source@0..8
          BindDecl@0..8
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..8
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              Error@7..8
                Identifier@7..8 "b"
        error at 7..8: expected ’to’, but found identifier
        error at 7..8: expected expression"#]],
    );
}

#[test]
fn recover_bind_decl_missing_binding() {
    // rejected during lowering/validation
    check(
        "bind a to",
        expect![[r#"
        Source@0..9
          BindDecl@0..9
            KwBind@0..4 "bind"
            Whitespace@4..5 " "
            BindItem@5..9
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              KwTo@7..9 "to"
        error at 7..9: expected expression"#]],
    );
}

#[test]
fn recover_just_bind() {
    // rejected during lowering/validation
    check(
        "bind",
        expect![[r#"
        Source@0..4
          BindDecl@0..4
            KwBind@0..4 "bind"
            BindItem@4..4
        error at 0..4: expected identifier
        error at 0..4: expected ’to’
        error at 0..4: expected expression"#]],
    );
}

#[test]
fn recover_on_bind() {
    // rejected during lowering/validation
    check(
        "var i := \nbind e to i",
        expect![[r#"
        Source@0..21
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          BindDecl@10..21
            KwBind@10..14 "bind"
            Whitespace@14..15 " "
            BindItem@15..21
              Name@15..17
                Identifier@15..16 "e"
                Whitespace@16..17 " "
              KwTo@17..19 "to"
              Whitespace@19..20 " "
              NameExpr@20..21
                Name@20..21
                  Identifier@20..21 "i"
        error at 10..14: expected expression, but found ’bind’"#]],
    );
}

#[test]
fn parse_proc_decl() {
    check(
        r#"
    procedure a (a : int)
        assert false
    end a"#,
        expect![[r#"
            Source@0..57
              Whitespace@0..5 "\n    "
              ProcDecl@5..57
                ProcHeader@5..35
                  KwProcedure@5..14 "procedure"
                  Whitespace@14..15 " "
                  Name@15..17
                    Identifier@15..16 "a"
                    Whitespace@16..17 " "
                  ParamSpec@17..35
                    LeftParen@17..18 "("
                    ParamDecl@18..25
                      NameList@18..20
                        Name@18..20
                          Identifier@18..19 "a"
                          Whitespace@19..20 " "
                      Colon@20..21 ":"
                      Whitespace@21..22 " "
                      PrimType@22..25
                        KwInt@22..25 "int"
                    RightParen@25..26 ")"
                    Whitespace@26..35 "\n        "
                StmtList@35..52
                  AssertStmt@35..52
                    KwAssert@35..41 "assert"
                    Whitespace@41..42 " "
                    LiteralExpr@42..52
                      KwFalse@42..47 "false"
                      Whitespace@47..52 "\n    "
                EndGroup@52..57
                  KwEnd@52..55 "end"
                  Whitespace@55..56 " "
                  Identifier@56..57 "a""#]],
    );
}

#[test]
fn parse_proc_decl_opt_devspec() {
    check(
        r#"
    procedure a (a : int) : 1 + 3
        assert false
    end a"#,
        expect![[r#"
            Source@0..65
              Whitespace@0..5 "\n    "
              ProcDecl@5..65
                ProcHeader@5..43
                  KwProcedure@5..14 "procedure"
                  Whitespace@14..15 " "
                  Name@15..17
                    Identifier@15..16 "a"
                    Whitespace@16..17 " "
                  ParamSpec@17..27
                    LeftParen@17..18 "("
                    ParamDecl@18..25
                      NameList@18..20
                        Name@18..20
                          Identifier@18..19 "a"
                          Whitespace@19..20 " "
                      Colon@20..21 ":"
                      Whitespace@21..22 " "
                      PrimType@22..25
                        KwInt@22..25 "int"
                    RightParen@25..26 ")"
                    Whitespace@26..27 " "
                  DeviceSpec@27..43
                    Colon@27..28 ":"
                    Whitespace@28..29 " "
                    BinaryExpr@29..43
                      LiteralExpr@29..31
                        IntLiteral@29..30 "1"
                        Whitespace@30..31 " "
                      Plus@31..32 "+"
                      Whitespace@32..33 " "
                      LiteralExpr@33..43
                        IntLiteral@33..34 "3"
                        Whitespace@34..43 "\n        "
                StmtList@43..60
                  AssertStmt@43..60
                    KwAssert@43..49 "assert"
                    Whitespace@49..50 " "
                    LiteralExpr@50..60
                      KwFalse@50..55 "false"
                      Whitespace@55..60 "\n    "
                EndGroup@60..65
                  KwEnd@60..63 "end"
                  Whitespace@63..64 " "
                  Identifier@64..65 "a""#]],
    );
}

#[test]
fn parse_proc_plain() {
    check(
        r#"
    procedure a
        assert false
    end a"#,
        expect![[r#"
            Source@0..47
              Whitespace@0..5 "\n    "
              ProcDecl@5..47
                ProcHeader@5..25
                  KwProcedure@5..14 "procedure"
                  Whitespace@14..15 " "
                  Name@15..25
                    Identifier@15..16 "a"
                    Whitespace@16..25 "\n        "
                StmtList@25..42
                  AssertStmt@25..42
                    KwAssert@25..31 "assert"
                    Whitespace@31..32 " "
                    LiteralExpr@32..42
                      KwFalse@32..37 "false"
                      Whitespace@37..42 "\n    "
                EndGroup@42..47
                  KwEnd@42..45 "end"
                  Whitespace@45..46 " "
                  Identifier@46..47 "a""#]],
    );
}

#[test]
fn recover_proc_decl_missing_name() {
    check(
        r#"
    procedure
        assert false
    end a"#,
        expect![[r#"
            Source@0..45
              Whitespace@0..5 "\n    "
              ProcDecl@5..45
                ProcHeader@5..23
                  KwProcedure@5..14 "procedure"
                  Whitespace@14..23 "\n        "
                StmtList@23..40
                  AssertStmt@23..40
                    KwAssert@23..29 "assert"
                    Whitespace@29..30 " "
                    LiteralExpr@30..40
                      KwFalse@30..35 "false"
                      Whitespace@35..40 "\n    "
                EndGroup@40..45
                  KwEnd@40..43 "end"
                  Whitespace@43..44 " "
                  Identifier@44..45 "a"
            error at 23..29: expected identifier, but found ’assert’"#]],
    );
}

#[test]
fn recover_proc_decl_missing_tail_name() {
    check(
        r#"
    procedure a
        assert false
    end"#,
        expect![[r#"
            Source@0..45
              Whitespace@0..5 "\n    "
              ProcDecl@5..45
                ProcHeader@5..25
                  KwProcedure@5..14 "procedure"
                  Whitespace@14..15 " "
                  Name@15..25
                    Identifier@15..16 "a"
                    Whitespace@16..25 "\n        "
                StmtList@25..42
                  AssertStmt@25..42
                    KwAssert@25..31 "assert"
                    Whitespace@31..32 " "
                    LiteralExpr@32..42
                      KwFalse@32..37 "false"
                      Whitespace@37..42 "\n    "
                EndGroup@42..45
                  KwEnd@42..45 "end"
            error at 42..45: expected identifier"#]],
    );
}

#[test]
fn recover_on_proc() {
    check(
        r#"
    var i :=
    procedure a
        assert false
    end a"#,
        expect![[r#"
            Source@0..60
              Whitespace@0..5 "\n    "
              ConstVarDecl@5..18
                KwVar@5..8 "var"
                Whitespace@8..9 " "
                NameList@9..11
                  Name@9..11
                    Identifier@9..10 "i"
                    Whitespace@10..11 " "
                Assign@11..13 ":="
                Whitespace@13..18 "\n    "
              ProcDecl@18..60
                ProcHeader@18..38
                  KwProcedure@18..27 "procedure"
                  Whitespace@27..28 " "
                  Name@28..38
                    Identifier@28..29 "a"
                    Whitespace@29..38 "\n        "
                StmtList@38..55
                  AssertStmt@38..55
                    KwAssert@38..44 "assert"
                    Whitespace@44..45 " "
                    LiteralExpr@45..55
                      KwFalse@45..50 "false"
                      Whitespace@50..55 "\n    "
                EndGroup@55..60
                  KwEnd@55..58 "end"
                  Whitespace@58..59 " "
                  Identifier@59..60 "a"
            error at 18..27: expected expression, but found ’procedure’"#]],
    );
}

#[test]
fn parse_fcn_decl() {
    check(
        r#"
    function a (a : int) : int
        assert false
    end a"#,
        expect![[r#"
            Source@0..62
              Whitespace@0..5 "\n    "
              FcnDecl@5..62
                FcnHeader@5..40
                  KwFunction@5..13 "function"
                  Whitespace@13..14 " "
                  Name@14..16
                    Identifier@14..15 "a"
                    Whitespace@15..16 " "
                  ParamSpec@16..26
                    LeftParen@16..17 "("
                    ParamDecl@17..24
                      NameList@17..19
                        Name@17..19
                          Identifier@17..18 "a"
                          Whitespace@18..19 " "
                      Colon@19..20 ":"
                      Whitespace@20..21 " "
                      PrimType@21..24
                        KwInt@21..24 "int"
                    RightParen@24..25 ")"
                    Whitespace@25..26 " "
                  FcnResult@26..40
                    Colon@26..27 ":"
                    Whitespace@27..28 " "
                    PrimType@28..40
                      KwInt@28..31 "int"
                      Whitespace@31..40 "\n        "
                StmtList@40..57
                  AssertStmt@40..57
                    KwAssert@40..46 "assert"
                    Whitespace@46..47 " "
                    LiteralExpr@47..57
                      KwFalse@47..52 "false"
                      Whitespace@52..57 "\n    "
                EndGroup@57..62
                  KwEnd@57..60 "end"
                  Whitespace@60..61 " "
                  Identifier@61..62 "a""#]],
    );
}

#[test]
fn parse_fcn_decl_opt_ret_name() {
    check(
        r#"
    fcn a (a : int) ae : int
        assert false
    end a"#,
        expect![[r#"
            Source@0..60
              Whitespace@0..5 "\n    "
              FcnDecl@5..60
                FcnHeader@5..38
                  KwFunction@5..8 "fcn"
                  Whitespace@8..9 " "
                  Name@9..11
                    Identifier@9..10 "a"
                    Whitespace@10..11 " "
                  ParamSpec@11..21
                    LeftParen@11..12 "("
                    ParamDecl@12..19
                      NameList@12..14
                        Name@12..14
                          Identifier@12..13 "a"
                          Whitespace@13..14 " "
                      Colon@14..15 ":"
                      Whitespace@15..16 " "
                      PrimType@16..19
                        KwInt@16..19 "int"
                    RightParen@19..20 ")"
                    Whitespace@20..21 " "
                  FcnResult@21..38
                    Name@21..24
                      Identifier@21..23 "ae"
                      Whitespace@23..24 " "
                    Colon@24..25 ":"
                    Whitespace@25..26 " "
                    PrimType@26..38
                      KwInt@26..29 "int"
                      Whitespace@29..38 "\n        "
                StmtList@38..55
                  AssertStmt@38..55
                    KwAssert@38..44 "assert"
                    Whitespace@44..45 " "
                    LiteralExpr@45..55
                      KwFalse@45..50 "false"
                      Whitespace@50..55 "\n    "
                EndGroup@55..60
                  KwEnd@55..58 "end"
                  Whitespace@58..59 " "
                  Identifier@59..60 "a""#]],
    );
}

#[test]
fn parse_fcn_plain() {
    check(
        r#"
    fcn a a : int
        assert false
    end a"#,
        expect![[r#"
            Source@0..49
              Whitespace@0..5 "\n    "
              FcnDecl@5..49
                FcnHeader@5..27
                  KwFunction@5..8 "fcn"
                  Whitespace@8..9 " "
                  Name@9..11
                    Identifier@9..10 "a"
                    Whitespace@10..11 " "
                  FcnResult@11..27
                    Name@11..13
                      Identifier@11..12 "a"
                      Whitespace@12..13 " "
                    Colon@13..14 ":"
                    Whitespace@14..15 " "
                    PrimType@15..27
                      KwInt@15..18 "int"
                      Whitespace@18..27 "\n        "
                StmtList@27..44
                  AssertStmt@27..44
                    KwAssert@27..33 "assert"
                    Whitespace@33..34 " "
                    LiteralExpr@34..44
                      KwFalse@34..39 "false"
                      Whitespace@39..44 "\n    "
                EndGroup@44..49
                  KwEnd@44..47 "end"
                  Whitespace@47..48 " "
                  Identifier@48..49 "a""#]],
    );
}

#[test]
fn recover_fcn_decl_missing_name() {
    check(
        r#"
    fcn : int
        assert false
    end a"#,
        expect![[r#"
            Source@0..45
              Whitespace@0..5 "\n    "
              FcnDecl@5..45
                FcnHeader@5..23
                  KwFunction@5..8 "fcn"
                  Whitespace@8..9 " "
                  FcnResult@9..23
                    Colon@9..10 ":"
                    Whitespace@10..11 " "
                    PrimType@11..23
                      KwInt@11..14 "int"
                      Whitespace@14..23 "\n        "
                StmtList@23..40
                  AssertStmt@23..40
                    KwAssert@23..29 "assert"
                    Whitespace@29..30 " "
                    LiteralExpr@30..40
                      KwFalse@30..35 "false"
                      Whitespace@35..40 "\n    "
                EndGroup@40..45
                  KwEnd@40..43 "end"
                  Whitespace@43..44 " "
                  Identifier@44..45 "a"
            error at 9..10: expected identifier, but found ’:’"#]],
    );
}

#[test]
fn recover_fcn_decl_missing_tail_name() {
    check(
        r#"
    fcn a : int
        assert false
    end"#,
        expect![[r#"
            Source@0..45
              Whitespace@0..5 "\n    "
              FcnDecl@5..45
                FcnHeader@5..25
                  KwFunction@5..8 "fcn"
                  Whitespace@8..9 " "
                  Name@9..11
                    Identifier@9..10 "a"
                    Whitespace@10..11 " "
                  FcnResult@11..25
                    Colon@11..12 ":"
                    Whitespace@12..13 " "
                    PrimType@13..25
                      KwInt@13..16 "int"
                      Whitespace@16..25 "\n        "
                StmtList@25..42
                  AssertStmt@25..42
                    KwAssert@25..31 "assert"
                    Whitespace@31..32 " "
                    LiteralExpr@32..42
                      KwFalse@32..37 "false"
                      Whitespace@37..42 "\n    "
                EndGroup@42..45
                  KwEnd@42..45 "end"
            error at 42..45: expected identifier"#]],
    );
}

#[test]
fn recover_fcn_decl_missing_ret_ty() {
    check(
        r#"
    fcn a :
        assert false
    end a"#,
        expect![[r#"
            Source@0..43
              Whitespace@0..5 "\n    "
              FcnDecl@5..43
                FcnHeader@5..21
                  KwFunction@5..8 "fcn"
                  Whitespace@8..9 " "
                  Name@9..11
                    Identifier@9..10 "a"
                    Whitespace@10..11 " "
                  FcnResult@11..21
                    Colon@11..12 ":"
                    Whitespace@12..21 "\n        "
                StmtList@21..38
                  AssertStmt@21..38
                    KwAssert@21..27 "assert"
                    Whitespace@27..28 " "
                    LiteralExpr@28..38
                      KwFalse@28..33 "false"
                      Whitespace@33..38 "\n    "
                EndGroup@38..43
                  KwEnd@38..41 "end"
                  Whitespace@41..42 " "
                  Identifier@42..43 "a"
            error at 21..27: expected type specifier, but found ’assert’"#]],
    );
}

#[test]
fn recover_fcn_decl_missing_colon() {
    check(
        r#"
    fcn a int
        assert false
    end a"#,
        expect![[r#"
            Source@0..45
              Whitespace@0..5 "\n    "
              FcnDecl@5..45
                FcnHeader@5..23
                  KwFunction@5..8 "fcn"
                  Whitespace@8..9 " "
                  Name@9..11
                    Identifier@9..10 "a"
                    Whitespace@10..11 " "
                  Error@11..23
                    KwInt@11..14 "int"
                    Whitespace@14..23 "\n        "
                StmtList@23..40
                  AssertStmt@23..40
                    KwAssert@23..29 "assert"
                    Whitespace@29..30 " "
                    LiteralExpr@30..40
                      KwFalse@30..35 "false"
                      Whitespace@35..40 "\n    "
                EndGroup@40..45
                  KwEnd@40..43 "end"
                  Whitespace@43..44 " "
                  Identifier@44..45 "a"
            error at 11..14: expected ’(’, identifier or ’:’, but found ’int’"#]],
    );
}

#[test]
fn recover_on_fcn() {
    check(
        r#"
    var i :=
    fcn a : int
        assert false
    end a"#,
        expect![[r#"
            Source@0..60
              Whitespace@0..5 "\n    "
              ConstVarDecl@5..18
                KwVar@5..8 "var"
                Whitespace@8..9 " "
                NameList@9..11
                  Name@9..11
                    Identifier@9..10 "i"
                    Whitespace@10..11 " "
                Assign@11..13 ":="
                Whitespace@13..18 "\n    "
              FcnDecl@18..60
                FcnHeader@18..38
                  KwFunction@18..21 "fcn"
                  Whitespace@21..22 " "
                  Name@22..24
                    Identifier@22..23 "a"
                    Whitespace@23..24 " "
                  FcnResult@24..38
                    Colon@24..25 ":"
                    Whitespace@25..26 " "
                    PrimType@26..38
                      KwInt@26..29 "int"
                      Whitespace@29..38 "\n        "
                StmtList@38..55
                  AssertStmt@38..55
                    KwAssert@38..44 "assert"
                    Whitespace@44..45 " "
                    LiteralExpr@45..55
                      KwFalse@45..50 "false"
                      Whitespace@50..55 "\n    "
                EndGroup@55..60
                  KwEnd@55..58 "end"
                  Whitespace@58..59 " "
                  Identifier@59..60 "a"
            error at 18..21: expected expression, but found ’function’"#]],
    );
}

#[test]
fn parse_pre_stmt() {
    check(
        "pre 1 + 1 = 2",
        expect![[r#"
        Source@0..13
          PreStmt@0..13
            KwPre@0..3 "pre"
            Whitespace@3..4 " "
            BinaryExpr@4..13
              BinaryExpr@4..10
                LiteralExpr@4..6
                  IntLiteral@4..5 "1"
                  Whitespace@5..6 " "
                Plus@6..7 "+"
                Whitespace@7..8 " "
                LiteralExpr@8..10
                  IntLiteral@8..9 "1"
                  Whitespace@9..10 " "
              Equ@10..11 "="
              Whitespace@11..12 " "
              LiteralExpr@12..13
                IntLiteral@12..13 "2""#]],
    );
}

#[test]
fn recover_just_pre() {
    check(
        "pre",
        expect![[r#"
        Source@0..3
          PreStmt@0..3
            KwPre@0..3 "pre"
        error at 0..3: expected expression"#]],
    );
}

#[test]
fn recover_on_pre() {
    check(
        "var i := \npre true",
        expect![[r#"
        Source@0..18
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          PreStmt@10..18
            KwPre@10..13 "pre"
            Whitespace@13..14 " "
            LiteralExpr@14..18
              KwTrue@14..18 "true"
        error at 10..13: expected expression, but found ’pre’"#]],
    );
}

#[test]
fn parse_post_stmt() {
    check(
        "post 1 + 1 = 2",
        expect![[r#"
        Source@0..14
          PostStmt@0..14
            KwPost@0..4 "post"
            Whitespace@4..5 " "
            BinaryExpr@5..14
              BinaryExpr@5..11
                LiteralExpr@5..7
                  IntLiteral@5..6 "1"
                  Whitespace@6..7 " "
                Plus@7..8 "+"
                Whitespace@8..9 " "
                LiteralExpr@9..11
                  IntLiteral@9..10 "1"
                  Whitespace@10..11 " "
              Equ@11..12 "="
              Whitespace@12..13 " "
              LiteralExpr@13..14
                IntLiteral@13..14 "2""#]],
    );
}

#[test]
fn recover_just_post() {
    check(
        "post",
        expect![[r#"
        Source@0..4
          PostStmt@0..4
            KwPost@0..4 "post"
        error at 0..4: expected expression"#]],
    );
}

#[test]
fn recover_on_post() {
    check(
        "var i := \npost true",
        expect![[r#"
        Source@0..19
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          PostStmt@10..19
            KwPost@10..14 "post"
            Whitespace@14..15 " "
            LiteralExpr@15..19
              KwTrue@15..19 "true"
        error at 10..14: expected expression, but found ’post’"#]],
    );
}

#[test]
fn parse_init_stmt() {
    check(
        "init k := 1",
        expect![[r#"
        Source@0..11
          InitStmt@0..11
            KwInit@0..4 "init"
            Whitespace@4..5 " "
            InitVar@5..11
              Name@5..7
                Identifier@5..6 "k"
                Whitespace@6..7 " "
              Assign@7..9 ":="
              Whitespace@9..10 " "
              LiteralExpr@10..11
                IntLiteral@10..11 "1""#]],
    );
}

#[test]
fn parse_init_stmt_alt_asn() {
    check(
        "init k = 1",
        expect![[r#"
        Source@0..10
          InitStmt@0..10
            KwInit@0..4 "init"
            Whitespace@4..5 " "
            InitVar@5..10
              Name@5..7
                Identifier@5..6 "k"
                Whitespace@6..7 " "
              Equ@7..8 "="
              Whitespace@8..9 " "
              LiteralExpr@9..10
                IntLiteral@9..10 "1"
        warn at 7..8: ’=’ found, assuming it to be :="#]],
    );
}

#[test]
fn parse_init_stmt_multiple() {
    check(
        "init k := 1, l := k, m := 3",
        expect![[r#"
        Source@0..27
          InitStmt@0..27
            KwInit@0..4 "init"
            Whitespace@4..5 " "
            InitVar@5..11
              Name@5..7
                Identifier@5..6 "k"
                Whitespace@6..7 " "
              Assign@7..9 ":="
              Whitespace@9..10 " "
              LiteralExpr@10..11
                IntLiteral@10..11 "1"
            Comma@11..12 ","
            Whitespace@12..13 " "
            InitVar@13..19
              Name@13..15
                Identifier@13..14 "l"
                Whitespace@14..15 " "
              Assign@15..17 ":="
              Whitespace@17..18 " "
              NameExpr@18..19
                Name@18..19
                  Identifier@18..19 "k"
            Comma@19..20 ","
            Whitespace@20..21 " "
            InitVar@21..27
              Name@21..23
                Identifier@21..22 "m"
                Whitespace@22..23 " "
              Assign@23..25 ":="
              Whitespace@25..26 " "
              LiteralExpr@26..27
                IntLiteral@26..27 "3""#]],
    );
}

#[test]
fn recover_init_stmt_missing_asn() {
    check(
        "init a 1",
        expect![[r#"
        Source@0..8
          InitStmt@0..8
            KwInit@0..4 "init"
            Whitespace@4..5 " "
            InitVar@5..8
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              Error@7..8
                IntLiteral@7..8 "1"
        error at 7..8: expected ’:=’, but found int literal
        error at 7..8: expected expression"#]],
    );
}

#[test]
fn recover_init_stmt_missing_expr() {
    check(
        "init a :=",
        expect![[r#"
        Source@0..9
          InitStmt@0..9
            KwInit@0..4 "init"
            Whitespace@4..5 " "
            InitVar@5..9
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
              Assign@7..9 ":="
        error at 7..9: expected expression"#]],
    );
}

#[test]
fn recover_init_stmt_missing_name() {
    check(
        "init := 1",
        expect![[r#"
        Source@0..9
          InitStmt@0..9
            KwInit@0..4 "init"
            Whitespace@4..5 " "
            InitVar@5..9
              Assign@5..7 ":="
              Whitespace@7..8 " "
              LiteralExpr@8..9
                IntLiteral@8..9 "1"
        error at 5..7: expected identifier, but found ’:=’"#]],
    );
}

#[test]
fn recover_just_init() {
    check(
        "init",
        expect![[r#"
        Source@0..4
          InitStmt@0..4
            KwInit@0..4 "init"
            InitVar@4..4
        error at 0..4: expected identifier
        error at 0..4: expected ’:=’
        error at 0..4: expected expression"#]],
    );
}

#[test]
fn recover_on_init() {
    check(
        "loop end\ninit a := 1",
        expect![[r#"
        Source@0..20
          LoopStmt@0..9
            KwLoop@0..4 "loop"
            Whitespace@4..5 " "
            StmtList@5..5
            EndGroup@5..9
              KwEnd@5..8 "end"
              Whitespace@8..9 "\n"
          InitStmt@9..20
            KwInit@9..13 "init"
            Whitespace@13..14 " "
            InitVar@14..20
              Name@14..16
                Identifier@14..15 "a"
                Whitespace@15..16 " "
              Assign@16..18 ":="
              Whitespace@18..19 " "
              LiteralExpr@19..20
                IntLiteral@19..20 "1"
        error at 9..13: expected ’loop’, but found ’init’"#]],
    );
}

#[test]
fn parse_handler_stmt() {
    check(
        "handler (k) return end handler",
        expect![[r#"
        Source@0..30
          HandlerStmt@0..30
            KwHandler@0..7 "handler"
            Whitespace@7..8 " "
            LeftParen@8..9 "("
            Name@9..10
              Identifier@9..10 "k"
            RightParen@10..11 ")"
            Whitespace@11..12 " "
            StmtList@12..19
              ReturnStmt@12..19
                KwReturn@12..18 "return"
                Whitespace@18..19 " "
            EndGroup@19..30
              KwEnd@19..22 "end"
              Whitespace@22..23 " "
              KwHandler@23..30 "handler""#]],
    );
}

#[test]
fn recover_handler_stmt_missing_name() {
    check(
        "handler () return end handler",
        expect![[r#"
        Source@0..29
          HandlerStmt@0..29
            KwHandler@0..7 "handler"
            Whitespace@7..8 " "
            LeftParen@8..9 "("
            RightParen@9..10 ")"
            Whitespace@10..11 " "
            StmtList@11..18
              ReturnStmt@11..18
                KwReturn@11..17 "return"
                Whitespace@17..18 " "
            EndGroup@18..29
              KwEnd@18..21 "end"
              Whitespace@21..22 " "
              KwHandler@22..29 "handler"
        error at 9..10: expected identifier, but found ’)’"#]],
    );
}

#[test]
fn recover_handler_stmt_missing_left_paren() {
    check(
        "handler a) return end handler",
        expect![[r#"
        Source@0..29
          HandlerStmt@0..29
            KwHandler@0..7 "handler"
            Whitespace@7..8 " "
            Name@8..9
              Identifier@8..9 "a"
            RightParen@9..10 ")"
            Whitespace@10..11 " "
            StmtList@11..18
              ReturnStmt@11..18
                KwReturn@11..17 "return"
                Whitespace@17..18 " "
            EndGroup@18..29
              KwEnd@18..21 "end"
              Whitespace@21..22 " "
              KwHandler@22..29 "handler"
        error at 8..9: expected ’(’, but found identifier"#]],
    );
}

#[test]
fn recover_handler_stmt_missing_right_paren() {
    check(
        "handler (a return end handler",
        expect![[r#"
        Source@0..29
          HandlerStmt@0..29
            KwHandler@0..7 "handler"
            Whitespace@7..8 " "
            LeftParen@8..9 "("
            Name@9..11
              Identifier@9..10 "a"
              Whitespace@10..11 " "
            StmtList@11..18
              ReturnStmt@11..18
                KwReturn@11..17 "return"
                Whitespace@17..18 " "
            EndGroup@18..29
              KwEnd@18..21 "end"
              Whitespace@21..22 " "
              KwHandler@22..29 "handler"
        error at 11..17: expected ’)’, but found ’return’"#]],
    );
}

#[test]
fn recover_handler_stmt_missing_name_portion() {
    check(
        "handler return end handler",
        expect![[r#"
        Source@0..26
          HandlerStmt@0..26
            KwHandler@0..7 "handler"
            Whitespace@7..8 " "
            StmtList@8..15
              ReturnStmt@8..15
                KwReturn@8..14 "return"
                Whitespace@14..15 " "
            EndGroup@15..26
              KwEnd@15..18 "end"
              Whitespace@18..19 " "
              KwHandler@19..26 "handler"
        error at 8..14: expected ’(’, but found ’return’
        error at 8..14: expected identifier, but found ’return’
        error at 8..14: expected ’)’, but found ’return’"#]],
    );
}

#[test]
fn recover_handler_stmt_missing_tail() {
    check(
        "handler (a) return end",
        expect![[r#"
        Source@0..22
          HandlerStmt@0..22
            KwHandler@0..7 "handler"
            Whitespace@7..8 " "
            LeftParen@8..9 "("
            Name@9..10
              Identifier@9..10 "a"
            RightParen@10..11 ")"
            Whitespace@11..12 " "
            StmtList@12..19
              ReturnStmt@12..19
                KwReturn@12..18 "return"
                Whitespace@18..19 " "
            EndGroup@19..22
              KwEnd@19..22 "end"
        error at 19..22: expected ’handler’"#]],
    );
}

#[test]
fn recover_just_handler() {
    check(
        "handler",
        expect![[r#"
        Source@0..7
          HandlerStmt@0..7
            KwHandler@0..7 "handler"
            StmtList@7..7
            EndGroup@7..7
        error at 0..7: expected ’(’
        error at 0..7: expected identifier
        error at 0..7: expected ’)’
        error at 0..7: expected ’end’
        error at 0..7: expected ’handler’"#]],
    );
}

#[test]
fn recover_on_handler() {
    check(
        "var i := \nhandler (a) end handler",
        expect![[r#"
        Source@0..33
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          HandlerStmt@10..33
            KwHandler@10..17 "handler"
            Whitespace@17..18 " "
            LeftParen@18..19 "("
            Name@19..20
              Identifier@19..20 "a"
            RightParen@20..21 ")"
            Whitespace@21..22 " "
            StmtList@22..22
            EndGroup@22..33
              KwEnd@22..25 "end"
              Whitespace@25..26 " "
              KwHandler@26..33 "handler"
        error at 10..17: expected expression, but found ’handler’"#]],
    );
}

#[test]
fn parse_quit_stmt() {
    check(
        "quit",
        expect![[r#"
        Source@0..4
          QuitStmt@0..4
            KwQuit@0..4 "quit""#]],
    );
}

#[test]
fn parse_quit_stmt_opt_reason_lt() {
    check(
        "quit <",
        expect![[r#"
        Source@0..6
          QuitStmt@0..6
            KwQuit@0..4 "quit"
            Whitespace@4..5 " "
            QuitCause@5..6
              Less@5..6 "<""#]],
    );
}

#[test]
fn parse_quit_stmt_opt_reason_gt() {
    check(
        "quit >",
        expect![[r#"
        Source@0..6
          QuitStmt@0..6
            KwQuit@0..4 "quit"
            Whitespace@4..5 " "
            QuitCause@5..6
              Greater@5..6 ">""#]],
    );
}

#[test]
fn parse_quit_stmt_opt_quit_code() {
    check(
        "quit : 1 + 1",
        expect![[r#"
        Source@0..12
          QuitStmt@0..12
            KwQuit@0..4 "quit"
            Whitespace@4..5 " "
            Colon@5..6 ":"
            Whitespace@6..7 " "
            BinaryExpr@7..12
              LiteralExpr@7..9
                IntLiteral@7..8 "1"
                Whitespace@8..9 " "
              Plus@9..10 "+"
              Whitespace@10..11 " "
              LiteralExpr@11..12
                IntLiteral@11..12 "1""#]],
    );
}

#[test]
fn parse_quit_stmt_all_opt() {
    check(
        "quit < : 1 + 1",
        expect![[r#"
        Source@0..14
          QuitStmt@0..14
            KwQuit@0..4 "quit"
            Whitespace@4..5 " "
            QuitCause@5..7
              Less@5..6 "<"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            BinaryExpr@9..14
              LiteralExpr@9..11
                IntLiteral@9..10 "1"
                Whitespace@10..11 " "
              Plus@11..12 "+"
              Whitespace@12..13 " "
              LiteralExpr@13..14
                IntLiteral@13..14 "1""#]],
    );
}

#[test]
fn recover_quit_stmt_missing_code_expr() {
    check(
        "quit : ",
        expect![[r#"
        Source@0..7
          QuitStmt@0..7
            KwQuit@0..4 "quit"
            Whitespace@4..5 " "
            Colon@5..6 ":"
            Whitespace@6..7 " "
        error at 6..7: expected expression"#]],
    );
}

#[test]
fn recover_on_quit() {
    check(
        "var i := \nquit",
        expect![[r#"
        Source@0..14
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          QuitStmt@10..14
            KwQuit@10..14 "quit"
        error at 10..14: expected expression, but found ’quit’"#]],
    );
}

#[test]
fn parse_tag_stmt() {
    check(
        "tag a, 1 + 2",
        expect![[r#"
        Source@0..12
          TagStmt@0..12
            KwTag@0..3 "tag"
            Whitespace@3..4 " "
            NameExpr@4..5
              Name@4..5
                Identifier@4..5 "a"
            Comma@5..6 ","
            Whitespace@6..7 " "
            BinaryExpr@7..12
              LiteralExpr@7..9
                IntLiteral@7..8 "1"
                Whitespace@8..9 " "
              Plus@9..10 "+"
              Whitespace@10..11 " "
              LiteralExpr@11..12
                IntLiteral@11..12 "2""#]],
    );
}

#[test]
fn recover_tag_stmt_missing_tag_val() {
    check(
        "tag a, ",
        expect![[r#"
        Source@0..7
          TagStmt@0..7
            KwTag@0..3 "tag"
            Whitespace@3..4 " "
            NameExpr@4..5
              Name@4..5
                Identifier@4..5 "a"
            Comma@5..6 ","
            Whitespace@6..7 " "
        error at 6..7: expected expression"#]],
    );
}

#[test]
fn recover_tag_stmt_missing_tag_comma() {
    check(
        "tag a 1",
        expect![[r#"
        Source@0..7
          TagStmt@0..7
            KwTag@0..3 "tag"
            Whitespace@3..4 " "
            NameExpr@4..6
              Name@4..6
                Identifier@4..5 "a"
                Whitespace@5..6 " "
            Error@6..7
              IntLiteral@6..7 "1"
        error at 6..7: expected ’,’, but found int literal
        error at 6..7: expected expression"#]],
    );
}

#[test]
fn recover_tag_stmt_missing_ref() {
    check(
        "tag , 1",
        expect![[r#"
        Source@0..7
          TagStmt@0..7
            KwTag@0..3 "tag"
            Whitespace@3..4 " "
            Comma@4..5 ","
            Whitespace@5..6 " "
            LiteralExpr@6..7
              IntLiteral@6..7 "1"
        error at 4..5: expected expression, but found ’,’"#]],
    );
}

#[test]
fn recover_just_tag() {
    check(
        "tag",
        expect![[r#"
        Source@0..3
          TagStmt@0..3
            KwTag@0..3 "tag"
        error at 0..3: expected expression
        error at 0..3: expected ’,’
        error at 0..3: expected expression"#]],
    );
}

#[test]
fn recover_on_tag() {
    check(
        "var i := \ntag a, 1",
        expect![[r#"
        Source@0..18
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          TagStmt@10..18
            KwTag@10..13 "tag"
            Whitespace@13..14 " "
            NameExpr@14..15
              Name@14..15
                Identifier@14..15 "a"
            Comma@15..16 ","
            Whitespace@16..17 " "
            LiteralExpr@17..18
              IntLiteral@17..18 "1"
        error at 10..13: expected expression, but found ’tag’"#]],
    );
}

#[test]
fn parse_fork_stmt() {
    check(
        "fork a(pa, ra)",
        expect![[r#"
        Source@0..14
          ForkStmt@0..14
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            CallExpr@5..14
              NameExpr@5..6
                Name@5..6
                  Identifier@5..6 "a"
              ParamList@6..14
                LeftParen@6..7 "("
                Param@7..11
                  NameExpr@7..9
                    Name@7..9
                      Identifier@7..9 "pa"
                  Comma@9..10 ","
                  Whitespace@10..11 " "
                Param@11..13
                  NameExpr@11..13
                    Name@11..13
                      Identifier@11..13 "ra"
                RightParen@13..14 ")""#]],
    );
}

#[test]
fn parse_fork_stmt_empty_params() {
    check(
        "fork a()",
        expect![[r#"
        Source@0..8
          ForkStmt@0..8
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            CallExpr@5..8
              NameExpr@5..6
                Name@5..6
                  Identifier@5..6 "a"
              ParamList@6..8
                LeftParen@6..7 "("
                RightParen@7..8 ")""#]],
    );
}

#[test]
fn parse_fork_stmt_no_params() {
    check(
        "fork a",
        expect![[r#"
        Source@0..6
          ForkStmt@0..6
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..6
              Name@5..6
                Identifier@5..6 "a""#]],
    );
}

#[test]
fn parse_fork_stmt_opt_status() {
    check(
        "fork a : stat",
        expect![[r#"
        Source@0..13
          ForkStmt@0..13
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..7
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ForkStatus@9..13
              NameExpr@9..13
                Name@9..13
                  Identifier@9..13 "stat""#]],
    );
}

#[test]
fn parse_fork_stmt_opt_stack_size() {
    check(
        "fork a : stat, 24",
        expect![[r#"
        Source@0..17
          ForkStmt@0..17
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..7
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ForkStatus@9..13
              NameExpr@9..13
                Name@9..13
                  Identifier@9..13 "stat"
            Comma@13..14 ","
            Whitespace@14..15 " "
            StackSize@15..17
              LiteralExpr@15..17
                IntLiteral@15..17 "24""#]],
    );
}

#[test]
fn parse_fork_stmt_opt_process_ref() {
    check(
        "fork a : stat, 24, a",
        expect![[r#"
        Source@0..20
          ForkStmt@0..20
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..7
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ForkStatus@9..13
              NameExpr@9..13
                Name@9..13
                  Identifier@9..13 "stat"
            Comma@13..14 ","
            Whitespace@14..15 " "
            StackSize@15..17
              LiteralExpr@15..17
                IntLiteral@15..17 "24"
            Comma@17..18 ","
            Whitespace@18..19 " "
            ProcessDesc@19..20
              NameExpr@19..20
                Name@19..20
                  Identifier@19..20 "a""#]],
    );
}

#[test]
fn recover_fork_stmt_process_ref_missing_ref() {
    check(
        "fork a : stat, 24, ",
        expect![[r#"
        Source@0..19
          ForkStmt@0..19
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..7
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ForkStatus@9..13
              NameExpr@9..13
                Name@9..13
                  Identifier@9..13 "stat"
            Comma@13..14 ","
            Whitespace@14..15 " "
            StackSize@15..17
              LiteralExpr@15..17
                IntLiteral@15..17 "24"
            Comma@17..18 ","
            Whitespace@18..19 " "
            ProcessDesc@19..19
        error at 18..19: expected expression"#]],
    );
}

#[test]
fn recover_fork_stmt_process_ref_missing_stack_size_expr() {
    check(
        "fork a : stat, , a",
        expect![[r#"
        Source@0..18
          ForkStmt@0..18
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..7
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ForkStatus@9..13
              NameExpr@9..13
                Name@9..13
                  Identifier@9..13 "stat"
            Comma@13..14 ","
            Whitespace@14..15 " "
            StackSize@15..15
            Comma@15..16 ","
            Whitespace@16..17 " "
            ProcessDesc@17..18
              NameExpr@17..18
                Name@17..18
                  Identifier@17..18 "a"
        error at 15..16: expected expression, but found ’,’"#]],
    );
}

#[test]
fn recover_fork_stmt_process_ref_missing_stat_ref() {
    check(
        "fork a : , , a",
        expect![[r#"
        Source@0..14
          ForkStmt@0..14
            KwFork@0..4 "fork"
            Whitespace@4..5 " "
            NameExpr@5..7
              Name@5..7
                Identifier@5..6 "a"
                Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ForkStatus@9..9
            Comma@9..10 ","
            Whitespace@10..11 " "
            StackSize@11..11
            Comma@11..12 ","
            Whitespace@12..13 " "
            ProcessDesc@13..14
              NameExpr@13..14
                Name@13..14
                  Identifier@13..14 "a"
        error at 9..10: expected expression, but found ’,’
        error at 11..12: expected expression, but found ’,’"#]],
    );
}

#[test]
fn recover_just_fork() {
    check(
        "fork",
        expect![[r#"
        Source@0..4
          ForkStmt@0..4
            KwFork@0..4 "fork"
        error at 0..4: expected expression"#]],
    );
}

#[test]
fn recover_on_fork() {
    check(
        "var i := \nfork a",
        expect![[r#"
        Source@0..16
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          ForkStmt@10..16
            KwFork@10..14 "fork"
            Whitespace@14..15 " "
            NameExpr@15..16
              Name@15..16
                Identifier@15..16 "a"
        error at 10..14: expected expression, but found ’fork’"#]],
    );
}

#[test]
fn parse_new_stmt() {
    check(
        "new a",
        expect![[r#"
        Source@0..5
          NewStmt@0..5
            KwNew@0..3 "new"
            Whitespace@3..4 " "
            ExprList@4..5
              NameExpr@4..5
                Name@4..5
                  Identifier@4..5 "a""#]],
    );
}

#[test]
fn parse_new_stmt_opt_pointer_spec() {
    check(
        "new A, ptr",
        expect![[r#"
        Source@0..10
          NewStmt@0..10
            KwNew@0..3 "new"
            Whitespace@3..4 " "
            ExprList@4..10
              NameExpr@4..5
                Name@4..5
                  Identifier@4..5 "A"
              Comma@5..6 ","
              Whitespace@6..7 " "
              NameExpr@7..10
                Name@7..10
                  Identifier@7..10 "ptr""#]],
    );
}

#[test]
fn parse_new_stmt_resize_array() {
    check(
        "new ary, 1, 2, 3, 4",
        expect![[r#"
        Source@0..19
          NewStmt@0..19
            KwNew@0..3 "new"
            Whitespace@3..4 " "
            ExprList@4..19
              NameExpr@4..7
                Name@4..7
                  Identifier@4..7 "ary"
              Comma@7..8 ","
              Whitespace@8..9 " "
              LiteralExpr@9..10
                IntLiteral@9..10 "1"
              Comma@10..11 ","
              Whitespace@11..12 " "
              LiteralExpr@12..13
                IntLiteral@12..13 "2"
              Comma@13..14 ","
              Whitespace@14..15 " "
              LiteralExpr@15..16
                IntLiteral@15..16 "3"
              Comma@16..17 ","
              Whitespace@17..18 " "
              LiteralExpr@18..19
                IntLiteral@18..19 "4""#]],
    );
}

#[test]
fn recover_just_new() {
    check(
        "new",
        expect![[r#"
        Source@0..3
          NewStmt@0..3
            KwNew@0..3 "new"
            ExprList@3..3
        error at 0..3: expected expression"#]],
    );
}

#[test]
fn recover_on_new() {
    check(
        "var i := \nnew a",
        expect![[r#"
        Source@0..15
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          NewStmt@10..15
            KwNew@10..13 "new"
            Whitespace@13..14 " "
            ExprList@14..15
              NameExpr@14..15
                Name@14..15
                  Identifier@14..15 "a"
        error at 10..13: expected expression, but found ’new’"#]],
    );
}

#[test]
fn parse_free_stmt() {
    check(
        "free a",
        expect![[r#"
        Source@0..6
          FreeStmt@0..6
            KwFree@0..4 "free"
            Whitespace@4..5 " "
            ExprList@5..6
              NameExpr@5..6
                Name@5..6
                  Identifier@5..6 "a""#]],
    );
}

#[test]
fn parse_free_stmt_opt_pointer_spec() {
    check(
        "free A, ptr",
        expect![[r#"
        Source@0..11
          FreeStmt@0..11
            KwFree@0..4 "free"
            Whitespace@4..5 " "
            ExprList@5..11
              NameExpr@5..6
                Name@5..6
                  Identifier@5..6 "A"
              Comma@6..7 ","
              Whitespace@7..8 " "
              NameExpr@8..11
                Name@8..11
                  Identifier@8..11 "ptr""#]],
    );
}

#[test]
fn parse_free_stmt_multiple_exprs() {
    // reject during lowering
    check(
        "free ary, 1, 2, 3, 4",
        expect![[r#"
        Source@0..20
          FreeStmt@0..20
            KwFree@0..4 "free"
            Whitespace@4..5 " "
            ExprList@5..20
              NameExpr@5..8
                Name@5..8
                  Identifier@5..8 "ary"
              Comma@8..9 ","
              Whitespace@9..10 " "
              LiteralExpr@10..11
                IntLiteral@10..11 "1"
              Comma@11..12 ","
              Whitespace@12..13 " "
              LiteralExpr@13..14
                IntLiteral@13..14 "2"
              Comma@14..15 ","
              Whitespace@15..16 " "
              LiteralExpr@16..17
                IntLiteral@16..17 "3"
              Comma@17..18 ","
              Whitespace@18..19 " "
              LiteralExpr@19..20
                IntLiteral@19..20 "4""#]],
    );
}

#[test]
fn recover_just_free() {
    check(
        "free",
        expect![[r#"
        Source@0..4
          FreeStmt@0..4
            KwFree@0..4 "free"
            ExprList@4..4
        error at 0..4: expected expression"#]],
    );
}

#[test]
fn recover_on_free() {
    check(
        "var i := \nfree a",
        expect![[r#"
        Source@0..16
          ConstVarDecl@0..10
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            NameList@4..6
              Name@4..6
                Identifier@4..5 "i"
                Whitespace@5..6 " "
            Assign@6..8 ":="
            Whitespace@8..10 " \n"
          FreeStmt@10..16
            KwFree@10..14 "free"
            Whitespace@14..15 " "
            ExprList@15..16
              NameExpr@15..16
                Name@15..16
                  Identifier@15..16 "a"
        error at 10..14: expected expression, but found ’free’"#]],
    );
}
