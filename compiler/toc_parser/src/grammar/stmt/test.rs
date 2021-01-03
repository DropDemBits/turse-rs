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
                  IntLiteral@4..5 "1""#]],
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
                  KwIf@49..51 "if""#]],
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
                  KwEndIf@13..18 "endif""#]],
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
                  KwIf@21..23 "if""#]],
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
                  KwIf@19..21 "if""#]],
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
                  KwEndIf@16..21 "endif""#]],
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
            error at 10..16: expected expression, but found ’elseif’"#]],
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
            error at 10..14: expected expression, but found ’elif’"#]],
    );
}
