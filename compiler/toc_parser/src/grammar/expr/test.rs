//! Expression testing
use crate::check;
use expect_test::expect;

#[test]
fn parse_ident_use() {
    check(
        "_:=a",
        expect![[r#"
            Source@0..4
              StmtList@0..4
                AssignStmt@0..4
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NameExpr@3..4
                    Name@3..4
                      Identifier@3..4 "a""#]],
    );
    check(
        "_:=abcde0123",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NameExpr@3..12
                    Name@3..12
                      Identifier@3..12 "abcde0123""#]],
    );
}

#[test]
fn parse_int_literal() {
    check(
        "_:=01234560",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..11
                    IntLiteral@3..11 "01234560""#]],
    );

    // Invalid literals are reported during HIR lowering

    // Overflow
    check(
        "_:=99999999999999999999",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                AssignStmt@0..23
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..23
                    IntLiteral@3..23 "99999999999999999999""#]],
    );

    // Digit cutoff
    check(
        "_:=999a999",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..6
                    IntLiteral@3..6 "999"
                CallStmt@6..10
                  NameExpr@6..10
                    Name@6..10
                      Identifier@6..10 "a999""#]],
    );
}

#[test]
fn parse_radix_literal() {
    // Examples
    check(
        "_:=16#EABC",
        expect![[r##"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..10
                    RadixLiteral@3..10 "16#EABC""##]],
    );
    check(
        "_:=02#1100",
        expect![[r##"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..10
                    RadixLiteral@3..10 "02#1100""##]],
    );

    // Errors

    // Overflow
    check(
        "_:=10#99999999999999999999",
        expect![[r##"
            Source@0..26
              StmtList@0..26
                AssignStmt@0..26
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..26
                    RadixLiteral@3..26 "10#99999999999999999999""##]],
    );

    // Invalid literals are reported during HIR lowering

    // No digits
    check(
        "_:=30#",
        expect![[r##"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..6
                    RadixLiteral@3..6 "30#""##]],
    );

    // Out of range (> 36)
    check(
        "_:=37#asda",
        expect![[r##"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..10
                    RadixLiteral@3..10 "37#asda""##]],
    );

    // Out of range (< 2)
    check(
        "_:=0#0000",
        expect![[r##"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..9
                    RadixLiteral@3..9 "0#0000""##]],
    );
    check(
        "_:=1#0000",
        expect![[r##"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..9
                    RadixLiteral@3..9 "1#0000""##]],
    );

    // Out of range (= overflow)
    check(
        "_:=18446744073709551616#0000",
        expect![[r##"
            Source@0..28
              StmtList@0..28
                AssignStmt@0..28
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..28
                    RadixLiteral@3..28 "18446744073709551616# ...""##]],
    );

    // Invalid digit
    check(
        "_:=10#999a999",
        expect![[r##"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..13
                    RadixLiteral@3..13 "10#999a999""##]],
    );
}

#[test]
fn parse_real_literal() {
    // Leading dot
    check(
        "_:=.12345",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..9
                    RealLiteral@3..9 ".12345""#]],
    );
    check(
        "_:=.12345.6789",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..9
                    RealLiteral@3..9 ".12345"
                CallStmt@9..14
                  LiteralExpr@9..14
                    RealLiteral@9..14 ".6789""#]],
    );

    // Valid variations
    check(
        "_:=1.",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..5
                    RealLiteral@3..5 "1.""#]],
    );
    check(
        "_:=100.00",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..9
                    RealLiteral@3..9 "100.00""#]],
    );
    check(
        "_:=100.00e10",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..12
                    RealLiteral@3..12 "100.00e10""#]],
    );
    check(
        "_:=100.00e100",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..13
                    RealLiteral@3..13 "100.00e100""#]],
    );

    // Negative and positive exponents are valid
    check(
        "_:=100.00e-100",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..14
                    RealLiteral@3..14 "100.00e-100""#]],
    );
    check(
        "_:=100.00e+100",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..14
                    RealLiteral@3..14 "100.00e+100""#]],
    );
    check(
        "_:=1e100",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..8
                    RealLiteral@3..8 "1e100""#]],
    );

    // Errors:

    // Invalid format
    check(
        "_:=1e+",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..6
                    RealLiteral@3..6 "1e+""#]],
    );
    check(
        "_:=1e-",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..6
                    RealLiteral@3..6 "1e-""#]],
    );
    check(
        "_:=1e",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..5
                    RealLiteral@3..5 "1e""#]],
    );

    // Too big
    check(
        "_:=1e600",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..8
                    RealLiteral@3..8 "1e600""#]],
    );
}

#[test]
fn parse_string_literal() {
    check(
        r#"_:="hello""#,
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..10
                    StringLiteral@3..10 "\"hello\"""#]],
    );
}

#[test]
fn parse_char_literal() {
    check(
        r#"_:='hello'"#,
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..10
                    CharLiteral@3..10 "'hello'""#]],
    );
}

#[test]
fn parse_boolean_literal() {
    check(
        r#"_:=true"#,
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..7
                    KwTrue@3..7 "true""#]],
    );
    check(
        r#"_:=false"#,
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..8
                    KwFalse@3..8 "false""#]],
    );
}

#[test]
fn parse_bin_expr_simple() {
    check(
        "_:=1+2",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..6
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Plus@4..5 "+"
                    LiteralExpr@5..6
                      IntLiteral@5..6 "2""#]],
    );
}

#[test]
fn parse_bin_expr_with_left_assoc() {
    check(
        "_:=1+2+3+4",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    BinaryExpr@3..8
                      BinaryExpr@3..6
                        LiteralExpr@3..4
                          IntLiteral@3..4 "1"
                        Plus@4..5 "+"
                        LiteralExpr@5..6
                          IntLiteral@5..6 "2"
                      Plus@6..7 "+"
                      LiteralExpr@7..8
                        IntLiteral@7..8 "3"
                    Plus@8..9 "+"
                    LiteralExpr@9..10
                      IntLiteral@9..10 "4""#]],
    );
}

#[test]
fn parse_bin_expr_with_mixed_binding_power() {
    check(
        "_:=1+2*3-4/5",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..12
                    BinaryExpr@3..8
                      LiteralExpr@3..4
                        IntLiteral@3..4 "1"
                      Plus@4..5 "+"
                      BinaryExpr@5..8
                        LiteralExpr@5..6
                          IntLiteral@5..6 "2"
                        Star@6..7 "*"
                        LiteralExpr@7..8
                          IntLiteral@7..8 "3"
                    Minus@8..9 "-"
                    BinaryExpr@9..12
                      LiteralExpr@9..10
                        IntLiteral@9..10 "4"
                      Slash@10..11 "/"
                      LiteralExpr@11..12
                        IntLiteral@11..12 "5""#]],
    );
}

#[test]
fn parse_expr_with_leading_ws() {
    check(
        "_:=     16#480",
        expect![[r##"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Whitespace@3..8 "     "
                  LiteralExpr@8..14
                    RadixLiteral@8..14 "16#480""##]],
    );
}

#[test]
fn parse_expr_with_trailing_ws() {
    check(
        "_:=1.0e5    ",
        expect![[r#"
            Source@0..12
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  LiteralExpr@3..8
                    RealLiteral@3..8 "1.0e5"
              Whitespace@8..12 "    ""#]],
    );
}

#[test]
fn parse_expr_with_surrounding_ws() {
    check(
        "_:=  12345    ",
        expect![[r#"
            Source@0..14
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Whitespace@3..5 "  "
                  LiteralExpr@5..10
                    IntLiteral@5..10 "12345"
              Whitespace@10..14 "    ""#]],
    );
}

#[test]
fn parse_bin_expr_with_ws() {
    check(
        "_:=  1 + 2 - 3* 4    ",
        expect![[r#"
            Source@0..21
              StmtList@0..17
                AssignStmt@0..17
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Whitespace@3..5 "  "
                  BinaryExpr@5..17
                    BinaryExpr@5..10
                      LiteralExpr@5..6
                        IntLiteral@5..6 "1"
                      Whitespace@6..7 " "
                      Plus@7..8 "+"
                      Whitespace@8..9 " "
                      LiteralExpr@9..10
                        IntLiteral@9..10 "2"
                    Whitespace@10..11 " "
                    Minus@11..12 "-"
                    Whitespace@12..13 " "
                    BinaryExpr@13..17
                      LiteralExpr@13..14
                        IntLiteral@13..14 "3"
                      Star@14..15 "*"
                      Whitespace@15..16 " "
                      LiteralExpr@16..17
                        IntLiteral@16..17 "4"
              Whitespace@17..21 "    ""#]],
    );
}

#[test]
fn parse_exprs_with_comments() {
    check(
        r#"_:=
        1
        + 2 % interspersed line comment
        + /* random interleaved comment */ 3

        _:=3 - 2 % step down two
        + 1 % go back up 1"#,
        expect![[r#"
            Source@0..159
              StmtList@0..144
                AssignStmt@0..98
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Whitespace@3..12 "\n        "
                  BinaryExpr@12..98
                    BinaryExpr@12..25
                      LiteralExpr@12..13
                        IntLiteral@12..13 "1"
                      Whitespace@13..22 "\n        "
                      Plus@22..23 "+"
                      Whitespace@23..24 " "
                      LiteralExpr@24..25
                        IntLiteral@24..25 "2"
                    Whitespace@25..26 " "
                    Comment@26..53 "% interspersed line c ..."
                    Whitespace@53..62 "\n        "
                    Plus@62..63 "+"
                    Whitespace@63..64 " "
                    Comment@64..96 "/* random interleaved ..."
                    Whitespace@96..97 " "
                    LiteralExpr@97..98
                      IntLiteral@97..98 "3"
                Whitespace@98..108 "\n\n        "
                AssignStmt@108..144
                  NameExpr@108..109
                    Name@108..109
                      Identifier@108..109 "_"
                  AsnOp@109..111
                    Assign@109..111 ":="
                  BinaryExpr@111..144
                    BinaryExpr@111..116
                      LiteralExpr@111..112
                        IntLiteral@111..112 "3"
                      Whitespace@112..113 " "
                      Minus@113..114 "-"
                      Whitespace@114..115 " "
                      LiteralExpr@115..116
                        IntLiteral@115..116 "2"
                    Whitespace@116..117 " "
                    Comment@117..132 "% step down two"
                    Whitespace@132..141 "\n        "
                    Plus@141..142 "+"
                    Whitespace@142..143 " "
                    LiteralExpr@143..144
                      IntLiteral@143..144 "1"
              Whitespace@144..145 " "
              Comment@145..159 "% go back up 1""#]],
    );
}

#[test]
fn parse_simple_infix() {
    check(
        "_:=1 => 2",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Imply@5..7 "=>"
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      IntLiteral@8..9 "2""#]],
    );
    check(
        "_:=1 or 2",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwOr@5..7 "or"
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      IntLiteral@8..9 "2""#]],
    );
    check(
        "_:=1 | 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Pipe@5..6 "|"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 and 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwAnd@5..8 "and"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
    check(
        "_:=1 & 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Ampersand@5..6 "&"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 < 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Less@5..6 "<"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 > 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Greater@5..6 ">"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 <= 2",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    LessEqu@5..7 "<="
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      IntLiteral@8..9 "2""#]],
    );
    check(
        "_:=1 >= 2",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    GreaterEqu@5..7 ">="
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      IntLiteral@8..9 "2""#]],
    );
    check(
        "_:=1 + 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Plus@5..6 "+"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 - 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Minus@5..6 "-"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 * 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Star@5..6 "*"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 / 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Slash@5..6 "/"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2""#]],
    );
    check(
        "_:=1 div 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwDiv@5..8 "div"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
    check(
        "_:=1 rem 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwRem@5..8 "rem"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
    check(
        "_:=1 mod 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwMod@5..8 "mod"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
    check(
        "_:=1 shl 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwShl@5..8 "shl"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
    check(
        "_:=1 shr 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwShr@5..8 "shr"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
    check(
        "_:=1 ** 2",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Exp@5..7 "**"
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      IntLiteral@8..9 "2""#]],
    );
    // call, dot, and arrow are complex operators
    // not in and not eq are compound infix
}

#[test]
fn exp_right_associativity() {
    check(
        "_:=2 ** 3 ** 4",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..14
                    LiteralExpr@3..4
                      IntLiteral@3..4 "2"
                    Whitespace@4..5 " "
                    Exp@5..7 "**"
                    Whitespace@7..8 " "
                    BinaryExpr@8..14
                      LiteralExpr@8..9
                        IntLiteral@8..9 "3"
                      Whitespace@9..10 " "
                      Exp@10..12 "**"
                      Whitespace@12..13 " "
                      LiteralExpr@13..14
                        IntLiteral@13..14 "4""#]],
    );
}

#[test]
fn parse_ne_form1() {
    check(
        "_:=1 ~= 2",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotEq@5..7
                      Tilde@5..6 "~"
                      Equ@6..7 "="
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      IntLiteral@8..9 "2""#]],
    );
}

#[test]
fn parse_ne_form2() {
    check(
        "_:=1 ~ = 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotEq@5..8
                      Tilde@5..6 "~"
                      Whitespace@6..7 " "
                      Equ@7..8 "="
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2""#]],
    );
}

#[test]
fn parse_ne_form3() {
    check(
        "_:=1 not = 2",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..12
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotEq@5..10
                      KwNot@5..8 "not"
                      Whitespace@8..9 " "
                      Equ@9..10 "="
                    Whitespace@10..11 " "
                    LiteralExpr@11..12
                      IntLiteral@11..12 "2""#]],
    );
}

#[test]
fn parse_ne_form4() {
    check(
        "_:=1 not= 2",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..11
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotEq@5..9
                      KwNot@5..8 "not"
                      Equ@8..9 "="
                    Whitespace@9..10 " "
                    LiteralExpr@10..11
                      IntLiteral@10..11 "2""#]],
    );
}

#[test]
fn parse_in() {
    check(
        "_:=1 in a",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    KwIn@5..7 "in"
                    Whitespace@7..8 " "
                    NameExpr@8..9
                      Name@8..9
                        Identifier@8..9 "a""#]],
    );
}

#[test]
fn parse_not_in_form1() {
    check(
        "_:=1 not in a",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..13
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotIn@5..11
                      KwNot@5..8 "not"
                      Whitespace@8..9 " "
                      KwIn@9..11 "in"
                    Whitespace@11..12 " "
                    NameExpr@12..13
                      Name@12..13
                        Identifier@12..13 "a""#]],
    );
}

#[test]
fn parse_not_in_form2() {
    check(
        "_:=1 ~in a",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotIn@5..8
                      Tilde@5..6 "~"
                      KwIn@6..8 "in"
                    Whitespace@8..9 " "
                    NameExpr@9..10
                      Name@9..10
                        Identifier@9..10 "a""#]],
    );
}

#[test]
fn parse_not_in_form3() {
    check(
        "_:=1 ~ in a",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..11
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    NotIn@5..9
                      Tilde@5..6 "~"
                      Whitespace@6..7 " "
                      KwIn@7..9 "in"
                    Whitespace@9..10 " "
                    NameExpr@10..11
                      Name@10..11
                        Identifier@10..11 "a""#]],
    );
}

#[test]
fn recover_tilde_as_infix() {
    check(
        "_:=1 ~ 2",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..8
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Error@5..6
                      Tilde@5..6 "~"
                    Whitespace@6..7 " "
                    LiteralExpr@7..8
                      IntLiteral@7..8 "2"
            error in file FileId(1) at 7..8: unexpected token
            | error in file FileId(1) for 7..8: expected ‘in’ or ‘=’, but found int literal"#]],
    );
}

#[test]
fn recover_not_as_infix() {
    check(
        "_:=1 not 2",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    Error@5..8
                      KwNot@5..8 "not"
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      IntLiteral@9..10 "2"
            error in file FileId(1) at 9..10: unexpected token
            | error in file FileId(1) for 9..10: expected ‘in’ or ‘=’, but found int literal"#]],
    );
}

#[test]
fn parse_simple_prefix() {
    check(
        "_:=-10",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  UnaryExpr@3..6
                    Minus@3..4 "-"
                    LiteralExpr@4..6
                      IntLiteral@4..6 "10""#]],
    );
    check(
        "_:=+a",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  UnaryExpr@3..5
                    Plus@3..4 "+"
                    NameExpr@4..5
                      Name@4..5
                        Identifier@4..5 "a""#]],
    );
    check(
        "_:=^a",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  DerefExpr@3..5
                    Caret@3..4 "^"
                    NameExpr@4..5
                      Name@4..5
                        Identifier@4..5 "a""#]],
    );
    check(
        "_:=#a",
        expect![[r##"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NatCheatExpr@3..5
                    Pound@3..4 "#"
                    NameExpr@4..5
                      Name@4..5
                        Identifier@4..5 "a""##]],
    );
    check(
        "_:=~a",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  UnaryExpr@3..5
                    Tilde@3..4 "~"
                    NameExpr@4..5
                      Name@4..5
                        Identifier@4..5 "a""#]],
    );
    check(
        "_:=not a",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  UnaryExpr@3..8
                    KwNot@3..6 "not"
                    Whitespace@6..7 " "
                    NameExpr@7..8
                      Name@7..8
                        Identifier@7..8 "a""#]],
    );
}

#[test]
fn negation_over_arithmetic() {
    check(
        "_:=-10+20",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    UnaryExpr@3..6
                      Minus@3..4 "-"
                      LiteralExpr@4..6
                        IntLiteral@4..6 "10"
                    Plus@6..7 "+"
                    LiteralExpr@7..9
                      IntLiteral@7..9 "20""#]],
    );
}

#[test]
fn recover_infix_missing_rhs() {
    check(
        "_:=not",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  UnaryExpr@3..6
                    KwNot@3..6 "not"
            error in file FileId(1) at 3..6: unexpected end of file
            | error in file FileId(1) for 3..6: expected expression after here"#]],
    );
}

#[test]
fn parse_nested_parens() {
    check(
        "_:=(((20)))",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ParenExpr@3..11
                    LeftParen@3..4 "("
                    ParenExpr@4..10
                      LeftParen@4..5 "("
                      ParenExpr@5..9
                        LeftParen@5..6 "("
                        LiteralExpr@6..8
                          IntLiteral@6..8 "20"
                        RightParen@8..9 ")"
                      RightParen@9..10 ")"
                    RightParen@10..11 ")""#]],
    );
}

#[test]
fn parens_alter_precedence() {
    check(
        "_:=1/(2+3)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Slash@4..5 "/"
                    ParenExpr@5..10
                      LeftParen@5..6 "("
                      BinaryExpr@6..9
                        LiteralExpr@6..7
                          IntLiteral@6..7 "2"
                        Plus@7..8 "+"
                        LiteralExpr@8..9
                          IntLiteral@8..9 "3"
                      RightParen@9..10 ")""#]],
    );
}

#[test]
fn just_parens() {
    // Just parens are allowed in the CST, but will be rejected when lowering the AST
    check(
        "(1)",
        expect![[r#"
            Source@0..3
              StmtList@0..3
                CallStmt@0..3
                  ParenExpr@0..3
                    LeftParen@0..1 "("
                    LiteralExpr@1..2
                      IntLiteral@1..2 "1"
                    RightParen@2..3 ")""#]],
    );
}

#[test]
fn recover_just_right_paren() {
    check(
        "_:=)",
        expect![[r#"
            Source@0..4
              StmtList@0..4
                AssignStmt@0..4
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Error@3..4
                    RightParen@3..4 ")"
            error in file FileId(1) at 3..4: unexpected token
            | error in file FileId(1) for 3..4: expected expression, but found ‘)’"#]],
    )
}

#[test]
fn recover_too_many_right_parens() {
    check(
        "_:=(1))",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ParenExpr@3..6
                    LeftParen@3..4 "("
                    LiteralExpr@4..5
                      IntLiteral@4..5 "1"
                    RightParen@5..6 ")"
                Error@6..7
                  RightParen@6..7 ")"
            error in file FileId(1) at 6..7: unexpected token
            | error in file FileId(1) for 6..7: expected statement, but found ‘)’"#]],
    )
}

#[test]
fn recover_missing_closing_paren() {
    check(
        "_:=(1",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ParenExpr@3..5
                    LeftParen@3..4 "("
                    LiteralExpr@4..5
                      IntLiteral@4..5 "1"
            error in file FileId(1) at 4..5: unexpected end of file
            | error in file FileId(1) for 4..5: expected ‘)’ after here"#]],
    );
}

#[test]
fn recover_missing_closing_paren_and_rhs() {
    check(
        "_:=(1+",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ParenExpr@3..6
                    LeftParen@3..4 "("
                    BinaryExpr@4..6
                      LiteralExpr@4..5
                        IntLiteral@4..5 "1"
                      Plus@5..6 "+"
            error in file FileId(1) at 5..6: unexpected end of file
            | error in file FileId(1) for 5..6: expected expression after here"#]],
    );
}

#[test]
fn recover_missing_rhs() {
    check(
        "_:=1+",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..5
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Plus@4..5 "+"
            error in file FileId(1) at 4..5: unexpected end of file
            | error in file FileId(1) for 4..5: expected expression after here"#]],
    );
}

// Field exprs

#[test]
fn parse_field_expr() {
    check(
        "_:=a.b",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  FieldExpr@3..6
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    Dot@4..5 "."
                    Name@5..6
                      Identifier@5..6 "b""#]],
    );
}

#[test]
fn chained_field_expr() {
    check(
        "_:=a.b.c.d",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  FieldExpr@3..10
                    FieldExpr@3..8
                      FieldExpr@3..6
                        NameExpr@3..4
                          Name@3..4
                            Identifier@3..4 "a"
                        Dot@4..5 "."
                        Name@5..6
                          Identifier@5..6 "b"
                      Dot@6..7 "."
                      Name@7..8
                        Identifier@7..8 "c"
                    Dot@8..9 "."
                    Name@9..10
                      Identifier@9..10 "d""#]],
    );
}

#[test]
fn recover_field_expr_missing_field() {
    check(
        "_:=a.",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  FieldExpr@3..5
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    Dot@4..5 "."
            error in file FileId(1) at 4..5: unexpected end of file
            | error in file FileId(1) for 4..5: expected identifier after here"#]],
    );
}

#[test]
fn recover_field_missing_closing_paren_and_field() {
    check(
        "_:=(a.",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ParenExpr@3..6
                    LeftParen@3..4 "("
                    FieldExpr@4..6
                      NameExpr@4..5
                        Name@4..5
                          Identifier@4..5 "a"
                      Dot@5..6 "."
            error in file FileId(1) at 5..6: unexpected end of file
            | error in file FileId(1) for 5..6: expected identifier after here"#]],
    );
}

// Arrow exprs

#[test]
fn parse_arrow_expr() {
    check(
        "_:=a->b",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ArrowExpr@3..7
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    Arrow@4..6 "->"
                    Name@6..7
                      Identifier@6..7 "b""#]],
    );
}

#[test]
fn chained_arrow_expr() {
    check(
        "_:=a->b->c->d",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ArrowExpr@3..13
                    ArrowExpr@3..10
                      ArrowExpr@3..7
                        NameExpr@3..4
                          Name@3..4
                            Identifier@3..4 "a"
                        Arrow@4..6 "->"
                        Name@6..7
                          Identifier@6..7 "b"
                      Arrow@7..9 "->"
                      Name@9..10
                        Identifier@9..10 "c"
                    Arrow@10..12 "->"
                    Name@12..13
                      Identifier@12..13 "d""#]],
    );
}

#[test]
fn recover_arrow_expr_missing_field() {
    check(
        "_:=a->",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ArrowExpr@3..6
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    Arrow@4..6 "->"
            error in file FileId(1) at 4..6: unexpected end of file
            | error in file FileId(1) for 4..6: expected identifier after here"#]],
    );
}

#[test]
fn recover_arrow_missing_closing_paren_and_field() {
    check(
        "_:=(a->",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ParenExpr@3..7
                    LeftParen@3..4 "("
                    ArrowExpr@4..7
                      NameExpr@4..5
                        Name@4..5
                          Identifier@4..5 "a"
                      Arrow@5..7 "->"
            error in file FileId(1) at 5..7: unexpected end of file
            | error in file FileId(1) for 5..7: expected identifier after here"#]],
    );
}

#[test]
fn chained_field_and_arrow_expr() {
    check(
        "_:=a.b->c->d.e->f.g->h",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                AssignStmt@0..22
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ArrowExpr@3..22
                    FieldExpr@3..19
                      ArrowExpr@3..17
                        FieldExpr@3..14
                          ArrowExpr@3..12
                            ArrowExpr@3..9
                              FieldExpr@3..6
                                NameExpr@3..4
                                  Name@3..4
                                    Identifier@3..4 "a"
                                Dot@4..5 "."
                                Name@5..6
                                  Identifier@5..6 "b"
                              Arrow@6..8 "->"
                              Name@8..9
                                Identifier@8..9 "c"
                            Arrow@9..11 "->"
                            Name@11..12
                              Identifier@11..12 "d"
                          Dot@12..13 "."
                          Name@13..14
                            Identifier@13..14 "e"
                        Arrow@14..16 "->"
                        Name@16..17
                          Identifier@16..17 "f"
                      Dot@17..18 "."
                      Name@18..19
                        Identifier@18..19 "g"
                    Arrow@19..21 "->"
                    Name@21..22
                      Identifier@21..22 "h""#]],
    );
}

// Call exprs
#[test]
fn parse_call_expr() {
    check(
        "_:=a(b)",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..7
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..7
                      LeftParen@4..5 "("
                      Param@5..6
                        NameExpr@5..6
                          Name@5..6
                            Identifier@5..6 "b"
                      RightParen@6..7 ")""#]],
    );
}

#[test]
fn parse_nested_call_expr() {
    check(
        "_:=a(b(c(), d, e), f, g)",
        expect![[r#"
            Source@0..24
              StmtList@0..24
                AssignStmt@0..24
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..24
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..24
                      LeftParen@4..5 "("
                      Param@5..18
                        CallExpr@5..17
                          NameExpr@5..6
                            Name@5..6
                              Identifier@5..6 "b"
                          ParamList@6..17
                            LeftParen@6..7 "("
                            Param@7..11
                              CallExpr@7..10
                                NameExpr@7..8
                                  Name@7..8
                                    Identifier@7..8 "c"
                                ParamList@8..10
                                  LeftParen@8..9 "("
                                  RightParen@9..10 ")"
                              Comma@10..11 ","
                            Whitespace@11..12 " "
                            Param@12..14
                              NameExpr@12..13
                                Name@12..13
                                  Identifier@12..13 "d"
                              Comma@13..14 ","
                            Whitespace@14..15 " "
                            Param@15..16
                              NameExpr@15..16
                                Name@15..16
                                  Identifier@15..16 "e"
                            RightParen@16..17 ")"
                        Comma@17..18 ","
                      Whitespace@18..19 " "
                      Param@19..21
                        NameExpr@19..20
                          Name@19..20
                            Identifier@19..20 "f"
                        Comma@20..21 ","
                      Whitespace@21..22 " "
                      Param@22..23
                        NameExpr@22..23
                          Name@22..23
                            Identifier@22..23 "g"
                      RightParen@23..24 ")""#]],
    );
}

#[test]
fn parse_empty_call_expr() {
    check(
        "_:=a()",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..6
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..6
                      LeftParen@4..5 "("
                      RightParen@5..6 ")""#]],
    );
}

#[test]
fn parse_call_expr_with_many_args() {
    check(
        "_:=a(1, 2 + 3, c)",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                AssignStmt@0..17
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..17
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..17
                      LeftParen@4..5 "("
                      Param@5..7
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
                        Comma@6..7 ","
                      Whitespace@7..8 " "
                      Param@8..14
                        BinaryExpr@8..13
                          LiteralExpr@8..9
                            IntLiteral@8..9 "2"
                          Whitespace@9..10 " "
                          Plus@10..11 "+"
                          Whitespace@11..12 " "
                          LiteralExpr@12..13
                            IntLiteral@12..13 "3"
                        Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..16
                        NameExpr@15..16
                          Name@15..16
                            Identifier@15..16 "c"
                      RightParen@16..17 ")""#]],
    );
}

#[test]
fn recover_call_expr_missing_closing_paren() {
    check(
        "_:=a(1",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..6
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..6
                      LeftParen@4..5 "("
                      Param@5..6
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
            error in file FileId(1) at 5..6: unexpected end of file
            | error in file FileId(1) for 5..6: expected ‘..’, ‘,’ or ‘)’ after here"#]],
    );
}

#[test]
fn recover_call_expr_missing_last_arg() {
    // FIXME: Rename once we accept trailing commas
    check(
        "_:=a(1,)",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..8
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..8
                      LeftParen@4..5 "("
                      Param@5..7
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
                        Comma@6..7 ","
                      Param@7..7
                      RightParen@7..8 ")"
            error in file FileId(1) at 7..8: unexpected token
            | error in file FileId(1) for 7..8: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_call_expr_missing_last_arg_and_closing_paren() {
    check(
        "_:=a(1,",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..7
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..7
                      LeftParen@4..5 "("
                      Param@5..7
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
                        Comma@6..7 ","
                      Param@7..7
            error in file FileId(1) at 6..7: unexpected end of file
            | error in file FileId(1) for 6..7: expected expression after here"#]],
    );
}

#[test]
fn recover_call_expr_missing_delim() {
    check(
        "_:=a(1 1)",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..6
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..6
                      LeftParen@4..5 "("
                      Param@5..6
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
                Whitespace@6..7 " "
                CallStmt@7..8
                  LiteralExpr@7..8
                    IntLiteral@7..8 "1"
                Error@8..9
                  RightParen@8..9 ")"
            error in file FileId(1) at 7..8: unexpected token
            | error in file FileId(1) for 7..8: expected ‘..’, ‘,’ or ‘)’, but found int literal
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected statement, but found ‘)’"#]],
    );
}

#[test]
fn recover_call_expr_missing_param() {
    check(
        "_:=a(1,,1)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..10
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..10
                      LeftParen@4..5 "("
                      Param@5..7
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
                        Comma@6..7 ","
                      Param@7..8
                        Comma@7..8 ","
                      Param@8..9
                        LiteralExpr@8..9
                          IntLiteral@8..9 "1"
                      RightParen@9..10 ")"
            error in file FileId(1) at 7..8: unexpected token
            | error in file FileId(1) for 7..8: expected expression, but found ‘,’"#]],
    );
}

#[test]
fn recover_call_expr_missing_params() {
    check(
        "_:=a(1,,,1)",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..11
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..11
                      LeftParen@4..5 "("
                      Param@5..7
                        LiteralExpr@5..6
                          IntLiteral@5..6 "1"
                        Comma@6..7 ","
                      Param@7..8
                        Comma@7..8 ","
                      Param@8..9
                        Comma@8..9 ","
                      Param@9..10
                        LiteralExpr@9..10
                          IntLiteral@9..10 "1"
                      RightParen@10..11 ")"
            error in file FileId(1) at 7..8: unexpected token
            | error in file FileId(1) for 7..8: expected expression, but found ‘,’
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected expression, but found ‘,’"#]],
    );
}

// Deref exprs

#[test]
fn parse_deref_expr() {
    check(
        "_:=^a",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  DerefExpr@3..5
                    Caret@3..4 "^"
                    NameExpr@4..5
                      Name@4..5
                        Identifier@4..5 "a""#]],
    );
}

#[test]
fn nested_deref() {
    check(
        "_:=^ ^ ^ ^ ^ a",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  DerefExpr@3..14
                    Caret@3..4 "^"
                    Whitespace@4..5 " "
                    DerefExpr@5..14
                      Caret@5..6 "^"
                      Whitespace@6..7 " "
                      DerefExpr@7..14
                        Caret@7..8 "^"
                        Whitespace@8..9 " "
                        DerefExpr@9..14
                          Caret@9..10 "^"
                          Whitespace@10..11 " "
                          DerefExpr@11..14
                            Caret@11..12 "^"
                            Whitespace@12..13 " "
                            NameExpr@13..14
                              Name@13..14
                                Identifier@13..14 "a""#]],
    );
}

#[test]
fn deref_binds_higher_than_dot() {
    check(
        "_:=^a.b",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  FieldExpr@3..7
                    DerefExpr@3..5
                      Caret@3..4 "^"
                      NameExpr@4..5
                        Name@4..5
                          Identifier@4..5 "a"
                    Dot@5..6 "."
                    Name@6..7
                      Identifier@6..7 "b""#]],
    );
}

#[test]
fn deref_binds_higher_than_arrow() {
    check(
        "_:=^a.b",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  FieldExpr@3..7
                    DerefExpr@3..5
                      Caret@3..4 "^"
                      NameExpr@4..5
                        Name@4..5
                          Identifier@4..5 "a"
                    Dot@5..6 "."
                    Name@6..7
                      Identifier@6..7 "b""#]],
    );
}

#[test]
fn deref_binds_higher_than_call() {
    check(
        "_:=^a()",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..7
                    DerefExpr@3..5
                      Caret@3..4 "^"
                      NameExpr@4..5
                        Name@4..5
                          Identifier@4..5 "a"
                    ParamList@5..7
                      LeftParen@5..6 "("
                      RightParen@6..7 ")""#]],
    );
}

#[test]
fn parens_override_deref_binding() {
    check(
        "^(a.b)",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                CallStmt@0..6
                  DerefExpr@0..6
                    Caret@0..1 "^"
                    ParenExpr@1..6
                      LeftParen@1..2 "("
                      FieldExpr@2..5
                        NameExpr@2..3
                          Name@2..3
                            Identifier@2..3 "a"
                        Dot@3..4 "."
                        Name@4..5
                          Identifier@4..5 "b"
                      RightParen@5..6 ")""#]],
    );
}

#[test]
fn recover_deref_not_reference() {
    // here, this should be parsed as a valid expression, but checked to be an invalid one
    check(
        "^^1",
        expect![[r#"
            Source@0..3
              StmtList@0..3
                CallStmt@0..3
                  DerefExpr@0..3
                    Caret@0..1 "^"
                    DerefExpr@1..3
                      Caret@1..2 "^"
                      LiteralExpr@2..3
                        IntLiteral@2..3 "1""#]],
    );
}

#[test]
fn recover_deref_missing_rhs() {
    check(
        "_:=^^",
        expect![[r#"
            Source@0..5
              StmtList@0..5
                AssignStmt@0..5
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  DerefExpr@3..5
                    Caret@3..4 "^"
                    DerefExpr@4..5
                      Caret@4..5 "^"
            error in file FileId(1) at 4..5: unexpected end of file
            | error in file FileId(1) for 4..5: expected expression after here"#]],
    );
}

#[test]
fn parse_init_expr() {
    check(
        "_:=init(1)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..10
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..9
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                    RightParen@9..10 ")""#]],
    );
}

#[test]
fn init_expr_multiple_exprs() {
    check(
        "_:=init(1, 2, 3)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..16
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..15
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Comma@9..10 ","
                      Whitespace@10..11 " "
                      LiteralExpr@11..12
                        IntLiteral@11..12 "2"
                      Comma@12..13 ","
                      Whitespace@13..14 " "
                      LiteralExpr@14..15
                        IntLiteral@14..15 "3"
                    RightParen@15..16 ")""#]],
    );
}

#[test]
fn init_expr_opt_trailing_comma() {
    check(
        "_:=init(1,2,3,)",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                AssignStmt@0..15
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..15
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..14
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Comma@9..10 ","
                      LiteralExpr@10..11
                        IntLiteral@10..11 "2"
                      Comma@11..12 ","
                      LiteralExpr@12..13
                        IntLiteral@12..13 "3"
                      Comma@13..14 ","
                    RightParen@14..15 ")""#]],
    );
}

#[test]
fn recover_init_expr_missing_expr_in_list() {
    check(
        "_:=init(1,,3)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..13
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..12
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Comma@9..10 ","
                      Comma@10..11 ","
                      LiteralExpr@11..12
                        IntLiteral@11..12 "3"
                    RightParen@12..13 ")"
            error in file FileId(1) at 10..11: unexpected token
            | error in file FileId(1) for 10..11: expected expression, but found ‘,’"#]],
    );
}
#[test]
fn recover_init_expr_missing_delimiter() {
    check(
        "_:=init(1, 2 3)",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..12
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..12
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Comma@9..10 ","
                      Whitespace@10..11 " "
                      LiteralExpr@11..12
                        IntLiteral@11..12 "2"
                Whitespace@12..13 " "
                CallStmt@13..14
                  LiteralExpr@13..14
                    IntLiteral@13..14 "3"
                Error@14..15
                  RightParen@14..15 ")"
            error in file FileId(1) at 13..14: unexpected token
            | error in file FileId(1) for 13..14: expected ‘)’, but found int literal
            error in file FileId(1) at 14..15: unexpected token
            | error in file FileId(1) for 14..15: expected statement, but found ‘)’"#]],
    );
}

#[test]
fn recover_init_expr_missing_right_paren() {
    check(
        "_:=init(1, 2",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..12
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..12
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Comma@9..10 ","
                      Whitespace@10..11 " "
                      LiteralExpr@11..12
                        IntLiteral@11..12 "2"
            error in file FileId(1) at 11..12: unexpected end of file
            | error in file FileId(1) for 11..12: expected ‘)’ after here"#]],
    );
}

#[test]
fn recover_init_expr_missing_left_paren() {
    check(
        "_:=init 1, 2",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..12
                    KwInit@3..7 "init"
                    Whitespace@7..8 " "
                    ExprList@8..12
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Comma@9..10 ","
                      Whitespace@10..11 " "
                      LiteralExpr@11..12
                        IntLiteral@11..12 "2"
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected ‘(’, but found int literal
            error in file FileId(1) at 11..12: unexpected end of file
            | error in file FileId(1) for 11..12: expected ‘)’ after here"#]],
    );
}

#[test]
fn recover_init_expr_empty() {
    check(
        "_:=init()",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..9
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..8
                    RightParen@8..9 ")"
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_init_expr_just_comma() {
    check(
        "_:=init(,)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  InitExpr@3..10
                    KwInit@3..7 "init"
                    LeftParen@7..8 "("
                    ExprList@8..9
                      Comma@8..9 ","
                    RightParen@9..10 ")"
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected expression, but found ‘,’"#]],
    )
}

#[test]
fn parse_indirect_expr_ty() {
    check(
        "_:=boolean @ (1)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..16
                    PrimType@3..10
                      KwBoolean@3..10 "boolean"
                    Whitespace@10..11 " "
                    At@11..12 "@"
                    Whitespace@12..13 " "
                    LeftParen@13..14 "("
                    LiteralExpr@14..15
                      IntLiteral@14..15 "1"
                    RightParen@15..16 ")""#]],
    );
    check(
        "_:=addressint @ (1)",
        expect![[r#"
            Source@0..19
              StmtList@0..19
                AssignStmt@0..19
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..19
                    PrimType@3..13
                      KwAddressint@3..13 "addressint"
                    Whitespace@13..14 " "
                    At@14..15 "@"
                    Whitespace@15..16 " "
                    LeftParen@16..17 "("
                    LiteralExpr@17..18
                      IntLiteral@17..18 "1"
                    RightParen@18..19 ")""#]],
    );
    check(
        "_:=int @ (1)",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..12
                    PrimType@3..6
                      KwInt@3..6 "int"
                    Whitespace@6..7 " "
                    At@7..8 "@"
                    Whitespace@8..9 " "
                    LeftParen@9..10 "("
                    LiteralExpr@10..11
                      IntLiteral@10..11 "1"
                    RightParen@11..12 ")""#]],
    );
    check(
        "_:=int1 @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwInt1@3..7 "int1"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=int2 @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwInt2@3..7 "int2"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=int4 @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwInt4@3..7 "int4"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=nat @ (1)",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..12
                    PrimType@3..6
                      KwNat@3..6 "nat"
                    Whitespace@6..7 " "
                    At@7..8 "@"
                    Whitespace@8..9 " "
                    LeftParen@9..10 "("
                    LiteralExpr@10..11
                      IntLiteral@10..11 "1"
                    RightParen@11..12 ")""#]],
    );
    check(
        "_:=nat1 @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwNat1@3..7 "nat1"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=nat2 @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwNat2@3..7 "nat2"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=nat4 @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwNat4@3..7 "nat4"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=real @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwReal@3..7 "real"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=real4 @ (1)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..14
                    PrimType@3..8
                      KwReal4@3..8 "real4"
                    Whitespace@8..9 " "
                    At@9..10 "@"
                    Whitespace@10..11 " "
                    LeftParen@11..12 "("
                    LiteralExpr@12..13
                      IntLiteral@12..13 "1"
                    RightParen@13..14 ")""#]],
    );
    check(
        "_:=real8 @ (1)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..14
                    PrimType@3..8
                      KwReal8@3..8 "real8"
                    Whitespace@8..9 " "
                    At@9..10 "@"
                    Whitespace@10..11 " "
                    LeftParen@11..12 "("
                    LiteralExpr@12..13
                      IntLiteral@12..13 "1"
                    RightParen@13..14 ")""#]],
    );
    check(
        "_:=string @ (1)",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                AssignStmt@0..15
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..15
                    PrimType@3..9
                      KwString@3..9 "string"
                    Whitespace@9..10 " "
                    At@10..11 "@"
                    Whitespace@11..12 " "
                    LeftParen@12..13 "("
                    LiteralExpr@13..14
                      IntLiteral@13..14 "1"
                    RightParen@14..15 ")""#]],
    );
    check(
        "_:=char @ (1)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..13
                    PrimType@3..7
                      KwChar@3..7 "char"
                    Whitespace@7..8 " "
                    At@8..9 "@"
                    Whitespace@9..10 " "
                    LeftParen@10..11 "("
                    LiteralExpr@11..12
                      IntLiteral@11..12 "1"
                    RightParen@12..13 ")""#]],
    );
    check(
        "_:=string(1+2+3) @ (1)",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                AssignStmt@0..22
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..22
                    PrimType@3..16
                      SizedStringType@3..16
                        KwString@3..9 "string"
                        LeftParen@9..10 "("
                        SeqLength@10..15
                          BinaryExpr@10..15
                            BinaryExpr@10..13
                              LiteralExpr@10..11
                                IntLiteral@10..11 "1"
                              Plus@11..12 "+"
                              LiteralExpr@12..13
                                IntLiteral@12..13 "2"
                            Plus@13..14 "+"
                            LiteralExpr@14..15
                              IntLiteral@14..15 "3"
                        RightParen@15..16 ")"
                    Whitespace@16..17 " "
                    At@17..18 "@"
                    Whitespace@18..19 " "
                    LeftParen@19..20 "("
                    LiteralExpr@20..21
                      IntLiteral@20..21 "1"
                    RightParen@21..22 ")""#]],
    );
    check(
        "_:=char(3+4+5) @ (1)",
        expect![[r#"
            Source@0..20
              StmtList@0..20
                AssignStmt@0..20
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..20
                    PrimType@3..14
                      SizedCharType@3..14
                        KwChar@3..7 "char"
                        LeftParen@7..8 "("
                        SeqLength@8..13
                          BinaryExpr@8..13
                            BinaryExpr@8..11
                              LiteralExpr@8..9
                                IntLiteral@8..9 "3"
                              Plus@9..10 "+"
                              LiteralExpr@10..11
                                IntLiteral@10..11 "4"
                            Plus@11..12 "+"
                            LiteralExpr@12..13
                              IntLiteral@12..13 "5"
                        RightParen@13..14 ")"
                    Whitespace@14..15 " "
                    At@15..16 "@"
                    Whitespace@16..17 " "
                    LeftParen@17..18 "("
                    LiteralExpr@18..19
                      IntLiteral@18..19 "1"
                    RightParen@19..20 ")""#]],
    );
    check(
        "_:=string(*) @ (1)",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                AssignStmt@0..18
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..18
                    PrimType@3..12
                      SizedStringType@3..12
                        KwString@3..9 "string"
                        LeftParen@9..10 "("
                        SeqLength@10..11
                          Star@10..11 "*"
                        RightParen@11..12 ")"
                    Whitespace@12..13 " "
                    At@13..14 "@"
                    Whitespace@14..15 " "
                    LeftParen@15..16 "("
                    LiteralExpr@16..17
                      IntLiteral@16..17 "1"
                    RightParen@17..18 ")""#]],
    );
    check(
        "_:=char(*) @ (1)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..16
                    PrimType@3..10
                      SizedCharType@3..10
                        KwChar@3..7 "char"
                        LeftParen@7..8 "("
                        SeqLength@8..9
                          Star@8..9 "*"
                        RightParen@9..10 ")"
                    Whitespace@10..11 " "
                    At@11..12 "@"
                    Whitespace@12..13 " "
                    LeftParen@13..14 "("
                    LiteralExpr@14..15
                      IntLiteral@14..15 "1"
                    RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_indirect_expr_ty_ref() {
    check(
        "_:=a @ (1)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..10
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    Whitespace@4..5 " "
                    At@5..6 "@"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    LiteralExpr@8..9
                      IntLiteral@8..9 "1"
                    RightParen@9..10 ")""#]],
    );
    check(
        "_:=a.b.c @ (1)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..14
                    FieldExpr@3..8
                      FieldExpr@3..6
                        NameExpr@3..4
                          Name@3..4
                            Identifier@3..4 "a"
                        Dot@4..5 "."
                        Name@5..6
                          Identifier@5..6 "b"
                      Dot@6..7 "."
                      Name@7..8
                        Identifier@7..8 "c"
                    Whitespace@8..9 " "
                    At@9..10 "@"
                    Whitespace@10..11 " "
                    LeftParen@11..12 "("
                    LiteralExpr@12..13
                      IntLiteral@12..13 "1"
                    RightParen@13..14 ")""#]],
    );
}

#[test]
fn parse_indirect_expr_not_ty_ref_deref() {
    check(
        "_:=^a @ (1)",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  DerefExpr@3..11
                    Caret@3..4 "^"
                    IndirectExpr@4..11
                      NameExpr@4..5
                        Name@4..5
                          Identifier@4..5 "a"
                      Whitespace@5..6 " "
                      At@6..7 "@"
                      Whitespace@7..8 " "
                      LeftParen@8..9 "("
                      LiteralExpr@9..10
                        IntLiteral@9..10 "1"
                      RightParen@10..11 ")""#]],
    );
}

#[test]
fn parse_indirect_expr_not_ty_ref_literal() {
    check(
        "_:=1 @ (1)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..10
                    LiteralExpr@3..4
                      IntLiteral@3..4 "1"
                    Whitespace@4..5 " "
                    At@5..6 "@"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    LiteralExpr@8..9
                      IntLiteral@8..9 "1"
                    RightParen@9..10 ")""#]],
    );
}

#[test]
fn parse_indirect_expr_not_ty_ref_parens() {
    check(
        "_:=(a.b.c) @ (1)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..16
                    ParenExpr@3..10
                      LeftParen@3..4 "("
                      FieldExpr@4..9
                        FieldExpr@4..7
                          NameExpr@4..5
                            Name@4..5
                              Identifier@4..5 "a"
                          Dot@5..6 "."
                          Name@6..7
                            Identifier@6..7 "b"
                        Dot@7..8 "."
                        Name@8..9
                          Identifier@8..9 "c"
                      RightParen@9..10 ")"
                    Whitespace@10..11 " "
                    At@11..12 "@"
                    Whitespace@12..13 " "
                    LeftParen@13..14 "("
                    LiteralExpr@14..15
                      IntLiteral@14..15 "1"
                    RightParen@15..16 ")""#]],
    );
}

#[test]
fn recover_chained_indirect_tails() {
    // can't chain indirect expr tails
    check(
        "_:=a @ (1) @ (2)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  IndirectExpr@3..10
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    Whitespace@4..5 " "
                    At@5..6 "@"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    LiteralExpr@8..9
                      IntLiteral@8..9 "1"
                    RightParen@9..10 ")"
                  Whitespace@10..11 " "
                  Error@11..12
                    At@11..12 "@"
                Whitespace@12..13 " "
                CallStmt@13..16
                  ParenExpr@13..16
                    LeftParen@13..14 "("
                    LiteralExpr@14..15
                      IntLiteral@14..15 "2"
                    RightParen@15..16 ")"
            error in file FileId(1) at 11..12: unexpected token
            | error in file FileId(1) for 11..12: expected infix operator, but found ‘@’"#]],
    );
}

#[test]
fn parse_indirect_expr_in_ref_pos() {
    check(
        "char @ (1) := 1",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                AssignStmt@0..15
                  IndirectExpr@0..10
                    PrimType@0..4
                      KwChar@0..4 "char"
                    Whitespace@4..5 " "
                    At@5..6 "@"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    LiteralExpr@8..9
                      IntLiteral@8..9 "1"
                    RightParen@9..10 ")"
                  Whitespace@10..11 " "
                  AsnOp@11..13
                    Assign@11..13 ":="
                  Whitespace@13..14 " "
                  LiteralExpr@14..15
                    IntLiteral@14..15 "1""#]],
    );
}

#[test]
fn recover_just_indirect_ty() {
    check(
        "char",
        expect![[r#"
            Source@0..4
              StmtList@0..4
                Error@0..4
                  PrimType@0..4
                    KwChar@0..4 "char"
            error in file FileId(1) at 0..4: unexpected end of file
            | error in file FileId(1) for 0..4: expected ‘(’ or ‘@’ after here"#]],
    );
}

#[test]
fn parse_bits_expr() {
    check(
        "_:= bits(a, 1)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Whitespace@3..4 " "
                  BitsExpr@4..14
                    KwBits@4..8 "bits"
                    ParamList@8..14
                      LeftParen@8..9 "("
                      Param@9..11
                        NameExpr@9..10
                          Name@9..10
                            Identifier@9..10 "a"
                        Comma@10..11 ","
                      Whitespace@11..12 " "
                      Param@12..13
                        LiteralExpr@12..13
                          IntLiteral@12..13 "1"
                      RightParen@13..14 ")""#]],
    );
}

#[test]
fn parse_bits_ref() {
    check(
        "bits(a, 1)",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                CallStmt@0..10
                  BitsExpr@0..10
                    KwBits@0..4 "bits"
                    ParamList@4..10
                      LeftParen@4..5 "("
                      Param@5..7
                        NameExpr@5..6
                          Name@5..6
                            Identifier@5..6 "a"
                        Comma@6..7 ","
                      Whitespace@7..8 " "
                      Param@8..9
                        LiteralExpr@8..9
                          IntLiteral@8..9 "1"
                      RightParen@9..10 ")""#]],
    );
}

#[test]
fn parse_bits_ref_range() {
    check(
        "bits(a, 1 .. 2 - 3)",
        expect![[r#"
            Source@0..19
              StmtList@0..19
                CallStmt@0..19
                  BitsExpr@0..19
                    KwBits@0..4 "bits"
                    ParamList@4..19
                      LeftParen@4..5 "("
                      Param@5..7
                        NameExpr@5..6
                          Name@5..6
                            Identifier@5..6 "a"
                        Comma@6..7 ","
                      Whitespace@7..8 " "
                      Param@8..18
                        RangeItem@8..18
                          LiteralExpr@8..9
                            IntLiteral@8..9 "1"
                          Whitespace@9..10 " "
                          Range@10..12 ".."
                          Whitespace@12..13 " "
                          BinaryExpr@13..18
                            LiteralExpr@13..14
                              IntLiteral@13..14 "2"
                            Whitespace@14..15 " "
                            Minus@15..16 "-"
                            Whitespace@16..17 " "
                            LiteralExpr@17..18
                              IntLiteral@17..18 "3"
                      RightParen@18..19 ")""#]],
    );
}

#[test]
fn parse_bits_ref_relative_range() {
    // Reject during lowering
    check(
        "bits(a, 1 .. *)",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                CallStmt@0..15
                  BitsExpr@0..15
                    KwBits@0..4 "bits"
                    ParamList@4..15
                      LeftParen@4..5 "("
                      Param@5..7
                        NameExpr@5..6
                          Name@5..6
                            Identifier@5..6 "a"
                        Comma@6..7 ","
                      Whitespace@7..8 " "
                      Param@8..14
                        RangeItem@8..14
                          LiteralExpr@8..9
                            IntLiteral@8..9 "1"
                          Whitespace@9..10 " "
                          Range@10..12 ".."
                          Whitespace@12..13 " "
                          RelativeBound@13..14
                            Star@13..14 "*"
                      RightParen@14..15 ")""#]],
    );
}

#[test]
fn parse_bits_single_arg() {
    // Rejected during lowering
    check(
        "bits(a)",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                CallStmt@0..7
                  BitsExpr@0..7
                    KwBits@0..4 "bits"
                    ParamList@4..7
                      LeftParen@4..5 "("
                      Param@5..6
                        NameExpr@5..6
                          Name@5..6
                            Identifier@5..6 "a"
                      RightParen@6..7 ")""#]],
    );
}

#[test]
fn parse_bits_no_args() {
    // Rejected during lowering
    check(
        "bits()",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                CallStmt@0..6
                  BitsExpr@0..6
                    KwBits@0..4 "bits"
                    ParamList@4..6
                      LeftParen@4..5 "("
                      RightParen@5..6 ")""#]],
    );
}

#[test]
fn recover_just_bits() {
    check(
        "bits",
        expect![[r#"
            Source@0..4
              StmtList@0..4
                CallStmt@0..4
                  BitsExpr@0..4
                    KwBits@0..4 "bits"
            error in file FileId(1) at 0..4: unexpected end of file
            | error in file FileId(1) for 0..4: expected ‘(’ after here"#]],
    );
}

#[test]
fn parse_objclass_expr() {
    check(
        "_:= objectclass(a)",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                AssignStmt@0..18
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  Whitespace@3..4 " "
                  ObjClassExpr@4..18
                    KwObjectClass@4..15 "objectclass"
                    ParamList@15..18
                      LeftParen@15..16 "("
                      Param@16..17
                        NameExpr@16..17
                          Name@16..17
                            Identifier@16..17 "a"
                      RightParen@17..18 ")""#]],
    );
}

#[test]
fn parse_objclass_ref() {
    // Rejected during lowering
    check(
        "objectclass(a)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                CallStmt@0..14
                  ObjClassExpr@0..14
                    KwObjectClass@0..11 "objectclass"
                    ParamList@11..14
                      LeftParen@11..12 "("
                      Param@12..13
                        NameExpr@12..13
                          Name@12..13
                            Identifier@12..13 "a"
                      RightParen@13..14 ")""#]],
    );
}

#[test]
fn parse_objclass_many_args() {
    // Rejected during lowering
    check(
        "objectclass(a, b)",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                CallStmt@0..17
                  ObjClassExpr@0..17
                    KwObjectClass@0..11 "objectclass"
                    ParamList@11..17
                      LeftParen@11..12 "("
                      Param@12..14
                        NameExpr@12..13
                          Name@12..13
                            Identifier@12..13 "a"
                        Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..16
                        NameExpr@15..16
                          Name@15..16
                            Identifier@15..16 "b"
                      RightParen@16..17 ")""#]],
    );
}

#[test]
fn parse_objclass_no_args() {
    // Rejected during lowering
    check(
        "objectclass()",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                CallStmt@0..13
                  ObjClassExpr@0..13
                    KwObjectClass@0..11 "objectclass"
                    ParamList@11..13
                      LeftParen@11..12 "("
                      RightParen@12..13 ")""#]],
    );
}

#[test]
fn recover_just_objclass() {
    check(
        "objectclass",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                CallStmt@0..11
                  ObjClassExpr@0..11
                    KwObjectClass@0..11 "objectclass"
            error in file FileId(1) at 0..11: unexpected end of file
            | error in file FileId(1) for 0..11: expected ‘(’ after here"#]],
    );
}

#[test]
fn parse_cheat_expr() {
    check(
        "_ := cheat(char(80), 8)",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                AssignStmt@0..23
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  Whitespace@1..2 " "
                  AsnOp@2..4
                    Assign@2..4 ":="
                  Whitespace@4..5 " "
                  CheatExpr@5..23
                    KwCheat@5..10 "cheat"
                    LeftParen@10..11 "("
                    PrimType@11..19
                      SizedCharType@11..19
                        KwChar@11..15 "char"
                        LeftParen@15..16 "("
                        SeqLength@16..18
                          LiteralExpr@16..18
                            IntLiteral@16..18 "80"
                        RightParen@18..19 ")"
                    Comma@19..20 ","
                    Whitespace@20..21 " "
                    LiteralExpr@21..22
                      IntLiteral@21..22 "8"
                    RightParen@22..23 ")""#]],
    );
}

#[test]
fn parse_cheat_expr_with_opt_size_spec() {
    check(
        "_ := cheat(int, 8 : 4)",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                AssignStmt@0..22
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  Whitespace@1..2 " "
                  AsnOp@2..4
                    Assign@2..4 ":="
                  Whitespace@4..5 " "
                  CheatExpr@5..22
                    KwCheat@5..10 "cheat"
                    LeftParen@10..11 "("
                    PrimType@11..14
                      KwInt@11..14 "int"
                    Comma@14..15 ","
                    Whitespace@15..16 " "
                    LiteralExpr@16..17
                      IntLiteral@16..17 "8"
                    Whitespace@17..18 " "
                    SizeSpec@18..21
                      Colon@18..19 ":"
                      Whitespace@19..20 " "
                      LiteralExpr@20..21
                        IntLiteral@20..21 "4"
                    RightParen@21..22 ")""#]],
    );
}

#[test]
fn parse_cheat_ref() {
    check(
        "cheat(int, a)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                CallStmt@0..13
                  CheatExpr@0..13
                    KwCheat@0..5 "cheat"
                    LeftParen@5..6 "("
                    PrimType@6..9
                      KwInt@6..9 "int"
                    Comma@9..10 ","
                    Whitespace@10..11 " "
                    NameExpr@11..12
                      Name@11..12
                        Identifier@11..12 "a"
                    RightParen@12..13 ")""#]],
    );
}

#[test]
fn recover_cheat_expr_missing_size_spec_expr() {
    check(
        "_:=cheat(int, a : )",
        expect![[r#"
            Source@0..19
              StmtList@0..19
                AssignStmt@0..19
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CheatExpr@3..19
                    KwCheat@3..8 "cheat"
                    LeftParen@8..9 "("
                    PrimType@9..12
                      KwInt@9..12 "int"
                    Comma@12..13 ","
                    Whitespace@13..14 " "
                    NameExpr@14..15
                      Name@14..15
                        Identifier@14..15 "a"
                    Whitespace@15..16 " "
                    SizeSpec@16..17
                      Colon@16..17 ":"
                    Whitespace@17..18 " "
                    RightParen@18..19 ")"
            error in file FileId(1) at 18..19: unexpected token
            | error in file FileId(1) for 18..19: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_cheat_expr_missing_expr() {
    check(
        "_:=cheat(int, )",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                AssignStmt@0..15
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CheatExpr@3..15
                    KwCheat@3..8 "cheat"
                    LeftParen@8..9 "("
                    PrimType@9..12
                      KwInt@9..12 "int"
                    Comma@12..13 ","
                    Whitespace@13..14 " "
                    RightParen@14..15 ")"
            error in file FileId(1) at 14..15: unexpected token
            | error in file FileId(1) for 14..15: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_cheat_expr_missing_comma() {
    check(
        "_:=cheat(int)",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CheatExpr@3..13
                    KwCheat@3..8 "cheat"
                    LeftParen@8..9 "("
                    PrimType@9..12
                      KwInt@9..12 "int"
                    RightParen@12..13 ")"
            error in file FileId(1) at 12..13: unexpected token
            | error in file FileId(1) for 12..13: expected ‘,’, but found ‘)’"#]],
    );
}

#[test]
fn recover_cheat_expr_empty() {
    check(
        "_:=cheat()",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CheatExpr@3..10
                    KwCheat@3..8 "cheat"
                    LeftParen@8..9 "("
                    RightParen@9..10 ")"
            error in file FileId(1) at 9..10: unexpected token
            | error in file FileId(1) for 9..10: expected type specifier, but found ‘)’"#]],
    );
}

#[test]
fn recover_just_cheat() {
    check(
        "_:=cheat",
        expect![[r#"
            Source@0..8
              StmtList@0..8
                AssignStmt@0..8
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CheatExpr@3..8
                    KwCheat@3..8 "cheat"
            error in file FileId(1) at 3..8: unexpected end of file
            | error in file FileId(1) for 3..8: expected ‘(’ after here"#]],
    );
}

#[test]
fn parse_call_expr_end_bound() {
    check(
        "_:=a(*)",
        expect![[r#"
            Source@0..7
              StmtList@0..7
                AssignStmt@0..7
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..7
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..7
                      LeftParen@4..5 "("
                      Param@5..6
                        RangeItem@5..6
                          RelativeBound@5..6
                            Star@5..6 "*"
                      RightParen@6..7 ")""#]],
    );
}

#[test]
fn parse_call_expr_relative_bound() {
    check(
        "_:=a(* - 1)",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..11
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..11
                      LeftParen@4..5 "("
                      Param@5..10
                        RangeItem@5..10
                          RelativeBound@5..10
                            Star@5..6 "*"
                            Whitespace@6..7 " "
                            Minus@7..8 "-"
                            Whitespace@8..9 " "
                            LiteralExpr@9..10
                              IntLiteral@9..10 "1"
                      RightParen@10..11 ")""#]],
    );
}

#[test]
fn parse_call_expr_range_item_left_bounded() {
    check(
        "_:=a(* - 1 .. a)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..16
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..16
                      LeftParen@4..5 "("
                      Param@5..15
                        RangeItem@5..15
                          RelativeBound@5..10
                            Star@5..6 "*"
                            Whitespace@6..7 " "
                            Minus@7..8 "-"
                            Whitespace@8..9 " "
                            LiteralExpr@9..10
                              IntLiteral@9..10 "1"
                          Whitespace@10..11 " "
                          Range@11..13 ".."
                          Whitespace@13..14 " "
                          NameExpr@14..15
                            Name@14..15
                              Identifier@14..15 "a"
                      RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_call_expr_range_item_right_bounded() {
    check(
        "_:=a(a .. * - 1)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..16
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..16
                      LeftParen@4..5 "("
                      Param@5..15
                        RangeItem@5..15
                          NameExpr@5..6
                            Name@5..6
                              Identifier@5..6 "a"
                          Whitespace@6..7 " "
                          Range@7..9 ".."
                          Whitespace@9..10 " "
                          RelativeBound@10..15
                            Star@10..11 "*"
                            Whitespace@11..12 " "
                            Minus@12..13 "-"
                            Whitespace@13..14 " "
                            LiteralExpr@14..15
                              IntLiteral@14..15 "1"
                      RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_call_expr_range_item_both_relatively_bounded() {
    check(
        "_:=a(* - 1 .. * - 1)",
        expect![[r#"
            Source@0..20
              StmtList@0..20
                AssignStmt@0..20
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..20
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..20
                      LeftParen@4..5 "("
                      Param@5..19
                        RangeItem@5..19
                          RelativeBound@5..10
                            Star@5..6 "*"
                            Whitespace@6..7 " "
                            Minus@7..8 "-"
                            Whitespace@8..9 " "
                            LiteralExpr@9..10
                              IntLiteral@9..10 "1"
                          Whitespace@10..11 " "
                          Range@11..13 ".."
                          Whitespace@13..14 " "
                          RelativeBound@14..19
                            Star@14..15 "*"
                            Whitespace@15..16 " "
                            Minus@16..17 "-"
                            Whitespace@17..18 " "
                            LiteralExpr@18..19
                              IntLiteral@18..19 "1"
                      RightParen@19..20 ")""#]],
    );
}

#[test]
fn parse_call_expr_range_item_both_end_bounded() {
    check(
        "_:=a(* .. *)",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..12
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..12
                      LeftParen@4..5 "("
                      Param@5..11
                        RangeItem@5..11
                          RelativeBound@5..6
                            Star@5..6 "*"
                          Whitespace@6..7 " "
                          Range@7..9 ".."
                          Whitespace@9..10 " "
                          RelativeBound@10..11
                            Star@10..11 "*"
                      RightParen@11..12 ")""#]],
    );
}

#[test]
fn recover_call_expr_range_item_missing_expr() {
    check(
        "_:=a(a .. )",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..11
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..11
                      LeftParen@4..5 "("
                      Param@5..9
                        RangeItem@5..9
                          NameExpr@5..6
                            Name@5..6
                              Identifier@5..6 "a"
                          Whitespace@6..7 " "
                          Range@7..9 ".."
                      Whitespace@9..10 " "
                      RightParen@10..11 ")"
            error in file FileId(1) at 10..11: unexpected token
            | error in file FileId(1) for 10..11: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_call_expr_relative_bound_missing_expr() {
    check(
        "_:=a(* - )",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                AssignStmt@0..10
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..10
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..10
                      LeftParen@4..5 "("
                      Param@5..8
                        RangeItem@5..8
                          RelativeBound@5..8
                            Star@5..6 "*"
                            Whitespace@6..7 " "
                            Minus@7..8 "-"
                      Whitespace@8..9 " "
                      RightParen@9..10 ")"
            error in file FileId(1) at 9..10: unexpected token
            | error in file FileId(1) for 9..10: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_call_expr_relative_bound_missing_minus() {
    check(
        "_:=a(* 1)",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..6
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "a"
                    ParamList@4..6
                      LeftParen@4..5 "("
                      Param@5..6
                        RangeItem@5..6
                          RelativeBound@5..6
                            Star@5..6 "*"
                Whitespace@6..7 " "
                CallStmt@7..8
                  LiteralExpr@7..8
                    IntLiteral@7..8 "1"
                Error@8..9
                  RightParen@8..9 ")"
            error in file FileId(1) at 7..8: unexpected token
            | error in file FileId(1) for 7..8: expected ‘-’, ‘..’, ‘,’ or ‘)’, but found int literal
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected statement, but found ‘)’"#]],
    );
}

#[test]
fn parse_call_expr_all_item() {
    check(
        "_:=set_cons(all)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..16
                    NameExpr@3..11
                      Name@3..11
                        Identifier@3..11 "set_cons"
                    ParamList@11..16
                      LeftParen@11..12 "("
                      Param@12..15
                        AllItem@12..15
                          KwAll@12..15 "all"
                      RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_call_expr_many_all_items() {
    // reject in validation
    check(
        "_:=set_cons(all, all, all, all)",
        expect![[r#"
            Source@0..31
              StmtList@0..31
                AssignStmt@0..31
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  CallExpr@3..31
                    NameExpr@3..11
                      Name@3..11
                        Identifier@3..11 "set_cons"
                    ParamList@11..31
                      LeftParen@11..12 "("
                      Param@12..16
                        AllItem@12..15
                          KwAll@12..15 "all"
                        Comma@15..16 ","
                      Whitespace@16..17 " "
                      Param@17..21
                        AllItem@17..20
                          KwAll@17..20 "all"
                        Comma@20..21 ","
                      Whitespace@21..22 " "
                      Param@22..26
                        AllItem@22..25
                          KwAll@22..25 "all"
                        Comma@25..26 ","
                      Whitespace@26..27 " "
                      Param@27..30
                        AllItem@27..30
                          KwAll@27..30 "all"
                      RightParen@30..31 ")""#]],
    );
}

#[test]
fn recover_all_not_primary() {
    check(
        "_:=set_cons(all + 1)",
        expect![[r#"
            Source@0..20
              StmtList@0..20
                AssignStmt@0..19
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..19
                    CallExpr@3..15
                      NameExpr@3..11
                        Name@3..11
                          Identifier@3..11 "set_cons"
                      ParamList@11..15
                        LeftParen@11..12 "("
                        Param@12..15
                          AllItem@12..15
                            KwAll@12..15 "all"
                    Whitespace@15..16 " "
                    Plus@16..17 "+"
                    Whitespace@17..18 " "
                    LiteralExpr@18..19
                      IntLiteral@18..19 "1"
                Error@19..20
                  RightParen@19..20 ")"
            error in file FileId(1) at 16..17: unexpected token
            | error in file FileId(1) for 16..17: expected ‘,’ or ‘)’, but found ‘+’
            error in file FileId(1) at 19..20: unexpected token
            | error in file FileId(1) for 19..20: expected statement, but found ‘)’"#]],
    );
}

#[test]
fn parse_self_expr() {
    check(
        "_:=self -> a",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  ArrowExpr@3..12
                    SelfExpr@3..7
                      KwSelf@3..7 "self"
                    Whitespace@7..8 " "
                    Arrow@8..10 "->"
                    Whitespace@10..11 " "
                    Name@11..12
                      Identifier@11..12 "a""#]],
    );
}

#[test]
fn parse_self_ref() {
    check(
        "self -> a",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                CallStmt@0..9
                  ArrowExpr@0..9
                    SelfExpr@0..4
                      KwSelf@0..4 "self"
                    Whitespace@4..5 " "
                    Arrow@5..7 "->"
                    Whitespace@7..8 " "
                    Name@8..9
                      Identifier@8..9 "a""#]],
    );
}

#[test]
fn parse_just_self() {
    check(
        "self",
        expect![[r#"
            Source@0..4
              StmtList@0..4
                CallStmt@0..4
                  SelfExpr@0..4
                    KwSelf@0..4 "self""#]],
    );
}

#[test]
fn parse_nil_expr() {
    check(
        "_:=nil",
        expect![[r#"
            Source@0..6
              StmtList@0..6
                AssignStmt@0..6
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NilExpr@3..6
                    KwNil@3..6 "nil""#]],
    );
}

#[test]
fn parse_nil_expr_opt_spec() {
    check(
        "_:=nil (a.b.c.d)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NilExpr@3..16
                    KwNil@3..6 "nil"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    FieldExpr@8..15
                      FieldExpr@8..13
                        FieldExpr@8..11
                          NameExpr@8..9
                            Name@8..9
                              Identifier@8..9 "a"
                          Dot@9..10 "."
                          Name@10..11
                            Identifier@10..11 "b"
                        Dot@11..12 "."
                        Name@12..13
                          Identifier@12..13 "c"
                      Dot@13..14 "."
                      Name@14..15
                        Identifier@14..15 "d"
                    RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_nil_expr_opt_spec_not_ref() {
    // reject in validation
    check(
        "_:=nil (1 + 1)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NilExpr@3..14
                    KwNil@3..6 "nil"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    BinaryExpr@8..13
                      LiteralExpr@8..9
                        IntLiteral@8..9 "1"
                      Whitespace@9..10 " "
                      Plus@10..11 "+"
                      Whitespace@11..12 " "
                      LiteralExpr@12..13
                        IntLiteral@12..13 "1"
                    RightParen@13..14 ")""#]],
    );
}

#[test]
fn recover_nil_expr_missing_spec() {
    check(
        "_:=nil ()",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NilExpr@3..9
                    KwNil@3..6 "nil"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    RightParen@8..9 ")"
            error in file FileId(1) at 8..9: unexpected token
            | error in file FileId(1) for 8..9: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn recover_nil_expr_missing_right_paren() {
    check(
        "_:=nil (a",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  NilExpr@3..9
                    KwNil@3..6 "nil"
                    Whitespace@6..7 " "
                    LeftParen@7..8 "("
                    NameExpr@8..9
                      Name@8..9
                        Identifier@8..9 "a"
            error in file FileId(1) at 8..9: unexpected end of file
            | error in file FileId(1) for 8..9: expected ‘)’ after here"#]],
    );
}

#[test]
fn recover_include_glob_expr() {
    // not allowed in expr/ref position
    check(
        r#"_:=include "here" + 1"#,
        expect![[r#"
            Source@0..21
              StmtList@0..21
                AssignStmt@0..3
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                PreprocGlob@3..17
                  PPInclude@3..17
                    KwInclude@3..10 "include"
                    Whitespace@10..11 " "
                    LiteralExpr@11..17
                      StringLiteral@11..17 "\"here\""
                Whitespace@17..18 " "
                Error@18..19
                  Plus@18..19 "+"
                Whitespace@19..20 " "
                CallStmt@20..21
                  LiteralExpr@20..21
                    IntLiteral@20..21 "1"
            error in file FileId(1) at 3..10: unexpected token
            | error in file FileId(1) for 3..10: expected expression, but found ‘include’
            error in file FileId(1) at 18..19: unexpected token
            | error in file FileId(1) for 18..19: expected statement, but found ‘+’"#]],
    );
}

#[test]
fn recover_include_glob_ref() {
    // not allowed in expr/ref position
    check(
        r#"include "here" -> and_there"#,
        expect![[r#"
            Source@0..27
              StmtList@0..27
                PreprocGlob@0..14
                  PPInclude@0..14
                    KwInclude@0..7 "include"
                    Whitespace@7..8 " "
                    LiteralExpr@8..14
                      StringLiteral@8..14 "\"here\""
                Whitespace@14..15 " "
                Error@15..17
                  Arrow@15..17 "->"
                Whitespace@17..18 " "
                CallStmt@18..27
                  NameExpr@18..27
                    Name@18..27
                      Identifier@18..27 "and_there"
            error in file FileId(1) at 15..17: unexpected token
            | error in file FileId(1) for 15..17: expected statement, but found ‘->’"#]],
    );
}

#[test]
fn parse_infix_after_indirect() {
    check(
        "_:=int @ (1) >= 1",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                AssignStmt@0..17
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..17
                    IndirectExpr@3..12
                      PrimType@3..6
                        KwInt@3..6 "int"
                      Whitespace@6..7 " "
                      At@7..8 "@"
                      Whitespace@8..9 " "
                      LeftParen@9..10 "("
                      LiteralExpr@10..11
                        IntLiteral@10..11 "1"
                      RightParen@11..12 ")"
                    Whitespace@12..13 " "
                    GreaterEqu@13..15 ">="
                    Whitespace@15..16 " "
                    LiteralExpr@16..17
                      IntLiteral@16..17 "1""#]],
    );
}

#[test]
fn parse_nat_cheat_as_ref() {
    check(
        "#woo := 1",
        expect![[r##"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NatCheatExpr@0..4
                    Pound@0..1 "#"
                    NameExpr@1..4
                      Name@1..4
                        Identifier@1..4 "woo"
                  Whitespace@4..5 " "
                  AsnOp@5..7
                    Assign@5..7 ":="
                  Whitespace@7..8 " "
                  LiteralExpr@8..9
                    IntLiteral@8..9 "1""##]],
    );
}

#[test]
fn parse_sizeof_expr_constexpr() {
    check(
        "_:=sizeof(1+2+3)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                AssignStmt@0..16
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..16
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    BinaryExpr@10..15
                      BinaryExpr@10..13
                        LiteralExpr@10..11
                          IntLiteral@10..11 "1"
                        Plus@11..12 "+"
                        LiteralExpr@12..13
                          IntLiteral@12..13 "2"
                      Plus@13..14 "+"
                      LiteralExpr@14..15
                        IntLiteral@14..15 "3"
                    RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_sizeof_expr_varref() {
    check(
        "_:=sizeof(a.b.c.d)",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                AssignStmt@0..18
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..18
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    FieldExpr@10..17
                      FieldExpr@10..15
                        FieldExpr@10..13
                          NameExpr@10..11
                            Name@10..11
                              Identifier@10..11 "a"
                          Dot@11..12 "."
                          Name@12..13
                            Identifier@12..13 "b"
                        Dot@13..14 "."
                        Name@14..15
                          Identifier@14..15 "c"
                      Dot@15..16 "."
                      Name@16..17
                        Identifier@16..17 "d"
                    RightParen@17..18 ")""#]],
    );
}

#[test]
fn parse_sizeof_expr_ty_prim() {
    check(
        "_:=sizeof(int)",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                AssignStmt@0..14
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..14
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    PrimType@10..13
                      KwInt@10..13 "int"
                    RightParen@13..14 ")""#]],
    );
}

#[test]
fn parse_sizeof_expr_ty_prim_sized() {
    check(
        "_:=sizeof(char(8))",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                AssignStmt@0..18
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..18
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    PrimType@10..17
                      SizedCharType@10..17
                        KwChar@10..14 "char"
                        LeftParen@14..15 "("
                        SeqLength@15..16
                          LiteralExpr@15..16
                            IntLiteral@15..16 "8"
                        RightParen@16..17 ")"
                    RightParen@17..18 ")""#]],
    );
}

#[test]
fn parse_sizeof_expr_ty_prim_dyn_sized() {
    // reject during validation
    check(
        "_:=sizeof(char(*))",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                AssignStmt@0..18
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..18
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    PrimType@10..17
                      SizedCharType@10..17
                        KwChar@10..14 "char"
                        LeftParen@14..15 "("
                        SeqLength@15..16
                          Star@15..16 "*"
                        RightParen@16..17 ")"
                    RightParen@17..18 ")""#]],
    );
}

#[test]
fn parse_sizeof_expr_not_ty_prim() {
    // reject in validation
    check(
        "_:=sizeof(collection of int)",
        expect![[r#"
            Source@0..28
              StmtList@0..28
                AssignStmt@0..28
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..28
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    CollectionType@10..27
                      KwCollection@10..20 "collection"
                      Whitespace@20..21 " "
                      KwOf@21..23 "of"
                      Whitespace@23..24 " "
                      PrimType@24..27
                        KwInt@24..27 "int"
                    RightParen@27..28 ")""#]],
    );
}

#[test]
fn recover_sizeof_expr_missing_right_paren() {
    check(
        "_:=sizeof(a",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..11
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    NameExpr@10..11
                      Name@10..11
                        Identifier@10..11 "a"
            error in file FileId(1) at 10..11: unexpected end of file
            | error in file FileId(1) for 10..11: expected ‘)’ after here"#]],
    );
}

#[test]
fn recover_sizeof_expr_missing_left_paren() {
    check(
        "_:=sizeof a)",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                AssignStmt@0..12
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..12
                    KwSizeOf@3..9 "sizeof"
                    Whitespace@9..10 " "
                    NameExpr@10..11
                      Name@10..11
                        Identifier@10..11 "a"
                    RightParen@11..12 ")"
            error in file FileId(1) at 10..11: unexpected token
            | error in file FileId(1) for 10..11: expected ‘(’, but found identifier"#]],
    );
}

#[test]
fn recover_sizeof_expr_missing_parens() {
    check(
        "_:=sizeof a",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..11
                    KwSizeOf@3..9 "sizeof"
                    Whitespace@9..10 " "
                    NameExpr@10..11
                      Name@10..11
                        Identifier@10..11 "a"
            error in file FileId(1) at 10..11: unexpected token
            | error in file FileId(1) for 10..11: expected ‘(’, but found identifier"#]],
    );
}

#[test]
fn recover_sizeof_expr_missing_arg() {
    check(
        "_:=sizeof()",
        expect![[r#"
            Source@0..11
              StmtList@0..11
                AssignStmt@0..11
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  SizeOfExpr@3..11
                    KwSizeOf@3..9 "sizeof"
                    LeftParen@9..10 "("
                    RightParen@10..11 ")"
            error in file FileId(1) at 10..11: unexpected token
            | error in file FileId(1) for 10..11: expected expression, but found ‘)’"#]],
    );
}

#[test]
fn parse_not_eq_after_higher_precedence_op() {
    check(
        "_:=A mod B not= 0",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                AssignStmt@0..17
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..17
                    BinaryExpr@3..10
                      NameExpr@3..4
                        Name@3..4
                          Identifier@3..4 "A"
                      Whitespace@4..5 " "
                      KwMod@5..8 "mod"
                      Whitespace@8..9 " "
                      NameExpr@9..10
                        Name@9..10
                          Identifier@9..10 "B"
                    Whitespace@10..11 " "
                    NotEq@11..15
                      KwNot@11..14 "not"
                      Equ@14..15 "="
                    Whitespace@15..16 " "
                    LiteralExpr@16..17
                      IntLiteral@16..17 "0""#]],
    );
}

#[test]
fn parse_invalid_tilde_before_indirect() {
    check(
        "_:=A~B@()",
        expect![[r#"
            Source@0..9
              StmtList@0..9
                AssignStmt@0..9
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..9
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "A"
                    Error@4..5
                      Tilde@4..5 "~"
                    IndirectExpr@5..9
                      NameExpr@5..6
                        Name@5..6
                          Identifier@5..6 "B"
                      At@6..7 "@"
                      LeftParen@7..8 "("
                      RightParen@8..9 ")"
            error in file FileId(1) at 5..6: unexpected token
            | error in file FileId(1) for 5..6: expected ‘in’ or ‘=’, but found identifier"#]],
    );
}

#[test]
fn parse_invalid_not_before_indirect() {
    check(
        "_:=A not B@()",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                AssignStmt@0..13
                  NameExpr@0..1
                    Name@0..1
                      Identifier@0..1 "_"
                  AsnOp@1..3
                    Assign@1..3 ":="
                  BinaryExpr@3..13
                    NameExpr@3..4
                      Name@3..4
                        Identifier@3..4 "A"
                    Whitespace@4..5 " "
                    Error@5..8
                      KwNot@5..8 "not"
                    Whitespace@8..9 " "
                    IndirectExpr@9..13
                      NameExpr@9..10
                        Name@9..10
                          Identifier@9..10 "B"
                      At@10..11 "@"
                      LeftParen@11..12 "("
                      RightParen@12..13 ")"
            error in file FileId(1) at 9..10: unexpected token
            | error in file FileId(1) for 9..10: expected ‘in’ or ‘=’, but found identifier"#]],
    );
}

#[test]
fn recover_empty_parens() {
    // Should only be one error (expected expression, but found `)`)
    check(
        "_ := () q",
        expect![[r#"
        Source@0..9
          StmtList@0..9
            AssignStmt@0..7
              NameExpr@0..1
                Name@0..1
                  Identifier@0..1 "_"
              Whitespace@1..2 " "
              AsnOp@2..4
                Assign@2..4 ":="
              Whitespace@4..5 " "
              ParenExpr@5..7
                LeftParen@5..6 "("
                RightParen@6..7 ")"
            Whitespace@7..8 " "
            CallStmt@8..9
              NameExpr@8..9
                Name@8..9
                  Identifier@8..9 "q"
        error in file FileId(1) at 6..7: unexpected token
        | error in file FileId(1) for 6..7: expected expression, but found ‘)’"#]],
    );
}
