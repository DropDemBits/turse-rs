//! Tests for types
use crate::check;
use expect_test::expect;

#[test]
fn parse_basic_prim_types() {
    check(
        r#"
type _ : addressint
type _ : boolean
type _ : int
type _ : int1
type _ : int2
type _ : int4
type _ : nat
type _ : nat1
type _ : nat2
type _ : nat4
type _ : real
type _ : real4
type _ : real8
type _ : char
type _ : string
    "#,
        expect![[r#"
            Source@0..226
              Whitespace@0..1 "\n"
              StmtList@1..221
                TypeDecl@1..20
                  KwType@1..5 "type"
                  Whitespace@5..6 " "
                  Name@6..7
                    Identifier@6..7 "_"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..20
                    KwAddressint@10..20 "addressint"
                Whitespace@20..21 "\n"
                TypeDecl@21..37
                  KwType@21..25 "type"
                  Whitespace@25..26 " "
                  Name@26..27
                    Identifier@26..27 "_"
                  Whitespace@27..28 " "
                  Colon@28..29 ":"
                  Whitespace@29..30 " "
                  PrimType@30..37
                    KwBoolean@30..37 "boolean"
                Whitespace@37..38 "\n"
                TypeDecl@38..50
                  KwType@38..42 "type"
                  Whitespace@42..43 " "
                  Name@43..44
                    Identifier@43..44 "_"
                  Whitespace@44..45 " "
                  Colon@45..46 ":"
                  Whitespace@46..47 " "
                  PrimType@47..50
                    KwInt@47..50 "int"
                Whitespace@50..51 "\n"
                TypeDecl@51..64
                  KwType@51..55 "type"
                  Whitespace@55..56 " "
                  Name@56..57
                    Identifier@56..57 "_"
                  Whitespace@57..58 " "
                  Colon@58..59 ":"
                  Whitespace@59..60 " "
                  PrimType@60..64
                    KwInt1@60..64 "int1"
                Whitespace@64..65 "\n"
                TypeDecl@65..78
                  KwType@65..69 "type"
                  Whitespace@69..70 " "
                  Name@70..71
                    Identifier@70..71 "_"
                  Whitespace@71..72 " "
                  Colon@72..73 ":"
                  Whitespace@73..74 " "
                  PrimType@74..78
                    KwInt2@74..78 "int2"
                Whitespace@78..79 "\n"
                TypeDecl@79..92
                  KwType@79..83 "type"
                  Whitespace@83..84 " "
                  Name@84..85
                    Identifier@84..85 "_"
                  Whitespace@85..86 " "
                  Colon@86..87 ":"
                  Whitespace@87..88 " "
                  PrimType@88..92
                    KwInt4@88..92 "int4"
                Whitespace@92..93 "\n"
                TypeDecl@93..105
                  KwType@93..97 "type"
                  Whitespace@97..98 " "
                  Name@98..99
                    Identifier@98..99 "_"
                  Whitespace@99..100 " "
                  Colon@100..101 ":"
                  Whitespace@101..102 " "
                  PrimType@102..105
                    KwNat@102..105 "nat"
                Whitespace@105..106 "\n"
                TypeDecl@106..119
                  KwType@106..110 "type"
                  Whitespace@110..111 " "
                  Name@111..112
                    Identifier@111..112 "_"
                  Whitespace@112..113 " "
                  Colon@113..114 ":"
                  Whitespace@114..115 " "
                  PrimType@115..119
                    KwNat1@115..119 "nat1"
                Whitespace@119..120 "\n"
                TypeDecl@120..133
                  KwType@120..124 "type"
                  Whitespace@124..125 " "
                  Name@125..126
                    Identifier@125..126 "_"
                  Whitespace@126..127 " "
                  Colon@127..128 ":"
                  Whitespace@128..129 " "
                  PrimType@129..133
                    KwNat2@129..133 "nat2"
                Whitespace@133..134 "\n"
                TypeDecl@134..147
                  KwType@134..138 "type"
                  Whitespace@138..139 " "
                  Name@139..140
                    Identifier@139..140 "_"
                  Whitespace@140..141 " "
                  Colon@141..142 ":"
                  Whitespace@142..143 " "
                  PrimType@143..147
                    KwNat4@143..147 "nat4"
                Whitespace@147..148 "\n"
                TypeDecl@148..161
                  KwType@148..152 "type"
                  Whitespace@152..153 " "
                  Name@153..154
                    Identifier@153..154 "_"
                  Whitespace@154..155 " "
                  Colon@155..156 ":"
                  Whitespace@156..157 " "
                  PrimType@157..161
                    KwReal@157..161 "real"
                Whitespace@161..162 "\n"
                TypeDecl@162..176
                  KwType@162..166 "type"
                  Whitespace@166..167 " "
                  Name@167..168
                    Identifier@167..168 "_"
                  Whitespace@168..169 " "
                  Colon@169..170 ":"
                  Whitespace@170..171 " "
                  PrimType@171..176
                    KwReal4@171..176 "real4"
                Whitespace@176..177 "\n"
                TypeDecl@177..191
                  KwType@177..181 "type"
                  Whitespace@181..182 " "
                  Name@182..183
                    Identifier@182..183 "_"
                  Whitespace@183..184 " "
                  Colon@184..185 ":"
                  Whitespace@185..186 " "
                  PrimType@186..191
                    KwReal8@186..191 "real8"
                Whitespace@191..192 "\n"
                TypeDecl@192..205
                  KwType@192..196 "type"
                  Whitespace@196..197 " "
                  Name@197..198
                    Identifier@197..198 "_"
                  Whitespace@198..199 " "
                  Colon@199..200 ":"
                  Whitespace@200..201 " "
                  PrimType@201..205
                    KwChar@201..205 "char"
                Whitespace@205..206 "\n"
                TypeDecl@206..221
                  KwType@206..210 "type"
                  Whitespace@210..211 " "
                  Name@211..212
                    Identifier@211..212 "_"
                  Whitespace@212..213 " "
                  Colon@213..214 ":"
                  Whitespace@214..215 " "
                  PrimType@215..221
                    KwString@215..221 "string"
              Whitespace@221..226 "\n    ""#]],
    );
}

#[test]
fn parse_sized_char_type() {
    check(
        "type sc : char(1 + k)",
        expect![[r#"
            Source@0..21
              StmtList@0..21
                TypeDecl@0..21
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..21
                    SizedCharType@10..21
                      KwChar@10..14 "char"
                      LeftParen@14..15 "("
                      SeqLength@15..20
                        BinaryExpr@15..20
                          LiteralExpr@15..16
                            IntLiteral@15..16 "1"
                          Whitespace@16..17 " "
                          Plus@17..18 "+"
                          Whitespace@18..19 " "
                          NameExpr@19..20
                            NameRef@19..20
                              Identifier@19..20 "k"
                      RightParen@20..21 ")""#]],
    );
}

#[test]
fn parse_sized_string_type() {
    check(
        "type sc : string(1 + k)",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                TypeDecl@0..23
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..23
                    SizedStringType@10..23
                      KwString@10..16 "string"
                      LeftParen@16..17 "("
                      SeqLength@17..22
                        BinaryExpr@17..22
                          LiteralExpr@17..18
                            IntLiteral@17..18 "1"
                          Whitespace@18..19 " "
                          Plus@19..20 "+"
                          Whitespace@20..21 " "
                          NameExpr@21..22
                            NameRef@21..22
                              Identifier@21..22 "k"
                      RightParen@22..23 ")""#]],
    );
}

#[test]
fn parse_any_sized_char_type() {
    check(
        "type _ : char(*)",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                TypeDecl@0..16
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PrimType@9..16
                    SizedCharType@9..16
                      KwChar@9..13 "char"
                      LeftParen@13..14 "("
                      SeqLength@14..15
                        Star@14..15 "*"
                      RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_any_sized_string_type() {
    check(
        "type _ : string(*)",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PrimType@9..18
                    SizedStringType@9..18
                      KwString@9..15 "string"
                      LeftParen@15..16 "("
                      SeqLength@16..17
                        Star@16..17 "*"
                      RightParen@17..18 ")""#]],
    );
}

#[test]
fn recover_empty_sized_char_type() {
    check(
        "type sc : char()",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                TypeDecl@0..16
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..16
                    SizedCharType@10..16
                      KwChar@10..14 "char"
                      LeftParen@14..15 "("
                      SeqLength@15..15
                      RightParen@15..16 ")"
            error at 15..16: unexpected token
            | error for 15..16: expected expression, but found `)`"#]],
    );
}

#[test]
fn recover_empty_sized_string_type() {
    check(
        "type sc : string()",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..18
                    SizedStringType@10..18
                      KwString@10..16 "string"
                      LeftParen@16..17 "("
                      SeqLength@17..17
                      RightParen@17..18 ")"
            error at 17..18: unexpected token
            | error for 17..18: expected expression, but found `)`"#]],
    );
}

#[test]
fn recover_not_expr_in_sized_char_type() {
    check(
        "type sc : char(to)",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..18
                    SizedCharType@10..18
                      KwChar@10..14 "char"
                      LeftParen@14..15 "("
                      SeqLength@15..17
                        Error@15..17
                          KwTo@15..17 "to"
                      RightParen@17..18 ")"
            error at 15..17: unexpected token
            | error for 15..17: expected expression, but found `to`"#]],
    );
}

#[test]
fn recover_not_expr_in_sized_string_type() {
    check(
        "type sc : string(to)",
        expect![[r#"
            Source@0..20
              StmtList@0..20
                TypeDecl@0..20
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..20
                    SizedStringType@10..20
                      KwString@10..16 "string"
                      LeftParen@16..17 "("
                      SeqLength@17..19
                        Error@17..19
                          KwTo@17..19 "to"
                      RightParen@19..20 ")"
            error at 17..19: unexpected token
            | error for 17..19: expected expression, but found `to`"#]],
    );
}

#[test]
fn recover_missing_right_paren_in_sized_char_type() {
    check(
        "type sc : char(1",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                TypeDecl@0..16
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..16
                    SizedCharType@10..16
                      KwChar@10..14 "char"
                      LeftParen@14..15 "("
                      SeqLength@15..16
                        LiteralExpr@15..16
                          IntLiteral@15..16 "1"
            error at 15..16: unexpected end of file
            | error for 15..16: expected `)` after here"#]],
    );
}

#[test]
fn recover_missing_right_paren_in_sized_string_type() {
    check(
        "type sc : string(1",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..7
                    Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                  Colon@8..9 ":"
                  Whitespace@9..10 " "
                  PrimType@10..18
                    SizedStringType@10..18
                      KwString@10..16 "string"
                      LeftParen@16..17 "("
                      SeqLength@17..18
                        LiteralExpr@17..18
                          IntLiteral@17..18 "1"
            error at 17..18: unexpected end of file
            | error for 17..18: expected `)` after here"#]],
    );
}

#[test]
fn parse_name_type() {
    check(
        "type _ : a",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                TypeDecl@0..10
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  NameType@9..10
                    NameExpr@9..10
                      NameRef@9..10
                        Identifier@9..10 "a""#]],
    );
    check(
        "type _ : a.b.c",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                TypeDecl@0..14
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  NameType@9..14
                    FieldExpr@9..14
                      FieldExpr@9..12
                        NameExpr@9..10
                          NameRef@9..10
                            Identifier@9..10 "a"
                        Dot@10..11 "."
                        NameRef@11..12
                          Identifier@11..12 "b"
                      Dot@12..13 "."
                      NameRef@13..14
                        Identifier@13..14 "c""#]],
    );
}

#[test]
fn parse_name_type_not_a_ref() {
    // primaries still included because of range expr expecting a general expr
    check(
        "type _ : 1",
        expect![[r#"
            Source@0..10
              StmtList@0..10
                TypeDecl@0..10
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  NameType@9..10
                    LiteralExpr@9..10
                      IntLiteral@9..10 "1""#]],
    );
    check(
        r#"type _ : "hello world""#,
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  NameType@9..22
                    LiteralExpr@9..22
                      StringLiteral@9..22 "\"hello world\"""#]],
    );
}

#[test]
fn parse_expr_as_name_type() {
    check(
        "type _ : 1 + 2 + 3 - 4",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  NameType@9..22
                    BinaryExpr@9..22
                      BinaryExpr@9..18
                        BinaryExpr@9..14
                          LiteralExpr@9..10
                            IntLiteral@9..10 "1"
                          Whitespace@10..11 " "
                          Plus@11..12 "+"
                          Whitespace@12..13 " "
                          LiteralExpr@13..14
                            IntLiteral@13..14 "2"
                        Whitespace@14..15 " "
                        Plus@15..16 "+"
                        Whitespace@16..17 " "
                        LiteralExpr@17..18
                          IntLiteral@17..18 "3"
                      Whitespace@18..19 " "
                      Minus@19..20 "-"
                      Whitespace@20..21 " "
                      LiteralExpr@21..22
                        IntLiteral@21..22 "4""#]],
    );
}

#[test]
fn parse_range_type() {
    check(
        "type _ : 1 .. 2",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                TypeDecl@0..15
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..15
                    LiteralExpr@9..10
                      IntLiteral@9..10 "1"
                    Whitespace@10..11 " "
                    Range@11..13 ".."
                    Whitespace@13..14 " "
                    LiteralExpr@14..15
                      IntLiteral@14..15 "2""#]],
    );
}

#[test]
fn parse_range_type_packed_attr() {
    check(
        "type _ : packed 1 .. 2",
        expect![[r#"
        Source@0..22
          StmtList@0..22
            TypeDecl@0..22
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              RangeType@9..22
                KwPacked@9..15 "packed"
                Whitespace@15..16 " "
                LiteralExpr@16..17
                  IntLiteral@16..17 "1"
                Whitespace@17..18 " "
                Range@18..20 ".."
                Whitespace@20..21 " "
                LiteralExpr@21..22
                  IntLiteral@21..22 "2""#]],
    );
}

#[test]
fn recover_range_type_packed_only_head() {
    check(
        "type _ : packed 1",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                TypeDecl@0..17
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  Error@9..17
                    KwPacked@9..15 "packed"
                    Whitespace@15..16 " "
                    LiteralExpr@16..17
                      IntLiteral@16..17 "1"
            error at 16..17: unexpected end of file
            | error for 16..17: expected type specifier after here"#]],
    )
}

#[test]
fn recover_range_type_packed_indirection_head() {
    // will conflict with just `packed int`, but gives an okay error for now
    check(
        "type _ : packed int @ (2) .. 3",
        expect![[r#"
            Source@0..30
              StmtList@0..30
                TypeDecl@0..30
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..30
                    KwPacked@9..15 "packed"
                    Whitespace@15..16 " "
                    IndirectExpr@16..25
                      PrimType@16..19
                        KwInt@16..19 "int"
                      Whitespace@19..20 " "
                      At@20..21 "@"
                      Whitespace@21..22 " "
                      LeftParen@22..23 "("
                      LiteralExpr@23..24
                        IntLiteral@23..24 "2"
                      RightParen@24..25 ")"
                    Whitespace@25..26 " "
                    Range@26..28 ".."
                    Whitespace@28..29 " "
                    LiteralExpr@29..30
                      IntLiteral@29..30 "3"
            error at 16..19: unexpected token
            | error for 16..19: expected `array`, `enum`, `set`, `record`, `union`, or a range type, but found `int`"#]],
    )
}

#[test]
fn parse_range_type_size_spec() {
    check(
        "type _ : 1 .. 2 : 2",
        expect![[r#"
        Source@0..19
          StmtList@0..19
            TypeDecl@0..19
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              RangeType@9..19
                LiteralExpr@9..10
                  IntLiteral@9..10 "1"
                Whitespace@10..11 " "
                Range@11..13 ".."
                Whitespace@13..14 " "
                LiteralExpr@14..15
                  IntLiteral@14..15 "2"
                Whitespace@15..16 " "
                SizeSpec@16..19
                  Colon@16..17 ":"
                  Whitespace@17..18 " "
                  LiteralExpr@18..19
                    IntLiteral@18..19 "2""#]],
    )
}

#[test]
fn recover_range_type_size_spec_missing_expr() {
    check(
        "type _ : 1 .. 2 : ",
        expect![[r#"
            Source@0..18
              StmtList@0..17
                TypeDecl@0..17
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..17
                    LiteralExpr@9..10
                      IntLiteral@9..10 "1"
                    Whitespace@10..11 " "
                    Range@11..13 ".."
                    Whitespace@13..14 " "
                    LiteralExpr@14..15
                      IntLiteral@14..15 "2"
                    Whitespace@15..16 " "
                    SizeSpec@16..17
                      Colon@16..17 ":"
              Whitespace@17..18 " "
            error at 16..17: unexpected end of file
            | error for 16..17: expected expression after here"#]],
    );
}

#[test]
fn parse_unbounded_range_type() {
    check(
        "type _ : 1 .. *",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                TypeDecl@0..15
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..15
                    LiteralExpr@9..10
                      IntLiteral@9..10 "1"
                    Whitespace@10..11 " "
                    Range@11..13 ".."
                    Whitespace@13..14 " "
                    UnsizedBound@14..15
                      Star@14..15 "*""#]],
    );
}

#[test]
fn recover_range_type_missing_tail() {
    check(
        "type _ : 1 ..",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                TypeDecl@0..13
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..13
                    LiteralExpr@9..10
                      IntLiteral@9..10 "1"
                    Whitespace@10..11 " "
                    Range@11..13 ".."
            error at 11..13: unexpected end of file
            | error for 11..13: expected expression after here"#]],
    );
}

#[test]
fn recover_range_type_missing_head() {
    check(
        "type _ : .. 1",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                TypeDecl@0..13
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..13
                    Range@9..11 ".."
                    Whitespace@11..12 " "
                    LiteralExpr@12..13
                      IntLiteral@12..13 "1"
            error at 9..11: unexpected token
            | error for 9..11: expected expression, but found `..`"#]],
    );
}

#[test]
fn recover_range_type_not_an_expr() {
    check(
        "type _ : 1 .. boolean",
        expect![[r#"
            Source@0..21
              StmtList@0..21
                TypeDecl@0..21
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RangeType@9..21
                    LiteralExpr@9..10
                      IntLiteral@9..10 "1"
                    Whitespace@10..11 " "
                    Range@11..13 ".."
                    Whitespace@13..14 " "
                    Error@14..21
                      PrimType@14..21
                        KwBoolean@14..21 "boolean"
            error at 14..21: unexpected end of file
            | error for 14..21: expected `@` after here"#]],
    );
}

#[test]
fn parse_pointer_type() {
    check(
        "type _ : pointer to int",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                TypeDecl@0..23
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..23
                    KwPointer@9..16 "pointer"
                    Whitespace@16..17 " "
                    KwTo@17..19 "to"
                    Whitespace@19..20 " "
                    PrimType@20..23
                      KwInt@20..23 "int""#]],
    );
}

#[test]
fn parse_pointer_type_to_named() {
    check(
        "type _ : pointer to some.named.ty",
        expect![[r#"
            Source@0..33
              StmtList@0..33
                TypeDecl@0..33
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..33
                    KwPointer@9..16 "pointer"
                    Whitespace@16..17 " "
                    KwTo@17..19 "to"
                    Whitespace@19..20 " "
                    NameType@20..33
                      FieldExpr@20..33
                        FieldExpr@20..30
                          NameExpr@20..24
                            NameRef@20..24
                              Identifier@20..24 "some"
                          Dot@24..25 "."
                          NameRef@25..30
                            Identifier@25..30 "named"
                        Dot@30..31 "."
                        NameRef@31..33
                          Identifier@31..33 "ty""#]],
    );
}

#[test]
fn parse_unchecked_pointer_type() {
    check(
        "type _ : unchecked pointer to addressint",
        expect![[r#"
            Source@0..40
              StmtList@0..40
                TypeDecl@0..40
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..40
                    Checkedness@9..18
                      KwUnchecked@9..18 "unchecked"
                    Whitespace@18..19 " "
                    KwPointer@19..26 "pointer"
                    Whitespace@26..27 " "
                    KwTo@27..29 "to"
                    Whitespace@29..30 " "
                    PrimType@30..40
                      KwAddressint@30..40 "addressint""#]],
    );
}

#[test]
fn parse_unchecked_short_pointer_type() {
    check(
        "type _ : unchecked ^addressint",
        expect![[r#"
            Source@0..30
              StmtList@0..30
                TypeDecl@0..30
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..30
                    Checkedness@9..18
                      KwUnchecked@9..18 "unchecked"
                    Whitespace@18..19 " "
                    Caret@19..20 "^"
                    PrimType@20..30
                      KwAddressint@20..30 "addressint""#]],
    );
}

#[test]
fn parse_short_pointer_type() {
    check(
        "type _ : ^int",
        expect![[r#"
            Source@0..13
              StmtList@0..13
                TypeDecl@0..13
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..13
                    Caret@9..10 "^"
                    PrimType@10..13
                      KwInt@10..13 "int""#]],
    );
}

#[test]
fn recover_pointer_type_missing_to() {
    check(
        "type _ : pointer addressint",
        expect![[r#"
            Source@0..27
              StmtList@0..27
                TypeDecl@0..27
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..27
                    KwPointer@9..16 "pointer"
                    Whitespace@16..17 " "
                    PrimType@17..27
                      KwAddressint@17..27 "addressint"
            error at 17..27: unexpected token
            | error for 17..27: expected `to`, but found `addressint`"#]],
    );
}

#[test]
fn recover_pointer_type_just_unchecked() {
    check(
        "type _ : unchecked",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  PointerType@9..18
                    Checkedness@9..18
                      KwUnchecked@9..18 "unchecked"
            error at 9..18: unexpected end of file
            | error for 9..18: expected `^` or `pointer` after here"#]],
    );
}

#[test]
fn parse_enum_type() {
    check(
        "type _ : enum (a)",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                TypeDecl@0..17
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..17
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..16
                      Name@15..16
                        Identifier@15..16 "a"
                    RightParen@16..17 ")""#]],
    )
}

#[test]
fn parse_enum_type_packed_attr() {
    check(
        "type _ : packed enum(a)",
        expect![[r#"
        Source@0..23
          StmtList@0..23
            TypeDecl@0..23
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              EnumType@9..23
                KwPacked@9..15 "packed"
                Whitespace@15..16 " "
                KwEnum@16..20 "enum"
                LeftParen@20..21 "("
                NameList@21..22
                  Name@21..22
                    Identifier@21..22 "a"
                RightParen@22..23 ")""#]],
    );
}

#[test]
fn parse_enum_type_size_spec() {
    check(
        "type _ : enum(a) : 1 + 2",
        expect![[r#"
        Source@0..24
          StmtList@0..24
            TypeDecl@0..24
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              EnumType@9..24
                KwEnum@9..13 "enum"
                LeftParen@13..14 "("
                NameList@14..15
                  Name@14..15
                    Identifier@14..15 "a"
                RightParen@15..16 ")"
                Whitespace@16..17 " "
                SizeSpec@17..24
                  Colon@17..18 ":"
                  Whitespace@18..19 " "
                  BinaryExpr@19..24
                    LiteralExpr@19..20
                      IntLiteral@19..20 "1"
                    Whitespace@20..21 " "
                    Plus@21..22 "+"
                    Whitespace@22..23 " "
                    LiteralExpr@23..24
                      IntLiteral@23..24 "2""#]],
    );
}

#[test]
fn recover_enum_type_size_spec_missing_expr() {
    check(
        "type _ : enum(a) : ",
        expect![[r#"
            Source@0..19
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..18
                    KwEnum@9..13 "enum"
                    LeftParen@13..14 "("
                    NameList@14..15
                      Name@14..15
                        Identifier@14..15 "a"
                    RightParen@15..16 ")"
                    Whitespace@16..17 " "
                    SizeSpec@17..18
                      Colon@17..18 ":"
              Whitespace@18..19 " "
            error at 17..18: unexpected end of file
            | error for 17..18: expected expression after here"#]],
    );
}

#[test]
fn parse_enum_type_multiple_names() {
    check(
        "type _ : enum (a, b, c)",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                TypeDecl@0..23
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..23
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..22
                      Name@15..16
                        Identifier@15..16 "a"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      Name@18..19
                        Identifier@18..19 "b"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      Name@21..22
                        Identifier@21..22 "c"
                    RightParen@22..23 ")""#]],
    )
}

#[test]
fn recover_enum_type_missing_delimiter() {
    check(
        "type _ : enum (a, b c)",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..19
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..19
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..19
                      Name@15..16
                        Identifier@15..16 "a"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      Name@18..19
                        Identifier@18..19 "b"
                Whitespace@19..20 " "
                CallStmt@20..21
                  NameExpr@20..21
                    NameRef@20..21
                      Identifier@20..21 "c"
                Error@21..22
                  RightParen@21..22 ")"
            error at 20..21: unexpected token
            | error for 20..21: expected `,` or `)`, but found identifier
            error at 21..22: unexpected token
            | error for 21..22: expected statement, but found `)`"#]],
    )
}

#[test]
fn recover_enum_type_missing_name() {
    check(
        "type _ : enum (a, b, )",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..22
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..20
                      Name@15..16
                        Identifier@15..16 "a"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      Name@18..19
                        Identifier@18..19 "b"
                      Comma@19..20 ","
                    Whitespace@20..21 " "
                    RightParen@21..22 ")"
            error at 21..22: unexpected token
            | error for 21..22: expected identifier, but found `)`"#]],
    );

    check(
        "type _ : enum (a, , c)",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..22
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..21
                      Name@15..16
                        Identifier@15..16 "a"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      Comma@18..19 ","
                      Whitespace@19..20 " "
                      Name@20..21
                        Identifier@20..21 "c"
                    RightParen@21..22 ")"
            error at 18..19: unexpected token
            | error for 18..19: expected identifier, but found `,`"#]],
    );
}

#[test]
fn recover_enum_type_missing_name_and_right_paren() {
    check(
        "type _ : enum (a, b,",
        expect![[r#"
            Source@0..20
              StmtList@0..20
                TypeDecl@0..20
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..20
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..20
                      Name@15..16
                        Identifier@15..16 "a"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      Name@18..19
                        Identifier@18..19 "b"
                      Comma@19..20 ","
            error at 19..20: unexpected end of file
            | error for 19..20: expected identifier after here"#]],
    )
}

#[test]
fn recover_enum_type_missing_right_paren() {
    check(
        "type _ : enum (a, b",
        expect![[r#"
            Source@0..19
              StmtList@0..19
                TypeDecl@0..19
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..19
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..19
                      Name@15..16
                        Identifier@15..16 "a"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      Name@18..19
                        Identifier@18..19 "b"
            error at 18..19: unexpected end of file
            | error for 18..19: expected `,` or `)` after here"#]],
    )
}

#[test]
fn recover_enum_type_missing_names() {
    check(
        "type _ : enum ()",
        expect![[r#"
            Source@0..16
              StmtList@0..16
                TypeDecl@0..16
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  EnumType@9..16
                    KwEnum@9..13 "enum"
                    Whitespace@13..14 " "
                    LeftParen@14..15 "("
                    NameList@15..15
                    RightParen@15..16 ")"
            error at 15..16: unexpected token
            | error for 15..16: expected identifier, but found `)`"#]],
    )
}

#[test]
fn parse_set_type() {
    check(
        "type _ : set of boolean",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                TypeDecl@0..23
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..23
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    KwOf@13..15 "of"
                    Whitespace@15..16 " "
                    PrimType@16..23
                      KwBoolean@16..23 "boolean""#]],
    );
}

#[test]
fn parse_set_type_packed_attr() {
    check(
        "type _ : packed set of boolean",
        expect![[r#"
        Source@0..30
          StmtList@0..30
            TypeDecl@0..30
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              SetType@9..30
                KwPacked@9..15 "packed"
                Whitespace@15..16 " "
                KwSet@16..19 "set"
                Whitespace@19..20 " "
                KwOf@20..22 "of"
                Whitespace@22..23 " "
                PrimType@23..30
                  KwBoolean@23..30 "boolean""#]],
    );
}

#[test]
fn parse_set_type_size_spec() {
    check(
        "type _ : set of boolean : 1 + 2",
        expect![[r#"
        Source@0..31
          StmtList@0..31
            TypeDecl@0..31
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              SetType@9..31
                KwSet@9..12 "set"
                Whitespace@12..13 " "
                KwOf@13..15 "of"
                Whitespace@15..16 " "
                PrimType@16..23
                  KwBoolean@16..23 "boolean"
                Whitespace@23..24 " "
                SizeSpec@24..31
                  Colon@24..25 ":"
                  Whitespace@25..26 " "
                  BinaryExpr@26..31
                    LiteralExpr@26..27
                      IntLiteral@26..27 "1"
                    Whitespace@27..28 " "
                    Plus@28..29 "+"
                    Whitespace@29..30 " "
                    LiteralExpr@30..31
                      IntLiteral@30..31 "2""#]],
    );
}

#[test]
fn recover_set_type_size_spec_missing_expr() {
    check(
        "type _ : set of boolean : ",
        expect![[r#"
            Source@0..26
              StmtList@0..25
                TypeDecl@0..25
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..25
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    KwOf@13..15 "of"
                    Whitespace@15..16 " "
                    PrimType@16..23
                      KwBoolean@16..23 "boolean"
                    Whitespace@23..24 " "
                    SizeSpec@24..25
                      Colon@24..25 ":"
              Whitespace@25..26 " "
            error at 24..25: unexpected end of file
            | error for 24..25: expected expression after here"#]],
    );
}

#[test]
fn parse_set_type_not_index_type() {
    // is still valid, checked in AST validation
    check(
        "type _ : set of int",
        expect![[r#"
            Source@0..19
              StmtList@0..19
                TypeDecl@0..19
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..19
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    KwOf@13..15 "of"
                    Whitespace@15..16 " "
                    PrimType@16..19
                      KwInt@16..19 "int""#]],
    );
}

#[test]
fn parse_set_type_of_range() {
    check(
        "type _ : set of 1 .. 3",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..22
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    KwOf@13..15 "of"
                    Whitespace@15..16 " "
                    RangeType@16..22
                      LiteralExpr@16..17
                        IntLiteral@16..17 "1"
                      Whitespace@17..18 " "
                      Range@18..20 ".."
                      Whitespace@20..21 " "
                      LiteralExpr@21..22
                        IntLiteral@21..22 "3""#]],
    );
}

#[test]
fn parse_set_type_of_unbounded_range() {
    // is still valid, checked in AST validation
    check(
        "type _ : set of 1 .. *",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..22
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    KwOf@13..15 "of"
                    Whitespace@15..16 " "
                    RangeType@16..22
                      LiteralExpr@16..17
                        IntLiteral@16..17 "1"
                      Whitespace@17..18 " "
                      Range@18..20 ".."
                      Whitespace@20..21 " "
                      UnsizedBound@21..22
                        Star@21..22 "*""#]],
    );
}

#[test]
fn recover_set_type_missing_of() {
    // is still valid, checked in AST validation
    check(
        "type _ : set char",
        expect![[r#"
            Source@0..17
              StmtList@0..17
                TypeDecl@0..17
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..17
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    PrimType@13..17
                      KwChar@13..17 "char"
            error at 13..17: unexpected token
            | error for 13..17: expected `of`, but found `char`"#]],
    );
}

#[test]
fn recover_set_type_missing_ty() {
    // is still valid, checked in AST validation
    check(
        "type _ : set of",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                TypeDecl@0..15
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..15
                    KwSet@9..12 "set"
                    Whitespace@12..13 " "
                    KwOf@13..15 "of"
            error at 13..15: unexpected end of file
            | error for 13..15: expected type specifier after here"#]],
    );
}

#[test]
fn recover_just_set() {
    // is still valid, checked in AST validation
    check(
        "type _ : set",
        expect![[r#"
            Source@0..12
              StmtList@0..12
                TypeDecl@0..12
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  SetType@9..12
                    KwSet@9..12 "set"
            error at 9..12: unexpected end of file
            | error for 9..12: expected `of` after here"#]],
    );
}

#[test]
fn parse_condition_type() {
    check(
        "type _ : condition",
        expect![[r#"
            Source@0..18
              StmtList@0..18
                TypeDecl@0..18
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ConditionType@9..18
                    KwCondition@9..18 "condition""#]],
    );
}

#[test]
fn parse_condition_type_attrs() {
    check(
        "type _ : priority condition",
        expect![[r#"
            Source@0..27
              StmtList@0..27
                TypeDecl@0..27
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ConditionType@9..27
                    ConditionKind@9..17
                      KwPriority@9..17 "priority"
                    Whitespace@17..18 " "
                    KwCondition@18..27 "condition""#]],
    );
    check(
        "type _ : deferred condition",
        expect![[r#"
            Source@0..27
              StmtList@0..27
                TypeDecl@0..27
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ConditionType@9..27
                    ConditionKind@9..17
                      KwDeferred@9..17 "deferred"
                    Whitespace@17..18 " "
                    KwCondition@18..27 "condition""#]],
    );
    check(
        "type _ : timeout condition",
        expect![[r#"
            Source@0..26
              StmtList@0..26
                TypeDecl@0..26
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ConditionType@9..26
                    ConditionKind@9..16
                      KwTimeout@9..16 "timeout"
                    Whitespace@16..17 " "
                    KwCondition@17..26 "condition""#]],
    );
}

#[test]
fn recover_condition_type_attrs_without_condition() {
    check(
        "type _ : priority not_condition",
        expect![[r#"
            Source@0..31
              StmtList@0..31
                TypeDecl@0..31
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ConditionType@9..31
                    ConditionKind@9..17
                      KwPriority@9..17 "priority"
                    Whitespace@17..18 " "
                    Error@18..31
                      Identifier@18..31 "not_condition"
            error at 18..31: unexpected token
            | error for 18..31: expected `condition`, but found identifier"#]],
    );
}

#[test]
fn parse_collection_type() {
    check(
        "type _ : collection of int",
        expect![[r#"
            Source@0..26
              StmtList@0..26
                TypeDecl@0..26
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  CollectionType@9..26
                    KwCollection@9..19 "collection"
                    Whitespace@19..20 " "
                    KwOf@20..22 "of"
                    Whitespace@22..23 " "
                    PrimType@23..26
                      KwInt@23..26 "int""#]],
    )
}

#[test]
fn parse_collection_type_forward() {
    check(
        "type _ : collection of forward a",
        expect![[r#"
            Source@0..32
              StmtList@0..32
                TypeDecl@0..32
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  CollectionType@9..32
                    KwCollection@9..19 "collection"
                    Whitespace@19..20 " "
                    KwOf@20..22 "of"
                    Whitespace@22..23 " "
                    KwForward@23..30 "forward"
                    Whitespace@30..31 " "
                    Name@31..32
                      Identifier@31..32 "a""#]],
    )
}

#[test]
fn recover_collection_type_no_ty() {
    check(
        "type _ : collection of",
        expect![[r#"
            Source@0..22
              StmtList@0..22
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  CollectionType@9..22
                    KwCollection@9..19 "collection"
                    Whitespace@19..20 " "
                    KwOf@20..22 "of"
            error at 20..22: unexpected end of file
            | error for 20..22: expected type specifier after here"#]],
    )
}

#[test]
fn recover_collection_type_no_of() {
    check(
        "type _ : collection int",
        expect![[r#"
            Source@0..23
              StmtList@0..23
                TypeDecl@0..23
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  CollectionType@9..23
                    KwCollection@9..19 "collection"
                    Whitespace@19..20 " "
                    PrimType@20..23
                      KwInt@20..23 "int"
            error at 20..23: unexpected token
            | error for 20..23: expected `of`, but found `int`"#]],
    )
}

#[test]
fn recover_collection_type_no_forward_name() {
    check(
        "type _ : collection of forward",
        expect![[r#"
            Source@0..30
              StmtList@0..30
                TypeDecl@0..30
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  CollectionType@9..30
                    KwCollection@9..19 "collection"
                    Whitespace@19..20 " "
                    KwOf@20..22 "of"
                    Whitespace@22..23 " "
                    KwForward@23..30 "forward"
            error at 23..30: unexpected end of file
            | error for 23..30: expected identifier after here"#]],
    )
}

#[test]
fn recover_just_collection() {
    check(
        "type _ : collection",
        expect![[r#"
            Source@0..19
              StmtList@0..19
                TypeDecl@0..19
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  CollectionType@9..19
                    KwCollection@9..19 "collection"
            error at 9..19: unexpected end of file
            | error for 9..19: expected `of` after here"#]],
    )
}

#[test]
fn parse_array_type() {
    check(
        "type _ : array 1 .. 3 of int",
        expect![[r#"
            Source@0..28
              StmtList@0..28
                TypeDecl@0..28
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ArrayType@9..28
                    KwArray@9..14 "array"
                    Whitespace@14..15 " "
                    RangeList@15..21
                      RangeType@15..21
                        LiteralExpr@15..16
                          IntLiteral@15..16 "1"
                        Whitespace@16..17 " "
                        Range@17..19 ".."
                        Whitespace@19..20 " "
                        LiteralExpr@20..21
                          IntLiteral@20..21 "3"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
                    Whitespace@24..25 " "
                    PrimType@25..28
                      KwInt@25..28 "int""#]],
    );
}

#[test]
fn parse_array_type_packed_attr() {
    check(
        "type _ : packed array 1 .. 3 of int",
        expect![[r#"
        Source@0..35
          StmtList@0..35
            TypeDecl@0..35
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              ArrayType@9..35
                KwPacked@9..15 "packed"
                Whitespace@15..16 " "
                KwArray@16..21 "array"
                Whitespace@21..22 " "
                RangeList@22..28
                  RangeType@22..28
                    LiteralExpr@22..23
                      IntLiteral@22..23 "1"
                    Whitespace@23..24 " "
                    Range@24..26 ".."
                    Whitespace@26..27 " "
                    LiteralExpr@27..28
                      IntLiteral@27..28 "3"
                Whitespace@28..29 " "
                KwOf@29..31 "of"
                Whitespace@31..32 " "
                PrimType@32..35
                  KwInt@32..35 "int""#]],
    );
}

#[test]
fn parse_array_type_with_many_ranges() {
    check(
        "type _ : array 1 .. 3, boolean, char of string",
        expect![[r#"
            Source@0..46
              StmtList@0..46
                TypeDecl@0..46
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ArrayType@9..46
                    KwArray@9..14 "array"
                    Whitespace@14..15 " "
                    RangeList@15..36
                      RangeType@15..21
                        LiteralExpr@15..16
                          IntLiteral@15..16 "1"
                        Whitespace@16..17 " "
                        Range@17..19 ".."
                        Whitespace@19..20 " "
                        LiteralExpr@20..21
                          IntLiteral@20..21 "3"
                      Comma@21..22 ","
                      Whitespace@22..23 " "
                      PrimType@23..30
                        KwBoolean@23..30 "boolean"
                      Comma@30..31 ","
                      Whitespace@31..32 " "
                      PrimType@32..36
                        KwChar@32..36 "char"
                    Whitespace@36..37 " "
                    KwOf@37..39 "of"
                    Whitespace@39..40 " "
                    PrimType@40..46
                      KwString@40..46 "string""#]],
    );
}

#[test]
fn parse_flexible_array_type() {
    check(
        "type _ : flexible array 1 .. 3 of char(*)",
        expect![[r#"
            Source@0..41
              StmtList@0..41
                TypeDecl@0..41
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ArrayType@9..41
                    KwFlexible@9..17 "flexible"
                    Whitespace@17..18 " "
                    KwArray@18..23 "array"
                    Whitespace@23..24 " "
                    RangeList@24..30
                      RangeType@24..30
                        LiteralExpr@24..25
                          IntLiteral@24..25 "1"
                        Whitespace@25..26 " "
                        Range@26..28 ".."
                        Whitespace@28..29 " "
                        LiteralExpr@29..30
                          IntLiteral@29..30 "3"
                    Whitespace@30..31 " "
                    KwOf@31..33 "of"
                    Whitespace@33..34 " "
                    PrimType@34..41
                      SizedCharType@34..41
                        KwChar@34..38 "char"
                        LeftParen@38..39 "("
                        SeqLength@39..40
                          Star@39..40 "*"
                        RightParen@40..41 ")""#]],
    );
}

#[test]
fn parse_flexible_array_type_packed_attr() {
    check(
        "type _ : packed flexible array 1 .. 2 of int",
        expect![[r#"
        Source@0..44
          StmtList@0..44
            TypeDecl@0..44
              KwType@0..4 "type"
              Whitespace@4..5 " "
              Name@5..6
                Identifier@5..6 "_"
              Whitespace@6..7 " "
              Colon@7..8 ":"
              Whitespace@8..9 " "
              ArrayType@9..44
                KwPacked@9..15 "packed"
                Whitespace@15..16 " "
                KwFlexible@16..24 "flexible"
                Whitespace@24..25 " "
                KwArray@25..30 "array"
                Whitespace@30..31 " "
                RangeList@31..37
                  RangeType@31..37
                    LiteralExpr@31..32
                      IntLiteral@31..32 "1"
                    Whitespace@32..33 " "
                    Range@33..35 ".."
                    Whitespace@35..36 " "
                    LiteralExpr@36..37
                      IntLiteral@36..37 "2"
                Whitespace@37..38 " "
                KwOf@38..40 "of"
                Whitespace@40..41 " "
                PrimType@41..44
                  KwInt@41..44 "int""#]],
    );
}

#[test]
fn recover_flexible_packed_array() {
    check(
        "type _ : flexible packed array 1 .. 2 of int",
        expect![[r#"
            Source@0..44
              StmtList@0..44
                TypeDecl@0..44
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ArrayType@9..44
                    KwFlexible@9..17 "flexible"
                    Whitespace@17..18 " "
                    Error@18..24
                      KwPacked@18..24 "packed"
                    Whitespace@24..25 " "
                    KwArray@25..30 "array"
                    Whitespace@30..31 " "
                    RangeList@31..37
                      RangeType@31..37
                        LiteralExpr@31..32
                          IntLiteral@31..32 "1"
                        Whitespace@32..33 " "
                        Range@33..35 ".."
                        Whitespace@35..36 " "
                        LiteralExpr@36..37
                          IntLiteral@36..37 "2"
                    Whitespace@37..38 " "
                    KwOf@38..40 "of"
                    Whitespace@40..41 " "
                    PrimType@41..44
                      KwInt@41..44 "int"
            error at 18..24: unexpected token
            | error for 18..24: expected `array`, but found `packed`"#]],
    );
}

#[test]
fn recover_flexible_not_array() {
    check(
        "type _ : flexible not_array im_stmt",
        expect![[r#"
            Source@0..35
              StmtList@0..35
                TypeDecl@0..27
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  Error@9..27
                    KwFlexible@9..17 "flexible"
                    Whitespace@17..18 " "
                    Identifier@18..27 "not_array"
                Whitespace@27..28 " "
                CallStmt@28..35
                  NameExpr@28..35
                    NameRef@28..35
                      Identifier@28..35 "im_stmt"
            error at 18..27: unexpected token
            | error for 18..27: expected `array`, but found identifier"#]],
    );
}

#[test]
fn recover_array_empty_range_list() {
    check(
        "type _ : array of int",
        expect![[r#"
            Source@0..21
              StmtList@0..21
                TypeDecl@0..21
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ArrayType@9..21
                    KwArray@9..14 "array"
                    Whitespace@14..15 " "
                    RangeList@15..15
                    KwOf@15..17 "of"
                    Whitespace@17..18 " "
                    PrimType@18..21
                      KwInt@18..21 "int"
            error at 15..17: unexpected token
            | error for 15..17: expected type specifier, but found `of`"#]],
    );
}

#[test]
fn recover_array_no_elem_ty() {
    check(
        "type _ : array 1 .. 3 of",
        expect![[r#"
            Source@0..24
              StmtList@0..24
                TypeDecl@0..24
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ArrayType@9..24
                    KwArray@9..14 "array"
                    Whitespace@14..15 " "
                    RangeList@15..21
                      RangeType@15..21
                        LiteralExpr@15..16
                          IntLiteral@15..16 "1"
                        Whitespace@16..17 " "
                        Range@17..19 ".."
                        Whitespace@19..20 " "
                        LiteralExpr@20..21
                          IntLiteral@20..21 "3"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
            error at 22..24: unexpected end of file
            | error for 22..24: expected type specifier after here"#]],
    );
}

#[test]
fn parse_fcn_type() {
    check(
        "type _ : function _a (a, b : int) : int",
        expect![[r#"
            Source@0..39
              StmtList@0..39
                TypeDecl@0..39
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  FcnType@9..39
                    KwFunction@9..17 "function"
                    Whitespace@17..18 " "
                    Name@18..20
                      Identifier@18..20 "_a"
                    Whitespace@20..21 " "
                    ParamSpec@21..33
                      LeftParen@21..22 "("
                      ConstVarParam@22..32
                        NameList@22..26
                          Name@22..23
                            Identifier@22..23 "a"
                          Comma@23..24 ","
                          Whitespace@24..25 " "
                          Name@25..26
                            Identifier@25..26 "b"
                        Whitespace@26..27 " "
                        Colon@27..28 ":"
                        Whitespace@28..29 " "
                        PrimType@29..32
                          KwInt@29..32 "int"
                      RightParen@32..33 ")"
                    Whitespace@33..34 " "
                    Colon@34..35 ":"
                    Whitespace@35..36 " "
                    PrimType@36..39
                      KwInt@36..39 "int""#]],
    );
}

#[test]
fn parse_proc_type() {
    check(
        "type _ : procedure _a (a, b : int)",
        expect![[r#"
            Source@0..34
              StmtList@0..34
                TypeDecl@0..34
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..34
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..34
                      LeftParen@22..23 "("
                      ConstVarParam@23..33
                        NameList@23..27
                          Name@23..24
                            Identifier@23..24 "a"
                          Comma@24..25 ","
                          Whitespace@25..26 " "
                          Name@26..27
                            Identifier@26..27 "b"
                        Whitespace@27..28 " "
                        Colon@28..29 ":"
                        Whitespace@29..30 " "
                        PrimType@30..33
                          KwInt@30..33 "int"
                      RightParen@33..34 ")""#]],
    );
}

#[test]
fn recover_proc_type_with_result_ty() {
    check(
        "type _ : procedure _a (a, b : int) : int",
        expect![[r#"
            Source@0..40
              StmtList@0..40
                TypeDecl@0..34
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..34
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..34
                      LeftParen@22..23 "("
                      ConstVarParam@23..33
                        NameList@23..27
                          Name@23..24
                            Identifier@23..24 "a"
                          Comma@24..25 ","
                          Whitespace@25..26 " "
                          Name@26..27
                            Identifier@26..27 "b"
                        Whitespace@27..28 " "
                        Colon@28..29 ":"
                        Whitespace@29..30 " "
                        PrimType@30..33
                          KwInt@30..33 "int"
                      RightParen@33..34 ")"
                Whitespace@34..35 " "
                Error@35..36
                  Colon@35..36 ":"
                Whitespace@36..37 " "
                Error@37..40
                  PrimType@37..40
                    KwInt@37..40 "int"
            error at 35..36: unexpected token
            | error for 35..36: expected statement, but found `:`
            error at 37..40: unexpected end of file
            | error for 37..40: expected `@` after here"#]],
    );
}

#[test]
fn recover_fcn_type_without_result_ty() {
    check(
        "type _ : function _a (a, b : int)",
        expect![[r#"
            Source@0..33
              StmtList@0..33
                TypeDecl@0..33
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  FcnType@9..33
                    KwFunction@9..17 "function"
                    Whitespace@17..18 " "
                    Name@18..20
                      Identifier@18..20 "_a"
                    Whitespace@20..21 " "
                    ParamSpec@21..33
                      LeftParen@21..22 "("
                      ConstVarParam@22..32
                        NameList@22..26
                          Name@22..23
                            Identifier@22..23 "a"
                          Comma@23..24 ","
                          Whitespace@24..25 " "
                          Name@25..26
                            Identifier@25..26 "b"
                        Whitespace@26..27 " "
                        Colon@27..28 ":"
                        Whitespace@28..29 " "
                        PrimType@29..32
                          KwInt@29..32 "int"
                      RightParen@32..33 ")"
            error at 32..33: unexpected end of file
            | error for 32..33: expected `:` after here"#]],
    );
}

#[test]
fn parse_fcn_type_opt_name() {
    check(
        "type _ : function (a, b : int) : int",
        expect![[r#"
            Source@0..36
              StmtList@0..36
                TypeDecl@0..36
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  FcnType@9..36
                    KwFunction@9..17 "function"
                    Whitespace@17..18 " "
                    ParamSpec@18..30
                      LeftParen@18..19 "("
                      ConstVarParam@19..29
                        NameList@19..23
                          Name@19..20
                            Identifier@19..20 "a"
                          Comma@20..21 ","
                          Whitespace@21..22 " "
                          Name@22..23
                            Identifier@22..23 "b"
                        Whitespace@23..24 " "
                        Colon@24..25 ":"
                        Whitespace@25..26 " "
                        PrimType@26..29
                          KwInt@26..29 "int"
                      RightParen@29..30 ")"
                    Whitespace@30..31 " "
                    Colon@31..32 ":"
                    Whitespace@32..33 " "
                    PrimType@33..36
                      KwInt@33..36 "int""#]],
    );
}

#[test]
fn parse_fcn_type_no_params() {
    check(
        "type _ : function _a () : int",
        expect![[r#"
            Source@0..29
              StmtList@0..29
                TypeDecl@0..29
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  FcnType@9..29
                    KwFunction@9..17 "function"
                    Whitespace@17..18 " "
                    Name@18..20
                      Identifier@18..20 "_a"
                    Whitespace@20..21 " "
                    ParamSpec@21..23
                      LeftParen@21..22 "("
                      RightParen@22..23 ")"
                    Whitespace@23..24 " "
                    Colon@24..25 ":"
                    Whitespace@25..26 " "
                    PrimType@26..29
                      KwInt@26..29 "int""#]],
    );
}

#[test]
fn parse_fcn_type_opt_parens() {
    check(
        "type _ : function _a : int",
        expect![[r#"
            Source@0..26
              StmtList@0..26
                TypeDecl@0..26
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  FcnType@9..26
                    KwFunction@9..17 "function"
                    Whitespace@17..18 " "
                    Name@18..20
                      Identifier@18..20 "_a"
                    Whitespace@20..21 " "
                    Colon@21..22 ":"
                    Whitespace@22..23 " "
                    PrimType@23..26
                      KwInt@23..26 "int""#]],
    );
}

#[test]
fn parse_proc_type_fcn_param() {
    check(
        "type _ : procedure _a (function embed (a : char) : int)",
        expect![[r#"
            Source@0..55
              StmtList@0..55
                TypeDecl@0..55
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..55
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..55
                      LeftParen@22..23 "("
                      FcnType@23..54
                        KwFunction@23..31 "function"
                        Whitespace@31..32 " "
                        Name@32..37
                          Identifier@32..37 "embed"
                        Whitespace@37..38 " "
                        ParamSpec@38..48
                          LeftParen@38..39 "("
                          ConstVarParam@39..47
                            NameList@39..40
                              Name@39..40
                                Identifier@39..40 "a"
                            Whitespace@40..41 " "
                            Colon@41..42 ":"
                            Whitespace@42..43 " "
                            PrimType@43..47
                              KwChar@43..47 "char"
                          RightParen@47..48 ")"
                        Whitespace@48..49 " "
                        Colon@49..50 ":"
                        Whitespace@50..51 " "
                        PrimType@51..54
                          KwInt@51..54 "int"
                      RightParen@54..55 ")""#]],
    );
}

#[test]
fn parse_proc_type_all_constvar_attrs() {
    check(
        "type _ : procedure _a (var register a : cheat int)",
        expect![[r#"
            Source@0..50
              StmtList@0..50
                TypeDecl@0..50
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..50
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..50
                      LeftParen@22..23 "("
                      ConstVarParam@23..49
                        VarAttr@23..26
                          KwVar@23..26 "var"
                        Whitespace@26..27 " "
                        RegisterAttr@27..35
                          KwRegister@27..35 "register"
                        Whitespace@35..36 " "
                        NameList@36..37
                          Name@36..37
                            Identifier@36..37 "a"
                        Whitespace@37..38 " "
                        Colon@38..39 ":"
                        Whitespace@39..40 " "
                        CheatAttr@40..45
                          KwCheat@40..45 "cheat"
                        Whitespace@45..46 " "
                        PrimType@46..49
                          KwInt@46..49 "int"
                      RightParen@49..50 ")""#]],
    );
}

#[test]
fn recover_proc_type_constvar_attrs_missing_name() {
    check(
        "type _ : procedure _a (var register : int)",
        expect![[r#"
            Source@0..42
              StmtList@0..42
                TypeDecl@0..42
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..42
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..42
                      LeftParen@22..23 "("
                      ConstVarParam@23..41
                        VarAttr@23..26
                          KwVar@23..26 "var"
                        Whitespace@26..27 " "
                        RegisterAttr@27..35
                          KwRegister@27..35 "register"
                        Whitespace@35..36 " "
                        NameList@36..36
                        Colon@36..37 ":"
                        Whitespace@37..38 " "
                        PrimType@38..41
                          KwInt@38..41 "int"
                      RightParen@41..42 ")"
            error at 36..37: unexpected token
            | error for 36..37: expected identifier, but found `:`"#]],
    );
    check(
        "type _ : procedure _a (var : int)",
        expect![[r#"
            Source@0..33
              StmtList@0..33
                TypeDecl@0..33
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..33
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..33
                      LeftParen@22..23 "("
                      ConstVarParam@23..32
                        VarAttr@23..26
                          KwVar@23..26 "var"
                        Whitespace@26..27 " "
                        NameList@27..27
                        Colon@27..28 ":"
                        Whitespace@28..29 " "
                        PrimType@29..32
                          KwInt@29..32 "int"
                      RightParen@32..33 ")"
            error at 27..28: unexpected token
            | error for 27..28: expected identifier, but found `:`"#]],
    );
    check(
        "type _ : procedure _a (register : int)",
        expect![[r#"
            Source@0..38
              StmtList@0..38
                TypeDecl@0..38
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..38
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..38
                      LeftParen@22..23 "("
                      ConstVarParam@23..37
                        RegisterAttr@23..31
                          KwRegister@23..31 "register"
                        Whitespace@31..32 " "
                        NameList@32..32
                        Colon@32..33 ":"
                        Whitespace@33..34 " "
                        PrimType@34..37
                          KwInt@34..37 "int"
                      RightParen@37..38 ")"
            error at 32..33: unexpected token
            | error for 32..33: expected identifier, but found `:`"#]],
    );
}

#[test]
fn recover_proc_type_no_attrs_missing_name() {
    check(
        "type _ : procedure( : int)",
        expect![[r#"
            Source@0..26
              StmtList@0..26
                TypeDecl@0..26
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..26
                    KwProcedure@9..18 "procedure"
                    ParamSpec@18..26
                      LeftParen@18..19 "("
                      Whitespace@19..20 " "
                      ConstVarParam@20..25
                        NameList@20..20
                        Colon@20..21 ":"
                        Whitespace@21..22 " "
                        PrimType@22..25
                          KwInt@22..25 "int"
                      RightParen@25..26 ")"
            error at 20..21: unexpected token
            | error for 20..21: expected `)`, `function`, `procedure`, `var`, `register` or identifier, but found `:`"#]],
    )
}

#[test]
fn recover_proc_type_constvar_missing_ty() {
    check(
        "type _ : procedure _a (a : )",
        expect![[r#"
            Source@0..28
              StmtList@0..28
                TypeDecl@0..28
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  ProcType@9..28
                    KwProcedure@9..18 "procedure"
                    Whitespace@18..19 " "
                    Name@19..21
                      Identifier@19..21 "_a"
                    Whitespace@21..22 " "
                    ParamSpec@22..28
                      LeftParen@22..23 "("
                      ConstVarParam@23..26
                        NameList@23..24
                          Name@23..24
                            Identifier@23..24 "a"
                        Whitespace@24..25 " "
                        Colon@25..26 ":"
                      Whitespace@26..27 " "
                      RightParen@27..28 ")"
            error at 27..28: unexpected token
            | error for 27..28: expected type specifier, but found `)`"#]],
    );
}

#[test]
fn parse_record_type() {
    check(
        "type _ : record a : int end record",
        expect![[r#"
            Source@0..34
              StmtList@0..34
                TypeDecl@0..34
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RecordType@9..34
                    KwRecord@9..15 "record"
                    Whitespace@15..16 " "
                    RecordField@16..23
                      NameList@16..17
                        Name@16..17
                          Identifier@16..17 "a"
                      Whitespace@17..18 " "
                      Colon@18..19 ":"
                      Whitespace@19..20 " "
                      PrimType@20..23
                        KwInt@20..23 "int"
                    Whitespace@23..24 " "
                    EndGroup@24..34
                      KwEnd@24..27 "end"
                      Whitespace@27..28 " "
                      KwRecord@28..34 "record""#]],
    );
}

#[test]
fn parse_record_type_packed_attr() {
    check(
        "type _ : packed record a : int end record",
        expect![[r#"
            Source@0..41
              StmtList@0..41
                TypeDecl@0..41
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RecordType@9..41
                    KwPacked@9..15 "packed"
                    Whitespace@15..16 " "
                    KwRecord@16..22 "record"
                    Whitespace@22..23 " "
                    RecordField@23..30
                      NameList@23..24
                        Name@23..24
                          Identifier@23..24 "a"
                      Whitespace@24..25 " "
                      Colon@25..26 ":"
                      Whitespace@26..27 " "
                      PrimType@27..30
                        KwInt@27..30 "int"
                    Whitespace@30..31 " "
                    EndGroup@31..41
                      KwEnd@31..34 "end"
                      Whitespace@34..35 " "
                      KwRecord@35..41 "record""#]],
    );
}

#[test]
fn parse_record_type_many_fields() {
    check(
        r#"
    type _ : record
        a : int
        b, c, d : int
        e : int
    end record"#,
        expect![[r#"
            Source@0..89
              Whitespace@0..5 "\n    "
              StmtList@5..89
                TypeDecl@5..89
                  KwType@5..9 "type"
                  Whitespace@9..10 " "
                  Name@10..11
                    Identifier@10..11 "_"
                  Whitespace@11..12 " "
                  Colon@12..13 ":"
                  Whitespace@13..14 " "
                  RecordType@14..89
                    KwRecord@14..20 "record"
                    Whitespace@20..29 "\n        "
                    RecordField@29..36
                      NameList@29..30
                        Name@29..30
                          Identifier@29..30 "a"
                      Whitespace@30..31 " "
                      Colon@31..32 ":"
                      Whitespace@32..33 " "
                      PrimType@33..36
                        KwInt@33..36 "int"
                    Whitespace@36..45 "\n        "
                    RecordField@45..58
                      NameList@45..52
                        Name@45..46
                          Identifier@45..46 "b"
                        Comma@46..47 ","
                        Whitespace@47..48 " "
                        Name@48..49
                          Identifier@48..49 "c"
                        Comma@49..50 ","
                        Whitespace@50..51 " "
                        Name@51..52
                          Identifier@51..52 "d"
                      Whitespace@52..53 " "
                      Colon@53..54 ":"
                      Whitespace@54..55 " "
                      PrimType@55..58
                        KwInt@55..58 "int"
                    Whitespace@58..67 "\n        "
                    RecordField@67..74
                      NameList@67..68
                        Name@67..68
                          Identifier@67..68 "e"
                      Whitespace@68..69 " "
                      Colon@69..70 ":"
                      Whitespace@70..71 " "
                      PrimType@71..74
                        KwInt@71..74 "int"
                    Whitespace@74..79 "\n    "
                    EndGroup@79..89
                      KwEnd@79..82 "end"
                      Whitespace@82..83 " "
                      KwRecord@83..89 "record""#]],
    );
}

#[test]
fn parse_record_type_opt_semicolon() {
    check(
        r#"
    type _ : record
        a : int;
        b, c, d : int;;;;;;;;;;;;
        e : int;
    end record"#,
        expect![[r#"
            Source@0..103
              Whitespace@0..5 "\n    "
              StmtList@5..103
                TypeDecl@5..103
                  KwType@5..9 "type"
                  Whitespace@9..10 " "
                  Name@10..11
                    Identifier@10..11 "_"
                  Whitespace@11..12 " "
                  Colon@12..13 ":"
                  Whitespace@13..14 " "
                  RecordType@14..103
                    KwRecord@14..20 "record"
                    Whitespace@20..29 "\n        "
                    RecordField@29..37
                      NameList@29..30
                        Name@29..30
                          Identifier@29..30 "a"
                      Whitespace@30..31 " "
                      Colon@31..32 ":"
                      Whitespace@32..33 " "
                      PrimType@33..36
                        KwInt@33..36 "int"
                      Semicolon@36..37 ";"
                    Whitespace@37..46 "\n        "
                    RecordField@46..71
                      NameList@46..53
                        Name@46..47
                          Identifier@46..47 "b"
                        Comma@47..48 ","
                        Whitespace@48..49 " "
                        Name@49..50
                          Identifier@49..50 "c"
                        Comma@50..51 ","
                        Whitespace@51..52 " "
                        Name@52..53
                          Identifier@52..53 "d"
                      Whitespace@53..54 " "
                      Colon@54..55 ":"
                      Whitespace@55..56 " "
                      PrimType@56..59
                        KwInt@56..59 "int"
                      Semicolon@59..60 ";"
                      Semicolon@60..61 ";"
                      Semicolon@61..62 ";"
                      Semicolon@62..63 ";"
                      Semicolon@63..64 ";"
                      Semicolon@64..65 ";"
                      Semicolon@65..66 ";"
                      Semicolon@66..67 ";"
                      Semicolon@67..68 ";"
                      Semicolon@68..69 ";"
                      Semicolon@69..70 ";"
                      Semicolon@70..71 ";"
                    Whitespace@71..80 "\n        "
                    RecordField@80..88
                      NameList@80..81
                        Name@80..81
                          Identifier@80..81 "e"
                      Whitespace@81..82 " "
                      Colon@82..83 ":"
                      Whitespace@83..84 " "
                      PrimType@84..87
                        KwInt@84..87 "int"
                      Semicolon@87..88 ";"
                    Whitespace@88..93 "\n    "
                    EndGroup@93..103
                      KwEnd@93..96 "end"
                      Whitespace@96..97 " "
                      KwRecord@97..103 "record""#]],
    );
}

#[test]
fn parse_record_type_empty() {
    // Accepted, but semantically invalid
    check(
        "type _ : record end record",
        expect![[r#"
            Source@0..26
              StmtList@0..26
                TypeDecl@0..26
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RecordType@9..26
                    KwRecord@9..15 "record"
                    Whitespace@15..16 " "
                    EndGroup@16..26
                      KwEnd@16..19 "end"
                      Whitespace@19..20 " "
                      KwRecord@20..26 "record""#]],
    );
}

#[test]
fn recover_record_type_missing_last_name() {
    check(
        "type _ : record a, : int end record",
        expect![[r#"
            Source@0..35
              StmtList@0..35
                TypeDecl@0..35
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RecordType@9..35
                    KwRecord@9..15 "record"
                    Whitespace@15..16 " "
                    RecordField@16..24
                      NameList@16..18
                        Name@16..17
                          Identifier@16..17 "a"
                        Comma@17..18 ","
                      Whitespace@18..19 " "
                      Colon@19..20 ":"
                      Whitespace@20..21 " "
                      PrimType@21..24
                        KwInt@21..24 "int"
                    Whitespace@24..25 " "
                    EndGroup@25..35
                      KwEnd@25..28 "end"
                      Whitespace@28..29 " "
                      KwRecord@29..35 "record"
            error at 19..20: unexpected token
            | error for 19..20: expected identifier, but found `:`"#]],
    );
}

#[test]
fn recover_just_record() {
    check(
        "type _ : record",
        expect![[r#"
            Source@0..15
              StmtList@0..15
                TypeDecl@0..15
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RecordType@9..15
                    KwRecord@9..15 "record"
                    EndGroup@15..15
            error at 9..15: unexpected end of file
            | error for 9..15: expected `end` after here"#]],
    );
}

#[test]
fn parse_union_type() {
    check(
        "type _ : union : char of label 1: a : int end union",
        expect![[r#"
            Source@0..51
              StmtList@0..51
                TypeDecl@0..51
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..51
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PrimType@17..21
                      KwChar@17..21 "char"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
                    Whitespace@24..25 " "
                    UnionVariant@25..41
                      KwLabel@25..30 "label"
                      Whitespace@30..31 " "
                      ExprList@31..32
                        LiteralExpr@31..32
                          IntLiteral@31..32 "1"
                      Colon@32..33 ":"
                      Whitespace@33..34 " "
                      RecordField@34..41
                        NameList@34..35
                          Name@34..35
                            Identifier@34..35 "a"
                        Whitespace@35..36 " "
                        Colon@36..37 ":"
                        Whitespace@37..38 " "
                        PrimType@38..41
                          KwInt@38..41 "int"
                    Whitespace@41..42 " "
                    EndGroup@42..51
                      KwEnd@42..45 "end"
                      Whitespace@45..46 " "
                      KwUnion@46..51 "union""#]],
    );
}

#[test]
fn parse_union_type_packed_attr() {
    check(
        "type _ : packed union : char of label : a : int end union",
        expect![[r#"
            Source@0..57
              StmtList@0..57
                TypeDecl@0..57
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..57
                    KwPacked@9..15 "packed"
                    Whitespace@15..16 " "
                    KwUnion@16..21 "union"
                    Whitespace@21..22 " "
                    Colon@22..23 ":"
                    Whitespace@23..24 " "
                    PrimType@24..28
                      KwChar@24..28 "char"
                    Whitespace@28..29 " "
                    KwOf@29..31 "of"
                    Whitespace@31..32 " "
                    UnionVariant@32..47
                      KwLabel@32..37 "label"
                      Whitespace@37..38 " "
                      Colon@38..39 ":"
                      Whitespace@39..40 " "
                      RecordField@40..47
                        NameList@40..41
                          Name@40..41
                            Identifier@40..41 "a"
                        Whitespace@41..42 " "
                        Colon@42..43 ":"
                        Whitespace@43..44 " "
                        PrimType@44..47
                          KwInt@44..47 "int"
                    Whitespace@47..48 " "
                    EndGroup@48..57
                      KwEnd@48..51 "end"
                      Whitespace@51..52 " "
                      KwUnion@52..57 "union""#]],
    );
}

#[test]
fn recover_just_union() {
    check(
        "type _ : union",
        expect![[r#"
            Source@0..14
              StmtList@0..14
                TypeDecl@0..14
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..14
                    KwUnion@9..14 "union"
                    EndGroup@14..14
            error at 9..14: unexpected end of file
            | error for 9..14: expected identifier or `:` after here"#]],
    );
}

#[test]
fn recover_just_union_head() {
    check(
        "type _ : union : boolean of",
        expect![[r#"
            Source@0..27
              StmtList@0..27
                TypeDecl@0..27
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..27
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PrimType@17..24
                      KwBoolean@17..24 "boolean"
                    Whitespace@24..25 " "
                    KwOf@25..27 "of"
                    EndGroup@27..27
            error at 25..27: unexpected end of file
            | error for 25..27: expected `label` or `end` after here"#]],
    );
}

#[test]
fn parse_empty_union() {
    check(
        "type _ : union : char of end union",
        expect![[r#"
            Source@0..34
              StmtList@0..34
                TypeDecl@0..34
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..34
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PrimType@17..21
                      KwChar@17..21 "char"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
                    Whitespace@24..25 " "
                    EndGroup@25..34
                      KwEnd@25..28 "end"
                      Whitespace@28..29 " "
                      KwUnion@29..34 "union""#]],
    );
}

#[test]
fn union_type_many_variants() {
    check(
        "type _ : union : boolean of label 1, 2: a : int b : int label : end union",
        expect![[r#"
            Source@0..73
              StmtList@0..73
                TypeDecl@0..73
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..73
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PrimType@17..24
                      KwBoolean@17..24 "boolean"
                    Whitespace@24..25 " "
                    KwOf@25..27 "of"
                    Whitespace@27..28 " "
                    UnionVariant@28..55
                      KwLabel@28..33 "label"
                      Whitespace@33..34 " "
                      ExprList@34..38
                        LiteralExpr@34..35
                          IntLiteral@34..35 "1"
                        Comma@35..36 ","
                        Whitespace@36..37 " "
                        LiteralExpr@37..38
                          IntLiteral@37..38 "2"
                      Colon@38..39 ":"
                      Whitespace@39..40 " "
                      RecordField@40..47
                        NameList@40..41
                          Name@40..41
                            Identifier@40..41 "a"
                        Whitespace@41..42 " "
                        Colon@42..43 ":"
                        Whitespace@43..44 " "
                        PrimType@44..47
                          KwInt@44..47 "int"
                      Whitespace@47..48 " "
                      RecordField@48..55
                        NameList@48..49
                          Name@48..49
                            Identifier@48..49 "b"
                        Whitespace@49..50 " "
                        Colon@50..51 ":"
                        Whitespace@51..52 " "
                        PrimType@52..55
                          KwInt@52..55 "int"
                    Whitespace@55..56 " "
                    UnionVariant@56..63
                      KwLabel@56..61 "label"
                      Whitespace@61..62 " "
                      Colon@62..63 ":"
                    Whitespace@63..64 " "
                    EndGroup@64..73
                      KwEnd@64..67 "end"
                      Whitespace@67..68 " "
                      KwUnion@68..73 "union""#]],
    );
}

#[test]
fn union_type_default_variant() {
    check(
        "type _ : union : 1..2 of label : end union",
        expect![[r#"
            Source@0..42
              StmtList@0..42
                TypeDecl@0..42
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..42
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    RangeType@17..21
                      LiteralExpr@17..18
                        IntLiteral@17..18 "1"
                      Range@18..20 ".."
                      LiteralExpr@20..21
                        IntLiteral@20..21 "2"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
                    Whitespace@24..25 " "
                    UnionVariant@25..32
                      KwLabel@25..30 "label"
                      Whitespace@30..31 " "
                      Colon@31..32 ":"
                    Whitespace@32..33 " "
                    EndGroup@33..42
                      KwEnd@33..36 "end"
                      Whitespace@36..37 " "
                      KwUnion@37..42 "union""#]],
    );
}

#[test]
fn recover_union_type_missing_label_colon() {
    check(
        "type _ : union : 1..2 of label end union",
        expect![[r#"
            Source@0..40
              StmtList@0..40
                TypeDecl@0..40
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..40
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    RangeType@17..21
                      LiteralExpr@17..18
                        IntLiteral@17..18 "1"
                      Range@18..20 ".."
                      LiteralExpr@20..21
                        IntLiteral@20..21 "2"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
                    Whitespace@24..25 " "
                    UnionVariant@25..31
                      KwLabel@25..30 "label"
                      Whitespace@30..31 " "
                      ExprList@31..31
                    EndGroup@31..40
                      KwEnd@31..34 "end"
                      Whitespace@34..35 " "
                      KwUnion@35..40 "union"
            error at 31..34: unexpected token
            | error for 31..34: expected expression, but found `end`"#]],
    );
}

#[test]
fn recover_union_type_not_label() {
    check(
        "type _ : union : 1..2 of nope end union",
        expect![[r#"
            Source@0..39
              StmtList@0..39
                TypeDecl@0..29
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..29
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    RangeType@17..21
                      LiteralExpr@17..18
                        IntLiteral@17..18 "1"
                      Range@18..20 ".."
                      LiteralExpr@20..21
                        IntLiteral@20..21 "2"
                    Whitespace@21..22 " "
                    KwOf@22..24 "of"
                    Whitespace@24..25 " "
                    EndGroup@25..29
                      Error@25..29
                        Identifier@25..29 "nope"
                Whitespace@29..30 " "
                Error@30..33
                  KwEnd@30..33 "end"
                Whitespace@33..34 " "
                Error@34..39
                  KwUnion@34..39 "union"
            error at 25..29: unexpected token
            | error for 25..29: expected `label` or `end`, but found identifier
            error at 30..33: unexpected token
            | error for 30..33: expected `union`, but found `end`
            error at 34..39: unexpected token
            | error for 34..39: expected statement, but found `union`"#]],
    );
}

#[test]
fn union_type_parse_tag_name() {
    check(
        "type _ : union taggged : boolean of end union",
        expect![[r#"
            Source@0..45
              StmtList@0..45
                TypeDecl@0..45
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..45
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Name@15..22
                      Identifier@15..22 "taggged"
                    Whitespace@22..23 " "
                    Colon@23..24 ":"
                    Whitespace@24..25 " "
                    PrimType@25..32
                      KwBoolean@25..32 "boolean"
                    Whitespace@32..33 " "
                    KwOf@33..35 "of"
                    Whitespace@35..36 " "
                    EndGroup@36..45
                      KwEnd@36..39 "end"
                      Whitespace@39..40 " "
                      KwUnion@40..45 "union""#]],
    );
}

#[test]
fn recover_bare_union_type() {
    check(
        "type _ : union end union",
        expect![[r#"
            Source@0..24
              StmtList@0..24
                TypeDecl@0..24
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..24
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    EndGroup@15..24
                      KwEnd@15..18 "end"
                      Whitespace@18..19 " "
                      KwUnion@19..24 "union"
            error at 15..18: unexpected token
            | error for 15..18: expected identifier or `:`, but found `end`"#]],
    );
}

#[test]
fn recover_record_type_on_var() {
    check(
        "type _ : record\nvar a : int",
        expect![[r#"
            Source@0..27
              StmtList@0..27
                TypeDecl@0..16
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  RecordType@9..16
                    KwRecord@9..15 "record"
                    Whitespace@15..16 "\n"
                    RecordField@16..16
                      NameList@16..16
                    EndGroup@16..16
                ConstVarDecl@16..27
                  KwVar@16..19 "var"
                  Whitespace@19..20 " "
                  NameList@20..21
                    Name@20..21
                      Identifier@20..21 "a"
                  Whitespace@21..22 " "
                  Colon@22..23 ":"
                  Whitespace@23..24 " "
                  PrimType@24..27
                    KwInt@24..27 "int"
            error at 16..19: unexpected token
            | error for 16..19: expected `end` or identifier, but found `var`"#]],
    );
}

#[test]
fn recover_union_variant_type_on_var() {
    check(
        "type _ : union : boolean of\nlabel :\nvar a : int",
        expect![[r#"
            Source@0..47
              StmtList@0..47
                TypeDecl@0..36
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  UnionType@9..36
                    KwUnion@9..14 "union"
                    Whitespace@14..15 " "
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PrimType@17..24
                      KwBoolean@17..24 "boolean"
                    Whitespace@24..25 " "
                    KwOf@25..27 "of"
                    Whitespace@27..28 "\n"
                    UnionVariant@28..36
                      KwLabel@28..33 "label"
                      Whitespace@33..34 " "
                      Colon@34..35 ":"
                      Whitespace@35..36 "\n"
                      RecordField@36..36
                        NameList@36..36
                    EndGroup@36..36
                ConstVarDecl@36..47
                  KwVar@36..39 "var"
                  Whitespace@39..40 " "
                  NameList@40..41
                    Name@40..41
                      Identifier@40..41 "a"
                  Whitespace@41..42 " "
                  Colon@42..43 ":"
                  Whitespace@43..44 " "
                  PrimType@44..47
                    KwInt@44..47 "int"
            error at 36..39: unexpected token
            | error for 36..39: expected `end`, `label` or identifier, but found `var`"#]],
    );
}

#[test]
fn recover_packed_not_packable_type() {
    // `begin end` to show not so great error reporting
    check(
        "type _ : packed int begin end",
        expect![[r#"
            Source@0..29
              StmtList@0..29
                TypeDecl@0..19
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  Error@9..19
                    KwPacked@9..15 "packed"
                    Whitespace@15..16 " "
                    Error@16..19
                      PrimType@16..19
                        KwInt@16..19 "int"
                Whitespace@19..20 " "
                BlockStmt@20..29
                  KwBegin@20..25 "begin"
                  Whitespace@25..26 " "
                  StmtList@26..26
                  EndGroup@26..29
                    KwEnd@26..29 "end"
            error at 16..19: unexpected token
            | error for 16..19: expected `array`, `enum`, `set`, `record`, `union`, or a range type, but found `int`
            error at 20..25: unexpected token
            | error for 20..25: expected `@`, but found `begin`"#]],
    );
}

#[test]
fn recover_packed_packed_packed() {
    check(
        "type _ : packed packed packed",
        expect![[r#"
            Source@0..29
              StmtList@0..29
                TypeDecl@0..22
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  Error@9..22
                    KwPacked@9..15 "packed"
                    Whitespace@15..16 " "
                    KwPacked@16..22 "packed"
                Whitespace@22..23 " "
                Error@23..29
                  KwPacked@23..29 "packed"
            error at 16..22: unexpected token
            | error for 16..22: expected `array`, `enum`, `set`, `record`, `union`, or a range type, but found `packed`
            error at 23..29: unexpected token
            | error for 23..29: expected statement, but found `packed`"#]],
    );
}

#[test]
fn recover_just_packed() {
    check(
        "type _ : packed ",
        expect![[r#"
            Source@0..16
              StmtList@0..15
                TypeDecl@0..15
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                  Whitespace@8..9 " "
                  Error@9..15
                    KwPacked@9..15 "packed"
              Whitespace@15..16 " "
            error at 9..15: unexpected end of file
            | error for 9..15: expected `array`, `enum`, `set`, `record`, `union`, or a range type after here"#]],
    );
}

#[test]
fn recover_include_glob_ty() {
    // not allowed in ty position
    check(
        r#"type _ : include "some_foreign_location" "#,
        expect![[r#"
            Source@0..41
              StmtList@0..40
                TypeDecl@0..8
                  KwType@0..4 "type"
                  Whitespace@4..5 " "
                  Name@5..6
                    Identifier@5..6 "_"
                  Whitespace@6..7 " "
                  Colon@7..8 ":"
                Whitespace@8..9 " "
                PreprocGlob@9..40
                  PPInclude@9..40
                    KwInclude@9..16 "include"
                    Whitespace@16..17 " "
                    LiteralExpr@17..40
                      StringLiteral@17..40 "\"some_foreign_location\""
              Whitespace@40..41 " "
            error at 9..16: unexpected token
            | error for 9..16: expected type specifier, but found `include`"#]],
    )
}
