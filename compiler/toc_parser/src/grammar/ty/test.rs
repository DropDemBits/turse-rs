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
            Root@0..226
              Whitespace@0..1 "\n"
              TypeDecl@1..21
                KwType@1..5 "type"
                Whitespace@5..6 " "
                Name@6..8
                  Identifier@6..7 "_"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                PrimType@10..21
                  KwAddressint@10..20 "addressint"
                  Whitespace@20..21 "\n"
              TypeDecl@21..38
                KwType@21..25 "type"
                Whitespace@25..26 " "
                Name@26..28
                  Identifier@26..27 "_"
                  Whitespace@27..28 " "
                Colon@28..29 ":"
                Whitespace@29..30 " "
                PrimType@30..38
                  KwBoolean@30..37 "boolean"
                  Whitespace@37..38 "\n"
              TypeDecl@38..51
                KwType@38..42 "type"
                Whitespace@42..43 " "
                Name@43..45
                  Identifier@43..44 "_"
                  Whitespace@44..45 " "
                Colon@45..46 ":"
                Whitespace@46..47 " "
                PrimType@47..51
                  KwInt@47..50 "int"
                  Whitespace@50..51 "\n"
              TypeDecl@51..65
                KwType@51..55 "type"
                Whitespace@55..56 " "
                Name@56..58
                  Identifier@56..57 "_"
                  Whitespace@57..58 " "
                Colon@58..59 ":"
                Whitespace@59..60 " "
                PrimType@60..65
                  KwInt1@60..64 "int1"
                  Whitespace@64..65 "\n"
              TypeDecl@65..79
                KwType@65..69 "type"
                Whitespace@69..70 " "
                Name@70..72
                  Identifier@70..71 "_"
                  Whitespace@71..72 " "
                Colon@72..73 ":"
                Whitespace@73..74 " "
                PrimType@74..79
                  KwInt2@74..78 "int2"
                  Whitespace@78..79 "\n"
              TypeDecl@79..93
                KwType@79..83 "type"
                Whitespace@83..84 " "
                Name@84..86
                  Identifier@84..85 "_"
                  Whitespace@85..86 " "
                Colon@86..87 ":"
                Whitespace@87..88 " "
                PrimType@88..93
                  KwInt4@88..92 "int4"
                  Whitespace@92..93 "\n"
              TypeDecl@93..106
                KwType@93..97 "type"
                Whitespace@97..98 " "
                Name@98..100
                  Identifier@98..99 "_"
                  Whitespace@99..100 " "
                Colon@100..101 ":"
                Whitespace@101..102 " "
                PrimType@102..106
                  KwNat@102..105 "nat"
                  Whitespace@105..106 "\n"
              TypeDecl@106..120
                KwType@106..110 "type"
                Whitespace@110..111 " "
                Name@111..113
                  Identifier@111..112 "_"
                  Whitespace@112..113 " "
                Colon@113..114 ":"
                Whitespace@114..115 " "
                PrimType@115..120
                  KwNat1@115..119 "nat1"
                  Whitespace@119..120 "\n"
              TypeDecl@120..134
                KwType@120..124 "type"
                Whitespace@124..125 " "
                Name@125..127
                  Identifier@125..126 "_"
                  Whitespace@126..127 " "
                Colon@127..128 ":"
                Whitespace@128..129 " "
                PrimType@129..134
                  KwNat2@129..133 "nat2"
                  Whitespace@133..134 "\n"
              TypeDecl@134..148
                KwType@134..138 "type"
                Whitespace@138..139 " "
                Name@139..141
                  Identifier@139..140 "_"
                  Whitespace@140..141 " "
                Colon@141..142 ":"
                Whitespace@142..143 " "
                PrimType@143..148
                  KwNat4@143..147 "nat4"
                  Whitespace@147..148 "\n"
              TypeDecl@148..162
                KwType@148..152 "type"
                Whitespace@152..153 " "
                Name@153..155
                  Identifier@153..154 "_"
                  Whitespace@154..155 " "
                Colon@155..156 ":"
                Whitespace@156..157 " "
                PrimType@157..162
                  KwReal@157..161 "real"
                  Whitespace@161..162 "\n"
              TypeDecl@162..177
                KwType@162..166 "type"
                Whitespace@166..167 " "
                Name@167..169
                  Identifier@167..168 "_"
                  Whitespace@168..169 " "
                Colon@169..170 ":"
                Whitespace@170..171 " "
                PrimType@171..177
                  KwReal4@171..176 "real4"
                  Whitespace@176..177 "\n"
              TypeDecl@177..192
                KwType@177..181 "type"
                Whitespace@181..182 " "
                Name@182..184
                  Identifier@182..183 "_"
                  Whitespace@183..184 " "
                Colon@184..185 ":"
                Whitespace@185..186 " "
                PrimType@186..192
                  KwReal8@186..191 "real8"
                  Whitespace@191..192 "\n"
              TypeDecl@192..206
                KwType@192..196 "type"
                Whitespace@196..197 " "
                Name@197..199
                  Identifier@197..198 "_"
                  Whitespace@198..199 " "
                Colon@199..200 ":"
                Whitespace@200..201 " "
                KwChar@201..206
                  KwChar@201..205 "char"
                  Whitespace@205..206 "\n"
              TypeDecl@206..226
                KwType@206..210 "type"
                Whitespace@210..211 " "
                Name@211..213
                  Identifier@211..212 "_"
                  Whitespace@212..213 " "
                Colon@213..214 ":"
                Whitespace@214..215 " "
                KwString@215..226
                  KwString@215..221 "string"
                  Whitespace@221..226 "\n    ""#]],
    );
}

#[test]
fn parse_sized_char_type() {
    check(
        "type sc : char(1 + k)",
        expect![[r#"
            Root@0..21
              TypeDecl@0..21
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedCharType@10..21
                  KwChar@10..14 "char"
                  LeftParen@14..15 "("
                  SeqLength@15..20
                    BinaryExpr@15..20
                      LiteralExpr@15..17
                        IntLiteral@15..16 "1"
                        Whitespace@16..17 " "
                      Plus@17..18 "+"
                      Whitespace@18..19 " "
                      NameExpr@19..20
                        Name@19..20
                          Identifier@19..20 "k"
                  RightParen@20..21 ")""#]],
    );
}

#[test]
fn parse_sized_string_type() {
    check(
        "type sc : string(1 + k)",
        expect![[r#"
            Root@0..23
              TypeDecl@0..23
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedStringType@10..23
                  KwString@10..16 "string"
                  LeftParen@16..17 "("
                  SeqLength@17..22
                    BinaryExpr@17..22
                      LiteralExpr@17..19
                        IntLiteral@17..18 "1"
                        Whitespace@18..19 " "
                      Plus@19..20 "+"
                      Whitespace@20..21 " "
                      NameExpr@21..22
                        Name@21..22
                          Identifier@21..22 "k"
                  RightParen@22..23 ")""#]],
    );
}

#[test]
fn parse_dyn_sized_char_type() {
    check(
        "type _ : char(*)",
        expect![[r#"
        Root@0..16
          TypeDecl@0..16
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedCharType@9..16
              KwChar@9..13 "char"
              LeftParen@13..14 "("
              SeqLength@14..15
                Star@14..15 "*"
              RightParen@15..16 ")""#]],
    );
}

#[test]
fn parse_dyn_sized_string_type() {
    check(
        "type _ : string(*)",
        expect![[r#"
        Root@0..18
          TypeDecl@0..18
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
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
            Root@0..16
              TypeDecl@0..16
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedCharType@10..16
                  KwChar@10..14 "char"
                  LeftParen@14..15 "("
                  SeqLength@15..15
                  RightParen@15..16 ")"
            error at 15..16: expected expression, but found ’)’"#]],
    );
}

#[test]
fn recover_empty_sized_string_type() {
    check(
        "type sc : string()",
        expect![[r#"
            Root@0..18
              TypeDecl@0..18
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedStringType@10..18
                  KwString@10..16 "string"
                  LeftParen@16..17 "("
                  SeqLength@17..17
                  RightParen@17..18 ")"
            error at 17..18: expected expression, but found ’)’"#]],
    );
}

#[test]
fn recover_not_expr_in_sized_char_type() {
    check(
        "type sc : char(to)",
        expect![[r#"
            Root@0..18
              TypeDecl@0..18
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedCharType@10..18
                  KwChar@10..14 "char"
                  LeftParen@14..15 "("
                  SeqLength@15..17
                    Error@15..17
                      KwTo@15..17 "to"
                  RightParen@17..18 ")"
            error at 15..17: expected expression, but found ’to’"#]],
    );
}

#[test]
fn recover_not_expr_in_sized_string_type() {
    check(
        "type sc : string(to)",
        expect![[r#"
            Root@0..20
              TypeDecl@0..20
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedStringType@10..20
                  KwString@10..16 "string"
                  LeftParen@16..17 "("
                  SeqLength@17..19
                    Error@17..19
                      KwTo@17..19 "to"
                  RightParen@19..20 ")"
            error at 17..19: expected expression, but found ’to’"#]],
    );
}

#[test]
fn recover_missing_right_paren_in_sized_char_type() {
    check(
        "type sc : char(1",
        expect![[r#"
            Root@0..16
              TypeDecl@0..16
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedCharType@10..16
                  KwChar@10..14 "char"
                  LeftParen@14..15 "("
                  SeqLength@15..16
                    LiteralExpr@15..16
                      IntLiteral@15..16 "1"
            error at 15..16: expected ’)’"#]],
    );
}

#[test]
fn recover_missing_right_paren_in_sized_string_type() {
    check(
        "type sc : string(1",
        expect![[r#"
            Root@0..18
              TypeDecl@0..18
                KwType@0..4 "type"
                Whitespace@4..5 " "
                Name@5..8
                  Identifier@5..7 "sc"
                  Whitespace@7..8 " "
                Colon@8..9 ":"
                Whitespace@9..10 " "
                SizedStringType@10..18
                  KwString@10..16 "string"
                  LeftParen@16..17 "("
                  SeqLength@17..18
                    LiteralExpr@17..18
                      IntLiteral@17..18 "1"
            error at 17..18: expected ’)’"#]],
    );
}

#[test]
fn parse_name_type() {
    check(
        "type _ : a",
        expect![[r#"
        Root@0..10
          TypeDecl@0..10
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            NameType@9..10
              NameExpr@9..10
                Name@9..10
                  Identifier@9..10 "a""#]],
    );
    check(
        "type _ : a.b.c",
        expect![[r#"
        Root@0..14
          TypeDecl@0..14
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            NameType@9..14
              FieldExpr@9..14
                FieldExpr@9..12
                  NameExpr@9..10
                    Name@9..10
                      Identifier@9..10 "a"
                  Dot@10..11 "."
                  Name@11..12
                    Identifier@11..12 "b"
                Dot@12..13 "."
                Name@13..14
                  Identifier@13..14 "c""#]],
    );
}

#[test]
fn parse_name_type_not_a_ref() {
    // primaries still included because of range expr expecting a general expr
    check(
        "type _ : 1",
        expect![[r#"
        Root@0..10
          TypeDecl@0..10
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..22
          TypeDecl@0..22
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..22
          TypeDecl@0..22
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            NameType@9..22
              BinaryExpr@9..22
                BinaryExpr@9..19
                  BinaryExpr@9..15
                    LiteralExpr@9..11
                      IntLiteral@9..10 "1"
                      Whitespace@10..11 " "
                    Plus@11..12 "+"
                    Whitespace@12..13 " "
                    LiteralExpr@13..15
                      IntLiteral@13..14 "2"
                      Whitespace@14..15 " "
                  Plus@15..16 "+"
                  Whitespace@16..17 " "
                  LiteralExpr@17..19
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
        Root@0..15
          TypeDecl@0..15
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            RangeType@9..15
              LiteralExpr@9..11
                IntLiteral@9..10 "1"
                Whitespace@10..11 " "
              Range@11..13 ".."
              Whitespace@13..14 " "
              LiteralExpr@14..15
                IntLiteral@14..15 "2""#]],
    );
}

#[test]
fn parse_unbounded_range_type() {
    check(
        "type _ : 1 .. *",
        expect![[r#"
        Root@0..15
          TypeDecl@0..15
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            RangeType@9..15
              LiteralExpr@9..11
                IntLiteral@9..10 "1"
                Whitespace@10..11 " "
              Range@11..13 ".."
              Whitespace@13..14 " "
              Star@14..15 "*""#]],
    );
}

#[test]
fn recover_range_type_missing_tail() {
    check(
        "type _ : 1 ..",
        expect![[r#"
        Root@0..13
          TypeDecl@0..13
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            RangeType@9..13
              LiteralExpr@9..11
                IntLiteral@9..10 "1"
                Whitespace@10..11 " "
              Range@11..13 ".."
        error at 11..13: expected expression"#]],
    );
}

#[test]
fn recover_range_type_not_an_expr() {
    check(
        "type _ : 1 .. boolean",
        expect![[r#"
        Root@0..21
          TypeDecl@0..21
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            RangeType@9..21
              LiteralExpr@9..11
                IntLiteral@9..10 "1"
                Whitespace@10..11 " "
              Range@11..13 ".."
              Whitespace@13..14 " "
              Error@14..21
                PrimType@14..21
                  KwBoolean@14..21 "boolean"
        error at 14..21: expected ’@’
        error at 14..21: expected expression"#]],
    );
}

#[test]
fn parse_pointer_type() {
    check(
        "type _ : pointer to int",
        expect![[r#"
        Root@0..23
          TypeDecl@0..23
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..33
          TypeDecl@0..33
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
                      Name@20..24
                        Identifier@20..24 "some"
                    Dot@24..25 "."
                    Name@25..30
                      Identifier@25..30 "named"
                  Dot@30..31 "."
                  Name@31..33
                    Identifier@31..33 "ty""#]],
    );
}

#[test]
fn parse_unchecked_pointer_type() {
    check(
        "type _ : unchecked pointer to addressint",
        expect![[r#"
        Root@0..40
          TypeDecl@0..40
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            PointerType@9..40
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
fn parse_short_pointer_type() {
    check(
        "type _ : ^int",
        expect![[r#"
        Root@0..13
          TypeDecl@0..13
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..27
          TypeDecl@0..27
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            PointerType@9..27
              KwPointer@9..16 "pointer"
              Whitespace@16..17 " "
              Error@17..27
                KwAddressint@17..27 "addressint"
        error at 17..27: expected ’to’, but found ’addressint’
        error at 17..27: expected type specifier"#]],
    );
}

#[test]
fn parse_enum_type() {
    check(
        "type _ : enum (a)",
        expect![[r#"
        Root@0..17
          TypeDecl@0..17
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
fn parse_enum_type_multiple_names() {
    check(
        "type _ : enum (a, b, c)",
        expect![[r#"
        Root@0..23
          TypeDecl@0..23
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..22
          TypeDecl@0..21
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            EnumType@9..21
              KwEnum@9..13 "enum"
              Whitespace@13..14 " "
              LeftParen@14..15 "("
              NameList@15..20
                Name@15..16
                  Identifier@15..16 "a"
                Comma@16..17 ","
                Whitespace@17..18 " "
                Name@18..20
                  Identifier@18..19 "b"
                  Whitespace@19..20 " "
              Error@20..21
                Identifier@20..21 "c"
          Error@21..22
            RightParen@21..22 ")"
        error at 20..21: expected ’,’ or ’)’, but found identifier
        error at 21..22: expected statement, but found ’)’"#]],
    )
}

#[test]
fn recover_enum_type_missing_name() {
    check(
        "type _ : enum (a, b, )",
        expect![[r#"
        Root@0..22
          TypeDecl@0..22
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
                Name@18..19
                  Identifier@18..19 "b"
                Comma@19..20 ","
                Whitespace@20..21 " "
              RightParen@21..22 ")"
        error at 21..22: expected identifier, but found ’)’"#]],
    )
}

#[test]
fn recover_enum_type_missing_name_and_right_paren() {
    check(
        "type _ : enum (a, b,",
        expect![[r#"
        Root@0..20
          TypeDecl@0..20
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        error at 19..20: expected identifier
        error at 19..20: expected ’,’ or ’)’"#]],
    )
}

#[test]
fn recover_enum_type_missing_right_paren() {
    check(
        "type _ : enum (a, b",
        expect![[r#"
        Root@0..19
          TypeDecl@0..19
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        error at 18..19: expected ’,’ or ’)’"#]],
    )
}

#[test]
fn recover_enum_type_missing_names() {
    check(
        "type _ : enum ()",
        expect![[r#"
        Root@0..16
          TypeDecl@0..16
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        error at 15..16: expected identifier, but found ’)’"#]],
    )
}

#[test]
fn parse_set_type() {
    check(
        "type _ : set of boolean",
        expect![[r#"
        Root@0..23
          TypeDecl@0..23
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
fn parse_set_type_not_index_type() {
    // is still valid, checked in AST validation
    check(
        "type _ : set of int",
        expect![[r#"
        Root@0..19
          TypeDecl@0..19
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..22
          TypeDecl@0..22
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
                LiteralExpr@16..18
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
        Root@0..22
          TypeDecl@0..22
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
                LiteralExpr@16..18
                  IntLiteral@16..17 "1"
                  Whitespace@17..18 " "
                Range@18..20 ".."
                Whitespace@20..21 " "
                Star@21..22 "*""#]],
    );
}

#[test]
fn recover_set_type_missing_of() {
    // is still valid, checked in AST validation
    check(
        "type _ : set char",
        expect![[r#"
        Root@0..17
          TypeDecl@0..17
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SetType@9..17
              KwSet@9..12 "set"
              Whitespace@12..13 " "
              Error@13..17
                KwChar@13..17 "char"
        error at 13..17: expected ’of’, but found ’char’
        error at 13..17: expected type specifier"#]],
    );
}

#[test]
fn recover_set_type_missing_ty() {
    // is still valid, checked in AST validation
    check(
        "type _ : set of",
        expect![[r#"
        Root@0..15
          TypeDecl@0..15
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SetType@9..15
              KwSet@9..12 "set"
              Whitespace@12..13 " "
              KwOf@13..15 "of"
        error at 13..15: expected type specifier"#]],
    );
}

#[test]
fn recover_just_set() {
    // is still valid, checked in AST validation
    check(
        "type _ : set",
        expect![[r#"
        Root@0..12
          TypeDecl@0..12
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SetType@9..12
              KwSet@9..12 "set"
        error at 9..12: expected ’of’
        error at 9..12: expected type specifier"#]],
    );
}

#[test]
fn parse_condition_type() {
    check(
        "type _ : condition",
        expect![[r#"
        Root@0..18
          TypeDecl@0..18
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        Root@0..27
          TypeDecl@0..27
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ConditionType@9..27
              ConditionKind@9..18
                KwPriority@9..17 "priority"
                Whitespace@17..18 " "
              KwCondition@18..27 "condition""#]],
    );
    check(
        "type _ : deferred condition",
        expect![[r#"
        Root@0..27
          TypeDecl@0..27
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ConditionType@9..27
              ConditionKind@9..18
                KwDeferred@9..17 "deferred"
                Whitespace@17..18 " "
              KwCondition@18..27 "condition""#]],
    );
    check(
        "type _ : timeout condition",
        expect![[r#"
        Root@0..26
          TypeDecl@0..26
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ConditionType@9..26
              ConditionKind@9..17
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
        Root@0..31
          TypeDecl@0..31
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            ConditionType@9..31
              ConditionKind@9..18
                KwPriority@9..17 "priority"
                Whitespace@17..18 " "
              Error@18..31
                Identifier@18..31 "not_condition"
        error at 18..31: expected ’condition’, but found identifier"#]],
    );
}

#[test]
fn parse_collection_type() {
    check(
        "type _ : collection of int",
        expect![[r#"
        Root@0..26
          TypeDecl@0..26
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
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
        "type _ : collection of forward",
        expect![[r#"
        Root@0..30
          TypeDecl@0..30
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            CollectionType@9..30
              KwCollection@9..19 "collection"
              Whitespace@19..20 " "
              KwOf@20..22 "of"
              Whitespace@22..23 " "
              KwForward@23..30 "forward""#]],
    )
}

#[test]
fn recover_collection_type_no_ty() {
    check(
        "type _ : collection of",
        expect![[r#"
        Root@0..22
          TypeDecl@0..22
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            CollectionType@9..22
              KwCollection@9..19 "collection"
              Whitespace@19..20 " "
              KwOf@20..22 "of"
        error at 20..22: expected type specifier"#]],
    )
}

#[test]
fn recover_collection_type_no_of() {
    check(
        "type _ : collection int",
        expect![[r#"
        Root@0..23
          TypeDecl@0..23
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            CollectionType@9..23
              KwCollection@9..19 "collection"
              Whitespace@19..20 " "
              Error@20..23
                KwInt@20..23 "int"
        error at 20..23: expected ’of’, but found ’int’
        error at 20..23: expected type specifier"#]],
    )
}

#[test]
fn recover_just_collection() {
    check(
        "type _ : collection",
        expect![[r#"
        Root@0..19
          TypeDecl@0..19
            KwType@0..4 "type"
            Whitespace@4..5 " "
            Name@5..7
              Identifier@5..6 "_"
              Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            CollectionType@9..19
              KwCollection@9..19 "collection"
        error at 9..19: expected ’of’
        error at 9..19: expected type specifier"#]],
    )
}
