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
    check("type _ : char(*)", expect![[r#"
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
              RightParen@15..16 ")""#]]);
}

#[test]
fn parse_dyn_sized_string_type() {
    check("type _ : string(*)", expect![[r#"
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
              RightParen@17..18 ")""#]]);
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
