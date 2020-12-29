//! Tests for types
use crate::check;
use expect_test::expect;

#[test]
fn parse_basic_prim_types() {
    check(
        r#"
var _ : addressint
var _ : boolean
var _ : int
var _ : int1
var _ : int2
var _ : int4
var _ : nat
var _ : nat1
var _ : nat2
var _ : nat4
var _ : real
var _ : real4
var _ : real8
var _ : char
var _ : string
    "#,
        expect![[r#"
            Root@0..211
              Whitespace@0..1 "\n"
              ConstVarDecl@1..20
                KwVar@1..4 "var"
                Whitespace@4..5 " "
                NameList@5..7
                  Name@5..7
                    Identifier@5..6 "_"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                PrimType@9..20
                  KwAddressint@9..19 "addressint"
                  Whitespace@19..20 "\n"
              ConstVarDecl@20..36
                KwVar@20..23 "var"
                Whitespace@23..24 " "
                NameList@24..26
                  Name@24..26
                    Identifier@24..25 "_"
                    Whitespace@25..26 " "
                Colon@26..27 ":"
                Whitespace@27..28 " "
                PrimType@28..36
                  KwBoolean@28..35 "boolean"
                  Whitespace@35..36 "\n"
              ConstVarDecl@36..48
                KwVar@36..39 "var"
                Whitespace@39..40 " "
                NameList@40..42
                  Name@40..42
                    Identifier@40..41 "_"
                    Whitespace@41..42 " "
                Colon@42..43 ":"
                Whitespace@43..44 " "
                PrimType@44..48
                  KwInt@44..47 "int"
                  Whitespace@47..48 "\n"
              ConstVarDecl@48..61
                KwVar@48..51 "var"
                Whitespace@51..52 " "
                NameList@52..54
                  Name@52..54
                    Identifier@52..53 "_"
                    Whitespace@53..54 " "
                Colon@54..55 ":"
                Whitespace@55..56 " "
                PrimType@56..61
                  KwInt1@56..60 "int1"
                  Whitespace@60..61 "\n"
              ConstVarDecl@61..74
                KwVar@61..64 "var"
                Whitespace@64..65 " "
                NameList@65..67
                  Name@65..67
                    Identifier@65..66 "_"
                    Whitespace@66..67 " "
                Colon@67..68 ":"
                Whitespace@68..69 " "
                PrimType@69..74
                  KwInt2@69..73 "int2"
                  Whitespace@73..74 "\n"
              ConstVarDecl@74..87
                KwVar@74..77 "var"
                Whitespace@77..78 " "
                NameList@78..80
                  Name@78..80
                    Identifier@78..79 "_"
                    Whitespace@79..80 " "
                Colon@80..81 ":"
                Whitespace@81..82 " "
                PrimType@82..87
                  KwInt4@82..86 "int4"
                  Whitespace@86..87 "\n"
              ConstVarDecl@87..99
                KwVar@87..90 "var"
                Whitespace@90..91 " "
                NameList@91..93
                  Name@91..93
                    Identifier@91..92 "_"
                    Whitespace@92..93 " "
                Colon@93..94 ":"
                Whitespace@94..95 " "
                PrimType@95..99
                  KwNat@95..98 "nat"
                  Whitespace@98..99 "\n"
              ConstVarDecl@99..112
                KwVar@99..102 "var"
                Whitespace@102..103 " "
                NameList@103..105
                  Name@103..105
                    Identifier@103..104 "_"
                    Whitespace@104..105 " "
                Colon@105..106 ":"
                Whitespace@106..107 " "
                PrimType@107..112
                  KwNat1@107..111 "nat1"
                  Whitespace@111..112 "\n"
              ConstVarDecl@112..125
                KwVar@112..115 "var"
                Whitespace@115..116 " "
                NameList@116..118
                  Name@116..118
                    Identifier@116..117 "_"
                    Whitespace@117..118 " "
                Colon@118..119 ":"
                Whitespace@119..120 " "
                PrimType@120..125
                  KwNat2@120..124 "nat2"
                  Whitespace@124..125 "\n"
              ConstVarDecl@125..138
                KwVar@125..128 "var"
                Whitespace@128..129 " "
                NameList@129..131
                  Name@129..131
                    Identifier@129..130 "_"
                    Whitespace@130..131 " "
                Colon@131..132 ":"
                Whitespace@132..133 " "
                PrimType@133..138
                  KwNat4@133..137 "nat4"
                  Whitespace@137..138 "\n"
              ConstVarDecl@138..151
                KwVar@138..141 "var"
                Whitespace@141..142 " "
                NameList@142..144
                  Name@142..144
                    Identifier@142..143 "_"
                    Whitespace@143..144 " "
                Colon@144..145 ":"
                Whitespace@145..146 " "
                PrimType@146..151
                  KwReal@146..150 "real"
                  Whitespace@150..151 "\n"
              ConstVarDecl@151..165
                KwVar@151..154 "var"
                Whitespace@154..155 " "
                NameList@155..157
                  Name@155..157
                    Identifier@155..156 "_"
                    Whitespace@156..157 " "
                Colon@157..158 ":"
                Whitespace@158..159 " "
                PrimType@159..165
                  KwReal4@159..164 "real4"
                  Whitespace@164..165 "\n"
              ConstVarDecl@165..179
                KwVar@165..168 "var"
                Whitespace@168..169 " "
                NameList@169..171
                  Name@169..171
                    Identifier@169..170 "_"
                    Whitespace@170..171 " "
                Colon@171..172 ":"
                Whitespace@172..173 " "
                PrimType@173..179
                  KwReal8@173..178 "real8"
                  Whitespace@178..179 "\n"
              ConstVarDecl@179..192
                KwVar@179..182 "var"
                Whitespace@182..183 " "
                NameList@183..185
                  Name@183..185
                    Identifier@183..184 "_"
                    Whitespace@184..185 " "
                Colon@185..186 ":"
                Whitespace@186..187 " "
                KwChar@187..192
                  KwChar@187..191 "char"
                  Whitespace@191..192 "\n"
              ConstVarDecl@192..211
                KwVar@192..195 "var"
                Whitespace@195..196 " "
                NameList@196..198
                  Name@196..198
                    Identifier@196..197 "_"
                    Whitespace@197..198 " "
                Colon@198..199 ":"
                Whitespace@199..200 " "
                KwString@200..211
                  KwString@200..206 "string"
                  Whitespace@206..211 "\n    ""#]],
    );
}

#[test]
fn parse_sized_char_type() {
    check(
        "var sc : char(1 + k)",
        expect![[r#"
            Root@0..20
              ConstVarDecl@0..20
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedCharType@9..20
                  KwChar@9..13 "char"
                  LeftParen@13..14 "("
                  BinaryExpr@14..19
                    LiteralExpr@14..16
                      IntLiteral@14..15 "1"
                      Whitespace@15..16 " "
                    Plus@16..17 "+"
                    Whitespace@17..18 " "
                    NameExpr@18..19
                      Name@18..19
                        Identifier@18..19 "k"
                  RightParen@19..20 ")""#]],
    );
}

#[test]
fn parse_sized_string_type() {
    check(
        "var sc : string(1 + k)",
        expect![[r#"
            Root@0..22
              ConstVarDecl@0..22
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedStringType@9..22
                  KwString@9..15 "string"
                  LeftParen@15..16 "("
                  BinaryExpr@16..21
                    LiteralExpr@16..18
                      IntLiteral@16..17 "1"
                      Whitespace@17..18 " "
                    Plus@18..19 "+"
                    Whitespace@19..20 " "
                    NameExpr@20..21
                      Name@20..21
                        Identifier@20..21 "k"
                  RightParen@21..22 ")""#]],
    );
}

#[test]
fn recover_empty_sized_char_type() {
    check(
        "var sc : char()",
        expect![[r##"
            Root@0..15
              ConstVarDecl@0..15
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedCharType@9..15
                  KwChar@9..13 "char"
                  LeftParen@13..14 "("
                  RightParen@14..15 ")"
            error at 14..15: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’"##]],
    );
}

#[test]
fn recover_empty_sized_string_type() {
    check(
        "var sc : string()",
        expect![[r##"
            Root@0..17
              ConstVarDecl@0..17
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedStringType@9..17
                  KwString@9..15 "string"
                  LeftParen@15..16 "("
                  RightParen@16..17 ")"
            error at 16..17: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’"##]],
    );
}

#[test]
fn recover_not_expr_in_sized_char_type() {
    check(
        "var sc : char(to)",
        expect![[r##"
            Root@0..17
              ConstVarDecl@0..17
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedCharType@9..17
                  KwChar@9..13 "char"
                  LeftParen@13..14 "("
                  Error@14..16
                    KwTo@14..16 "to"
                  RightParen@16..17 ")"
            error at 14..16: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’to’"##]],
    );
}

#[test]
fn recover_not_expr_in_sized_string_type() {
    check(
        "var sc : string(to)",
        expect![[r##"
            Root@0..19
              ConstVarDecl@0..19
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedStringType@9..19
                  KwString@9..15 "string"
                  LeftParen@15..16 "("
                  Error@16..18
                    KwTo@16..18 "to"
                  RightParen@18..19 ")"
            error at 16..18: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’to’"##]],
    );
}

#[test]
fn recover_missing_right_paren_in_sized_char_type() {
    check(
        "var sc : char(1",
        expect![[r#"
            Root@0..15
              ConstVarDecl@0..15
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedCharType@9..15
                  KwChar@9..13 "char"
                  LeftParen@13..14 "("
                  LiteralExpr@14..15
                    IntLiteral@14..15 "1"
            error at 14..15: expected ’)’"#]],
    );
}

#[test]
fn recover_missing_right_paren_in_sized_string_type() {
    check(
        "var sc : string(1",
        expect![[r#"
            Root@0..17
              ConstVarDecl@0..17
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..7
                  Name@4..7
                    Identifier@4..6 "sc"
                    Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                SizedStringType@9..17
                  KwString@9..15 "string"
                  LeftParen@15..16 "("
                  LiteralExpr@16..17
                    IntLiteral@16..17 "1"
            error at 16..17: expected ’)’"#]],
    );
}
