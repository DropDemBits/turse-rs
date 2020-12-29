//! Tests for types
use crate::check;
use expect_test::expect;

#[test]
fn parse_basic_prim_types() {
    check(
        r#"
var _ : addressint := 1
var _ : boolean := 1
var _ : int := 1
var _ : int1 := 1
var _ : int2 := 1
var _ : int4 := 1
var _ : nat := 1
var _ : nat1 := 1
var _ : nat2 := 1
var _ : nat4 := 1
var _ : real := 1
var _ : real4 := 1
var _ : real8 := 1
var _ : char := 1
var _ : string := 1
    "#,
        expect![[r#"
            Root@0..286
              Whitespace@0..1 "\n"
              ConstVarDecl@1..25
                KwVar@1..4 "var"
                Whitespace@4..5 " "
                Identifier@5..6 "_"
                Whitespace@6..7 " "
                Colon@7..8 ":"
                Whitespace@8..9 " "
                PrimType@9..20
                  KwAddressint@9..19 "addressint"
                  Whitespace@19..20 " "
                Assign@20..22 ":="
                Whitespace@22..23 " "
                LiteralExpr@23..25
                  IntLiteral@23..24 "1"
                  Whitespace@24..25 "\n"
              ConstVarDecl@25..46
                KwVar@25..28 "var"
                Whitespace@28..29 " "
                Identifier@29..30 "_"
                Whitespace@30..31 " "
                Colon@31..32 ":"
                Whitespace@32..33 " "
                PrimType@33..41
                  KwBoolean@33..40 "boolean"
                  Whitespace@40..41 " "
                Assign@41..43 ":="
                Whitespace@43..44 " "
                LiteralExpr@44..46
                  IntLiteral@44..45 "1"
                  Whitespace@45..46 "\n"
              ConstVarDecl@46..63
                KwVar@46..49 "var"
                Whitespace@49..50 " "
                Identifier@50..51 "_"
                Whitespace@51..52 " "
                Colon@52..53 ":"
                Whitespace@53..54 " "
                PrimType@54..58
                  KwInt@54..57 "int"
                  Whitespace@57..58 " "
                Assign@58..60 ":="
                Whitespace@60..61 " "
                LiteralExpr@61..63
                  IntLiteral@61..62 "1"
                  Whitespace@62..63 "\n"
              ConstVarDecl@63..81
                KwVar@63..66 "var"
                Whitespace@66..67 " "
                Identifier@67..68 "_"
                Whitespace@68..69 " "
                Colon@69..70 ":"
                Whitespace@70..71 " "
                PrimType@71..76
                  KwInt1@71..75 "int1"
                  Whitespace@75..76 " "
                Assign@76..78 ":="
                Whitespace@78..79 " "
                LiteralExpr@79..81
                  IntLiteral@79..80 "1"
                  Whitespace@80..81 "\n"
              ConstVarDecl@81..99
                KwVar@81..84 "var"
                Whitespace@84..85 " "
                Identifier@85..86 "_"
                Whitespace@86..87 " "
                Colon@87..88 ":"
                Whitespace@88..89 " "
                PrimType@89..94
                  KwInt2@89..93 "int2"
                  Whitespace@93..94 " "
                Assign@94..96 ":="
                Whitespace@96..97 " "
                LiteralExpr@97..99
                  IntLiteral@97..98 "1"
                  Whitespace@98..99 "\n"
              ConstVarDecl@99..117
                KwVar@99..102 "var"
                Whitespace@102..103 " "
                Identifier@103..104 "_"
                Whitespace@104..105 " "
                Colon@105..106 ":"
                Whitespace@106..107 " "
                PrimType@107..112
                  KwInt4@107..111 "int4"
                  Whitespace@111..112 " "
                Assign@112..114 ":="
                Whitespace@114..115 " "
                LiteralExpr@115..117
                  IntLiteral@115..116 "1"
                  Whitespace@116..117 "\n"
              ConstVarDecl@117..134
                KwVar@117..120 "var"
                Whitespace@120..121 " "
                Identifier@121..122 "_"
                Whitespace@122..123 " "
                Colon@123..124 ":"
                Whitespace@124..125 " "
                PrimType@125..129
                  KwNat@125..128 "nat"
                  Whitespace@128..129 " "
                Assign@129..131 ":="
                Whitespace@131..132 " "
                LiteralExpr@132..134
                  IntLiteral@132..133 "1"
                  Whitespace@133..134 "\n"
              ConstVarDecl@134..152
                KwVar@134..137 "var"
                Whitespace@137..138 " "
                Identifier@138..139 "_"
                Whitespace@139..140 " "
                Colon@140..141 ":"
                Whitespace@141..142 " "
                PrimType@142..147
                  KwNat1@142..146 "nat1"
                  Whitespace@146..147 " "
                Assign@147..149 ":="
                Whitespace@149..150 " "
                LiteralExpr@150..152
                  IntLiteral@150..151 "1"
                  Whitespace@151..152 "\n"
              ConstVarDecl@152..170
                KwVar@152..155 "var"
                Whitespace@155..156 " "
                Identifier@156..157 "_"
                Whitespace@157..158 " "
                Colon@158..159 ":"
                Whitespace@159..160 " "
                PrimType@160..165
                  KwNat2@160..164 "nat2"
                  Whitespace@164..165 " "
                Assign@165..167 ":="
                Whitespace@167..168 " "
                LiteralExpr@168..170
                  IntLiteral@168..169 "1"
                  Whitespace@169..170 "\n"
              ConstVarDecl@170..188
                KwVar@170..173 "var"
                Whitespace@173..174 " "
                Identifier@174..175 "_"
                Whitespace@175..176 " "
                Colon@176..177 ":"
                Whitespace@177..178 " "
                PrimType@178..183
                  KwNat4@178..182 "nat4"
                  Whitespace@182..183 " "
                Assign@183..185 ":="
                Whitespace@185..186 " "
                LiteralExpr@186..188
                  IntLiteral@186..187 "1"
                  Whitespace@187..188 "\n"
              ConstVarDecl@188..206
                KwVar@188..191 "var"
                Whitespace@191..192 " "
                Identifier@192..193 "_"
                Whitespace@193..194 " "
                Colon@194..195 ":"
                Whitespace@195..196 " "
                PrimType@196..201
                  KwReal@196..200 "real"
                  Whitespace@200..201 " "
                Assign@201..203 ":="
                Whitespace@203..204 " "
                LiteralExpr@204..206
                  IntLiteral@204..205 "1"
                  Whitespace@205..206 "\n"
              ConstVarDecl@206..225
                KwVar@206..209 "var"
                Whitespace@209..210 " "
                Identifier@210..211 "_"
                Whitespace@211..212 " "
                Colon@212..213 ":"
                Whitespace@213..214 " "
                PrimType@214..220
                  KwReal4@214..219 "real4"
                  Whitespace@219..220 " "
                Assign@220..222 ":="
                Whitespace@222..223 " "
                LiteralExpr@223..225
                  IntLiteral@223..224 "1"
                  Whitespace@224..225 "\n"
              ConstVarDecl@225..244
                KwVar@225..228 "var"
                Whitespace@228..229 " "
                Identifier@229..230 "_"
                Whitespace@230..231 " "
                Colon@231..232 ":"
                Whitespace@232..233 " "
                PrimType@233..239
                  KwReal8@233..238 "real8"
                  Whitespace@238..239 " "
                Assign@239..241 ":="
                Whitespace@241..242 " "
                LiteralExpr@242..244
                  IntLiteral@242..243 "1"
                  Whitespace@243..244 "\n"
              ConstVarDecl@244..262
                KwVar@244..247 "var"
                Whitespace@247..248 " "
                Identifier@248..249 "_"
                Whitespace@249..250 " "
                Colon@250..251 ":"
                Whitespace@251..252 " "
                KwChar@252..257
                  KwChar@252..256 "char"
                  Whitespace@256..257 " "
                Assign@257..259 ":="
                Whitespace@259..260 " "
                LiteralExpr@260..262
                  IntLiteral@260..261 "1"
                  Whitespace@261..262 "\n"
              ConstVarDecl@262..286
                KwVar@262..265 "var"
                Whitespace@265..266 " "
                Identifier@266..267 "_"
                Whitespace@267..268 " "
                Colon@268..269 ":"
                Whitespace@269..270 " "
                KwString@270..277
                  KwString@270..276 "string"
                  Whitespace@276..277 " "
                Assign@277..279 ":="
                Whitespace@279..280 " "
                LiteralExpr@280..286
                  IntLiteral@280..281 "1"
                  Whitespace@281..286 "\n    ""#]],
    );
}

#[test]
fn parse_sized_char_type() {
    check("var sc : char(1 + k) := 1", expect![[r#"
        Root@0..25
          ConstVarDecl@0..25
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedCharType@9..21
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
              RightParen@19..20 ")"
              Whitespace@20..21 " "
            Assign@21..23 ":="
            Whitespace@23..24 " "
            LiteralExpr@24..25
              IntLiteral@24..25 "1""#]]);
}

#[test]
fn parse_sized_string_type() {
    check("var sc : string(1 + k) := 1", expect![[r#"
        Root@0..27
          ConstVarDecl@0..27
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedStringType@9..23
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
              RightParen@21..22 ")"
              Whitespace@22..23 " "
            Assign@23..25 ":="
            Whitespace@25..26 " "
            LiteralExpr@26..27
              IntLiteral@26..27 "1""#]]);
}

#[test]
fn recover_empty_sized_char_type() {
    check("var sc : char() := 1", expect![[r##"
        Root@0..20
          ConstVarDecl@0..20
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedCharType@9..16
              KwChar@9..13 "char"
              LeftParen@13..14 "("
              RightParen@14..15 ")"
              Whitespace@15..16 " "
            Assign@16..18 ":="
            Whitespace@18..19 " "
            LiteralExpr@19..20
              IntLiteral@19..20 "1"
        error at 14..15: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’"##]]);
}

#[test]
fn recover_empty_sized_string_type() {
    check("var sc : string() := 1", expect![[r##"
        Root@0..22
          ConstVarDecl@0..22
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedStringType@9..18
              KwString@9..15 "string"
              LeftParen@15..16 "("
              RightParen@16..17 ")"
              Whitespace@17..18 " "
            Assign@18..20 ":="
            Whitespace@20..21 " "
            LiteralExpr@21..22
              IntLiteral@21..22 "1"
        error at 16..17: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’)’"##]]);
}

#[test]
fn recover_not_expr_in_sized_char_type() {
    check("var sc : char(to) := 1", expect![[r##"
        Root@0..22
          ConstVarDecl@0..22
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedCharType@9..18
              KwChar@9..13 "char"
              LeftParen@13..14 "("
              Error@14..16
                KwTo@14..16 "to"
              RightParen@16..17 ")"
              Whitespace@17..18 " "
            Assign@18..20 ":="
            Whitespace@20..21 " "
            LiteralExpr@21..22
              IntLiteral@21..22 "1"
        error at 14..16: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’to’"##]]);
}

#[test]
fn recover_not_expr_in_sized_string_type() {
    check("var sc : string(to) := 1", expect![[r##"
        Root@0..24
          ConstVarDecl@0..24
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedStringType@9..20
              KwString@9..15 "string"
              LeftParen@15..16 "("
              Error@16..18
                KwTo@16..18 "to"
              RightParen@18..19 ")"
              Whitespace@19..20 " "
            Assign@20..22 ":="
            Whitespace@22..23 " "
            LiteralExpr@23..24
              IntLiteral@23..24 "1"
        error at 16..18: expected identifier, ’^’, ’bits’, int literal, explicit int literal, real literal, ’(’, ’not’, ’+’, ’-’ or ’#’, but found ’to’"##]]);
}

#[test]
fn recover_missing_right_paren_in_sized_char_type() {
    check("var sc : char(1 := 1", expect![[r#"
        Root@0..20
          ConstVarDecl@0..20
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedCharType@9..16
              KwChar@9..13 "char"
              LeftParen@13..14 "("
              LiteralExpr@14..16
                IntLiteral@14..15 "1"
                Whitespace@15..16 " "
            Assign@16..18 ":="
            Whitespace@18..19 " "
            LiteralExpr@19..20
              IntLiteral@19..20 "1"
        error at 16..18: expected ’)’, but found ’:=’"#]]);
}

#[test]
fn recover_missing_right_paren_in_sized_string_type() {
    check("var sc : string(1 := 1", expect![[r#"
        Root@0..22
          ConstVarDecl@0..22
            KwVar@0..3 "var"
            Whitespace@3..4 " "
            Identifier@4..6 "sc"
            Whitespace@6..7 " "
            Colon@7..8 ":"
            Whitespace@8..9 " "
            SizedStringType@9..18
              KwString@9..15 "string"
              LeftParen@15..16 "("
              LiteralExpr@16..18
                IntLiteral@16..17 "1"
                Whitespace@17..18 " "
            Assign@18..20 ":="
            Whitespace@20..21 " "
            LiteralExpr@21..22
              IntLiteral@21..22 "1"
        error at 18..20: expected ’)’, but found ’:=’"#]]);
}
