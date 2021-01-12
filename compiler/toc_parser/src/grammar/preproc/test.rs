use expect_test::expect;

use crate::check;

/// Testing conditionals
mod cond {
    use super::*;

    #[test]
    fn parse_if_preproc() {
        check(
            r##"
    #if A then
    assert true
    begin end
    #end if
    "##,
            expect![[r##"
                Source@0..62
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..62
                    PPIf@5..62
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      PPNameExpr@9..11
                        Name@9..11
                          Identifier@9..10 "A"
                          Whitespace@10..11 " "
                      KwThen@11..15 "then"
                      Whitespace@15..20 "\n    "
                      PPTokenBody@20..50
                        AssertStmt@20..36
                          KwAssert@20..26 "assert"
                          Whitespace@26..27 " "
                          LiteralExpr@27..36
                            KwTrue@27..31 "true"
                            Whitespace@31..36 "\n    "
                        BlockStmt@36..50
                          KwBegin@36..41 "begin"
                          Whitespace@41..42 " "
                          StmtList@42..42
                          EndGroup@42..50
                            KwEnd@42..45 "end"
                            Whitespace@45..50 "\n    "
                      PPEndIf@50..62
                        PPKwEnd@50..54 "#end"
                        Whitespace@54..55 " "
                        KwIf@55..57 "if"
                        Whitespace@57..62 "\n    ""##]],
        );
    }

    #[test]
    fn recover_if_preproc_missing_expr() {
        check(
            r##"
    #if then
    assert true
    begin end
    #end if
    "##,
            expect![[r##"
                Source@0..60
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..60
                    PPIf@5..60
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      KwThen@9..13 "then"
                      Whitespace@13..18 "\n    "
                      PPTokenBody@18..48
                        AssertStmt@18..34
                          KwAssert@18..24 "assert"
                          Whitespace@24..25 " "
                          LiteralExpr@25..34
                            KwTrue@25..29 "true"
                            Whitespace@29..34 "\n    "
                        BlockStmt@34..48
                          KwBegin@34..39 "begin"
                          Whitespace@39..40 " "
                          StmtList@40..40
                          EndGroup@40..48
                            KwEnd@40..43 "end"
                            Whitespace@43..48 "\n    "
                      PPEndIf@48..60
                        PPKwEnd@48..52 "#end"
                        Whitespace@52..53 " "
                        KwIf@53..55 "if"
                        Whitespace@55..60 "\n    "
                error at 9..13: expected preprocessor condition, but found ‘then’"##]],
        );
    }

    #[test]
    fn recover_if_preproc_missing_then() {
        check(
            r##"
    #if A
    assert true
    begin end
    #end if
    "##,
            expect![[r##"
                Source@0..57
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..57
                    PPIf@5..57
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      PPNameExpr@9..15
                        Name@9..15
                          Identifier@9..10 "A"
                          Whitespace@10..15 "\n    "
                      PPTokenBody@15..45
                        AssertStmt@15..31
                          KwAssert@15..21 "assert"
                          Whitespace@21..22 " "
                          LiteralExpr@22..31
                            KwTrue@22..26 "true"
                            Whitespace@26..31 "\n    "
                        BlockStmt@31..45
                          KwBegin@31..36 "begin"
                          Whitespace@36..37 " "
                          StmtList@37..37
                          EndGroup@37..45
                            KwEnd@37..40 "end"
                            Whitespace@40..45 "\n    "
                      PPEndIf@45..57
                        PPKwEnd@45..49 "#end"
                        Whitespace@49..50 " "
                        KwIf@50..52 "if"
                        Whitespace@52..57 "\n    "
                error at 15..21: expected ‘then’, but found ‘assert’"##]],
        );
    }

    #[test]
    fn recover_if_preproc_missing_tail() {
        check(
            r##"
    #if A then
    assert true
    begin end
    "##,
            expect![[r##"
                Source@0..50
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..50
                    PPIf@5..50
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      PPNameExpr@9..11
                        Name@9..11
                          Identifier@9..10 "A"
                          Whitespace@10..11 " "
                      KwThen@11..15 "then"
                      Whitespace@15..20 "\n    "
                      PPTokenBody@20..50
                        AssertStmt@20..36
                          KwAssert@20..26 "assert"
                          Whitespace@26..27 " "
                          LiteralExpr@27..36
                            KwTrue@27..31 "true"
                            Whitespace@31..36 "\n    "
                        BlockStmt@36..50
                          KwBegin@36..41 "begin"
                          Whitespace@41..42 " "
                          StmtList@42..42
                          EndGroup@42..50
                            KwEnd@42..45 "end"
                            Whitespace@45..50 "\n    "
                error at 45..50: expected ‘#elseif’, ‘#elsif’, ‘#else’, ‘#end’ or ‘#endif’"##]],
        );
    }

    #[test]
    fn parse_if_elsif_preproc() {
        check(
            r##"
    #if A then
    assert true
    #elsif B then
    assert false
    begin end
    #end if
    "##,
            expect![[r##"
                Source@0..97
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..97
                    PPIf@5..97
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      PPNameExpr@9..11
                        Name@9..11
                          Identifier@9..10 "A"
                          Whitespace@10..11 " "
                      KwThen@11..15 "then"
                      Whitespace@15..20 "\n    "
                      PPTokenBody@20..36
                        AssertStmt@20..36
                          KwAssert@20..26 "assert"
                          Whitespace@26..27 " "
                          LiteralExpr@27..36
                            KwTrue@27..31 "true"
                            Whitespace@31..36 "\n    "
                      PPElseif@36..85
                        PPKwElsif@36..42 "#elsif"
                        Whitespace@42..43 " "
                        PPNameExpr@43..45
                          Name@43..45
                            Identifier@43..44 "B"
                            Whitespace@44..45 " "
                        KwThen@45..49 "then"
                        Whitespace@49..54 "\n    "
                        PPTokenBody@54..85
                          AssertStmt@54..71
                            KwAssert@54..60 "assert"
                            Whitespace@60..61 " "
                            LiteralExpr@61..71
                              KwFalse@61..66 "false"
                              Whitespace@66..71 "\n    "
                          BlockStmt@71..85
                            KwBegin@71..76 "begin"
                            Whitespace@76..77 " "
                            StmtList@77..77
                            EndGroup@77..85
                              KwEnd@77..80 "end"
                              Whitespace@80..85 "\n    "
                      PPEndIf@85..97
                        PPKwEnd@85..89 "#end"
                        Whitespace@89..90 " "
                        KwIf@90..92 "if"
                        Whitespace@92..97 "\n    ""##]],
        );
    }

    #[test]
    fn parse_if_elsif_elseif_preproc() {
        check(
            r##"
    #if A then
    assert true
    #elsif B then
    assert false
    begin end
    #elseif C then
    put "hello"
    #end if
    "##,
            expect![[r##"
                Source@0..132
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..132
                    PPIf@5..132
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      PPNameExpr@9..11
                        Name@9..11
                          Identifier@9..10 "A"
                          Whitespace@10..11 " "
                      KwThen@11..15 "then"
                      Whitespace@15..20 "\n    "
                      PPTokenBody@20..36
                        AssertStmt@20..36
                          KwAssert@20..26 "assert"
                          Whitespace@26..27 " "
                          LiteralExpr@27..36
                            KwTrue@27..31 "true"
                            Whitespace@31..36 "\n    "
                      PPElseif@36..120
                        PPKwElsif@36..42 "#elsif"
                        Whitespace@42..43 " "
                        PPNameExpr@43..45
                          Name@43..45
                            Identifier@43..44 "B"
                            Whitespace@44..45 " "
                        KwThen@45..49 "then"
                        Whitespace@49..54 "\n    "
                        PPTokenBody@54..85
                          AssertStmt@54..71
                            KwAssert@54..60 "assert"
                            Whitespace@60..61 " "
                            LiteralExpr@61..71
                              KwFalse@61..66 "false"
                              Whitespace@66..71 "\n    "
                          BlockStmt@71..85
                            KwBegin@71..76 "begin"
                            Whitespace@76..77 " "
                            StmtList@77..77
                            EndGroup@77..85
                              KwEnd@77..80 "end"
                              Whitespace@80..85 "\n    "
                        PPElseif@85..120
                          PPKwElseif@85..92 "#elseif"
                          Whitespace@92..93 " "
                          PPNameExpr@93..95
                            Name@93..95
                              Identifier@93..94 "C"
                              Whitespace@94..95 " "
                          KwThen@95..99 "then"
                          Whitespace@99..104 "\n    "
                          PPTokenBody@104..120
                            PutStmt@104..120
                              KwPut@104..107 "put"
                              Whitespace@107..108 " "
                              PutItem@108..120
                                LiteralExpr@108..120
                                  StringLiteral@108..115 "\"hello\""
                                  Whitespace@115..120 "\n    "
                      PPEndIf@120..132
                        PPKwEnd@120..124 "#end"
                        Whitespace@124..125 " "
                        KwIf@125..127 "if"
                        Whitespace@127..132 "\n    "
                warn at 85..92: ‘#elseif’ found, assuming it to be ‘#elsif’"##]],
        );
    }

    #[test]
    fn parse_if_elsif_elseif_else_preproc() {
        check(
            r##"
    #if A then
    assert true
    #elsif B then
    assert false
    begin end
    #elseif C then
    put "hello"
    #else
    put "actually exec'd"..
    #end if
    "##,
            expect![[r##"
                Source@0..170
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..170
                    PPIf@5..170
                      PPKwIf@5..8 "#if"
                      Whitespace@8..9 " "
                      PPNameExpr@9..11
                        Name@9..11
                          Identifier@9..10 "A"
                          Whitespace@10..11 " "
                      KwThen@11..15 "then"
                      Whitespace@15..20 "\n    "
                      PPTokenBody@20..36
                        AssertStmt@20..36
                          KwAssert@20..26 "assert"
                          Whitespace@26..27 " "
                          LiteralExpr@27..36
                            KwTrue@27..31 "true"
                            Whitespace@31..36 "\n    "
                      PPElseif@36..158
                        PPKwElsif@36..42 "#elsif"
                        Whitespace@42..43 " "
                        PPNameExpr@43..45
                          Name@43..45
                            Identifier@43..44 "B"
                            Whitespace@44..45 " "
                        KwThen@45..49 "then"
                        Whitespace@49..54 "\n    "
                        PPTokenBody@54..85
                          AssertStmt@54..71
                            KwAssert@54..60 "assert"
                            Whitespace@60..61 " "
                            LiteralExpr@61..71
                              KwFalse@61..66 "false"
                              Whitespace@66..71 "\n    "
                          BlockStmt@71..85
                            KwBegin@71..76 "begin"
                            Whitespace@76..77 " "
                            StmtList@77..77
                            EndGroup@77..85
                              KwEnd@77..80 "end"
                              Whitespace@80..85 "\n    "
                        PPElseif@85..158
                          PPKwElseif@85..92 "#elseif"
                          Whitespace@92..93 " "
                          PPNameExpr@93..95
                            Name@93..95
                              Identifier@93..94 "C"
                              Whitespace@94..95 " "
                          KwThen@95..99 "then"
                          Whitespace@99..104 "\n    "
                          PPTokenBody@104..120
                            PutStmt@104..120
                              KwPut@104..107 "put"
                              Whitespace@107..108 " "
                              PutItem@108..120
                                LiteralExpr@108..120
                                  StringLiteral@108..115 "\"hello\""
                                  Whitespace@115..120 "\n    "
                          PPElse@120..158
                            PPKwElse@120..125 "#else"
                            Whitespace@125..130 "\n    "
                            PPTokenBody@130..158
                              PutStmt@130..158
                                KwPut@130..133 "put"
                                Whitespace@133..134 " "
                                PutItem@134..151
                                  LiteralExpr@134..151
                                    StringLiteral@134..151 "\"actually exec\'d\""
                                Range@151..153 ".."
                                Whitespace@153..158 "\n    "
                      PPEndIf@158..170
                        PPKwEnd@158..162 "#end"
                        Whitespace@162..163 " "
                        KwIf@163..165 "if"
                        Whitespace@165..170 "\n    "
                warn at 85..92: ‘#elseif’ found, assuming it to be ‘#elsif’"##]],
        );
    }

    #[test]
    fn parse_only_elsif_preproc() {
        // reject in validation
        check(
            r##"
    #elsif A then
    pause e
    #end if
    "##,
            expect![[r##"
                Source@0..47
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..47
                    PPElseif@5..47
                      PPKwElsif@5..11 "#elsif"
                      Whitespace@11..12 " "
                      PPNameExpr@12..14
                        Name@12..14
                          Identifier@12..13 "A"
                          Whitespace@13..14 " "
                      KwThen@14..18 "then"
                      Whitespace@18..23 "\n    "
                      PPTokenBody@23..35
                        PauseStmt@23..35
                          KwPause@23..28 "pause"
                          Whitespace@28..29 " "
                          NameExpr@29..35
                            Name@29..35
                              Identifier@29..30 "e"
                              Whitespace@30..35 "\n    "
                      PPEndIf@35..47
                        PPKwEnd@35..39 "#end"
                        Whitespace@39..40 " "
                        KwIf@40..42 "if"
                        Whitespace@42..47 "\n    ""##]],
        );
    }

    #[test]
    fn parse_only_elseif_preproc() {
        // reject in validation
        check(
            r##"
    #elseif A then
    pause e
    #end if
    "##,
            expect![[r##"
                Source@0..48
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..48
                    PPElseif@5..48
                      PPKwElseif@5..12 "#elseif"
                      Whitespace@12..13 " "
                      PPNameExpr@13..15
                        Name@13..15
                          Identifier@13..14 "A"
                          Whitespace@14..15 " "
                      KwThen@15..19 "then"
                      Whitespace@19..24 "\n    "
                      PPTokenBody@24..36
                        PauseStmt@24..36
                          KwPause@24..29 "pause"
                          Whitespace@29..30 " "
                          NameExpr@30..36
                            Name@30..36
                              Identifier@30..31 "e"
                              Whitespace@31..36 "\n    "
                      PPEndIf@36..48
                        PPKwEnd@36..40 "#end"
                        Whitespace@40..41 " "
                        KwIf@41..43 "if"
                        Whitespace@43..48 "\n    "
                warn at 5..12: ‘#elseif’ found, assuming it to be ‘#elsif’"##]],
        );
    }

    #[test]
    fn parse_only_else_preproc() {
        // reject in validation
        check(
            r##"
    #else
    pause e
    #end if
    "##,
            expect![[r##"
                Source@0..39
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..39
                    PPElse@5..39
                      PPKwElse@5..10 "#else"
                      Whitespace@10..15 "\n    "
                      PPTokenBody@15..27
                        PauseStmt@15..27
                          KwPause@15..20 "pause"
                          Whitespace@20..21 " "
                          NameExpr@21..27
                            Name@21..27
                              Identifier@21..22 "e"
                              Whitespace@22..27 "\n    "
                      PPEndIf@27..39
                        PPKwEnd@27..31 "#end"
                        Whitespace@31..32 " "
                        KwIf@32..34 "if"
                        Whitespace@34..39 "\n    ""##]],
        );
    }

    #[test]
    fn parse_only_end_if_preproc() {
        // reject in validation
        check(
            r##"
    #end if
    "##,
            expect![[r##"
                Source@0..17
                  Whitespace@0..5 "\n    "
                  PreprocGlob@5..17
                    PPEndIf@5..17
                      PPKwEnd@5..9 "#end"
                      Whitespace@9..10 " "
                      KwIf@10..12 "if"
                      Whitespace@12..17 "\n    ""##]],
        );
    }

    #[test]
    fn parse_only_endif_preproc() {
        check(
            "#endif",
            expect![[r##"
                Source@0..6
                  PreprocGlob@0..6
                    PPEndIf@0..6
                      PPKwEndIf@0..6 "#endif"
                warn at 0..6: ‘#endif’ found, assuming it to be ‘#end if’"##]],
        );
    }

    #[test]
    fn recover_just_if_preproc() {
        check(
            "#if",
            expect![[r##"
                Source@0..3
                  PreprocGlob@0..3
                    PPIf@0..3
                      PPKwIf@0..3 "#if"
                      PPTokenBody@3..3
                error at 0..3: expected preprocessor condition
                error at 0..3: expected ‘then’
                error at 0..3: expected ‘#elseif’, ‘#elsif’, ‘#else’, ‘#end’ or ‘#endif’"##]],
        );
    }

    #[test]
    fn recover_just_elsif_preproc() {
        check(
            "#elsif",
            expect![[r##"
                Source@0..6
                  PreprocGlob@0..6
                    PPElseif@0..6
                      PPKwElsif@0..6 "#elsif"
                      PPTokenBody@6..6
                error at 0..6: expected preprocessor condition
                error at 0..6: expected ‘then’
                error at 0..6: expected ‘#elseif’, ‘#elsif’, ‘#else’, ‘#end’ or ‘#endif’"##]],
        );
    }

    #[test]
    fn recover_just_elseif_preproc() {
        check(
            "#elseif",
            expect![[r##"
                Source@0..7
                  PreprocGlob@0..7
                    PPElseif@0..7
                      PPKwElseif@0..7 "#elseif"
                      PPTokenBody@7..7
                warn at 0..7: ‘#elseif’ found, assuming it to be ‘#elsif’
                error at 0..7: expected preprocessor condition
                error at 0..7: expected ‘then’
                error at 0..7: expected ‘#elseif’, ‘#elsif’, ‘#else’, ‘#end’ or ‘#endif’"##]],
        );
    }

    #[test]
    fn recover_just_else_preproc() {
        check(
            "#else",
            expect![[r##"
                Source@0..5
                  PreprocGlob@0..5
                    PPElse@0..5
                      PPKwElse@0..5 "#else"
                      PPTokenBody@5..5
                error at 0..5: expected ‘#end’ or ‘#endif’"##]],
        );
    }

    #[test]
    fn recover_just_end_preproc() {
        check(
            "#end",
            expect![[r##"
                Source@0..4
                  PreprocGlob@0..4
                    PPEndIf@0..4
                      PPKwEnd@0..4 "#end"
                error at 0..4: expected ‘if’"##]],
        );
    }

    #[test]
    fn parse_just_endif_preproc() {
        check(
            "#endif",
            expect![[r##"
                Source@0..6
                  PreprocGlob@0..6
                    PPEndIf@0..6
                      PPKwEndIf@0..6 "#endif"
                warn at 0..6: ‘#endif’ found, assuming it to be ‘#end if’"##]],
        );
    }

    #[test]
    fn recover_on_if_preproc() {
        // reject in validation
        check(
            r##"
    var i :=
    #if A then
    pause e
    #end if
    "##,
            expect![[r##"
                Source@0..57
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
                  PreprocGlob@18..57
                    PPIf@18..57
                      PPKwIf@18..21 "#if"
                      Whitespace@21..22 " "
                      PPNameExpr@22..24
                        Name@22..24
                          Identifier@22..23 "A"
                          Whitespace@23..24 " "
                      KwThen@24..28 "then"
                      Whitespace@28..33 "\n    "
                      PPTokenBody@33..45
                        PauseStmt@33..45
                          KwPause@33..38 "pause"
                          Whitespace@38..39 " "
                          NameExpr@39..45
                            Name@39..45
                              Identifier@39..40 "e"
                              Whitespace@40..45 "\n    "
                      PPEndIf@45..57
                        PPKwEnd@45..49 "#end"
                        Whitespace@49..50 " "
                        KwIf@50..52 "if"
                        Whitespace@52..57 "\n    "
                error at 18..21: expected expression, but found ‘#if’"##]],
        );
    }

    #[test]
    fn recover_on_elsif_preproc() {
        // reject in validation
        check(
            r##"
    var i :=
    #elsif A then
    pause e
    #end if
    "##,
            expect![[r##"
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
                  PreprocGlob@18..60
                    PPElseif@18..60
                      PPKwElsif@18..24 "#elsif"
                      Whitespace@24..25 " "
                      PPNameExpr@25..27
                        Name@25..27
                          Identifier@25..26 "A"
                          Whitespace@26..27 " "
                      KwThen@27..31 "then"
                      Whitespace@31..36 "\n    "
                      PPTokenBody@36..48
                        PauseStmt@36..48
                          KwPause@36..41 "pause"
                          Whitespace@41..42 " "
                          NameExpr@42..48
                            Name@42..48
                              Identifier@42..43 "e"
                              Whitespace@43..48 "\n    "
                      PPEndIf@48..60
                        PPKwEnd@48..52 "#end"
                        Whitespace@52..53 " "
                        KwIf@53..55 "if"
                        Whitespace@55..60 "\n    "
                error at 18..24: expected expression, but found ‘#elsif’"##]],
        );
    }

    #[test]
    fn recover_on_elseif_preproc() {
        // reject in validation
        check(
            r##"
    var i :=
    #elseif A then
    pause e
    #end if
    "##,
            expect![[r##"
                Source@0..61
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
                  PreprocGlob@18..61
                    PPElseif@18..61
                      PPKwElseif@18..25 "#elseif"
                      Whitespace@25..26 " "
                      PPNameExpr@26..28
                        Name@26..28
                          Identifier@26..27 "A"
                          Whitespace@27..28 " "
                      KwThen@28..32 "then"
                      Whitespace@32..37 "\n    "
                      PPTokenBody@37..49
                        PauseStmt@37..49
                          KwPause@37..42 "pause"
                          Whitespace@42..43 " "
                          NameExpr@43..49
                            Name@43..49
                              Identifier@43..44 "e"
                              Whitespace@44..49 "\n    "
                      PPEndIf@49..61
                        PPKwEnd@49..53 "#end"
                        Whitespace@53..54 " "
                        KwIf@54..56 "if"
                        Whitespace@56..61 "\n    "
                error at 18..25: expected expression, but found ‘#elseif’
                warn at 18..25: ‘#elseif’ found, assuming it to be ‘#elsif’"##]],
        );
    }

    #[test]
    fn recover_on_else_preproc() {
        // reject in validation
        check(
            r##"
    var i :=
    #else
    pause e
    #end if
    "##,
            expect![[r##"
                Source@0..52
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
                  PreprocGlob@18..52
                    PPElse@18..52
                      PPKwElse@18..23 "#else"
                      Whitespace@23..28 "\n    "
                      PPTokenBody@28..40
                        PauseStmt@28..40
                          KwPause@28..33 "pause"
                          Whitespace@33..34 " "
                          NameExpr@34..40
                            Name@34..40
                              Identifier@34..35 "e"
                              Whitespace@35..40 "\n    "
                      PPEndIf@40..52
                        PPKwEnd@40..44 "#end"
                        Whitespace@44..45 " "
                        KwIf@45..47 "if"
                        Whitespace@47..52 "\n    "
                error at 18..23: expected expression, but found ‘#else’"##]],
        );
    }

    #[test]
    fn recover_on_end_if_preproc() {
        // reject in validation
        check(
            r##"
    var i :=
    #end if
    "##,
            expect![[r##"
                Source@0..30
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
                  PreprocGlob@18..30
                    PPEndIf@18..30
                      PPKwEnd@18..22 "#end"
                      Whitespace@22..23 " "
                      KwIf@23..25 "if"
                      Whitespace@25..30 "\n    "
                error at 18..22: expected expression, but found ‘#end’"##]],
        );
    }

    #[test]
    fn recover_on_endif_preproc() {
        check(
            r##"
    var i :=
    #endif"##,
            expect![[r##"
                Source@0..24
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
                  PreprocGlob@18..24
                    PPEndIf@18..24
                      PPKwEndIf@18..24 "#endif"
                error at 18..24: expected expression, but found ‘#endif’
                warn at 18..24: ‘#endif’ found, assuming it to be ‘#end if’"##]],
        );
    }
}

/// Testing exprs
mod expr {
    use super::*;

    #[test]
    fn parse_unary_not() {
        check(
            "#if not A then #end if",
            expect![[r##"
                Source@0..22
                  PreprocGlob@0..22
                    PPIf@0..22
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPUnaryExpr@4..10
                        KwNot@4..7 "not"
                        Whitespace@7..8 " "
                        PPNameExpr@8..10
                          Name@8..10
                            Identifier@8..9 "A"
                            Whitespace@9..10 " "
                      KwThen@10..14 "then"
                      Whitespace@14..15 " "
                      PPTokenBody@15..15
                      PPEndIf@15..22
                        PPKwEnd@15..19 "#end"
                        Whitespace@19..20 " "
                        KwIf@20..22 "if""##]],
        )
    }

    #[test]
    fn parse_unary_not_alt() {
        check(
            "#if ~ A then #end if",
            expect![[r##"
                Source@0..20
                  PreprocGlob@0..20
                    PPIf@0..20
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPUnaryExpr@4..8
                        Tilde@4..5 "~"
                        Whitespace@5..6 " "
                        PPNameExpr@6..8
                          Name@6..8
                            Identifier@6..7 "A"
                            Whitespace@7..8 " "
                      KwThen@8..12 "then"
                      Whitespace@12..13 " "
                      PPTokenBody@13..13
                      PPEndIf@13..20
                        PPKwEnd@13..17 "#end"
                        Whitespace@17..18 " "
                        KwIf@18..20 "if""##]],
        )
    }

    #[test]
    fn parse_chained_not() {
        check(
            "#if ~ not ~ not A then #end if",
            expect![[r##"
                Source@0..30
                  PreprocGlob@0..30
                    PPIf@0..30
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPUnaryExpr@4..18
                        Tilde@4..5 "~"
                        Whitespace@5..6 " "
                        PPUnaryExpr@6..18
                          KwNot@6..9 "not"
                          Whitespace@9..10 " "
                          PPUnaryExpr@10..18
                            Tilde@10..11 "~"
                            Whitespace@11..12 " "
                            PPUnaryExpr@12..18
                              KwNot@12..15 "not"
                              Whitespace@15..16 " "
                              PPNameExpr@16..18
                                Name@16..18
                                  Identifier@16..17 "A"
                                  Whitespace@17..18 " "
                      KwThen@18..22 "then"
                      Whitespace@22..23 " "
                      PPTokenBody@23..23
                      PPEndIf@23..30
                        PPKwEnd@23..27 "#end"
                        Whitespace@27..28 " "
                        KwIf@28..30 "if""##]],
        )
    }

    #[test]
    fn parse_binary_and() {
        check(
            "#if A and B then #end if",
            expect![[r##"
                Source@0..24
                  PreprocGlob@0..24
                    PPIf@0..24
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPBinaryExpr@4..12
                        PPNameExpr@4..6
                          Name@4..6
                            Identifier@4..5 "A"
                            Whitespace@5..6 " "
                        KwAnd@6..9 "and"
                        Whitespace@9..10 " "
                        PPNameExpr@10..12
                          Name@10..12
                            Identifier@10..11 "B"
                            Whitespace@11..12 " "
                      KwThen@12..16 "then"
                      Whitespace@16..17 " "
                      PPTokenBody@17..17
                      PPEndIf@17..24
                        PPKwEnd@17..21 "#end"
                        Whitespace@21..22 " "
                        KwIf@22..24 "if""##]],
        )
    }

    #[test]
    fn parse_binary_and_alt() {
        check(
            "#if A & B then #end if",
            expect![[r##"
                Source@0..22
                  PreprocGlob@0..22
                    PPIf@0..22
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPBinaryExpr@4..10
                        PPNameExpr@4..6
                          Name@4..6
                            Identifier@4..5 "A"
                            Whitespace@5..6 " "
                        Ampersand@6..7 "&"
                        Whitespace@7..8 " "
                        PPNameExpr@8..10
                          Name@8..10
                            Identifier@8..9 "B"
                            Whitespace@9..10 " "
                      KwThen@10..14 "then"
                      Whitespace@14..15 " "
                      PPTokenBody@15..15
                      PPEndIf@15..22
                        PPKwEnd@15..19 "#end"
                        Whitespace@19..20 " "
                        KwIf@20..22 "if""##]],
        )
    }

    #[test]
    fn parse_binary_or() {
        check(
            "#if A or B then #end if",
            expect![[r##"
                Source@0..23
                  PreprocGlob@0..23
                    PPIf@0..23
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPBinaryExpr@4..11
                        PPNameExpr@4..6
                          Name@4..6
                            Identifier@4..5 "A"
                            Whitespace@5..6 " "
                        KwOr@6..8 "or"
                        Whitespace@8..9 " "
                        PPNameExpr@9..11
                          Name@9..11
                            Identifier@9..10 "B"
                            Whitespace@10..11 " "
                      KwThen@11..15 "then"
                      Whitespace@15..16 " "
                      PPTokenBody@16..16
                      PPEndIf@16..23
                        PPKwEnd@16..20 "#end"
                        Whitespace@20..21 " "
                        KwIf@21..23 "if""##]],
        )
    }

    #[test]
    fn parse_binary_or_alt() {
        check(
            "#if A | B then #end if",
            expect![[r##"
                Source@0..22
                  PreprocGlob@0..22
                    PPIf@0..22
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPBinaryExpr@4..10
                        PPNameExpr@4..6
                          Name@4..6
                            Identifier@4..5 "A"
                            Whitespace@5..6 " "
                        Pipe@6..7 "|"
                        Whitespace@7..8 " "
                        PPNameExpr@8..10
                          Name@8..10
                            Identifier@8..9 "B"
                            Whitespace@9..10 " "
                      KwThen@10..14 "then"
                      Whitespace@14..15 " "
                      PPTokenBody@15..15
                      PPEndIf@15..22
                        PPKwEnd@15..19 "#end"
                        Whitespace@19..20 " "
                        KwIf@20..22 "if""##]],
        )
    }

    #[test]
    fn parse_binary_precedence_and_higher_than_or() {
        check(
            "#if A | B and C | D then #end if",
            expect![[r##"
                Source@0..32
                  PreprocGlob@0..32
                    PPIf@0..32
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPBinaryExpr@4..20
                        PPBinaryExpr@4..16
                          PPNameExpr@4..6
                            Name@4..6
                              Identifier@4..5 "A"
                              Whitespace@5..6 " "
                          Pipe@6..7 "|"
                          Whitespace@7..8 " "
                          PPBinaryExpr@8..16
                            PPNameExpr@8..10
                              Name@8..10
                                Identifier@8..9 "B"
                                Whitespace@9..10 " "
                            KwAnd@10..13 "and"
                            Whitespace@13..14 " "
                            PPNameExpr@14..16
                              Name@14..16
                                Identifier@14..15 "C"
                                Whitespace@15..16 " "
                        Pipe@16..17 "|"
                        Whitespace@17..18 " "
                        PPNameExpr@18..20
                          Name@18..20
                            Identifier@18..19 "D"
                            Whitespace@19..20 " "
                      KwThen@20..24 "then"
                      Whitespace@24..25 " "
                      PPTokenBody@25..25
                      PPEndIf@25..32
                        PPKwEnd@25..29 "#end"
                        Whitespace@29..30 " "
                        KwIf@30..32 "if""##]],
        )
    }

    #[test]
    fn recover_unary_missing_rhs() {
        check(
            "#if not then #end if",
            expect![[r##"
                Source@0..20
                  PreprocGlob@0..20
                    PPIf@0..20
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPUnaryExpr@4..8
                        KwNot@4..7 "not"
                        Whitespace@7..8 " "
                      KwThen@8..12 "then"
                      Whitespace@12..13 " "
                      PPTokenBody@13..13
                      PPEndIf@13..20
                        PPKwEnd@13..17 "#end"
                        Whitespace@17..18 " "
                        KwIf@18..20 "if"
                error at 8..12: expected preprocessor condition, but found ‘then’"##]],
        )
    }

    #[test]
    fn recover_binary_missing_rhs() {
        check(
            "#if A and then #end if",
            expect![[r##"
                Source@0..22
                  PreprocGlob@0..22
                    PPIf@0..22
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPBinaryExpr@4..10
                        PPNameExpr@4..6
                          Name@4..6
                            Identifier@4..5 "A"
                            Whitespace@5..6 " "
                        KwAnd@6..9 "and"
                        Whitespace@9..10 " "
                      KwThen@10..14 "then"
                      Whitespace@14..15 " "
                      PPTokenBody@15..15
                      PPEndIf@15..22
                        PPKwEnd@15..19 "#end"
                        Whitespace@19..20 " "
                        KwIf@20..22 "if"
                error at 10..14: expected preprocessor condition, but found ‘then’"##]],
        )
    }

    #[test]
    fn recover_prefix_as_infix() {
        check(
            "#if A not B then #end if",
            expect![[r##"
                Source@0..24
                  PreprocGlob@0..24
                    PPIf@0..24
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPNameExpr@4..6
                        Name@4..6
                          Identifier@4..5 "A"
                          Whitespace@5..6 " "
                      Error@6..10
                        KwNot@6..9 "not"
                        Whitespace@9..10 " "
                      PPTokenBody@10..17
                        CallStmt@10..12
                          NameExpr@10..12
                            Name@10..12
                              Identifier@10..11 "B"
                              Whitespace@11..12 " "
                        Error@12..17
                          KwThen@12..16 "then"
                          Whitespace@16..17 " "
                      PPEndIf@17..24
                        PPKwEnd@17..21 "#end"
                        Whitespace@21..22 " "
                        KwIf@22..24 "if"
                error at 6..9: expected ‘then’, but found ‘not’
                error at 12..16: expected statement, but found ‘then’"##]],
        )
    }

    #[test]
    fn recover_infix_as_prefix() {
        check(
            "#if and B then #end if",
            expect![[r##"
                Source@0..22
                  PreprocGlob@0..22
                    PPIf@0..22
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      Error@4..8
                        KwAnd@4..7 "and"
                        Whitespace@7..8 " "
                      Error@8..10
                        Identifier@8..9 "B"
                        Whitespace@9..10 " "
                      PPTokenBody@10..15
                        Error@10..15
                          KwThen@10..14 "then"
                          Whitespace@14..15 " "
                      PPEndIf@15..22
                        PPKwEnd@15..19 "#end"
                        Whitespace@19..20 " "
                        KwIf@20..22 "if"
                error at 4..7: expected preprocessor condition, but found ‘and’
                error at 8..9: expected ‘then’, but found identifier
                error at 10..14: expected statement, but found ‘then’"##]],
        )
    }

    #[test]
    fn parse_parens_preproc() {
        check(
            "#if (A and B) or C then #end if",
            expect![[r##"
            Source@0..31
              PreprocGlob@0..31
                PPIf@0..31
                  PPKwIf@0..3 "#if"
                  Whitespace@3..4 " "
                  PPBinaryExpr@4..19
                    PPParenExpr@4..14
                      LeftParen@4..5 "("
                      PPBinaryExpr@5..12
                        PPNameExpr@5..7
                          Name@5..7
                            Identifier@5..6 "A"
                            Whitespace@6..7 " "
                        KwAnd@7..10 "and"
                        Whitespace@10..11 " "
                        PPNameExpr@11..12
                          Name@11..12
                            Identifier@11..12 "B"
                      RightParen@12..13 ")"
                      Whitespace@13..14 " "
                    KwOr@14..16 "or"
                    Whitespace@16..17 " "
                    PPNameExpr@17..19
                      Name@17..19
                        Identifier@17..18 "C"
                        Whitespace@18..19 " "
                  KwThen@19..23 "then"
                  Whitespace@23..24 " "
                  PPTokenBody@24..24
                  PPEndIf@24..31
                    PPKwEnd@24..28 "#end"
                    Whitespace@28..29 " "
                    KwIf@29..31 "if""##]],
        )
    }

    #[test]
    fn parens_change_precedence_preproc() {
        check(
            "#if (A | B) and (C | D) then #end if",
            expect![[r##"
            Source@0..36
              PreprocGlob@0..36
                PPIf@0..36
                  PPKwIf@0..3 "#if"
                  Whitespace@3..4 " "
                  PPBinaryExpr@4..24
                    PPParenExpr@4..12
                      LeftParen@4..5 "("
                      PPBinaryExpr@5..10
                        PPNameExpr@5..7
                          Name@5..7
                            Identifier@5..6 "A"
                            Whitespace@6..7 " "
                        Pipe@7..8 "|"
                        Whitespace@8..9 " "
                        PPNameExpr@9..10
                          Name@9..10
                            Identifier@9..10 "B"
                      RightParen@10..11 ")"
                      Whitespace@11..12 " "
                    KwAnd@12..15 "and"
                    Whitespace@15..16 " "
                    PPParenExpr@16..24
                      LeftParen@16..17 "("
                      PPBinaryExpr@17..22
                        PPNameExpr@17..19
                          Name@17..19
                            Identifier@17..18 "C"
                            Whitespace@18..19 " "
                        Pipe@19..20 "|"
                        Whitespace@20..21 " "
                        PPNameExpr@21..22
                          Name@21..22
                            Identifier@21..22 "D"
                      RightParen@22..23 ")"
                      Whitespace@23..24 " "
                  KwThen@24..28 "then"
                  Whitespace@28..29 " "
                  PPTokenBody@29..29
                  PPEndIf@29..36
                    PPKwEnd@29..33 "#end"
                    Whitespace@33..34 " "
                    KwIf@34..36 "if""##]],
        )
    }

    #[test]
    fn recover_parens_preproc_missing_right_paren() {
        check(
            "#if (A then #end if",
            expect![[r##"
                Source@0..19
                  PreprocGlob@0..19
                    PPIf@0..19
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPParenExpr@4..7
                        LeftParen@4..5 "("
                        PPNameExpr@5..7
                          Name@5..7
                            Identifier@5..6 "A"
                            Whitespace@6..7 " "
                      KwThen@7..11 "then"
                      Whitespace@11..12 " "
                      PPTokenBody@12..12
                      PPEndIf@12..19
                        PPKwEnd@12..16 "#end"
                        Whitespace@16..17 " "
                        KwIf@17..19 "if"
                error at 7..11: expected ‘)’, but found ‘then’"##]],
        )
    }

    #[test]
    fn recover_parens_preproc_missing_expr() {
        check(
            "#if () then #end if",
            expect![[r##"
                Source@0..19
                  PreprocGlob@0..19
                    PPIf@0..19
                      PPKwIf@0..3 "#if"
                      Whitespace@3..4 " "
                      PPParenExpr@4..7
                        LeftParen@4..5 "("
                        RightParen@5..6 ")"
                        Whitespace@6..7 " "
                      KwThen@7..11 "then"
                      Whitespace@11..12 " "
                      PPTokenBody@12..12
                      PPEndIf@12..19
                        PPKwEnd@12..16 "#end"
                        Whitespace@16..17 " "
                        KwIf@17..19 "if"
                error at 5..6: expected preprocessor condition, but found ‘)’"##]],
        )
    }
}
