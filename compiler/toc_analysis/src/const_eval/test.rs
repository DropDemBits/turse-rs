use std::cell::RefCell;

use toc_hir::library::{LibraryId, LoweredLibrary};
use toc_hir_db::db::HirDatabase;
use toc_reporting::{MessageBundle, MessageSink};
use unindent::unindent;

use crate::{const_eval::Const, db::ConstEval, test_db::TestDb, ty};

#[track_caller]
fn assert_const_eval(source: &str) {
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(source), source);
}

/// Expr version
#[track_caller]
fn assert_const_eval_expr(expr: &str) {
    let source = format!("const _ := {expr}");
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(&source), expr);
}

macro_rules! for_all_const_exprs {
    ($($src:literal)+) => {
        $(assert_const_eval_expr($src);)+
    };
}

fn do_const_eval(source: &str) -> String {
    let (db, library_id) = TestDb::from_source(source);
    let library = db.library(library_id);

    // Eagerly evaluate all of the available const vars
    let const_evaluator = ConstEvaluator {
        db: &db,
        library_id,
        library: library.clone(),
        reporter: Default::default(),
        results: Default::default(),
    };

    toc_hir::visitor::Walker::from_library(&library).visit_preorder(&const_evaluator);

    struct ConstEvaluator<'db> {
        db: &'db TestDb,
        library_id: LibraryId,
        library: LoweredLibrary,
        reporter: RefCell<MessageSink>,
        results: RefCell<String>,
    }

    impl toc_hir::visitor::HirVisitor for ConstEvaluator<'_> {
        fn visit_constvar(&self, id: toc_hir::item::ItemId, item: &toc_hir::item::ConstVar) {
            let def_id = self.library.item(id).def_id;
            let body = if let Some(body) = item.init_expr {
                body
            } else {
                return;
            };

            let eval_res = self
                .db
                .evaluate_const(Const::from_body(self.library_id, body), Default::default());

            let name = {
                let def_info = self.library.local_def(def_id);
                format!(
                    "{:?}@{:?}",
                    def_info.name.item(),
                    def_info.name.span().lookup_in(&self.library.span_map)
                )
            };

            let results = match eval_res {
                Ok(v) => format!("{name} -> {v:?}\n"),
                Err(err) => {
                    let text = format!("{name} -> {err:?}\n");
                    err.report_to(self.db, &mut *self.reporter.borrow_mut());
                    text
                }
            };

            self.results.borrow_mut().push_str(&results);
        }
    }

    let ConstEvaluator {
        reporter, results, ..
    } = const_evaluator;
    let reporter = reporter.into_inner();
    let results = results.into_inner();

    // Errors are bundled into the const error context
    stringify_const_eval_results(&results, reporter.finish())
}

fn stringify_const_eval_results(results: &str, messages: MessageBundle) -> String {
    // Pretty print const eval ctx
    // Want:
    // - Evaluation state of all accessible DefIds
    let mut s = format!("{results}\n");

    // Pretty print the messages
    for err in messages.iter() {
        s.push_str(&format!("{err}\n"));
    }

    s
}

#[test]
fn complex_arithmetic_expr() {
    assert_const_eval_expr("1 + 2 * 3 div 3 - 4 + 5");
}

#[test]
fn arithmetic_const_ops() {
    // All operations should be evaluated
    for_all_const_exprs![
        "1 + 1"
        "10 - 2"
        "2 * 5"
        "3 div 2"
        "-1"
        "+-1"

        "1 + 1.0"
        "10 - 2.0"
        "2 * 5.0"
        "3 div 2.0"
        "-1.0"
        "+-1.0"

        // RealDiv
        "1.0 / 2.0"
        "1 / 2"
        "1.0 / -2"

        // Mod
        " 3 mod 10"
        "-3 mod 10"
        " 3 mod -10"
        "-3 mod -10"

        " 3.0 mod 10.0"
        "-3.0 mod 10.0"
        " 3.0 mod -10.0"
        "-3.0 mod -10.0"

        " 3.5 mod 10.0   % 3.5"
        "-3.5 mod 10.0   % 6.5"
        " 3.5 mod -10.0   % -6.5"
        "-3.5 mod -10.0   % -3.5"

        " 3.0 mod 10.5   % 3"
        "-3.0 mod 10.5   % 7.5"
        " 3.0 mod -10.5   % -7.5"
        "-3.0 mod -10.5   % -3"

        // Rem
        " 3 mod 10"
        "-3 mod 10"
        " 3 mod -10"
        "-3 mod -10"

        " 3.0 mod 10.0"
        "-3.0 mod 10.0"
        " 3.0 mod -10.0"
        "-3.0 mod -10.0"

        // Exp
        "2 ** 4"
        "2 ** 0"
        "-2 ** 4"
        "-2 ** 0"

        "2.0 ** 4"
        "2.0 ** 0"
        "-2.0 ** -4"
        "-2.0 ** -2"
    ];
}

#[test]
fn logic_const_ops() {
    for_all_const_exprs![
        "false and false" // 0
        "false and true"  // 0
        "true and false"  // 0
        "true and true"   // 1

        "false or false"  // 0
        "false or true"   // 1
        "true or false"   // 1
        "true or true"    // 1

        "false xor false" // 0
        "false xor true"  // 1
        "true xor false"  // 1
        "true xor true"   // 0

        "false => false"  // 1
        "false => true"   // 1
        "true => false"   // 0
        "true => true"    // 1

        "not true"
        "not false"
    ];
}

#[test]
fn bitwise_const_ops() {
    for_all_const_exprs![
        "0 and 0"
        "0 and 1"
        "1 and 0"
        "1 and 1"

        "0 or 0"
        "0 or 1"
        "1 or 0"
        "1 or 1"

        "0 xor 0"
        "0 xor 1"
        "1 xor 0"
        "1 xor 1"

        "not 0"
        "not 1"

        // Applicable to negative numbers too
        "0 and -0"
        "0 and -1"
        "1 and -0"
        "1 and -1"

        "0 or -0"
        "0 or -1"
        "1 or -0"
        "1 or -1"

        "0 xor -0"
        "0 xor -1"
        "1 xor -0"
        "1 xor -1"

        "not -0"
        "not -1"

        "1 shl 3"
        "-1 shl 0"

        "24 shr 3"
        "-24 shr 3"

        // Wrapping around the mask
        "1 shl 32"
        "8 shr 32"
    ];
}

#[test]
fn string_concat_ops() {
    for_all_const_exprs![
        // Special cases, produce CharN values
        // char char
        r#"'b' + 'a'"#
        // char charN
        // charN char
        r#"'a' + 'cab'"#
        r#"'aca' + 'b'"#
        // charN charN
        r#"'wow' + 'ie'"#

        // The rest, produce String values
        // char string
        // string char
        r#"'v' + "ee""#
        r#""ve" + 'e'"#
        // charN string
        // string charN
        r#""o" + 'wo'"#
        r#"'ow' + "o""#
        // string string
        r#""v" + "ee""#
    ];
}

#[test]
fn compare_ops_numeric() {
    // Testing ordering
    for_all_const_exprs![
        // With reals
        "1.0 < 2.0"
        "2.0 < 1.0"
        "1.0 > 2.0"
        "2.0 > 1.0"

        "1.0 <= 1.0"
        "1.0 <= 2.0"
        "2.0 >= 2.0"
        "2.0 >= 1.0"

        // With integers
        "1 < 2"
        "2 < 1"
        "1 > 2"
        "2 > 1"

        "1 <= 1"
        "1 <= 2"
        "2 >= 2"
        "2 >= 1"
    ];
}

#[test]
fn compare_ops_charseq() {
    for_all_const_exprs! [
        r#"'c' < 'c'"#
        r#"'c' > 'c'"#
        r#"'c' <= 'c'"#
        r#"'c' >= 'c'"#

        r#"'c' < 'cc'"#
        r#"'c' > 'cc'"#
        r#"'c' <= 'cc'"#
        r#"'c' >= 'cc'"#

        r#"'cc' < 'c'"#
        r#"'cc' > 'c'"#
        r#"'cc' <= 'c'"#
        r#"'cc' >= 'c'"#

        r#""" < 'c'"#
        r#""" > 'c'"#
        r#""" <= 'c'"#
        r#""" >= 'c'"#

        r#"'c' < """#
        r#"'c' > """#
        r#"'c' <= """#
        r#"'c' >= """#

        r#"'cc' < "dd""#
        r#"'cc' > "dd""#
        r#"'cc' <= "dd""#
        r#"'cc' >= "dd""#
    ];
}

#[test]
fn equality_ops_numeric() {
    for_all_const_exprs![
        // With reals
        "1.0 = 2.0"
        "2.0 = 2.0"
        "1.0 ~= 2.0"
        "2.0 ~= 2.0"

        // With integers
        "1 = 2"
        "2 = 2"
        "1 ~= 2"
        "2 ~= 2"
    ];
}

#[test]
fn equality_ops_bool() {
    for_all_const_exprs! [
        "true = true"
        "true = false"
        "false = true"
        "false = false"

        "true ~= true"
        "true ~= false"
        "false ~= true"
        "false ~= false"
    ];
}

#[test]
fn equality_ops_charseq() {
    for_all_const_exprs! [
        r#"'c' = 'c'"#
        r#"'c' ~= 'c'"#

        r#"'c' = 'cc'"#
        r#"'c' ~= 'cc'"#

        r#"'cc' = 'c'"#
        r#"'cc' ~= 'c'"#

        r#""" = 'c'"#
        r#""" ~= 'c'"#

        r#"'c' = """#
        r#"'c' ~= """#

        r#"'cc' = "dd""#
        r#"'cc' ~= "dd""#
    ];
}

#[test]
fn real_promotion() {
    for_all_const_exprs![
        "1.0 + 1"
        "  1 + 1.0"
        "1.0 + 1.0"

        "1.0 - 1"
        "  1 - 1.0"
        "1.0 - 1.0"

        "1.0 * 1"
        "  1 * 1.0"
        "1.0 * 1.0"

        "1.0 div 1"
        "  1 div 1.0"
        "1.0 div 1.0"

        "1 < 1.0"
        "1.0 < 1"

        "1 > 1.0"
        "1.0 > 1"

        "1 >= 1.0"
        "1.0 >= 1"

        "1 <= 1.0"
        "1.0 <= 1"
    ];
}

#[test]
fn const_local_var_lookup() {
    assert_const_eval_expr(&unindent(
        r#"
    const a := 1
    const b := a + 1
    const c := b
    "#,
    ));
}

#[test]
fn const_unqualified_export_var_lookup() {
    assert_const_eval_expr(&unindent(
        "
    module z
        export ~.a
        const a : int := 2
    end z
    const k := a
    ",
    ));
}

#[test]
fn error_div_by_zero() {
    // integer div
    for_all_const_exprs![
        "1 div 0"
        "1 div 0.0"
        "1.0 div 0"
        "1.0 div 0.0"
    ];

    // real div
    for_all_const_exprs![
        "1 / 0"
        "1 / 0.0"
        "1.0 / 0"
        "1.0 / 0.0"
    ];

    // modulus
    for_all_const_exprs![
        "1 mod 0"
        "1 mod 0.0"
        "1.0 mod 0"
        "1.0 mod 0.0"
    ];

    // remainder
    for_all_const_exprs![
        "1 rem 0"
        "1 rem 0.0"
        "1.0 rem 0"
        "1.0 rem 0.0"
    ];
}

#[test]
fn error_int_overflow_32bit() {
    // TODO: Test operations on 64-bit values

    for_all_const_exprs![
        // 64-bit literals should overflow right away
        "16#100000000"
        // Unsigned 32-bit literals shouldn't
        "16#80000000 % no overflow"
        "16#FFFFFFFF % no overflow"
        // Over add
        "16#ffffffff + 1"
        "-16#80000000 + -1"
        // Over sub
        "16#ffffffff - (-1)"
        "-16#80000000 - 1"
        "16#ffffffff - 16#ffffffff % no overflow"
        // Over mul (positive result)
        "16#80000000 * 2"
        "-16#80000000 * -1 % no overflow"
        // Over mul (negative result)
        "16#80000001 * -1"
        "-16#80000000 * 2"
        // Over idiv (positive result)
        "1e100 div 1"
        "1e100 div 1.0"
        // Over idiv (negative result)
        "16#FFFFFFFF div -1"
        "16#80000001 div -1"
        // Over negate
        "-16#FFFFFFFF"
        "--16#80000000 % no overflow"
        // Over identity (no overflow)
        "+16#FFFFFFFF % no overflow"
        "+-16#80000000 % no overflow"

        // modulo is safe except for large inputs (overflow before then)
        // rem is safe except for large inputs (overflow before then)

        // Over Exp
        "2 ** 33"
        "2 ** 32"
        "(-2) ** 33"

        // Over shl
        "2 shl 31"
        // Any shl over negative integers will overflow
        "-1 shl 1"
    ];
}

#[test]
fn error_real_overflow() {
    for_all_const_exprs![
        // Over add
        "1e308 + 1e308"
        "-1e308 + (-1e308)"
        // Over sub
        "1e308 - (-1e308)"
        "-1e308 - 1e308"
        // Over mul
        "1e308 * 10"
        "-1e308 * 10"
        // Over idiv (checked in `error_int_overflow_{32,64}_bit`)
        // Over rdiv
        "1e308 / 0.1"
        "-1e308 / 0.1"
        // modulo is safe except for large inputs (overflow before then)
        // rem is safe except for large inputs (overflow before then)
        // Over exp
        "10.0 ** 309"
        "(-10.0) ** 309"
    ];
}

#[test]
fn error_arithmetic_wrong_types() {
    for_all_const_exprs![
        "1 + true"
        "1 - true"
        "1 * true"
        "1 div true"
        "false + 1"
        "false - 1"
        "false * 1"
        "false div 1"

        "1.0 + true"
        "1.0 - true"
        "1.0 * true"
        "1.0 div true"
        "false + 1.0"
        "false - 1.0"
        "false * 1.0"
        "false div 1.0"

        "-false"
        "+true"

        "1 / true"
        "1 mod true"
        "1 rem true"
        "1 ** true"
        "1.0 / true"
        "1.0 mod true"
        "1.0 rem true"
        "1.0 ** true"
    ];
}

#[test]
fn error_logical_wrong_types() {
    for_all_const_exprs![
        "1.0 and false"
        "1.0 or false"
        "1.0 xor false"
        "1.0 => false"

        "false and 1.0"
        "false or 1.0"
        "false xor 1.0"
        "false => 1.0"

        "not 1.0"

        "1 shl 1.0"
        "false shr 1"
    ];
}

#[test]
fn error_comparison_wrong_types() {
    for_all_const_exprs! [
        r#"1 < 'c'"#
        r#"1 < 'cc'"#
        r#"1 < "cc""#
        r#"1 < true"#

        r#"1.0 < 'c'"#
        r#"1.0 < 'cc'"#
        r#"1.0 < "cc""#
        r#"1.0 < true"#

        r#"'c' < 1"#
        r#"'cc' < 1"#
        r#""cc" < 1"#
        r#"true < 1"#

        r#"'c' < 1.0"#
        r#"'cc' < 1.0"#
        r#""cc" < 1.0"#
        r#"true < 1.0"#

        // Bools cannot be compared order-wise
        r#"true < true"#
        r#"true >= true"#
        r#"true < true"#
        r#"true >= true"#
    ];
}

#[test]
fn error_equality_wrong_types() {
    for_all_const_exprs! [
        r#"1 = 'c'"#
        r#"1 = 'cc'"#
        r#"1 = "cc""#
        r#"1 = true"#

        r#"1.0 = 'c'"#
        r#"1.0 = 'cc'"#
        r#"1.0 = "cc""#
        r#"1.0 = true"#

        r#"'c' = 1"#
        r#"'cc' = 1"#
        r#""cc" = 1"#
        r#"true = 1"#

        r#"'c' = 1.0"#
        r#"'cc' = 1.0"#
        r#""cc" = 1.0"#
        r#"true = 1.0"#
    ];
}

#[test]
fn error_no_const_expr() {
    // Referencing a runtime-evaluated var
    assert_const_eval(&unindent(
        r#"
    var a := 1
    const b := a
    "#,
    ));

    // Referencing a runtime-evaluated const
    assert_const_eval(&unindent(
        r#"
    var a := 1
    const b := a
    const c := b
    "#,
    ));

    // Referencing a type
    assert_const_eval(&unindent(
        r#"
    type a : int
    const b := a
    "#,
    ));

    // Referencing an undeclared def
    assert_const_eval("const b := a");

    // Using a missing expression
    assert_const_eval("const b := ()");

    // Referencing `self`
    // TODO: Uncomment when `self` is lowered again
    /*
    assert_const_eval(&unindent(
        r#"
    const a := self
    "#,
    ));
    */
}

#[test]
fn error_outside_range_nat() {
    // FIXME: Uncomment large values once we support 64-bit literals
    // nat1
    assert_const_eval(&unindent(
        "
        const v0 : nat1 := -1
        const v1 : nat1 := 0
        const v2 : nat1 := 16#ff
        const v3 : nat1 := 16#100
        const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
    // nat2
    assert_const_eval(&unindent(
        "
        const v0 : nat2 := -1
        const v1 : nat2 := 0
        const v2 : nat2 := 16#ffff
        const v3 : nat2 := 16#10000
        const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
    // nat4
    assert_const_eval(&unindent(
        "
        const v0 : nat4 := -1
        const v1 : nat4 := 0
        const v2 : nat4 := 16#ffffffff
        %const v3 : nat4 := 16#100000000
        const _ := v0
        const _ := v1
        const _ := v2
        %const _ := v3
    ",
    ));
    // nat
    assert_const_eval(&unindent(
        "
        const v0 : nat := -1
        const v1 : nat := 0
        const v2 : nat := 16#fffffffe
        const v3 : nat := 16#ffffffff
        const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
    // addrint
    assert_const_eval(&unindent(
        "
        const v0 : addressint := -1
        const v1 : addressint := 0
        const v2 : addressint := 16#ffffffff
        %const v3 : addressint := 16#100000000
        const _ := v0
        const _ := v1
        const _ := v2
        %const _ := v3
    ",
    ));
}

#[test]
fn error_outside_range_int() {
    // FIXME: Uncomment large values once we support 64-bit literals
    // int1
    assert_const_eval(&unindent(
        "
        const v0 : int1 := -16#81
        const v1 : int1 := -16#80
        const v2 : int1 := 16#7f
        const v3 : int1 := 16#80
        const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
    // int2
    assert_const_eval(&unindent(
        "
        const v0 : int2 := -16#8001
        const v1 : int2 := -16#8000
        const v2 : int2 := 16#7fff
        const v3 : int2 := 16#8000
        const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
    // int4
    assert_const_eval(&unindent(
        "
        %const v0 : int4 := -16#80000001
        const v1 : int4 := -16#80000000
        const v2 : int4 := 16#7fffffff
        const v3 : int4 := 16#80000000
        %const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
    // int
    assert_const_eval(&unindent(
        "
        const v0 : int := -16#80000000
        const v1 : int := -16#7fffffff
        const v2 : int := 16#7fffffff
        const v3 : int := 16#80000000
        const _ := v0
        const _ := v1
        const _ := v2
        const _ := v3
    ",
    ));
}

#[test]
fn error_propagation() {
    // Propagation of errors
    assert_const_eval(&unindent(
        r#"
    const a := 1 div 0
    const b := a
    "#,
    ));
}

#[test]
fn error_negative_int_exp() {
    assert_const_eval_expr("2 ** -1");
}

#[test]
fn error_negative_int_shift() {
    assert_const_eval_expr("2 shl -1");
    assert_const_eval_expr("2 shr -1");
}

#[test]
fn error_charseq_too_big() {
    // Character sequences are very long, so we'll have to make the string programmatically
    const SEQ_LEN: usize = ty::MAX_CHAR_N_LEN as usize;
    const FILLER: &str = "✨✨✨✨✨✨✨✨";
    let mut big_charseq = String::with_capacity(SEQ_LEN);

    for _ in 0..(SEQ_LEN / FILLER.len()) {
        // ✨sparkle✨ to death
        big_charseq.push_str(FILLER);
    }

    // Fill the remaining length with a's
    for _ in big_charseq.len()..SEQ_LEN {
        big_charseq.push('a');
    }

    let mid = {
        // Skip towards a char
        let mut cursor = big_charseq.len() / 2;
        while !big_charseq.is_char_boundary(cursor) {
            cursor += 1
        }
        cursor
    };

    // char, charN
    let almost_slice = &big_charseq[..big_charseq.len() - 1];
    assert_const_eval_expr(&format!("'a'+'{almost_slice}'"));
    // charN, char
    assert_const_eval_expr(&format!("'{almost_slice}'+'a'"));
    // charN, charN
    let (left, right) = big_charseq.split_at(mid);
    assert_const_eval_expr(&format!("'{left}'+'{right}'"));
}

#[test]
fn error_string_too_big() {
    // Max string size is sorta long, so we'll still make the string programmatically
    const SEQ_LEN: usize = ty::MAX_STRING_LEN as usize;
    const FILLER: &str = "✨✨✨✨✨✨✨✨";
    let mut big_charseq = String::with_capacity(SEQ_LEN);

    for _ in 0..(SEQ_LEN / FILLER.len()) {
        // ✨sparkle✨ to death
        big_charseq.push_str(FILLER);
    }

    // Fill the remaining length with a's
    for _ in big_charseq.len()..SEQ_LEN {
        big_charseq.push('a');
    }

    // char, string
    let almost_slice = &big_charseq[..big_charseq.len() - 1];
    assert_const_eval_expr(&format!(r#"'a'+"{}""#, almost_slice));
    // string, char
    assert_const_eval_expr(&format!(r#""{}"+'a'"#, almost_slice));

    let mid = {
        // Skip towards a char
        let mut cursor = big_charseq.len() / 2;
        while !big_charseq.is_char_boundary(cursor) {
            cursor += 1
        }
        cursor
    };
    // string, charN
    let (left, right) = big_charseq.split_at(mid);
    assert_const_eval_expr(&format!(r#""{}"+'{}'"#, left, right));
    // charN, string
    assert_const_eval_expr(&format!(r#"'{}'+"{}""#, left, right));
    // string, string
    assert_const_eval_expr(&format!(r#""{}"+"{}""#, left, right));
}

#[test]
fn restrict_assign_type() {
    // Assignment type restriction is only checked on const use (for eval only)
    // Boolean is assignable into boolean
    assert_const_eval(r#"const a : boolean := false const _:=a"#);
    // Integer is not assignable into boolean
    assert_const_eval(r#"const a : boolean := 1 const _:=a"#);
    // Real is not assignable into boolean
    assert_const_eval(r#"const a : boolean := 1.0 const _:=a"#);

    // Boolean is not assignable into integers
    assert_const_eval(r#"const a : int := false const _:=a"#);
    // Integer is assignable into integer
    assert_const_eval(r#"const a : int := 1 const _:=a"#);
    // Real is not assignable into integers
    assert_const_eval(r#"const a : int := 1.0 const _:=a"#);

    // Boolean is not assignable into real
    assert_const_eval(r#"const a : real := false const _:=a"#);
    // Integers are assignable into reals (promoted)
    assert_const_eval(r#"const a : real := 1 const _:=a"#);
    // Real is assignable into real
    assert_const_eval(r#"const a : real := 1.0 const _:=a"#);
}

#[test]
fn supported_values() {
    // All handled const values
    for_all_const_exprs![
        r#"1"#
        r#"1.0"#
        r#"true"#
        r#""alphabet""#
        r#"'fun times'"#
        r#"'e'"#
    ];
}

#[test]
fn unsupported_ops() {
    for_all_const_exprs![
        "1 in 1"
        "1 ~in 1"
    ];
}
