use std::sync::Arc;

use unindent::unindent;

use crate::const_eval::ConstEvalCtx;

#[track_caller]
fn assert_const_eval(source: &str) {
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(source), source);
}

/// Expr version
#[track_caller]
fn assert_const_eval_expr(expr: &str) {
    let source = format!("const _ := {}", expr);
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(&source), &expr);
}

macro_rules! for_all_const_exprs {
    ($($src:literal)+) => {
        $(assert_const_eval_expr($src);)+
    };
}

fn do_const_eval(source: &str) -> String {
    let parsed = toc_parser::parse(&source);
    let mut unit_map = toc_hir::UnitMapBuilder::new();
    let hir_res = toc_hir_lowering::lower_ast(parsed.syntax(), &mut unit_map);
    let unit_map = Arc::new(unit_map.finish());

    let unit = unit_map.get_unit(hir_res.id);
    let const_eval_ctx = Arc::new(ConstEvalCtx::new(unit_map.clone()));
    super::collect_const_vars(unit, const_eval_ctx.clone());

    let mut results_str = String::new();

    // Need access to the inner state of the ConstEvalCtx, which is behind a lock
    {
        // Eagerly evaluate all of the available const vars
        let mut inner = const_eval_ctx.inner.write().unwrap();
        let const_exprs = inner.var_to_expr.values().copied().collect::<Vec<_>>();

        for expr in const_exprs {
            let results = match inner.eval_expr(expr) {
                Ok(v) => format!("{:?} -> {:?}\n", expr, v),
                Err(err) => format!("{:?} -> {:?}\n", expr, err),
            };

            results_str.push_str(&results);
        }
    }

    // Errors are bundled into the const error context
    stringify_const_eval_results(&results_str, &const_eval_ctx)
}

fn stringify_const_eval_results(results: &str, const_eval: &ConstEvalCtx) -> String {
    // Pretty print const eval ctx
    format!("{:#?}\n{}", const_eval, results)
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
fn error_div_by_zero() {
    // TODO: Add tests cases for real div, mod & rem
    for_all_const_exprs!["1 div 0"];
    for_all_const_exprs!["1 div 0.0"];
    for_all_const_exprs!["1.0 div 0"];
    for_all_const_exprs!["1.0 div 0.0"];
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

    // Referencing `self`
    assert_const_eval(&unindent(
        r#"
    const a := self
    "#,
    ));
}

#[test]
fn error_propogation() {
    // Propogation of errors
    assert_const_eval(&unindent(
        r#"
    const a := 1 div 0
    const b := a
    "#,
    ));
}
