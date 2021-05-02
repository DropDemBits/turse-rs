use std::sync::Arc;

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

    // Need access to the inner state of the ConstEvalCtx, which is behind a lock
    {
        // Eagerly evaluate all of the available const vars
        let mut inner = const_eval_ctx.inner.write().unwrap();
        let const_exprs = inner.var_to_expr.values().copied().collect::<Vec<_>>();

        for expr in const_exprs {
            let _ = inner.eval_expr(expr);
        }
    }

    // Errors are bundled into the const error context
    stringify_const_eval_results(&const_eval_ctx)
}

fn stringify_const_eval_results(const_eval: &ConstEvalCtx) -> String {
    // Pretty print const eval ctx
    format!("{:#?}", const_eval)
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
        "+(1-2)"
    ];
}

#[test]
fn error_div_by_zero() {
    // TODO: Add tests cases for real div, mod & rem
    for_all_const_exprs!["1 div 0"];
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
        "(0 - 16#80000000) + (1 - 2)"
        // Over sub
        "16#ffffffff - (1 - 2)"
        "(0 - 16#80000000) - 1"
        // Over mul (positive result)
        "16#80000000 * 2"
        "(0 - 16#80000000) * (1 - 2) % no overflow"
        // Over mul (negative result)
        "16#80000001 * (1 - 2)"
        "(0 - 16#80000000) * 2"
        // Over div (negative result)
        "16#FFFFFFFF div (1 - 2)"
        "16#80000001 div (1 - 2)"
    ];
}
