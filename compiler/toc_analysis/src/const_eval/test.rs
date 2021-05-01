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
fn error_int_overflow() {
    // `mul` is the only operation that can overflow right now, since
    // even when using u64::MAX with add & sub, it still fits within an i128
    //
    // Requires testing with eval restrictions
    for_all_const_exprs!["16#FFFFFFFFFFFFFFFF * (16#8000000000000000 + 1)"];
}
