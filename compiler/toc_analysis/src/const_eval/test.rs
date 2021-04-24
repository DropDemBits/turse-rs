use std::sync::Arc;

use toc_reporting::ReportMessage;

use crate::const_eval::ConstEvalCtx;

#[track_caller]
fn assert_const_eval(source: &str) {
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(source), source);
}

fn do_const_eval(source: &str) -> String {
    let parsed = toc_parser::parse(&source);
    let mut unit_map = toc_hir::UnitMapBuilder::new();
    let hir_res = toc_hir_lowering::lower_ast(parsed.syntax(), &mut unit_map);
    let unit_map = Arc::new(unit_map.finish());

    let unit = unit_map.get_unit(hir_res.id);
    let const_eval_ctx = Arc::new(ConstEvalCtx::new(unit_map.clone()));
    // TODO: Use prepass to build GlobalDefId -> ConstExpr mappings
    // - Wouldn't have to dep on typeck anymore
    let (_ty_ctx, typeck_messages) = crate::typeck::typecheck_unit(unit, const_eval_ctx.clone());

    stringify_const_eval_results(&const_eval_ctx, &typeck_messages)
}

fn stringify_const_eval_results(const_eval: &ConstEvalCtx, messages: &[ReportMessage]) -> String {
    let mut s = String::new();
    // Pretty print const eval ctx
    s.push_str(&format!("{:#?}", const_eval));

    // Pretty print the messages
    for err in messages.iter() {
        s.push_str(&format!("\n{}", err));
    }

    s
}

#[test]
fn eval_simple() {
    // All operations should be evaluated
    assert_const_eval("var _ : char(1 + 1)");
}
