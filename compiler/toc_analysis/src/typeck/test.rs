//! Type check tests
use std::sync::Arc;

use toc_reporting::ReportMessage;
use unindent::unindent;

use crate::{const_eval::ConstEvalCtx, ty::TyCtx};

macro_rules! test_for_each_op {
    ($top_level_name:ident, [$(($op:literal, $sub_name:ident)),+ $(,)?] => $source:literal) => {
        ::paste::paste! {
                $(
                #[test]
                fn [<$top_level_name _ $sub_name>]() {
                    let source = format!($source, $op);
                    assert_typecheck(&source);
                }
            )+
        }
    };
}

#[track_caller]
fn assert_typecheck(source: &str) {
    insta::assert_snapshot!(insta::internals::AutoName, do_typecheck(source), source);
}

#[track_caller]
fn assert_named_typecheck(name: &str, source: &str) {
    insta::assert_snapshot!(name, do_typecheck(source), source);
}

fn do_typecheck(source: &str) -> String {
    let parsed = toc_parser::parse(&source);
    let mut unit_map = toc_hir::UnitMapBuilder::new();
    let hir_res = toc_hir_lowering::lower_ast(parsed.syntax(), &mut unit_map);
    let unit_map = Arc::new(unit_map.finish());

    let unit = unit_map.get_unit(hir_res.id);
    let const_eval_ctx = Arc::new(ConstEvalCtx::new(unit_map.clone()));
    let (ty_ctx, typeck_messages) = crate::typeck::typecheck_unit(unit, const_eval_ctx);

    stringify_typeck_results(&ty_ctx, &typeck_messages)
}

fn stringify_typeck_results(ty_ctx: &TyCtx, messages: &[ReportMessage]) -> String {
    let mut s = String::new();
    // Pretty print typectx
    let mut pretty_ty_ctx = crate::ty::pretty_dump_typectx(ty_ctx);
    // Trim trailing newline
    pretty_ty_ctx.pop();
    s.push_str(&pretty_ty_ctx);

    // Pretty print the messages
    for err in messages.iter() {
        s.push_str(&format!("\n{}", err));
    }

    s
}

#[test]
fn var_decl_type_spec() {
    assert_typecheck(r#"var k : string"#);
}

#[test]
fn var_decl_inference() {
    assert_typecheck(r#"var k := "oeuf""#);
}

#[test]
fn var_decl_init_typecheck() {
    assert_typecheck(r#"var k : string := "oeuf""#);
}

#[test]
fn bare_var_decl() {
    // Invariant, to be covered by the parser stage
    // Should not amount to anything
    assert_typecheck("var k");
}

#[test]
fn undeclared_symbol() {
    assert_typecheck("var a := b");
    // No cyclic deps
    assert_typecheck("var a := a");
}

// Typecheck numerical ops
test_for_each_op! {
    number_op,
    [
        ("+", add),
        ("-", sub),
        ("*", mul),
        ("div", idiv),
        ("/", rdiv),
        ("mod", r#mod),
        ("rem", rem),
        ("**", exp),
    ] => r#"
    % Compatiblitly with all variant of numbers
    var r : real
    var i : int
    var n : nat
    var _rr := r {0} r
    var _ri := r {0} i
    var _ir := i {0} r
    var _rn := r {0} n
    var _nr := n {0} r
    var _ii := i {0} i
    var _in := i {0} n
    var _ni := n {0} i
    var _nn := n {0} n
"#
}

// Test integer inference for all compatible operators
test_for_each_op!(
    integer_inferrence,
    [
        ("+", add),
        ("-", sub),
        ("*", mul),
        ("div", idiv),
        ("/", rdiv),
        ("mod", r#mod),
        ("rem", rem),
        ("**", exp),
    ] => r#"
    % Inferred integer types should pass
    % Decl should be a concrete type
    var a := 1 {0} 1
    % Types of operands should make the type concrete
    var r : real
    var i : int
    var n : nat
    var _r0 := 1 {0} r
    var _r1 := r {0} 1
    var _i0 := 1 {0} i
    var _i1 := i {0} 1
    var _n0 := 1 {0} n
    var _n1 := n {0} 1
"#);

#[test]
fn typecheck_error_prop() {
    // Only one error should be reported, propogated error supresses the rest
    assert_typecheck(&unindent(
        r#"
    var a : int
    var b : string
    var c := a + b
    var j := c + a
    "#,
    ));
}

#[test]
fn typecheck_sized_char() {
    assert_named_typecheck("sized_char_literal", r#"var _ : char(1)"#);
    // trip through negatives shouldn't affect anything
    assert_named_typecheck("sized_char_simple_expr", r#"var _ : char(1 - 1 * 1 + 2)"#);
    assert_named_typecheck("sized_char_zero_sized", r#"var _ : char(0)"#);
    assert_named_typecheck("sized_char_max_sized", r#"var _ : char(32768)"#);
    assert_named_typecheck("sized_char_wrong_type", r#"var _ : char(1.0)"#);
    assert_named_typecheck("sized_char_wrong_type_bool", r#"var _ : char(true)"#);
    assert_named_typecheck("sized_char_const_err", r#"var _ : char(1.0 div 0.0)"#);
}

#[test]
fn typecheck_sized_string() {
    assert_named_typecheck("sized_string_literal", r#"var _ : string(1)"#);
    // trip through negatives shouldn't affect anything
    assert_named_typecheck(
        "sized_string_simple_expr",
        r#"var _ : string(1 - 1 * 1 + 2)"#,
    );
    assert_named_typecheck("sized_string_zero_sized", r#"var _ : string(0)"#);
    assert_named_typecheck("sized_string_max_sized", r#"var _ : string(32768)"#);
    assert_named_typecheck("sized_string_wrong_type", r#"var _ : string(1.0)"#);
    assert_named_typecheck("sized_string_wrong_type_bool", r#"var _ : string(true)"#);
    assert_named_typecheck("sized_string_const_err", r#"var _ : string(1.0 div 0.0)"#);
}

#[test]
fn typeck_constvar_initializer() {
    assert_named_typecheck("typeck_var_compatible", r#"var k : int := 100"#);
    assert_named_typecheck("typeck_var_incompatible", r#"var k : char := 20"#);
    assert_named_typecheck(
        "typeck_var_error_prop",
        &unindent(
            r#"
    var k := 20 + false
    var l : int := k   % Nothing reported here
    "#,
        ),
    );

    assert_named_typecheck("typeck_const_compatible", r#"const k : int := 100"#);
    assert_named_typecheck("typeck_const_incompatible", r#"const k : char := 20"#);
    assert_named_typecheck(
        "typeck_const_error_prop",
        &unindent(
            r#"
    const k := 20 + false
    const l : int := k   % Nothing reported here
    "#,
        ),
    );
}
