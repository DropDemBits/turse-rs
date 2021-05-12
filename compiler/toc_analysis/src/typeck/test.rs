//! Type check tests
use std::sync::Arc;

use toc_reporting::ReportMessage;
use unindent::unindent;

use crate::{const_eval::ConstEvalCtx, ty::TyCtx};

macro_rules! test_for_each_op {
    ($top_level_name:ident, $([$(($op:literal, $sub_name:ident)),+ $(,)?] => $source:literal),+ $(,)?) => {
        $(::paste::paste! {
                $(
                #[test]
                fn [<$top_level_name _ $sub_name>]() {
                    let source = format!($source, $op);
                    assert_typecheck(&source);
                }
            )+
        })+
    };
}

macro_rules! test_named_group {
    ($top_level_name:ident, [$($sub_name:ident => $source:literal),+ $(,)?]) => {
        ::paste::paste! {
            $(
                #[test]
                fn [<$top_level_name _ $sub_name>]() {
                    assert_typecheck(&::unindent::unindent($source));
                }
            )+
        }
    }
}

#[track_caller]
fn assert_typecheck(source: &str) {
    insta::assert_snapshot!(insta::internals::AutoName, do_typecheck(source), source);
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

// Typecheck basic ops
test_for_each_op! {
    arithmetic_op,
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
"#,
    [
        ("+", identity),
        ("-", negate),
    ] => r#"
    var r : real
    var i : int
    var n : nat
    var _r := {0} r
    var _i := {0} i
    var _n := {0} n
    "#
}

test_for_each_op! {
    arithmetic_op_wrong_type,
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
    var b : boolean
    var r : real
    var i : int
    var n : nat
    var _br := b {0} r
    var _bi := b {0} i
    var _bn := b {0} n
    var _rb := r {0} b
    var _ib := i {0} b
    var _nb := n {0} b
    var _bb := b {0} b
"#,
    [
        ("+", identity),
        ("-", negate),
    ] => r#"
    var b : boolean
    var _b := {0} b
"#
}

test_for_each_op! {
    bitwise_op,
    [
        ("and", and),
        ("or", or),
        ("xor", xor),
        ("shl", shl),
        ("shr", shr),
    ] => r#"
    % Compatiblitly with all variant of integers
    var i : int
    var n : nat
    var _ii := i {0} i
    var _in := i {0} n
    var _ni := n {0} i
    var _nn := n {0} n
"#,
    [
        ("not", not),
    ] => r#"
    % Compatiblitly with all variant of integers
    var i : int
    var n : nat
    var _i := {0} i
    var _n := {0} n
"#
}

test_for_each_op! {
    bitwise_op_wrong_type,
    [
        ("and", and),
        ("or", or),
        ("xor", xor),
        ("shl", shl),
        ("shr", shr),
    ] => r#"
    var b : boolean
    var r : real
    var i : int
    var n : nat
    var _bi := b {0} i
    var _bn := b {0} n
    var _ib := i {0} b
    var _nb := n {0} b
    var _ri := r {0} i
    var _rn := r {0} n
    var _ir := i {0} r
    var _nr := n {0} r
"#,
    [
        ("not", not),
    ] => r#"
    var r : real
    var _r := {0} r
"#
}

test_for_each_op! {
    logical_op,
    [
        ("and", and),
        ("or", or),
        ("=>", imply),
    ] => r#"
    % Compatiblitly with booleans
    var b : boolean
    var _bb := b {0} b
"#,
    [
        ("not", not),
    ] => r#"
    % Compatiblitly with booleans
    var b : boolean
    var _b := {0} b
"#
}

test_for_each_op! {
    logical_op_wrong_type,
    [
        ("and", and),
        ("or", or),
        ("=>", imply),
    ] => r#"
    var b : boolean
    var r : real
    var i : int
    var n : nat
    var _bi := b {0} i
    var _bn := b {0} n
    var _ib := i {0} b
    var _nb := n {0} b
    var _ri := r {0} i
    var _rn := r {0} n
    var _ir := i {0} r
    var _nr := n {0} r
"#,
    // unary `not` also covered in `bitwise_op_wrong_type`
}

// Test integer inference for all compatible operators
test_for_each_op! {
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
"#,
    [
        ("+", identity),
        ("-", negate),
    ] => r#"
    % Inferred integer types should pass
    % Decl should be a concrete type
    var a := {0} 1
    % Types of operands should make the type concrete
    var r : real
    var i : int
    var n : nat
    var _r0 := {0} r
    var _i0 := {0} i
    var _n0 := {0} n
"#
}

test_named_group! {
    sized_char,
    [
        literal => r#"var _ : char(1)"#,
        // trip through negatives shouldn't affect anything
        simple_expr => r#"var _ : char(1 - 1 * 1 + 2)"#,
        zero_sized => r#"var _ : char(0)"#,
        max_sized => r#"var _ : char(32768)"#,
        wrong_type => r#"var _ : char(1.0)"#,
        wrong_type_bool => r#"var _ : char(true)"#,
        const_err => r#"var _ : char(1.0 div 0.0)"#,
    ]
}

test_named_group! {
    sized_string,
    [
        literal => r#"var _ : string(1)"#,
        // trip through negatives shouldn't affect anything
        simple_expr => r#"var _ : string(1 - 1 * 1 + 2)"#,
        zero_sized => r#"var _ : string(0)"#,
        max_sized => r#"var _ : string(256)"#,
        over_sized => r#"var _ : string(512)"#,
        wrong_type => r#"var _ : string(1.0)"#,
        wrong_type_bool => r#"var _ : string(true)"#,
        const_err => r#"var _ : string(1.0 div 0.0)"#,
    ]
}

test_named_group! {
    typeck_var,
    [
        compatible => r#"var k : int := 100"#,
        incompatible => r#"var k : char := 20"#,
        error_prop => r#"
            var k := 20 + false
            var l : int := k   % Nothing reported here
            "#
    ]
}

test_named_group! {
    typeck_const,
    [
        compatible => r#"const k : int := 100"#,
        incompatible => r#"const k : char := 20"#,
        error_prop => r#"
            const k := 20 + false
            const l : int := k   % Nothing reported here
            "#
    ]
}

test_named_group! {
    typeck_assignment,
    [
        valid => r#"
            const j : int := 2
            var k : int := 1
            k := j
            "#,
        lhs_not_mut_is_const => r#"
            const j : int := 2
            const k : int := 3
            k := j
            "#,
        valid_compound_add => r#"
            var lhs : real
            var rhs : int
            lhs += rhs
            "#,
        invalid_compound_add => r#"
            var lhs : real
            var rhs : boolean
            lhs += rhs
            "#,
    ]
}
