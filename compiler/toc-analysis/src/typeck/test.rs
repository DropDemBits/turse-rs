//! Type check tests

use toc_hir::symbol::DefId;
use toc_hir_db::Db;
use toc_reporting::{MessageBundle, WithDisplayLocations};
use unindent::unindent;

use crate::{
    db::{HirAnalysis, TypeDatabase},
    test_db::TestDb,
};

macro_rules! assert_typecheck {
    ($source:expr) => {
        let source: &str = $source;
        insta::assert_snapshot!(::insta::internals::AutoName, do_typecheck(source), source);
    };
}

macro_rules! test_for_each_op {
    ($top_level_name:ident, $([$(($op:literal, $sub_name:ident)),+ $(,)?] => $source:literal),+ $(,)?) => {
        $(::paste::paste! {
                $(
                #[test]
                fn [<$top_level_name _ $sub_name>]() {
                    let source = format!($source, $op);
                    assert_typecheck!(&source);
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
                    assert_typecheck!(&::unindent::unindent($source));
                }
            )+
        }
    }
}

fn do_typecheck(source: &str) -> String {
    let (db, package) = TestDb::from_source(source);
    let typeck_res = db.typecheck_package(package);
    typeck_res.messages().assert_no_delayed_reports();

    stringify_typeck_results(&db, package.into(), typeck_res.messages())
}

fn stringify_typeck_results(
    db: &TestDb,
    package_id: toc_hir::package::PackageId,
    messages: &MessageBundle,
) -> String {
    use std::fmt::Write;

    let mut s = String::new();

    // Pretty print typectx
    // Want: `type_of` all reachable DefIds
    // - Printed type nodes because we wanted to see the full type
    let package = db.package(package_id);
    for did in package.local_defs() {
        let def_info = package.local_def(did);
        let name = def_info.name;
        let name_span = db.span_to_location(def_info.def_at.lookup_in(&package));
        let sym_kind = match def_info.kind {
            Some(kind) => format!("{kind:?}"),
            None => "Undeclared".to_string(),
        };
        let ty = db.type_of(DefId(package_id, did).into()).debug(db);

        writeln!(&mut s, "{name:?}@{name_span:?} [{sym_kind}]: {ty:?}").unwrap();
    }

    // Pretty print the messages
    for err in messages.iter() {
        write!(&mut s, "\n{}", err.display_spans(db.location_display())).unwrap();
    }

    s
}
#[test]
fn var_decl_type_spec() {
    assert_typecheck!(r#"var k : string"#);
}

#[test]
fn var_decl_inference() {
    assert_typecheck!(r#"var k := "oeuf""#);
    assert_typecheck!(r#"var k := 'oe'"#);
    assert_typecheck!(r#"var k := 'o'"#);
}

#[test]
fn var_decl_init_typecheck() {
    assert_typecheck!(r#"var k : string := "oeuf""#);
}

#[test]
fn bare_var_decl() {
    assert_typecheck!("var k k := 3");
    // should yell about this
    assert_typecheck!("const k k := 3");
}

#[test]
fn typecheck_error_prop() {
    // Only one error should be reported, propagated error suppresses the rest
    assert_typecheck!(&unindent(
        r#"
    var a : int
    var b : string
    var c := a + b
    var j := c + a
    "#,
    ));
}

#[test]
fn typecheck_missing_exprs() {
    // Be resilient against missing expressions
    assert_typecheck!("const ke : int := ()");

    assert_typecheck!("get ()");

    assert_typecheck!("begin var aaaa : int bind aaaa to end");
}

#[test]
fn block_stmt_check() {
    // Check inside block stmts
    assert_typecheck!(r#"begin var k : char := 'baz' end"#);
}

// Typecheck basic ops
test_for_each_op! { common_op_wrong_value,
    [
        ("+", add),
        ("-", sub),
        ("*", mul),
        ("div", idiv),
        ("/", rdiv),
        ("mod", r#mod),
        ("rem", rem),
        ("**", exp),
        ("and", and),
        ("or", or),
        ("xor", xor),
        ("shl", shl),
        ("shr", shr),
        ("=>", imply),

        // These also support types, but only for class comparisons
        // FIXME: Move to a separate test when class types are supported
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => "
    type t : int
    var k : int

    % Type operand prevents checking of type compatibility
    var _tt := t {0} t
    var _tk := t {0} k
    var _kt := k {0} t
    ",
    [
        ("+", identity),
        ("-", negate),
        ("not", not),
    ] => "
    type t : boolean

    % Type operand prevents checking of type compatibility
    var _t := {0} t
    "
}

test_for_each_op! { arithmetic_op,
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
    % Compatibility with all variant of numbers
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

test_for_each_op! { arithmetic_op_wrong_type,
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

test_for_each_op! { bitwise_op,
    [
        ("and", and),
        ("or", or),
        ("xor", xor),
        ("shl", shl),
        ("shr", shr),
    ] => r#"
    % Compatibility with all variant of integers
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
    % Compatibility with all variant of integers
    var i : int
    var n : nat
    var _i := {0} i
    var _n := {0} n
"#
}

test_for_each_op! { bitwise_op_wrong_type,
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

test_for_each_op! { logical_op,
    [
        ("and", and),
        ("or", or),
        ("=>", imply),
    ] => r#"
    % Compatibility with booleans
    var b : boolean
    var _bb := b {0} b
"#,
    [
        ("not", not),
    ] => r#"
    % Compatibility with booleans
    var b : boolean
    var _b := {0} b
"#
}

test_for_each_op! { logical_op_wrong_type,
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

test_for_each_op! { string_manip_op,
    [("+", concat)] =>
    // over all 30 permutations + 6 same-arg permutations
    r#"
    proc __ (var c_any : char(*), var s_any : string(*))
    var c : char
    var c_sz : char(6)
    var s : string
    var s_sz : string(6)

    % special cases first
    var _t00 := c {0} c
    var _t02 := c {0} c_sz
    var _t20 := c_sz {0} c
    var _t22 := c_sz {0} c_sz

    % the rest of them down here (should produce string)
    var _t01 := c {0} c_any
    var _t03 := c {0} s
    var _t04 := c {0} s_any
    var _t05 := c {0} s_sz

    var _t10 := c_any {0} c
    var _t30 := s {0} c
    var _t40 := s_any {0} c
    var _t50 := s_sz {0} c

    var _t11 := c_any {0} c_any
    var _t12 := c_any {0} c_sz
    var _t13 := c_any {0} s
    var _t14 := c_any {0} s_any
    var _t15 := c_any {0} s_sz

    var _t21 := c_sz {0} c_any
    var _t31 := s {0} c_any
    var _t41 := s_any {0} c_any
    var _t51 := s_sz {0} c_any

    var _t23 := c_sz {0} s
    var _t24 := c_sz {0} s_any
    var _t25 := c_sz {0} s_sz

    var _t32 := s {0} c_sz
    var _t42 := s_any {0} c_sz
    var _t52 := s_sz {0} c_sz

    var _t33 := s {0} s
    var _t34 := s {0} s_any
    var _t35 := s {0} s_sz

    var _t43 := s_any {0} s
    var _t53 := s_sz {0} s

    var _t44 := s_any {0} s_any
    var _t45 := s_any {0} s_sz

    var _t54 := s_sz {0} s_any

    var _t55 := s_sz {0} s_sz
    end __
    "#
}

test_for_each_op! { string_manip_op_wrong_type,
    [("+", concat)] => r#"
    proc __ (var c_any : char(*), var s_any : string(*))
    var c : char
    var c_sz : char(6)
    var s : string
    var s_sz : string(6)
    var i : int

    var _e00 := c {0} i
    var _e01 := c_any {0} i
    var _e02 := c_sz {0} i
    var _e03 := s {0} i
    var _e04 := s_any {0} i
    var _e05 := s_sz {0} i

    var _e10 := i {0} c
    var _e11 := i {0} c_any
    var _e12 := i {0} c_sz
    var _e13 := i {0} s
    var _e14 := i {0} s_any
    var _e15 := i {0} s_sz

    % TODO: Uncomment to verify incompatible types
    /*
    var _e20 : char(13) := c_sz {0} c_sz
    var _e21 : char(8) := c_sz {0} c
    var _e22 : char(8) := c {0} c_sz
    var _e23 : char(3) := c {0} c
    var _e24 : char := c {0} c
    */
    end __
    "#
}

test_for_each_op! { comparison_op_numerics,
    [
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => r#"
    % Numerics
    var r : real
    var i : int
    var n : nat

    % should all produce booleans
    var _v_res : boolean

    _v_res := 1 {0} 1
    _v_res := 1 {0} 1.0
    _v_res := 1 {0} r
    _v_res := 1 {0} i
    _v_res := 1 {0} n

    _v_res := 1.0 {0} 1
    _v_res := 1.0 {0} 1.0
    _v_res := 1.0 {0} r
    _v_res := 1.0 {0} i
    _v_res := 1.0 {0} n

    _v_res := r {0} 1
    _v_res := r {0} 1.0
    _v_res := r {0} r
    _v_res := r {0} i
    _v_res := r {0} n

    _v_res := i {0} 1
    _v_res := i {0} 1.0
    _v_res := i {0} r
    _v_res := i {0} i
    _v_res := i {0} n

    _v_res := n {0} 1
    _v_res := n {0} 1.0
    _v_res := n {0} r
    _v_res := n {0} i
    _v_res := n {0} n
    "#,
}

test_for_each_op! { comparison_op_charseqs,
    [
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => r#"
    % Sized charseqs
    var c : char
    var c_sz : char(6)
    var s : string
    var s_sz : string(6)

    % should all produce booleans
    var _v_res : boolean

    _v_res := c {0} c
    _v_res := c {0} c_sz
    _v_res := c {0} s
    _v_res := c {0} s_sz

    _v_res := c_sz {0} c
    _v_res := c_sz {0} c_sz
    _v_res := c_sz {0} s
    _v_res := c_sz {0} s_sz

    _v_res := s {0} c
    _v_res := s {0} c_sz
    _v_res := s {0} s
    _v_res := s {0} s_sz

    _v_res := s_sz {0} c
    _v_res := s_sz {0} c_sz
    _v_res := s_sz {0} s
    _v_res := s_sz {0} s_sz
    "#,
}

test_for_each_op! { comparison_op_enums,
    [
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => r#"
    type s : enum(a)
    var a, b : s

    % should all produce booleans
    var _v_res : boolean

    _v_res := a {0} b
    _v_res := b {0} a
    % enum variants are covered by equivalence rules
    "#,
}

test_for_each_op! { comparison_op_sets,
    [
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => r#"
    type s : set of boolean
    var a, b : s

    % should all produce booleans
    var _v_res : boolean

    _v_res := a {0} b
    _v_res := b {0} a
    "#,
}

test_for_each_op! { comparison_op_wrong_types,
    [
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => r#"
    % Numerics
    var r : real
    var i : int
    var n : nat

    % Other scalars
    var b : boolean

    % Sized charseqs
    var c : char
    var c_sz : char(6)
    var s : string
    var s_sz : string(6)

    % Different sets
    type s1 : set of boolean
    type s2 : set of boolean
    var as1, as2 : s1
    var bs1, bs2 : s2
    var aas : set of boolean

    % should all produce boolean anyway
    var _v_res : boolean

    _v_res := r {0} b
    _v_res := r {0} c
    _v_res := r {0} c_sz
    _v_res := r {0} s
    _v_res := r {0} s_sz

    _v_res := i {0} b
    _v_res := i {0} c
    _v_res := i {0} c_sz
    _v_res := i {0} s
    _v_res := i {0} s_sz

    _v_res := n {0} b
    _v_res := n {0} c
    _v_res := n {0} c_sz
    _v_res := n {0} s
    _v_res := n {0} s_sz

    _v_res := b {0} r
    _v_res := b {0} i
    _v_res := b {0} n
    _v_res := b {0} c
    _v_res := b {0} c_sz
    _v_res := b {0} s
    _v_res := b {0} s_sz

    _v_res := c {0} r
    _v_res := c {0} i
    _v_res := c {0} n
    _v_res := c {0} b

    _v_res := c_sz {0} r
    _v_res := c_sz {0} i
    _v_res := c_sz {0} n
    _v_res := c_sz {0} b

    _v_res := s {0} r
    _v_res := s {0} i
    _v_res := s {0} n
    _v_res := s {0} b

    _v_res := s_sz {0} r
    _v_res := s_sz {0} i
    _v_res := s_sz {0} n
    _v_res := s_sz {0} b

    % Incompatible sets
    _v_res := as1 {0} bs2
    _v_res := bs1 {0} as2
    _v_res := aas {0} as1
    _v_res := as1 {0} aas
    "#
}

test_for_each_op! { comparison_op_wrong_types_enums,
    [
        ("<", less),
        (">", greater),
        ("<=", less_eq),
        (">=", greater_eq),
        ("=", equal),
        ("not=", not_equal),
    ] => r#"
    % Different enums
    type e1 : enum(v)
    type e2 : enum(v)
    var ae : e1
    var be : e2
    var aA : enum(v)
    var bA : enum(v)

    % should all produce boolean anyway
    var _v_res : boolean

    _v_res := ae {0} be
    _v_res := be {0} ae
    _v_res := aA {0} ae
    _v_res := ae {0} aA
    _v_res := aA {0} bA
    "#
}

test_for_each_op! { equality_op_scalars,
    [
        ("=", equal),
        ("not=", not_equal)
    ] => r#"
    % Other scalars
    var b : boolean

    % should all produce boolean
    var _v_res : boolean

    _v_res := b {0} b
    "#,
}

test_for_each_op! { equality_op_pointer,
    [
        ("=", equal),
        ("not=", not_equal)
    ] => r#"
    % Pointers
    var b : ^int

    % should all produce boolean
    var _v_res : boolean

    _v_res := b {0} b
    "#,
}

test_for_each_op! { equality_op_pointer_wrong_types,
    [
        ("=", equal),
        ("not=", not_equal)
    ] => r#"
    % Pointers
    var a : unchecked ^int
    var b : ^int
    var c : ^char
    var d : int

    % should all produce boolean
    var _v_res : boolean

    _v_res := a {0} b
    _v_res := c {0} b
    _v_res := d {0} b
    "#,
}

test_for_each_op! { set_ops,
    [
        ("+", add),
        ("-", sub),
        ("*", mul),
    ] => r#"
    type s : set of boolean
    var a, b : s

    % should all produce the same set
    var _v_res : s

    _v_res := a {0} b
    _v_res := b {0} a
    "#
}

test_for_each_op! { set_ops_wrong_types,
    [
        ("+", add),
        ("-", sub),
        ("*", mul),
    ] => r#"
    type s1 : set of boolean
    type s2 : set of boolean
    var a : s1
    var b : s2
    var i : int

    % should all produce the left set
    var _v_res : s1

    _v_res := a {0} b

    % error types
    _v_res := a {0} i
    _v_res := i {0} a
    "#
}

test_for_each_op! { set_member_op,
    [
        ("in", r#in),
        ("~in", not_in),
    ] => r#"
    type s : set of boolean
    var a : s
    var b : boolean

    % should all produce booleans
    var _v_res : boolean

    _v_res := b {0} a
    _v_res := true {0} a
    % FIXME: add tests for range types
    "#
}

test_for_each_op! { set_member_op_wrong_types,
    [
        ("in", r#in),
        ("~in", not_in),
    ] => r#"
    type ts : set of boolean
    var s : ts

    % should all produce booleans
    var _v_res : boolean

    % not a set
    _v_res := true {0} true
    % incompatible element types
    _v_res := 1 {0} s
    "#
}

test_named_group! { deref_op,
    [
        as_rhs => "
        var ptr : ^int
        var j := ^ptr
        ",
        as_lhs => "
        var ptr : ^int
        ^ptr := 2
        ",
        to_err => "
        type p : ^
        var ptr : p
        var j := ^ptr
        ",
        with_err => "
        var _ := ^()
        ",
        err_not_ptr => "
        var putty : int
        var j := ^putty
        ",
        err_wrong_value => "
        type ptr : ^int
        ^ptr
        ",
        // Mutability does not carry over
        from_const_ptr => "
        const ptr : ^int
        ^ptr := 2
        ",
    ]
}

// Test integer inference for all compatible operators
test_for_each_op! { integer_inference,
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

// Anything using `is_equivalent` and using type coercion needs to be added here
test_named_group! { do_type_coercion,
    [
        // These check if we are performing type coercion at all
        // The type coercion tests are somewhere else
        comparison_ops => r#"
        var i1 : int1
        var i : int
        var _res : boolean
        _res := i < i1
        "#,
        equality_ops => r#"
        var c1 : char(1)
        var c : char
        var _res : boolean
        _res := c = c1
        "#,
        for_bounds => r#"
        var i1 : int1
        var i : int
        var _res : boolean
        for : i .. i1 end for
        "#,
        // TODO: Uncomment once we allow coercion in case selectors
        /*
        case_selectors => r#"
        var i1 : int1
        const k : int := 1
        case i1 of
        label k:
        end case
        "#,
        */
    ]
}

test_named_group! { sized_char,
    [
        literal => r#"var _ : char(1)"#,
        // trip through negatives shouldn't affect anything
        simple_expr => r#"var _ : char(1 - 1 * 1 + 2)"#,
        indirect_expr => r#"
        const N := 1
        var _ : char(N)
        "#,
        zero_sized => r#"var _ : char(0)"#,
        max_sized => r#"var _ : char(32768)"#,
        wrong_type => r#"var _ : char(1.0)"#,
        wrong_type_bool => r#"var _ : char(true)"#,
        const_err => r#"var _ : char(1.0 div 0.0)"#,
        dyn_sized => "var N : int var _ : char(N)",
        // shouldn't die when reporting a dynamic char(N)
        err_dyn_sized => "var N : int var _ : char(N) := 1",
        // or when reporting a missing `N`
        err_missing => "var _ : char() := 1",
    ]
}

test_named_group! { sized_string,
    [
        literal => r#"var _ : string(1)"#,
        // trip through negatives shouldn't affect anything
        simple_expr => r#"var _ : string(1 - 1 * 1 + 2)"#,
        indirect_expr => r#"
        const N := 1
        var _ : string(N)
        "#,
        zero_sized => r#"var _ : string(0)"#,
        max_sized => r#"var _ : string(256)"#,
        over_sized => r#"var _ : string(512)"#,
        wrong_type => r#"var _ : string(1.0)"#,
        wrong_type_bool => r#"var _ : string(true)"#,
        const_err => r#"var _ : string(1.0 div 0.0)"#,
        dyn_sized => "var N : int var _ : string(N)"
    ]
}

test_named_group! { typeck_module_field,
    [
        normal => "
        module a
            export b
            var b : int
        end a
        var c := a.b",
        chained => "
        module a export b module b export c module c export d
            var d : int
        end c end b end a
        var k := a.b.c.d
        ",
        no_exports => "
        module no end no
        var k := no.exports
        ",
        not_exported => "
        module a export b var b : int end a
        var k := a.c
        ",
        export_mutability => "
        module a export var b
            var b : int
        end a
        a.b := 2",
        unqualified_export_mutability => "
        module a export ~. var b
            var b : int
        end a
        b := 2",
        export_not_mut => "
        module a export b, var c
            var b : int
            const c : int := 1
        end a
        a.b := 1
        a.c := 2
        ",
        unqualified_export_not_mut => "
        module a export ~.b
            var b : int
        end a
        b := 1
        ",
        unqualified_export_lhs_not_mut => "
        % crashed because we we weren't poking through resolution
        module indirection
            export ~. lhs
            module lhs
                export field
                var field : int
            end lhs
        end indirection

        % triggers `exporting_def` with an indirect lhs def
        lhs.field := 6
        "
    ]
}

test_named_group! { typeck_var,
    [
        compatible => r#"var k : int := 100"#,
        incompatible => r#"var k : char := 20"#,
        error_prop => r#"
            var k := 20 + false
            var l : int := k   % Nothing reported here
            "#,
        error_invalid_storage_ty_any_char => r#"var _ : char(*)"#,
        error_invalid_storage_ty_any_str => r#"var _ : string(*)"#,
    ]
}

test_named_group! { typeck_const,
    [
        compatible => r#"const k : int := 100"#,
        incompatible => r#"const k : char := 20"#,
        error_prop => r#"
            const k := 20 + false
            const l : int := k   % Nothing reported here
            "#,
        error_invalid_storage_ty_any_char => r#"const _ : char(*)"#,
        error_invalid_storage_ty_any_str => r#"const _ : string(*)"#,
    ]
}

test_named_group! { typeck_assignment,
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
        mismatched_types => r#"
            var lhs : int
            lhs := 1 + 1.0
        "#,
        lhs_not_mut_missing_rhs => r#"
            const j : int := 1
            j := 
        "#,
        lhs_not_value => r#"type k : int k := 1"#,
        rhs_not_value => r#"type j : int var k : int k := j"#
    ]
}

// For `is_coercible_into` uses, but in assignment position
test_named_group! { assignability_into,
    [
        boolean => r#"
        var b : boolean

        var _v00 : boolean := b
        "#,
        boolean_err => r#"
        var r : real
        var i : int
        var n : nat

        var _e00 : boolean := 1
        var _e01 : boolean := i
        var _e02 : boolean := n
        var _e03 : boolean := r
        "#,
        int => r#"
        var i : int
        var n : nat

        var _v00 : int := 1
        var _v01 : int := i
        var _v02 : int := n
        "#,
        int_err => r#"
        var b : boolean
        var r : real

        var _e00 : int := b
        var _e01 : int := r
        "#,
        nat => r#"
        var i : int
        var n : nat

        var _v00 : nat := 1
        var _v01 : nat := i
        var _v02 : nat := n
        "#,
        nat_err => r#"
        var b : boolean
        var r : real

        var _e00 : nat := b
        var _e01 : nat := r
        "#,
        real => r#"
        var r : real
        var i : int
        var n : nat

        var _v00 : real := 1
        var _v01 : real := i
        var _v02 : real := n
        var _v03 : real := r
        "#,
        real_err => r#"
        var b : boolean

        var _e00 : real := b
        "#,
        r#char => r#"
        var c : char
        var c1 : char(1)
        var s : string
        var s1 : string(1)

        var _v00 : char := c
        var _v01 : char := c1
        var _v02 : char := s1
        var _v03 : char := s % runtime checked
        "#,
        r#char_err => r#"
        var c5 : char(5)
        var s5 : string(5)

        var _e00 : char := s5
        var _e01 : char := c5 % [not captured by ctc]
        "#,
        r#char_1 => r#"
        const N := 1
        var c : char
        var c1 : char(1)
        var s : string
        var s1 : string(1)
        var s5 : string(5)

        var _v00 : char(N) := c
        var _v01 : char(N) := c1
        var _v02 : char(N) := s % runtime checked
        var _v03 : char(N) := s1
        var _v04 : char(N) := s5 % runtime checked
        "#,
        r#char_1_err => r#"
        const N := 1
        var c5 : char(5)

        var _e00 : char(N) := c5
        "#,
        r#char_3 => r#"
        const N := 3
        var c3 : char(3)
        var s : string
        var s3 : string(3)
        var s5 : string(5)

        var _v00 : char(N) := c3
        var _v01 : char(N) := s % runtime checked
        var _v02 : char(N) := s3
        var _v03 : char(N) := s5 % runtime checked
        "#,
        r#char_3_err => r#"
        const N := 3
        var c : char
        var c1 : char(1)
        var c5 : char(5)
        var s1 : string(1)

        var _e00 : char(N) := c
        var _e01 : char(N) := c1
        var _e02 : char(N) := c5
        var _e03 : char(N) := s1 % [not captured by ctc]
        "#,
        r#char_256 => r#"
        const N := 256
        var c256 : char(256)
        var s : string

        var _v00 : char(N) := c256
        var _v01 : char(N) := s % runtime checked, always fails
        "#,
        r#char_256_err => r#"
        const N := 256
        var c : char
        var c5 : char(5)
        var c257 : char(257)
        var s5 : string(5)

        var _e00 : char(N) := c
        var _e01 : char(N) := c5
        var _e01 : char(N) := c257
        var _e02 : char(N) := s5 % [not captured by ctc]
        "#,
        r#string => r#"
        var c : char
        var c1 : char(1)
        var c5 : char(5)
        var c255 : char(255)
        var s : string
        var s1 : string(1)
        var s5 : string(5)

        var _v00 : string := c
        var _v01 : string := c1
        var _v02 : string := c5
        var _v03 : string := c255
        var _v04 : string := s
        var _v05 : string := s1
        var _v06 : string := s5
        "#,
        r#string_err => r#"
        var cmx : char(256)

        var _e00 : string := cmx % [not captured by ctc]
        "#,
        r#string_1 => r#"
        const N := 1
        var c : char
        var c1 : char(1)
        var s : string
        var s1 : string(1)
        var s5 : string(5)

        var _v00 : string(N) := c
        var _v01 : string(N) := c1
        var _v02 : string(N) := s % runtime checked
        var _v03 : string(N) := s1
        var _v04 : string(N) := s5 % runtime checked
        "#,
        r#string_1_err => r#"
        const N := 1
        var c5 : char(5)

        var _e00 : string(N) := c5 % [not captured by ctc]
        "#,
        r#string_3 => r#"
        const N := 3
        var c : char
        var c1 : char(1)
        var c3 : char(3)
        var s : string
        var s1 : string(1)
        var s3 : string(3)
        var s5 : string(5)

        var _v00 : string(N) := c
        var _v01 : string(N) := c1
        var _v03 : string(N) := c3
        var _v05 : string(N) := s % runtime checked
        var _v02 : string(N) := s1
        var _v04 : string(N) := s3
        var _v06 : string(N) := s5 % runtime checked
        "#,
        r#string_3_err => r#"
        const N := 3
        var c5 : char(5)

        var _e00 : string(N) := c5 % [not captured by ctc]
        "#,
        r#string_255 => r#"
        const N := 255
        var c : char
        var c1 : char(1)
        var c255 : char(255)
        var s : string
        var s1 : string(1)
        var s255 : string(255)

        var _v00 : string(N) := c
        var _v01 : string(N) := c1
        var _v02 : string(N) := c255
        var _v03 : string(N) := s % runtime checked, always good
        var _v04 : string(N) := s1
        var _v05 : string(N) := s255
        "#,
        r#string_255_err => r#"
        const N := 255
        var c256 : char(256)

        var _e00 : string(N) := c256 % [not captured by ctc]
        "#,
        r#string_any_lhs => r#"
        proc __(var c_any : char(*), var s_any : string(*), var _v00, _v01, _v02, _v03, _v04, _v05, _v06, _v07, _v08 : string(*))
        % all runtime checked
        var c : char
        var c1 : char(1)
        var c5 : char(5)
        var c255 : char(255)
        var s : string
        var s1 : string(1)
        var s5 : string(5)

        _v00 := c
        _v01 := c1
        _v02 := c5
        _v03 := c255
        _v04 := c_any
        _v05 := s
        _v06 := s1
        _v07 := s5
        _v08 := s_any
        end __
        "#,
        r#string_err_lhs => r#"
        var cmx : char(256)

        proc __(var _e00 : string(*))
        _e00 := cmx % [not captured by ctc]
        end __
        "#,
        r#string_any_rhs => r#"
        % all runtime checked
        proc __(var s_any, _v09 : string(*), var c_any, _v05 : char(*))

        var _v00 : char := s_any
        var _v01 : char(1) := s_any
        var _v02 : char(5) := s_any
        var _v03 : char(255) := s_any
        var _v04 : char(256) := s_any
        _v05 := s_any
        var _v06 : string := s_any
        var _v07 : string(1) := s_any
        var _v08 : string(5) := s_any
        _v09 := s_any
        end __
        "#,
        r#char_any_lhs => r#"
        proc __(var c_any : char(*), var s_any : string(*), var _v00, _v01, _v02, _v03, _v04, _v05, _v06, _v07, _v08 : char(*))
        % all runtime checked
        var c : char
        var c1 : char(1)
        var c5 : char(5)
        var c255 : char(255)
        var s : string
        var s1 : string(1)
        var s5 : string(5)

        _v00 := c
        _v01 := c1
        _v02 := c5
        _v03 := c255
        _v04 := c_any
        _v05 := s
        _v06 := s1
        _v07 := s5
        _v08 := s_any
        end __
        "#,
        r#char_any_rhs => r#"
        % all runtime checked
        proc __(var s_any, _v09 : string(*), var c_any, _v05 : char(*))

        var _v00 : char := c_any
        var _v01 : char(1) := c_any
        var _v02 : char(5) := c_any
        var _v03 : char(255) := c_any
        var _v04 : char(256) := c_any
        _v05 := c_any
        var _v06 : string := c_any
        var _v07 : string(1) := c_any
        var _v08 : string(5) := c_any
        _v09 := c_any
        end __
        "#,
        constrained => "
        type c : 1 .. 2
        var a, b : c := 2
        var i : int
        var n : nat
        % can be coerced into the base type
        a := b
        a := 1
        a := i
        a := n
        % coercible into the base type
        i := a
        n := a
        ",
        constrained_set_elem => "
        type cs : set of 1 .. 2
        var i : int
        var n : int
        var _ : boolean

        var s : cs := cs(1, 2)
        _ := 1 in s
        _ := i in s
        _ := n in s
        ",
        // note: `constrained_array_range` and `array_param` also test that param-coercion
        // is applicable to both by-value and by-reference parameters
        constrained_array_range => "
        type tp : proc(var _ : array 1 .. * of int)
        var a : array 1 .. 3 of int
        var b : array 2 .. 3 of int
        var p : tp

        % only coercible if start bound is the same
        p(a) % success
        p(b) % fail
        ",
        array_param => "
        type tp: proc(_ : array 1..2 of char(*))
        var a : array 1..2 of char
        var b : array 1..2 of char(42)
        var p : tp

        % element type is also coercible
        p(a)
        p(b)
        ",
        dyn_array_param_err => "
        % dyn arrays aren't allowed, since the range is expected to be known at compile-time
        var c : int
        var a : array 1..c of char
        proc p(_ : array 1..2 of char(*)) end p

        p(a)
        ",
        // FIXME: add coercion tests for set member ops
    ]
}

test_named_group! { equivalence_of,
    [
        integer => r#"
        var n : nat
        var i : int
        var r : real

        for : 1 .. n end for
        for : 1 .. i end for
        for : 1 .. r end for
        for : n .. 1 end for
        for : i .. 1 end for
        for : r .. 1 end for
        "#,
        aliases => r#"
        type a0 : int
        type a1 : int
        var i : int
        var ia0 : a0
        var ia1 : a1

        % base type & alias
        for : i .. ia0 end for
        for : i .. ia1 end for
        for : ia0 .. i end for
        for : ia1 .. i end for
        % alias with same base type
        for : ia0 .. ia1 end for
        for : ia1 .. ia0 end for
        "#,
        // Over opaque types
        opaques => "
        module z
            export unqualified opaque ty, unwrap, make

            type ty : int

            fcn unwrap(t : ty) : int
                result t
            end unwrap

            fcn make(i : int) : ty
                result i
            end make
        end z

        % can't use path syntax since it isn't lowered yet
        var a, b : ty
        var c := z.make(1)

        % is equivalent to self
        a := b
        a := c

        % to the original alias def
        var i : int := z.unwrap(a)

        % but not the alias type
        i := a
        ",
        // Over enum types
        enums => "
        type e : enum(v)
        var a, b : e

        % compatible with itself
        a := b
        % with its own variants
        a := e.v

        % incompatible with different defs
        type f : enum(v)
        var c : f
        a := c
        a := f.v
        ",
        // Over constrained types
        constraineds => "
        % using array types as a proxy for constrained ty equivalence
        type r1 : 1..2
        type r2 : 3..4

        var a : array r1 of int
        var b : array r2 of int
        var c : array (1+1-1)..(4 div 2) of int

        % bounds must be equal
        a := b
        % if they evaluate to the same values, they're equivalent
        a := c

        var d : array 0..0 of int
        var e : array false..false of int

        % base types must be the same
        d := e
        ",
        // Over arrays
        // Note: `0+` is used to guarantee uniqueness of ranges
        arrays_elements => "
        % over array elements
        var a : array 0+1..1 of int
        var b : array 0+1..1 of int1
        var c : array 0+1..1 of int % trying to make unique types

        % element types must be equivalent
        a := b
        a := c
        ",
        arrays_ranges => "
        % over array ranges
        var a : array 0+1..1 of int
        var b : array 0+2..2 of int
        var c : array 0+1..1 of int

        % ranges must be equivalent
        a := b % not equivalent
        a := c % is equivalent

        var d : array 0+1..1, 0+2..2 of char
        var e : array 0+1..1 of char
        var f : array 0+1..1, 0+2..2 of char

        % must have the same number of ranges
        d := e
        d := f
        ",
        arrays_sizing => "
        % categories
        % - Static
        % - MaybeDyn-Static
        % - MaybeDyn-Dynamic
        % - Flexible
        % init-sized are implied Static

        % guarantee that these are static arrays
        type tsta : array 0+1..1 of int
        type tstb : array 0+1..1 of int
        var c : int

        var fxa : flexible array 0+1..1 of int
        var fxb : flexible array 0+1..1 of int
        var fxc : flexible array 0+1..2 of int
        var fxd : flexible array 0+1..1 of int1

        var dyna : array 0+1..c of int
        var dynb : array 0+1..c of int

        var msta : array 0+1..1 of int
        var mstb : array 0+1..1 of int

        var sta : tsta
        var stb : tstb

        % identity
        sta := stb
        msta := mstb

        % Static & MaybeDyn-Static are equivalent
        sta := msta
        msta := sta

        % MaybeDyn-Dynamic is never equivalent to anything (except itself)
        dyna := dyna % succeed
        dyna := dynb % fail

        % usual equivalence rules apply for flexible arrays...
        fxa := fxb % same ranges
        fxa := fxc % different ranges
        fxa := fxd % different elem tys

        % except that arrays must be of the same flexibility
        sta := fxa
        ",
        // Over set types
        sets => r#"
        type sb : set of boolean
        type sc : set of char
        type sc2 : set of char

        var v_sb : sb
        var v_sc : sc
        var v_sc2 : sc2
        var v_anon : set of char

        % compat
        v_sb := v_sb
        v_sc := v_sc
        v_sc2 := v_sc2

        % incompatible - different elem types
        v_sc := v_sb
        % incompatible - different def locations
        v_sc := v_sc2
        v_sc := v_anon

        % compatible through aliases
        type asc : sc
        var v_asc : asc
        v_sc := v_asc
        "#,
        // Over pointer types
        pointers => "
        type i : int
        type tpi : pointer to int
        type tpc : pointer to char
        type tpai : pointer to i
        type tupi : unchecked pointer to int

        var v_pi : tpi
        var v_pc : tpc
        var v_pai : tpai
        var v_upi : tupi

        % compat through aliases
        v_pi := v_pai

        % incompatible - different target types
        v_pi := v_pc

        % incompatible - different checkedness
        v_pi := v_upi
        ",
        // Over subprogram types
        subprogram_formals => r#"
        type t_p : procedure(a, b : int, var c : string)
        procedure p(a, b : int, var c : string) end p

        var _ : t_p := p
        "#,
        subprogram_formals_cheat => r#"
        type t_f : function(a, b : int, var c : string) : int
        type t_fc : function(a, b : cheat int, var c : cheat string) : int
        var f : t_f
        var fc : t_fc

        % transitive
        f := fc
        fc := f
        "#,
        subprogram_formals_register => r#"
        type t_f : function(a, b : int, var c : string) : int
        type t_fr : function(register a, b : int, var register c : string) : int
        var f : t_f
        var fc : t_fr

        % transitive
        f := fc
        fc := f
        "#,
        subprogram_formals_bare => r#"
        type t_f : function : int
        type t_p : procedure
        type t_fp : function() : int
        type t_pp : procedure()

        var f : t_f
        var p : t_p
        var fp : t_fp
        var pp : t_pp

        % transitive
        f := fp
        fp := f

        p := pp
        pp := p
        "#,
        subprogram_formals_err_too_few => r#"
        type t_p : procedure(a, b : int, var c : string)
        procedure p(a : int, var c : string) end p

        var _ : t_p := p
        "#,
        subprogram_formals_err_too_many => r#"
        type t_p : procedure(a, b : int, var c : string)
        procedure p(a, b, k : int, var c : string) end p

        var _ : t_p := p
        "#,
        subprogram_formals_err_not_var => r#"
        type t_p : procedure(a, b : int, var c : string)
        procedure p(a, b : int, c : string) end p

        var _ : t_p := p
        "#,
        subprogram_result => r#"
        type fa : function () : int
        function fb () : int end fb

        var _ : fa := fb
        "#,
        subprogram_result_err => r#"
        type fa : function () : int1
        function fb () : int end fb

        var _ : fa := fb
        "#,
    ]
}

test_named_group! { typeck_put_stmt,
    [
        normal_items => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var cn : char(4)
        var s : string
        var sn : string(4)
        type en: enum(a, b) var ef : en

        put i : 0
        put n : 0
        put r : 0
        put c : 0
        put cn : 0
        put s : 0
        put sn : 0
        put ef : 0
        "#,
        valid_extended_opts => r#"
        var i : int
        var n : nat
        var r : real

        put i : 0 : 0 : 0
        put n : 0 : 0 : 0
        put r : 0 : 0 : 0
        "#,
        valid_aliased_types => "
        type i : int
        var o, w, u : i
        put : o, w : o : u : o..
        ",
        invalid_extended_opts => r#"
        var c : char
        var cn : char(4)
        var s : string
        var sn : string(4)
        type en: enum(a, b) var ef : en

        put c : 0 : 0 : 0
        put cn : 0 : 0 : 0
        put s : 0 : 0 : 0
        put sn : 0 : 0 : 0
        put ef : 0 : 0 : 0
        "#,
        wrong_type_stream => r#"
        var s : real
        put : s,  1
        "#,
        wrong_type_width => r#"
        var w : real
        put 1 : w
        "#,
        wrong_type_fract => r#"
        var f : real
        put 1 : 0 : f
        "#,
        wrong_type_exp_width => r#"
        var e : real
        put 1 : 0 : 0 : e
        "#,
        wrong_type_only_steam => r#"put : 1.0"#,
        wrong_type_item => "
        type p : procedure
        var i : p
        put i
        ",
        wrong_value_stream => "
        type k : int
        put : k, 1
        ",
        wrong_value_item => "
        type k : int
        put k
        ",
        wrong_value_opt => "
        type o : int
        type w : int
        put 1 : o : w : o
        "
    ]
}

test_named_group! { typeck_get_stmt,
    [
        normal_items => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var b: boolean
        var cn : char(4)
        var s : string
        var sn : string(4)
        type en: enum(a, b) var ef : en

        get i
        get n
        get r
        get c
        get b
        get cn
        get s
        get sn
        get ef
        "#,
        valid_opts => r#"
        var cn : char(4)
        var s : string
        var sn : string(4)

        % chars
        get cn : 0
        get s : 0
        get sn : 0

        % lines
        get s : *
        get sn : *
        "#,
        valid_aliased_types => "
        type i : int
        type s : string
        var o : i
        var w : s
        get : o, w : o ..
        get w : * ..
        ",
        invalid_opts_chars => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var b: boolean
        type en: enum(a, b) var ef : en

        get i : 0
        get n : 0
        get r : 0
        get c : 0
        get b : 0
        get ef : 0
        "#,
        invalid_opts_lines => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var b: boolean
        var cn : char(4)
        type en: enum(a, b) var ef : en

        get i : *
        get n : *
        get r : *
        get c : *
        get b : *
        get cn : *
        get ef : *
        "#,
        wrong_type_stream => r#"
        var s : real
        get : s, skip
        "#,
        wrong_type_width => r#"
        var w : real
        var s : string
        get s : w
        "#,
        wrong_ref_const => r#"
        const i : int := 1
        get i
        "#,
        wrong_ref_value => r#"
        var i : int := 1
        get i + i
        "#,
        wrong_ref_literal => r#"
        get 1
        "#,
        wrong_type_only_stream => r#"get : 1.0"#,
        wrong_type_item => "
        type p : procedure
        var i : p
        get i
        ",
        wrong_value_stream => "
        type s : int
        var k : int
        get : s, k
        ",
        wrong_value_item => "
        type k : int
        get k
        ",
        wrong_value_opt => "
        type k : int
        var s : string
        get s : k
        "
    ]
}

test_named_group! { typeck_if,
    [
        matching_types => r#"
        if true then
        elsif true then
        endif
        "#,
        wrong_types => r#"
        if 1 then
        elsif 1.0 then
        elsif 'yee' then
        elsif "wahhh" then
        end if
        "#,
        wrong_value_condition => "
        type b : boolean
        if b then
        elsif b then
        end if"
    ]
}

test_named_group! { typeck_exit,
    [
        no_condition => r#"loop exit end loop"#,
        matching_types => r#"loop exit when true end loop"#,
        wrong_types => r#"loop exit when "gekdu" end loop"#,
        wrong_value_condition => "
        loop
            type b : boolean
            exit when b
        end loop
        "
    ]
}

test_named_group! { typeck_for,
    [
        infer_counter_ty => r#"
        for a : 1 .. 10
            var q : int := a
        end for
        "#,
        infer_concrete_counter_ty => r#"
        % Both should fail
        for c : 1.0 .. 1 var k : int := c end for
        for c : 1 .. 1.0 var k : int := c end for
        "#,
        infer_err_counter_ty => "
        % Both should fail
        for c : 1.0 .. () var k : int := c end for
        for c : () .. 1.0 var k : int := c end for
        ",
        infer_left_counter_ty => "
        % Biased towards left inference
        var i : int
        for c : i .. 1.0 var k : int := c end for
        ",
        normal_boolean_bounds => r#"for : false .. true end for"#,
        normal_char_bounds => r#"for : 'a' .. 'z' end for"#,
        normal_int_bounds => r#"for : 1 .. 10 end for"#,
        normal_enum_bounds => r#"type e : enum(a, b, c) for : e.a .. e.c end for"#,
        wrong_types_bounds_not_same => r#"for : 1 .. true end for"#,
        wrong_types_bounds_not_index => r#"for : "no" .. "yes" end for"#,
        // Specialize error message in case of {integer} combined with concrete type
        wrong_types_bounds_concrete_integer => r#"var r : real for : r .. 1 end for"#,
        wrong_types_bounds_integer_concrete => r#"var r : real for : 1 .. r end for"#,

        normal_step_by_ty => r#"for : false .. true by 2 end for"#,
        wrong_types_step_by => r#"for : false .. true by false end for"#,

        immut_counter => r#"for i : false .. true i := false end for"#,

        // Be resilient against missing bounds
        missing_left_bound => "for : .. 1 end for",
        missing_right_bound => "for : 1 .. end for",

        wrong_value_bounds => "
        type k : string
        for : k .. k end for
        for : 1 .. k end for
        for : k .. 1 end for
        ",
        wrong_value_by => "type k : int for : 1 .. 2 by k end for",

        // implicit bounds tests:
        // - not ty alias
        //   - is expr
        //   - other
        // - is ty alias
        //   - is constrained
        //   - is boolean
        //   - is char
        //   - is enum
        //   - is int (error, range too large)
        //   - other
        for_each_not_iterable => "for : 1 end for",
        for_each_unimpl => "
        var a : array 1 .. 2 of int
        for : a end for
        ",

        implied_bounds_boolean => "type b : boolean for _ : b end for",
        implied_bounds_char => "type b : char for _ : b end for",
        implied_bounds_enum => "type b : enum(a) for _ : b end for",
        implied_bounds_constrained => "type b : 1 .. 2 for _ : b end for",
        implied_bounds_int_err => "type b : int for _ : b end for",
        implied_bounds_real_err => "type b : real for _ : b end for",
        implied_not_ty => "module b end b /**/ for : b end for",
    ]
}

test_named_group! { typeck_case,
    [
        // contracts to test over:
        // - discriminant type
        normal_discriminant_ty => r#"
        case 1 of label 1: end case
        case 'c' of label 'c': end case
        case true of label true: end case
        case "a" of label "a": end case
        "#,
        wrong_discriminant_ty => r#"
        case 1.0 of label 1.0: end case
        case 'aa' of label 'aa': end case
        "#,

        // discriminant - selector equivalence
        normal_discrim_select_ty => r#"case 'c' of label 'c', 'd', 'e' end case"#,
        mismatch_discrim_select_ty => r#"case 'c' of label 123, 'dd', false end case"#,
        wrong_mismatch_discrim_select_ty => r#"case 1.0 of label 1, 'd', false end case"#,
        coerced_discrim_select_ty => r#"case 'c' of label "c": end case"#,
        wrong_coerced_discrim_select_ty => r#"case 'c' of label "cc", "": end case"#,

        // - selectors are compile-time evaluable
        comptime_selector_exprs => r#"
        case 1 of label 1 + 1 - 1: end case
        case true of label true and false: end case
        % TODO: uncomment once `chr` is a comptime operation
        %case 'E' of label chr(17 * 4 + 1): end case
        case "vee" of label "v" + "e" + 'e': end case
        "#,
        non_comptime_selector_expr => r#"
        var k : int
        case 1 of label k + 1: end case
        "#,

        // should handle aliases
        alias_discriminant_ty => "
        type c : char
        var d : c
        case c of label 'c': end case
        ",
        alias_selector_ty => "
        type c : char
        const k : c := 'c'
        case 'k' of label k: end case
        ",

        wrong_value_discriminant => "
        type c : char
        case c of label : end case
        ",
        wrong_value_selectors => "
        type c : char
        case 'k' of label c: end case
        ",
    ]
}

test_named_group! { typeck_type_alias,
    [
        normal => "type a : int var _ : a",
        chained => "
        type a : int
        type b : a
        type c : a",
        resolved_forward => "
        type fowo : forward
        type fowo : int
        var _ : fowo",
        // unresolved forwards can't even be used in type decls
        unresolved_forward => "
        type fowo : forward
        type a : fowo",
        as_expr => "type k : int var a := k",
        // only type bindings are accepted
        from_var => "var a : int type k : a",
        from_const => "const a : int type k : a",

        path_normal => "
        module a export b module b export c type c : int end b end a
        var d : a.b.c := 1
        ",
        path_no_fields => "
        module a export b type b : int end a
        var e : a.b.c := 1
        var f : a.nope := 1
        ",
        // only type bindings are accepted
        path_not_ty => "
        module a export b var b : int end a
        var e : a.b
        ",
        path_field_not_ty => "
        type e : enum(v)
        var a : e.v
        ",
        from_unresolved_def => "
        module _
            import tail
            type _t : tail
        end _
        ",
    ]
}

test_named_group! { test_enum_field,
    [
        // only associated with the type name
        on_type => "
        type e : enum(a, b, c)
        var _ := e.a
        ",
        // not on instances of the type
        on_var => "
        type e : enum(a, b, c)
        var a : e
        a := a.a
        ",
        not_variant => "
        type e : enum(a, b, c)
        var _ := e.e
        ",
    ]
}

test_named_group! { typeck_bind_decl,
    [
        normal => "
        begin
        var me : int
        bind us to me
        end",
        require_mut => "
        begin
        const me : int := 0
        bind var us to me
        end",
        // only storage bindings are accepted
        from_expr => "
        begin
        bind you to false
        end",
        from_register => "
        begin
        const register reg := 1
        bind you to reg, var me to reg
        end
        ",
        from_ty => "
        begin
        type no : int
        bind you to no
        end",
    ]
}

test_named_group! { typeck_subprog_decl,
    [
        sized_return => "function sha : int end sha",
        unsized_return_err => "function sha : char(*) end sha",
        named_res_use => "function sha() wa : int wa end sha",
        bare_named_res_use => "function sha wa : int wa end sha",

        wrong_type_dev_spec => "proc a : 1.0 end a",
        wrong_type_stack_size => "process a : 1.0 end a",

        wrong_value_dev_spec => "type k : real proc a : k end a",
        wrong_value_stack_size => "type k : real process a : k end a",
    ]
}

test_named_group! { typeck_subprog_ty,
    [
        from_function => "function sha(a : int, b : char) : int end sha",
        from_procedure => "procedure sha(a : int, b : char) end sha",
        from_process => "process sha(a : int, b : char) end sha",
        from_alias_function => "type sha : function (a : int, b : char) : int",
        from_alias_procedure => "type sha : procedure (a : int, b : char)",

        param_attrs => "type _ : procedure (
            ki : int,
            var v : int,
            register r : int,
            var register vr : int,
            ci : cheat int,
            var vci : cheat int,
            register rci : cheat int,
            var register vrci : cheat int
        )",
        bare_procedure => "type _ : procedure",
        bare_function => "function a : int end a",

        char_any_err => "type _ : function () : char(*)",
        string_any_err => "type _ : function () : string(*)",
    ]
}

test_named_group! { typeck_subprog_param,
    [
        infer_ty => "
        type tyres : string
        function own(me : nat, pie : real) sammy : tyres
        end own",
        infer_binding => "function ka(register a : int, b : int, var c : int) r : int
            bind
                ra to a, % should fail
                rb to b,
                rc to c
            r := 0 % should fail
        end ka"
    ]
}

test_named_group! { typeck_subprog_result_ty,
    [
        normal => "function tree : int end tree",
        char_any_err => "function _ : char(*) end _",
        string_any_err => "function _ : string(*) end _",
    ]
}

test_named_group! { typeck_subprog_call,
    [
        as_stmt => "
        function key() : int end key
        procedure lime() end lime
        key() lime()
        ",
        as_stmt_bare => "
        function key : int end key
        procedure lime end lime
        key lime
        key() lime()
        ",
        as_stmt_err => "
        function key() : int end key
        procedure lime() end lime
        % can't be used like this, need paren
        key lime
        ",
        as_expr => "
        function key() : int end key
        var res := key()
        ",
        as_expr_bare => "
        function key : int end key
        var res := key
        ",
        as_expr_err => "
        procedure lime() end lime
        var res := lime()
        ",
        // `process`es can never be called like this
        on_process_err => "
        process never end never
        never
        never()
        var _ := never()
        ",
        on_expr_err => "1 1(1, 2, 3)",
        on_type_err => "type kall : procedure() kall",
        on_var_err => "var sha : int sha()",
        on_undecl => "
        % pass through, since we can't say anything about this
        no_compile",

        args_exact => "
        var tree : int
        procedure boop(a, b : int, var c : int) end boop
        boop(1, 2, tree)
        ",
        args_err_cheat_ty => r#"
        var tree : string
        procedure boop(a, b : cheat int, var c : cheat int) end boop
        boop("tree", tree, tree)
        "#,
        args_err_wrong_binding => "
        procedure boop(a, b : int, var c : int) end boop
        boop(1, 2, 3)
        ",
        args_err_not_expr => "
        type a : int
        procedure boop(a, b : int, var c : int) end boop
        boop(1, a, a)
        ",
        args_err_wrong_ty => "
        var tree : string
        procedure boop(a, b : int, var c : int) end boop
        boop(1, 2, tree)
        ",
        args_err_few => "
        procedure boop(a, b, c : int) end boop
        boop(1, 2)
        boop(1)
        ",
        args_err_many => "
        procedure boop(a, b, c : int) end boop
        boop(1, 2, 3, 4)
        boop(1, 2, 3, 4, 5)
        ",
        args_err_missing => "
        procedure boop(a : int) end boop
        % shouldn't die, not reporting an error is fine
        boop(())
        ",

        // Coercion interactions
        coerce_value_arg => "
        procedure p(a : real) end p
        var i : int
        p(i)
        ",
        coerce_ref_arg_err => "
        procedure p(var a : real) end p
        var i : int
        p(i)
        ",
        coerce_ref_arg_cheated => "
        procedure p(var a : cheat real) end p
        var i : int
        p(i)
        ",
        coerce_ref_arg_char_any => "
        procedure p(var a : char(*)) end p
        var c : char
        var cs : char(6)
        p(c) p(cs)
        ",
        coerce_ref_arg_char_any_err => "
        procedure p(var a : char(*)) end p
        var c : string
        p(c)
        ",
        coerce_ref_arg_string_any => "
        procedure p(var a : string(*)) end p
        var c : string
        p(c)
        ",
        coerce_ref_arg_string_any_err => "
        procedure p(var a : string(*)) end p
        var c : char
        p(c)
        ",

        // Invalid argument kinds
        wrong_arg_range_err => "
        procedure k(var j : int) end p
        k(1 .. * - 2)
        ",
        wrong_arg_all_err => "
        procedure k(var j : int) end p
        k(all)
        ",
    ]
}

test_named_group! { typeck_import_decl,
    [
        // Should infer type correctly
        on_use => "
        var blah : int
        module _
            import blah
            var k := blah
        end _
        ",

        // handling resolution chains
        import_chain => "
        var outer : int
        module a
            import outer

            module b
                import outer

                module c
                    import outer

                    var k := outer
                end c
            end b

        end a
        ",

        as_const => "
        var outer : int
        module _
            import const outer
            outer := 6
        end _
        ",
        as_var => "
        var outer : int
        module _
            import var outer
            outer := 6
        end _
        ",
        as_var_on_unapplicable => "
        const outer : int
        module _
            import var outer
            outer := 6 % this should still be fine, since we take the specified mut for its word
        end _
        ",

        mutable_on_never_applicable => "
        type ty : int
        module _
            import var ty
            type tou : ty
        end _
        ",
        const_on_never_applicable => "
        type ty : int
        module _
            import const ty
            type tou : ty
        end _
        ",
        implied_on_never_applicable => "
        type ty : int
        module _
            import ty
            type tou : ty
        end _
        ",
    ]
}

test_named_group! { typeck_set_cons_call,
    [
        normal => "
        type s : set of boolean
        var _ := s(true, false, true, false)
        ",
        empty_set => "
        type s : set of boolean
        var _ := s()
        ",
        all => "
        type s : set of boolean
        var _ := s(all)
        ",
        err_just_itself => "
        type s : set of boolean
        var _ := s
        s
        ",
        err_as_stmt => "
        type s : set of boolean
        s()
        ",

        all_err_extra_args_before => "
        type s : set of boolean
        var b : boolean
        var _ := s(b, b, b, b, all)
        ",
        all_err_extra_args_after => "
        type s : set of boolean
        var b : boolean
        var _ := s(all, b, b, b, b)
        ",
        all_err_extra_args_between => "
        type s : set of boolean
        var b : boolean
        var _ := s(b, b, all, b, b)
        ",
        // For now, range exprs aren't supported
        all_err_extra_args_range => "
        type s : set of boolean
        var b : boolean
        var _ := s(1 .. 2, all, 3 .. 4)
        ",
        // end-relative ranges will never be supported
        all_err_extra_arg_end_range => "
        type s : set of boolean
        var _ := s(*, all, b .. * - b)
        ",
        all_err_wrong_ty => "
        type s : set of boolean
        var _ := s(all, 1)
        ",
        all_err_wrong_binding => "
        type s : set of boolean
        type b : boolean
        var _ := s(all, b)
        ",

        args_err_wrong_ty => "
        type s : set of boolean
        var _ := s(1, 2, 3)
        ",
        args_err_wrong_binding => "
        type s : set of boolean
        type b : boolean
        var _ := s(b)
        ",
        // can't construct from a `set` variable
        args_err_not_from_ty => "
        type s : set of boolean
        var k : s
        k := k()
        "

        // FIXME: Test set cons with range types once those are lowered
    ]
}

test_named_group! { typeck_array_indexing_call,
    [
        single_index => "
        var a : array 1..2 of int
        var _ : int := a(1)",
        multi_index => "
        var a : array 1..2, 'c'..'d' of int
        var _ : int := a(1, 'c')",

        err_extra_for_single => "
        var a : array 1..2 of int
        var _ := a(1,2,3,4)",
        err_extra_for_multiple => "
        var a : array 1..2, 3..4 of int
        var _ := a(1,2,3,4)",
        err_missing_for_single => "
        var a : array 1..2 of int
        var _ := a()",
        err_missing_for_multiple => "
        var a : array 1..2, 3..4 of int
        var _ := a(1)",

        err_wrong_ty => "
        var a : array 1..2, 'c'..'d' of int
        var _ : int := a(1.0, true)",
        err_wrong_binding => "
        type i : int
        var a : array 1..2 of int
        var _ : int := a(i)",

        // mutability is inherited
        from_const_ref => "
        const a : array 1..* of int := init(1)
        a(1) := 1",
        from_var_ref => "
        var a : array 1..* of int := init(1)
        a(1) := 1",
        err_from_type_binding => "
        type a : array 1..1 of int
        a(1) := 1",
    ]
}

test_named_group! { typeck_return_stmt,
    [
        in_top_level => "return",
        in_procedure => "proc q return end q",
        in_process => "proc q return end q",
        in_function_err => "fcn oeuf : int return end oeuf",
    ]
}

test_named_group! { typeck_result_stmt,
    [
        in_top_level_err => "result 0",
        in_procedure_err => "proc q result 0 end q",
        in_process_err => "proc q result 0 end q",
        in_function => "fcn oeuf : int result 0 end oeuf",
        mismatched_types => "fcn oeuf : int result 'egg' end oeuf",
        wrong_value_result => "fcn b : int type k : int result k end b",
    ]
}

test_named_group! { typeck_charn_ty,
    [
        any_size => "type _ : char(*)",
        // not supported yet
        runtime_size => "var k : int type _ : char(k)",
        wrong_size_too_small => "type _ : char(0)",
        wrong_size_too_big => "type _ : char(32768)",

        wrong_type_size => "type _ : char(1.0)",
        wrong_value_size => "type k : int type _ : char(k)",
    ]
}

test_named_group! { typeck_stringn_ty,
    [
        any_size => "type _ : string(*)",
        // not allowed for string
        runtime_size => "var k : int type _ : string(k)",
        wrong_size_too_small => "type _ : string(0)",
        wrong_size_too_big => "type _ : string(256)",

        wrong_type_size => "type _ : string(1.0)",
        wrong_value_size => "type k : int type _ : string(k)",
    ]
}

test_named_group! { typeck_opaque_ty,
    [
        // Outside module: gets inferred as an opaque type
        outside_module => "
        module k
            export opaque ~.* t, f

            type t : int

            fcn f() : t loop end loop end f
        end k

        var a : t
        var b := k.f()
        var c : k.t
        ",
        // inside module: also gets inferred as an opaque type
        inside_module => "
        module k
            export opaque t
            type t : int
            var a : t := 2
            var b : k.t := 3
        end k
        ",
        poke_opaque_constvar => "
        module m
            export opaque t, ~.* a
            type t : char

            var a : t := 'c'
        end m

        % Hidden type shouldn't be leaked
        var b := a
        b := 'c'
        ",
        poke_opaque_result => "
        module m
            export opaque ~.* t
            type t : int

            % should be equivalent
            fcn f() : t result 2 end f
        end m

        % should fail
        fcn k() : t result 2 end k
        ",
        // assignment is also handled by name
        poke_opaque_name => "
        module m
            export opaque t, a, ~.* b
            type t : int

            var a : t
            var b := a
            a := 2
        end m

        % It's fine if `b` leaks the hidden type
        var c : int := b
        ",
        poke_opaque_field => "
        module m
            export opaque t, var a, b
            type t : int

            var a : t
            m.a := 2
            var b := m.a
        end m

        % It's fine if `b` leaks the hidden type
        var c : int := m.b
        ",
        // FIXME: add inside arrow poke once records are lowered
        poke_opaque_deref => "
        module m
            export opaque t, a
            type t : int
            var a : ^t
            var b : int := ^a
        end m

        var b := m.a
        % should fail
        var c : int := ^b
        ",
        poke_opaque_call_parens => "
        module m
            export opaque t, f
            type t : int
            fcn f() : t loop end loop end f

            var a : int := f()
        end m

        % should fail
        var b : int := m.f()
        ",
        poke_opaque_call_no_parens => "
        module m
            export opaque t, f
            type t : int
            fcn f : t loop end loop end f

            var a : int := f
        end m

        % should fail
        var b : int := m.f
        ",
        poke_opaque_call_param => "
        module m
            export opaque t, a, p
            type t : int
            proc p(a : t) end p
            var a : t

            % should be coercible
            m.p(a)
            m.p(69)
        end m

        % should be fine
        var a := m.a
        m.p(a)
        % shouldn't be fine
        m.p(2)
        ",
        poke_opaque_set_cons_param => "
        module m
            export opaque t, ~.* s, make
            type t : char
            type s : set of t

            fcn make(c : char) : t
                result c
            end mk

            % both should succeed
            var a : s := s('6', '9')
            a := s(make('i'))
        end m

        % should fail
        var vs : s := s('i')
        % should succeed
        vs := s(m.make('i'))
        ",

        poke_opaque_set_elem_ty => "
        module m
            export opaque ~.* t, ~.* s, make
            type t : char
            type s : set of t
            fcn make(c : char) : t result c end make
        end m

        % both should fail, is strictly opaque
        type xs : set of t
        var vs : s := s('i')

        % should succeed
        var _ : boolean := m.make('i') in vs
        ",

        poke_opaque_for_bounds_explicit => "
        module m
            export opaque t, v
            type t : int
            var v : t
            for _ : v .. v end for
        end m

        for _ : m.v .. m.v end for
        ",
        poke_opaque_for_bounds_implicit => "
        module m
            export opaque t
            type t : 1 .. 2
            for _ : t end for
        end m

        for _ : m.t end for
        ",
    ]
}

test_named_group! { typeck_opaque_alias,
    [
        // Enums, sets, records, and unions deal with opaque aliases specially
        of_enum => "
        var *_ : boolean

        module m
            export opaque e, var a
            type e : enum(a)
            var a : e := e.a

            _ := e.a = a

            % should give a not usable as ty error
            type not_ty : e.a
        end m

        var u : m.e := m.e.a
        _ := m.e.a = u
        type no_field : m.e.a
        ",
        of_set => "
        var *_ : boolean

        module m
            export opaque s, var a
            type s : set of boolean
            var a := s(true)
            _ := true in a
        end m

        % should fail
        var vs := m.s(false)
        _ := false in m.a
        ",
    ]
}

test_named_group! { typeck_constrained_ty,
    [
        // Sized variation
        from_ints => "type c : 1 .. 1",
        from_chars => "type c : 'a' .. 'c'",
        from_booleans => "type c : false .. true",
        from_enums => "
        type e : enum(uwu, owo)
        type c : e.uwu .. e.owo",

        mismatched_tys => "type c : 1 .. true",
        missing_end => "type c : 1 .. ",
        missing_start => "type c : .. 2",
        missing_both => "type c : .. ",

        not_index_ty_both => "type _ : 1.0 .. 1.0",
        not_index_ty_l => "type _ : 1.0 .. 1",
        not_index_ty_r => "type _ : 1 .. 1.0",

        double_constrained => "
        const c : 1 .. 2 := 1
        type _ : c .. c",

        not_constexprs => "
        var c := 1
        type _ : c .. c",

        outside_ty_range_signed => "
        const min : int1 := -16#7F
        type _ : min .. 16#80
        ",
        outside_ty_range_unsigned => "
        const max : nat1 := 1
        type _ : -1 .. max
        ",

        in_err_msg => "var _ : 1 .. 2 := 'c'",
        in_err_msg_dyn_start => "var v : int const _ : v .. 2 := 'c'",
        in_err_msg_dyn_end => "var v : int const _ : 1 .. v := 'c'",
        in_err_msg_missing_start => "const _ : .. 2 := 'c'",
        in_err_msg_missing_end => "const _ : 1 ..  := 'c'",
        in_err_msg_missing_both => "const _ :   ..  := 'c'",

        // Unsized variation
        // FIXME: add tests for unsized range once arrays are being lowered
    ]
}

test_named_group! { typeck_array_ty,
    [
        static_size => "type _ : array 1 .. 3 of int",
        dynamic_size => "
        var c : int
        var _ : array 1 .. c of int",
        flexible_size => "var _ : flexible array 1 .. 0 of int",
        flexible_dynamic => "
        var c : int
        var _ : flexible array 1 .. c of int",
        init_sized => "var _ : array 1 .. * of int := init(1, 2, 3)",

        // not dyn
        not_dynamic_size => "
        var c : int
        type _ : array 1 .. c of int",

        // not index ty
        not_index_ty => "type _ : array real of real",
        // range sizing
        positive_static_size => "type _ : array 1..1 of int",
        zero_static_size => "type _ : array 1..0 of int",
        negative_static_size => "type _ : array 1..-1 of int",
        // Only flexible arrays allow zero-sized ranges
        positive_flexible_size => "var _ : flexible array 1..1 of int",
        zero_flexible_size => "type _ : flexible array 1..0 of int",
        negative_flexible_size => "type _ : flexible array 1..-1 of int",

        // array sizing
        sizing_int1 => "type _ : array int1, 1..2 of int",
        sizing_nat1 => "type _ : array nat1, 1..2 of int",
        sizing_int2 => "type _ : array int2, 1..2 of int",
        sizing_nat2 => "type _ : array nat2, 1..2 of int",
        sizing_int => "type _ : array int, 1..2 of int",
        sizing_nat => "type _ : array nat, 1..2 of int",
        // until we have 64-bit types or widen ints during computation, this will always be an overflow
        sizing_large => "type _ : array -16#7FFFFFFF..16#7FFFFFFF, 1..2 of int",
        sizing_overflow => "type _ : array 0..16#7FFFFFFF, 0..16#7FFFFFFF of int",
    ]
}

test_named_group! { typeck_array_ty_init,
    [
        on_flexible => "var a : flexible array 1..1 of int := init(1)",
        on_dynamic => "
        var c : int
        var a : array 1..c of int := init(1)",
        on_static => "var a : array 1..1 of int := init(1)",
        on_init_sized => "var a : array 1..* of int := init(1,2,3,4,5)",
        on_too_large => "
        const mx : nat4 := 16#FFFFFFFF
        var a : array 0..mx, 0..mx of int := init(1,2,3)",

        more_two => "var a : array 1..3 of int := init(1,2,3,4,5)",
        more_one => "var a : array 1..3 of int := init(1,2,3,4)",
        less_one => "var a : array 1..3 of int := init(1,2)",
        less_two => "var a : array 1..3 of int := init(1)",

        not_comp_time => "
        var c : int
        var a : array 1..1 of int := init(c)",
        mismatched_ty => "var a : array 1..1 of int := init(1.0)",
    ]
}

test_named_group! { typeck_enum_ty,
    [
        from_type_alias => "type e : enum(a, b, c)",
        from_var_decl => "var _ : enum(nice)",
        no_variants => "type e : enum()",
    ]
}

test_named_group! { typeck_set_ty,
    [
        from_type_alias => "type s : set of boolean var _ : s",
        from_var_decl => "var _ : set of boolean",
        valid_index_ty => "
        % FIXME: Add corresponding test for range types
        type r : char
        type e : enum(v)
        type _a : set of r
        type _b : set of boolean
        type _c : set of char
        type _d : set of e
        ",
        not_index_ty => "type _ : set of real",
        large_range_integer => "
        type bogos : int
        type _si : set of bogos
        type _sn : set of nat
        ",
    ]
}

test_named_group! { typeck_pointer_ty,
    [
        normal => "type _ : ^int",
        normal_unchecked => "type _ : unchecked ^int",
        nested => "type _ : unchecked ^ unchecked ^ int",
        to_err => "type _ : ^",
        // FIXME: Add test for unchecked collection types
    ]
}

test_named_group! { require_resolved_type,
    [
        in_type_decl => "type fowo : forward type _ : fowo",
        in_constvar => "type fowo : forward var _ : fowo",
    ]
}

test_named_group! { report_aliased_type,
    [
        in_inferred_ty => r#"
        type a0 : real
        type a1 : int
        var k : a0
        var i : a1 := k"#,
        in_assign => r#"
        type a0 : real
        type a1 : int
        var k : a0
        var i : a1
        i := k"#,
        in_binary_expr => r#"
        type a0 : real
        type a1 : int
        var k : a0
        var i : int
        i := i + k % unchanged"#,
        in_unary_expr => r#"
        type a0 : real
        var k : a0
        var _ := not k % unchanged
        "#,
        in_for => r#"
        type a0 : string
        var sa0 : a0
        var s : string

        for : sa0 .. sa0 end for
        for : s .. sa0 end for
        for : sa0 .. s end for

        for : 1 .. sa0 end for
        for : sa0 .. 1 end for
        "#,
        in_case => r#"
        type a0 : char
        type a1 : char(6)
        var ca0 : a0
        const cna1 : a1 := 'aaaaaa'

        case ca0 of
        label cna1:
        end case
        "#,
        in_subprog_ty => r#"
        type i : int
        type am_in : real
        type misery : char(*)
        type eat_em_up : string
        type f : function(c : i, p : am_in, r : misery) : eat_em_up

        var y : f
        var _ : int := y
        "#,
        // FIXME: add tests for the following types
        // - set
        // - record
        // - union
        // - array
    ]
}

test_named_group! { typeck_undecl_def,
    [
        // don't produce an error
        in_lvalue => "undecl := 1",
        in_rvalue => "var lvalue := undecl",
    ]
}

test_named_group! { peel_ref,
    [
        in_assign => r#"var a : int var k : int := a"#,
        in_inferred_ty => r#"var a : int var k := a"#,
        in_binary_expr => r#"var a : int var k := a + a"#,
        in_unary_expr => r#"var a : int var k := -a"#,
        in_put_stmt => r#"var a : int put a : 0 : 0 : 0"#,
        in_put_arg => r#"var a : int put a : a : a : a"#,
        in_get_arg => r#"var a : int var s : string get s : a"#,
        in_for_bound => r#"var a : int for : a .. a end for"#
    ]
}

test_named_group! { resolve_defs,
    [
        on_field_lookup => "
        module m
            export ~.* n
            module n
                export o, p
                var o : int
                type p : int
            end n
        end m

        put n.o
        var c : n.p
        ",
        on_undecl_field_lookup => "
        module base
            export ~. tail
        end base

        module target
            import tail
            tail.truce
        end target
        ",
        on_undecl_value_produced => "
        module _
            import undecl
            begin
                bind a to undecl
            end
        end _
        ",
    ]
}

test_named_group! { require_positive_size,
    [
        with_negative_size => "var _ : 2 .. 0",
        // Note: this test is partially overshadowed by range value bound tests
        with_overflowing_size => "
        const min : int4 := -16#7FFFFFFF - 1
        var _ : min..(16#7FFFFFFF + 1)
        ",

        in_constvar => "var _ : 1 .. 0",
        in_type_decl => "type _ : 1 .. 0",
        in_subprogram_param => "proc p(_:1..0) end p",
        in_subprogram_result => "fcn _() : 1..0 end _",
        in_subprogram_ty_param => "type _ : proc p(_:1..0)",
        in_subprogram_ty_result => "type _ : fcn _() : 1..0",
        // FIXME: Add positive size tests for the following items
        // - body decl (via headers)
        // - forward decl (via headers)
        // - external subprog decl (via headers)
        // - external var decl (via headers)

        in_set_ty => "type _ : set of 1..0",
        in_pointer_ty => "type _ : ^1..0",
        // FIXME: Add positive size tests for the following types
        // - collection type
        // - arrays (static)
        // - record field
        // - union field
        // - union tag
        // - collection type
    ]
}
