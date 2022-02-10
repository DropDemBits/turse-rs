//! Type check tests

use toc_hir::symbol::DefId;
use toc_hir_db::db::HirDatabase;
use toc_reporting::MessageBundle;
use unindent::unindent;

use crate::db::HirAnalysis;
use crate::db::TypeDatabase;
use crate::test_db::TestDb;

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
    let (db, lib) = TestDb::from_source(source);
    let typeck_res = db.typecheck_library(lib);

    stringify_typeck_results(&db, lib, typeck_res.messages())
}

fn stringify_typeck_results(
    db: &TestDb,
    lib: toc_hir::library::LibraryId,
    messages: &MessageBundle,
) -> String {
    let mut s = String::new();
    // Pretty print typectx
    // Want: `type_of` all reachable DefIds
    // - Printed type nodes because we wanted to see the full type
    let library = db.library(lib);
    for did in library.local_defs() {
        let def_info = library.local_def(did);
        let name = def_info.name.item();
        let name_span = library.lookup_span(def_info.name.span());
        let ty = db.type_of(DefId(lib, did).into());
        let name_fmt = format!("{:?}@{:?} [{:?}]: ", name, name_span, def_info.kind);

        s.push_str(&name_fmt);
        s.push_str(&format!("{:?}", ty.in_db(db)));
        s.push('\n');
    }

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
    assert_typecheck(r#"var k := 'oe'"#);
    assert_typecheck(r#"var k := 'o'"#);
}

#[test]
fn var_decl_init_typecheck() {
    assert_typecheck(r#"var k : string := "oeuf""#);
}

#[test]
fn bare_var_decl() {
    assert_typecheck("var k k := 3");
    // should yell about this
    assert_typecheck("const k k := 3");
}

#[test]
fn typecheck_error_prop() {
    // Only one error should be reported, propagated error suppresses the rest
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
fn typecheck_missing_exprs() {
    // Be resilient against missing expressions
    assert_typecheck("const ke : int := ()");

    assert_typecheck("get ()");

    assert_typecheck("begin var aaaa : int bind aaaa to end");
}

#[test]
fn block_stmt_check() {
    // Check inside block stmts
    assert_typecheck(r#"begin var k : char := 'baz' end"#);
}

// Typecheck basic ops
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
    var c : char
    var c_dyn : char(*)
    var c_sz : char(6)
    var s : string
    var s_dyn : string(*)
    var s_sz : string(6)

    % special cases first
    var _t00 := c {0} c
    var _t02 := c {0} c_sz
    var _t20 := c_sz {0} c
    var _t22 := c_sz {0} c_sz

    % the rest of them down here (should produce string)
    var _t01 := c {0} c_dyn
    var _t03 := c {0} s
    var _t04 := c {0} s_dyn
    var _t05 := c {0} s_sz

    var _t10 := c_dyn {0} c
    var _t30 := s {0} c
    var _t40 := s_dyn {0} c
    var _t50 := s_sz {0} c

    var _t11 := c_dyn {0} c_dyn
    var _t12 := c_dyn {0} c_sz
    var _t13 := c_dyn {0} s
    var _t14 := c_dyn {0} s_dyn
    var _t15 := c_dyn {0} s_sz

    var _t21 := c_sz {0} c_dyn
    var _t31 := s {0} c_dyn
    var _t41 := s_dyn {0} c_dyn
    var _t51 := s_sz {0} c_dyn

    var _t23 := c_sz {0} s
    var _t24 := c_sz {0} s_dyn
    var _t25 := c_sz {0} s_sz

    var _t32 := s {0} c_sz
    var _t42 := s_dyn {0} c_sz
    var _t52 := s_sz {0} c_sz

    var _t33 := s {0} s
    var _t34 := s {0} s_dyn
    var _t35 := s {0} s_sz

    var _t43 := s_dyn {0} s
    var _t53 := s_sz {0} s

    var _t44 := s_dyn {0} s_dyn
    var _t45 := s_dyn {0} s_sz

    var _t54 := s_sz {0} s_dyn

    var _t55 := s_sz {0} s_sz
    "#
}

test_for_each_op! { string_manip_op_wrong_type,
    [("+", concat)] => r#"
    var c : char
    var c_dyn : char(*)
    var c_sz : char(6)
    var s : string
    var s_dyn : string(*)
    var s_sz : string(6)
    var i : int

    var _e00 := c {0} i
    var _e01 := c_dyn {0} i
    var _e02 := c_sz {0} i
    var _e03 := s {0} i
    var _e04 := s_dyn {0} i
    var _e05 := s_sz {0} i

    var _e10 := i {0} c
    var _e11 := i {0} c_dyn
    var _e12 := i {0} c_sz
    var _e13 := i {0} s
    var _e14 := i {0} s_dyn
    var _e15 := i {0} s_sz

    % TODO: Uncomment to verify incompatible types
    /*
    var _e20 : char(13) := c_sz {0} c_sz
    var _e21 : char(8) := c_sz {0} c
    var _e22 : char(8) := c {0} c_sz
    var _e23 : char(3) := c {0} c
    var _e24 : char := c {0} c
    */
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
    "#
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
        "#
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
    ]
}

test_named_group! { typeck_var,
    [
        compatible => r#"var k : int := 100"#,
        incompatible => r#"var k : char := 20"#,
        error_prop => r#"
            var k := 20 + false
            var l : int := k   % Nothing reported here
            "#
    ]
}

test_named_group! { typeck_const,
    [
        compatible => r#"const k : int := 100"#,
        incompatible => r#"const k : char := 20"#,
        error_prop => r#"
            const k := 20 + false
            const l : int := k   % Nothing reported here
            "#
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
        "#
    ]
}

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
        r#string_dyn_lhs => r#"
        % all runtime checked
        var c : char
        var c1 : char(1)
        var c5 : char(5)
        var c255 : char(255)
        var c_dyn : char(*)
        var s : string
        var s1 : string(1)
        var s5 : string(5)
        var s_dyn : string(*)

        var _v00 : string(*) := c
        var _v01 : string(*) := c1
        var _v02 : string(*) := c5
        var _v03 : string(*) := c255
        var _v04 : string(*) := c_dyn
        var _v05 : string(*) := s
        var _v06 : string(*) := s1
        var _v07 : string(*) := s5
        var _v08 : string(*) := s_dyn
        "#,
        r#string_err_lhs => r#"
        var cmx : char(256)

        var _e00 : string(*) := cmx % [not captured by ctc]
        "#,
        r#string_dyn_rhs => r#"
        % all runtime checked
        var s_dyn : string(*)

        var _v00 : char := s_dyn
        var _v01 : char(1) := s_dyn
        var _v02 : char(5) := s_dyn
        var _v03 : char(255) := s_dyn
        var _v04 : char(256) := s_dyn
        var _v05 : char(*) := s_dyn
        var _v06 : string := s_dyn
        var _v07 : string(1) := s_dyn
        var _v08 : string(5) := s_dyn
        var _v09 : string(*) := s_dyn
        "#,
        r#char_dyn_lhs => r#"
        % all runtime checked
        var c : char
        var c1 : char(1)
        var c5 : char(5)
        var c255 : char(255)
        var c_dyn : char(*)
        var s : string
        var s1 : string(1)
        var s5 : string(5)
        var s_dyn : string(*)

        var _v00 : char(*) := c
        var _v01 : char(*) := c1
        var _v02 : char(*) := c5
        var _v03 : char(*) := c255
        var _v04 : char(*) := c_dyn
        var _v05 : char(*) := s
        var _v06 : char(*) := s1
        var _v07 : char(*) := s5
        var _v08 : char(*) := s_dyn
        "#,
        r#char_dyn_rhs => r#"
        % all runtime checked
        var c_dyn : string(*)

        var _v00 : char := c_dyn
        var _v01 : char(1) := c_dyn
        var _v02 : char(5) := c_dyn
        var _v03 : char(255) := c_dyn
        var _v04 : char(256) := c_dyn
        var _v05 : char(*) := c_dyn
        var _v06 : string := c_dyn
        var _v07 : string(1) := c_dyn
        var _v08 : string(5) := c_dyn
        var _v09 : string(*) := c_dyn
        "#,
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
        "#
    ]
}

test_named_group! { typeck_put_stmt,
    [
        normal_items => r#"
        % TODO: Uncomment enum lines once enum types are lowered & checked
        var i : int
        var n : nat
        var r : real
        var c : char
        var cn : char(4)
        var s : string
        var sn : string(4)
        %type en: enum(a, b) var ef : en

        put i : 0
        put n : 0
        put r : 0
        put c : 0
        put cn : 0
        put s : 0
        put sn : 0
        %put ef : 0
        "#,
        valid_extended_opts => r#"
        var i : int
        var n : nat
        var r : real

        put i : 0 : 0 : 0
        put n : 0 : 0 : 0
        put r : 0 : 0 : 0
        "#,
        invalid_extended_opts => r#"
        % TODO: Uncomment enum lines once enum types are lowered & checked
        var c : char
        var cn : char(4)
        var s : string
        var sn : string(4)
        %type en: enum(a, b) var ef : en

        put c : 0 : 0 : 0
        put cn : 0 : 0 : 0
        put s : 0 : 0 : 0
        put sn : 0 : 0 : 0
        %put ef : 0 : 0 : 0
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
        wrong_type_only_steam => r#"put : 1.0"#
        // TODO: Add test for non-put-able items once non-primitive types are lowered
    ]
}

test_named_group! { typeck_get_stmt,
    [
        // TODO: Uncomment enum lines once enum types are lowered & checked
        normal_items => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var b: boolean
        var cn : char(4)
        var s : string
        var sn : string(4)
        %type en: enum(a, b) var ef : en

        get i
        get n
        get r
        get c
        get b
        get cn
        get s
        get sn
        %get ef
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
        invalid_opts_chars => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var b: boolean
        %type en: enum(a, b) var ef : en

        get i : 0
        get n : 0
        get r : 0
        get c : 0
        get b : 0
        %get ef : 0
        "#,
        invalid_opts_lines => r#"
        var i : int
        var n : nat
        var r : real
        var c : char
        var b: boolean
        var cn : char(4)
        %type en: enum(a, b) var ef : en

        get i : *
        get n : *
        get r : *
        get c : *
        get b : *
        get cn : *
        %get ef : *
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
        wrong_type_only_stream => r#"get : 1.0"#
        // TODO: Add test for non-get-able items once non-primitive types are lowered
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
        "#
    ]
}

test_named_group! {
    typeck_exit,
    [
        no_condition => r#"loop exit end loop"#,
        matching_types => r#"loop exit when true end loop"#,
        wrong_types => r#"loop exit when "gekdu" end loop"#,
    ]
}

test_named_group! {
    typeck_for,
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
        normal_boolean_bounds => r#"for : false .. true end for"#,
        normal_char_bounds => r#"for : 'a' .. 'z' end for"#,
        normal_int_bounds => r#"for : 1 .. 10 end for"#,
        // TODO: Uncomment once enum types are lowered
        //normal_enum_bounds => r#"type e : enum(a, b, c) for : e.a .. e.c end for"#,
        wrong_types_bounds_not_same => r#"for : 1 .. true end for"#,
        wrong_types_bounds_not_index => r#"for : "no" .. "yes" end for"#,
        // Specialize error message in case of {integer} combined with concrete type
        wrong_types_bounds_concrete_integer => r#"var r : real for : r .. 1 end for"#,
        wrong_types_bounds_integer_concrete => r#"var r : real for : 1 .. r end for"#,

        normal_step_by_ty => r#"for : false .. true by 2 end for"#,
        wrong_types_step_by => r#"for : false .. true by false end for"#,

        immut_counter => r#"for i : false .. true i := false end for"#,

        unsupported_implicit_bounds => r#"var implied : int for : implied end for"#,

        // Be resilient against missing bounds
        missing_left_bound => "for : 1 .. end for",
        missing_right_bound => "for : 1 .. end for",
    ]
}

test_named_group! {
    typeck_case,
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
        "#
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
    ]
}

test_named_group! {
    typeck_subprog_param,
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
