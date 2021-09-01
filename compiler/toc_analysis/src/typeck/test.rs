//! Type check tests

use toc_ast_db::source::SourceParser;
use toc_hir::symbol::DefId;
use toc_hir_db::HirDatabase;
use toc_reporting::ReportMessage;
use toc_salsa::salsa;
use toc_vfs::query::VfsDatabaseExt;
use unindent::unindent;

use crate::db::TypeDatabase;
use crate::HirAnalysis;

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
    let mut db = TestDb::default();
    let root_file = db.vfs.intern_path("src/main.t".into());
    db.update_file(root_file, Some(source.into()));

    let source_roots = toc_ast_db::source::SourceRoots::new(vec![root_file]);
    db.set_source_roots(source_roots);

    let lib = db.library_graph().result().library_of(root_file);
    let typeck_res = db.typecheck_library(lib);

    stringify_typeck_results(&db, lib, typeck_res.messages())
}

fn stringify_typeck_results(
    db: &TestDb,
    lib: toc_hir::library::LibraryId,
    messages: &[ReportMessage],
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
        s.push_str(&db.debug_ty(ty));
        s.push('\n');
    }

    // Pretty print the messages
    for err in messages.iter() {
        s.push_str(&format!("\n{}", err));
    }

    s
}

#[salsa::database(
    toc_vfs::query::FileSystemStorage,
    toc_ast_db::source::SourceParserStorage,
    toc_hir_db::HirDatabaseStorage,
    crate::db::TypeInternStorage,
    crate::db::TypeDatabaseStorage,
    crate::db::ConstEvalStorage,
    crate::HirAnalysisStorage
)]
#[derive(Default)]
struct TestDb {
    storage: salsa::Storage<Self>,
    vfs: toc_vfs::Vfs,
}

impl salsa::Database for TestDb {}

impl toc_vfs::HasVfs for TestDb {
    fn get_vfs(&self) -> &toc_vfs::Vfs {
        &self.vfs
    }
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
    // Invariant, to be covered by the parser & hir stage
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
        var _v02 : char(N) := s1
        var _v03 : char(N) := s % runtime checked
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
        var s3 : string(3)
        var s : string
        var s5 : string(5)

        var _v00 : char(N) := c3
        var _v01 : char(N) := s3
        var _v02 : char(N) := s % runtime checked
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
        var s1 : string(1)
        var c5 : char(5)
        var c255 : char(255)
        var s : string
        var s5 : string(5)

        var _v00 : string := c
        var _v01 : string := c1
        var _v02 : string := c5
        var _v05 : string := c255
        var _v03 : string := s1
        var _v04 : string := s5
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
        var _v02 : string(N) := s1
        var _v03 : string(N) := s % runtime checked
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
        var _v02 : string(N) := s1
        var _v03 : string(N) := c3
        var _v04 : string(N) := s3
        var _v05 : string(N) := s % runtime checked
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
        var _v02 : string(N) := s1
        var _v03 : string(N) := c255
        var _v04 : string(N) := s255
        var _v05 : string(N) := s % runtime checked, always good
        "#,
        r#string_255_err => r#"
        const N := 255
        var c256 : char(256)

        var _e00 : string(N) := c256 % [not captured by ctc]
        "#,
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
        // TODO: Add test for non-put-able items once non-primitive types are lowered
    ]
}

test_named_group! { typeck_get_stmt,
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

        get i : 0
        get n : 0
        get r : 0
        get c : 0
        get cn : 0
        get s : 0
        get sn : 0
        %get ef : 0
        "#,
        wrong_type_stream => r#"
        var s : real
        get : s, skip
        "#,
        wrong_type_width => r#"
        var w : real
        var i : int
        get i : w
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
        // TODO: Add test for non-get-able items once non-primitive types are lowered
    ]
}
