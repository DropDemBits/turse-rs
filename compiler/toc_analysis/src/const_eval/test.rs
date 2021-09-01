use std::cell::RefCell;

use toc_ast_db::source::SourceParser;
use toc_hir::library::{LibraryId, LoweredLibrary};
use toc_hir_db::HirDatabase;
use toc_reporting::{MessageSink, ReportMessage};
use toc_salsa::salsa;
use toc_vfs::query::VfsDatabaseExt;
use unindent::unindent;

use crate::{const_eval::Const, db::ConstEval};

#[track_caller]
fn assert_const_eval(source: &str) {
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(source), source);
}

/// Expr version
#[track_caller]
fn assert_const_eval_expr(expr: &str) {
    let source = format!("const _ := {}", expr);
    insta::assert_snapshot!(insta::internals::AutoName, do_const_eval(&source), expr);
}

macro_rules! for_all_const_exprs {
    ($($src:literal)+) => {
        $(assert_const_eval_expr($src);)+
    };
}

fn do_const_eval(source: &str) -> String {
    let mut db = TestDb::default();
    let root_file = db.vfs.intern_path("src/main.t".into());
    db.update_file(root_file, Some(source.into()));

    let source_roots = toc_ast_db::source::SourceRoots::new(vec![root_file]);
    db.set_source_roots(source_roots);

    let library_id = db.library_graph().result().library_of(root_file);
    let library = db.library(library_id);

    // Eagerly evaluate all of the available const vars
    let const_evaluator = ConstEvaluator {
        db: &db,
        library_id,
        library: library.clone(),
        reporter: Default::default(),
        results: Default::default(),
    };

    toc_hir::visitor::preorder_visit_library(&library, &const_evaluator);

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
            let body = if let Some(body) = item.tail.init_expr() {
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
                Ok(v) => format!("{} -> {:?}\n", name, v),
                Err(err) => {
                    let text = format!("{} -> {:?}\n", name, err);
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
    stringify_const_eval_results(&results, &reporter.finish())
}

fn stringify_const_eval_results(results: &str, messages: &[ReportMessage]) -> String {
    // Pretty print const eval ctx
    // Want:
    // - Evaluation state of all accessible DefIds
    let mut s = format!("{}\n", results);

    // Pretty print the messages
    for err in messages.iter() {
        s.push_str(&format!("{}\n", err));
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
fn unsupported_values() {
    assert_const_eval(r#"const a := "alphabet""#);
    assert_const_eval(r#"const a := 'fun times'"#);
    assert_const_eval(r#"const a := 'e'"#);
}

#[test]
fn unsupported_ops() {
    for_all_const_exprs![
        "1 > 1"
        "1 >= 1"
        "1 < 1"
        "1 <= 1"
        "1 = 1"
        "1 ~= 1"
        "1 in 1"
        "1 ~in 1"
    ];
}
