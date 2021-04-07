//! Type check tests
use expect_test::expect;
use toc_reporting::ReportMessage;

use crate::ty::TyCtx;

fn check_typecheck(source: &str, expect: expect_test::Expect) {
    let parsed = toc_parser::parse(&source);
    let hir_res = toc_hir_lowering::lower_ast(parsed.syntax());
    let (ty_ctx, typeck_messages) = crate::typeck::typecheck_unit(&hir_res.unit);

    let s = stringify_typeck_results(&ty_ctx, &typeck_messages);
    expect.assert_eq(&s);
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
    check_typecheck(
        r#"var k : string"#,
        expect![[r#"
        ty_nodes:
            Idx::<Type>(0) TyRef(134229648 : String)
        def_kinds:
            DefId(0) Var(TyRef(134229648 : String))"#]],
    );
}

#[test]
fn var_decl_inference() {
    check_typecheck(
        r#"var k := "oeuf""#,
        expect![[r#"
        ty_nodes:
        def_kinds:
            DefId(0) Var(TyRef(134229648 : String))"#]],
    );
}

#[test]
fn var_decl_init_typecheck() {
    check_typecheck(
        r#"var k : string := "oeuf""#,
        expect![[r#"
        ty_nodes:
            Idx::<Type>(0) TyRef(134229648 : String)
        def_kinds:
            DefId(0) Var(TyRef(134229648 : String))"#]],
    );
}

#[test]
#[should_panic(expected = "Encountered bare ConstVar decl")]
fn bare_var_decl() {
    // Invariant, to be covered by the parser stage
    check_typecheck("var k", expect![[]]);
}
