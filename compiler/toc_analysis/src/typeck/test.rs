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
                Idx::<Type>(0) TyRef(String)
            def_kinds:
                DefId(0) Var(TyRef(String))"#]],
    );
}

#[test]
fn var_decl_inference() {
    check_typecheck(
        r#"var k := "oeuf""#,
        expect![[r#"
            ty_nodes:
            def_kinds:
                DefId(0) Var(TyRef(String))"#]],
    );
}

#[test]
fn var_decl_init_typecheck() {
    check_typecheck(
        r#"var k : string := "oeuf""#,
        expect![[r#"
            ty_nodes:
                Idx::<Type>(0) TyRef(String)
            def_kinds:
                DefId(0) Var(TyRef(String))"#]],
    );
}

#[test]
#[should_panic(expected = "Encountered bare ConstVar decl")]
fn bare_var_decl() {
    // Invariant, to be covered by the parser stage
    check_typecheck("var k", expect![[]]);
}

#[test]
fn undeclared_symbol() {
    check_typecheck(
        "var a := b",
        expect![[r#"
        ty_nodes:
        def_kinds:
            DefId(0) Error(TyRef(Error))
            DefId(1) Var(TyRef(Error))
        error at 9..10: `b` is not declared"#]],
    );
    // No cyclic deps
    check_typecheck(
        "var a := a",
        expect![[r#"
        ty_nodes:
        def_kinds:
            DefId(0) Error(TyRef(Error))
            DefId(1) Var(TyRef(Error))
        error at 9..10: `a` is not declared"#]],
    );
}

#[test]
fn add_op_typecheck() {
    check_typecheck(
        "var a : int\nvar b : int\nvar c := a + b",
        expect![[r#"
        ty_nodes:
            Idx::<Type>(0) TyRef(Int(Int))
            Idx::<Type>(1) TyRef(Int(Int))
        def_kinds:
            DefId(0) Var(TyRef(Int(Int)))
            DefId(1) Var(TyRef(Int(Int)))
            DefId(2) Var(TyRef(Int(Int)))"#]],
    );
    check_typecheck(
        "var a : int\nvar b : string\nvar c := a + b",
        expect![[r#"
            ty_nodes:
                Idx::<Type>(0) TyRef(Int(Int))
                Idx::<Type>(1) TyRef(String)
            def_kinds:
                DefId(0) Var(TyRef(Int(Int)))
                DefId(1) Var(TyRef(String))
                DefId(2) Var(TyRef(Error))
            error at 38..39: Incompatible types for addition"#]],
    );
}

#[test]
fn integer_inferrence() {
    // Should be the same for all arithmetic operators
    let operators = vec![
        (
            "+",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
        (
            "-",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
        (
            "*",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
        (
            "div",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
        (
            "/",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Real(Real)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Real(Real)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Real(Real)))"#]],
        ),
        (
            "mod",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
        (
            "rem",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
        (
            "**",
            expect![[r#"
                ty_nodes:
                def_kinds:
                    DefId(0) Var(TyRef(Int(Int)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
            expect![[r#"
                ty_nodes:
                    Idx::<Type>(0) TyRef(Nat(Nat))
                def_kinds:
                    DefId(0) Var(TyRef(Nat(Nat)))
                    DefId(1) Var(TyRef(Nat(Nat)))"#]],
        ),
    ];

    for (op, a, b, c) in operators.into_iter() {
        // Inferred integer types should pass
        // Decl should be a concrete type
        check_typecheck(&format!("var a := 1 {} 1", op), a);
        // Types of operands should make the type concrete
        check_typecheck(&format!("var k : nat\nvar a := 1 {} k", op), b);
        check_typecheck(&format!("var k : nat\nvar a := k {} 1", op), c);
    }
}

#[test]
fn typecheck_error_prop() {
    // Only one error should be reported, propogated error supresses the rest
    check_typecheck(
        "var a : int\nvar b : string\nvar c := a + b\nvar j := c + a",
        expect![[r#"
            ty_nodes:
                Idx::<Type>(0) TyRef(Int(Int))
                Idx::<Type>(1) TyRef(String)
            def_kinds:
                DefId(0) Var(TyRef(Int(Int)))
                DefId(1) Var(TyRef(String))
                DefId(2) Var(TyRef(Error))
                DefId(3) Var(TyRef(Error))
            error at 38..39: Incompatible types for addition"#]],
    );
}

// TODO: Add missing typecheck tests for the following binary operations
// - Sub
// - Mul
// - IntDiv
// - RealDiv
// - Mod
// - Rem
// - Exp
