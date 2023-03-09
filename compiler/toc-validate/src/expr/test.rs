//! Expr validation tests
use crate::check;
use expect_test::expect;

#[test]
fn init_expr_in_const() {
    check("const uwu : array 1..2 of int := init(1)", expect![[]]);
}

#[test]
fn init_expr_in_var() {
    check("var uwu : array 1..2 of int := init(1)", expect![[]]);
}

#[test]
fn report_init_expr_not_in_const_var() {
    check(
        "var uwu : int uwu := init(1)",
        expect![[r#"
        error in file FileId(1) at 21..28: cannot use `init` expression here
        | error in file FileId(1) for 21..28: `init` expression can only be used as a `const` or `var` initializer"#]],
    );
}

#[test]
fn self_expr_in_class_subprogram() {
    check("class c proc p self end p end c", expect![[]]);
}

#[test]
fn self_expr_in_class_subprogram_inner() {
    check("class c proc p begin self end end p end c", expect![[]]);
}

#[test]
fn self_expr_in_class_subprogram_loop() {
    check("class c proc p loop self end loop end p end c", expect![[]]);
}

#[test]
fn report_self_expr_in_class_init() {
    check(
        "class c self end c",
        expect![[r#"
        error in file FileId(1) at 8..12: cannot use `self` here
        | error in file FileId(1) for 8..12: `self` cannot be used during class initialization
        | info: during class initialization, `self` does not refer to anything"#]],
    );
}

#[test]
fn report_self_expr_in_module_subprogram() {
    check(
        "module m proc p self end p end m",
        expect![[r#"
        error in file FileId(1) at 16..20: cannot use `self` here
        | error in file FileId(1) for 16..20: `self` is only allowed inside of class subprograms"#]],
    );
}

#[test]
fn report_self_expr_in_module_init() {
    check(
        "module m self end m",
        expect![[r#"
        error in file FileId(1) at 9..13: cannot use `self` here
        | error in file FileId(1) for 9..13: `self` is only allowed inside of class subprograms"#]],
    );
}

#[test]
fn report_self_expr_in_program_level() {
    check(
        "self",
        expect![[r#"
        error in file FileId(1) at 0..4: cannot use `self` here
        | error in file FileId(1) for 0..4: `self` is only allowed inside of class subprograms"#]],
    );
}
