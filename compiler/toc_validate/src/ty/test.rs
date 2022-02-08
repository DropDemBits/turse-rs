//! Stmt & Decl validation tests
use crate::check;
use expect_test::expect;

#[test]
fn report_function_type_parameterless_named() {
    check(
        "type _ : function bare_with_me : int",
        expect![[r#"
        warn in file FileId(1) at 9..36: parameterless function types should specify `()`
        | warn in file FileId(1) for 18..30: add `()` after here
        | info: referring to a parameterless function will call it instead of producing a reference to it"#]],
    );
}

#[test]
fn report_function_type_parameterless_unnamed() {
    check(
        "type _ : function : int",
        expect![[r#"
        warn in file FileId(1) at 9..23: parameterless function types should specify `()`
        | warn in file FileId(1) for 9..17: add `()` after here
        | info: referring to a parameterless function will call it instead of producing a reference to it"#]],
    );
}
