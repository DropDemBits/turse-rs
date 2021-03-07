//! Validated things
use crate::check;
use expect_test::expect;

#[test]
fn unit_module() {
    check("unit module a end a", expect![[]]);
}

#[test]
fn unit_class() {
    check("unit class a end a", expect![[]]);
}

#[test]
fn unit_monitor() {
    check("unit monitor a end a", expect![[]]);
}

#[test]
fn unit_monitor_class() {
    check("unit monitor class a end a", expect![[]]);
}

#[test]
fn report_unit_not_allowed_decl() {
    check(
        "unit begin end",
        expect![[r#"error at 5..14: expected a module, class, or monitor declaration"#]],
    );
}

#[test]
fn report_unit_not_allowed_decl_and_extra() {
    // reported for all text after the decl
    check(
        "unit begin end begin end begin end",
        expect![[r#"
            error at 5..15: expected a module, class, or monitor declaration
            error at 15..34: found extra text after unit declaration"#]],
    );
}

#[test]
fn report_just_unit() {
    check(
        "unit",
        expect![[r#"error at 0..4: expected a module, class, or monitor declaration"#]],
    );
}
