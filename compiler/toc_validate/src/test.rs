//! Validated things
use super::check;
use expect_test::expect;

#[test]
fn report_dangling_else() {
    check(
        "else end if",
        expect![[r#"error at 0..4: found ‘else’ without matching ‘if’"#]],
    );
}

#[test]
fn report_dangling_elseif() {
    check(
        "elsif true then end if",
        expect![[r#"error at 0..5: found ‘elsif’ without matching ‘if’"#]],
    );
}

#[test]
fn report_dangling_pp_else() {
    check(
        "#else #end if",
        expect![[r##"error at 0..5: found ‘#else’ without matching ‘#if’"##]],
    );
}

#[test]
fn report_dangling_pp_elseif() {
    check(
        "#elsif Some then #end if",
        expect![[r##"error at 0..6: found ‘#elsif’ without matching ‘#if’"##]],
    );
}

#[test]
fn report_dangling_pp_endif() {
    check(
        "#end if",
        expect![[r##"error at 0..4: found ‘#end’ without matching ‘#if’"##]],
    );
}

#[test]
fn not_dangling_alternates() {
    check("if false then elsif false then else end if", expect![[]]);
}

#[test]
fn not_dangling_pp_alternates() {
    check("#if A then #elsif B then #else #end if", expect![[]]);
}

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
    // only reported for the first stmt after the decl
    check(
        "unit begin end begin end begin end",
        expect![[r#"
        error at 5..15: expected a module, class, or monitor declaration
        error at 15..25: found extra text after unit declaration"#]],
    );
}

#[test]
fn report_just_unit() {
    check(
        "unit",
        expect![[r#"error at 0..4: expected a module, class, or monitor declaration"#]],
    );
}

#[test]
fn report_monitor_class_with_dev_spec() {
    check(
        "monitor class a : 1 end a",
        expect![[r#"error at 16..20: device specification is not allowed for monitor classes"#]],
    );
}

#[test]
fn report_mismatched_module_names() {
    check(
        "module a end b",
        expect![[r#"error at 13..14: end identifier ‘b’ does not match ‘a’"#]],
    );
}

#[test]
fn report_mismatched_class_names() {
    check(
        "class a end b",
        expect![[r#"error at 12..13: end identifier ‘b’ does not match ‘a’"#]],
    );
}

#[test]
fn report_mismatched_monitor_names() {
    check(
        "monitor a end b",
        expect![[r#"error at 14..15: end identifier ‘b’ does not match ‘a’"#]],
    );
}

#[test]
fn only_missing_module_decl_name() {
    check(
        "monitor end b",
        expect![[r#"error at 8..11: expected identifier, but found ‘end’"#]],
    );
}

#[test]
fn only_missing_module_end_name() {
    check(
        "monitor a end",
        expect![[r#"error at 10..13: expected identifier"#]],
    );
}
