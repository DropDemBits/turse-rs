//! Preproc validation tests
use crate::check;
use expect_test::expect;

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
