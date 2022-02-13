//! Preproc validation tests
use crate::check;
use expect_test::expect;

#[test]
fn report_dangling_pp_else() {
    check(
        "#else #end if",
        expect![[r##"
            error in file FileId(1) at 0..5: found dangling `#else`
            | error in file FileId(1) for 0..5: this `#else` does not have a matching `#if`"##]],
    );
}

#[test]
fn report_dangling_pp_elseif() {
    check(
        "#elsif Some then #end if",
        expect![[r##"
            error in file FileId(1) at 0..6: found dangling `#elsif`
            | error in file FileId(1) for 0..6: this `#elsif` does not have a matching `#if`"##]],
    );
}

#[test]
fn report_dangling_pp_endif() {
    check(
        "#end if",
        expect![[r##"
            error in file FileId(1) at 0..4: found dangling `#end`
            | error in file FileId(1) for 0..4: this `#end` does not have a matching `#if`"##]],
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
