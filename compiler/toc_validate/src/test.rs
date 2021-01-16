//! Validated things
use super::check;
use expect_test::expect;

#[test]
fn dangling_else() {
    check("else end if", expect![[r#"error at 0..4: found ‘else’ without matching ‘if’"#]]);
}

#[test]
fn dangling_elseif() {
    check("elsif true then end if", expect![[r#"error at 0..5: found ‘elsif’ without matching ‘if’"#]]);
}

#[test]
fn dangling_pp_else() {
    check("#else #end if", expect![[r##"error at 0..5: found ‘#else’ without matching ‘#if’"##]]);
}

#[test]
fn dangling_pp_elseif() {
    check("#elsif Some then #end if", expect![[r##"error at 0..6: found ‘#elsif’ without matching ‘#if’"##]]);
}

#[test]
fn dangling_pp_endif() {
    check("#end if", expect![[r##"error at 0..4: found ‘#end’ without matching ‘#if’"##]]);
}
