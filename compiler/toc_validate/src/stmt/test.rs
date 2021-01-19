//! Stmt & Decl validation tests
use crate::check;
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

#[test]
fn report_var_register_attr_in_main() {
    check(
        "var register a : int",
        expect![[r#"error at 4..13: ‘register’ attribute is not allowed at module level"#]],
    );
}

#[test]
fn report_var_register_attr_in_unit() {
    check(
        "unit var register a : int",
        expect![[r#"
        error at 5..25: expected a module, class, or monitor declaration
        error at 9..18: ‘register’ attribute is not allowed at module level"#]],
    );
}

#[test]
fn report_var_register_attr_in_module() {
    check(
        "module a var register a : int end a",
        expect![[r#"error at 13..22: ‘register’ attribute is not allowed at module level"#]],
    );
}

#[test]
fn report_var_register_attr_in_monitor() {
    check(
        "monitor a var register a : int end a",
        expect![[r#"error at 14..23: ‘register’ attribute is not allowed at module level"#]],
    );
}

#[test]
fn report_var_register_attr_in_class() {
    check(
        "class a var register a : int end a",
        expect![[r#"error at 12..21: ‘register’ attribute is not allowed at module level"#]],
    );
}

#[test]
fn var_register_attr_in_inner_class_block() {
    check("class a begin var register a : int end end a", expect![[]]);
}

#[test]
fn var_register_attr_with_class_ancestor() {
    check("begin var register a : int end class a end a", expect![[]]);
}

#[test]
fn var_register_attr_in_inner() {
    check(
        "begin var register a : int end if true then var register a : int end if",
        expect![[]],
    );
}

#[test]
fn nesting_inside_module() {
    check(
        r#"
module a
module b
end b
class b
end b
monitor b
end b
monitor class b
end b
end a
    "#,
        expect![[]],
    );
}

#[test]
fn report_nesting_inside_class() {
    check(
        r#"
class a
module b
end b
class b
end b
monitor b
end b
monitor class b
end b
end a
    "#,
        expect![[r#"
            error at 24..29: classes cannot be declared inside of other classes
            error at 62..67: monitor classes cannot be declared inside of classes"#]],
    );
}

#[test]
fn report_nesting_inside_monitor() {
    check(
        r#"
monitor a
module b
end b
class b
end b
monitor b
end b
monitor class b
end b
end a
    "#,
        expect![[r#"
            error at 26..31: classes cannot be declared inside of monitors
            error at 40..47: monitors cannot be declared inside of other monitors
            error at 64..69: monitor classes cannot be declared inside of monitors"#]],
    );
}

#[test]
fn report_nesting_inside_monitor_class() {
    check(
        r#"
monitor class a
module b
end b
class b
end b
monitor b
end b
monitor class b
end b
end a
    "#,
        expect![[r#"
            error at 32..37: classes cannot be declared inside of monitors
            error at 46..53: monitors cannot be declared inside of other monitors
            error at 70..75: monitor classes cannot be declared inside of monitors"#]],
    );
}
