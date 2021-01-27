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

#[test]
fn report_bind_decl_in_main_block() {
    check(
        "bind a to b",
        expect![[r#"error at 0..11: ‘bind’ declaration is not allowed at module level"#]],
    );
}

#[test]
fn report_bind_decl_in_module_block() {
    check(
        "module q bind a to b end q",
        expect![[r#"error at 9..21: ‘bind’ declaration is not allowed at module level"#]],
    );
}

#[test]
fn report_bind_decl_in_class_block() {
    check(
        "class q bind a to b end q",
        expect![[r#"error at 8..20: ‘bind’ declaration is not allowed at module level"#]],
    );
}

#[test]
fn report_bind_decl_in_monitor_block() {
    check(
        "monitor q bind a to b end q",
        expect![[r#"error at 10..22: ‘bind’ declaration is not allowed at module level"#]],
    );
}

#[test]
fn report_bind_decl_in_monitor_class_block() {
    check(
        "monitor class q bind a to b end q",
        expect![[r#"error at 16..28: ‘bind’ declaration is not allowed at module level"#]],
    );
}

#[test]
fn bind_decl_in_inner_blocks() {
    check(
        "begin bind a to b end if true then bind c to d end if",
        expect![[]],
    );
}

#[test]
fn report_init_expr_with_int_ty() {
    check(
        "var a : int := init(1)",
        expect![[r#"error at 15..22: ‘init’ initializer is not allowed here"#]],
    );
}

#[test]
fn report_init_expr_with_no_ty() {
    check(
        "var a := init(1)",
        expect![[r#"error at 9..16: ‘init’ initializer is not allowed here"#]],
    );
}

#[test]
fn init_expr_with_array_ty() {
    check("var a : array 1 .. 3 of int := init(1, 2, 3)", expect![[]]);
}

#[test]
fn init_expr_with_record_ty() {
    check("var a : record a : int end record := init(1)", expect![[]]);
}

#[test]
fn init_expr_with_union_ty() {
    check(
        "var a : union : 1 .. 3 of label : a : int end union := init(1, 2)",
        expect![[]],
    );
}

#[test]
fn report_not_init_expr_with_unbounded_array() {
    check(
        "var a : array 1 .. * of int := 2",
        expect![[r#"error at 31..32: ‘init’ initializer is required here"#]],
    );
}

#[test]
fn report_no_initializer_with_unbounded_array() {
    check(
        "var a : array 1 .. * of int",
        expect![[r#"error at 8..27: ‘init’ initializer is required after here"#]],
    );
}

#[test]
fn proc_decl_in_main() {
    check("proc a end a", expect![[]]);
}

#[test]
fn proc_decl_in_module() {
    check("module q proc a end a end q", expect![[]]);
}

#[test]
fn report_proc_decl_in_proc_decl() {
    check(
        "proc a proc a end a end a",
        expect![[r#"error at 7..20: ‘procedure’ declaration is only allowed at module level"#]],
    );
}

#[test]
fn report_proc_decl_in_block_stmt() {
    check(
        "begin proc a end a end",
        expect![[r#"error at 6..19: ‘procedure’ declaration is only allowed at module level"#]],
    );
}

#[test]
fn report_dev_spec_in_forward_decl() {
    check(
        "forward proc a : 2",
        expect![[r#"error at 15..18: device specification is not allowed here"#]],
    );
}

#[test]
fn report_dev_spec_in_main_proc() {
    check(
        "proc a : 2 end a",
        expect![[r#"error at 7..11: device specification is not allowed here"#]],
    );
}

#[test]
fn report_dev_spec_in_monitor_proc() {
    check(
        "monitor a proc a : 2 end a end a",
        expect![[r#"error at 17..21: device specification is not allowed here"#]],
    );
    panic!("oop");
}

#[test]
fn dev_spec_in_dev_monitor_proc() {
    check("monitor a : 3 proc a : 2 end a end a", expect![[]]);
}

#[test]
fn fcn_decl_in_main() {
    check("fcn a : int end a", expect![[]]);
}

#[test]
fn fcn_decl_in_module() {
    check("module q fcn a : int end a end q", expect![[]]);
}

#[test]
fn report_fcn_decl_in_fcn_decl() {
    check(
        "fcn a : int fcn a : int end a end a",
        expect![[r#"error at 12..30: ‘function’ declaration is only allowed at module level"#]],
    );
}

#[test]
fn report_fcn_decl_in_block_stmt() {
    check(
        "begin fcn a : int end a end",
        expect![[r#"error at 6..24: ‘function’ declaration is only allowed at module level"#]],
    );
}

#[test]
fn process_decl_in_main() {
    check("proc a end a", expect![[]]);
}

#[test]
fn process_decl_in_module() {
    check("module q process a end a end q", expect![[]]);
}

#[test]
fn process_decl_in_monitor() {
    check("monitor q process a end a end q", expect![[]]);
}

#[test]
fn report_process_decl_in_monitor_class() {
    check(
        "monitor class q process a end a end q",
        expect![[
            r#"error at 16..32: ‘process’ declarations is not allowed in monitor classes or classes"#
        ]],
    );
}

#[test]
fn report_process_decl_in_class() {
    check(
        "class q process a end a end q",
        expect![[
            r#"error at 8..24: ‘process’ declarations is not allowed in monitor classes or classes"#
        ]],
    );
}

#[test]
fn report_process_decl_in_process_decl() {
    check(
        "process a process a end a end a",
        expect![[
            r#"error at 10..26: ‘process’ declaration is only allowed at the top level of ‘monitor’s and ‘module’s"#
        ]],
    );
}

#[test]
fn report_process_decl_in_block_stmt() {
    check(
        "begin process a end a end",
        expect![[
            r#"error at 6..22: ‘process’ declaration is only allowed at the top level of ‘monitor’s and ‘module’s"#
        ]],
    );
}

#[test]
fn report_external_var() {
    check(
        "external \"eee\" var a := 1",
        expect![[r#"error at 15..25: ‘external’ variables are not supported in this compiler"#]],
    );
}

#[test]
fn report_deferred_decl_in_main() {
    check(
        "deferred proc a",
        expect![[
            r#"error at 0..15: ‘deferred’ declaration is only allowed in module-kind blocks"#
        ]],
    );
}

#[test]
fn deferred_decl_in_module() {
    check("module q deferred proc a end q", expect![[]]);
}

#[test]
fn deferred_decl_in_monitor() {
    check("monitor q deferred proc a end q", expect![[]]);
}

#[test]
fn deferred_decl_in_monitor_class() {
    check("monitor class q deferred proc a end q", expect![[]]);
}

#[test]
fn deferred_decl_in_class() {
    check("class q deferred proc a end q", expect![[]]);
}
