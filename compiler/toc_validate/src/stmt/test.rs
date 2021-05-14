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
        expect![[r#"error at 16..19: device specification is not allowed for monitor classes"#]],
    );
}

#[test]
fn report_mismatched_module_names() {
    check(
        "module a end b",
        expect![[r#"
            error at 13..14: end identifier ‘b’ does not match ‘a’
            | note for 7..8: defined here"#]],
    );
}

#[test]
fn report_mismatched_class_names() {
    check(
        "class a end b",
        expect![[r#"
            error at 12..13: end identifier ‘b’ does not match ‘a’
            | note for 6..7: defined here"#]],
    );
}

#[test]
fn report_mismatched_monitor_names() {
    check(
        "monitor a end b",
        expect![[r#"
            error at 14..15: end identifier ‘b’ does not match ‘a’
            | note for 8..9: defined here"#]],
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
        expect![[r#"error at 10..13: expected identifier after here"#]],
    );
}

#[test]
fn report_var_register_attr_in_main() {
    check(
        "var register a : int",
        expect![[r#"error at 4..12: ‘register’ attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_unit() {
    check(
        "unit var register a : int",
        expect![[r#"
            error at 5..25: expected a module, class, or monitor declaration
            error at 9..17: ‘register’ attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_module() {
    check(
        "module a var register a : int end a",
        expect![[r#"error at 13..21: ‘register’ attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_monitor() {
    check(
        "monitor a var register a : int end a",
        expect![[r#"error at 14..22: ‘register’ attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_class() {
    check(
        "class a var register a : int end a",
        expect![[r#"error at 12..20: ‘register’ attribute is not allowed at module-like or program level"#]],
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
        expect![[
            r#"error at 0..11: ‘bind’ declaration is not allowed at module-like or program level"#
        ]],
    );
}

#[test]
fn report_bind_decl_in_module_block() {
    check(
        "module q bind a to b end q",
        expect![[r#"error at 9..20: ‘bind’ declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_class_block() {
    check(
        "class q bind a to b end q",
        expect![[r#"error at 8..19: ‘bind’ declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_monitor_block() {
    check(
        "monitor q bind a to b end q",
        expect![[r#"error at 10..21: ‘bind’ declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_monitor_class_block() {
    check(
        "monitor class q bind a to b end q",
        expect![[r#"error at 16..27: ‘bind’ declaration is not allowed at module-like or program level"#]],
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
        expect![[r#"
            error at 15..22: ‘init’ initializer is not allowed here
            | info for 8..11: ‘init’ initializer can only be used with array, record, or union types"#]],
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
        expect![[r#"
            error at 31..32: ‘init’ initializer is required here
            | note for 8..27: this is an unbounded array type
            | info: unbounded arrays have their upper bounds specified by ‘init’ initializers"#]],
    );
}

#[test]
fn report_no_initializer_with_unbounded_array() {
    check(
        "var a : array 1 .. * of int",
        expect![[r#"
            error at 8..27: ‘init’ initializer is required after here
            | note for 8..27: this is an unbounded array type
            | info: unbounded arrays have their upper bounds specified by ‘init’ initializers"#]],
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
        expect![[r#"error at 7..19: ‘procedure’ declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_proc_decl_in_block_stmt() {
    check(
        "begin proc a end a end",
        expect![[r#"error at 6..18: ‘procedure’ declaration is only allowed at module-like or program level"#]],
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
        expect![[r#"error at 7..10: device specification is not allowed here"#]],
    );
}

#[test]
fn report_dev_spec_in_monitor_proc() {
    check(
        "monitor a proc a : 2 end a end a",
        expect![[r#"error at 17..20: device specification is not allowed here"#]],
    );
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
        expect![[r#"error at 12..29: ‘function’ declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_fcn_decl_in_block_stmt() {
    check(
        "begin fcn a : int end a end",
        expect![[r#"error at 6..23: ‘function’ declaration is only allowed at module-like or program level"#]],
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
        expect![[r#"error at 16..31: ‘process’ declarations is not allowed in classes or monitor classes"#]],
    );
}

#[test]
fn report_process_decl_in_class() {
    check(
        "class q process a end a end q",
        expect![[r#"error at 8..23: ‘process’ declarations is not allowed in classes or monitor classes"#]],
    );
}

#[test]
fn report_process_decl_in_process_decl() {
    check(
        "process a process a end a end a",
        expect![[r#"error at 10..25: ‘process’ declaration is only allowed at the top level of ‘monitor’s and ‘module’s"#]],
    );
}

#[test]
fn report_process_decl_in_block_stmt() {
    check(
        "begin process a end a end",
        expect![[r#"error at 6..21: ‘process’ declaration is only allowed at the top level of ‘monitor’s and ‘module’s"#]],
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
            r#"error at 0..15: ‘deferred’ declaration is only allowed in module-like blocks"#
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

#[test]
fn forward_decl_in_main() {
    check("forward proc a", expect![[]]);
}

#[test]
fn forward_decl_in_module() {
    check("module q forward proc a end q", expect![[]]);
}

#[test]
fn report_forward_decl_in_proc_decl() {
    check(
        "proc a forward proc a end a",
        expect![[r#"error at 7..21: ‘forward’ declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_forward_decl_in_block_stmt() {
    check(
        "begin forward proc a end",
        expect![[r#"error at 6..20: ‘forward’ declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn body_decl_in_main() {
    check("body a end a", expect![[]]);
}

#[test]
fn body_decl_in_module() {
    check("module q body a end a end q", expect![[]]);
}

#[test]
fn report_body_decl_in_proc_decl() {
    check(
        "proc a body a end a end a",
        expect![[r#"error at 7..19: ‘body’ declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_body_decl_in_block_stmt() {
    check(
        "begin body a end a end",
        expect![[r#"error at 6..18: ‘body’ declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn new_open_text_caps() {
    check("open : _, _, get, put, mod", expect![[]]);
}

#[test]
fn new_open_binary_caps() {
    check("open : _, _, read, write, seek", expect![[]]);
}

#[test]
fn report_new_open_conflicting_read_caps() {
    check(
        "open : _, _, get, read",
        expect![[r#"
            error at 13..16: cannot use ‘get’/‘put’ with ‘read’/‘write’
            | note for 18..22: first conflicting binary capability"#]],
    );
}

#[test]
fn report_new_open_conflicting_write_caps() {
    check(
        "open : _, _, put, write",
        expect![[r#"
            error at 13..16: cannot use ‘get’/‘put’ with ‘read’/‘write’
            | note for 18..23: first conflicting binary capability"#]],
    );
}

#[test]
fn report_new_open_conflicting_reversed_caps() {
    check(
        "open : _, _, read, put",
        expect![[r#"
            error at 19..22: cannot use ‘get’/‘put’ with ‘read’/‘write’
            | note for 13..17: first conflicting binary capability"#]],
    );
}

#[test]
fn report_new_open_conflicting_mixed_caps() {
    check(
        "open : _, _, get, write",
        expect![[r#"
            error at 13..16: cannot use ‘get’/‘put’ with ‘read’/‘write’
            | note for 18..23: first conflicting binary capability"#]],
    );
}

#[test]
fn for_stmt_partial_increasing() {
    check("for : a end for", expect![[]]);
}

#[test]
fn for_stmt_full_decreasing() {
    check("for decreasing : a .. b end for", expect![[]]);
}

#[test]
fn report_for_stmt_partial_decreasing() {
    check(
        "for decreasing : a end for",
        expect![[r#"error at 17..18: decreasing for-loop requires explicit end bound"#]],
    );
}

#[test]
fn for_stmt_partial_decreasing_no_bounds() {
    // Don't duplicate errors
    check(
        "for decreasing : end for",
        expect![[r#"error at 17..20: expected expression, but found ‘end’"#]],
    );
}

#[test]
fn report_case_stmt_missing_arms() {
    check(
        "case a of end case",
        expect![[r#"error at 0..18: Missing ‘label’ arms for ‘case’ statement"#]],
    );
}

#[test]
fn case_stmt_one_arm() {
    check("case a of label 1: end case", expect![[]]);
}

#[test]
fn report_case_stmt_one_arm_default() {
    check(
        "case a of label : end case",
        expect![[
            r#"error at 10..18: First ‘label’ arm must have at least one selector expression"#
        ]],
    );
}

#[test]
fn case_stmt_two_arms() {
    check("case a of label 1: label : end case", expect![[]]);
}

#[test]
fn case_stmt_many_arms() {
    check("case a of label 1: label 2: label : end case", expect![[]]);
}

#[test]
fn report_case_stmt_many_arms_many_defaults() {
    check(
        "case a of label 1: label : label : end case",
        expect![[r#"error at 27..35: Extra ‘label’ arm found after default arm"#]],
    );
}

#[test]
fn report_case_stmt_many_arms_many_after_default() {
    check(
        "case a of label 1: label : label 2: label 2: label : end case",
        expect![[r#"error at 27..53: Extra ‘label’ arms found after default arm"#]],
    );
}

#[test]
fn report_case_stmt_many_arms_after_first_default() {
    check(
        "case a of label : label 2: label 2: label : end case",
        expect![[r#"
            error at 10..18: First ‘label’ arm must have at least one selector expression
            error at 18..44: Extra ‘label’ arms found after default arm"#]],
    );
}

#[test]
fn invariant_stmt_in_loop() {
    check(
        "loop invariant false invariant false invariant false end loop",
        expect![[]],
    );
}

#[test]
fn invariant_stmt_in_for_loop() {
    check(
        "for : a invariant false invariant false invariant false end for",
        expect![[]],
    );
}

#[test]
fn invariant_stmt_in_module() {
    check(
        "module a invariant false invariant false end a",
        expect![[]],
    );
}

#[test]
fn invariant_stmt_in_monitor() {
    check(
        "monitor a invariant false invariant false end a",
        expect![[]],
    );
}

#[test]
fn invariant_stmt_in_class() {
    check("class a invariant false invariant false end a", expect![[]]);
}

#[test]
fn invariant_stmt_in_monitor_class() {
    check(
        "monitor class a invariant false invariant false end a",
        expect![[]],
    );
}

#[test]
fn report_invariant_stmt_in_main() {
    check(
        "invariant false",
        expect![[
            r#"error at 0..15: ‘invariant’ statement is only allowed in loop statements and module-kind declarations"#
        ]],
    );
}

#[test]
fn report_invariant_stmt_in_inner() {
    check(
        "begin invariant false end",
        expect![[r#"error at 6..21: ‘invariant’ statement is only allowed in loop statements and module-kind declarations"#]],
    );
}
