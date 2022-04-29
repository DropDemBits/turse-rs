//! Stmt & Decl validation tests
use crate::check;
use expect_test::expect;

#[test]
fn report_dangling_else() {
    check(
        "else end if",
        expect![[r#"
            error in file FileId(1) at 0..4: found dangling `else`
            | error in file FileId(1) for 0..4: this `else` does not have a matching `if`"#]],
    );
}

#[test]
fn report_dangling_elseif() {
    check(
        "elsif true then end if",
        expect![[r#"
            error in file FileId(1) at 0..5: found dangling `elsif`
            | error in file FileId(1) for 0..5: this `elsif` does not have a matching `if`"#]],
    );
}

#[test]
fn report_monitor_class_with_dev_spec() {
    check(
        "monitor class a : 1 end a",
        expect![[r#"
            error in file FileId(1) at 16..19: device specification is not allowed here
            | error in file FileId(1) for 16..19: device specification is not allowed for monitor classes"#]],
    );
}

#[test]
fn report_mismatched_module_names() {
    check(
        "module a end b",
        expect![[r#"
            error in file FileId(1) at 13..14: mismatched identifier names
            | note in file FileId(1) for 7..8: `a` does not match...
            | note in file FileId(1) for 13..14: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_class_names() {
    check(
        "class a end b",
        expect![[r#"
            error in file FileId(1) at 12..13: mismatched identifier names
            | note in file FileId(1) for 6..7: `a` does not match...
            | note in file FileId(1) for 12..13: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_monitor_names() {
    check(
        "monitor a end b",
        expect![[r#"
            error in file FileId(1) at 14..15: mismatched identifier names
            | note in file FileId(1) for 8..9: `a` does not match...
            | note in file FileId(1) for 14..15: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_fcn_names() {
    check(
        "fcn a() : int end b",
        expect![[r#"
        error in file FileId(1) at 18..19: mismatched identifier names
        | note in file FileId(1) for 4..5: `a` does not match...
        | note in file FileId(1) for 18..19: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_process_names() {
    check(
        "process a() end b",
        expect![[r#"
        error in file FileId(1) at 16..17: mismatched identifier names
        | note in file FileId(1) for 8..9: `a` does not match...
        | note in file FileId(1) for 16..17: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_body_proc_names() {
    check(
        "body proc a end b",
        expect![[r#"
        error in file FileId(1) at 16..17: mismatched identifier names
        | note in file FileId(1) for 10..11: `a` does not match...
        | note in file FileId(1) for 16..17: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_body_fcn_names() {
    check(
        "body function a : int end b",
        expect![[r#"
        error in file FileId(1) at 26..27: mismatched identifier names
        | note in file FileId(1) for 14..15: `a` does not match...
        | note in file FileId(1) for 26..27: ...`b` defined here"#]],
    );
}

#[test]
fn report_mismatched_body_plain_names() {
    check(
        "body a end b",
        expect![[r#"
        error in file FileId(1) at 11..12: mismatched identifier names
        | note in file FileId(1) for 5..6: `a` does not match...
        | note in file FileId(1) for 11..12: ...`b` defined here"#]],
    );
}

#[test]
fn only_missing_module_decl_name() {
    check(
        "monitor end b",
        expect![[r#"
            error in file FileId(1) at 8..11: unexpected token
            | error in file FileId(1) for 8..11: expected identifier, but found `end`"#]],
    );
}

#[test]
fn only_missing_module_end_name() {
    check(
        "monitor a end",
        expect![[r#"
            error in file FileId(1) at 10..13: unexpected end of file
            | error in file FileId(1) for 10..13: expected identifier after here"#]],
    );
}

#[test]
fn report_var_register_attr_in_main() {
    check(
        "var register a : int",
        expect![[r#"
            error in file FileId(1) at 4..12: cannot use `register` here
            | error in file FileId(1) for 4..12: `register` attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_unit() {
    check(
        "unit var register a : int",
        expect![[r#"
            error in file FileId(1) at 5..25: invalid unit file
            | error in file FileId(1) for 5..25: expected a module, class, or monitor declaration
            error in file FileId(1) at 9..17: cannot use `register` here
            | error in file FileId(1) for 9..17: `register` attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_module() {
    check(
        "module a var register a : int end a",
        expect![[r#"
            error in file FileId(1) at 13..21: cannot use `register` here
            | error in file FileId(1) for 13..21: `register` attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_monitor() {
    check(
        "monitor a var register a : int end a",
        expect![[r#"
            error in file FileId(1) at 14..22: cannot use `register` here
            | error in file FileId(1) for 14..22: `register` attribute is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_var_register_attr_in_class() {
    check(
        "class a var register a : int end a",
        expect![[r#"
            error in file FileId(1) at 12..20: cannot use `register` here
            | error in file FileId(1) for 12..20: `register` attribute is not allowed at module-like or program level"#]],
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
            error in file FileId(1) at 24..29: cannot declare a `class` here
            | error in file FileId(1) for 24..29: classes cannot be declared inside of other classes
            error in file FileId(1) at 62..67: cannot declare a `monitor class` here
            | error in file FileId(1) for 62..67: monitor classes cannot be declared inside of classes"#]],
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
            error in file FileId(1) at 26..31: cannot declare a `class` here
            | error in file FileId(1) for 26..31: classes cannot be declared inside of monitors
            error in file FileId(1) at 40..47: cannot declare a `monitor` here
            | error in file FileId(1) for 40..47: monitors cannot be declared inside of other monitors
            error in file FileId(1) at 64..69: cannot declare a `monitor class` here
            | error in file FileId(1) for 64..69: monitor classes cannot be declared inside of monitors"#]],
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
            error in file FileId(1) at 32..37: cannot declare a `class` here
            | error in file FileId(1) for 32..37: classes cannot be declared inside of monitors
            error in file FileId(1) at 46..53: cannot declare a `monitor` here
            | error in file FileId(1) for 46..53: monitors cannot be declared inside of other monitors
            error in file FileId(1) at 70..75: cannot declare a `monitor class` here
            | error in file FileId(1) for 70..75: monitor classes cannot be declared inside of monitors"#]],
    );
}

#[test]
fn report_bind_decl_in_main_block() {
    check(
        "bind a to b",
        expect![[r#"
            error in file FileId(1) at 0..11: cannot use `bind` here
            | error in file FileId(1) for 0..11: `bind` declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_module_block() {
    check(
        "module q bind a to b end q",
        expect![[r#"
            error in file FileId(1) at 9..20: cannot use `bind` here
            | error in file FileId(1) for 9..20: `bind` declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_class_block() {
    check(
        "class q bind a to b end q",
        expect![[r#"
            error in file FileId(1) at 8..19: cannot use `bind` here
            | error in file FileId(1) for 8..19: `bind` declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_monitor_block() {
    check(
        "monitor q bind a to b end q",
        expect![[r#"
            error in file FileId(1) at 10..21: cannot use `bind` here
            | error in file FileId(1) for 10..21: `bind` declaration is not allowed at module-like or program level"#]],
    );
}

#[test]
fn report_bind_decl_in_monitor_class_block() {
    check(
        "monitor class q bind a to b end q",
        expect![[r#"
            error in file FileId(1) at 16..27: cannot use `bind` here
            | error in file FileId(1) for 16..27: `bind` declaration is not allowed at module-like or program level"#]],
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
            error in file FileId(1) at 15..22: mismatched initializer
            | error in file FileId(1) for 15..22: `init` initializer is not allowed here
            | error in file FileId(1) for 8..11: cannot use `init` initializer with this type
            | info: `init` initializer can only be used with array, record, or union types"#]],
    );
}

#[test]
fn report_init_expr_with_no_ty() {
    check(
        "var a := init(1)",
        expect![[r#"
            error in file FileId(1) at 9..16: mismatched initializer
            | error in file FileId(1) for 9..16: `init` initializer is not allowed here
            | info: `init` initializer requires a type to be specified"#]],
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
            error in file FileId(1) at 31..32: mismatched initializer
            | error in file FileId(1) for 31..32: `init` initializer required here
            | note in file FileId(1) for 8..27: this is an unbounded array type
            | info: unbounded arrays have their upper bounds specified by `init` initializers"#]],
    );
}

#[test]
fn report_no_initializer_with_unbounded_array() {
    check(
        "var a : array 1 .. * of int",
        expect![[r#"
            error in file FileId(1) at 8..27: mismatched initializer
            | error in file FileId(1) for 8..27: `init` initializer required after here
            | note in file FileId(1) for 8..27: this is an unbounded array type
            | info: unbounded arrays have their upper bounds specified by `init` initializers"#]],
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
        expect![[r#"
            error in file FileId(1) at 7..19: cannot use `procedure` declaration here
            | error in file FileId(1) for 7..19: `procedure` declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_proc_decl_in_block_stmt() {
    check(
        "begin proc a end a end",
        expect![[r#"
            error in file FileId(1) at 6..18: cannot use `procedure` declaration here
            | error in file FileId(1) for 6..18: `procedure` declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_dev_spec_in_forward_decl() {
    check(
        "forward proc a : 2",
        expect![[r#"
            error in file FileId(1) at 15..18: device specification is not allowed here
            | error in file FileId(1) for 15..18: not part of a `procedure` declaration"#]],
    );
}

#[test]
fn report_dev_spec_in_main_proc() {
    check(
        "proc a : 2 end a",
        expect![[r#"
            error in file FileId(1) at 7..10: device specification is not allowed here
            | error in file FileId(1) for 7..10: `procedure` is not in a device monitor"#]],
    );
}

#[test]
fn report_dev_spec_in_monitor_proc() {
    check(
        "monitor a proc a : 2 end a end a",
        expect![[r#"
            error in file FileId(1) at 17..20: device specification is not allowed here
            | error in file FileId(1) for 17..20: `procedure` is not in a device monitor"#]],
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
        expect![[r#"
            error in file FileId(1) at 12..29: cannot use `function` declaration here
            | error in file FileId(1) for 12..29: `function` declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_fcn_decl_in_block_stmt() {
    check(
        "begin fcn a : int end a end",
        expect![[r#"
            error in file FileId(1) at 6..23: cannot use `function` declaration here
            | error in file FileId(1) for 6..23: `function` declaration is only allowed at module-like or program level"#]],
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
        expect![[r#"
            error in file FileId(1) at 16..31: cannot declare a `process` here
            | error in file FileId(1) for 16..31: `process` declarations are not allowed in classes or monitor classes"#]],
    );
}

#[test]
fn report_process_decl_in_class() {
    check(
        "class q process a end a end q",
        expect![[r#"
            error in file FileId(1) at 8..23: cannot declare a `process` here
            | error in file FileId(1) for 8..23: `process` declarations are not allowed in classes or monitor classes"#]],
    );
}

#[test]
fn report_process_decl_in_process_decl() {
    check(
        "process a process a end a end a",
        expect![[r#"
            error in file FileId(1) at 10..25: cannot declare a `process` here
            | error in file FileId(1) for 10..25: `process` declaration is only allowed at the top level of `monitor`s and `module`s"#]],
    );
}

#[test]
fn report_process_decl_in_block_stmt() {
    check(
        "begin process a end a end",
        expect![[r#"
            error in file FileId(1) at 6..21: cannot declare a `process` here
            | error in file FileId(1) for 6..21: `process` declaration is only allowed at the top level of `monitor`s and `module`s"#]],
    );
}

#[test]
fn report_external_var() {
    check(
        "external \"eee\" var a := 1",
        expect![[r#"
            error in file FileId(1) at 15..25: unsupported declaration
            | error in file FileId(1) for 15..25: `external` variables are not supported in this compiler"#]],
    );
}

#[test]
fn report_deferred_decl_in_main() {
    check(
        "deferred proc a",
        expect![[r#"
            error in file FileId(1) at 0..15: cannot use `deferred` here
            | error in file FileId(1) for 0..15: `deferred` is only allowed in module-like blocks"#]],
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
        expect![[r#"
            error in file FileId(1) at 7..21: cannot use `forward` declaration here
            | error in file FileId(1) for 7..21: `forward` declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_forward_decl_in_block_stmt() {
    check(
        "begin forward proc a end",
        expect![[r#"
            error in file FileId(1) at 6..20: cannot use `forward` declaration here
            | error in file FileId(1) for 6..20: `forward` declaration is only allowed at module-like or program level"#]],
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
        expect![[r#"
            error in file FileId(1) at 7..19: cannot use `body` declaration here
            | error in file FileId(1) for 7..19: `body` declaration is only allowed at module-like or program level"#]],
    );
}

#[test]
fn report_body_decl_in_block_stmt() {
    check(
        "begin body a end a end",
        expect![[r#"
            error in file FileId(1) at 6..18: cannot use `body` declaration here
            | error in file FileId(1) for 6..18: `body` declaration is only allowed at module-like or program level"#]],
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
            error in file FileId(1) at 13..16: cannot use `get`/`put` with `read`/`write`
            | note in file FileId(1) for 18..22: first conflicting binary capability"#]],
    );
}

#[test]
fn report_new_open_conflicting_write_caps() {
    check(
        "open : _, _, put, write",
        expect![[r#"
            error in file FileId(1) at 13..16: cannot use `get`/`put` with `read`/`write`
            | note in file FileId(1) for 18..23: first conflicting binary capability"#]],
    );
}

#[test]
fn report_new_open_conflicting_reversed_caps() {
    check(
        "open : _, _, read, put",
        expect![[r#"
            error in file FileId(1) at 19..22: cannot use `get`/`put` with `read`/`write`
            | note in file FileId(1) for 13..17: first conflicting binary capability"#]],
    );
}

#[test]
fn report_new_open_conflicting_mixed_caps() {
    check(
        "open : _, _, get, write",
        expect![[r#"
            error in file FileId(1) at 13..16: cannot use `get`/`put` with `read`/`write`
            | note in file FileId(1) for 18..23: first conflicting binary capability"#]],
    );
}

#[test]
fn for_valid_bounds() {
    // increasing explicit
    check("for : 1 .. 10 end for", expect![[]]);
    // decreasing explicit
    check("for decreasing : 1 .. 10 end for", expect![[]]);
    // increasing implicit
    check("for : implied end for", expect![[]]);
}

#[test]
fn report_for_invalid_bounds() {
    // decreasing implicit
    check(
        "for decreasing : implied end for",
        expect![[r#"
            error in file FileId(1) at 17..24: `decreasing` for-loops cannot use implicit range bounds
            | error in file FileId(1) for 17..24: range bounds are implied from here
            | note in file FileId(1) for 4..14: `decreasing` for-loop specified here
            | info: `decreasing` for-loops can only use explicit range bounds (e.g. `1 .. 2`)"#]],
    );
}

#[test]
fn report_exit_outside_loop() {
    check(
        "exit",
        expect![[r#"
            error in file FileId(1) at 0..4: cannot use `exit` statement here
            | error in file FileId(1) for 0..4: can only be used inside of `loop` and `for` statements"#]],
    );
}

#[test]
fn exit_in_loop() {
    // no error
    check("loop  exit  end loop", expect![[]]);
    check("for : 1 .. 10  exit  end for", expect![[]]);
}

#[test]
fn exit_in_if_loop() {
    // no error
    check("loop if true then exit end if end loop", expect![[]]);
    check(
        "for : 1 .. 10 if true then exit end if end for",
        expect![[]],
    );
}

#[test]
fn report_case_stmt_missing_arms() {
    check(
        "case a of end case",
        expect![[r#"
            error in file FileId(1) at 0..18: invalid `case` statement
            | error in file FileId(1) for 0..18: missing `label` arms"#]],
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
        expect![[r#"
            error in file FileId(1) at 10..18: cannot have a default `label` arm as the first `case` arm
            | error in file FileId(1) for 10..18: First `label` arm must have at least one selector expression"#]],
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
        expect![[r#"
            error in file FileId(1) at 27..35: extra `label` arm found after default arm
            | error in file FileId(1) for 27..35: extra `label` arm"#]],
    );
}

#[test]
fn report_case_stmt_many_arms_many_after_default() {
    check(
        "case a of label 1: label : label 2: label 2: label : end case",
        expect![[r#"
            error in file FileId(1) at 27..53: extra `label` arms found after default arm
            | error in file FileId(1) for 27..53: extra `label` arms"#]],
    );
}

#[test]
fn report_case_stmt_many_arms_after_first_default() {
    check(
        "case a of label : label 2: label 2: label : end case",
        expect![[r#"
            error in file FileId(1) at 10..18: cannot have a default `label` arm as the first `case` arm
            | error in file FileId(1) for 10..18: First `label` arm must have at least one selector expression
            error in file FileId(1) at 18..44: extra `label` arms found after default arm
            | error in file FileId(1) for 18..44: extra `label` arms"#]],
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
        expect![[r#"
            error in file FileId(1) at 0..15: cannot use `invariant` here
            | error in file FileId(1) for 0..15: `invariant` statement is only allowed in loop statements and module-kind declarations"#]],
    );
}

#[test]
fn report_invariant_stmt_in_inner() {
    check(
        "begin invariant false end",
        expect![[r#"
            error in file FileId(1) at 6..21: cannot use `invariant` here
            | error in file FileId(1) for 6..21: `invariant` statement is only allowed in loop statements and module-kind declarations"#]],
    );
}

#[test]
fn return_stmt_in_main() {
    check("return", expect![[r#""#]]);
}

#[test]
fn report_return_stmt_in_unit() {
    check(
        "unit return",
        expect![[r#"
        error in file FileId(1) at 5..11: invalid unit file
        | error in file FileId(1) for 5..11: expected a module, class, or monitor declaration"#]],
    );
}

#[test]
fn return_stmt_in_module_body() {
    check("module m return end m", expect![[r#""#]]);
}

#[test]
fn return_stmt_in_procedure_body() {
    check("procedure p return end p", expect![[r#""#]]);
}

#[test]
fn return_stmt_in_process_body() {
    check("process p return end p", expect![[r#""#]]);
}

#[test]
fn return_stmt_in_body_body() {
    // Note: see `stmt::validate_result_stmt` on why this is always accepted
    check("body p return end p", expect![[r#""#]]);
}

#[test]
fn report_return_stmt_in_function_body() {
    check(
        "function p : int return end p",
        expect![[r#"
        error in file FileId(1) at 17..23: cannot use `return` here
        | error in file FileId(1) for 17..23: `result` statement is used to return values in function bodies"#]],
    );
}

#[test]
fn return_stmt_in_procedure_inner_body() {
    check("proc p begin return end end p", expect![[r#""#]]);
}

#[test]
fn report_result_stmt_in_main() {
    check(
        "result 'uwu'",
        expect![[r#"
        error in file FileId(1) at 0..12: cannot use `result` here
        | error in file FileId(1) for 0..12: `result` statement is only allowed in function bodies"#]],
    );
}

#[test]
fn report_result_stmt_in_unit() {
    check(
        "unit result 'uwu'",
        expect![[r#"
        error in file FileId(1) at 5..17: invalid unit file
        | error in file FileId(1) for 5..17: expected a module, class, or monitor declaration"#]],
    );
}

#[test]
fn report_result_stmt_in_module_body() {
    check(
        "module m result 'uwu' end m",
        expect![[r#"
        error in file FileId(1) at 9..21: cannot use `result` here
        | error in file FileId(1) for 9..21: `result` statement is only allowed in function bodies"#]],
    );
}

#[test]
fn report_result_stmt_in_procedure_body() {
    check(
        "procedure p result 'uwu' end p",
        expect![[r#"
        error in file FileId(1) at 12..24: cannot use `result` here
        | error in file FileId(1) for 12..24: `result` statement is only allowed in function bodies"#]],
    );
}

#[test]
fn report_result_stmt_in_process_body() {
    check(
        "process p result 'uwu' end p",
        expect![[r#"
        error in file FileId(1) at 10..22: cannot use `result` here
        | error in file FileId(1) for 10..22: `result` statement is only allowed in function bodies"#]],
    );
}

#[test]
fn result_stmt_in_body_body() {
    // Note: see `stmt::validate_result_stmt` on why this is always accepted
    check("body p result 'uwu' end p", expect![[r#""#]]);
}

#[test]
fn result_stmt_in_function_body() {
    check("function p : int result 'uwu' end p", expect![[r#""#]]);
}

#[test]
fn result_stmt_in_function_inner_body() {
    check(
        "function p : int begin result 'uwu' end end p",
        expect![[r#""#]],
    );
}

#[test]
fn pre_stmt_in_module_top_level() {
    check("module m pre true end m", expect![[]]);
}

#[test]
fn report_pre_stmt_in_module_inner() {
    check(
        "module m begin pre true end end m",
        expect![[r#"
        error in file FileId(1) at 15..23: cannot use `pre` statement here
        | error in file FileId(1) for 15..23: `pre` statement is only allowed at the top level of module-likes and subprograms"#]],
    );
}

#[test]
fn pre_stmt_in_subprogram_top_level() {
    check("proc p pre true end p", expect![[]]);
}

#[test]
fn report_pre_stmt_in_subprogram_inner() {
    check(
        "proc p begin pre true end end p",
        expect![[r#"
        error in file FileId(1) at 13..21: cannot use `pre` statement here
        | error in file FileId(1) for 13..21: `pre` statement is only allowed at the top level of module-likes and subprograms"#]],
    );
}

#[test]
fn init_stmt_in_subprogram_top_level() {
    check("proc p init uwu := 2 end p", expect![[]])
}

#[test]
fn report_init_stmt_in_subprogram_inner() {
    check(
        "proc p begin init uwu := 2 end end p",
        expect![[r#"
        error in file FileId(1) at 13..26: cannot use `init` statement here
        | error in file FileId(1) for 13..26: `init` statement is only allowed at the top level of subprograms"#]],
    )
}

#[test]
fn post_stmt_in_module_top_level() {
    check("module m post true end m", expect![[]]);
}

#[test]
fn report_post_stmt_in_module_inner() {
    check(
        "module m begin post true end end m",
        expect![[r#"
            error in file FileId(1) at 15..24: cannot use `post` statement here
            | error in file FileId(1) for 15..24: `post` statement is only allowed at the top level of module-likes and subprograms"#]],
    );
}

#[test]
fn post_stmt_in_subprogram_top_level() {
    check("proc p post true end p", expect![[]]);
}

#[test]
fn report_post_stmt_in_subprogram_inner() {
    check(
        "proc p begin post true end end p",
        expect![[r#"
            error in file FileId(1) at 13..22: cannot use `post` statement here
            | error in file FileId(1) for 13..22: `post` statement is only allowed at the top level of module-likes and subprograms"#]],
    );
}

#[test]
fn handler_stmt_in_subprogram_top_level() {
    check("proc p handler(owo) end handler end p", expect![[]]);
}

#[test]
fn report_handler_stmt_in_subprogram_inner() {
    check(
        "proc p begin handler(owo) end handler end end p",
        expect![[r#"
            error in file FileId(1) at 13..37: cannot use `handler` statement here
            | error in file FileId(1) for 13..37: `handler` statement is only allowed at the top level of subprograms"#]],
    );
}

#[test]
fn inherit_stmt_in_class_top_level() {
    check("class c inherit owo end c", expect![[]]);
}

#[test]
fn inherit_stmt_in_monitor_class_top_level() {
    check("monitor class c inherit owo end c", expect![[]]);
}

#[test]
fn report_inherit_stmt_in_class_inner() {
    check(
        "class c begin inherit owo end end c",
        expect![[r#"
        error in file FileId(1) at 14..25: cannot use `inherit` statement here
        | error in file FileId(1) for 14..25: `inherit` statement is only allowed in classes"#]],
    );
}

#[test]
fn report_inherit_stmt_in_monitor_class_inner() {
    check(
        "monitor class c begin inherit owo end end c",
        expect![[r#"
        error in file FileId(1) at 22..33: cannot use `inherit` statement here
        | error in file FileId(1) for 22..33: `inherit` statement is only allowed in classes"#]],
    );
}

#[test]
fn report_inherit_stmt_in_module() {
    check(
        "module m inherit owo end m",
        expect![[r#"
        error in file FileId(1) at 9..20: cannot use `inherit` statement here
        | error in file FileId(1) for 9..20: `inherit` statement is only allowed in classes"#]],
    );
}

#[test]
fn implement_stmt_in_module_top_level() {
    check("module m implement sus end m", expect![[]]);
}

#[test]
fn implement_stmt_in_monitor_top_level() {
    check("monitor m implement sus end m", expect![[]]);
}

#[test]
fn implement_stmt_in_class_top_level() {
    check("class m implement sus end m", expect![[]]);
}

#[test]
fn implement_stmt_in_monitor_class_top_level() {
    check("monitor class m implement sus end m", expect![[]]);
}

#[test]
fn report_implement_stmt_in_module_inner() {
    check(
        "module m begin implement sus end end m",
        expect![[r#"
        error in file FileId(1) at 15..28: cannot use `implement` statement here
        | error in file FileId(1) for 15..28: `implement` statement is only allowed in module-like blocks"#]],
    );
}

#[test]
fn report_implement_stmt_in_top_level() {
    check(
        "implement sus",
        expect![[r#"
        error in file FileId(1) at 0..13: cannot use `implement` statement here
        | error in file FileId(1) for 0..13: `implement` statement is only allowed in module-like blocks"#]],
    );
}

#[test]
fn implement_by_stmt_in_module_top_level() {
    check("module m implement by sus end m", expect![[]]);
}

#[test]
fn implement_by_stmt_in_monitor_top_level() {
    check("monitor m implement by sus end m", expect![[]]);
}

#[test]
fn implement_by_stmt_in_class_top_level() {
    check("class m implement by sus end m", expect![[]]);
}

#[test]
fn implement_by_stmt_in_monitor_class_top_level() {
    check("monitor class m implement by sus end m", expect![[]]);
}

#[test]
fn report_implement_by_stmt_in_module_inner() {
    check(
        "module m begin implement by sus end end m",
        expect![[r#"
        error in file FileId(1) at 15..31: cannot use `implement by` statement here
        | error in file FileId(1) for 15..31: `implement by` statement is only allowed in module-like blocks"#]],
    );
}

#[test]
fn report_implement_by_stmt_in_top_level() {
    check(
        "implement by sus",
        expect![[r#"
        error in file FileId(1) at 0..16: cannot use `implement by` statement here
        | error in file FileId(1) for 0..16: `implement by` statement is only allowed in module-like blocks"#]],
    );
}

#[test]
fn import_stmt_in_program() {
    check("import ()", expect![[]]);
}

#[test]
fn report_import_stmt_in_unit() {
    check(
        "unit import ()",
        expect![[r#"
        error in file FileId(1) at 5..14: invalid unit file
        | error in file FileId(1) for 5..14: expected a module, class, or monitor declaration"#]],
    );
}

#[test]
fn import_stmt_in_program_inner() {
    check(
        "begin import () end",
        expect![[r#"
        error in file FileId(1) at 6..15: cannot use `import` statement here
        | error in file FileId(1) for 6..15: `import` statement is only allowed at the top level of subprograms, module-likes, or programs"#]],
    );
}

#[test]
fn import_stmt_in_module() {
    check("module m import () end m", expect![[]]);
}

#[test]
fn import_stmt_in_subprogram() {
    check("proc p import () end p", expect![[]]);
}

#[test]
fn report_import_stmt_in_body() {
    check(
        "body a import nothing end a",
        expect![[r#"
        error in file FileId(1) at 7..21: useless `import` statement
        | error in file FileId(1) for 7..21: `import` statements are ignored in `body` declaration"#]],
    );
}

#[test]
fn export_stmt_in_module() {
    check("module m export() end m", expect![[]]);
}

#[test]
fn report_export_stmt_in_module_inner() {
    check(
        "module m begin export() end end m",
        expect![[r#"
        error in file FileId(1) at 15..23: cannot use `export` statement here
        | error in file FileId(1) for 15..23: `export` statement is only allowed in module-like blocks"#]],
    );
}

#[test]
fn report_export_stmt_in_program_level() {
    check(
        "export()",
        expect![[r#"
        error in file FileId(1) at 0..8: cannot use `export` statement here
        | error in file FileId(1) for 0..8: `export` statement is only allowed in module-like blocks"#]],
    );
}

#[test]
fn forward_attr_in_forward_decl() {
    check("forward proc uwu import forward owo", expect![[]]);
}

#[test]
fn report_forward_attr_in_plain_import() {
    check(
        "import forward owo",
        expect![[r#"
        error in file FileId(1) at 7..14: cannot use `forward` attribute here
        | error in file FileId(1) for 7..14: `forward` attribute can only be used in `forward` declarations"#]],
    );
}

#[test]
fn external_item_explicit_path_in_program_level() {
    check(r#"import "sus""#, expect![[]])
}

#[test]
fn report_external_item_explicit_path_in_program_level_inner() {
    check(r#"begin import "sus" end"#, expect![[r#"
        error in file FileId(1) at 6..18: cannot use `import` statement here
        | error in file FileId(1) for 6..18: `import` statement is only allowed at the top level of subprograms, module-likes, or programs
        error in file FileId(1) at 13..18: cannot use external path here
        | error in file FileId(1) for 13..18: external paths can only be used in top-level `import` statements"#]])
}

#[test]
fn external_item_explicit_path_in_module_unit_level() {
    check(r#"unit module u import "sus" end u"#, expect![[]])
}

#[test]
fn external_item_explicit_path_in_class_unit() {
    check(r#"unit class m inherit "sus" end m"#, expect![[]])
}

#[test]
fn report_external_item_explicit_path_in_forward_decl() {
    check(r#"forward proc owo import "sus""#, expect![[r#"
        error in file FileId(1) at 24..29: cannot use external path here
        | error in file FileId(1) for 24..29: external paths can only be used in top-level `import` statements"#]])
}

#[test]
fn report_external_item_explicit_path_in_nested_module() {
    check(r#"module m module n import "sus" end n end m"#, expect![[r#"
        error in file FileId(1) at 25..30: cannot use external path here
        | error in file FileId(1) for 25..30: external paths can only be used in top-level `import` statements"#]])
}
