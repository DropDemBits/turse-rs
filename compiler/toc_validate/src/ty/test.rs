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

#[test]
fn set_type_in_type_decl() {
    check("type _ : set of boolean", expect![[]]);
}

#[test]
fn report_set_type_in_var() {
    check(
        "var _ : set of boolean",
        expect![[r#"
        error in file FileId(1) at 8..22: `set` type is not allowed here
        | error in file FileId(1) for 8..22: `set` types can only be specified in `type` declarations"#]],
    );
}

// collection type:
// - must be in a var decl
//   - not ty alias
//   - not const
// - that var decl must be at top level
//   - not in subprog
//   - not in inner

#[test]
fn collection_type_in_var_decl() {
    check("var _ : collection of int", expect![[]]);
}

#[test]
fn report_collection_type_in_const_decl() {
    check(
        "const _ : collection of int",
        expect![[r#"
        error in file FileId(1) at 24..27: unexpected end of file
        | error in file FileId(1) for 24..27: expected `:=` after here
        error in file FileId(1) at 10..27: `collection` type is not allowed here
        | error in file FileId(1) for 10..27: `collection` type cannot be used in a `const` variable
        | info: adding or removing elements from a `collection` requires allowing changes to it"#]],
    );
}

#[test]
fn report_collection_type_in_type_decl() {
    check(
        "type _ : collection of int",
        expect![[r#"
        error in file FileId(1) at 9..26: `collection` type is not allowed here
        | error in file FileId(1) for 9..26: `collection` types can only be specified in `var` declarations"#]],
    );
}

#[test]
fn collection_type_in_module() {
    check("module a var _ : collection of int end a", expect![[]]);
}

#[test]
fn report_collection_type_in_module_subprogram() {
    check(
        "module a proc b var _ : collection of int end b end a",
        expect![[r#"
            error in file FileId(1) at 24..41: `collection` type is not allowed here
            | error in file FileId(1) for 24..41: `collection` types can only be specified at the top-level of module-likes or programs
            | info: `collection` variables need to live for longer than a single subprogram calls"#]],
    );
}

#[test]
fn report_collection_type_in_module_inner() {
    check(
        "module a begin var _ : collection of int end end a",
        expect![[r#"
            error in file FileId(1) at 23..40: `collection` type is not allowed here
            | error in file FileId(1) for 23..40: `collection` types can only be specified at the top-level of module-likes or programs"#]],
    );
}

// condition type
// - must be in a var decl
//   - not ty alias
//   - not const
// - or must be in array in var decl
//    - any position is safe
//    - except nested inside of an array
// - that var decl must be at top level of monitor
//   - not in subprog
//   - not in inner

#[test]
fn condition_type_in_var_decl() {
    check(
        "monitor m var _ : condition end m",
        expect![[r#""#]],
    );
}

#[test]
fn report_condition_type_in_const_decl() {
    check(
        "monitor m const _ : condition end m",
        expect![[r#"
            error in file FileId(1) at 30..33: unexpected token
            | error in file FileId(1) for 30..33: expected `:=`, but found `end`
            error in file FileId(1) at 20..29: `condition` type is not allowed here
            | error in file FileId(1) for 20..29: `condition` type cannot be specified in a `const` variable
            | info: adding or removing processes from a `condition` requires allowing changes to it"#]],
    );
}

#[test]
fn report_condition_type_in_type_decl() {
    check(
        "monitor m type _ : condition end m",
        expect![[r#"
            error in file FileId(1) at 19..28: `condition` type is not allowed here
            | error in file FileId(1) for 19..28: `condition` types can only be specified in `var` declarations, or as the `array` element in `var` declarations"#]],
    );
}

#[test]
fn condition_type_in_var_decl_array_element() {
    check(
        "monitor m var _ : array 1 .. 2 of condition end m",
        expect![[r#""#]],
    );
}

#[test]
fn report_condition_type_in_var_decl_array_index() {
    check(
        "monitor m var _ : array condition of int end m",
        expect![[r#"
            error in file FileId(1) at 24..33: `condition` type is not allowed here
            | error in file FileId(1) for 24..33: `condition` types can only be specified in `var` declarations, or as the `array` element in `var` declarations"#]],
    );
}

#[test]
fn report_condition_type_in_var_decl_array_array_element() {
    check(
        "monitor m var _ : array 1 .. 2 of array 3 .. 4 of condition end m",
        expect![[r#"
            error in file FileId(1) at 50..59: `condition` type is not allowed here
            | error in file FileId(1) for 50..59: `condition` types can only be specified in `var` declarations, or as the `array` element in `var` declarations"#]],
    );
}

#[test]
fn report_condition_type_in_monitor_subprogram() {
    check(
        "monitor m proc p var _ : condition end p end m",
        expect![[r#"
            error in file FileId(1) at 25..34: `condition` type is not allowed here
            | error in file FileId(1) for 25..34: `condition` types can only be specified at the top-level of monitors
            | info: `condition` variables need to live for longer than a single subprogram calls"#]],
    );
}

#[test]
fn report_condition_type_in_monitor_inner() {
    check(
        "monitor m begin var _ : condition end end m",
        expect![[r#"
            error in file FileId(1) at 24..33: `condition` type is not allowed here
            | error in file FileId(1) for 24..33: `condition` types can only be specified at the top-level of monitors"#]],
    );
}

#[test]
fn report_condition_type_in_module() {
    check(
        "module m var _ : condition end m",
        expect![[r#"
            error in file FileId(1) at 17..26: `condition` type is not allowed here
            | error in file FileId(1) for 17..26: `condition` types can only be specified at the top-level of monitors"#]],
    );
}

#[test]
fn report_condition_type_in_program() {
    check(
        "var _ : condition",
        expect![[r#"
            error in file FileId(1) at 8..17: `condition` type is not allowed here
            | error in file FileId(1) for 8..17: `condition` types can only be specified at the top-level of monitors"#]],
    );
}
