//! Validation for statements and declarations
#[cfg(test)]
mod test;

use toc_syntax::{
    ast::{self, AstNode},
    SyntaxKind,
};

use crate::{block_containing_node, without_matching};
use crate::{BlockKind, ValidateCtx};

pub(super) fn validate_constvar_decl(decl: ast::ConstVarDecl, ctx: &mut ValidateCtx) {
    if let Some(attr) = decl.register_attr() {
        // 'register' attr is not allowed in top-level blocks:

        if block_containing_node(decl.syntax()).is_top_level() {
            ctx.push_error(
                "‘register’ attribute is not allowed at module level",
                attr.syntax().text_range(),
            );
        }
    }

    // TODO: stuff relating to init expr
}

pub(super) fn validate_module_decl(decl: ast::ModuleDecl, ctx: &mut ValidateCtx) {
    // Check contained in location
    if !block_containing_node(decl.syntax()).is_top_level() {
        ctx.push_error(
            "modules can only be declared at the program, module, or monitor level",
            decl.module_token().unwrap().text_range(),
        );
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

pub(super) fn validate_class_decl(decl: ast::ClassDecl, ctx: &mut ValidateCtx) {
    let is_monitor_class = decl.monitor_token().is_some();

    if let Some(dev_spec) = decl.device_spec() {
        assert!(is_monitor_class, "non-monitor class has device spec");

        ctx.push_error(
            "device specification is not allowed for monitor classes",
            dev_spec.syntax().text_range(),
        )
    }

    // Check contained in location
    if is_monitor_class {
        // specialize for monitor classes
        match block_containing_node(decl.syntax()) {
            block if block.is_monitor() => {
                ctx.push_error(
                    "monitor classes cannot be declared inside of monitors",
                    decl.class_token().unwrap().text_range(),
                );
            }
            BlockKind::Class => {
                ctx.push_error(
                    "monitor classes cannot be declared inside of classes",
                    decl.class_token().unwrap().text_range(),
                );
            }
            block if !block.is_top_level() => {
                ctx.push_error(
                    "monitor classes can only be declared at the program, module, or monitor level",
                    decl.class_token().unwrap().text_range(),
                );
            }
            _ => {}
        }
    } else {
        match block_containing_node(decl.syntax()) {
            block if block.is_monitor() => {
                ctx.push_error(
                    "classes cannot be declared inside of monitors",
                    decl.class_token().unwrap().text_range(),
                );
            }
            BlockKind::Class => {
                ctx.push_error(
                    "classes cannot be declared inside of other classes",
                    decl.class_token().unwrap().text_range(),
                );
            }
            block if !block.is_top_level() => {
                ctx.push_error(
                    "classes can only be declared at the program, module, or monitor level",
                    decl.class_token().unwrap().text_range(),
                );
            }
            _ => {}
        }
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

pub(super) fn validate_monitor_decl(decl: ast::MonitorDecl, ctx: &mut ValidateCtx) {
    // Check contained in location
    match block_containing_node(decl.syntax()) {
        block if block.is_monitor() => {
            ctx.push_error(
                "monitors cannot be declared inside of other monitors",
                decl.monitor_token().unwrap().text_range(),
            );
        }
        block if !block.is_top_level() => {
            ctx.push_error(
                "monitors can only be declared at the program, module, or monitor level",
                decl.monitor_token().unwrap().text_range(),
            );
        }
        _ => {}
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

pub(super) fn check_matching_names(
    decl_name: Option<ast::Name>,
    end_group: Option<ast::EndGroup>,
    ctx: &mut ValidateCtx,
) {
    if let Some(decl_name) = decl_name.and_then(|end| end.identifier_token()) {
        if let Some(end_name) = end_group.and_then(|end| end.identifier_token()) {
            if end_name.text() != decl_name.text() {
                ctx.push_error(
                    &format!(
                        "end identifier ‘{}’ does not match ‘{}’",
                        end_name.text(),
                        decl_name.text()
                    ),
                    end_name.text_range(),
                )
                // TODO: add additional diagnostic referring to the declared identifier
            }
        }
    }
}

pub(super) fn validate_else_stmt(stmt: ast::ElseStmt, ctx: &mut ValidateCtx) {
    let parent_kind = stmt.syntax().parent().map(|p| p.kind());

    if !matches!(parent_kind, Some(SyntaxKind::IfBody)) {
        without_matching(stmt.syntax(), "if", ctx);
    }
}

pub(super) fn validate_elseif_stmt(stmt: ast::ElseifStmt, ctx: &mut ValidateCtx) {
    let parent_kind = stmt.syntax().parent().map(|p| p.kind());

    if !matches!(parent_kind, Some(SyntaxKind::IfBody)) {
        without_matching(stmt.syntax(), "if", ctx);
    }
}
