//! Validation for statements and declarations
#[cfg(test)]
mod test;

use toc_syntax::SyntaxNode;
use toc_syntax::{
    ast::{self, AstNode},
    SyntaxKind,
};

use crate::{block_containing_node, walk_blocks, without_matching};
use crate::{BlockKind, ValidateCtx};

pub(super) fn validate_constvar_decl(decl: ast::ConstVarDecl, ctx: &mut ValidateCtx) {
    if let Some(attr) = decl.register_attr() {
        // 'register' attr is not allowed in top-level blocks:

        if block_containing_node(decl.syntax()).is_top_level() {
            ctx.push_error(
                "‘register’ attribute is not allowed at module-like or program level",
                attr.syntax().text_range(),
            );
        }
    }

    // Check for the required presence or absence of an init expr

    // Require Conditions:
    // Require(Expr::InitExpr)
    //     if decl.type_spec() is Some(Type::ArrayType(array_ty))
    //        and array_ty.ranges() contains unbounded range

    // Accept Conditions:
    // Accept(Expr::InitExpr) if decl.type_spec() is Some(Type::ArrayType | Type::RecordType | Type::UnionType)

    // Reject Conditions:
    // Reject(Expr::InitExpr) if not (decl.type_spec() is Some(Type::ArrayType | Type::RecordType | Type::UnionType))
    // Reject(_)
    //     if decl.type_spec() is Some(Type::ArrayType(array_ty))
    //        and array_ty.ranges() contains unbounded range

    if let Some(ast::Expr::InitExpr(init_expr)) = decl.init() {
        // Has init expr initializer, allowed here?
        if let Some(ty) = decl.type_spec() {
            // Yes type spec, only allowed for array, record, or union types
            if !matches!(ty, ast::Type::ArrayType(_) | ast::Type::RecordType(_) | ast::Type::UnionType(_))
            {
                ctx.push_error(
                    "‘init’ initializer is not allowed here",
                    init_expr.syntax().text_range(),
                );
                // TODO: add additional diagnostic referring to the type spec not being an array, record, or union type
            }
        } else {
            // No type spec, never allowed
            ctx.push_error(
                "‘init’ initializer is not allowed here",
                init_expr.syntax().text_range(),
            );
        }
    } else if let Some(ast::Type::ArrayType(array_ty)) = decl.type_spec() {
        // Has array type spec, is unbounded?
        // An array is unbounded if at least one of its ranges is unbounded
        if let Some(ranges) = array_ty.range_list() {
            let mut ranges = ranges.ranges();

            // Check if the array is unbounded
            let is_unbounded = loop {
                let range = if let Some(range) = ranges.next() {
                    range
                } else {
                    break false;
                };

                if let ast::Type::RangeType(range_ty) = range {
                    let is_unbounded = range_ty
                        .end()
                        .map(|end_bound| matches!(end_bound, ast::EndBound::UnsizedBound(_)))
                        .unwrap_or_default();

                    if is_unbounded {
                        break true;
                    }
                }
            };

            if is_unbounded {
                // init expr is required
                if !matches!(decl.init(), Some(ast::Expr::InitExpr(_))) {
                    // Report at either the initializer expr, or the array type spec
                    if let Some(init) = decl.init() {
                        let report_here = init.syntax().text_range();
                        ctx.push_error("‘init’ initializer is required here", report_here);
                    } else {
                        let report_after = array_ty.syntax().text_range();
                        ctx.push_error("‘init’ initializer is required after here", report_after);
                    }
                }
                // TODO: add additional diagnostic referring to the type spec saying that it's because it's an unbounded array
            }
        }
    }
}

pub(super) fn validate_bind_decl(decl: ast::BindDecl, ctx: &mut ValidateCtx) {
    if block_containing_node(decl.syntax()).is_top_level() {
        ctx.push_error(
            "‘bind’ declaration is not allowed at module-like or program level",
            decl.syntax().text_range(),
        );
    }
}

pub(super) fn validate_proc_header(node: ast::ProcHeader, ctx: &mut ValidateCtx) {
    if let Some(dev_spec) = node.device_spec() {
        // Check if we're in a device monitor block, at the top level
        if !matches!(
            node.syntax().parent().map(|parent| parent.kind()),
            Some(SyntaxKind::ProcDecl)
        ) {
            // not allowed here
            ctx.push_error(
                "device specification is not allowed here",
                dev_spec.syntax().text_range(),
            );
        } else {
            let mut parent_blocks = walk_blocks(node.syntax());
            debug_assert_eq!(parent_blocks.next(), Some(BlockKind::Procedure)); // over parent proc decl

            if !matches!(parent_blocks.next(), Some(BlockKind::MonitorDevice)) {
                // not in a device monitor
                ctx.push_error(
                    "device specification is not allowed here",
                    dev_spec.syntax().text_range(),
                );
            }
        }
    }
}

pub(super) fn validate_process_decl(decl: ast::ProcessDecl, ctx: &mut ValidateCtx) {
    let parent_kind = block_containing_node(decl.syntax());

    if !parent_kind.is_top_level() {
        ctx.push_error(
            "‘process’ declaration is only allowed at the top level of ‘monitor’s and ‘module’s",
            decl.syntax().text_range(),
        );
    } else if matches!(parent_kind, BlockKind::Class | BlockKind::MonitorClass) {
        ctx.push_error(
            "‘process’ declarations is not allowed in classes or monitor classes",
            decl.syntax().text_range(),
        );
    }
}

pub(super) fn validate_external_var(decl: ast::ExternalVar, ctx: &mut ValidateCtx) {
    ctx.push_error(
        "‘external’ variables are not supported in this compiler",
        decl.syntax().text_range(),
    );
}

pub(super) fn validate_deferred_decl(decl: ast::DeferredDecl, ctx: &mut ValidateCtx) {
    validate_in_module_kind(decl.syntax(), "‘deferred’ declaration", ctx);
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

fn check_matching_names(
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

pub(super) fn validate_in_module_kind(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !dbg!(block_containing_node(node)).is_module_kind() {
        ctx.push_error(
            &format!("{} is only allowed in module-like blocks", kind),
            node.text_range(),
        );
    }
}

pub(super) fn validate_in_top_level(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !dbg!(block_containing_node(node)).is_top_level() {
        ctx.push_error(
            &format!("{} is only allowed at module-like or program level", kind),
            node.text_range(),
        );
    }
}
