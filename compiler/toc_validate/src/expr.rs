//! Validation for expressions
#[cfg(test)]
mod test;

use toc_syntax::ast::{self, AstNode};

use crate::{item_block_containing_node, walk_item_blocks, ValidateCtx};

pub(super) fn validate_init_expr(expr: ast::InitExpr, ctx: &mut ValidateCtx) {
    // Only allowed in ConstVar decl
    if expr
        .syntax()
        .parent()
        .and_then(ast::ConstVarDecl::cast)
        .is_none()
    {
        ctx.push_error(
            "cannot use `init` expression here",
            "`init` expression can only be used as a `const` or `var` initializer",
            expr.syntax().text_range(),
        );
    }
}

pub(super) fn validate_self_expr(expr: ast::SelfExpr, ctx: &mut ValidateCtx) {
    // Only allowed in class subprograms
    let mut item_blocks = walk_item_blocks(expr.syntax());

    let is_allowed =
        if let Some((maybe_subprog, maybe_class)) = item_blocks.next().zip(item_blocks.next()) {
            // Must be inside of a subprogram, which is itself inside of a class
            maybe_subprog.is_subprogram() && maybe_class.is_class()
        } else {
            // Not surrounded by anything relevant
            false
        };

    if !is_allowed {
        if item_block_containing_node(expr.syntax()).is_class() {
            // Specialize for when inside class init
            let span = ctx.mk_span(expr.syntax().text_range());

            ctx.push_detailed_error("cannot use `self` here", span)
                .with_error("`self` cannot be used during class initialization", span)
                .with_info("during class initialization, `self` does not refer to anything")
                .finish();
        } else {
            ctx.push_error(
                "cannot use `self` here",
                "`self` is only allowed inside of class subprograms",
                expr.syntax().text_range(),
            )
        }
    }
}
