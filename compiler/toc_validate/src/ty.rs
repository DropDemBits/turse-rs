//! Validation for statements and declarations
#[cfg(test)]
mod test;

use toc_syntax::ast::{self, AstNode};

use crate::ValidateCtx;

pub(super) fn validate_function_type(ty: ast::FcnType, ctx: &mut ValidateCtx) {
    if ty.params().is_none() {
        let span = ctx.mk_span(ty.syntax().text_range());
        let place_after = ty.name().map_or_else(
            || ty.function_token().unwrap().text_range(),
            |node| node.syntax().text_range(),
        );
        let place_after = ctx.mk_span(place_after);

        ctx.sink
            .warn_detailed("parameterless function types should specify `()`", span)
            .with_warn("add `()` after here", place_after)
            .with_info("referring to a parameterless function will call it instead of producing a reference to it")
            .finish();
    }
}