//! Validation for statements and declarations
#[cfg(test)]
mod test;

use toc_syntax::{
    ast::{self, AstNode},
    match_ast,
};

use crate::{block_containing_node, ValidateCtx};

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

pub(super) fn validate_set_type(ty: ast::SetType, ctx: &mut ValidateCtx) {
    // Must be in a type decl
    if ty.syntax().parent().and_then(ast::TypeDecl::cast).is_none() {
        let span = ctx.mk_span(ty.syntax().text_range());
        ctx.sink.error(
            "`set` type is not allowed here",
            "`set` types can only be specified in `type` declarations",
            span,
        );
    }
}

pub(super) fn validate_union_type(ty: ast::UnionType, ctx: &mut ValidateCtx) {
    // At least one variant must be present
    if ty.union_variant().next().is_none() {
        let span = ctx.mk_span(ty.syntax().text_range());
        ctx.sink.error(
            "missing `union` variants",
            "at least one `union` variant must be present",
            span,
        );
    }
}

pub(super) fn validate_collection_type(ty: ast::CollectionType, ctx: &mut ValidateCtx) {
    // Must be in a `var` decl
    let cv_decl = if let Some(cv_decl) = ty.syntax().parent().and_then(ast::ConstVarDecl::cast) {
        cv_decl
    } else {
        let span = ctx.mk_span(ty.syntax().text_range());
        ctx.sink.error(
            "`collection` type is not allowed here",
            "`collection` types can only be specified in `var` declarations",
            span,
        );

        return;
    };

    if cv_decl.var_token().is_none() {
        let span = ctx.mk_span(ty.syntax().text_range());
        ctx.sink
            .error_detailed("`collection` type is not allowed here", span)
            .with_error(
                "`collection` type cannot be used in a `const` variable",
                span,
            )
            .with_info(
                "adding or removing elements from a `collection` requires allowing changes to it",
            )
            .finish();

        return;
    }

    // Must be at the top level of a module-like
    let item_block = block_containing_node(ty.syntax());

    if !item_block.is_top_level() {
        let span = ctx.mk_span(ty.syntax().text_range());

        let mut builder = ctx.sink
            .error_detailed("`collection` type is not allowed here", span)
            .with_error("`collection` types can only be specified at the top-level of module-likes or programs", span);

        if item_block.is_subprogram() {
            // how to word persistence?
            builder = builder.with_info(
                "`collection` variables need to live for longer than a single subprogram calls",
            );
            // FIXME(reporter): add "help: move this declaration outside of the {block_kind}"
        }

        builder.finish();
    }
}

pub(super) fn validate_condition_type(ty: ast::ConditionType, ctx: &mut ValidateCtx) {
    // Must be in a var decl, or in an array in a var decl
    // - Don't need to worry about if it's in index or elem pos, that's handled by typeck
    let cv_decl = ty.syntax().parent().and_then(|parent| {
        match_ast! {
            match (parent) {
                ast::RangeList(_node) => {
                    // In array index position
                    // This is reported by typeck as well, though default to failing
                    None
                },
                ast::ArrayType(node) => {
                    // In array element position
                    // Use the node's parent
                    node.syntax().parent().and_then(ast::ConstVarDecl::cast)
                },
                ast::ConstVarDecl(node) => {
                    // In a ConstVar decl
                    // Use the node itself
                    Some(node)
                },
                _ => None,
            }
        }
    });

    let cv_decl = if let Some(cv_decl) = cv_decl {
        cv_decl
    } else {
        let span = ctx.mk_span(ty.syntax().text_range());
        ctx.sink.error(
            "`condition` type is not allowed here",
            "`condition` types can only be specified in `var` declarations, or as the `array` element in `var` declarations",
            span,
        );

        return;
    };

    if cv_decl.var_token().is_none() {
        let span = ctx.mk_span(ty.syntax().text_range());
        ctx.sink
            .error_detailed("`condition` type is not allowed here", span)
            .with_error(
                "`condition` type cannot be specified in a `const` variable",
                span,
            )
            .with_info(
                "adding or removing processes from a `condition` requires allowing changes to it",
            )
            .finish();

        return;
    }

    // Must be at the top level of a monitor
    let item_block = block_containing_node(cv_decl.syntax());
    if !item_block.is_monitor() {
        let span = ctx.mk_span(ty.syntax().text_range());

        let mut builder = ctx
            .sink
            .error_detailed("`condition` type is not allowed here", span)
            .with_error(
                "`condition` types can only be specified at the top-level of monitors",
                span,
            );

        if item_block.is_subprogram() {
            // how to word persistence?
            builder = builder.with_info(
                "`condition` variables need to live for longer than a single subprogram calls",
            );
            // FIXME(reporter): add "help: move this declaration outside of the {block_kind}"
        }

        builder.finish();
    }
}
