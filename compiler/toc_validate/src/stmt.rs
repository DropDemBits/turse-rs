//! Validation for statements and declarations
#[cfg(test)]
mod test;

use toc_syntax::{
    ast::{self, AstNode},
    IoKind, SyntaxKind, SyntaxNode,
};

use crate::{
    block_containing_node, item_block_containing_node, walk_blocks, without_matching, BlockKind,
    ValidateCtx,
};

pub(super) fn validate_constvar_decl(decl: ast::ConstVarDecl, ctx: &mut ValidateCtx) {
    if let Some(attr) = decl.register_attr() {
        // 'register' attr is not allowed in top-level blocks:

        if block_containing_node(decl.syntax()).is_top_level() {
            ctx.push_error(
                "cannot use `register` here",
                "`register` attribute is not allowed at module-like or program level",
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

    // Note: array case + `init` initializer is handled during lowering to HIR

    if let Some(ast::Expr::InitExpr(init_expr)) = decl.init() {
        // Has init expr initializer, allowed here?
        if let Some(ty) = decl.type_spec() {
            // Yes type spec, only allowed for array, record, or union types
            if !matches!(
                ty,
                ast::Type::ArrayType(_) | ast::Type::RecordType(_) | ast::Type::UnionType(_)
            ) {
                let span = ctx.mk_span(ty.syntax().text_range());
                let init_span = ctx.mk_span(init_expr.syntax().text_range());

                ctx.push_detailed_error("mismatched initializer", init_span)
                    .with_error("`init` initializer is not allowed here", init_span)
                    .with_error("cannot use `init` initializer with this type", span)
                    .with_info(
                        "`init` initializer can only be used with array, record, or union types",
                    )
                    .finish();
            }
        } else {
            // No type spec, never allowed
            let span = ctx.mk_span(init_expr.syntax().text_range());

            ctx.push_detailed_error("mismatched initializer", span)
                .with_error("`init` initializer is not allowed here", span)
                .with_info("`init` initializer requires a type to be specified")
                .finish();
        }
    }
}

pub(super) fn validate_bind_decl(decl: ast::BindDecl, ctx: &mut ValidateCtx) {
    if block_containing_node(decl.syntax()).is_top_level() {
        ctx.push_error(
            "cannot use `bind` here",
            "`bind` declaration is not allowed at module-like or program level",
            decl.syntax().text_range(),
        );
    }
}

pub(super) fn validate_proc_decl(decl: ast::ProcDecl, ctx: &mut ValidateCtx) {
    validate_in_top_level(decl.syntax(), "`procedure` declaration", ctx);

    check_matching_names(
        decl.proc_header().and_then(|header| header.name()),
        decl.end_group(),
        ctx,
    );
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
                "not part of a `procedure` declaration",
                dev_spec.syntax().text_range(),
            );
        } else {
            let mut parent_blocks = walk_blocks(node.syntax());
            debug_assert_eq!(parent_blocks.next(), Some(BlockKind::Procedure)); // over parent proc decl

            if !matches!(parent_blocks.next(), Some(BlockKind::MonitorDevice)) {
                // not in a device monitor
                ctx.push_error(
                    "device specification is not allowed here",
                    "`procedure` is not in a device monitor",
                    dev_spec.syntax().text_range(),
                );
            }
        }
    }
}

pub(super) fn validate_fcn_decl(decl: ast::FcnDecl, ctx: &mut ValidateCtx) {
    validate_in_top_level(decl.syntax(), "`function` declaration", ctx);

    check_matching_names(
        decl.fcn_header().and_then(|header| header.name()),
        decl.end_group(),
        ctx,
    );
}

pub(super) fn validate_process_decl(decl: ast::ProcessDecl, ctx: &mut ValidateCtx) {
    let location_error = match block_containing_node(decl.syntax()) {
        BlockKind::Class | BlockKind::MonitorClass => {
            Some("`process` declarations are not allowed in classes or monitor classes")
        }
        kind if kind.is_top_level() => None,
        _ => Some(
            "`process` declaration is only allowed at the top level of `monitor`s and `module`s",
        ),
    };

    if let Some(error) = location_error {
        ctx.push_error(
            "cannot declare a `process` here",
            error,
            decl.syntax().text_range(),
        );
    }

    check_matching_names(
        decl.process_header().and_then(|header| header.name()),
        decl.end_group(),
        ctx,
    );
}

pub(super) fn validate_external_var(decl: ast::ExternalVar, ctx: &mut ValidateCtx) {
    ctx.push_error(
        "unsupported declaration",
        "`external` variables are not supported in this compiler",
        decl.syntax().text_range(),
    );
}

pub(super) fn validate_deferred_decl(decl: ast::DeferredDecl, ctx: &mut ValidateCtx) {
    validate_in_module_kind(decl.syntax(), "`deferred`", ctx);
}

pub(super) fn validate_body_decl(decl: ast::BodyDecl, ctx: &mut ValidateCtx) {
    validate_in_top_level(decl.syntax(), "`body` declaration", ctx);

    let body_name = decl.body_kind().and_then(|kind| match kind {
        ast::BodyKind::PlainHeader(decl) => decl.name(),
        ast::BodyKind::ProcHeader(decl) => decl.name(),
        ast::BodyKind::FcnHeader(decl) => decl.name(),
    });
    check_matching_names(body_name, decl.end_group(), ctx);
}

pub(super) fn validate_module_decl(decl: ast::ModuleDecl, ctx: &mut ValidateCtx) {
    // Check contained in location
    if !block_containing_node(decl.syntax()).is_top_level() {
        ctx.push_error(
            "cannot declare a `module` here",
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
            "device specification is not allowed here",
            "device specification is not allowed for monitor classes",
            dev_spec.syntax().text_range(),
        )
    }

    // Check contained in location
    let (item_thing, maybe_other_classes) = if is_monitor_class {
        // specialize for monitor classes
        ("monitor class", "classes")
    } else {
        ("class", "other classes")
    };

    let location_error = {
        match block_containing_node(decl.syntax()) {
            BlockKind::Class => Some(format!(
                "{item_thing}es cannot be declared inside of {maybe_other_classes}"
            )),
            block if block.is_monitor() => Some(format!(
                "{item_thing}es cannot be declared inside of monitors"
            )),
            block if !block.is_top_level() => Some(format!(
                "{item_thing}es can only be declared at the program, module, or monitor level"
            )),
            _ => None,
        }
    };

    if let Some(error) = location_error {
        ctx.push_error(
            format!("cannot declare a `{item_thing}` here"),
            error,
            decl.class_token().unwrap().text_range(),
        );
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

pub(super) fn validate_monitor_decl(decl: ast::MonitorDecl, ctx: &mut ValidateCtx) {
    // Check contained in location
    let location_error = match block_containing_node(decl.syntax()) {
        block if block.is_monitor() => Some("monitors cannot be declared inside of other monitors"),
        block if block.is_top_level() => None,
        _ => Some("monitors can only be declared at the program, module, or monitor level"),
    };

    if let Some(error) = location_error {
        ctx.push_error(
            "cannot declare a `monitor` here",
            error,
            decl.monitor_token().unwrap().text_range(),
        );
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

pub(super) fn validate_new_open(open: ast::NewOpen, ctx: &mut ValidateCtx) {
    // Check for conflicting io caps
    let mut used_caps = vec![];

    for cap in open.io_caps() {
        if let Some(kind) = cap.io_kind() {
            if !used_caps.iter().any(|(k, _)| *k == kind) {
                // don't insert duplicates
                used_caps.push((kind, cap.syntax().text_range()));
            }
        }
    }

    // Conflicting caps:
    // - Any text cap with a binary cap present (and the other way too)
    let find_cap_pair: _ = |a, b| used_caps.iter().find(|cap| cap.0 == a || cap.0 == b);

    let text_cap = find_cap_pair(IoKind::Get, IoKind::Put);
    let binary_cap = find_cap_pair(IoKind::Read, IoKind::Write);

    if let Some((text_cap, binary_cap)) = text_cap.zip(binary_cap) {
        // Conflicting io pair
        let text_span = ctx.mk_span(text_cap.1);
        let binary_span = ctx.mk_span(binary_cap.1);

        ctx.push_detailed_error("cannot use `get`/`put` with `read`/`write`", text_span)
            .with_note("first conflicting binary capability", binary_span)
            .finish();
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

pub(super) fn validate_for_stmt(stmt: ast::ForStmt, ctx: &mut ValidateCtx) {
    let is_decreasing = stmt.decreasing_token().is_some();
    let for_bounds = stmt.for_bounds().unwrap();
    // Is only implicit bounds if there's an expression, but no range token
    let is_implicit_bounds = for_bounds.start().is_some() && for_bounds.range_token().is_none();

    // If `decreasing` is present, then the for-loop must have both bounds defined
    if is_decreasing && is_implicit_bounds {
        let bounds_span = ctx.mk_span(for_bounds.syntax().text_range());
        let decreasing_span = ctx.mk_span(stmt.decreasing_token().unwrap().text_range());

        // ???: Are we able to include the potentially implied bounds in the error message?
        // Not during validation since it requires name resolution and type lookup
        // It's only be useful for suggestions on how to fix it
        ctx.push_detailed_error(
            "`decreasing` for-loops cannot use implicit range bounds",
            bounds_span,
        )
        .with_error("range bounds are implied from here", bounds_span)
        .with_note("`decreasing` for-loop specified here", decreasing_span)
        .with_info("`decreasing` for-loops can only use explicit range bounds (e.g. `1 .. 2`)")
        .finish();
    }
}

pub(super) fn validate_exit_stmt(stmt: ast::ExitStmt, ctx: &mut ValidateCtx) {
    // Report if we're outside of a loop or for statement
    if !walk_blocks(stmt.syntax()).any(|kind| kind == BlockKind::Loop) {
        ctx.push_error(
            "cannot use `exit` statement here",
            "can only be used inside of `loop` and `for` statements",
            stmt.syntax().text_range(),
        );
    }
}

pub(super) fn validate_case_stmt(stmt: ast::CaseStmt, ctx: &mut ValidateCtx) {
    #[derive(Clone, Copy)]
    enum ArmKind {
        Normal,
        Default,
    }

    let all_arms: Vec<_> = stmt
        .case_arm()
        .map(|arm| {
            let kind = if arm.select().is_some() {
                ArmKind::Normal
            } else {
                ArmKind::Default
            };

            (kind, arm.syntax().text_range())
        })
        .collect();

    // Accept Situations:
    // ArmKind::Normal+ ArmKind::Default?

    // Reject Conditions
    // [None] -> Missing arm
    // <ArmKind::Default> -> At least one case arm should have things
    // ArmKind::Normal* ArmKind::Default <(ArmKind::Normal | ArmKind::Default)+> -> Extra case arms found

    if all_arms.is_empty() {
        ctx.push_error(
            "invalid `case` statement",
            "missing `label` arms",
            stmt.syntax().text_range(),
        );
    } else {
        // At least 1 arm present, check if it's a default arm
        if let Some((ArmKind::Default, arm_range)) = all_arms.first() {
            ctx.push_error(
                "cannot have a default `label` arm as the first `case` arm",
                "First `label` arm must have at least one selector expression",
                *arm_range,
            );
        }

        // If there are many arms present, find the first arm after the default arm
        let mut past_default_arm = all_arms
            .iter()
            .skip_while(|(kind, _)| matches!(kind, ArmKind::Normal))
            .skip(1);

        if let Some((_, extra_arm)) = past_default_arm.next() {
            // At least one extra arm is found
            let (_, last_arm) = all_arms.last().expect("last arm always expected");
            let full_range = extra_arm.cover(*last_arm);
            let arms = if extra_arm == last_arm {
                // Single extra arm
                "arm"
            } else {
                // Many extra arms
                "arms"
            };

            ctx.push_error(
                format!("extra `label` {arms} found after default arm"),
                format!("extra `label` {arms}"),
                full_range,
            );
        }
    }
}

pub(super) fn validate_invariant_stmt(stmt: ast::InvariantStmt, ctx: &mut ValidateCtx) {
    match block_containing_node(stmt.syntax()) {
        BlockKind::Loop => (),
        kind if kind.is_module_kind() => (),
        _ => ctx.push_error(
            "cannot use `invariant` here",
            "`invariant` statement is only allowed in loop statements and module-kind declarations",
            stmt.syntax().text_range(),
        ),
    }
}

pub(super) fn validate_return_stmt(stmt: ast::ReturnStmt, ctx: &mut ValidateCtx) {
    // Note: see `validate_result_stmt` for why we don't check exactly what kind of body we're in
    let location_error = match item_block_containing_node(stmt.syntax()) {
        BlockKind::Function => {
            Some("`result` statement is used to return values in function bodies")
        }
        BlockKind::Main => None,
        kind if kind.is_module_kind() => None,
        kind if kind.is_subprogram() => None,
        _ => Some(
            "`return` statement is only allowed in subprogram bodies and module-kind declarations",
        ),
    };

    if let Some(location_error) = location_error {
        ctx.push_error(
            "cannot use `return` here",
            location_error,
            stmt.syntax().text_range(),
        );
    }
}

pub(super) fn validate_result_stmt(stmt: ast::ResultStmt, ctx: &mut ValidateCtx) {
    // Note: We can't reject `result` from all `procedure` bodies, since that requires
    // either binding kind lookup, or knowing the return type as `ty::Void` disambiguates
    // between functions and procedures
    match item_block_containing_node(stmt.syntax()) {
        BlockKind::Function | BlockKind::Body => (),
        _ => {
            ctx.push_error(
                "cannot use `result` here",
                "`result` statement is only allowed in function bodies",
                stmt.syntax().text_range(),
            );
        }
    }
}

pub(super) fn validate_inherit_stmt(stmt: ast::InheritStmt, ctx: &mut ValidateCtx) {
    if !block_containing_node(stmt.syntax()).is_class() {
        ctx.push_error(
            "cannot use `inherit` statement here",
            "`inherit` statement is only allowed in classes",
            stmt.syntax().text_range(),
        );
    }
}

pub(super) fn validate_import_stmt(stmt: ast::ImportStmt, ctx: &mut ValidateCtx) {
    // Valid in top level of program module-likes, and subprogram bodies (except for `body`)
    let block = block_containing_node(stmt.syntax());

    if !(matches!(block, BlockKind::Main)
        || block.is_module_kind()
        || (block.is_subprogram() && !matches!(block, BlockKind::Body)))
    {
        if matches!(block, BlockKind::Body) {
            // Specialize message for body blocks
            ctx.push_error(
                "useless `import` statement",
                "`import` statements are ignored in `body` declaration",
                stmt.syntax().text_range(),
            )
        } else {
            ctx.push_error(
                "cannot use `import` statement here",
                "`import` statement is only allowed at the top level of subprograms, module-likes, or programs",
                stmt.syntax().text_range(),
            );
        }
    }
}

pub(super) fn validate_import_item(item: ast::ImportItem, ctx: &mut ValidateCtx) {
    let forward_attr = item.attrs().and_then(|attr| match attr {
        ast::ImportAttr::ForwardAttr(attr) => Some(attr),
        _ => None,
    });

    if let Some(forward_attr) = forward_attr {
        // Only allowed when inside of a `ForwardDecl`
        let import_list = item
            .syntax()
            .parent()
            .and_then(ast::ImportList::cast)
            .unwrap();

        let inside_forward_decl = import_list
            .syntax()
            .parent()
            .map(|node| ast::ForwardDecl::can_cast(&node))
            .unwrap_or(false);

        if !inside_forward_decl {
            ctx.push_error(
                "cannot use `forward` attribute here",
                "`forward` attribute can only be used in `forward` declarations",
                forward_attr.syntax().text_range(),
            );
        }
    }
}

pub(super) fn validate_external_item(item: ast::ExternalItem, ctx: &mut ValidateCtx) {
    // Only caring about external items with paths
    let path_node = if let Some(node) = item.path() {
        node
    } else {
        return;
    };

    // If we're in an import list, it must not be inside of a forward decl
    let import_list = item
        .syntax()
        .ancestors()
        .nth(2)
        .and_then(ast::ImportList::cast);

    if let Some(import_list) = import_list {
        let inside_forward_decl = import_list
            .syntax()
            .parent()
            .map(|node| ast::ForwardDecl::can_cast(&node))
            .unwrap_or(false);

        if inside_forward_decl {
            ctx.push_error(
                "cannot use external path here",
                "external paths can only be used in top-level `import` statements",
                path_node.syntax().text_range(),
            );

            return;
        }
    }

    // We need to be in something containing an import stmt, then that should be contained in the root level
    let mut ancestor_blocks = walk_blocks(item.syntax());

    let is_external_path_allowed = match (ancestor_blocks.next(), ancestor_blocks.next()) {
        // Allowed in immediate top-level
        (Some(BlockKind::Main | BlockKind::Unit), None) => true,
        // Or allowed in a module-like contained in a unit
        (Some(kind), Some(BlockKind::Unit)) => kind.is_module_kind(),
        _ => false,
    };

    if !is_external_path_allowed {
        ctx.push_error(
            "cannot use external path here",
            "external paths can only be used in top-level `import` statements",
            path_node.syntax().text_range(),
        );
    }
}

// Helpers //

pub(super) fn validate_in_module_kind(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !block_containing_node(node).is_module_kind() {
        ctx.push_error(
            format!("cannot use {kind} here"),
            format!("{kind} is only allowed in module-like blocks"),
            node.text_range(),
        );
    }
}

pub(super) fn validate_in_top_level(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !block_containing_node(node).is_top_level() {
        ctx.push_error(
            format!("cannot use {kind} here"),
            format!("{kind} is only allowed at module-like or program level"),
            node.text_range(),
        );
    }
}

pub(super) fn validate_in_subprogram(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !block_containing_node(node).is_subprogram() {
        ctx.push_error(
            format!("cannot use {kind} here"),
            format!("{kind} is only allowed at the top level of subprograms"),
            node.text_range(),
        );
    }
}

pub(super) fn validate_in_module_or_subprogram(
    node: &SyntaxNode,
    kind: &str,
    ctx: &mut ValidateCtx,
) {
    let block = block_containing_node(node);
    if !block.is_top_level() && !block.is_subprogram() {
        ctx.push_error(
            format!("cannot use {kind} here"),
            format!("{kind} is only allowed at the top level of module-likes and subprograms"),
            node.text_range(),
        );
    }
}

fn check_matching_names(
    decl_name: Option<ast::Name>,
    end_group: Option<ast::EndGroup>,
    ctx: &mut ValidateCtx,
) {
    if let Some(decl_name) = decl_name.and_then(|end| end.identifier_token()) {
        if let Some(end_name) = end_group.and_then(|end| end.identifier_token()) {
            let decl_span = ctx.mk_span(decl_name.text_range());
            let end_span = ctx.mk_span(end_name.text_range());

            if end_name.text() != decl_name.text() {
                ctx.push_detailed_error(
                    "mismatched identifier names",
                    ctx.mk_span(end_name.text_range()),
                )
                .with_note(
                    format!("`{}` does not match...", decl_name.text()),
                    decl_span,
                )
                .with_note(format!("...`{}` defined here", end_name.text()), end_span)
                .finish();
            }
        }
    }
}
