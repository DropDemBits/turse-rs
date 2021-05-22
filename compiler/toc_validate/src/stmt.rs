//! Validation for statements and declarations
#[cfg(test)]
mod test;

use toc_span::Span;
use toc_syntax::{
    ast::{self, AstNode},
    SyntaxKind,
};
use toc_syntax::{IoKind, SyntaxNode};

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
            if !matches!(
                ty,
                ast::Type::ArrayType(_) | ast::Type::RecordType(_) | ast::Type::UnionType(_)
            ) {
                let span = Span::new(ctx.file, ty.syntax().text_range());

                ctx.push_detailed_error(
                    "‘init’ initializer is not allowed here",
                    init_expr.syntax().text_range(),
                )
                .with_info(
                    "‘init’ initializer can only be used with array, record, or union types",
                    span,
                )
                .finish();
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
                    let span = Span::new(ctx.file, array_ty.syntax().text_range());

                    let builder = if let Some(init) = decl.init() {
                        let report_here = init.syntax().text_range();
                        ctx.push_detailed_error("‘init’ initializer is required here", report_here)
                    } else {
                        let report_after = array_ty.syntax().text_range();
                        ctx.push_detailed_error(
                            "‘init’ initializer is required after here",
                            report_after,
                        )
                    };

                    builder
                        .with_note("this is an unbounded array type", span)
                        .with_info("unbounded arrays have their upper bounds specified by ‘init’ initializers", None)
                        .finish();
                }
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
        let span = Span::new(ctx.file, binary_cap.1);

        ctx.push_detailed_error("cannot use ‘get’/‘put’ with ‘read’/‘write’", text_cap.1)
            .with_note("first conflicting binary capability", span)
            .finish();
    }
}

pub(super) fn validate_for_stmt(stmt: ast::ForStmt, ctx: &mut ValidateCtx) {
    if let Some(bounds) = stmt.for_bounds() {
        if stmt.decreasing_token().is_some() && bounds.range_token().is_none() {
            if let Some(first) = bounds.from() {
                // Missing other range part
                ctx.push_error(
                    "decreasing for-loop requires explicit end bound",
                    first.syntax().text_range(),
                );
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
            "Missing ‘label’ arms for ‘case’ statement",
            stmt.syntax().text_range(),
        );
    } else {
        // At least 1 arm present, check if it's a default arm
        if let Some((ArmKind::Default, arm_range)) = all_arms.first() {
            ctx.push_error(
                "First ‘label’ arm must have at least one selector expression",
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

            if extra_arm == last_arm {
                // Single extra arm
                ctx.push_error("Extra ‘label’ arm found after default arm", full_range);
            } else {
                // Many extra arms
                ctx.push_error("Extra ‘label’ arms found after default arm", full_range);
            }
        }
    }
}

pub(super) fn validate_invariant_stmt(stmt: ast::InvariantStmt, ctx: &mut ValidateCtx) {
    let kind = block_containing_node(stmt.syntax());
    if kind != BlockKind::Loop && !kind.is_module_kind() {
        ctx.push_error(
            "‘invariant’ statement is only allowed in loop statements and module-kind declarations",
            stmt.syntax().text_range(),
        );
    }
}

pub(super) fn validate_in_module_kind(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !block_containing_node(node).is_module_kind() {
        ctx.push_error(
            &format!("{} is only allowed in module-like blocks", kind),
            node.text_range(),
        );
    }
}

pub(super) fn validate_in_top_level(node: &SyntaxNode, kind: &str, ctx: &mut ValidateCtx) {
    if !block_containing_node(node).is_top_level() {
        ctx.push_error(
            &format!("{} is only allowed at module-like or program level", kind),
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
            let span = Span::new(ctx.file, decl_name.text_range());

            if end_name.text() != decl_name.text() {
                ctx.push_detailed_error(
                    &format!(
                        "end identifier ‘{}’ does not match ‘{}’",
                        end_name.text(),
                        decl_name.text()
                    ),
                    end_name.text_range(),
                )
                .with_note("defined here", span)
                .finish();
            }
        }
    }
}
