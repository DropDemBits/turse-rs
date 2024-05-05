//! Lowering implementation.
//! Fragmented into submodules by node class
//!
//! Note: Anything that is considered invalid syntax but still HIR representable should
//! be moved into `toc_validator`
#![allow(clippy::unnecessary_wraps)] // Top level lowering points also return Option

// lower_package
// - lower_file(A)
//   -
// - lower_file(B)
//   - ...
// - ...

// Package requires lowering files
// Lowering files requires lowering them as module items
// Lowering module items involves lowering bodies
// Lowering bodies also involves lowering bodies
// (need to recursively lower bodies from bodies)
// - so lower_expr_body and lower_stmt_body needs to be in BodyLowering

mod expr;
mod stmt;
mod ty;

use std::collections::HashMap;
use std::sync::Arc;

use toc_hir::package_graph::SourcePackage;
use toc_hir::symbol::{syms, IsMonitor, IsPervasive, NodeSpan, SymbolKind};
use toc_hir::{
    body,
    builder::{self, BodyBuilder},
    expr::{Expr, ExprKind},
    item,
    span::{HasSpanTable, SpanId},
    stmt::{Stmt, StmtId, StmtKind},
    symbol::LocalDefId,
};
use toc_reporting::{CompileResult, MessageBundle, MessageSink};
use toc_span::{Span, TextRange};
use toc_syntax::ast::{self, AstNode};
use toc_vfs_db::SourceFile;

use crate::{Db, LoweredPackage};

/// Lowers the given package into a HIR package.
///
/// ## Returns
///
/// Returns the [`LoweredPackage`] of the newly lowered HIR tree.
#[salsa::tracked]
pub fn lower_package(db: &dyn Db, package: SourcePackage) -> CompileResult<LoweredPackage> {
    let mut messages = MessageBundle::default();

    // take the package from the source graph
    let package_root = package.root(db.up());
    let root_source = toc_vfs_db::source_of(db.up(), package_root);

    // Report if the root file is missing
    if let Some(err) = root_source.errors(db.up()) {
        // Report the missing file
        // Note: we can't lookup the path directly because we don't
        // have access to the file info table from just a source file
        // without having to import toc_vfs_db
        let mut report = MessageSink::new();
        report
            .error_detailed(
                format!("{err}"),
                Span::new(package_root.into(), TextRange::empty(0.into())),
            )
            .finish();
        messages.aggregate(&report.finish());
    }

    // Reachable files isn't the way to go
    // Unfortunately, we can't quite precollect defs since that also depends on expansion
    //
    // Note that unit files (i.e. files linked from by import) serve as expansion roots
    // we could probably do precollection again by recording what items are bound to
    // which scopes, and then expanding from there (since once we've gone through
    // expanding, the scopes are final)
    //
    // for now though, this just replicates the old behavior

    let reachable_files = toc_ast_db::reachable_imported_files(db.up(), root_source)
        .iter()
        .copied()
        .chain(std::iter::once(root_source))
        .collect::<Vec<_>>();

    // Collect all the defs in the package
    let (collect_res, msgs) = crate::collector::collect_defs(db, &reachable_files).take();
    messages = messages.combine(msgs);

    let mut root_items = vec![];
    let mut package = builder::PackageBuilder::new(collect_res.spans, collect_res.defs);

    // Lower all files reachable from the package root
    for file in reachable_files {
        let (item, msgs) = FileLowering::new(db, file, &mut package, &collect_res.node_defs)
            .lower_file()
            .take();
        messages = messages.combine(msgs);
        root_items.push((file.path(db.up()).into(), item));
    }

    let package = package.freeze_root_items(root_items);

    // FIXME: This needs be punted to after all HIR trees are constructed,
    // as we need to know the module exports of a foreign package
    let (resolve_map, resolve_msgs) = crate::resolver::resolve_defs(&package).take();
    messages = messages.combine(resolve_msgs);

    let pkg = package.finish(resolve_map);

    CompileResult::new(Arc::new(pkg), messages)
}

/// Lowers the entire source graph
#[salsa::tracked]
pub fn lower_source_graph(db: &dyn Db) -> CompileResult<()> {
    let mut messages = MessageBundle::default();
    let source_graph = toc_source_graph::source_graph(db.up())
        .as_ref()
        .ok()
        .unwrap();

    for &package in source_graph.all_packages(db.up()) {
        lower_package(db, package).bundle_messages(&mut messages);
    }

    CompileResult::new((), messages)
}

struct FileLowering<'ctx> {
    db: &'ctx dyn Db,
    file: SourceFile,
    package: &'ctx mut builder::PackageBuilder,
    node_defs: &'ctx HashMap<NodeSpan, LocalDefId>,
    messages: MessageSink,
}

impl<'ctx> FileLowering<'ctx> {
    fn new(
        db: &'ctx dyn Db,
        file: SourceFile,
        package: &'ctx mut builder::PackageBuilder,
        node_defs: &'ctx HashMap<NodeSpan, LocalDefId>,
    ) -> Self {
        Self {
            db,
            file,
            package,
            node_defs,
            messages: MessageSink::new(),
        }
    }

    fn lower_file(self) -> CompileResult<item::ItemId> {
        // Parse & validate file
        let parse_res = toc_ast_db::parse_file(self.db.up(), self.file);
        let validate_res = toc_ast_db::validate_file(self.db.up(), self.file);

        // Enter the actual lowering
        let mut ctx = self;
        let root = ast::Source::cast(parse_res.result().syntax()).unwrap();
        let root_item = ctx.lower_root(root);

        let Self { messages, .. } = ctx;

        // Bundle up messages
        let mut bundle = messages.finish();
        parse_res.bundle_messages(&mut bundle);
        validate_res.bundle_messages(&mut bundle);

        CompileResult::new(root_item, bundle)
    }

    fn lower_root(&mut self, root: ast::Source) -> item::ItemId {
        // lower root as a module
        // - lower stmts as part of a body

        // We aren't handling external imports yet.
        if let Some(import) = root.import_stmt() {
            let span = self.mk_span(import.syntax().text_range());
            self.messages.error(
                "unsupported statement",
                "importing from other files is not supported yet",
                span,
            );
        }

        // FIXME: Handle external imports at somepoint
        // They'll get resolved to the final target later

        let (body, declared_items) = self.lower_stmt_body(root.stmt_list().unwrap(), vec![], None);

        let module_def = self.package.add_def(
            *syms::Root,
            self.package.span_table().dummy_span(),
            Some(SymbolKind::Module(IsMonitor::No)),
            IsPervasive::No,
        );
        let module_span = self.intern_range(root.syntax().text_range());
        let module = item::Module {
            as_monitor: false,
            def_id: module_def,
            declares: declared_items,
            imports: vec![],
            exports: vec![],
            body,
        };

        self.package.add_item(item::Item {
            kind: item::ItemKind::Module(module),
            def_id: module_def,
            span: module_span,
        })
    }

    fn lower_stmt_body(
        &mut self,
        stmt_list: ast::StmtList,
        param_defs: Vec<LocalDefId>,
        result_name: Option<LocalDefId>,
    ) -> (body::BodyId, Vec<item::ItemId>) {
        // Lower stmts
        let span = stmt_list.syntax().text_range();
        let mut body = builder::BodyBuilder::default();
        let body_stmts = {
            let body_stmts = BodyLowering::new(self, &mut body).lower_stmt_list(stmt_list);

            body_stmts
        };

        // Collect declared items
        let declared_items = {
            let mut declared_items = vec![];

            for stmt_id in &body_stmts {
                let stmt = body.stmt(*stmt_id);

                if let StmtKind::Item(item) = &stmt.kind {
                    declared_items.push(*item);
                }
            }

            declared_items
        };

        // Actually make the body
        let span = self.intern_range(span);

        let body = body.finish_stmts(body_stmts, param_defs, result_name, span);
        let body = self.package.add_body(body);

        (body, declared_items)
    }

    /// ConstVar and StackSize gets a special exception for now
    fn lower_constvar_expr_body(&mut self, expr: ast::Expr) -> body::BodyId {
        // Lower expr
        let mut body = builder::BodyBuilder::default();
        let root_expr = BodyLowering::new(self, &mut body).lower_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.package.add_body(body)
    }

    /// Bind gets a special exception for now
    fn lower_required_bind_expr_body(&mut self, expr: Option<ast::Expr>) -> body::BodyId {
        match expr {
            Some(expr) => self.lower_constvar_expr_body(expr),
            None => self.lower_empty_expr_body(),
        }
    }

    fn lower_expr_body(&mut self, expr: ast::CompTimeExpr) -> body::BodyId {
        let expr = expr.expr().unwrap();

        // Lower expr
        let mut body = builder::BodyBuilder::default();
        let root_expr = BodyLowering::new(self, &mut body).lower_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.package.add_body(body)
    }

    fn lower_empty_expr_body(&mut self) -> body::BodyId {
        // Lower expr
        let expr = Expr {
            kind: ExprKind::Missing,
            span: self.package.span_table().dummy_span(),
        };
        let mut body = builder::BodyBuilder::default();
        let root_expr = body.add_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.package.add_body(body)
    }

    fn mk_span(&self, range: toc_span::TextRange) -> Span {
        Span::new(self.file.path(self.db.up()).into(), range)
    }

    fn intern_range(&mut self, range: toc_span::TextRange) -> SpanId {
        let span = self.mk_span(range);
        self.package.intern_span(span)
    }

    /// Constructs a [`NodeSpan`] from an AST node's [`TextRange`]
    fn node_span(&mut self, range: toc_span::TextRange) -> NodeSpan {
        NodeSpan(self.intern_range(range))
    }

    /// Finds the def bound to a specific AST node.
    /// Assumes that there is a def at the given `node_span`
    pub fn node_def(&self, node_span: NodeSpan) -> LocalDefId {
        self.node_defs[&node_span]
    }

    /// Gathers the defs associated with `name_list`,
    /// holding up the invariant that it always contains at least one identifier.
    ///
    /// `no_name_span` is used if there isn't any names in `name_list`
    fn collect_name_defs(
        &mut self,
        name_list: impl Iterator<Item = ast::Name>,
        no_name_span: SpanId,
    ) -> Vec<LocalDefId> {
        let mut names = self.collect_optional_name_defs(name_list);

        if names.is_empty() {
            // maintain invariant that there's at least one name
            names.push(
                self.package
                    .add_def(*syms::Unnamed, no_name_span, None, IsPervasive::No),
            )
        }

        names
    }

    /// Gathers the defs associated with `name_list`,
    /// filling empty name places with `fill_with`
    fn collect_name_defs_with_missing(
        &mut self,
        name_list: impl Iterator<Item = Option<ast::Name>>,
        fill_with: LocalDefId,
    ) -> Vec<LocalDefId> {
        let mut names = name_list
            .map(|name| match name {
                Some(name) => self.collect_required_name(name),
                None => fill_with,
            })
            .collect::<Vec<_>>();

        if names.is_empty() {
            // maintain invariant that there's at least one name
            names.push(fill_with)
        }

        names
    }

    /// Gathers the defs associated with `name_list`, but allows
    /// no names to be present
    fn collect_optional_name_defs(
        &mut self,
        name_list: impl Iterator<Item = ast::Name>,
    ) -> Vec<LocalDefId> {
        name_list
            .map(|name| self.collect_required_name(name))
            .collect::<Vec<_>>()
    }

    fn collect_name(&mut self, name: Option<ast::Name>, no_name_span: SpanId) -> LocalDefId {
        match name {
            Some(name) => self.collect_required_name(name),
            None => self
                .package
                .add_def(*syms::Unnamed, no_name_span, None, IsPervasive::No),
        }
    }

    fn collect_required_name(&mut self, name: ast::Name) -> LocalDefId {
        let name_tok = name.identifier_token().unwrap();
        let node_span = self.node_span(name_tok.text_range());
        self.node_def(node_span)
    }

    fn collect_optional_name(&mut self, name: Option<ast::Name>) -> Option<LocalDefId> {
        Some(self.collect_required_name(name?))
    }
}

/// For lowering things within a body
struct BodyLowering<'ctx: 'body, 'body> {
    ctx: &'body mut FileLowering<'ctx>,
    body: &'body mut BodyBuilder,
}

impl<'ctx: 'body, 'body> BodyLowering<'ctx, 'body> {
    fn new(ctx: &'body mut FileLowering<'ctx>, body: &'body mut BodyBuilder) -> Self {
        Self { ctx, body }
    }

    /// Does not automatically enclose the lowered statements in a a scope
    fn lower_stmt_list(&mut self, stmt_list: ast::StmtList) -> Vec<StmtId> {
        let mut stmts = vec![];

        for node in stmt_list.stmts() {
            let range = node.syntax().text_range();
            let Some(lowered) = self.lower_stmt(node) else {
                continue;
            };

            let span = self.ctx.intern_range(range);

            let mut add_kind = |kind| {
                let id = self.body.add_stmt(Stmt { kind, span });
                stmts.push(id);
            };

            match lowered {
                LoweredStmt::Single(kind) => add_kind(kind),
                LoweredStmt::Multiple(kinds) => kinds.into_iter().for_each(add_kind),
            }
        }

        stmts
    }
}

/// Either a single lowered statement, or a group of lowered statements
enum LoweredStmt {
    Single(StmtKind),
    Multiple(Vec<StmtKind>),
}
