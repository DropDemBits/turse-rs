//! Lowering implementation.
//! Fragmented into submodules by node class
//!
//! Note: Anything that is considered invalid syntax but still HIR representable should
//! be moved into `toc_validator`
#![allow(clippy::unnecessary_wraps)] // Top level lowering points also return Option

// lower_library
// - lower_file(A)
//   -
// - lower_file(B)
//   - ...
// - ...

// Library requires lowering files
// Lowering files requires lowering them as module items
// Lowering module items involves lowering bodies
// Lowering bodies also involves lowering bodies
// (need to recursively lower bodies from bodies)
// - so lower_expr_body and lower_stmt_body needs to be in BodyLowering

mod expr;
mod stmt;
mod ty;

use std::sync::Arc;

use toc_ast_db::db::SourceParser;
use toc_hir::library_graph::{GraphBuilder, LibraryGraph};
use toc_hir::{
    body,
    builder::{self, BodyBuilder},
    expr::{Expr, ExprKind},
    item,
    stmt::{Stmt, StmtId, StmtKind},
    symbol::{self, LocalDefId},
};
use toc_reporting::{CompileResult, MessageBundle, MessageSink};
use toc_span::{FileId, Span, SpanId};
use toc_syntax::ast::{self, AstNode};

use crate::{scopes, LoweredLibrary, LoweringDb};

// Implement for anything that can provide an AST source.
impl<T> LoweringDb for T
where
    T: SourceParser,
{
    fn lower_library(&self, library_root: FileId) -> CompileResult<LoweredLibrary> {
        let db = self;

        let reachable_files: Vec<_> = self.depend_graph(library_root).unit_sources().collect();

        // Lower all files reachable from the library root
        let mut root_items = vec![];
        let mut messages = MessageBundle::default();
        let mut library = builder::LibraryBuilder::default();

        for file in reachable_files {
            let (item, msgs) = FileLowering::lower_file(db, &mut library, file).take();
            root_items.push((file, item));
            messages = messages.combine(msgs);
        }

        let lib = library.finish(root_items);

        CompileResult::new(Arc::new(lib), messages)
    }

    fn lower_library_graph(&self) -> CompileResult<LibraryGraph> {
        let mut messages = MessageBundle::default();
        let source_graph = self.source_graph();
        let mut graph = GraphBuilder::new();

        for root in source_graph.library_roots() {
            graph.add_library(root);
            self.lower_library(root).bundle_messages(&mut messages);
        }

        CompileResult::new(graph.finish(), messages)
    }
}

struct FileLowering<'ctx> {
    file: FileId,
    library: &'ctx mut builder::LibraryBuilder,
    scopes: scopes::ScopeTracker,
    messages: MessageSink,
}

impl<'ctx> FileLowering<'ctx> {
    fn new(file: FileId, library: &'ctx mut builder::LibraryBuilder) -> Self {
        Self {
            file,
            library,
            messages: MessageSink::new(),
            scopes: scopes::ScopeTracker::new(),
        }
    }

    fn lower_file(
        db: &dyn LoweringDb,
        library: &'ctx mut builder::LibraryBuilder,
        file: FileId,
    ) -> CompileResult<item::ItemId> {
        // Parse & validate file
        let parse_res = db.parse_file(file);
        let validate_res = db.validate_file(file);

        // Enter the actual lowering
        let mut ctx = Self::new(file, library);
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

        let (body, declared_items) = self.lower_stmt_body(root.stmt_list().unwrap(), vec![]);

        let module_def = self.library.add_def(
            "<root>",
            self.library.span_map.dummy_span(),
            symbol::SymbolKind::Declared,
        );
        let module_span = self
            .library
            .intern_span(Span::new(Some(self.file), root.syntax().text_range()));
        let module = item::Module {
            as_monitor: false,
            def_id: module_def,
            declares: declared_items,
            body,
        };

        self.library.add_item(item::Item {
            kind: item::ItemKind::Module(module),
            def_id: module_def,
            span: module_span,
        })
    }

    fn lower_stmt_body(
        &mut self,
        stmt_list: ast::StmtList,
        param_defs: Vec<LocalDefId>,
    ) -> (body::BodyId, Vec<item::ItemId>) {
        // Lower stmts
        let span = stmt_list.syntax().text_range();
        let mut body = builder::BodyBuilder::default();
        let body_stmts = BodyLowering::new(self, &mut body).lower_stmt_list(stmt_list);

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
        let span = self.library.intern_span(Span::new(Some(self.file), span));

        let body = body.finish_stmts(body_stmts, param_defs, span);
        let body = self.library.add_body(body);

        (body, declared_items)
    }

    fn lower_expr_body(&mut self, expr: ast::Expr) -> body::BodyId {
        // Lower expr
        let mut body = builder::BodyBuilder::default();
        let root_expr = BodyLowering::new(self, &mut body).lower_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.library.add_body(body)
    }

    fn lower_empty_expr_body(&mut self) -> body::BodyId {
        // Lower expr
        let expr = Expr {
            kind: ExprKind::Missing,
            span: self.library.span_map.dummy_span(),
        };
        let mut body = builder::BodyBuilder::default();
        let root_expr = body.add_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.library.add_body(body)
    }

    fn mk_span(&self, range: toc_span::TextRange) -> Span {
        Span::new(Some(self.file), range)
    }

    fn intern_range(&mut self, range: toc_span::TextRange) -> SpanId {
        let span = self.mk_span(range);
        self.library.intern_span(span)
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
            let lowered = if let Some(lowered) = self.lower_stmt(node) {
                lowered
            } else {
                continue;
            };

            let span = self
                .ctx
                .library
                .intern_span(Span::new(Some(self.ctx.file), range));

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
