//! Lowering implementation.
//! Fragmented into submodules by node class
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

use std::collections::VecDeque;

use indexmap::IndexSet;
use toc_ast_db::source::SourceParser;
use toc_hir::{
    body,
    builder::{self, BodyBuilder},
    expr::{Expr, ExprKind},
    item,
    stmt::{Block, BlockKind, Stmt, StmtId, StmtKind},
    symbol::{self, LocalDefId},
    ty::TypeInterner,
};
use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, Span, SpanId, SpanTable, Spanned};
use toc_syntax::ast::{self, AstNode};

use crate::{scopes, LoweringDb, SpannedLibrary};

// Implement for anything that can intern types and provides an AST source.
impl<T> LoweringDb for T
where
    T: SourceParser + TypeInterner,
{
    fn lower_library(&self, library_root: FileId) -> CompileResult<SpannedLibrary> {
        let db = self;

        // Gather all files reachable from the library root
        let mut reachable_files = IndexSet::new();

        let mut pending_files = VecDeque::from(vec![library_root]);
        while let Some(current_file) = pending_files.pop_front() {
            reachable_files.insert(current_file);

            let deps = db.parse_depends(current_file);
            for dep in deps.result().dependencies() {
                let child = db.resolve_path(current_file, &dep.relative_path);
                pending_files.push_back(child);
            }
        }

        // Lower them all
        let mut root_items = vec![];
        let mut messages = vec![];
        let mut interners = Interners::default();

        for file in reachable_files {
            let (item, mut msgs) = FileLowering::lower_file(db, &mut interners, file).take();
            root_items.push((file, item));
            messages.append(&mut msgs);
        }

        let Interners { library, span, .. } = interners;
        let lib = SpannedLibrary::new(library.finish(root_items), span);

        CompileResult::new(lib, messages)
    }
}

/// Interners that are shared between library files and aren't part of the database.
///
/// Contains delegated methods to ease use.
#[derive(Default)]
struct Interners {
    library: builder::LibraryBuilder,
    span: SpanTable,
}

impl Interners {
    /// Helper for declaring a new symbol.
    ///
    /// ## Parameters
    /// - `name`: The name of the symbol to define
    /// - `span`: The text span of the definition
    /// - `kind`: The kind of symbol to define
    ///
    /// ## Returns
    /// The [`LocalDefId`] associated with the definition
    pub fn add_def(&mut self, name: &str, span: SpanId, kind: symbol::SymbolKind) -> LocalDefId {
        self.library.add_def(symbol::DefInfo {
            name: Spanned::new(name.to_string(), span),
            kind,
        })
    }
}

struct FileLowering<'db, 'ctx> {
    file: FileId,
    db: &'db dyn LoweringDb,
    interns: &'ctx mut Interners,
    scopes: scopes::ScopeTracker,
    messages: MessageSink,
}

impl<'db, 'ctx> FileLowering<'db, 'ctx> {
    fn new(file: FileId, db: &'db dyn LoweringDb, interners: &'ctx mut Interners) -> Self {
        Self {
            file,
            db,
            interns: interners,
            messages: MessageSink::new(),
            scopes: scopes::ScopeTracker::new(),
        }
    }

    fn lower_file(
        db: &'db dyn LoweringDb,
        interners: &'ctx mut Interners,
        file: FileId,
    ) -> CompileResult<item::ItemId> {
        let mut ctx = Self::new(file, db, interners);

        // Parse & validate file
        let parse_res = db.parse_file(file);
        let validate_res = db.validate_file(file);

        // Enter the actual lowering
        let root = ast::Source::cast(parse_res.result().syntax()).unwrap();
        let root_item = ctx.lower_root(root);

        let Self { messages, .. } = ctx;

        // Bundle up messages
        let mut bundled_messages = messages.finish();
        parse_res.bundle_messages(&mut bundled_messages);
        validate_res.bundle_messages(&mut bundled_messages);

        CompileResult::new(root_item, bundled_messages)
    }

    fn lower_root(&mut self, root: ast::Source) -> item::ItemId {
        // lower root as a module
        // - lower stmts as part of a body

        let (body, declared_items) = self.lower_stmt_body(root.stmt_list().unwrap(), vec![]);

        let module_def = self.interns.add_def(
            "<root>",
            self.interns.span.dummy_span(),
            symbol::SymbolKind::Declared,
        );
        let module_span = self
            .interns
            .span
            .intern_span(Span::new(Some(self.file), root.syntax().text_range()));
        let module = item::Module {
            as_monitor: false,
            declares: declared_items,
            body,
        };

        self.interns.library.add_item(item::Item {
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

        let Interners {
            library,
            span: span_interner,
        } = &mut self.interns;

        // Collect declared items
        let declared_items = {
            let mut declared_items = vec![];

            for stmt in body_stmts.iter().map(|id| body.stmt(*id)) {
                match &stmt.kind {
                    StmtKind::Item(item) => declared_items.push(*item),
                    StmtKind::Block(Block {
                        kind: BlockKind::ItemGroup,
                        stmts,
                    }) => {
                        // Is an item group, guaranteed to have items
                        let items = stmts.iter().map(|id| match &body.stmt(*id).kind {
                            StmtKind::Item(item) => *item,
                            _ => unreachable!(),
                        });

                        declared_items.extend(items);
                    }
                    _ => {}
                }
            }

            declared_items
        };

        // Actually make the body
        let span = span_interner.intern_span(Span::new(Some(self.file), span));

        let body = body.finish_stmts(body_stmts, param_defs, span);
        let body = library.add_body(body);

        (body, declared_items)
    }

    fn lower_expr_body(&mut self, expr: ast::Expr) -> body::BodyId {
        // Lower expr
        let mut body = builder::BodyBuilder::default();
        let root_expr = BodyLowering::new(self, &mut body).lower_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.interns.library.add_body(body)
    }

    fn lower_empty_expr_body(&mut self) -> body::BodyId {
        // Lower expr
        let expr = Expr {
            kind: ExprKind::Missing,
            span: self.interns.span.dummy_span(),
        };
        let mut body = builder::BodyBuilder::default();
        let root_expr = body.add_expr(expr);

        // Actually make the body
        let body = body.finish_expr(root_expr);
        self.interns.library.add_body(body)
    }

    fn mk_span(&self, range: toc_span::TextRange) -> Span {
        Span::new(Some(self.file), range)
    }

    fn intern_range(&mut self, range: toc_span::TextRange) -> SpanId {
        let span = self.mk_span(range);
        self.interns.span.intern_span(span)
    }
}

/// For lowering things within a body
struct BodyLowering<'db, 'ctx: 'body, 'body> {
    ctx: &'body mut FileLowering<'db, 'ctx>,
    body: &'body mut BodyBuilder,
}

impl<'db, 'ctx: 'body, 'body> BodyLowering<'db, 'ctx, 'body> {
    fn new(ctx: &'body mut FileLowering<'db, 'ctx>, body: &'body mut BodyBuilder) -> Self {
        Self { ctx, body }
    }

    fn lower_stmt_list(&mut self, stmt_list: ast::StmtList) -> Vec<StmtId> {
        let mut stmts = vec![];

        for node in stmt_list.stmts() {
            let range = node.syntax().text_range();
            let kind = {
                if let Some(kind) = self.lower_stmt(node) {
                    kind
                } else {
                    continue;
                }
            };

            let span = self
                .ctx
                .interns
                .span
                .intern_span(Span::new(Some(self.ctx.file), range));
            let id = self.body.add_stmt(Stmt { kind, span });
            stmts.push(id);
        }

        stmts
    }
}
