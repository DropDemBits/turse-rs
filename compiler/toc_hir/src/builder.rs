//! Helper builders for creating the HIR tree

use la_arena::Arena;
use toc_span::{FileId, Span, SpanId, Spanned};

use crate::{body, expr, item, library, stmt, symbol, ty};

/// Builder for constructing a [`Library`]
///
/// [`Library`]: library::Library
pub struct LibraryBuilder {
    library: library::Library,
}

impl LibraryBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_item(&mut self, item: item::Item) -> item::ItemId {
        let index = self.items.alloc(item);
        item::ItemId(index)
    }

    /// Helper for declaring a new symbol.
    ///
    /// ## Parameters
    /// - `name`: The name of the symbol to define
    /// - `span`: The text span of the definition
    /// - `kind`: The kind of symbol to define
    ///
    /// ## Returns
    /// The [`LocalDefId`] associated with the definition
    pub fn add_def(
        &mut self,
        name: &str,
        span: SpanId,
        kind: symbol::SymbolKind,
    ) -> symbol::LocalDefId {
        let def = symbol::DefInfo {
            name: Spanned::new(name.to_string(), span),
            kind,
        };
        let index = self.defs.alloc(def);
        symbol::LocalDefId(index)
    }

    pub fn add_body(&mut self, body: body::Body) -> body::BodyId {
        let index = self.bodies.alloc(body);
        body::BodyId(index)
    }

    pub fn intern_type(&mut self, ty: ty::Type) -> ty::TypeId {
        self.type_map.intern_type(ty)
    }

    pub fn intern_span(&mut self, span: Span) -> SpanId {
        self.span_map.intern_span(span)
    }

    pub fn make_body_with(
        &mut self,
        build: impl FnOnce(BodyBuilder) -> body::Body,
    ) -> body::BodyId {
        let body = build(BodyBuilder::default());
        self.add_body(body)
    }

    pub fn finish(self, root_items: Vec<(FileId, item::ItemId)>) -> library::Library {
        let Self { library } = self;

        library::Library {
            root_items: root_items.into_iter().collect(),
            ..library
        }
    }
}

impl Default for LibraryBuilder {
    fn default() -> Self {
        Self {
            library: library::Library {
                root_items: Default::default(),
                items: Default::default(),
                defs: Default::default(),
                bodies: Default::default(),
                type_map: Default::default(),
                span_map: Default::default(),
            },
        }
    }
}

impl std::ops::Deref for LibraryBuilder {
    type Target = library::Library;

    fn deref(&self) -> &Self::Target {
        &self.library
    }
}

impl std::ops::DerefMut for LibraryBuilder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.library
    }
}

/// Builder for constructing a [`Body`]
///
/// [`Body`]: body::Body
#[derive(Default)]
pub struct BodyBuilder {
    exprs: Arena<expr::Expr>,
    stmts: Arena<stmt::Stmt>,
}

impl BodyBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_expr(&mut self, expr: expr::Expr) -> expr::ExprId {
        let index = self.exprs.alloc(expr);
        expr::ExprId(index)
    }

    pub fn add_stmt(&mut self, stmt: stmt::Stmt) -> stmt::StmtId {
        let index = self.stmts.alloc(stmt);
        stmt::StmtId(index)
    }

    pub fn expr(&self, expr_id: expr::ExprId) -> &expr::Expr {
        &self.exprs[expr_id.into()]
    }

    pub fn stmt(&self, stmt_id: stmt::StmtId) -> &stmt::Stmt {
        &self.stmts[stmt_id.into()]
    }

    /// Finish as an expression body
    pub fn finish_expr(self, root_expr: expr::ExprId) -> body::Body {
        let Self { exprs, stmts } = self;
        debug_assert!(stmts.is_empty());

        // Body span is the same as the root expression
        let root_span = exprs[root_expr.into()].span;
        body::Body {
            kind: body::BodyKind::Exprs(root_expr),
            span: root_span,
            exprs,
            stmts,
        }
    }

    /// Finish as a statment group body
    pub fn finish_stmts(
        self,
        body_stmts: Vec<stmt::StmtId>,
        param_defs: Vec<symbol::LocalDefId>,
        span: SpanId,
    ) -> body::Body {
        let Self { exprs, stmts } = self;

        body::Body {
            kind: body::BodyKind::Stmts(body_stmts, param_defs),
            span,
            exprs,
            stmts,
        }
    }
}
