//! Helper builders for creating the HIR tree

use la_arena::{Arena, Idx};
use toc_span::{FileId, Span};

use crate::{
    body, expr, item, package,
    span::{HasSpanTable, SpanId, SpanTable},
    stmt, symbol, ty,
};

/// Builder for constructing a [`Package`]
///
/// [`Package`]: package::Package
pub struct PackageBuilder {
    package: package::Package,
}

impl PackageBuilder {
    pub fn new(span_map: SpanTable, defs: symbol::DefInfoTable) -> Self {
        Self {
            package: package::Package {
                span_map,
                defs,

                root_items: Default::default(),
                items: Default::default(),
                bodies: Default::default(),
                type_map: Default::default(),
                resolve_map: Default::default(),
            },
        }
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
    /// - `kind`: The kind of symbol to define, or None if it's undeclared
    ///
    /// ## Returns
    /// The [`LocalDefId`] associated with the definition
    ///
    /// [`LocalDefId`]: crate::symbol::LocalDefId
    pub fn add_def(
        &mut self,
        name: symbol::Symbol,
        span: SpanId,
        kind: Option<symbol::SymbolKind>,
        pervasive: symbol::IsPervasive,
    ) -> symbol::LocalDefId {
        self.defs.add_def(name, span, kind, pervasive)
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

    pub fn freeze_root_items(self, root_items: Vec<(FileId, item::ItemId)>) -> Self {
        Self {
            package: package::Package {
                // Convert from vec of tuples to an index map
                root_items: root_items.into_iter().collect(),
                ..self.package
            },
        }
    }

    pub fn finish(self, resolve_map: symbol::ResolutionMap) -> package::Package {
        let Self { package, .. } = self;

        package::Package {
            resolve_map,
            ..package
        }
    }
}

impl std::ops::Deref for PackageBuilder {
    type Target = package::Package;

    fn deref(&self) -> &Self::Target {
        &self.package
    }
}

impl std::ops::DerefMut for PackageBuilder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.package
    }
}

impl HasSpanTable for PackageBuilder {
    fn span_table(&self) -> &SpanTable {
        self.package.span_table()
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
        &self.exprs[Idx::from(expr_id)]
    }

    pub fn stmt(&self, stmt_id: stmt::StmtId) -> &stmt::Stmt {
        &self.stmts[Idx::from(stmt_id)]
    }

    /// Finish as an expression body
    pub fn finish_expr(self, root_expr: expr::ExprId) -> body::Body {
        let Self { exprs, stmts } = self;
        debug_assert!(stmts.is_empty());

        // Body span is the same as the root expression
        let root_span = exprs[Idx::from(root_expr)].span;
        body::Body {
            kind: body::BodyKind::Exprs(root_expr),
            span: root_span,
            exprs,
            stmts,
        }
    }

    /// Finish as a statement group body
    pub fn finish_stmts(
        self,
        body_stmts: Vec<stmt::StmtId>,
        param_defs: Vec<symbol::LocalDefId>,
        result_name: Option<symbol::LocalDefId>,
        span: SpanId,
    ) -> body::Body {
        let Self { exprs, stmts } = self;

        body::Body {
            kind: body::BodyKind::Stmts(body_stmts, param_defs, result_name),
            span,
            exprs,
            stmts,
        }
    }
}
