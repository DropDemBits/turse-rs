//! HIR Database related structures

use std::sync::{Arc, Mutex};

use indexmap::IndexMap;
use la_arena::{Arena, Idx};
use toc_span::Span;

use crate::{expr, stmt, symbol, ty, unit};

/// HIR Database Builder
///
/// All state is contained inside of an `Arc`, so this is trivially cloneable
#[derive(Debug, Clone, Default)]
pub struct HirBuilder {
    inner: Arc<Mutex<Inner>>,
}

#[derive(Debug, Default)]
struct Inner {
    symbol_table: symbol::SymbolTable,
    arena: Arena<HirNode>,
    spans: IndexMap<Idx<HirNode>, Span>,
}

impl HirBuilder {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
        }
    }

    pub fn add_expr(&self, node: expr::Expr, span: Span) -> expr::ExprId {
        let mut inner = self.inner.lock().unwrap();

        let idx = inner.arena.alloc(HirNode::Expr(node));
        inner.spans.insert(idx, span);
        expr::ExprId(HirId(idx))
    }

    pub fn add_type(&self, node: ty::Type, span: Span) -> ty::TypeId {
        let mut inner = self.inner.lock().unwrap();

        let idx = inner.arena.alloc(HirNode::Type(node));
        inner.spans.insert(idx, span);
        ty::TypeId(HirId(idx))
    }

    pub fn add_stmt(&self, node: stmt::Stmt, span: Span) -> stmt::StmtId {
        let mut inner = self.inner.lock().unwrap();

        let idx = inner.arena.alloc(HirNode::Stmt(node));
        inner.spans.insert(idx, span);
        stmt::StmtId(HirId(idx))
    }

    pub fn add_unit_with<F>(&self, make_unit: F, span: Span) -> unit::UnitId
    where
        F: FnOnce(unit::UnitId) -> unit::Unit,
    {
        // Create empty placeholder node
        let idx = {
            let mut inner = self.inner.lock().unwrap();
            inner.arena.alloc(HirNode::Empty)
        };

        let unit = make_unit(unit::UnitId(HirId(idx)));

        let mut inner = self.inner.lock().unwrap();
        inner.arena[idx] = HirNode::Unit(unit);
        inner.spans.insert(idx, span);
        unit::UnitId(HirId(idx))
    }

    pub fn def_sym(
        &self,
        name: &str,
        span: Span,
        kind: symbol::SymbolKind,
        is_pervasive: bool,
    ) -> symbol::DefId {
        self.inner
            .lock()
            .unwrap()
            .symbol_table
            .def_sym(name, span, kind, is_pervasive)
    }

    pub fn use_sym(&self, def: symbol::DefId, span: Span) -> symbol::UseId {
        self.inner.lock().unwrap().symbol_table.use_sym(def, span)
    }

    pub fn is_pervasive(&self, def: symbol::DefId) -> bool {
        self.inner
            .lock()
            .unwrap()
            .symbol_table
            .get_symbol(def)
            .is_pervasive
    }

    pub fn get_def_span(&self, def_id: symbol::DefId) -> Span {
        self.inner.lock().unwrap().symbol_table.get_def_span(def_id)
    }

    pub fn get_use_span(&self, use_id: symbol::UseId) -> Span {
        self.inner.lock().unwrap().symbol_table.get_use_span(use_id)
    }

    pub fn finish(self) -> HirDb {
        // Transpose inner state into another `Arc`
        let inner_state =
            Arc::try_unwrap(self.inner).expect("still existing references to inner state");
        let inner = inner_state.into_inner().unwrap();

        let inner = Arc::new(inner);
        HirDb::new(inner)
    }
}

/// Aggregate HIR structure
///
/// All state is stored inside of an `Arc`, so this is trivially cloneable.
#[derive(Debug, Clone)]
pub struct HirDb {
    inner: Arc<Inner>,
}

impl HirDb {
    fn new(inner: Arc<Inner>) -> Self {
        Self { inner }
    }

    pub fn get_span(&self, id: HirId) -> Span {
        self.inner.spans.get(&id.0).copied().unwrap()
    }

    pub fn get_node(&self, id: HirId) -> &HirNode {
        &self.inner.arena[id.0]
    }

    pub fn get_expr(&self, id: expr::ExprId) -> &expr::Expr {
        self.get_node(id.into()).as_expr().unwrap()
    }

    pub fn get_type(&self, id: ty::TypeId) -> &ty::Type {
        self.get_node(id.into()).as_type().unwrap()
    }

    pub fn get_stmt(&self, id: stmt::StmtId) -> &stmt::Stmt {
        self.get_node(id.into()).as_stmt().unwrap()
    }

    pub fn get_unit(&self, id: unit::UnitId) -> &unit::Unit {
        self.get_node(id.into()).as_unit().unwrap()
    }

    pub fn nodes(&self) -> impl Iterator<Item = (HirId, &HirNode)> {
        self.inner.arena.iter().map(|(id, node)| (HirId(id), node))
    }

    pub fn get_symbol(&self, def: symbol::DefId) -> &symbol::Symbol {
        self.inner.symbol_table.get_symbol(def)
    }

    pub fn get_def_span(&self, def_id: symbol::DefId) -> Span {
        self.inner.symbol_table.get_def_span(def_id)
    }

    pub fn get_use_span(&self, use_id: symbol::UseId) -> Span {
        self.inner.symbol_table.get_use_span(use_id)
    }
}

#[derive(Debug)]
#[allow(
    // Already stored inside of an arena, which absorbs the cost of a large variant
    clippy::large_enum_variant
)]
pub enum HirNode {
    /// Placeholder node. Should not be observable outside of the node's construction
    Empty,
    /// Expression node
    Expr(expr::Expr),
    /// Type node
    Type(ty::Type),
    /// Statement node
    Stmt(stmt::Stmt),
    /// Unit node
    Unit(unit::Unit),
}

impl HirNode {
    pub fn as_expr(&self) -> Option<&expr::Expr> {
        match self {
            HirNode::Expr(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&ty::Type> {
        match self {
            HirNode::Type(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_stmt(&self) -> Option<&stmt::Stmt> {
        match self {
            HirNode::Stmt(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_unit(&self) -> Option<&unit::Unit> {
        match self {
            HirNode::Unit(unit) => Some(unit),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId(Idx<HirNode>);
