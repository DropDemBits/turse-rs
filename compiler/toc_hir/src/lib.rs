//! Crate containing all of the HIR node representations
//!
//! Note: All `expr`, `stmt`, and `ty` nodes are to be used with the module's
//! prefix, e.g. `expr::Name` instead of importing the node directly

use std::collections::HashMap;

use la_arena::{Arena, Idx};
use toc_span::Span;

use crate::symbol::SymbolTable;
pub mod expr;
pub mod stmt;
pub mod symbol;
pub mod ty;
mod unit_map;

pub use unit_map::{UnitId, UnitMap, UnitMapBuilder};

#[derive(Debug)]
pub enum HirNode {
    /// Expression node
    Expr(expr::Expr),
    /// Type node
    Type(ty::Type),
    /// Statement node
    Stmt(stmt::Stmt),
}

impl HirNode {
    fn as_expr(&self) -> Option<&expr::Expr> {
        match self {
            HirNode::Expr(node) => Some(node),
            _ => None,
        }
    }

    fn as_type(&self) -> Option<&ty::Type> {
        match self {
            HirNode::Type(node) => Some(node),
            _ => None,
        }
    }

    fn as_stmt(&self) -> Option<&stmt::Stmt> {
        match self {
            HirNode::Stmt(node) => Some(node),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId(Idx<HirNode>);

/// Code Unit
#[derive(Debug)]
pub struct Unit {
    /// Id in the `UnitMap`
    pub id: UnitId,
    /// All nodes associated with the unit
    pub database: Database,
    /// Top level statements in the unit
    pub stmts: Vec<stmt::StmtId>,
    /// Unit-local symbol table
    pub symbol_table: SymbolTable,
}

impl Unit {
    pub fn walk_nodes(&self, visitor: &mut dyn HirVisitor) {
        let mut walker = Walker {
            unit: self,
            visitor,
        };

        walker.walk_nodes();
    }
}

/// Aggregate HIR structure
#[derive(Debug)]
pub struct Database {
    arena: Arena<HirNode>,
    spans: HashMap<Idx<HirNode>, Span>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            spans: HashMap::new(),
        }
    }

    pub fn add_expr(&mut self, node: expr::Expr, span: Span) -> expr::ExprId {
        let idx = self.arena.alloc(HirNode::Expr(node));
        self.spans.insert(idx, span);
        expr::ExprId(HirId(idx))
    }

    pub fn add_type(&mut self, node: ty::Type, span: Span) -> ty::TypeId {
        let idx = self.arena.alloc(HirNode::Type(node));
        self.spans.insert(idx, span);
        ty::TypeId(HirId(idx))
    }

    pub fn add_stmt(&mut self, node: stmt::Stmt, span: Span) -> stmt::StmtId {
        let idx = self.arena.alloc(HirNode::Stmt(node));
        self.spans.insert(idx, span);
        stmt::StmtId(HirId(idx))
    }

    pub fn get_span(&self, id: HirId) -> Span {
        self.spans.get(&id.0).copied().unwrap()
    }

    pub fn get_node(&self, id: HirId) -> &HirNode {
        &self.arena[id.0]
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

    pub fn nodes(&self) -> impl Iterator<Item = (HirId, &HirNode)> {
        self.arena.iter().map(|(id, node)| (HirId(id), node))
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}

/// Walker for traversing the HIR tree, in postfix order
struct Walker<'a> {
    unit: &'a Unit,
    visitor: &'a mut dyn HirVisitor,
}

impl Walker<'_> {
    pub(crate) fn walk_nodes(&mut self) {
        self.visitor.visit_unit(self.unit);

        for stmt in &self.unit.stmts {
            self.walk_stmt(*stmt)
        }
    }

    fn walk_stmt(&mut self, id: stmt::StmtId) {
        let stmt = self.unit.database.get_stmt(id);

        match stmt {
            stmt::Stmt::ConstVar(decl) => self.walk_constvar(id, decl),
            stmt::Stmt::Assign(stmt) => self.walk_assign(id, stmt),
            stmt::Stmt::Put(stmt) => self.walk_put(id, stmt),
            stmt::Stmt::Get(stmt) => self.walk_get(id, stmt),
            stmt::Stmt::Block(stmt) => self.walk_block(id, stmt),
        }
    }

    fn walk_constvar(&mut self, id: stmt::StmtId, node: &stmt::ConstVar) {
        if let Some(ty) = &node.tail.type_spec() {
            self.walk_type(*ty);
        }

        if let Some(expr) = &node.tail.init_expr() {
            self.walk_expr(*expr);
        }

        self.visitor.visit_constvar(id, node);
    }

    fn walk_assign(&mut self, id: stmt::StmtId, node: &stmt::Assign) {
        self.walk_expr(node.lhs);
        self.walk_expr(node.rhs);

        self.visitor.visit_assign(id, node);
    }

    fn walk_put(&mut self, id: stmt::StmtId, node: &stmt::Put) {
        if let Some(expr) = &node.stream_num {
            self.walk_expr(*expr);
        }

        for item in &node.items {
            if let stmt::Skippable::Item(item) = item {
                self.walk_expr(item.expr);

                if let Some(expr) = item.opts.width() {
                    self.walk_expr(expr);
                }

                if let Some(expr) = item.opts.precision() {
                    self.walk_expr(expr);
                }

                if let Some(expr) = item.opts.exponent_width() {
                    self.walk_expr(expr);
                }
            }
        }

        self.visitor.visit_put(id, node);
    }

    fn walk_get(&mut self, id: stmt::StmtId, node: &stmt::Get) {
        if let Some(expr) = &node.stream_num {
            self.walk_expr(*expr);
        }

        for item in &node.items {
            if let stmt::Skippable::Item(item) = item {
                self.walk_expr(item.expr);

                if let stmt::GetWidth::Chars(expr) = &item.width {
                    self.walk_expr(*expr);
                }
            }
        }

        self.visitor.visit_get(id, node);
    }

    fn walk_block(&mut self, id: stmt::StmtId, node: &stmt::Block) {
        for stmt in &node.stmts {
            self.walk_stmt(*stmt)
        }

        self.visitor.visit_block(id, node);
    }

    fn walk_expr(&mut self, id: expr::ExprId) {
        let node = self.unit.database.get_expr(id);

        match node {
            expr::Expr::Missing => {}
            expr::Expr::Literal(expr) => self.walk_literal(id, expr),
            expr::Expr::Binary(expr) => self.walk_binary(id, expr),
            expr::Expr::Unary(expr) => self.walk_unary(id, expr),
            expr::Expr::Paren(expr) => self.walk_paren(id, expr),
            expr::Expr::Name(expr) => self.walk_name(id, expr),
        }
    }

    fn walk_literal(&mut self, id: expr::ExprId, node: &expr::Literal) {
        self.visitor.visit_literal(id, node);
    }

    fn walk_binary(&mut self, id: expr::ExprId, node: &expr::Binary) {
        self.walk_expr(node.lhs);
        self.walk_expr(node.rhs);

        self.visitor.visit_binary(id, node);
    }

    fn walk_unary(&mut self, id: expr::ExprId, node: &expr::Unary) {
        self.walk_expr(node.rhs);

        self.visitor.visit_unary(id, node);
    }

    fn walk_paren(&mut self, id: expr::ExprId, node: &expr::Paren) {
        self.walk_expr(node.expr);

        self.visitor.visit_paren(id, node);
    }

    fn walk_name(&mut self, id: expr::ExprId, node: &expr::Name) {
        self.visitor.visit_name(id, node);
    }

    fn walk_type(&mut self, id: ty::TypeId) {
        let node = self.unit.database.get_type(id);

        match node {
            ty::Type::Missing => {}
            ty::Type::Primitive(ty) => self.walk_primitive(id, ty),
        }
    }

    fn walk_primitive(&mut self, id: ty::TypeId, node: &ty::Primitive) {
        match node {
            ty::Primitive::SizedChar(ty::SeqLength::Expr(expr))
            | ty::Primitive::SizedString(ty::SeqLength::Expr(expr)) => self.walk_expr(*expr),
            _ => {}
        }

        self.visitor.visit_primitive(id, node);
    }
}

/// Visitor over all nodes in the HIR tree, in postfix order
#[allow(unused_variables)]
pub trait HirVisitor {
    fn visit_unit(&mut self, unit: &Unit) {}
    // Decls
    fn visit_constvar(&mut self, id: stmt::StmtId, decl: &stmt::ConstVar) {}
    // Stmts
    fn visit_assign(&mut self, id: stmt::StmtId, stmt: &stmt::Assign) {}
    fn visit_put(&mut self, id: stmt::StmtId, stmt: &stmt::Put) {}
    fn visit_get(&mut self, id: stmt::StmtId, stmt: &stmt::Get) {}
    fn visit_block(&mut self, id: stmt::StmtId, stmt: &stmt::Block) {}
    // Exprs
    fn visit_literal(&mut self, id: expr::ExprId, expr: &expr::Literal) {}
    fn visit_binary(&mut self, id: expr::ExprId, expr: &expr::Binary) {}
    fn visit_unary(&mut self, id: expr::ExprId, expr: &expr::Unary) {}
    fn visit_paren(&mut self, id: expr::ExprId, expr: &expr::Paren) {}
    fn visit_name(&mut self, id: expr::ExprId, expr: &expr::Name) {}
    // Types
    fn visit_primitive(&mut self, id: ty::TypeId, ty: &ty::Primitive) {}
}
