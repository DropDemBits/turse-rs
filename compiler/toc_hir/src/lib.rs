//! Crate containing all of the HIR node representations
//!
//! Note: All `expr` & `stmt` nodes are to be used with the module's prefix, e.g.
//! `expr::Name` instead of importing the node directly

use std::collections::HashMap;
use std::ops;

use la_arena::{Arena, Idx};
use toc_span::TextRange;

use crate::expr::ExprIdx;
use crate::stmt::StmtIdx;
use crate::symbol::SymbolTable;
use crate::ty::TypeIdx;
pub mod expr;
pub mod stmt;
pub mod symbol;
pub mod ty;

/// Code Unit
#[derive(Debug)]
pub struct Unit {
    /// All nodes associated with the unit
    pub database: Database,
    /// Top level statements in the unit
    pub stmts: Vec<stmt::StmtIdx>,
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
    pub stmt_nodes: SpannedArena<stmt::Stmt>,
    pub expr_nodes: SpannedArena<expr::Expr>,
    pub type_nodes: SpannedArena<ty::Type>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            stmt_nodes: SpannedArena::new(),
            expr_nodes: SpannedArena::new(),
            type_nodes: SpannedArena::new(),
        }
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}

impl ops::Index<StmtIdx> for Database {
    type Output = stmt::Stmt;

    fn index(&self, index: StmtIdx) -> &Self::Output {
        &self.stmt_nodes.arena[index]
    }
}

impl ops::Index<ExprIdx> for Database {
    type Output = expr::Expr;

    fn index(&self, index: ExprIdx) -> &Self::Output {
        &self.expr_nodes.arena[index]
    }
}

impl ops::Index<TypeIdx> for Database {
    type Output = ty::Type;

    fn index(&self, index: TypeIdx) -> &Self::Output {
        &self.type_nodes.arena[index]
    }
}

#[derive(Debug)]
pub struct SpannedArena<T> {
    pub arena: Arena<T>,
    pub spans: HashMap<Idx<T>, TextRange>,
}

impl<T> SpannedArena<T> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            spans: HashMap::new(),
        }
    }

    pub fn alloc_spanned(&mut self, value: T, span: TextRange) -> Idx<T> {
        let idx = self.arena.alloc(value);
        self.spans.insert(idx, span);
        idx
    }
}

impl<T> Default for SpannedArena<T> {
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

    fn walk_stmt(&mut self, id: stmt::StmtIdx) {
        let stmt = &self.unit.database[id];

        match stmt {
            stmt::Stmt::ConstVar(decl) => self.walk_constvar(id, decl),
            stmt::Stmt::Assign(stmt) => self.walk_assign(id, stmt),
            stmt::Stmt::Put(stmt) => self.walk_put(id, stmt),
            stmt::Stmt::Get(stmt) => self.walk_get(id, stmt),
            stmt::Stmt::Block(stmt) => self.walk_block(id, stmt),
        }
    }

    fn walk_constvar(&mut self, id: stmt::StmtIdx, node: &stmt::ConstVar) {
        if let Some(ty) = &node.type_spec {
            self.walk_type(*ty);
        }

        if let Some(expr) = &node.init_expr {
            self.walk_expr(*expr);
        }

        self.visitor.visit_constvar(id, node);
    }

    fn walk_assign(&mut self, id: stmt::StmtIdx, node: &stmt::Assign) {
        self.walk_expr(node.lhs);
        self.walk_expr(node.rhs);

        self.visitor.visit_assign(id, node);
    }

    fn walk_put(&mut self, id: stmt::StmtIdx, node: &stmt::Put) {
        if let Some(expr) = &node.stream_num {
            self.walk_expr(*expr);
        }

        for item in &node.items {
            if let stmt::Skippable::Item(item) = item {
                self.walk_expr(item.expr);

                if let Some(expr) = &item.width {
                    self.walk_expr(*expr);
                }

                if let Some(expr) = &item.precision {
                    self.walk_expr(*expr);
                }

                if let Some(expr) = &item.exponent_width {
                    self.walk_expr(*expr);
                }
            }
        }

        self.visitor.visit_put(id, node);
    }

    fn walk_get(&mut self, id: stmt::StmtIdx, node: &stmt::Get) {
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

    fn walk_block(&mut self, id: stmt::StmtIdx, node: &stmt::Block) {
        for stmt in &self.unit.stmts {
            self.walk_stmt(*stmt)
        }

        self.visitor.visit_block(id, node);
    }

    fn walk_expr(&mut self, id: expr::ExprIdx) {
        let node = &self.unit.database[id];
        match node {
            expr::Expr::Missing => {}
            expr::Expr::Literal(expr) => self.walk_literal(id, expr),
            expr::Expr::Binary(expr) => self.walk_binary(id, expr),
            expr::Expr::Unary(expr) => self.walk_unary(id, expr),
            expr::Expr::Paren(expr) => self.walk_paren(id, expr),
            expr::Expr::Name(expr) => self.walk_name(id, expr),
        }
    }

    fn walk_literal(&mut self, id: expr::ExprIdx, node: &expr::Literal) {
        self.visitor.visit_literal(id, node);
    }

    fn walk_binary(&mut self, id: expr::ExprIdx, node: &expr::Binary) {
        self.walk_expr(node.lhs);
        self.walk_expr(node.rhs);

        self.visitor.visit_binary(id, node);
    }

    fn walk_unary(&mut self, id: expr::ExprIdx, node: &expr::Unary) {
        self.walk_expr(node.rhs);

        self.visitor.visit_unary(id, node);
    }

    fn walk_paren(&mut self, id: expr::ExprIdx, node: &expr::Paren) {
        self.walk_expr(node.expr);

        self.visitor.visit_paren(id, node);
    }

    fn walk_name(&mut self, id: expr::ExprIdx, node: &expr::Name) {
        self.visitor.visit_name(id, node);
    }

    fn walk_type(&mut self, id: ty::TypeIdx) {
        let node = &self.unit.database[id];
        match node {
            ty::Type::Missing => {}
            ty::Type::Primitive(ty) => self.walk_primitive(id, ty),
        }
    }

    fn walk_primitive(&mut self, id: ty::TypeIdx, node: &ty::Primitive) {
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
    fn visit_constvar(&mut self, id: stmt::StmtIdx, decl: &stmt::ConstVar) {}
    // Stmts
    fn visit_assign(&mut self, id: stmt::StmtIdx, stmt: &stmt::Assign) {}
    fn visit_put(&mut self, id: stmt::StmtIdx, stmt: &stmt::Put) {}
    fn visit_get(&mut self, id: stmt::StmtIdx, stmt: &stmt::Get) {}
    fn visit_block(&mut self, id: stmt::StmtIdx, stmt: &stmt::Block) {}
    // Exprs
    fn visit_literal(&mut self, id: expr::ExprIdx, expr: &expr::Literal) {}
    fn visit_binary(&mut self, id: expr::ExprIdx, expr: &expr::Binary) {}
    fn visit_unary(&mut self, id: expr::ExprIdx, expr: &expr::Unary) {}
    fn visit_paren(&mut self, id: expr::ExprIdx, expr: &expr::Paren) {}
    fn visit_name(&mut self, id: expr::ExprIdx, expr: &expr::Name) {}
    // Types
    fn visit_primitive(&mut self, id: ty::TypeIdx, ty: &ty::Primitive) {}
}
