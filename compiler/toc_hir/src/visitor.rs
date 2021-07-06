//! HIR Tree visiting related structures

use crate::{db, expr, stmt, ty, unit};

/// Walker for traversing the HIR tree, in postfix order
pub(crate) struct Walker<'unit, 'visitor> {
    hir_db: db::HirDb,
    unit: &'unit unit::Unit,
    visitor: &'visitor dyn HirVisitor,
}

impl<'unit, 'visitor> Walker<'unit, 'visitor> {
    pub(crate) fn new(
        hir_db: db::HirDb,
        unit: &'unit unit::Unit,
        visitor: &'visitor dyn HirVisitor,
    ) -> Self {
        Self {
            hir_db,
            unit,
            visitor,
        }
    }

    pub(crate) fn walk_nodes(&self) {
        self.visitor.visit_unit(self.unit);

        for stmt in &self.unit.stmts {
            self.walk_stmt(*stmt)
        }
    }

    fn walk_stmt(&self, id: stmt::StmtId) {
        let stmt = self.hir_db.get_stmt(id);

        match stmt {
            stmt::Stmt::ConstVar(decl) => self.walk_constvar(id, decl),
            stmt::Stmt::Assign(stmt) => self.walk_assign(id, stmt),
            stmt::Stmt::Put(stmt) => self.walk_put(id, stmt),
            stmt::Stmt::Get(stmt) => self.walk_get(id, stmt),
            stmt::Stmt::Block(stmt) => self.walk_block(id, stmt),
        }
    }

    fn walk_constvar(&self, id: stmt::StmtId, node: &stmt::ConstVar) {
        if let Some(ty) = &node.tail.type_spec() {
            self.walk_type(*ty);
        }

        if let Some(expr) = &node.tail.init_expr() {
            self.walk_expr(*expr);
        }

        self.visitor.visit_constvar(id, node);
    }

    fn walk_assign(&self, id: stmt::StmtId, node: &stmt::Assign) {
        self.walk_expr(node.lhs);
        self.walk_expr(node.rhs);

        self.visitor.visit_assign(id, node);
    }

    fn walk_put(&self, id: stmt::StmtId, node: &stmt::Put) {
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

    fn walk_get(&self, id: stmt::StmtId, node: &stmt::Get) {
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

    fn walk_block(&self, id: stmt::StmtId, node: &stmt::Block) {
        for stmt in &node.stmts {
            self.walk_stmt(*stmt)
        }

        self.visitor.visit_block(id, node);
    }

    fn walk_expr(&self, id: expr::ExprId) {
        let node = self.hir_db.get_expr(id);

        match node {
            expr::Expr::Missing => {}
            expr::Expr::Literal(expr) => self.walk_literal(id, expr),
            expr::Expr::Binary(expr) => self.walk_binary(id, expr),
            expr::Expr::Unary(expr) => self.walk_unary(id, expr),
            expr::Expr::Paren(expr) => self.walk_paren(id, expr),
            expr::Expr::Name(expr) => self.walk_name(id, expr),
        }
    }

    fn walk_literal(&self, id: expr::ExprId, node: &expr::Literal) {
        self.visitor.visit_literal(id, node);
    }

    fn walk_binary(&self, id: expr::ExprId, node: &expr::Binary) {
        self.walk_expr(node.lhs);
        self.walk_expr(node.rhs);

        self.visitor.visit_binary(id, node);
    }

    fn walk_unary(&self, id: expr::ExprId, node: &expr::Unary) {
        self.walk_expr(node.rhs);

        self.visitor.visit_unary(id, node);
    }

    fn walk_paren(&self, id: expr::ExprId, node: &expr::Paren) {
        self.walk_expr(node.expr);

        self.visitor.visit_paren(id, node);
    }

    fn walk_name(&self, id: expr::ExprId, node: &expr::Name) {
        self.visitor.visit_name(id, node);
    }

    fn walk_type(&self, id: ty::TypeId) {
        let node = self.hir_db.get_type(id);

        match node {
            ty::Type::Missing => {}
            ty::Type::Primitive(ty) => self.walk_primitive(id, ty),
        }
    }

    fn walk_primitive(&self, id: ty::TypeId, node: &ty::Primitive) {
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
    fn visit_unit(&self, unit: &unit::Unit) {}
    // Decls
    fn visit_constvar(&self, id: stmt::StmtId, decl: &stmt::ConstVar) {}
    // Stmts
    fn visit_assign(&self, id: stmt::StmtId, stmt: &stmt::Assign) {}
    fn visit_put(&self, id: stmt::StmtId, stmt: &stmt::Put) {}
    fn visit_get(&self, id: stmt::StmtId, stmt: &stmt::Get) {}
    fn visit_block(&self, id: stmt::StmtId, stmt: &stmt::Block) {}
    // Exprs
    fn visit_literal(&self, id: expr::ExprId, expr: &expr::Literal) {}
    fn visit_binary(&self, id: expr::ExprId, expr: &expr::Binary) {}
    fn visit_unary(&self, id: expr::ExprId, expr: &expr::Unary) {}
    fn visit_paren(&self, id: expr::ExprId, expr: &expr::Paren) {}
    fn visit_name(&self, id: expr::ExprId, expr: &expr::Name) {}
    // Types
    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {}
}
