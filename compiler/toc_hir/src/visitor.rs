//! HIR Tree visiting related structures

use std::{collections::VecDeque, sync::Arc};

use toc_span::FileId;

use crate::{body, expr, item, library, stmt, ty};

/// Uniquely identifies a statement within a library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyStmt(pub body::BodyId, pub stmt::StmtId);

/// Uniquely identifies an expression within a library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyExpr(pub body::BodyId, pub expr::ExprId);

/// Visits the library in postorder
pub fn postorder_visit_library(
    lib: &library::Library,
    ty_interner: &dyn ty::TypeInterner,
    visitor: &dyn HirVisitor,
) {
    let mut walker = Walker::new(lib, ty_interner);

    while let Some(event) = walker.next_event() {
        let node = match event {
            WalkEvent::Leave(node) => node,
            _ => continue,
        };

        node.visit_node(visitor);
    }
}

/// Visits the library in preorder
pub fn preorder_visit_library(
    lib: &library::Library,
    ty_interner: &dyn ty::TypeInterner,
    visitor: &dyn HirVisitor,
) {
    let mut walker = Walker::new(lib, ty_interner);

    while let Some(event) = walker.next_event() {
        let node = match event {
            WalkEvent::Enter(node) => node,
            _ => continue,
        };

        node.visit_node(visitor);
    }
}

/// Visitor over all nodes in the HIR tree.
#[allow(unused_variables)]
pub trait HirVisitor {
    fn visit_library(&self, library: &library::Library) {}
    fn visit_file_root(&self, file: toc_span::FileId, id: item::ItemId) {}
    // Items
    fn visit_item(&self, id: item::ItemId, item: &item::Item) {
        self.specify_item(id, item);
    }
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {}
    fn visit_module(&self, id: item::ItemId, item: &item::Module) {}
    // Body
    fn visit_body(&self, id: body::BodyId, body: &body::Body) {}
    // Stmts
    fn visit_stmt(&self, id: BodyStmt, stmt: &stmt::Stmt) {
        self.specify_stmt(id, stmt);
    }
    fn visit_item_stmt(&self, id: BodyStmt, item: item::ItemId) {}
    fn visit_assign(&self, id: BodyStmt, stmt: &stmt::Assign) {}
    fn visit_put(&self, id: BodyStmt, stmt: &stmt::Put) {}
    fn visit_get(&self, id: BodyStmt, stmt: &stmt::Get) {}
    fn visit_block(&self, id: BodyStmt, stmt: &stmt::Block) {}
    // Exprs
    fn visit_expr(&self, id: BodyExpr, expr: &expr::Expr) {
        self.specify_expr(id, expr);
    }
    fn visit_literal(&self, id: BodyExpr, expr: &expr::Literal) {}
    fn visit_binary(&self, id: BodyExpr, expr: &expr::Binary) {}
    fn visit_unary(&self, id: BodyExpr, expr: &expr::Unary) {}
    fn visit_paren(&self, id: BodyExpr, expr: &expr::Paren) {}
    fn visit_name(&self, id: BodyExpr, expr: &expr::Name) {}
    // Types
    fn visit_type(&self, id: ty::TypeId, ty: &ty::Type) {
        self.specify_type(id, ty);
    }
    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {}

    // Node specification //

    fn specify_item(&self, id: item::ItemId, item: &item::Item) {
        match &item.kind {
            item::ItemKind::ConstVar(item) => self.visit_constvar(id, item),
            item::ItemKind::Module(item) => self.visit_module(id, item),
        }
    }

    fn specify_stmt(&self, id: BodyStmt, stmt: &stmt::Stmt) {
        match &stmt.kind {
            stmt::StmtKind::Item(item) => self.visit_item_stmt(id, *item),
            stmt::StmtKind::Assign(stmt) => self.visit_assign(id, stmt),
            stmt::StmtKind::Put(stmt) => self.visit_put(id, stmt),
            stmt::StmtKind::Get(stmt) => self.visit_get(id, stmt),
            stmt::StmtKind::Block(stmt) => self.visit_block(id, stmt),
        }
    }

    fn specify_expr(&self, id: BodyExpr, expr: &expr::Expr) {
        match &expr.kind {
            expr::ExprKind::Missing => {}
            expr::ExprKind::Literal(expr) => self.visit_literal(id, expr),
            expr::ExprKind::Binary(expr) => self.visit_binary(id, expr),
            expr::ExprKind::Unary(expr) => self.visit_unary(id, expr),
            expr::ExprKind::Paren(expr) => self.visit_paren(id, expr),
            expr::ExprKind::Name(expr) => self.visit_name(id, expr),
        }
    }

    fn specify_type(&self, id: ty::TypeId, ty: &ty::Type) {
        match &ty.kind {
            ty::TypeKind::Missing => {}
            ty::TypeKind::Primitive(ty) => self.visit_primitive(id, ty),
        }
    }
}

#[derive(Debug, Clone)]
pub enum WalkEvent<'hir> {
    Enter(WalkNode<'hir>),
    Leave(WalkNode<'hir>),
}

#[derive(Debug, Clone)]
pub enum WalkNode<'hir> {
    Library(&'hir library::Library),
    FileRoot(FileId, item::ItemId),
    Item(item::ItemId, &'hir item::Item),
    Body(body::BodyId, &'hir body::Body),
    Stmt(BodyStmt, &'hir stmt::Stmt),
    Expr(BodyExpr, &'hir expr::Expr),
    Type(ty::TypeId, Arc<ty::Type>),
}

impl WalkNode<'_> {
    pub fn visit_node(&self, visitor: &dyn HirVisitor) {
        match self {
            WalkNode::Library(library) => visitor.visit_library(library),
            WalkNode::FileRoot(file, id) => visitor.visit_file_root(*file, *id),
            WalkNode::Item(id, item) => visitor.visit_item(*id, item),
            WalkNode::Body(id, body) => visitor.visit_body(*id, body),
            WalkNode::Stmt(id, stmt) => visitor.visit_stmt(*id, stmt),
            WalkNode::Expr(id, expr) => visitor.visit_expr(*id, expr),
            WalkNode::Type(id, ty) => visitor.visit_type(*id, ty.as_ref()),
        }
    }
}

/// Walker for traversing the HIR tree, generating [`WalkEvent`]s
pub struct Walker<'hir, 'ty: 'hir> {
    lib: &'hir library::Library,
    ty_interner: &'ty dyn ty::TypeInterner,
    // Events awaiting to be inserted
    pending: VecDeque<WalkEvent<'hir>>,
    // Events to process
    process: VecDeque<WalkEvent<'hir>>,
}

impl<'hir, 'ty: 'hir> Walker<'hir, 'ty> {
    pub fn new(lib: &'hir library::Library, ty_interner: &'ty dyn ty::TypeInterner) -> Self {
        Self {
            lib,
            ty_interner,
            pending: vec![].into(),
            process: vec![WalkEvent::Enter(WalkNode::Library(lib))].into(),
        }
    }

    pub fn next_event(&mut self) -> Option<WalkEvent> {
        let event = self.process.pop_front()?;

        if let WalkEvent::Enter(node) = &event {
            // Push in a leave event
            self.process.push_front(WalkEvent::Leave(node.clone()));

            // Fill up pending list with descendants
            match node {
                WalkNode::Library(library) => self.walk_library(library),
                WalkNode::FileRoot(_, item) => self.walk_file_root(*item),
                WalkNode::Item(_, item) => self.walk_item(item),
                WalkNode::Body(id, body) => self.walk_body(*id, body),
                WalkNode::Stmt(id, stmt) => self.walk_stmt(id.0, stmt),
                WalkNode::Expr(id, expr) => self.walk_expr(id.0, expr),
                WalkNode::Type(_, ty) => self.walk_type(ty.as_ref()),
            }

            // Insert pending in reversed order
            for event in self.pending.drain(..).rev() {
                self.process.push_front(event)
            }
        }

        Some(event)
    }

    fn enter_item_root(&mut self, file: FileId, item: item::ItemId) {
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::FileRoot(file, item)));
    }

    fn enter_item(&mut self, id: item::ItemId, item: &'hir item::Item) {
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::Item(id, item)));
    }

    fn enter_body(&mut self, id: body::BodyId, body: &'hir body::Body) {
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::Body(id, body)));
    }

    fn enter_stmt(&mut self, in_body: body::BodyId, stmt: stmt::StmtId) {
        let id = BodyStmt(in_body, stmt);
        let stmt = self.lib.body(in_body).stmt(stmt);
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::Stmt(id, stmt)));
    }

    fn enter_expr(&mut self, in_body: body::BodyId, expr: expr::ExprId) {
        let id = BodyExpr(in_body, expr);
        let expr = self.lib.body(in_body).expr(expr);
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::Expr(id, expr)));
    }

    fn enter_type(&mut self, id: ty::TypeId, ty: Arc<ty::Type>) {
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::Type(id, ty)));
    }

    fn walk_library(&mut self, library: &library::Library) {
        for (&file, &item) in &library.root_items {
            self.enter_item_root(file, item);
        }
    }

    fn walk_file_root(&mut self, item: item::ItemId) {
        self.enter_item(item, self.lib.item(item));
    }

    fn walk_body(&mut self, id: body::BodyId, body: &'hir body::Body) {
        match &body.kind {
            body::BodyKind::Stmts(stmts, _) => {
                for &stmt in stmts {
                    self.enter_stmt(id, stmt);
                }
            }
            body::BodyKind::Exprs(expr) => self.enter_expr(id, *expr),
        }
    }

    fn walk_item(&mut self, item: &item::Item) {
        match &item.kind {
            item::ItemKind::ConstVar(item) => self.walk_constvar(item),
            item::ItemKind::Module(item) => self.walk_module(item),
        }
    }

    fn walk_constvar(&mut self, node: &item::ConstVar) {
        if let Some(ty) = node.tail.type_spec() {
            self.enter_type(ty, self.ty_interner.lookup_type(ty));
        }

        if let Some(id) = node.tail.init_expr() {
            self.enter_body(id, self.lib.body(id));
        }
    }

    fn walk_module(&mut self, node: &item::Module) {
        self.enter_body(node.body, self.lib.body(node.body))
    }

    fn walk_stmt(&mut self, in_body: body::BodyId, stmt: &stmt::Stmt) {
        match &stmt.kind {
            stmt::StmtKind::Item(item) => self.walk_item_stmt(*item),
            stmt::StmtKind::Assign(node) => self.walk_assign(in_body, node),
            stmt::StmtKind::Put(node) => self.walk_put(in_body, node),
            stmt::StmtKind::Get(node) => self.walk_get(in_body, node),
            stmt::StmtKind::Block(node) => self.walk_block(in_body, node),
        }
    }

    fn walk_item_stmt(&mut self, item: item::ItemId) {
        self.enter_item(item, self.lib.item(item));
    }

    fn walk_assign(&mut self, in_body: body::BodyId, node: &stmt::Assign) {
        self.enter_expr(in_body, node.lhs);
        self.enter_expr(in_body, node.rhs);
    }

    fn walk_put(&mut self, in_body: body::BodyId, node: &stmt::Put) {
        if let Some(expr) = &node.stream_num {
            self.enter_expr(in_body, *expr);
        }

        for item in &node.items {
            if let stmt::Skippable::Item(item) = item {
                // Order of these entered expressions stays the same, since it matches
                // evaluation order
                self.enter_expr(in_body, item.expr);

                if let Some(expr) = item.opts.width() {
                    self.enter_expr(in_body, expr);
                }

                if let Some(expr) = item.opts.precision() {
                    self.enter_expr(in_body, expr);
                }

                if let Some(expr) = item.opts.exponent_width() {
                    self.enter_expr(in_body, expr);
                }
            }
        }
    }

    fn walk_get(&mut self, in_body: body::BodyId, node: &stmt::Get) {
        if let Some(expr) = node.stream_num {
            self.enter_expr(in_body, expr);
        }

        for item in &node.items {
            if let stmt::Skippable::Item(item) = item {
                // Order of these entered expressions stays the same, since it matches
                // evaluation order
                self.enter_expr(in_body, item.expr);

                if let stmt::GetWidth::Chars(expr) = item.width {
                    self.enter_expr(in_body, expr);
                }
            }
        }
    }

    fn walk_block(&mut self, in_body: body::BodyId, node: &stmt::Block) {
        for stmt in &node.stmts {
            self.enter_stmt(in_body, *stmt);
        }
    }

    fn walk_expr(&mut self, in_body: body::BodyId, expr: &expr::Expr) {
        match &expr.kind {
            expr::ExprKind::Missing => {}
            expr::ExprKind::Literal(_) => {}
            expr::ExprKind::Binary(node) => self.walk_binary(in_body, node),
            expr::ExprKind::Unary(node) => self.walk_unary(in_body, node),
            expr::ExprKind::Paren(node) => self.walk_paren(in_body, node),
            expr::ExprKind::Name(_) => {}
        }
    }

    fn walk_binary(&mut self, in_body: body::BodyId, node: &expr::Binary) {
        self.enter_expr(in_body, node.lhs);
        self.enter_expr(in_body, node.rhs);
    }

    fn walk_unary(&mut self, in_body: body::BodyId, node: &expr::Unary) {
        self.enter_expr(in_body, node.rhs);
    }

    fn walk_paren(&mut self, in_body: body::BodyId, node: &expr::Paren) {
        self.enter_expr(in_body, node.expr);
    }

    fn walk_type(&mut self, node: &ty::Type) {
        match &node.kind {
            ty::TypeKind::Missing => {}
            ty::TypeKind::Primitive(ty) => self.walk_primitive(ty),
        }
    }

    fn walk_primitive(&mut self, node: &ty::Primitive) {
        match node {
            ty::Primitive::SizedChar(ty::SeqLength::Expr(id))
            | ty::Primitive::SizedString(ty::SeqLength::Expr(id)) => {
                self.enter_body(*id, self.lib.body(*id));
            }
            _ => {}
        }
    }
}
