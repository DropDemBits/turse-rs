//! HIR Tree visiting related structures

use std::collections::VecDeque;

use toc_span::FileId;

use crate::{
    body,
    expr::{self, BodyExpr},
    item, library,
    stmt::{self, BodyStmt},
    ty,
};

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
    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {}
    fn visit_bind_decl(&self, id: item::ItemId, item: &item::Binding) {}
    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {}
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
    fn visit_for(&self, id: BodyStmt, stmt: &stmt::For) {}
    fn visit_loop(&self, id: BodyStmt, stmt: &stmt::Loop) {}
    fn visit_exit(&self, id: BodyStmt, stmt: &stmt::Exit) {}
    fn visit_if(&self, id: BodyStmt, stmt: &stmt::If) {}
    fn visit_case(&self, id: BodyStmt, stmt: &stmt::Case) {}
    fn visit_block(&self, id: BodyStmt, stmt: &stmt::Block) {}
    // Exprs
    fn visit_expr(&self, id: BodyExpr, expr: &expr::Expr) {
        self.specify_expr(id, expr);
    }
    fn visit_literal(&self, id: BodyExpr, expr: &expr::Literal) {}
    fn visit_binary(&self, id: BodyExpr, expr: &expr::Binary) {}
    fn visit_unary(&self, id: BodyExpr, expr: &expr::Unary) {}
    fn visit_name(&self, id: BodyExpr, expr: &expr::Name) {}
    // Types
    fn visit_type(&self, id: ty::TypeId, ty: &ty::Type) {
        self.specify_type(id, ty);
    }
    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {}
    fn visit_alias(&self, id: ty::TypeId, ty: &ty::Alias) {}
    fn visit_void(&self, id: ty::TypeId) {}

    // Node specification //

    fn specify_item(&self, id: item::ItemId, item: &item::Item) {
        match &item.kind {
            item::ItemKind::ConstVar(item) => self.visit_constvar(id, item),
            item::ItemKind::Type(item) => self.visit_type_decl(id, item),
            item::ItemKind::Binding(item) => self.visit_bind_decl(id, item),
            item::ItemKind::Subprogram(item) => self.visit_subprogram_decl(id, item),
            item::ItemKind::Module(item) => self.visit_module(id, item),
        }
    }

    fn specify_stmt(&self, id: BodyStmt, stmt: &stmt::Stmt) {
        match &stmt.kind {
            stmt::StmtKind::Item(item) => self.visit_item_stmt(id, *item),
            stmt::StmtKind::Assign(stmt) => self.visit_assign(id, stmt),
            stmt::StmtKind::Put(stmt) => self.visit_put(id, stmt),
            stmt::StmtKind::Get(stmt) => self.visit_get(id, stmt),
            stmt::StmtKind::For(stmt) => self.visit_for(id, stmt),
            stmt::StmtKind::Loop(stmt) => self.visit_loop(id, stmt),
            stmt::StmtKind::Exit(stmt) => self.visit_exit(id, stmt),
            stmt::StmtKind::If(stmt) => self.visit_if(id, stmt),
            stmt::StmtKind::Case(stmt) => self.visit_case(id, stmt),
            stmt::StmtKind::Block(stmt) => self.visit_block(id, stmt),
        }
    }

    fn specify_expr(&self, id: BodyExpr, expr: &expr::Expr) {
        match &expr.kind {
            expr::ExprKind::Missing => {}
            expr::ExprKind::Literal(expr) => self.visit_literal(id, expr),
            expr::ExprKind::Binary(expr) => self.visit_binary(id, expr),
            expr::ExprKind::Unary(expr) => self.visit_unary(id, expr),
            expr::ExprKind::Name(expr) => self.visit_name(id, expr),
        }
    }

    fn specify_type(&self, id: ty::TypeId, ty: &ty::Type) {
        match &ty.kind {
            ty::TypeKind::Missing => {}
            ty::TypeKind::Primitive(ty) => self.visit_primitive(id, ty),
            ty::TypeKind::Alias(ty) => self.visit_alias(id, ty),
            ty::TypeKind::Void => self.visit_void(id),
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
    Type(ty::TypeId, &'hir ty::Type),
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
            WalkNode::Type(id, ty) => visitor.visit_type(*id, ty),
        }
    }
}

/// Walker for traversing the HIR tree, generating [`WalkEvent`]s
pub struct Walker<'hir> {
    lib: &'hir library::Library,
    // Events awaiting to be inserted
    pending: VecDeque<WalkEvent<'hir>>,
    // Events to process
    process: VecDeque<WalkEvent<'hir>>,
}

impl<'hir> Walker<'hir> {
    /// Starts walking the HIR from the library root
    pub fn from_library(lib: &'hir library::Library) -> Self {
        Self {
            lib,
            pending: vec![].into(),
            process: vec![WalkEvent::Enter(WalkNode::Library(lib))].into(),
        }
    }

    /// Starts walking the HIR from the given body
    pub fn from_body(lib: &'hir library::Library, body_id: body::BodyId) -> Self {
        Self {
            lib,
            pending: vec![].into(),
            process: vec![WalkEvent::Enter(WalkNode::Body(body_id, lib.body(body_id)))].into(),
        }
    }

    /// Gets the next walking event
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
                WalkNode::Type(_, ty) => self.walk_type(ty),
            }

            // Insert pending in reversed order to retain insertion order
            for event in self.pending.drain(..).rev() {
                self.process.push_front(event)
            }
        }

        Some(event)
    }

    /// Walks the HIR tree in pre-order, invoking the given visitor
    pub fn visit_preorder(mut self, visitor: &dyn HirVisitor) {
        while let Some(event) = self.next_event() {
            let node = match event {
                WalkEvent::Enter(node) => node,
                _ => continue,
            };

            node.visit_node(visitor);
        }
    }

    /// Walks the HIR tree in post-order, invoking the given visitor
    pub fn visit_postorder(mut self, visitor: &dyn HirVisitor) {
        while let Some(event) = self.next_event() {
            let node = match event {
                WalkEvent::Leave(node) => node,
                _ => continue,
            };

            node.visit_node(visitor);
        }
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

    fn enter_stmts(&mut self, in_body: body::BodyId, stmts: &[stmt::StmtId]) {
        for stmt in stmts {
            self.enter_stmt(in_body, *stmt);
        }
    }

    fn enter_expr(&mut self, in_body: body::BodyId, expr: expr::ExprId) {
        let id = BodyExpr(in_body, expr);
        let expr = self.lib.body(in_body).expr(expr);
        self.pending
            .push_back(WalkEvent::Enter(WalkNode::Expr(id, expr)));
    }

    fn enter_type(&mut self, id: ty::TypeId, ty: &'hir ty::Type) {
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
            body::BodyKind::Stmts(stmts, ..) => {
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
            item::ItemKind::Type(item) => self.walk_type_decl(item),
            item::ItemKind::Binding(item) => self.walk_bind_decl(item),
            item::ItemKind::Subprogram(item) => self.walk_subprogram_decl(item),
            item::ItemKind::Module(item) => self.walk_module(item),
        }
    }

    fn walk_constvar(&mut self, node: &item::ConstVar) {
        if let Some(ty) = node.type_spec {
            self.enter_type(ty, self.lib.lookup_type(ty));
        }

        if let Some(id) = node.init_expr {
            self.enter_body(id, self.lib.body(id));
        }
    }

    fn walk_type_decl(&mut self, node: &item::Type) {
        match node.type_def {
            item::DefinedType::Alias(ty) => self.enter_type(ty, self.lib.lookup_type(ty)),
            item::DefinedType::Forward(_) => {}
        }
    }

    fn walk_bind_decl(&mut self, node: &item::Binding) {
        self.enter_body(node.bind_to, self.lib.body(node.bind_to));
    }

    fn walk_subprogram_decl(&mut self, node: &item::Subprogram) {
        match node.extra {
            item::SubprogramExtra::None => {}
            item::SubprogramExtra::DeviceSpec(body) | item::SubprogramExtra::StackSize(body) => {
                self.enter_body(body, self.lib.body(body))
            }
        }

        if let Some(params) = &node.param_list {
            for param in &params.tys {
                let ty = param.param_ty;
                self.enter_type(ty, self.lib.lookup_type(ty));
            }
        }

        let result_ty = node.result.ty;
        self.enter_type(result_ty, self.lib.lookup_type(result_ty));

        self.enter_body(node.body.body, self.lib.body(node.body.body));
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
            stmt::StmtKind::For(node) => self.walk_for(in_body, node),
            stmt::StmtKind::Loop(node) => self.walk_loop(in_body, node),
            stmt::StmtKind::Exit(node) => self.walk_exit(in_body, node),
            stmt::StmtKind::If(node) => self.walk_if(in_body, node),
            stmt::StmtKind::Case(node) => self.walk_case(in_body, node),
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

    fn walk_for(&mut self, in_body: body::BodyId, node: &stmt::For) {
        match node.bounds {
            stmt::ForBounds::Implicit(expr) => self.enter_expr(in_body, expr),
            stmt::ForBounds::Full(start, end) => {
                self.enter_expr(in_body, start);
                self.enter_expr(in_body, end);
            }
        }

        if let Some(expr) = node.step_by {
            self.enter_expr(in_body, expr);
        }

        self.enter_stmts(in_body, &node.stmts);
    }

    fn walk_loop(&mut self, in_body: body::BodyId, node: &stmt::Loop) {
        self.enter_stmts(in_body, &node.stmts);
    }

    fn walk_exit(&mut self, in_body: body::BodyId, node: &stmt::Exit) {
        if let Some(expr) = node.when_condition {
            self.enter_expr(in_body, expr);
        }
    }

    fn walk_if(&mut self, in_body: body::BodyId, node: &stmt::If) {
        self.enter_expr(in_body, node.condition);
        self.enter_stmt(in_body, node.true_branch);
        if let Some(false_branch) = node.false_branch {
            self.enter_stmt(in_body, false_branch);
        }
    }

    fn walk_case(&mut self, in_body: body::BodyId, node: &stmt::Case) {
        self.enter_expr(in_body, node.discriminant);

        for arm in &node.arms {
            if let stmt::CaseSelector::Exprs(exprs) = &arm.selectors {
                exprs
                    .iter()
                    .for_each(|&expr| self.enter_expr(in_body, expr));
            }

            self.enter_stmts(in_body, &arm.stmts);
        }
    }

    fn walk_block(&mut self, in_body: body::BodyId, node: &stmt::Block) {
        let stmts = &node.stmts;
        self.enter_stmts(in_body, stmts);
    }

    fn walk_expr(&mut self, in_body: body::BodyId, expr: &expr::Expr) {
        match &expr.kind {
            expr::ExprKind::Missing => {}
            expr::ExprKind::Literal(_) => {}
            expr::ExprKind::Binary(node) => self.walk_binary(in_body, node),
            expr::ExprKind::Unary(node) => self.walk_unary(in_body, node),
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

    fn walk_type(&mut self, node: &ty::Type) {
        match &node.kind {
            ty::TypeKind::Missing => {}
            ty::TypeKind::Primitive(ty) => self.walk_primitive(ty),
            ty::TypeKind::Alias(_) => {}
            ty::TypeKind::Void => {}
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
