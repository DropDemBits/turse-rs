use toc_hir::{
    body, expr, item,
    library::{InLibrary, LibraryId},
    stmt, ty,
};

/// Any HIR node that can uniquely be inside of a module
///
/// ## Note
///
/// [`DefId`]s can't be uniquely inside of a module as undeclared don't have a
/// declaration that pins them to a specific module.
///
/// [`DefId`]: toc_hir::symbol::DefId
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InsideModule {
    Item(LibraryId, item::ItemId),
    Type(LibraryId, ty::TypeId),
    Body(LibraryId, body::BodyId),
    Stmt(LibraryId, stmt::BodyStmt),
    Expr(LibraryId, expr::BodyExpr),
}

impl From<(LibraryId, item::ItemId)> for InsideModule {
    fn from((library_id, item_id): (LibraryId, item::ItemId)) -> Self {
        Self::Item(library_id, item_id)
    }
}

impl From<InLibrary<item::ItemId>> for InsideModule {
    fn from(InLibrary(library_id, item_id): InLibrary<item::ItemId>) -> Self {
        Self::Item(library_id, item_id)
    }
}

impl From<(LibraryId, ty::TypeId)> for InsideModule {
    fn from((library_id, type_id): (LibraryId, ty::TypeId)) -> Self {
        Self::Type(library_id, type_id)
    }
}

impl From<InLibrary<ty::TypeId>> for InsideModule {
    fn from(InLibrary(library_id, type_id): InLibrary<ty::TypeId>) -> Self {
        Self::Type(library_id, type_id)
    }
}

impl From<(LibraryId, body::BodyId)> for InsideModule {
    fn from((library_id, body_id): (LibraryId, body::BodyId)) -> Self {
        Self::Body(library_id, body_id)
    }
}

impl From<InLibrary<body::BodyId>> for InsideModule {
    fn from(InLibrary(library_id, body_id): InLibrary<body::BodyId>) -> Self {
        Self::Body(library_id, body_id)
    }
}

impl From<(LibraryId, expr::BodyExpr)> for InsideModule {
    fn from((library_id, body_expr): (LibraryId, expr::BodyExpr)) -> Self {
        Self::Expr(library_id, body_expr)
    }
}

impl From<InLibrary<expr::BodyExpr>> for InsideModule {
    fn from(InLibrary(library_id, body_expr): InLibrary<expr::BodyExpr>) -> Self {
        Self::Expr(library_id, body_expr)
    }
}

impl From<(LibraryId, body::BodyId, expr::ExprId)> for InsideModule {
    fn from((library_id, body_id, expr_id): (LibraryId, body::BodyId, expr::ExprId)) -> Self {
        Self::Expr(library_id, expr::BodyExpr(body_id, expr_id))
    }
}

impl From<(LibraryId, stmt::BodyStmt)> for InsideModule {
    fn from((library_id, body_stmt): (LibraryId, stmt::BodyStmt)) -> Self {
        Self::Stmt(library_id, body_stmt)
    }
}

impl From<(LibraryId, body::BodyId, stmt::StmtId)> for InsideModule {
    fn from((library_id, body_id, stmt_id): (LibraryId, body::BodyId, stmt::StmtId)) -> Self {
        Self::Stmt(library_id, stmt::BodyStmt(body_id, stmt_id))
    }
}
