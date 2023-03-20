use toc_hir::{
    body, expr, item,
    package::{InPackage, PackageId},
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
    Item(PackageId, item::ItemId),
    Type(PackageId, ty::TypeId),
    Body(PackageId, body::BodyId),
    Stmt(PackageId, stmt::BodyStmt),
    Expr(PackageId, expr::BodyExpr),
}

impl From<(PackageId, item::ItemId)> for InsideModule {
    fn from((package_id, item_id): (PackageId, item::ItemId)) -> Self {
        Self::Item(package_id, item_id)
    }
}

impl From<InPackage<item::ItemId>> for InsideModule {
    fn from(InPackage(package_id, item_id): InPackage<item::ItemId>) -> Self {
        Self::Item(package_id, item_id)
    }
}

impl From<(PackageId, ty::TypeId)> for InsideModule {
    fn from((package_id, type_id): (PackageId, ty::TypeId)) -> Self {
        Self::Type(package_id, type_id)
    }
}

impl From<InPackage<ty::TypeId>> for InsideModule {
    fn from(InPackage(package_id, type_id): InPackage<ty::TypeId>) -> Self {
        Self::Type(package_id, type_id)
    }
}

impl From<(PackageId, body::BodyId)> for InsideModule {
    fn from((package_id, body_id): (PackageId, body::BodyId)) -> Self {
        Self::Body(package_id, body_id)
    }
}

impl From<InPackage<body::BodyId>> for InsideModule {
    fn from(InPackage(package_id, body_id): InPackage<body::BodyId>) -> Self {
        Self::Body(package_id, body_id)
    }
}

impl From<(PackageId, expr::BodyExpr)> for InsideModule {
    fn from((package_id, body_expr): (PackageId, expr::BodyExpr)) -> Self {
        Self::Expr(package_id, body_expr)
    }
}

impl From<InPackage<expr::BodyExpr>> for InsideModule {
    fn from(InPackage(package_id, body_expr): InPackage<expr::BodyExpr>) -> Self {
        Self::Expr(package_id, body_expr)
    }
}

impl From<(PackageId, body::BodyId, expr::ExprId)> for InsideModule {
    fn from((package_id, body_id, expr_id): (PackageId, body::BodyId, expr::ExprId)) -> Self {
        Self::Expr(package_id, expr::BodyExpr(body_id, expr_id))
    }
}

impl From<(PackageId, stmt::BodyStmt)> for InsideModule {
    fn from((package_id, body_stmt): (PackageId, stmt::BodyStmt)) -> Self {
        Self::Stmt(package_id, body_stmt)
    }
}

impl From<(PackageId, body::BodyId, stmt::StmtId)> for InsideModule {
    fn from((package_id, body_id, stmt_id): (PackageId, body::BodyId, stmt::StmtId)) -> Self {
        Self::Stmt(package_id, stmt::BodyStmt(body_id, stmt_id))
    }
}
