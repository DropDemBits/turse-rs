use std::sync::Arc;

use toc_hir::{
    body::{self, BodyTable},
    expr,
    item::{self, ModuleTree},
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    stmt,
    symbol::{DefId, DefOwner, DefTable},
    ty::{self, TypeOwners},
};
use toc_salsa::salsa;

use crate::query;

/// HIR tree related queries
#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: toc_hir_lowering::LoweringDb {
    /// Get the library associated with the given id
    #[salsa::invoke(query::library_query)]
    fn library(&self, library: LibraryId) -> LoweredLibrary;

    /// Graph of all libraries
    #[salsa::invoke(query::library_graph_query)]
    fn library_graph(&self) -> LibraryGraph;

    /// Gets all of the definitions in the library,
    /// providing a mapping between definitions and definition owners.
    #[salsa::invoke(query::collect_defs)]
    fn defs_of(&self, library: LibraryId) -> Arc<DefTable>;

    /// Gets all of the body owners in the library,
    /// providing a mapping between bodies and body owners
    #[salsa::invoke(query::collect_body_owners)]
    fn body_owners_of(&self, library: LibraryId) -> Arc<BodyTable>;

    /// Gets all of the type owners in the library,
    /// providing a link between types and type owners.
    #[salsa::invoke(query::collect_type_owners)]
    fn type_owners_of(&self, library: LibraryId) -> Arc<TypeOwners>;

    /// Gets the module tree from the library,
    /// providing a link from child modules to parent modules.
    #[salsa::invoke(query::collect_module_tree)]
    fn module_tree_of(&self, library: LibraryId) -> Arc<ModuleTree>;

    /// Gets the corresponding [`DefOwner`] to the given [`DefId`]
    ///
    /// This does not perform any form of definition resolution.
    #[salsa::invoke(query::lookup_def_owner)]
    fn def_owner(&self, def_id: DefId) -> Option<DefOwner>;

    // FIXME: Make body_owner lookup infallible
    /// Gets the corresponding [`BodyOwner`](body::BodyOwner) to the given [`BodyId`](body::BodyId)
    #[salsa::invoke(query::lookup_body_owner)]
    fn body_owner(&self, body: InLibrary<body::BodyId>) -> Option<body::BodyOwner>;

    /// Gets the corresponding [`TypeOwner`](ty::TypeOwner) for the given [`TypeId`](ty::TypeId)
    #[salsa::invoke(query::lookup_type_owner)]
    fn type_owner(&self, ty: InLibrary<ty::TypeId>) -> ty::TypeOwner;

    /// Gets the parent module of the given module.
    #[salsa::invoke(query::lookup_module_parent)]
    fn module_parent(&self, module: InLibrary<item::ModuleId>) -> Option<item::ModuleId>;

    /// Tests if `parent` is an ancestor module of `child`.
    /// All modules are their own ancestors.
    #[salsa::invoke(query::is_module_ancestor)]
    fn is_module_ancestor(
        &self,
        parent: InLibrary<item::ModuleId>,
        child: InLibrary<item::ModuleId>,
    ) -> bool;

    /// Looks up which module the given HIR node is in
    #[salsa::invoke(query::lookup_inside_module)]
    fn inside_module(&self, inside_mod: InsideModule) -> item::ModuleId;

    /// Looks up the corresponding item for the given [`DefId`],
    /// or `None` if it doesn't exist.
    ///
    /// This does not perform any form of definition resolution.
    #[salsa::invoke(query::lookup_item)]
    fn item_of(&self, def_id: DefId) -> Option<InLibrary<item::ItemId>>;

    /// Gets all of the bodies in the given library
    #[salsa::invoke(query::lookup_bodies)]
    fn bodies_of(&self, library: LibraryId) -> Arc<Vec<body::BodyId>>;

    /// Resolved the given `def_id` to the canonical definition (i.e. beyond any exports),
    /// or itself if it is one.
    #[salsa::invoke(query::resolve_def)]
    fn resolve_def(&self, def_id: DefId) -> DefId;
}

/// Any HIR node that can uniquely be inside of a module
///
/// ## Note
///
/// [`DefId`]s can't be uniquely inside of a module as undeclared don't have a
/// declaration that pins them to a specific module.
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
