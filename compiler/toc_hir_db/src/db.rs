use std::sync::Arc;

use toc_hir::{
    body,
    body::BodyTable,
    expr, item,
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    symbol::{self, DefId, DefOwner, DefTable},
    ty,
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

    /// Gets the corresponding [`DefOwner`] to the given [`DefId`]
    #[salsa::invoke(query::lookup_def_owner)]
    fn def_owner(&self, def_id: DefId) -> Option<DefOwner>;

    /// Gets the corresponding [`BodyOwner`](body::BodyOwner) to the given [`BodyId`](body::BodyId)
    #[salsa::invoke(query::lookup_body_owner)]
    fn body_owner(&self, body: InLibrary<body::BodyId>) -> Option<body::BodyOwner>;

    /// Looks up the corresponding item for the given `DefId`,
    /// or `None` if it doesn't exist.
    ///
    /// This does not perform any form of identifier resolution.
    #[salsa::invoke(query::lookup_item)]
    fn item_of(&self, def_id: DefId) -> Option<InLibrary<item::ItemId>>;

    /// Gets all of the bodies in the given library
    #[salsa::invoke(query::lookup_bodies)]
    fn bodies_of(&self, library: LibraryId) -> Arc<Vec<body::BodyId>>;

    /// Gets the corresponding definition from the given [`BindingSource`], or `None` if there isn't one.
    #[salsa::invoke(query::binding_to)]
    fn binding_to(&self, ref_src: BindingSource) -> Option<DefId>;

    /// Gets the binding kind of a [`BindingSource`], or a [`NotBinding`](symbol::NotBinding) if it isn't one.
    #[salsa::invoke(query::binding_kind)]
    fn binding_kind(
        &self,
        bind_src: BindingSource,
    ) -> Result<symbol::BindingKind, symbol::NotBinding>;
}

/// Salsa-backed type interner
#[salsa::query_group(InternedTypeStorage)]
pub trait InternedType {
    #[salsa::interned]
    fn intern_type(&self, ty: Arc<ty::Type>) -> salsa::InternId;
}

/// Anything that can produce a reference to a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingSource {
    DefId(DefId),
    Body(LibraryId, body::BodyId),
    BodyExpr(LibraryId, expr::BodyExpr),
}

impl From<symbol::DefId> for BindingSource {
    fn from(def_id: symbol::DefId) -> Self {
        Self::DefId(def_id)
    }
}

impl From<(LibraryId, body::BodyId)> for BindingSource {
    fn from((lib_id, body): (LibraryId, body::BodyId)) -> Self {
        Self::Body(lib_id, body)
    }
}

impl From<(LibraryId, expr::BodyExpr)> for BindingSource {
    fn from(expr: (LibraryId, expr::BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(LibraryId, body::BodyId, expr::ExprId)> for BindingSource {
    fn from(expr: (LibraryId, body::BodyId, expr::ExprId)) -> Self {
        Self::BodyExpr(expr.0, expr::BodyExpr(expr.1, expr.2))
    }
}
