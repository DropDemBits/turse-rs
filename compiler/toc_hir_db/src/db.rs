use std::sync::Arc;

use toc_hir::{
    item,
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    symbol::DefId,
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

    /// Looks up the corresponding item for the given `DefId`,
    /// or `None` if it doesn't exist.
    ///
    /// This does not perform any form of identifier resolution.
    #[salsa::invoke(query::lookup_item)]
    fn item_of(&self, def_id: DefId) -> Option<InLibrary<item::ItemId>>;
}

/// Salsa-backed type interner
#[salsa::query_group(InternedTypeStorage)]
pub trait InternedType {
    #[salsa::interned]
    fn intern_type(&self, ty: Arc<ty::Type>) -> salsa::InternId;
}
