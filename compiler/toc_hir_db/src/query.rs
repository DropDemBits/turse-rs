//! Query implementations

use toc_hir::{
    item,
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    symbol::DefId,
};

use crate::db::HirDatabase;

pub fn library_query(db: &dyn HirDatabase, library: LibraryId) -> LoweredLibrary {
    let file = db.library_graph().file_of(library);
    db.lower_library(file).result().clone()
}

pub fn library_graph_query(db: &dyn HirDatabase) -> LibraryGraph {
    db.lower_library_graph().result().clone()
}

pub fn lookup_item(db: &dyn HirDatabase, def_id: DefId) -> Option<InLibrary<item::ItemId>> {
    db.library(def_id.0)
        .item_of(def_id.1)
        .map(|id| InLibrary(def_id.0, id))
}
