//! Query implementions

use toc_hir::{
    item,
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::{GraphBuilder, LibraryGraph},
    symbol::DefId,
};
use toc_reporting::CompileResult;

use crate::HirDatabase;

pub fn library_query(db: &dyn HirDatabase, library: LibraryId) -> LoweredLibrary {
    let lib_graph = db.library_graph();
    let file = lib_graph.result().file_of(library);
    db.lower_library(file).result().clone()
}

pub fn library_graph_query(db: &dyn HirDatabase) -> CompileResult<LibraryGraph> {
    let mut messages = vec![];
    let source_roots = db.source_roots();
    let mut graph = GraphBuilder::new();

    for root in source_roots.roots() {
        graph.add_library(root);

        let res = db.lower_library(root);
        res.bundle_messages(&mut messages);
    }

    CompileResult::new(graph.finish(), messages)
}

pub fn lookup_item(db: &dyn HirDatabase, def_id: DefId) -> Option<InLibrary<item::ItemId>> {
    db.library(def_id.0)
        .item_of(def_id.1)
        .map(|id| InLibrary(def_id.0, id))
}
