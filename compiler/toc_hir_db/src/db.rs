use std::sync::Arc;

use toc_hir::item::ModuleTree;
use toc_hir::{
    body,
    body::BodyTable,
    item,
    library::{InLibrary, LibraryId, LoweredLibrary},
    library_graph::LibraryGraph,
    symbol::{DefId, DefOwner, DefTable},
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

    /// Gets the module tree from the library,
    /// providing a link from child modules to parent modules.
    #[salsa::invoke(query::collect_module_tree)]
    fn module_tree_of(&self, library: LibraryId) -> Arc<ModuleTree>;

    /// Gets the corresponding [`DefOwner`] to the given [`DefId`]
    ///
    /// This does not perform any form of definition resolution.
    #[salsa::invoke(query::lookup_def_owner)]
    fn def_owner(&self, def_id: DefId) -> Option<DefOwner>;

    /// Gets the corresponding [`BodyOwner`](body::BodyOwner) to the given [`BodyId`](body::BodyId)
    #[salsa::invoke(query::lookup_body_owner)]
    fn body_owner(&self, body: InLibrary<body::BodyId>) -> Option<body::BodyOwner>;

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
