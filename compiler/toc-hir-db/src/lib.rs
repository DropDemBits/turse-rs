//! Definition of HIR related queries (including identifier resolution)

// How to generate the Library graph:
// We're passed a graph of `FileId`s and asked to lower them
// `SourceRoots` -> `LibraryGraph`
// It's just a simple `map` operation

use std::sync::Arc;

use crate::db::InsideModule;
use toc_hir::{
    body::{self, BodyTable},
    item::{self, ModuleTree},
    library::{InLibrary, LoweredLibrary},
    symbol::{DefId, DefOwner, DefTable, SymbolKind},
    ty::{self, TypeOwners},
};
use toc_source_graph::LibraryId;
use upcast::{Upcast, UpcastFrom};

pub mod db;
mod query;

#[salsa::jar(db = Db)]
pub struct Jar(
    query::hir_library,
    query::defs_of,
    query::bodies_of,
    query::body_owners_of,
    query::type_owners_of,
    query::module_tree_of,
    query::is_module_ancestor,
);

// These are still routed through the db trait instead of being called directly
// since that would require changing a whole lot of places
/// HIR tree related queries
pub trait Db:
    salsa::DbWithJar<Jar> + toc_hir_lowering::Db + Upcast<dyn toc_hir_lowering::Db>
{
    /// Get the library associated with the given id
    fn library(&self, library: LibraryId) -> LoweredLibrary;

    /// Gets all of the definitions in the library,
    /// providing a mapping between definitions and definition owners.
    fn defs_of(&self, library: LibraryId) -> Arc<DefTable>;

    /// Gets all of the body owners in the library,
    /// providing a mapping between bodies and body owners
    fn body_owners_of(&self, library: LibraryId) -> Arc<BodyTable>;

    /// Gets all of the type owners in the library,
    /// providing a link between types and type owners.
    fn type_owners_of(&self, library: LibraryId) -> Arc<TypeOwners>;

    /// Gets the module tree from the library,
    /// providing a link from child modules to parent modules.
    fn module_tree_of(&self, library: LibraryId) -> Arc<ModuleTree>;

    /// Gets the corresponding [`DefOwner`] to the given [`DefId`]
    ///
    /// This does not perform any form of definition resolution.
    fn def_owner(&self, def_id: DefId) -> Option<DefOwner>;

    // FIXME: Make body_owner lookup infallible
    /// Gets the corresponding [`BodyOwner`](body::BodyOwner) to the given [`BodyId`](body::BodyId)
    fn body_owner(&self, body: InLibrary<body::BodyId>) -> Option<body::BodyOwner>;

    /// Gets the corresponding [`TypeOwner`](ty::TypeOwner) for the given [`TypeId`](ty::TypeId)
    fn type_owner(&self, ty: InLibrary<ty::TypeId>) -> ty::TypeOwner;

    /// Gets the parent module of the given module.
    fn module_parent(&self, module: InLibrary<item::ModuleId>) -> Option<item::ModuleId>;

    /// Tests if `parent` is an ancestor module of `child`.
    /// All modules are their own ancestors.
    fn is_module_ancestor(
        &self,
        parent: InLibrary<item::ModuleId>,
        child: InLibrary<item::ModuleId>,
    ) -> bool;

    /// Looks up which module the given HIR node is in
    fn inside_module(&self, inside_module: InsideModule) -> item::ModuleId;

    /// Looks up the corresponding item for the given [`DefId`],
    /// or `None` if it doesn't exist.
    ///
    /// This does not perform any form of definition resolution.
    fn item_of(&self, def_id: DefId) -> Option<InLibrary<item::ItemId>>;

    /// Gets all of the bodies in the given library
    fn bodies_of(&self, library: LibraryId) -> Arc<Vec<body::BodyId>>;

    /// Resolved the given `def_id` to the canonical definition (i.e. beyond any exports),
    /// or the last def before an undeclared was encountered.
    fn resolve_def(&self, def_id: DefId) -> Result<DefId, DefId>;

    /// Gets the [`SymbolKind`] of the given `def_id`, or [`None`] if it's an undeclared definition.
    ///
    /// This does not perform any form of definition resolution.
    fn symbol_kind(&self, def_id: DefId) -> Option<SymbolKind>;
}

impl<DB> Db for DB
where
    DB: salsa::DbWithJar<Jar> + toc_hir_lowering::Db + Upcast<dyn toc_hir_lowering::Db>,
{
    fn library(&self, library: LibraryId) -> LoweredLibrary {
        query::hir_library(self, library.0)
    }

    fn defs_of(&self, library: LibraryId) -> Arc<DefTable> {
        query::defs_of(self, library.0)
    }

    fn body_owners_of(&self, library: LibraryId) -> Arc<BodyTable> {
        query::body_owners_of(self, library.0)
    }

    fn type_owners_of(&self, library: LibraryId) -> Arc<TypeOwners> {
        query::type_owners_of(self, library.0)
    }

    fn module_tree_of(&self, library: LibraryId) -> Arc<ModuleTree> {
        query::module_tree_of(self, library.0)
    }

    fn def_owner(&self, def_id: DefId) -> Option<DefOwner> {
        query::def_owner(self, def_id)
    }

    fn body_owner(&self, body: InLibrary<body::BodyId>) -> Option<body::BodyOwner> {
        query::body_owner(self, body)
    }

    fn type_owner(&self, ty: InLibrary<ty::TypeId>) -> ty::TypeOwner {
        query::type_owner(self, ty)
    }

    fn module_parent(&self, module: InLibrary<item::ModuleId>) -> Option<item::ModuleId> {
        query::module_parent(self, module)
    }

    fn is_module_ancestor(
        &self,
        parent: InLibrary<item::ModuleId>,
        child: InLibrary<item::ModuleId>,
    ) -> bool {
        // Must be from the same library
        if parent.0 != child.0 {
            return false;
        }
        query::is_module_ancestor(self, parent.0 .0, parent.1, child.1)
    }

    fn inside_module(&self, inside_module: InsideModule) -> item::ModuleId {
        query::inside_module(self, inside_module)
    }

    fn item_of(&self, def_id: DefId) -> Option<InLibrary<item::ItemId>> {
        query::item_of(self, def_id)
    }

    fn bodies_of(&self, library: LibraryId) -> Arc<Vec<body::BodyId>> {
        query::bodies_of(self, library.0)
    }

    fn resolve_def(&self, def_id: DefId) -> Result<DefId, DefId> {
        query::resolve_def(self, def_id)
    }

    fn symbol_kind(&self, def_id: DefId) -> Option<SymbolKind> {
        query::symbol_kind(self, def_id)
    }
}

impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}
