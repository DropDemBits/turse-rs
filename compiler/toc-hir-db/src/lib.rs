//! Definition of HIR related queries (including identifier resolution)

// How to generate the Package graph:
// We're passed a graph of `FileId`s and asked to lower them
// `SourceRoots` -> `PackageGraph`
// It's just a simple `map` operation

use std::sync::Arc;

use crate::db::InsideModule;
use toc_hir::{
    body::{self, BodyTable},
    item::{self, ModuleTree},
    package::{InPackage, LoweredPackage, PackageId},
    symbol::{DefId, DefOwner, DefTable, SymbolKind},
    ty::{self, TypeOwners},
};

pub mod db;
mod query;

// These are still routed through the db trait instead of being called directly
// since that would require changing a whole lot of places
/// HIR tree related queries
#[salsa::db]
pub trait Db: toc_hir_lowering::Db {
    /// Get the package associated with the given id
    fn package(&self, package: PackageId) -> LoweredPackage;

    /// Gets all of the definitions in the package,
    /// providing a mapping between definitions and definition owners.
    fn defs_of(&self, package: PackageId) -> Arc<DefTable>;

    /// Gets all of the body owners in the package,
    /// providing a mapping between bodies and body owners
    fn body_owners_of(&self, package: PackageId) -> Arc<BodyTable>;

    /// Gets all of the type owners in the package,
    /// providing a link between types and type owners.
    fn type_owners_of(&self, package: PackageId) -> Arc<TypeOwners>;

    /// Gets the module tree from the package,
    /// providing a link from child modules to parent modules.
    fn module_tree_of(&self, package: PackageId) -> Arc<ModuleTree>;

    /// Gets the corresponding [`DefOwner`] to the given [`DefId`]
    ///
    /// This does not perform any form of definition resolution.
    fn def_owner(&self, def_id: DefId) -> Option<DefOwner>;

    // FIXME: Make body_owner lookup infallible
    /// Gets the corresponding [`BodyOwner`](body::BodyOwner) to the given [`BodyId`](body::BodyId)
    fn body_owner(&self, body: InPackage<body::BodyId>) -> Option<body::BodyOwner>;

    /// Gets the corresponding [`TypeOwner`](ty::TypeOwner) for the given [`TypeId`](ty::TypeId)
    fn type_owner(&self, ty: InPackage<ty::TypeId>) -> ty::TypeOwner;

    /// Gets the parent module of the given module.
    fn module_parent(&self, module: InPackage<item::ModuleId>) -> Option<item::ModuleId>;

    /// Tests if `parent` is an ancestor module of `child`.
    /// All modules are their own ancestors.
    fn is_module_ancestor(
        &self,
        parent: InPackage<item::ModuleId>,
        child: InPackage<item::ModuleId>,
    ) -> bool;

    /// Looks up which module the given HIR node is in
    fn inside_module(&self, inside_module: InsideModule) -> item::ModuleId;

    /// Looks up the corresponding item for the given [`DefId`],
    /// or `None` if it doesn't exist.
    ///
    /// This does not perform any form of definition resolution.
    fn item_of(&self, def_id: DefId) -> Option<InPackage<item::ItemId>>;

    /// Gets all of the bodies in the given package
    fn bodies_of(&self, package: PackageId) -> Arc<Vec<body::BodyId>>;

    /// Resolved the given `def_id` to the canonical definition (i.e. beyond any exports),
    /// or the last def before an undeclared was encountered.
    fn resolve_def(&self, def_id: DefId) -> Result<DefId, DefId>;

    /// Gets the [`SymbolKind`] of the given `def_id`, or [`None`] if it's an undeclared definition.
    ///
    /// This does not perform any form of definition resolution.
    fn symbol_kind(&self, def_id: DefId) -> Option<SymbolKind>;
}

#[salsa::db]
impl<DB> Db for DB
where
    DB: toc_hir_lowering::Db,
{
    fn package(&self, package: PackageId) -> LoweredPackage {
        query::hir_package(self, package.0)
    }

    fn defs_of(&self, package: PackageId) -> Arc<DefTable> {
        query::defs_of(self, package.0)
    }

    fn body_owners_of(&self, package: PackageId) -> Arc<BodyTable> {
        query::body_owners_of(self, package.0)
    }

    fn type_owners_of(&self, package: PackageId) -> Arc<TypeOwners> {
        query::type_owners_of(self, package.0)
    }

    fn module_tree_of(&self, package: PackageId) -> Arc<ModuleTree> {
        query::module_tree_of(self, package.0)
    }

    fn def_owner(&self, def_id: DefId) -> Option<DefOwner> {
        query::def_owner(self, def_id)
    }

    fn body_owner(&self, body: InPackage<body::BodyId>) -> Option<body::BodyOwner> {
        query::body_owner(self, body)
    }

    fn type_owner(&self, ty: InPackage<ty::TypeId>) -> ty::TypeOwner {
        query::type_owner(self, ty)
    }

    fn module_parent(&self, module: InPackage<item::ModuleId>) -> Option<item::ModuleId> {
        query::module_parent(self, module)
    }

    fn is_module_ancestor(
        &self,
        parent: InPackage<item::ModuleId>,
        child: InPackage<item::ModuleId>,
    ) -> bool {
        // Must be from the same package
        if parent.0 != child.0 {
            return false;
        }
        query::is_module_ancestor(self, parent.0.0, parent.1, child.1)
    }

    fn inside_module(&self, inside_module: InsideModule) -> item::ModuleId {
        query::inside_module(self, inside_module)
    }

    fn item_of(&self, def_id: DefId) -> Option<InPackage<item::ItemId>> {
        query::item_of(self, def_id)
    }

    fn bodies_of(&self, package: PackageId) -> Arc<Vec<body::BodyId>> {
        query::bodies_of(self, package.0)
    }

    fn resolve_def(&self, def_id: DefId) -> Result<DefId, DefId> {
        query::resolve_def(self, def_id)
    }

    fn symbol_kind(&self, def_id: DefId) -> Option<SymbolKind> {
        query::symbol_kind(self, def_id)
    }
}
