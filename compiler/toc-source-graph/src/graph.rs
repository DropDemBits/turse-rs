//! Roots to build the implicit source graph

use toc_salsa_collections::IdMap;

use crate::{Db, DependencyList, Package};

/// Packages to start walking the dependency graph from
#[salsa::input(singleton)]
pub struct RootPackages {
    #[return_ref]
    pub roots: Vec<Package>,
}

/// Gets the full source graph with all of its dependencies.
///
/// If there is a cycle in the dependency graph, [`CyclicDependencies`]
/// describes which package had a cyclic dependency
#[salsa::tracked(return_ref)]
pub fn source_graph(db: &dyn Db) -> Result<SourceGraph, CyclicDependencies> {
    // FIXME: Actually explore the dependencies of the roots
    let roots = RootPackages::get(db).roots(db);
    let graph = roots.iter().map(|pkg| (*pkg, pkg.depends(db))).collect();

    Ok(SourceGraph::new(db, graph))
}

#[salsa::tracked]
pub struct SourceGraph {
    graph: IdMap<Package, DependencyList>,
}

#[salsa::tracked]
impl SourceGraph {
    #[salsa::tracked(return_ref)]
    pub fn all_packages(self, db: &dyn Db) -> Vec<Package> {
        self.graph(db).keys().collect::<Vec<_>>()
    }
}

/// A dependency cycle is present in at least one package
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CyclicDependencies {
    // FIXME: Record entire cycle
    /// Package which participates in the cycle
    package: Package,
}
