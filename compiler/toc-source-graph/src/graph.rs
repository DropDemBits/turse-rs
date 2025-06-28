//! Roots to build the implicit source graph

use toc_salsa_collections::FxIndexMap;

use crate::{Db, DependencyList, Package};

/// Packages to start walking the dependency graph from
#[salsa::input(singleton)]
pub struct RootPackages {
    #[returns(ref)]
    pub roots: Vec<Package>,
}

/// Gets the full source graph with all of its dependencies.
///
/// If there is a cycle in the dependency graph, [`CyclicDependencies`]
/// describes which package had a cyclic dependency
#[salsa::tracked(returns(ref))]
pub fn source_graph<'db>(db: &'db dyn Db) -> Result<SourceGraph<'db>, CyclicDependencies> {
    // FIXME: Actually explore the dependencies of the roots
    let roots = RootPackages::get(db).roots(db);
    let graph = roots.iter().map(|pkg| (*pkg, pkg.depends(db))).collect();

    Ok(SourceGraph::new(db, graph))
}

#[salsa::tracked]
pub struct SourceGraph<'db> {
    #[tracked]
    #[returns(ref)]
    graph: FxIndexMap<Package, DependencyList>,
}

#[salsa::tracked]
impl<'db> SourceGraph<'db> {
    #[salsa::tracked(returns(ref))]
    pub fn all_packages(self, db: &'db dyn Db) -> Vec<Package> {
        self.graph(db).keys().copied().collect::<Vec<_>>()
    }
}

/// A dependency cycle is present in at least one package
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct CyclicDependencies {
    // FIXME: Record entire cycle
    /// Package which participates in the cycle
    package: Package,
}
