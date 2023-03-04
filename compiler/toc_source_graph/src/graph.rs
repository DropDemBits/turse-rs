//! Roots to build the implicit source graph

use toc_salsa_collections::IdMap;

use crate::{Db, DependencyList, Library};

/// Libraries to start walking the dependency graph from
#[salsa::input(singleton)]
pub struct RootLibraries {
    #[return_ref]
    pub roots: Vec<Library>,
}

/// Gets the full source graph with all of its dependencies.
///
/// If there is a cycle in the dependency graph, [`CyclicDependencies`]
/// describes which library had a cyclic dependency
#[salsa::tracked(return_ref)]
pub fn source_graph(db: &dyn Db) -> Result<SourceGraph, CyclicDependencies> {
    // FIXME: Actually explore the dependencies of the roots
    let roots = RootLibraries::get(db).roots(db);
    let graph = roots.iter().map(|lib| (*lib, lib.depends(db))).collect();

    Ok(SourceGraph::new(db, graph))
}

#[salsa::tracked]
pub struct SourceGraph {
    graph: IdMap<Library, DependencyList>,
}

#[salsa::tracked]
impl SourceGraph {
    #[salsa::tracked(return_ref)]
    #[allow(clippy::needless_lifetimes)] // FIXME(salsa-2022): embed in proc-macro codegen
    pub fn all_libraries(self, db: &dyn Db) -> Vec<Library> {
        self.graph(db).keys().collect::<Vec<_>>()
    }
}

/// A dependency cycle is present in at least one library
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CyclicDependencies {
    // FIXME: Record entire cycle
    /// Library which participates in the cycle
    library: Library,
}
