use std::{collections::BTreeMap, fmt};

use salsa::AsId;
use toc_paths::RawPath;
use toc_salsa_collections::IdMap;

#[salsa::jar(db = Db)]
pub struct Jar(
    Library,
    RootLibraries,
    source_graph,
    SourceGraph,
    DependencyList,
    SourceGraph_all_libraries,
);

pub trait Db: salsa::DbWithJar<Jar> {
    fn upcast_to_source_graph_db(&self) -> &dyn Db;
}

impl<DB> Db for DB
where
    DB: salsa::DbWithJar<Jar>,
{
    fn upcast_to_source_graph_db(&self) -> &dyn Db {
        self
    }
}

#[cfg(test)]
mod test;

/// Libraries to start walking the dependency graph from
#[salsa::input(singleton)]
pub struct RootLibraries {
    #[return_ref]
    pub roots: Vec<Library>,
}

/// Source information about a library
#[salsa::input]
pub struct Library {
    /// Name of the library
    #[return_ref]
    pub name: String,
    /// Path to the main file of the library, where all of the other files are discovered from
    pub root: RawPath,
    /// What kind of build artifact this is
    pub artifact: ArtifactKind,
    /// What other libraries this depends on
    pub depends: DependencyList,
}

/// What kind of build artifact this is
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    /// A runnable package. Only one of these can be selected as runnable.
    Binary,
    /// A library package.
    Library,
}

/// What libraries a [`Library`] depends on
#[salsa::input]
pub struct DependencyList {
    depends: BTreeMap<Library, DependencyInfo>,
}

impl DependencyList {
    pub fn empty(db: &dyn Db) -> Self {
        Self::new(db, Default::default())
    }

    /// Adds a library to depend on
    #[allow(unused)]
    pub fn add(self, db: &mut dyn Db, library: Library, info: DependencyInfo) {
        todo!()
    }

    /// Removes a library that is currently depended on
    #[allow(unused)]
    pub fn remove(self, db: &mut dyn Db, library: Library) {
        todo!()
    }

    // ???: Do we want to pass ref to the dep info?
    // Only relevant when we store that information
}

/// Information about a library dependency
#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct DependencyInfo();

/// A reference to a library in the [`SourceGraph`]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(pub Library);

impl fmt::Debug for LibraryId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LibraryId")
            .field(&self.0.as_id().as_u32())
            .finish()
    }
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
