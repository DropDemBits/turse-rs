use std::fmt;

use petgraph::{
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{DfsPostOrder, Visitable},
};
use salsa::AsId;
use toc_paths::RawPath;
use toc_salsa_collections::IdMap;

#[salsa::jar(db = Db)]
pub struct Jar(Library);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

#[cfg(test)]
mod test;

/// Source information about a library
#[salsa::input]
pub struct Library {
    /// Name of the library
    pub name: String,
    /// Path to the main file of the library, where all of the other files are discovered from
    pub root: RawPath,
    /// What kind of build artifact this is
    pub artifact: ArtifactKind,
}

/// What kind of build artifact this is
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    /// A runnable package. Only one of these can be selected as runnable.
    Binary,
    /// A library package.
    Library,
}

/// Information about a library dependency
#[derive(Debug, Clone)]
struct LibraryDep();

/// A reference to a library in the [`SourceGraph`]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(Library);

/// Library source graph
#[derive(Debug, Clone, Default)]
pub struct SourceGraph {
    libraries: LibraryGraph,
    to_node_idx: IdMap<Library, NodeIndex>,
}

impl fmt::Debug for LibraryId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LibraryId")
            .field(&self.0.as_id().as_u32())
            .finish()
    }
}

type LibraryGraph = StableDiGraph<Library, LibraryDep>;

impl SourceGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a library to the graph
    pub fn add_library(&mut self, library: Library) -> LibraryId {
        let idx = self.libraries.add_node(library);
        self.to_node_idx.insert(library, idx);
        LibraryId(library)
    }

    /// Removes the specified library from the graph
    pub fn remove_library(&mut self, library_id: LibraryId) {
        let idx = self.to_idx(library_id.0);
        self.libraries.remove_node(idx);
    }

    /// Adds a dependency between libraries
    pub fn add_library_dep(&mut self, from: LibraryId, to: LibraryId) {
        self.libraries
            .update_edge(self.to_idx(from.0), self.to_idx(to.0), LibraryDep());
    }

    /// Gets the corresponding [`Library`] for a [`LibraryId`]
    pub fn library(&self, library_id: LibraryId) -> Library {
        library_id.0
    }

    /// Traverses the library and its dependencies in DFS order
    pub fn library_deps(&self, from_library: LibraryId) -> LibraryDeps<'_> {
        LibraryDeps {
            graph: &self.libraries,
            visitor: DfsPostOrder::new(&self.libraries, self.to_idx(from_library.0)),
        }
    }

    /// Iterates over all libraries in the graph
    pub fn all_libraries(&self) -> impl Iterator<Item = (LibraryId, Library)> + '_ {
        self.libraries
            .node_indices()
            .map(|idx| (LibraryId(self.libraries[idx]), self.libraries[idx]))
    }

    fn to_idx(&self, id: Library) -> NodeIndex {
        *self
            .to_node_idx
            .get(id)
            .expect("library not added to source graph")
    }
}

#[derive(Debug, Clone)]
pub struct LibraryDeps<'g> {
    graph: &'g LibraryGraph,
    visitor: DfsPostOrder<NodeIndex, <LibraryGraph as Visitable>::Map>,
}

impl<'g> Iterator for LibraryDeps<'g> {
    type Item = (LibraryId, Library);

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor
            .next(self.graph)
            .map(|lib_id| (LibraryId(self.graph[lib_id]), self.graph[lib_id]))
    }
}
