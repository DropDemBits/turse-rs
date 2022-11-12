use std::fmt;

use petgraph::{
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{DfsPostOrder, Visitable},
};
use toc_span::FileId;

#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
pub struct Library {
    /// Name of the library
    pub name: String,
    /// The main file of the library where all of the other files are discovered from
    pub root: FileId,
    /// What kind of build artifact this is
    pub artifact: ArtifactKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    /// A runnable package. Only one of these can be selected as runnable.
    Binary,
    /// A library package.
    Library,
}

/// Information about a library depdendency
#[derive(Debug, Clone)]
struct LibraryDep();

type LibraryGraph = StableDiGraph<Library, LibraryDep>;

/// A reference to a library in the [`SourceGraph`]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(NodeIndex);

impl fmt::Debug for LibraryId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LibraryId").field(&self.0.index()).finish()
    }
}

/// Library source graph
#[derive(Debug, Clone, Default)]
pub struct SourceGraph {
    libraries: LibraryGraph,
}

impl SourceGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a library to the graph
    pub fn add_library(&mut self, library: Library) -> LibraryId {
        LibraryId(self.libraries.add_node(library))
    }

    /// Removes the specified library from the graph
    pub fn remove_library(&mut self, library_id: LibraryId) {
        self.libraries.remove_node(library_id.0);
    }

    /// Adds a dependency between libraries
    pub fn add_library_dep(&mut self, from: LibraryId, to: LibraryId) {
        self.libraries.update_edge(from.0, to.0, LibraryDep());
    }

    /// Gets the corresponding [`Library`] for a [`LibraryId`]
    pub fn library(&self, library_id: LibraryId) -> &Library {
        &self.libraries[library_id.0]
    }

    /// Traverses the library and its dependencies in DFS order
    pub fn library_deps(&self, from_library: LibraryId) -> LibraryDeps<'_> {
        LibraryDeps {
            graph: &self.libraries,
            visitor: DfsPostOrder::new(&self.libraries, from_library.0),
        }
    }

    /// Iterates over all libraries in the graph
    pub fn all_libraries(&self) -> impl Iterator<Item = (LibraryId, &'_ Library)> + '_ {
        self.libraries
            .node_indices()
            .map(|idx| (LibraryId(idx), &self.libraries[idx]))
    }
}

#[derive(Debug, Clone)]
pub struct LibraryDeps<'g> {
    graph: &'g LibraryGraph,
    visitor: DfsPostOrder<NodeIndex, <LibraryGraph as Visitable>::Map>,
}

impl<'g> Iterator for LibraryDeps<'g> {
    type Item = (LibraryId, &'g Library);

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor
            .next(self.graph)
            .map(|lib_id| (LibraryId(lib_id), &self.graph[lib_id]))
    }
}
