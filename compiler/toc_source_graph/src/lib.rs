use std::{fmt, ops::Deref, sync::Arc};

use petgraph::{
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{DfsPostOrder, Visitable},
};
use toc_span::FileId;

#[cfg(test)]
mod test;

/// Source information about a library
#[derive(Debug, Clone)]
pub struct SourceLibrary {
    /// Name of the library
    pub name: String,
    /// The main file of the library where all of the other files are discovered from
    pub root: FileId,
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

/// Information about a library depdendency
#[derive(Debug, Clone)]
struct LibraryDep();

/// A reference to a library in the [`SourceGraph`]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(NodeIndex);

/// Library source graph
#[derive(Debug, Clone, Default)]
pub struct SourceGraph {
    libraries: LibraryGraph,
}

#[derive(Debug, Clone)]
pub struct LibraryDeps<'g> {
    graph: &'g LibraryGraph,
    visitor: DfsPostOrder<NodeIndex, <LibraryGraph as Visitable>::Map>,
}

/// Wrapper reference to a specific [`SourceLibrary`].
/// Workaround for the salsa version we use not being able to return references
#[derive(Debug, Clone)]
pub struct LibraryRef {
    source_graph: Arc<SourceGraph>,
    library_id: LibraryId,
}

impl Eq for LibraryRef {}

impl PartialEq for LibraryRef {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.source_graph, &other.source_graph) && self.library_id == other.library_id
    }
}

type LibraryGraph = StableDiGraph<SourceLibrary, LibraryDep>;

impl fmt::Debug for LibraryId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LibraryId").field(&self.0.index()).finish()
    }
}

impl SourceGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a library to the graph
    pub fn add_library(&mut self, library: SourceLibrary) -> LibraryId {
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

    /// Gets the corresponding [`SourceLibrary`] for a [`LibraryId`]
    pub fn library(&self, library_id: LibraryId) -> &SourceLibrary {
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
    pub fn all_libraries(&self) -> impl Iterator<Item = (LibraryId, &'_ SourceLibrary)> + '_ {
        self.libraries
            .node_indices()
            .map(|idx| (LibraryId(idx), &self.libraries[idx]))
    }
}

impl LibraryRef {
    pub fn new(source_graph: Arc<SourceGraph>, library_id: LibraryId) -> Self {
        Self {
            source_graph,
            library_id,
        }
    }
}

impl Deref for LibraryRef {
    type Target = SourceLibrary;

    fn deref(&self) -> &Self::Target {
        self.source_graph.library(self.library_id)
    }
}

impl<'g> Iterator for LibraryDeps<'g> {
    type Item = (LibraryId, &'g SourceLibrary);

    fn next(&mut self) -> Option<Self::Item> {
        self.visitor
            .next(self.graph)
            .map(|lib_id| (LibraryId(lib_id), &self.graph[lib_id]))
    }
}
