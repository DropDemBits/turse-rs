use std::collections::HashMap;

use indexmap::IndexSet;
use petgraph::{
    graph::NodeIndex,
    stable_graph::StableDiGraph,
    visit::{DfsPostOrder, Visitable},
};
use toc_span::FileId;

#[cfg(test)]
mod test;

type LibraryGraph = StableDiGraph<FileId, ()>;

/// Library source graph
#[derive(Debug, Clone, Default)]
pub struct SourceGraph {
    /// Dependencies between library roots
    libraries: LibraryGraph,
    library_roots: IndexSet<NodeIndex>,
    library_to_node: HashMap<FileId, NodeIndex>,
}

impl SourceGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a library to the source graph, marking it as a library graph root.
    /// Does nothing if it's already a library graph root.
    pub fn add_root(&mut self, library: FileId) {
        let root = self.get_or_insert_library(library);
        self.library_roots.insert(root);
    }

    /// Removes a library from the source graph
    pub fn remove_root(&mut self, library: FileId) {
        let root = self
            .library_to_node
            .remove(&library)
            .expect("file was never added to the source graph");
        self.libraries.remove_node(root);
        self.library_roots.retain(|node| *node != root);
    }

    /// Adds a library dependency between library roots
    ///
    /// Automatically adds library nodes to the source graph
    pub fn add_library_dep(&mut self, from: FileId, to: FileId) {
        let from = self.get_or_insert_library(from);
        let to = self.get_or_insert_library(to);
        self.libraries.update_edge(from, to, ());
    }

    /// Traverses the library roots and their dependencies in DFS post-order
    pub fn library_roots(&self) -> LibraryRoots<'_, '_> {
        LibraryRoots {
            graph: &self.libraries,
            roots: self.library_roots.iter(),
            visitor: None,
        }
    }

    fn get_or_insert_library(&mut self, library: FileId) -> NodeIndex {
        *self
            .library_to_node
            .entry(library)
            .or_insert_with(|| self.libraries.add_node(library))
    }
}

#[derive(Debug, Clone)]
pub struct LibraryRoots<'g, 'r> {
    graph: &'g LibraryGraph,
    roots: indexmap::set::Iter<'r, NodeIndex>,
    visitor: Option<DfsPostOrder<NodeIndex, <LibraryGraph as Visitable>::Map>>,
}

impl Iterator for LibraryRoots<'_, '_> {
    type Item = FileId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let node = {
                self.visitor
                    .as_mut()
                    .and_then(|visitor| visitor.next(self.graph))
            };

            if let Some(node) = node {
                let file = self.graph.node_weight(node).copied().unwrap();

                break Some(file);
            }

            // Move to next (or first) root
            let root = *self.roots.next()?;
            self.visitor = Some(DfsPostOrder::new(self.graph, root));
        }
    }
}
