use std::collections::HashMap;

use petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use petgraph::visit::{DfsPostOrder, Visitable};
use toc_span::FileId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceDepend {
    pub relative_path: String,
    pub kind: SourceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    Unit,
    Include,
}

type LibraryGraph = DiGraph<FileId, ()>;
type SourceDepGraph = DiGraph<(FileId, SourceKind), ()>;

/// Library source graph
#[derive(Debug, Clone, Default)]
pub struct SourceGraph {
    /// Dependencies between library roots
    libraries: LibraryGraph,
    library_roots: Vec<NodeIndex>,
    library_to_node: HashMap<FileId, NodeIndex>,
}

impl SourceGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a library to the source graph, marking it as a library graph root
    pub fn add_root(&mut self, library: FileId) {
        let root = self.get_or_insert_library(library);
        self.library_roots.push(root);
    }

    // TODO: Add library removal for when we retain db in lsp-server

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
        // Disjoint borrow because we aren't using new closure capture rules
        // FIXME: Simplify once we're on Rust 2021
        let libraries = &mut self.libraries;

        *self
            .library_to_node
            .entry(library)
            .or_insert_with(|| libraries.add_node(library))
    }
}

/// Graph of the source dependencies of a given library
#[derive(Debug, Clone)]
pub struct DependGraph {
    root: FileId,
    /// Files accessible from the given library
    source_deps: SourceDepGraph,
    source_to_node: HashMap<FileId, NodeIndex>,
    depend_links: HashMap<(NodeIndex, String), EdgeIndex>,
}

impl DependGraph {
    pub fn new(root: FileId) -> Self {
        let mut graph = Self {
            root,
            depend_links: Default::default(),
            source_deps: Default::default(),
            source_to_node: Default::default(),
        };

        // Insert the root node
        graph.get_or_insert_dep(root, SourceKind::Unit);

        graph
    }

    /// Adds a source dependency between two files
    pub fn add_source_dep(&mut self, from: FileId, to: FileId, info: SourceDepend) {
        let from = self.source_to_node.get(&from).copied().unwrap();
        let to = self.get_or_insert_dep(to, info.kind);

        let edge_idx = self.source_deps.update_edge(from, to, ());
        self.depend_links
            .insert((from, info.relative_path), edge_idx);
    }

    pub fn depend_of(&self, from: FileId, relative_path: String) -> (FileId, SourceKind) {
        let from = self.source_to_node.get(&from).unwrap();
        let dep_edge = *self
            .depend_links
            .get(&(*from, relative_path))
            .expect("missing depend info");

        let (_from, to) = self.source_deps.edge_endpoints(dep_edge).unwrap();
        let (to_file, to_kind) = self.source_deps[to];

        (to_file, to_kind)
    }

    /// Traverses the sources of the library in DFS post-order
    ///
    /// Ignores any non-unit sources
    pub fn unit_sources(&self) -> LibrarySources<'_> {
        let root = *self.source_to_node.get(&self.root).unwrap();

        LibrarySources {
            graph: &self.source_deps,
            visitor: DfsPostOrder::new(&self.source_deps, root),
        }
    }

    fn get_or_insert_dep(&mut self, dep: FileId, kind: SourceKind) -> NodeIndex {
        // Disjoint borrow because we aren't using new closure capture rules
        // FIXME: Simplify once we're on Rust 2021
        let source_deps = &mut self.source_deps;

        *self
            .source_to_node
            .entry(dep)
            .or_insert_with(|| source_deps.add_node((dep, kind)))
    }
}

#[derive(Debug, Clone)]
pub struct LibraryRoots<'g, 'r> {
    graph: &'g LibraryGraph,
    roots: std::slice::Iter<'r, NodeIndex>,
    visitor: Option<DfsPostOrder<NodeIndex, <LibraryGraph as Visitable>::Map>>,
}

impl Iterator for LibraryRoots<'_, '_> {
    type Item = FileId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let node = {
                // Disjoint borrows
                let graph = self.graph;

                self.visitor
                    .as_mut()
                    .and_then(|visitor| visitor.next(graph))
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

#[derive(Debug, Clone)]
pub struct LibrarySources<'g> {
    graph: &'g SourceDepGraph,
    visitor: DfsPostOrder<NodeIndex, <SourceDepGraph as Visitable>::Map>,
}

impl Iterator for LibrarySources<'_> {
    type Item = FileId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let node = self.visitor.next(&self.graph)?;
            let (file, kind) = self.graph.node_weight(node).copied().unwrap();

            if matches!(kind, SourceKind::Unit) {
                break Some(file);
            }
        }
    }
}
