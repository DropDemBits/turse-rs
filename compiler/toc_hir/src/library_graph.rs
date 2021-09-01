//! Library graph structures

use std::{convert::TryInto, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use toc_span::FileId;

use crate::library::LibraryId;

/// Graph of all library dependencies, represented by [`LibraryId`]s.
/// Provides a bijective mapping between [`LibraryId`]s and [`FileId`]s.
///
/// Data is wrapped in an [`Arc`], so cloning is trivially cloneable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LibraryGraph {
    graph_data: Arc<GraphData>,
}

impl LibraryGraph {
    pub fn library_of(&self, file: FileId) -> LibraryId {
        *self.graph_data.root_map.get(&file).unwrap()
    }

    pub fn file_of(&self, library: LibraryId) -> FileId {
        *self
            .graph_data
            .lib_files
            .get_index(library.0 as usize)
            .unwrap()
    }

    pub fn library_roots(&self) -> impl Iterator<Item = (FileId, LibraryId)> + '_ {
        self.graph_data.root_map.iter().map(|v| (*v.0, *v.1))
    }
}

/// Builder for constructing a library graph
// This is not a real dependency graph, and will never be until we expose
// creating library roots in some form (either in syntax,
// or a project description)
// FIXME: Actually make this a dependency graph builder
// once user specifiable library roots are implemented#[derive(Default)]
#[derive(Default)]
pub struct GraphBuilder {
    graph_data: GraphData,
}

impl GraphBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_library(&mut self, root: FileId) {
        let idx = self.graph_data.lib_files.insert_full(root).0;
        let raw = idx.try_into().expect("too many libraries");
        let id = LibraryId(raw);
        self.graph_data.root_map.insert(root, id);
    }

    pub fn finish(self) -> LibraryGraph {
        LibraryGraph {
            graph_data: Arc::new(self.graph_data),
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct GraphData {
    root_map: IndexMap<FileId, LibraryId>,
    lib_files: IndexSet<FileId>,
}
