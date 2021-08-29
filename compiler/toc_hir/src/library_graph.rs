//! Library graph structures

use std::{convert::TryInto, sync::Arc};

use indexmap::IndexMap;
use toc_span::FileId;

use crate::library::{self, LibraryId, LoweredLibrary};

/// Graph of all libraries
#[derive(Debug, Clone)]
pub struct LibraryGraph {
    graph_data: Arc<GraphData>,
}

impl LibraryGraph {
    pub fn of_file(&self, file: FileId) -> &LoweredLibrary {
        let id = *self.graph_data.root_map.get(&file).unwrap();
        self.library(id)
    }

    pub fn library(&self, id: LibraryId) -> &LoweredLibrary {
        &self.graph_data.libraries[id.0 as usize]
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

    pub fn add_library(&mut self, root: FileId, library: LoweredLibrary) {
        let raw: library::LibraryIndex = self
            .graph_data
            .libraries
            .len()
            .try_into()
            .expect("too many libraries");
        let id = LibraryId(raw);

        self.graph_data.libraries.push(library);
        self.graph_data.root_map.insert(root, id);
    }

    pub fn finish(self) -> LibraryGraph {
        LibraryGraph {
            graph_data: Arc::new(self.graph_data),
        }
    }
}

#[derive(Debug, Default)]
struct GraphData {
    libraries: Vec<LoweredLibrary>,
    root_map: IndexMap<FileId, LibraryId>,
}
