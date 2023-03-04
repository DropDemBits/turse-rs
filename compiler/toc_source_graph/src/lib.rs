//! Input graph of all of the libraries and their dependencies

mod graph;
mod library;

#[cfg(test)]
mod test;

pub use graph::{source_graph, CyclicDependencies, RootLibraries, SourceGraph};
pub use library::{ArtifactKind, DependencyInfo, DependencyList, Library, LibraryId};

#[salsa::jar(db = Db)]
pub struct Jar(
    library::Library,
    graph::RootLibraries,
    graph::source_graph,
    graph::SourceGraph,
    library::DependencyList,
    graph::SourceGraph_all_libraries,
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
