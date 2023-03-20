//! Input graph of all of the packages and their dependencies

mod graph;
mod package;

#[cfg(test)]
mod test;

pub use graph::{source_graph, CyclicDependencies, RootPackages, SourceGraph};
pub use package::{ArtifactKind, DependencyInfo, DependencyList, Package, PackageId};

#[salsa::jar(db = Db)]
pub struct Jar(
    package::Package,
    graph::RootPackages,
    graph::source_graph,
    graph::SourceGraph,
    package::DependencyList,
    graph::SourceGraph_all_packages,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

impl<'db, DB: Db + 'db> upcast::UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}
