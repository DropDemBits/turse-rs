//! Input graph of all of the packages and their dependencies

mod graph;
mod package;

#[cfg(test)]
mod test;

pub use graph::{CyclicDependencies, RootPackages, SourceGraph, source_graph};
pub use package::{ArtifactKind, DependencyInfo, DependencyList, Package, PackageId};

#[salsa::db]
pub trait Db: salsa::Database {}

#[salsa::db]
impl<DB> Db for DB where DB: salsa::Database {}
