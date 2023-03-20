//! Source package structures.
//!
//! This reexports from [`toc_source_graph`] since HIR is an abstraction boundary,
//! and we reuse the same [`PackageId`] to refer to the HIR version of the package defs.

pub use toc_source_graph::{
    ArtifactKind, DependencyList, Package as SourcePackage, PackageId, SourceGraph,
};
