//! Source library structures.
//!
//! This reexports from [`toc_source_graph`] since HIR is an abstraction boundary,
//! and we reuse the same [`LibraryId`] to refer to the HIR version of the library defs.

pub use toc_source_graph::{ArtifactKind, LibraryId, SourceGraph, SourceLibrary};
