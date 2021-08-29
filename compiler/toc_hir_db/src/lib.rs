//! Definition of HIR related queries (including identifier resolution)

use std::sync::Arc;

use toc_hir::{library_graph::LibraryGraph, ty};
use toc_reporting::CompileResult;
use toc_salsa::salsa;

// ???: How do we generate the Library graph?
// We're probably passed a graph of `FileId`s and asked to lower them
// `SourceRoots` -> `LibraryGraph`

mod query;

/// HIR tree related queries
#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: toc_hir_lowering::LoweringDb {
    /// Graph of all libraries
    #[salsa::invoke(query::library_graph_query)]
    #[salsa::dependencies]
    fn library_graph(&self) -> CompileResult<LibraryGraph>;
}

/// Salsa-backed type interner
#[salsa::query_group(InternedTypeStorage)]
pub trait InternedType {
    #[salsa::interned]
    fn intern_type(&self, ty: Arc<ty::Type>) -> salsa::InternId;
}
