//! Definition of analysis queries, as well as re-exports of internal database types

use toc_hir::library::LibraryId;
use toc_reporting::CompileResult;
use toc_salsa::salsa;

use crate::{lints, query, typeck};

pub use crate::const_eval::db::*;
pub use crate::ty::db::*;

/// HIR Analysis queries
#[salsa::query_group(HirAnalysisStorage)]
pub trait HirAnalysis: TypeDatabase + ConstEval {
    /// Performs analysis passes on all libraries
    #[salsa::invoke(query::analyze_libraries)]
    fn analyze_libraries(&self) -> CompileResult<()>;

    /// Checks the given library to ensure that all type rules are followed,
    /// and that all types are well-formed.
    #[salsa::invoke(typeck::typecheck_library)]
    fn typecheck_library(&self, library: LibraryId) -> CompileResult<()>;

    /// Runs the lint passes over given library
    #[salsa::invoke(lints::lint_library)]
    fn lint_library(&self, library: LibraryId) -> CompileResult<()>;
}
