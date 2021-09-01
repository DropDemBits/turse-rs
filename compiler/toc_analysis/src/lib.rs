//! General code analysis, including type checking and dead code reporting

use toc_hir::library::LibraryId;
use toc_reporting::CompileResult;
use toc_salsa::salsa;

mod const_eval;
mod query;
mod typeck;

pub mod db;
pub mod ty;

#[cfg(test)]
mod test_db;

/// HIR Analysis queries
#[salsa::query_group(HirAnalysisStorage)]
pub trait HirAnalysis: db::TypeDatabase + db::ConstEval {
    /// Performs analysis passes on all libraries
    #[salsa::invoke(query::analyze_libraries)]
    fn analyze_libraries(&self) -> CompileResult<()>;

    /// Checks the given library to ensure that all type rules are followed,
    /// and that all types are well-formed.
    #[salsa::invoke(typeck::typecheck_library)]
    fn typecheck_library(&self, library: LibraryId) -> CompileResult<()>;
}
