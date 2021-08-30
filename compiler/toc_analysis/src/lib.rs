//! General code analysis, including type checking and dead code reporting

use toc_hir::library::LibraryId;
use toc_reporting::CompileResult;
use toc_salsa::salsa;

//mod const_eval; // TEMPORARY, uncomment when porting this stage
mod query;
mod typeck;

pub mod db;
pub mod ty;

// TODO: Mock-up what exactly needs to be done to port to the query system
//
// General idea is to split typeck from type collection & HIR constant evaluation
//
// See also:
// - `notes/typeck queryfy/End to end typeck description.md`
// - `notes/query related/Queryfication Plan.mdnotes/query related/Queryfication Plan.md`

/// HIR Analysis queries
#[salsa::query_group(HirAnalysisStorage)]
pub trait HirAnalysis: db::TypeDatabase {
    /// Performs analysis passes on all libraries
    #[salsa::invoke(query::analyze_libraries)]
    fn analyze_libraries(&self) -> CompileResult<()>;

    /// Checks the given library to ensure that all type rules are followed,
    /// and that all types are well-formed.
    #[salsa::invoke(typeck::typecheck_library)]
    fn typecheck_library(&self, library: LibraryId) -> CompileResult<()>;
}
