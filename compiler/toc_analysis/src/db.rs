//! Definition of analysis queries, as well as re-exports of internal database types

use toc_hir::library_graph::SourceLibrary;
use toc_reporting::CompileResult;

use crate::{lints, query, typeck};

pub use crate::{const_eval::db::*, ty::db::*};

#[salsa::jar(db = HirAnalysis)]
pub struct AnalysisJar(
    query::analyze_libraries,
    typeck::typecheck_library,
    lints::lint_library,
);

/// HIR Analysis queries
pub trait HirAnalysis:
    salsa::DbWithJar<AnalysisJar> + TypeDatabase + ConstEval + toc_source_graph::Db
{
    fn upcast_to_anaylsis_db(&self) -> &dyn HirAnalysis;

    /// Performs analysis passes on all libraries
    fn analyze_libraries(&self) -> CompileResult<()>;

    /// Checks the given library to ensure that all type rules are followed,
    /// and that all types are well-formed.
    fn typecheck_library(&self, library: SourceLibrary) -> CompileResult<()>;

    /// Runs the lint passes over given library
    fn lint_library(&self, library: SourceLibrary) -> CompileResult<()>;
}

impl<DB> HirAnalysis for DB
where
    DB: salsa::DbWithJar<AnalysisJar> + TypeDatabase + ConstEval + toc_source_graph::Db,
{
    fn upcast_to_anaylsis_db(&self) -> &dyn HirAnalysis {
        self
    }

    fn analyze_libraries(&self) -> CompileResult<()> {
        query::analyze_libraries(self)
    }

    fn typecheck_library(&self, library: SourceLibrary) -> CompileResult<()> {
        typeck::typecheck_library(self, library)
    }

    fn lint_library(&self, library: SourceLibrary) -> CompileResult<()> {
        lints::lint_library(self, library)
    }
}
