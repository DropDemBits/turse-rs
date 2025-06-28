//! Definition of analysis queries, as well as re-exports of internal database types

use toc_hir::package_graph::SourcePackage;
use toc_reporting::CompileResult;

use crate::{lints, query, typeck};

pub use crate::{const_eval::db::*, ty::db::*};

/// HIR Analysis queries
#[salsa::db]
pub trait HirAnalysis: TypeDatabase + ConstEval {
    /// Performs analysis passes on all packages
    fn analyze_packages(&self) -> CompileResult<()>;

    /// Checks the given package to ensure that all type rules are followed,
    /// and that all types are well-formed.
    fn typecheck_package(&self, package: SourcePackage) -> CompileResult<()>;

    /// Runs the lint passes over given package
    fn lint_package(&self, package: SourcePackage) -> CompileResult<()>;
}

#[salsa::db]
impl<DB> HirAnalysis for DB
where
    DB: TypeDatabase + ConstEval,
{
    fn analyze_packages(&self) -> CompileResult<()> {
        query::analyze_packages(self)
    }

    fn typecheck_package(&self, package: SourcePackage) -> CompileResult<()> {
        typeck::typecheck_package(self, package)
    }

    fn lint_package(&self, package: SourcePackage) -> CompileResult<()> {
        lints::lint_package(self, package)
    }
}
