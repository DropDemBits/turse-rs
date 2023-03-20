//! Definition of analysis queries, as well as re-exports of internal database types

use toc_hir::package_graph::SourcePackage;
use toc_reporting::CompileResult;
use upcast::{Upcast, UpcastFrom};

use crate::{lints, query, typeck};

pub use crate::{const_eval::db::*, ty::db::*};

#[salsa::jar(db = HirAnalysis)]
pub struct AnalysisJar(
    query::analyze_packages,
    typeck::typecheck_package,
    lints::lint_package,
);

/// HIR Analysis queries
pub trait HirAnalysis:
    salsa::DbWithJar<AnalysisJar>
    + TypeDatabase
    + ConstEval
    + Upcast<dyn TypeDatabase>
    + Upcast<dyn ConstEval>
{
    /// Performs analysis passes on all packages
    fn analyze_packages(&self) -> CompileResult<()>;

    /// Checks the given package to ensure that all type rules are followed,
    /// and that all types are well-formed.
    fn typecheck_package(&self, package: SourcePackage) -> CompileResult<()>;

    /// Runs the lint passes over given package
    fn lint_package(&self, package: SourcePackage) -> CompileResult<()>;
}

impl<DB> HirAnalysis for DB
where
    DB: salsa::DbWithJar<AnalysisJar>
        + TypeDatabase
        + ConstEval
        + Upcast<dyn TypeDatabase>
        + Upcast<dyn ConstEval>,
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

impl<'db, DB: HirAnalysis + 'db> UpcastFrom<DB> for dyn HirAnalysis + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}
