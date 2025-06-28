//! Individual package inputs

use std::{collections::BTreeMap, fmt};

use toc_paths::RawPath;

use crate::Db;

/// Source information about a package
#[salsa::input(debug)]
#[derive(PartialOrd, Ord)]
pub struct Package {
    /// Name of the package
    #[returns(ref)]
    pub name: String,
    /// Path to the main file of the package, where all of the other files are discovered from
    pub root: RawPath,
    /// What kind of build artifact this is
    pub artifact: ArtifactKind,
    /// What other packages this depends on
    pub depends: DependencyList,
}

/// What kind of build artifact this is
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    /// A runnable package. Only one of these can be selected as runnable.
    Binary,
    /// A package package.
    Package,
}

/// What packages a [`Package`] depends on
#[salsa::input(debug)]
pub struct DependencyList {
    depends: BTreeMap<Package, DependencyInfo>,
}

impl DependencyList {
    pub fn empty(db: &dyn Db) -> Self {
        Self::new(db, Default::default())
    }

    /// Adds a package to depend on
    #[allow(unused)]
    pub fn add(self, db: &mut dyn Db, package: Package, info: DependencyInfo) {
        todo!()
    }

    /// Removes a package that is currently depended on
    #[allow(unused)]
    pub fn remove(self, db: &mut dyn Db, package: Package) {
        todo!()
    }

    // ???: Do we want to pass ref to the dep info?
    // Only relevant when we store that information
}

/// Information about a package dependency
#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct DependencyInfo();

/// A reference to a package in the [`SourceGraph`](crate::SourceGraph)
// FIXME: Move all uses to [`Package`] directly (implict through new tracked struct rewrite)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PackageId(pub Package);

impl fmt::Debug for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("PackageId").field(&self.0.0.index()).finish()
    }
}

impl From<PackageId> for Package {
    fn from(value: PackageId) -> Self {
        value.0
    }
}

impl From<Package> for PackageId {
    fn from(value: Package) -> Self {
        Self(value)
    }
}
