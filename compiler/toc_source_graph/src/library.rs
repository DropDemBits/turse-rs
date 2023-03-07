//! Individual library inputs

use std::{collections::BTreeMap, fmt};

use salsa::AsId;
use toc_paths::RawPath;

use crate::Db;

/// Source information about a library
#[salsa::input]
pub struct Library {
    /// Name of the library
    #[return_ref]
    pub name: String,
    /// Path to the main file of the library, where all of the other files are discovered from
    pub root: RawPath,
    /// What kind of build artifact this is
    pub artifact: ArtifactKind,
    /// What other libraries this depends on
    pub depends: DependencyList,
}

/// What kind of build artifact this is
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    /// A runnable package. Only one of these can be selected as runnable.
    Binary,
    /// A library package.
    Library,
}

/// What libraries a [`Library`] depends on
#[salsa::input]
pub struct DependencyList {
    depends: BTreeMap<Library, DependencyInfo>,
}

impl DependencyList {
    pub fn empty(db: &dyn Db) -> Self {
        Self::new(db, Default::default())
    }

    /// Adds a library to depend on
    #[allow(unused)]
    pub fn add(self, db: &mut dyn Db, library: Library, info: DependencyInfo) {
        todo!()
    }

    /// Removes a library that is currently depended on
    #[allow(unused)]
    pub fn remove(self, db: &mut dyn Db, library: Library) {
        todo!()
    }

    // ???: Do we want to pass ref to the dep info?
    // Only relevant when we store that information
}

/// Information about a library dependency
#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct DependencyInfo();

/// A reference to a library in the [`SourceGraph`](crate::SourceGraph)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(pub Library);

impl fmt::Debug for LibraryId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LibraryId")
            .field(&self.0.as_id().as_u32())
            .finish()
    }
}

impl From<LibraryId> for Library {
    fn from(value: LibraryId) -> Self {
        value.0
    }
}

impl From<Library> for LibraryId {
    fn from(value: Library) -> Self {
        Self(value)
    }
}
