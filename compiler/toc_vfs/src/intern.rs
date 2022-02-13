//! Path interning structures

use std::{
    num::NonZeroU32,
    path::{Path, PathBuf},
};

use toc_span::FileId;

/// Interner for paths.
#[derive(Debug, Default)]
pub(crate) struct PathInterner {
    paths: indexmap::IndexSet<PathBuf>,
}

impl PathInterner {
    /// Interns the given path into the corresponding [`FileId`]
    ///
    /// # Panics
    ///
    /// Panics if there are too many paths that are interned
    ///
    /// # Returns
    ///
    /// The corresponding [`FileId`] for the path
    pub(crate) fn intern_path(&mut self, path: impl AsRef<Path>) -> FileId {
        let (id, _exists) = self.paths.insert_full(path.as_ref().to_path_buf());

        Self::mk_file_id(id)
    }

    /// Looks up the path corresponding to the given [`FileId`]
    pub(crate) fn lookup_path(&self, file_id: FileId) -> &std::path::Path {
        self.paths[Self::as_index(file_id)].as_path()
    }

    /// Looks up the [`FileId`] corresponding to the given path.
    pub(crate) fn lookup_id(&self, path: &Path) -> Option<FileId> {
        self.paths.get_index_of(path).map(Self::mk_file_id)
    }

    fn mk_file_id(index: usize) -> FileId {
        FileId::new(NonZeroU32::new((index + 1) as u32).unwrap())
    }

    fn as_index(file_id: FileId) -> usize {
        file_id.raw_id().get() as usize - 1
    }
}

/// The result of resolving a path, which may or may not be interned.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathResolution {
    /// An interned path.
    Interned(FileId),
    /// An absolute path that has not been interned yet.
    NewPath(PathBuf),
}

impl PathResolution {
    /// Tries to extract the file id from self.
    pub fn as_file_id(&self) -> Option<FileId> {
        match self {
            PathResolution::Interned(id) => Some(*id),
            PathResolution::NewPath(_) => None,
        }
    }

    /// Extracts the file id from self.
    ///
    /// Panics if this does not represent an interned path
    pub fn into_file_id(self) -> FileId {
        self.as_file_id().expect("path was not interned")
    }
}
