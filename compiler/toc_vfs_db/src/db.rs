//! VFS query system definitions

use std::{path::Path, sync::Arc};

use camino::{Utf8Path, Utf8PathBuf};
use toc_salsa::salsa;
use toc_span::FileId;
use toc_vfs::{BuiltinPrefix, FileLoader, FixtureFiles, LoadError, LoadResult};

/// Query interface into the virtual file system.
#[salsa::query_group(FileSystemStorage)]
pub trait FileSystem {
    /// Gets the file source of a text.
    ///
    /// Provides an `Option<LoadError>`, to notify of things like being unable to open a file,
    /// or a file not being encoded in UTF-8
    ///
    /// # Returns
    /// An owned file source, as well as an error message to be passed to a message sink
    #[salsa::input]
    fn file_source(&self, file: FileId) -> (Arc<String>, Option<LoadError>);

    /// Gets the corresponding path for a builtin path prefix.
    #[salsa::input]
    fn prefix_expansion(&self, prefix: BuiltinPrefix) -> Utf8PathBuf;
}

/// Path interning for the virtual file system
#[salsa::query_group(PathInternStorage)]
pub trait PathIntern {
    /// Interns a path into the database
    #[salsa::interned]
    fn intern_path(&self, path: Utf8PathBuf) -> FileId;
}

/// Interface over the base [`FileSystem`] and [`PathIntern`] traits
pub trait FilesystemExt: PathIntern + FileSystem {
    /// Resolves a path relative to a given file.
    ///
    /// If `path` expands into an absolute path, then `relative_to` is ignored.
    ///
    /// If `relative_to` is [`None`], then the expanded path will be treated as an absolute one.
    fn resolve_path(
        &self,
        relative_to: Option<FileId>,
        path: &str,
        loader: &dyn FileLoader,
    ) -> FileId;

    /// Expands a path, dealing with any percent prefixes
    fn expand_path(&self, path: &Utf8Path) -> Utf8PathBuf;
}

/// Helper extension trait for inserting & updating files
pub trait VfsDatabaseExt {
    /// Inserts a file into the database, producing a [`FileId`]
    ///
    /// Mainly used in tests
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, source: &str) -> FileId;

    /// Inserts a generated fixture tree into the database
    fn insert_fixture(&mut self, fixture: FixtureFiles);

    /// Updates the contents of the specified file, using the given load result
    fn update_file(&mut self, file_id: FileId, result: LoadResult);
}
