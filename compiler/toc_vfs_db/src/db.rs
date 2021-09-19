//! VFS query system definitions

use std::path::Path;
use std::sync::Arc;

use toc_salsa::salsa;
use toc_span::FileId;
use toc_vfs::{FixtureFiles, LoadError, LoadResult};

/// Query interface into the virtual file system, backed by [`HasVfs`].
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
}

/// Helper extension trait for databases with [`Vfs`]'s
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
