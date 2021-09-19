//! VFS query system definitions

use std::path::Path;
use std::sync::Arc;

use toc_salsa::salsa;
use toc_span::FileId;

use crate::vfs::HasVfs;
use crate::{LoadError, LoadResult};

/// Query interface into the virtual file system, backed by [`HasVfs`].
#[salsa::query_group(FileSystemStorage)]
pub trait FileSystem: HasVfs {
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
///
/// [`Vfs`]: crate::Vfs
pub trait VfsDatabaseExt: HasVfs + FileSystem {
    /// Inserts a file into the database, producing a [`FileId`]
    ///
    /// Mainly used in tests
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, source: &str) -> FileId;

    /// Updates the contents of the specified file, using the given load result
    fn update_file(&mut self, file_id: FileId, result: LoadResult);
}
