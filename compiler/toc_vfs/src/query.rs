//! Query interface for the VFS system

use std::borrow::Cow;
use std::sync::Arc;

use toc_salsa::salsa;
use toc_span::FileId;

use crate::db::{FileSystem, VfsDatabaseExt};
use crate::vfs::HasVfs;
use crate::LoadError;

pub(crate) fn resolve_path(db: &dyn FileSystem, relative_to: FileId, path: &str) -> FileId {
    db.get_vfs()
        .resolve_path(Some(relative_to), path)
        .into_file_id()
        .expect("path was not interned yet")
}

// Non query stuff //

impl<T> VfsDatabaseExt for T
where
    T: HasVfs + FileSystem + salsa::Database,
{
    fn invalidate_files(&mut self) {
        let sources = self.get_vfs().file_store.clone();

        for (file_id, source) in sources {
            self.update_file(file_id, source)
        }
    }

    fn update_file(&mut self, file_id: FileId, new_source: Option<Vec<u8>>) {
        let result = if let Some(byte_source) = new_source {
            // FIXME: Deal with different character encodings (per `VFS Interface.md`)
            // This is likely the place where we'd do it

            // Try converting the file into UTF-8
            let source = String::from_utf8_lossy(&byte_source);

            match source {
                Cow::Borrowed(_source) => {
                    // Steal memory from the cloning process
                    (String::from_utf8(byte_source).unwrap(), None)
                }
                Cow::Owned(invalid) => {
                    // Non UTF-8 encoded characters
                    (invalid, Some(LoadError::InvalidEncoding))
                }
            }
        } else {
            // File does not exist, or was removed
            (String::new(), Some(LoadError::NotFound))
        };

        self.set_file_source(file_id, Arc::new(result));
    }
}
