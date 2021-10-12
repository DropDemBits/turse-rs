//! Query interface for the VFS system

use std::borrow::Cow;
use std::path::Path;
use std::sync::Arc;

use toc_salsa::salsa;
use toc_span::FileId;

use crate::db::{FileSystem, VfsDatabaseExt};
use toc_vfs::{HasVfs, LoadError, LoadResult, LoadStatus};

// Non query stuff //

impl<T> VfsDatabaseExt for T
where
    T: HasVfs + FileSystem + salsa::Database,
{
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, source: &str) -> FileId {
        // Intern the path, then add it to the db
        let file_id = self.get_vfs_mut().intern_path(path.as_ref().into());
        self.set_file_source(file_id, (Arc::new(source.into()), None));
        file_id
    }

    fn insert_fixture(&mut self, fixture: toc_vfs::FixtureFiles) {
        for (file, source) in fixture.files {
            self.update_file(file, source);
        }
    }

    fn update_file(&mut self, file_id: FileId, result: LoadResult) {
        let result = match result {
            Ok(LoadStatus::Unchanged) => return,
            Ok(LoadStatus::Modified(byte_source)) => {
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
            }
            Err(err) => {
                // Error encountered during loading
                (String::new(), Some(err))
            }
        };
        let result = (Arc::new(result.0), result.1);

        self.set_file_source(file_id, result);
    }
}