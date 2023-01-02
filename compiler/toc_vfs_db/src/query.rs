//! Query interface for the VFS system
//!
use std::{borrow::Cow, path::Path, sync::Arc};

use camino::{Utf8Component, Utf8PathBuf};
use toc_span::FileId;

use crate::db::{FileSystem, FilesystemExt, PathIntern, VfsDatabaseExt};
use toc_vfs::{BuiltinPrefix, ErrorKind, LoadError, LoadResult, LoadStatus};

// Non query stuff //

impl<T> FilesystemExt for T
where
    T: FileSystem + PathIntern,
{
    fn resolve_path(
        &self,
        relative_to: Option<FileId>,
        path: &str,
        loader: &dyn toc_vfs::FileLoader,
    ) -> FileId {
        // Convert `path` into an absolute one
        let expanded_path = self.expand_path(path.into());

        let full_path = if expanded_path.is_absolute() {
            // Already an absolute path
            expanded_path
        } else if let Some(relative_to) = relative_to {
            // Tack on the parent path
            let mut parent_path = self.lookup_intern_path(relative_to);
            assert!(parent_path.pop(), "parent path for file was empty");

            // Join paths together, applying path de-dotting
            parent_path.join(expanded_path)
        } else {
            // The only place that I can think that needs an absolute path is
            // for looking up the predef list, which always becomes an absolute path.
            //
            // However, even then the predef list is usually accessible via the `%oot`
            // prefix, so it's likely just bad practice that's happening here
            //
            // For now, treat as-is, but may or may not be an absolute path.
            expanded_path
        };

        // Use the provided path normalizer to guarantee that we have a uniform form of path
        let full_path = loader
            .normalize_path(full_path.as_path().as_std_path())
            .map_or(full_path, |aa| Utf8PathBuf::try_from(aa).unwrap());

        self.intern_path(full_path)
    }

    fn expand_path(&self, path: &camino::Utf8Path) -> Utf8PathBuf {
        // Check if the given path has a percent prefix
        let prefix_name = if let Some(Utf8Component::Normal(comp)) = path.components().next() {
            if let Some(prefix_name) = comp.strip_prefix('%') {
                prefix_name
            } else {
                return path.to_owned();
            }
        } else {
            return path.to_owned();
        };

        match BuiltinPrefix::try_from(prefix_name) {
            Ok(prefix_path) => {
                let base_path = self.prefix_expansion(prefix_path);

                base_path.join(path.strip_prefix(prefix_path.to_string()).unwrap())
            }
            Err(_) => {
                // No corresponding path prefix, return the path as-is
                path.to_owned()
            }
        }
    }
}

impl<T> VfsDatabaseExt for T
where
    T: FileSystem + PathIntern,
{
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, source: &str) -> FileId {
        // Intern the path, then add it to the db
        let file_id = self.intern_path(Utf8PathBuf::try_from(path.as_ref().to_owned()).unwrap());
        self.set_file_source(file_id, (Arc::new(source.into()), None));
        file_id
    }

    fn insert_fixture(&mut self, fixture: toc_vfs::FixtureFiles) {
        // Collect all of the interned paths first
        let files = fixture
            .files
            .into_iter()
            .map(|(path, src)| (self.intern_path(path.try_into().unwrap()), src))
            .collect::<Vec<_>>();

        for (file, source) in files {
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
                        (
                            invalid,
                            Some(LoadError::new(
                                self.lookup_intern_path(file_id).as_std_path(),
                                ErrorKind::InvalidEncoding,
                            )),
                        )
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
