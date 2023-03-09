//! Helpers for using the VFS during testing

use std::{borrow::Cow, path::Path};

use camino::Utf8PathBuf;
use toc_paths::RawPath;
use toc_vfs::{ErrorKind, FixtureFiles, LoadError, LoadResult, LoadStatus};
use upcast::Upcast;

use crate::VfsBridge;

/// Helper extension trait for inserting & updating files
pub trait VfsDbExt {
    /// Inserts a file into the database, producing a [`RawPath`]
    ///
    /// Mainly used in tests
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, source: &str) -> RawPath;

    /// Inserts a generated fixture tree into the database
    fn insert_fixture(&mut self, fixture: FixtureFiles);

    /// Updates the contents of the specified file, using the given load result
    fn update_file(&mut self, file_id: RawPath, result: LoadResult);
}

impl<DB> VfsDbExt for DB
where
    DB: salsa::DbWithJar<crate::Jar> + toc_paths::Db + Upcast<dyn toc_paths::Db> + VfsBridge,
{
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, contents: &str) -> RawPath {
        // Intern the path, then add it to the db
        let path = RawPath::new(self, path.as_ref().to_str().unwrap().into());

        let source = crate::source_of(self, path);
        source.set_contents(self).to(contents.into());
        source.set_errors(self).to(None);
        path
    }

    fn insert_fixture(&mut self, fixture: toc_vfs::FixtureFiles) {
        // Collect all of the interned paths first
        let files = fixture
            .files
            .into_iter()
            .map(|(path, src)| {
                (
                    RawPath::new(self, Utf8PathBuf::try_from(path).unwrap()),
                    src,
                )
            })
            .collect::<Vec<_>>();

        for (file, source) in files {
            self.update_file(file, source);
        }
    }

    fn update_file(&mut self, path: RawPath, result: LoadResult) {
        let (contents, errors) = match result {
            Ok(LoadStatus::Unchanged) => return,
            Ok(LoadStatus::Modified(byte_source)) => {
                // FIXME: Deal with different character encodings (per `VFS Interface.md`)
                // This is likely the place where we'd do it

                // ???: Actually, this might not be the place where we do char transcoding
                // Would likely make more sense externally, so that we wouldn't have to
                // deal with it when we don't need it (e.g. if we're in a web context)
                // where we can safely assume that all files are UTF-16

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
                            Some(LoadError::new("", ErrorKind::InvalidEncoding)),
                        )
                    }
                }
            }
            Err(err) => {
                // Error encountered during loading
                (String::new(), Some(err))
            }
        };

        let source = crate::source_of(self, path);
        source.set_contents(self).to(contents);
        source.set_errors(self).to(errors);
    }
}
