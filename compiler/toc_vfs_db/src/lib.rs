//! Interface layer for the database that abstracts over the native file system.
//!
//! Interned paths are stored in [`db::PathIntern`]'s storage.
//! File sources and built-in prefixes are stored in [`db::FileSystem`]'s storage.
//!
//! Path expansion and (relative) resolution is provided by the [`db::FilesystemExt`] trait,
//! which is required during initial include file expansion and import dependency resolution.
//!
//! However, path resolution in the compiler should use the `file_link_of` query in `toc_ast_db`,
//! since we embed a source dependency graph through that.

// FIXME: Flesh out documentation using VFS Interface.md

use camino::Utf8PathBuf;
use toc_paths::RawPath;

pub use crate::db_ext::VfsDbExt;
pub use crate::sources::{source_of, SourceFile, SourceTable, VfsBridge};

#[cfg(test)]
mod test;

#[salsa::jar(db = Db)]
pub struct Jar(sources::SourceFile, sources::source_of, resolve_path);

pub trait Db: salsa::DbWithJar<Jar> + toc_paths::Db + VfsBridge {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + toc_paths::Db + VfsBridge {}

mod db_ext {
    use std::{borrow::Cow, path::Path};

    use camino::Utf8PathBuf;
    use toc_paths::RawPath;
    use toc_vfs::{ErrorKind, FixtureFiles, LoadError, LoadResult, LoadStatus};

    use crate::VfsBridge;

    /// Helper extension trait for inserting & updating files
    pub trait VfsDbExt {
        /// Inserts a file into the database, producing a [`FileId`]
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
        DB: salsa::DbWithJar<crate::Jar> + toc_paths::Db + VfsBridge,
    {
        fn insert_file<P: AsRef<Path>>(&mut self, path: P, contents: &str) -> RawPath {
            // Intern the path, then add it to the db
            let path = RawPath::new(
                self.upcast_to_path_db(),
                path.as_ref().to_str().unwrap().into(),
            );

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
}

mod sources {
    use std::sync::Arc;

    use camino::{Utf8Path, Utf8PathBuf};
    use parking_lot::Mutex;
    use salsa::AsId;
    use toc_paths::RawPath;
    use toc_vfs::LoadError;

    use crate::Db;

    /// A map from [`RawPath`]s to [`SourceFile`]s.
    ///
    /// To play nicely with incrementality, once a mapping is added, it can't be
    /// Mappings are meant to be stable, so replacing source files is not allowed
    #[derive(Debug, Clone, Default)]
    pub struct SourceTable {
        sources: Arc<Mutex<Vec<Option<SourceFile>>>>,
    }

    impl SourceTable {
        pub fn new() -> Self {
            Self::default()
        }

        /// Looks up `path`'s [`SourceFile`]
        pub fn source(&self, path: RawPath) -> Option<SourceFile> {
            let idx = Self::to_idx(path);
            let sources = self.sources.lock();
            sources.get(idx).copied().flatten()
        }

        /// Inserts a source into the table, linking it to path
        ///
        /// ## Panics
        ///
        /// If `path` already has a mapping
        pub fn insert(&self, path: RawPath, source: SourceFile) {
            let idx = Self::to_idx(path);
            let mut sources = self.sources.lock();

            // resize truncates, so make sure that we don't go below the source length
            // add one since we want the number of elements instead of the index
            let len = sources.len();
            sources.resize((idx + 1).max(len), None);

            // Keep invariant of never changing old links
            let old = sources[idx].replace(source);
            assert_eq!(
                old, None,
                "duplicate mapping for {path:?} (tried to replace {source:?} with {old:?})"
            );
        }

        fn to_idx(path: RawPath) -> usize {
            usize::try_from(path.as_id().as_u32()).unwrap()
        }
    }

    /// Bridge from the internal virtual filesystem to the real filesystem
    pub trait VfsBridge {
        /// Mapping between [`RawPath`]s to their corresponding [`SourceFile`]
        fn source_table(&self) -> &SourceTable;

        /// Normalizes a path to its common representation.
        /// A valid implementation is to return the path as-is,
        /// but even better would be to turn it into an absolute
        /// version of the path.
        ///
        /// It's not recommended to use canonicalization, since
        /// on Windows, it can fail to canonicalize if the backing
        /// driver doesn't mount properly.
        ///
        /// Note that this assumes that the path normalization
        /// does not change, so this should be a pure map to
        /// the same form, even thought it may be incorrect.
        fn normalize_path(&self, path: &Utf8Path) -> Utf8PathBuf {
            path.to_owned()
        }

        /// Loads a file from the file system.
        /// This is invoked when a new file is discovered,
        /// and provides an opportunity to track the new file.
        ///
        /// The [`RawPath`] is provided instead of the actual path so that
        /// it can be linked to the resultant [`SourceFile`] later.
        ///
        /// An optional [`LoadError`] can be returned to report any issues
        /// encountered during the initial load.
        ///
        /// The default implementation just reports files as not being loaded
        /// yet
        #[allow(unused)]
        fn load_new_file(&self, path: RawPath) -> (String, Option<LoadError>) {
            (
                Default::default(),
                Some(LoadError::new("", toc_vfs::ErrorKind::NotLoaded)),
            )
        }
    }

    /// Source contents of a [`RawPath`].
    ///
    /// Provides an `Option<LoadError>`, to notify of things like being unable to open a file,
    /// or a file not being encoded in UTF-8
    #[salsa::input]
    pub struct SourceFile {
        /// Originating source path
        pub path: RawPath,
        #[return_ref]
        pub contents: String,
        pub errors: Option<LoadError>,
    }

    #[salsa::tracked]
    pub fn source_of(db: &dyn Db, path: RawPath) -> SourceFile {
        // Defer to the cache first
        if let Some(file) = db.source_table().source(path) {
            return file;
        }

        // New file, need to load it
        let (contents, err) = db.load_new_file(path);
        let source = SourceFile::new(db, path, contents, err);
        db.source_table().insert(path, source);

        source
    }
}

/// Resolves a path relative to a given [`RawPath`].
/// If `path` expands into an absolute path, then `relative_to` is ignored.
///
/// Performs path normalization using [`VfsBridge::normalize_path`]
#[salsa::tracked]
pub fn resolve_path(db: &dyn Db, anchor: RawPath, path: String) -> RawPath {
    // Convert `path` into an absolute one
    let path = toc_paths::expand_path(db.upcast_to_path_db(), Utf8PathBuf::from(path));

    let full_path = if path.is_absolute() {
        // Already an absolute path
        path
    } else {
        // Tack on the parent path
        let mut parent_path = anchor.raw_path(db.upcast_to_path_db()).clone();
        assert!(parent_path.pop(), "parent path for file was empty");

        // Join paths together, applying path de-dotting
        parent_path.join(path)
    };

    // Use the provided path normalizer to guarantee that we have a uniform path representation
    let full_path = db.normalize_path(&full_path);

    RawPath::new(db.upcast_to_path_db(), full_path)
}
