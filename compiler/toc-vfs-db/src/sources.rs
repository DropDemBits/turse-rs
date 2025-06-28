//! Individual source files, as well as loading new ones

use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use toc_paths::RawOwnedPath;
use toc_vfs::LoadError;

use crate::Db;

/// A map from [`RawOwnedPath`]s to [`SourceFile`]s.
///
/// To play nicely with incrementality, once a mapping is added, it can't be
/// Mappings are meant to be stable, so replacing source files is not allowed
#[derive(Debug, Clone, Default)]
pub struct SourceTable {
    sources: Arc<Mutex<FxHashMap<RawOwnedPath, SourceFile>>>,
}

impl SourceTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Looks up `path`'s [`SourceFile`]
    pub fn source(&self, path: &Utf8Path) -> Option<SourceFile> {
        let sources = self.sources.lock();
        sources.get(path).copied()
    }

    /// Inserts a source into the table, linking it to path
    ///
    /// ## Panics
    ///
    /// If `path` already has a mapping
    pub fn insert(&self, path: &Utf8Path, source: SourceFile) {
        let mut sources = self.sources.lock();

        // Keep invariant of never changing old links
        let old = sources.insert(path.to_owned(), source);
        assert_eq!(
            old, None,
            "duplicate mapping for {path:?} (tried to replace {source:?} with {old:?})"
        );
    }
}

/// Bridge from the internal virtual filesystem to the real filesystem
pub trait VfsBridge: salsa::Database {
    /// Mapping between [`RawOwnedPath`]s to their corresponding [`SourceFile`]
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

    /// Loads a source file from the specified path.
    /// This is invoked every time the source text of a file is requested.
    fn load_source_file(&self, path: &Utf8Path) -> SourceFile {
        // Defer to the cache first
        if let Some(file) = self.source_table().source(path) {
            return file;
        }

        // New file, need to load it
        let (contents, err) = self.load_new_file(path);
        let source = SourceFile::new(self, path.to_owned(), contents, err);
        self.source_table().insert(path, source);

        source
    }

    /// Loads a file from the file system.
    /// This is invoked when a new file is discovered,
    /// and provides an opportunity to track the new file.
    ///
    /// An optional [`LoadError`] can be returned to report any issues
    /// encountered during the initial load.
    ///
    /// The default implementation just reports files as not being loaded
    /// yet
    #[allow(unused)]
    fn load_new_file(&self, path: &Utf8Path) -> (String, Option<LoadError>) {
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
#[salsa::input(debug)]
pub struct SourceFile {
    /// Originating source path
    #[return_ref]
    pub path: RawOwnedPath,
    #[return_ref]
    pub contents: String,
    pub errors: Option<LoadError>,
}

pub fn source_of(db: &dyn Db, path: &Utf8Path) -> SourceFile {
    db.load_source_file(path)
}
