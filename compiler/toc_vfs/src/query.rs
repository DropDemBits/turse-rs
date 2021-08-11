//! Query interface for the VFS system

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::num::NonZeroU32;
use std::ops::Index;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use salsa::Database;
use toc_span::FileId;

use crate::{BuiltinPrefix, LoadError};

/// Query interface into the virtual file system, backed by [`HasVfs`].
#[salsa::query_group(FileSystemStorage)]
pub trait FileSystem: HasVfs {
    /// Resolves the given `relative_path` (with `base_path` as the file to have lookups relative to)
    /// into a FileId.
    ///
    /// Results are not cached since results can change on a whim.
    ///
    /// # Returns
    /// A `FileId` corresponding to the final resolved path.
    //
    // Note: This should not be the entry point for where paths could be interned
    //
    // ???: How does this work where we have VFS results being submitted
    #[salsa::transparent]
    fn resolve_path(&self, base_path: FileId, relative_path: &str) -> FileId;

    /// Gets the file source of a text.
    ///
    /// Provides an `Option<LoadError>`, to notify of things like being unable to open a file,
    /// or a file not being encoded in UTF-8
    ///
    /// # Returns
    /// An owned file source, as well as an error message to be passed to a message sink
    #[salsa::input]
    fn file_source(&self, file: FileId) -> Arc<(String, Option<LoadError>)>;
}

fn resolve_path(db: &dyn FileSystem, relative_to: FileId, path: &str) -> FileId {
    db.get_vfs()
        .resolve_path(Some(relative_to), path)
        .into_file_id()
        .expect("path was not interned yet")
}

// Non query stuff //

/// Trait providing the query system access to the virtual file system
pub trait HasVfs: Database {
    // Get access to the underlying virtual file system
    fn get_vfs(&self) -> &Vfs;
}

/// Helper extension trait for databases with [`Vfs`]'s
pub trait VfsDatabaseExt: HasVfs + FileSystem {
    /// Invalidates all files, uploading every single source into the database
    /// regardless of if it has changed or not.
    fn invalidate_files(&mut self);

    /// Updates the contents of the specified file.
    ///
    /// Specifying [`None`] as the `new_source` is equivalent to removing a file,
    /// or indicating that it does not exist.
    fn update_file(&mut self, file_id: FileId, new_source: Option<Vec<u8>>);
}

impl<T> VfsDatabaseExt for T
where
    T: HasVfs + FileSystem,
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

/// Interner for paths.
#[derive(Debug, Default)]
pub struct PathInterner {
    paths: indexmap::IndexSet<PathBuf>,
}

impl PathInterner {
    pub fn new() -> Self {
        Self::default()
    }

    /// Interns the given path into the corresponding [`FileId`]
    ///
    /// ## Panics
    ///
    /// Panics if there are too many paths that are interned
    ///
    /// ## Returns
    ///
    /// The corresponding [`FileId`] for the path
    pub fn intern_path(&mut self, path: impl AsRef<Path>) -> FileId {
        let (id, _exists) = self.paths.insert_full(path.as_ref().to_path_buf());

        Self::mk_file_id(id)
    }

    /// Looks up the path corresponding to the given [`FileId`]
    pub fn lookup_path(&self, file_id: FileId) -> &std::path::Path {
        self.paths.index(Self::as_index(file_id)).as_path()
    }

    /// Looks up the [`FileId`] corresponding to the given path.
    pub fn lookup_id(&self, path: &Path) -> Option<FileId> {
        self.paths.get_index_of(path).map(Self::mk_file_id)
    }

    fn mk_file_id(index: usize) -> FileId {
        FileId::new(NonZeroU32::new((index + 1) as u32).unwrap())
    }

    fn as_index(file_id: FileId) -> usize {
        file_id.raw_id().get() as usize - 1
    }
}

/// Concrete virtual filesystem interface
///
/// File sources are loaded into the main `Vfs` type, and file sources in
/// the form of raw binary blobs can come from anywhere.
/// For example, they can be generated from diffs provided from a
/// language client, or they can be loaded from the filesystem directly
/// through [`std::fs::read`].
#[derive(Debug, Default)]
pub struct Vfs {
    path_interner: PathInterner,
    builtin_expansions: HashMap<BuiltinPrefix, PathBuf>,
    file_store: HashMap<FileId, Option<Vec<u8>>>,
}

impl Vfs {
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolves a path relative to a given file.
    ///
    /// If `path` expands into an absolute path, then `relative_to` is ignored
    ///
    /// If `relative_to` is [`None`], then the expanded path will be treated as an absolute one.
    pub fn resolve_path(&self, relative_to: Option<FileId>, path: &str) -> PathResolution {
        // Convert `path` into an absolute one
        let expanded_path = self.expand_path(path);

        let full_path = if expanded_path.is_absolute() {
            // Already an absolute path
            expanded_path
        } else if let Some(relative_to) = relative_to {
            // Tack on the parent path
            let mut parent_path = self.path_interner.lookup_path(relative_to).to_owned();
            assert!(parent_path.pop(), "parent path for file was empty");
            parent_path.join(expanded_path)
        } else {
            // The only place that I can think that needs an absolute path is
            // for looking up the predef list, which always becomes an absolute path.
            //
            // For now, treat as-is, but may or may not be an absolute path.
            expanded_path
        };

        // FIXME: Apply path normalization
        // Use a provided path normalizer to guarantee that we have a uniform form of path

        if let Some(id) = self.path_interner.lookup_id(full_path.as_path()) {
            PathResolution::Interned(id)
        } else {
            PathResolution::NewPath(full_path)
        }
    }

    /// Expands a path, dealing with any percent prefixes
    pub fn expand_path(&self, path: impl AsRef<Path>) -> PathBuf {
        let path = path.as_ref();

        // Check if the given path has a percent prefix
        let prefix_name = if_chain::if_chain! {
            if let Some(Component::Normal(comp)) = path.components().next();
            // If the given path component is not a valid unicode string, then it's safe to bail out
            // None of the builtin percent prefixes contain any unicode characters.
            if let Some(comp) = comp.to_str();
            if let Some(prefix_name) = comp.strip_prefix('%');
            then {
                prefix_name
            }
            else {
                return path.to_owned();
            }
        };

        match BuiltinPrefix::try_from(prefix_name) {
            Ok(prefix_path) => {
                let base_path = self
                    .builtin_expansions
                    .get(&prefix_path)
                    .expect("missing path prefix");
                base_path.join(path.strip_prefix(prefix_path.to_string()).unwrap())
            }
            Err(_) => {
                // No corresponding path prefix, return the path as-is
                path.to_owned()
            }
        }
    }

    /// Sets the path to expand to for a given prefix
    pub fn set_prefix_expansion(&mut self, prefix: BuiltinPrefix, expansion: impl AsRef<Path>) {
        self.builtin_expansions
            .insert(prefix, expansion.as_ref().to_path_buf());
    }

    /// Interns the given path into the corresponding [`FileId`]
    ///
    /// ## Panics
    ///
    /// Panics if there are too many paths that are interned
    ///
    /// ## Returns
    ///
    /// The corresponding [`FileId`] for the path
    pub fn intern_path(&mut self, path: PathBuf) -> FileId {
        self.path_interner.intern_path(path)
    }

    /// Looks up the path corresponding to the given [`FileId`]
    pub fn lookup_path(&self, file_id: FileId) -> &std::path::Path {
        self.path_interner.lookup_path(file_id)
    }

    /// Inserts a file, producing a file id
    ///
    /// Mainly used in tests.
    pub fn insert_file(&mut self, path: impl AsRef<Path>, source: &str) -> FileId {
        let id = self.intern_path(path.as_ref().to_path_buf());
        self.file_store
            .insert(id, Some(source.to_string().into_bytes()));
        id
    }

    /// Replaces the file source of the given `file_id` with the `new_source`.
    ///
    /// [`None`] is used to indicate that a file does not exist.
    pub fn update_file(&mut self, file_id: FileId, new_source: Option<Vec<u8>>) {
        self.file_store.insert(file_id, new_source);
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
    /// Extracts the file id from self.
    pub fn into_file_id(self) -> Option<FileId> {
        match self {
            PathResolution::Interned(id) => Some(id),
            PathResolution::NewPath(_) => None,
        }
    }
}
