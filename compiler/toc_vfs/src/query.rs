//! Query interface for the VFS system

use std::convert::TryFrom;
use std::num::NonZeroU32;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use salsa::Database;
use toc_span::FileId;

use crate::fs_impl::{FsBackend, LoadError};
use crate::BuiltinPrefix;

/// Wrapper type for interning `FileId`s in the salsa database.
///
/// Internal use only.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InternedFileId(FileId);

impl salsa::InternKey for InternedFileId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(FileId::new(NonZeroU32::new(v.as_u32() + 1).unwrap()))
    }

    fn as_intern_id(&self) -> salsa::InternId {
        salsa::InternId::from(self.0.raw_id().get() - 1)
    }
}

/// Query interface into the virtual file system, backed by `toc_vfs::HasVFS`.
#[salsa::query_group(FileSystemStorage)]
pub trait FileSystem: Database + HasVFS + PathInterner {
    /// Path to the root file that serves as the root of all compilation.
    #[salsa::input]
    fn root_file_path(&self) -> PathBuf;

    /// The path to replace for the corresponding `prefix`.
    #[salsa::input]
    fn prefix_expansion(&self, prefix: BuiltinPrefix) -> PathBuf;

    /// The corresponding `FileId` for the root file.
    fn root_file(&self) -> FileId;

    /// Resolves the given `relative_path` (with `base_path` as the file to have lookups relative to)
    /// into a FileId.
    ///
    /// Results are not cached since results can change on a whim.
    ///
    /// # Returns
    /// A `FileId` corresponding to the final resolved path.
    #[salsa::transparent]
    fn resolve_path(&self, base_path: FileId, relative_path: &str) -> FileId;

    /// Gets the file source of a text.
    /// In the event of an error while loading the file, the file source will be an empty string,
    /// and the relevant error info will be provided.
    ///
    /// # Returns
    /// An owned file path, as well as an error message to be passed to a message sink
    fn file_source(&self, file: FileId) -> Arc<(String, Option<LoadError>)>;
}

/// Interner for paths used by `FileSystem` queries.
///
/// Internal use only.
#[salsa::query_group(PathInternStorage)]
pub trait PathInterner {
    #[salsa::interned]
    fn intern_path(&self, path: PathBuf) -> InternedFileId;
}

fn root_file(db: &dyn FileSystem) -> FileId {
    let normalized_path = db.get_vfs().normalize_path(db.root_file_path());
    db.intern_path(normalized_path).0
}

fn resolve_path(db: &dyn FileSystem, relative_to: FileId, relative_path: &str) -> FileId {
    let vfs = db.get_vfs();

    // 3 different paths:
    // - percent prefix (always absolute, will handle later)
    // - relative to current directory
    // - absolute
    //   - relative to current drive (windows only)
    //   - absolute to all fs items

    let expanded_path = expand_path(db, relative_path);

    let full_path = if expanded_path.is_absolute() {
        // No adjusting to be done
        expanded_path
    } else {
        // Tack on the directory of the base file
        let mut parent_path: PathBuf = db.lookup_intern_path(InternedFileId(relative_to));
        parent_path.pop();
        parent_path.join(expanded_path)
    };

    let full_path = vfs.normalize_path(full_path);
    db.intern_path(full_path).0
}

fn file_source(db: &dyn FileSystem, file: FileId) -> Arc<(String, Option<LoadError>)> {
    db.salsa_runtime()
        .report_synthetic_read(salsa::Durability::LOW);

    let path = db.lookup_intern_path(InternedFileId(file));
    let result = match db.get_vfs().file_source(&path) {
        Ok(source) => (source, None),
        Err(err) => (String::new(), Some(err)),
    };
    Arc::new(result)
}

fn expand_path(db: &dyn FileSystem, path: impl AsRef<Path>) -> PathBuf {
    fn inner(db: &dyn FileSystem, path: &Path) -> PathBuf {
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
                let base_path = db.prefix_expansion(prefix_path);
                base_path.join(path.strip_prefix(prefix_path.to_string()).unwrap())
            }
            Err(_) => {
                // No corresponding path prefix, return the path as-is
                path.to_owned()
            }
        }
    }

    inner(db, path.as_ref())
}

/// Trait providing the query system access to the virtual file system.
pub trait HasVFS {
    /// Get access to the underlying VFS (filesystem backend).
    fn get_vfs(&self) -> &dyn FsBackend;
}
