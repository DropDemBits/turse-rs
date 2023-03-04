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
mod db_ext;
mod sources;

#[cfg(test)]
mod test;

use camino::Utf8PathBuf;
use toc_paths::RawPath;

pub use crate::db_ext::VfsDbExt;
pub use crate::sources::{source_of, SourceFile, SourceTable, VfsBridge};

#[salsa::jar(db = Db)]
pub struct Jar(sources::SourceFile, sources::source_of, resolve_path);

pub trait Db: salsa::DbWithJar<Jar> + toc_paths::Db + VfsBridge {
    fn upcast_to_vfs_db(&self) -> &dyn Db;
}

impl<DB> Db for DB
where
    DB: salsa::DbWithJar<Jar> + toc_paths::Db + VfsBridge,
{
    fn upcast_to_vfs_db(&self) -> &dyn Db {
        self
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
