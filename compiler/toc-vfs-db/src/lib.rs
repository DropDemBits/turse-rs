//! Interface layer for the database that abstracts over the native file system.
//!
//! Paths are interned into [`RawPath`](toc_paths::RawPath) entities, which are
//! in `toc-paths` so that other crates can just depend on `toc-paths` and not
//! have to depend on the entirety of `toc-vfs-db`.
//!
//! Paths that correspond to source files are represented by [`SourceFile`]
//! inputs. [`source_of()`] is used to lookup the corresponding [`SourceFile`],
//! and the mapping is ultimately backed by a [`SourceTable`] stored in the db.
//!
//! Expansion of builtin percent prefixes are handled by [`resolve_path()`],
//! which relies on [`expand_path`](toc_paths::expand_path) to do most of the
//! work. Path normalization is also performed before getting the final
//! interned path.
//!
//! The foundational trait is [`VfsBridge`], which bridges from the database's
//! internal representation of the filesystem to the real underlying filesystem.
//! It's implemented on the real top level database, and handles converting
//! source files on disk into the UTF-8 format, as well as actual path
//! normalization.
//!
//! For more details on the specific bridging between the real and virtual
//! filesystem representations, see the VFS Interface in the top-level notes
//! directory.

mod db_ext;
mod sources;

#[cfg(test)]
mod test;

use camino::Utf8PathBuf;
use toc_paths::{RawOwnedPath, RawRefPath};

pub use crate::db_ext::VfsDbExt;
pub use crate::sources::{SourceFile, SourceTable, VfsBridge, source_of};

#[salsa::db]
pub trait Db: salsa::Database + toc_paths::Db + VfsBridge {}

#[salsa::db]
impl<DB> Db for DB where DB: salsa::Database + toc_paths::Db + VfsBridge {}

/// Resolves a path relative to a given [`RawPath`](toc_paths::RawPath).
/// If `path` expands into an absolute path, then `relative_to` is ignored.
///
/// Performs path normalization using [`VfsBridge::normalize_path`]
// FIXME: Should be tracked and use RawPath
pub fn resolve_path<'db>(db: &'db dyn Db, anchor: &'db RawRefPath, path: String) -> RawOwnedPath {
    // Convert `path` into an absolute one
    let path = toc_paths::expand_path(db, Utf8PathBuf::from(path));

    let full_path = if path.is_absolute() {
        // Already an absolute path
        path
    } else {
        // Tack on the parent path
        let mut parent_path = anchor.to_owned();
        assert!(parent_path.pop(), "parent path for file was empty");

        // Join paths together, applying path de-dotting
        parent_path.join(path)
    };

    // Use the provided path normalizer to guarantee that we have a uniform path representation
    let full_path = db.normalize_path(&full_path);

    full_path
}
