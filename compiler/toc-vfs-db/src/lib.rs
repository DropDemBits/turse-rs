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
use toc_paths::RawPath;
use upcast::{Upcast, UpcastFrom};

pub use crate::db_ext::VfsDbExt;
pub use crate::sources::{SourceFile, SourceTable, VfsBridge, source_of};

#[salsa::jar(db = Db)]
pub struct Jar(sources::SourceFile, sources::source_of, resolve_path);

pub trait Db:
    salsa::DbWithJar<Jar> + toc_paths::Db + Upcast<dyn toc_paths::Db> + VfsBridge
{
}

impl<DB> Db for DB where
    DB: salsa::DbWithJar<Jar> + toc_paths::Db + Upcast<dyn toc_paths::Db> + VfsBridge
{
}

impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}

/// Resolves a path relative to a given [`RawPath`].
/// If `path` expands into an absolute path, then `relative_to` is ignored.
///
/// Performs path normalization using [`VfsBridge::normalize_path`]
#[salsa::tracked]
pub fn resolve_path(db: &dyn Db, anchor: RawPath, path: String) -> RawPath {
    // Convert `path` into an absolute one
    let path = toc_paths::expand_path(db.up(), Utf8PathBuf::from(path));

    let full_path = if path.is_absolute() {
        // Already an absolute path
        path
    } else {
        // Tack on the parent path
        let mut parent_path = anchor.raw_path(db.up()).clone();
        assert!(parent_path.pop(), "parent path for file was empty");

        // Join paths together, applying path de-dotting
        parent_path.join(path)
    };

    // Use the provided path normalizer to guarantee that we have a uniform path representation
    let full_path = db.normalize_path(&full_path);

    RawPath::new(db.up(), full_path)
}
