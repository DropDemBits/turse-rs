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

pub mod db;
mod query;

#[cfg(test)]
mod test;
