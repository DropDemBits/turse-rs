//! Query interface for the VFS
//!
//! [`FileSystem::file_source`] is used to access file sources used by the compiler
//!
//! ## Note
//!
//! All paths must be encountered before executing any queries, since no paths are interned from queries
//!
//! [`FileSystem::file_source`]: crate::db::FileSystem::file_source

pub mod db;
mod query;

#[cfg(test)]
mod test;
