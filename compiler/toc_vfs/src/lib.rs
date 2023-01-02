//! Abstraction over interfacing the native filesystem
//!
//! Path expansion operations, path interning, and intermediate file storage
//! are held in the concrete [`Vfs`] type.
//!
//! Resolving paths into [`FileId`]s via the [`Vfs::resolve_path`]
//! method is required during lowering the concrete syntax tree into HIR,
//! specifically during:
//!
//! - Include file expansion in order to get the file id for getting the parse tree
//! - Import dependency resolution for generating dependencies between units
//!
//! However, path resolution in the actual compilation process should use the
//! `depend_of` query in `toc_ast_db`,since we cache a source dependency graph in
//! there.
//!
//! [`Vfs`]: crate::Vfs
//! [`Vfs::resolve_path`]: crate::Vfs::resolve_path
//! [`FileId`]: toc_span::FileId

// TODO: Flesh out documentation using VFS Interface.md

mod fixture;
mod intern;
mod vfs;

use std::{
    convert::TryFrom,
    fmt,
    path::{Path, PathBuf},
    sync::Arc,
};

pub use fixture::{generate_vfs, FixtureFiles, ParseError};
pub use intern::PathResolution;
pub use vfs::Vfs;

/// Built-in prefixes for paths.
///
/// The [`Vfs::set_prefix_expansion`][set_prefix_expansion] method
/// should be used to set the corresponding path the path expands into.
///
/// [set_prefix_expansion]: crate::Vfs::set_prefix_expansion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinPrefix {
    /// %oot, should point to the Turing home directory
    /// (the directory containing "support" from an (Open)Turing installation).
    Oot,
    /// `%help`, should point to the Turing help directory
    /// (normally set as "%oot/support/help").
    Help,
    /// `%user`, should point to the current user's home directory
    /// (normally "$HOME" on Linux, or "C:/Users/%USER%" on Windows).
    UserHome,
    /// `%job`, can be specified by the user
    /// (set to a user provided path, or the temp directory root otherwise).
    Job,
    /// `%tmp`, should point to the temp directory
    /// (normally should be the path returned from [`env::temp_dir`][temp_dir])
    ///
    /// [temp_dir]: std::env::temp_dir
    Temp,
}

impl TryFrom<&str> for BuiltinPrefix {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "oot" => Self::Oot,
            "help" => Self::Help,
            "home" => Self::UserHome,
            "job" => Self::Job,
            "tmp" => Self::Temp,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for BuiltinPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BuiltinPrefix::Oot => "%oot",
            BuiltinPrefix::Help => "%help",
            BuiltinPrefix::UserHome => "%home",
            BuiltinPrefix::Job => "%job",
            BuiltinPrefix::Temp => "%tmp",
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoadError {
    path: Arc<String>,
    kind: ErrorKind,
}

impl LoadError {
    pub fn new(path: impl AsRef<Path>, kind: ErrorKind) -> Self {
        Self {
            path: Arc::new(path.as_ref().display().to_string()),
            kind,
        }
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { path, kind } = self;
        write!(f, "unable to load source for `{path}`: {kind}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    /// File was not found
    NotFound,
    /// File is in an unsupported encoding
    InvalidEncoding,
    /// Other loading error
    Other(Arc<String>),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotFound => f.write_str("File not found"),
            Self::InvalidEncoding => f.write_str("Invalid file encoding"),
            Self::Other(err) => f.write_str(err),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LoadStatus {
    Unchanged,
    Modified(Vec<u8>),
}

pub type LoadResult = Result<LoadStatus, LoadError>;

/// [`FileLoader`] is used for loading files that are not tracked by
/// the database yet.
pub trait FileLoader {
    /// Loads the file at the given path
    fn load_file(&self, path: &Path) -> LoadResult;

    /// Normalizes the given path into a common representation
    fn normalize_path(&self, path: &Path) -> Option<PathBuf>;
}

/// Dummy file loader that effectively performs a no-op
///
/// Must ensure that all files are already loaded into the database,
/// and that all paths passed are in normalized format. This can be
/// done via [`generate_vfs`] for tests.
pub struct DummyFileLoader;

impl FileLoader for DummyFileLoader {
    fn load_file(&self, _path: &Path) -> LoadResult {
        Ok(LoadStatus::Unchanged)
    }

    fn normalize_path(&self, _path: &Path) -> Option<PathBuf> {
        None
    }
}
