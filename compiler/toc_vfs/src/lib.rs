//! VFS-related data structures that don't require the database.
//!
//! This is mostly a leftover from when the path interning was stored
//! separately from the rest of datebase storage. Most crates still depend on
//! both `toc_vfs` and `toc_vfs_db`, so it's not much of a separation. However,
//! it's still nice to separate the non-db components (even though it'll likely
//! not be a useful distinction in the future).

mod fixture;

use std::{
    convert::TryFrom,
    fmt,
    path::{Path, PathBuf},
    sync::Arc,
};

pub use fixture::{generate_vfs, FixtureFiles, ParseError};

/// Built-in prefixes for paths.
///
/// The [`FileSystem::set_prefix_expansion`][set_prefix_expansion] query
/// should be used to set the corresponding path the path expands into.
///
/// [set_prefix_expansion]: toc_vfs_db::db::FileSystem::set_prefix_expansion
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
    // FIXME: remove this hack now that we can fetch paths from the db's path interner
    // basically undo https://github.com/DropDemBits/turse-rs/pull/51/commits/49cdbfd
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
