//! `FsBackend` Implementations

pub mod memory;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LoadError {}

/// File system backend stuff.
/// Deals with file loading and path expansion
pub trait FsBackend {
    fn normalize_path(&self, path: std::path::PathBuf) -> std::path::PathBuf;

    fn file_source(&self, file: &std::path::Path) -> Result<String, LoadError>;

    // TODO: Somehow propagate notifications from backend?
    // Change notifier gets passed in (NotifyHandle)?
}
