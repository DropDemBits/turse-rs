//! `FsBackend` Implementations

pub mod memory;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LoadError {}

/// File system backend.
/// Deals with file loading and path normalization.
pub trait FsBackend {
    /// Transforms the given path into normalized form.
    fn normalize_path(&self, path: std::path::PathBuf) -> std::path::PathBuf;

    /// Acquires the file source of the file at the given path.
    fn file_source(&self, file: &std::path::Path) -> Result<String, LoadError>;

    // TODO: Somehow propagate notifications from backend?
    // Change notifier gets passed in (NotifyHandle)?
}
