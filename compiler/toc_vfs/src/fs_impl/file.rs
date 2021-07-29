//! Real filesystem backend
use std::io;
use std::path::{Path, PathBuf};

use normpath::BasePathBuf;

use crate::fs_impl::{FsBackend, LoadError};

pub struct FileBackend {}

impl FileBackend {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for FileBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl FsBackend for FileBackend {
    fn normalize_path(&self, path: PathBuf) -> PathBuf {
        let base_path = match BasePathBuf::try_new(path.clone()) {
            Ok(base_path) => base_path,
            Err(err) => return err.into_path_buf(), // Untouched
        };

        base_path
            .normalize()
            .map(BasePathBuf::into_path_buf)
            .unwrap_or(path)
    }

    fn file_source(&self, path: &Path) -> Result<String, LoadError> {
        // TODO: Handle different file encodings
        match std::fs::read_to_string(path) {
            Ok(file) => Ok(file),
            Err(err) => match err.kind() {
                io::ErrorKind::NotFound => Err(LoadError::NotFound),
                _ => Err(LoadError::Other(err.to_string())),
            },
        }
    }
}
