//! Fake filesystem only existing in memory

use std::sync::Arc;

use crate::fs_impl::{FsBackend, LoadError};

/// Artificial file system backend, used for testing
#[derive(Debug)]
pub struct MemoryFs {
    inner: Arc<MemoryFiles>,
}

impl MemoryFs {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(MemoryFiles::new()),
        }
    }

    pub fn add_file(&mut self, path: &str, contents: &str) {
        let inner = Arc::get_mut(&mut self.inner).expect("can't add files after moving to VFS");
        inner.path_maps.insert(path.to_owned(), contents.to_owned());
    }
}

impl Default for MemoryFs {
    fn default() -> Self {
        Self::new()
    }
}

impl FsBackend for MemoryFs {
    fn normalize_path(&self, path: std::path::PathBuf) -> std::path::PathBuf {
        // No path normalization
        path
    }

    fn file_source(&self, file: &std::path::Path) -> Result<String, LoadError> {
        if let Some(source) = self.inner.path_maps.get(file.to_string_lossy().as_ref()) {
            Ok(source.clone())
        } else {
            Err(LoadError::NotFound)
        }
    }
}

#[derive(Debug)]
struct MemoryFiles {
    path_maps: indexmap::IndexMap<String, String>,
}

impl MemoryFiles {
    fn new() -> Self {
        Self {
            path_maps: indexmap::IndexMap::new(),
        }
    }
}
