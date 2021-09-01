//! Database queries & structures for the AST level of compilation

use std::sync::Arc;

use toc_span::FileId;

pub mod db;
pub mod span;

mod source;

/// Library source roots
#[derive(Debug, Clone)]
pub struct SourceRoots {
    roots: Arc<Vec<FileId>>,
}

impl SourceRoots {
    pub fn new(roots: Vec<FileId>) -> Self {
        Self {
            roots: Arc::new(roots),
        }
    }

    /// Iterates over the source roots, from the bottom of the dependency graph and up
    pub fn roots(&self) -> impl Iterator<Item = FileId> + '_ {
        self.roots.iter().copied()
    }
}
