//! Re-exports of `text_size` structs.
//! Keeps a common `text_size` version between dependents.

pub use text_size::{TextRange, TextSize};

/// An item with an associated text span
#[derive(Debug, Clone, Copy)]
pub struct Spanned<T>(T, TextRange);

impl<T> Spanned<T> {
    pub fn new(item: T, span: TextRange) -> Self {
        Self(item, span)
    }

    pub fn item(&self) -> &T {
        &self.0
    }

    pub fn span(&self) -> TextRange {
        self.1
    }
}
