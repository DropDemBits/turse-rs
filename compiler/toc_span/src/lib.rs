//! Re-exports of `text_size` structs, as well as providing report location helpers.
//! Keeps a common `text_size` version between dependents.

use std::num::NonZeroU32;

pub use text_size::{TextRange, TextSize};

/// A unique file id
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FileId(NonZeroU32);

impl FileId {
    /// Constructs a new file id
    ///
    /// Must only be constructed in the context of a file db
    pub fn new(id: NonZeroU32) -> Self {
        Self(id)
    }

    /// Gets the underlying raw file id
    pub fn raw_id(&self) -> NonZeroU32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: Option<FileId>,
    pub range: TextRange,
}

impl Span {
    pub fn new(file: Option<FileId>, range: TextRange) -> Self {
        Span { file, range }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::new(None, TextRange::default())
    }
}

/// An item with an associated text span
#[derive(Debug, Clone, Copy)]
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self(item, span)
    }

    pub fn item(&self) -> &T {
        &self.0
    }

    pub fn span(&self) -> Span {
        self.1
    }
}
