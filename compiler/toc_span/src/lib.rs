//! Re-exports of `text_size` structs, as well as providing report location helpers.
//! Keeps a common `text_size` version between dependents.

use std::{fmt, num::NonZeroU32};

pub use text_size::{TextRange, TextSize};
use toc_salsa::salsa::{InternId, InternKey};

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

    /// Constructs a new file id, for testing purposes
    pub fn new_testing(id: u32) -> Option<Self> {
        NonZeroU32::new(id).map(Self::new)
    }
}

impl InternKey for FileId {
    fn from_intern_id(v: InternId) -> Self {
        Self(NonZeroU32::new(v.as_u32() + 1).unwrap())
    }

    fn as_intern_id(&self) -> InternId {
        (self.0.get() - 1).into()
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    file: Option<FileId>,
    range: TextRange,
}

impl Span {
    pub fn new(file: FileId, range: TextRange) -> Self {
        Span {
            file: Some(file),
            range,
        }
    }

    /// Deconstructs a span into its components, optionally referring
    /// to an actual span.
    pub fn into_parts(self) -> Option<(FileId, TextRange)> {
        self.file.map(|file| (file, self.range))
    }

    /// Makes a new span covering both text ranges.
    /// If either span is a dummy span, then `None` is returned.
    ///
    /// # Panics
    /// Panics if both spans aren't in the same file.
    pub fn cover(self, other: Self) -> Option<Self> {
        let (this_file, other_file) = self.file.zip(other.file)?;
        assert_eq!(
            this_file, other_file,
            "trying to cover spans in different files"
        );

        Some(Span {
            range: self.range.cover(other.range),
            ..self
        })
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Sorted by file, then by start range
        self.file
            .cmp(&other.file)
            .then(self.range.start().cmp(&other.range.start()))
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((file, range)) = self.into_parts() {
            f.write_fmt(format_args!("({file:?}, {range:?})"))
        } else {
            f.write_str("(dummy)")
        }
    }
}
