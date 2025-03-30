//! Re-exports of `text_size` structs, as well as providing report location helpers.
//! Keeps a common `text_size` version between dependents.

use std::fmt;

pub use text_size::{TextRange, TextSize};

/// A unique file id
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FileId(toc_paths::RawPath);

// FIXME: This is so that we don't have to review 500+ snapshots
mod debug_hax {
    use super::*;

    use toc_paths::RawPath;

    impl FileId {
        pub fn into_raw(self) -> RawPath {
            self.0
        }
    }

    impl fmt::Debug for FileId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_tuple("FileId").field(&self.0).finish()
        }
    }

    impl From<toc_paths::RawPath> for FileId {
        fn from(value: toc_paths::RawPath) -> Self {
            Self(value)
        }
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
