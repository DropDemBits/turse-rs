//! Re-exports of `text_size` structs, as well as providing report location helpers.
//! Keeps a common `text_size` version between dependents.

use std::{fmt, num::NonZeroU32};

use indexmap::IndexSet;
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

/// An item with an associated text span
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T>(T, SpanId);

impl<T> Spanned<T> {
    pub fn new(item: T, span: SpanId) -> Self {
        Self(item, span)
    }

    pub fn item(&self) -> &T {
        &self.0
    }

    pub fn span(&self) -> SpanId {
        self.1
    }
}

/// A handle to an interned [`Span`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct SpanId(NonZeroU32);

impl SpanId {
    /// Looks up the span in the given span table
    ///
    /// Infix/postfix version of using the span table
    pub fn lookup_in(self, span_map: impl HasSpanTable) -> Span {
        span_map.span_table().lookup_span(self)
    }
}

/// An interner for interning [`Span`]s.
///
/// Produces [`SpanId`]s
#[derive(Debug, PartialEq, Eq)]
pub struct SpanTable {
    spans: IndexSet<Span>,
    dummy_span: SpanId,
}

impl Default for SpanTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SpanTable {
    pub fn new() -> Self {
        let mut spans = IndexSet::default();
        // intern it
        let index = spans.insert_full(Default::default()).0;
        let id = NonZeroU32::new(index.wrapping_add(1) as u32).expect("too many ids");
        let dummy_span = SpanId(id);

        Self { spans, dummy_span }
    }

    /// Interns the given span
    pub fn intern_span(&mut self, span: Span) -> SpanId {
        let index = self.spans.insert_full(span).0;
        let id = NonZeroU32::new(index.wrapping_add(1) as u32).expect("too many ids");
        SpanId(id)
    }

    /// Looks up the span
    pub fn lookup_span(&self, id: SpanId) -> Span {
        // Ids are only constructed here
        let index = id.0.get().wrapping_sub(1);
        *self.spans.get_index(index as usize).unwrap()
    }

    /// Makes up a dummy span
    pub fn dummy_span(&self) -> SpanId {
        self.dummy_span
    }
}

/// Anything which has a span table
pub trait HasSpanTable {
    fn span_table(&self) -> &SpanTable;
}

impl HasSpanTable for SpanTable {
    fn span_table(&self) -> &SpanTable {
        self
    }
}

impl<T> HasSpanTable for &T
where
    T: HasSpanTable,
{
    fn span_table(&self) -> &SpanTable {
        T::span_table(self)
    }
}

impl<T> HasSpanTable for &mut T
where
    T: HasSpanTable,
{
    fn span_table(&self) -> &SpanTable {
        T::span_table(self)
    }
}

impl<T> HasSpanTable for std::sync::Arc<T>
where
    T: HasSpanTable,
{
    fn span_table(&self) -> &SpanTable {
        T::span_table(self)
    }
}
