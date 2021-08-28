//! Re-exports of `text_size` structs, as well as providing report location helpers.
//! Keeps a common `text_size` version between dependents.

use std::num::NonZeroU32;

use indexmap::IndexSet;
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

/// An interner for interning [`Span`]s.
///
/// Produces [`SpanId`]s
#[derive(Debug)]
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
