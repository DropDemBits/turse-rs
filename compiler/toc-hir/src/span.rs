//! Span interning in HIR

use std::num::NonZeroU32;

use indexmap::IndexSet;
use toc_span::Span;

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
