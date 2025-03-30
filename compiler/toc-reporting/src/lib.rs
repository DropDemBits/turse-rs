//! Common message reporting for all compiler crates
use std::{fmt::Debug, sync::Arc};

macro_rules! impl_with_display_locations {
    ($ty:ident) => {
        impl<L: crate::Location> crate::display::WithDisplayLocations<L> for $ty<L> {}

        impl std::fmt::Display for $ty<toc_span::Span> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use crate::display::WithDisplayLocations as _;
                use std::fmt::Display;

                Display::fmt(&self.display_spans(crate::display::SpanDisplay), f)
            }
        }

        impl std::fmt::Display for $ty<crate::FileRange> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use crate::display::WithDisplayLocations as _;
                use std::fmt::Display;

                Display::fmt(&self.display_spans(crate::display::FileRangeDisplay), f)
            }
        }
    };
}

mod annotate;
mod display;
mod message;
mod reporter;

pub use annotate::{AnnotateKind, Annotation, SourceAnnotation};
pub use display::{
    DisplayLocation, DisplayWithLocations, FileRangeDisplay, FnDisplay, SpanDisplay,
    WithDisplayLocations,
};
pub use message::{MessageBundle, ReportMessage, ReportWhen};
pub use reporter::{MessageBuilder, MessageSink};
use toc_span::{Span, TextRange, TextSize};

/// A compilation result, including a bundle of associated messages
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileResult<T, L: Location = Span> {
    result: T,
    messages: Arc<MessageBundle<L>>,
}

impl<T, L: Location> CompileResult<T, L> {
    pub fn new(result: T, messages: MessageBundle<L>) -> Self {
        Self {
            result,
            messages: Arc::new(messages),
        }
    }

    /// Gets the produced item.
    pub fn result(&self) -> &T {
        &self.result
    }

    /// Gets the associated message bundle.
    pub fn messages(&self) -> &MessageBundle<L> {
        &self.messages
    }

    pub fn bundle_messages(&self, bundle: &mut MessageBundle<L>) {
        bundle.aggregate(self.messages())
    }

    /// Destructures the result into its component parts
    ///
    /// There must not be any other clones to self
    pub fn take(mut self) -> (T, MessageBundle<L>) {
        let messages =
            Arc::get_mut(&mut self.messages).expect("other clones of CompileResult exists");
        let messages = std::mem::take(messages);
        (self.result, messages)
    }

    /// Maps a `CompileResult<T>` into a `CompileResult<U>`
    pub fn map<F, U>(self, f: F) -> CompileResult<U, L>
    where
        F: FnOnce(T) -> U,
    {
        CompileResult {
            result: f(self.result),
            messages: self.messages,
        }
    }
}

impl<T: Clone, L: Location> CompileResult<T, L> {
    pub fn remap_spans<M: Location, F: Fn(L) -> M>(&self, map: F) -> CompileResult<T, M> {
        let mapped = self
            .messages
            .iter()
            .cloned()
            .map(|m| m.map_spans(&map))
            .collect();

        CompileResult {
            result: self.result.clone(),
            messages: Arc::new(MessageBundle::from_messages(mapped)),
        }
    }
}

/// Locations to attach message annotations to
pub trait Location: Copy + Eq + Debug + Ord {}

impl Location for toc_span::Span {}
impl Location for FileRange {}

/// Wrapper around [`TextRange`] so that it's suitable
/// as a report location
#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct FileRange(pub TextRange);

impl Debug for FileRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (start, end): (u32, u32) = (self.0.start().into(), self.0.end().into());
        f.write_fmt(format_args!("{start}..{end}"))
    }
}

impl FileRange {
    pub fn new(start: TextSize, end: TextSize) -> Self {
        Self(TextRange::new(start, end))
    }
}

impl PartialOrd for FileRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.start().cmp(&other.0.start())
    }
}

impl From<TextRange> for FileRange {
    fn from(value: TextRange) -> Self {
        Self(value)
    }
}
