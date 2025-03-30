//! Constructed messages

use std::collections::BTreeSet;
use std::fmt;

use toc_span::Span;

use crate::{AnnotateKind, Annotation, FileRange, Location, SourceAnnotation};

/// A bundle of messages
///
/// Messages are already sorted by file, then by starting location
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageBundle<L: Location = Span> {
    pub(crate) messages: Vec<ReportMessage<L>>,
}

impl<L: Location> MessageBundle<L> {
    pub(crate) fn from_messages(messages: Vec<ReportMessage<L>>) -> Self {
        let mut s = Self { messages };
        s.make_uniform();
        s
    }

    /// Creates an empty message bundle
    pub fn empty() -> Self {
        Self { messages: vec![] }
    }

    /// Combines two message bundles together
    pub fn combine(mut self, other: Self) -> Self {
        self.messages.extend(other.messages);
        self.make_uniform();
        self
    }

    /// Aggregates messages from another bundle
    pub fn aggregate(&mut self, other: &Self) {
        self.messages.extend_from_slice(&other.messages);
        self.make_uniform();
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ ReportMessage<L>> + '_ {
        self.messages.iter()
    }

    /// Puts the message bundle into a consistent state (sorted + all other metadata is correct)
    fn make_uniform(&mut self) {
        use std::cmp::Ordering;

        // Deal with occluding `FirstAlways` spans
        {
            let mut reported_at = BTreeSet::new();
            self.messages.retain(|msg| {
                if msg.when == ReportWhen::FirstAlways {
                    // Only accept the first kind at this span
                    reported_at.insert((msg.span(), msg.kind()))
                } else {
                    // Dealt with later
                    true
                }
            })
        }

        // Want to deal with all dupes, so needs to be sorted
        self.messages
            .sort_by(|a, b| Ord::cmp(&a.span(), &b.span()).then(Ord::cmp(&a.when, &b.when)));
        // Reverse vec so that messages get occluded in the correct order
        self.messages.reverse();

        // Occlude messages by kind and span
        self.messages.dedup_by(move |a, b| {
            // Spans must be the same
            if a.span() != b.span() {
                return false;
            }

            // Main annotation kinds must be the same
            if a.header.kind() != b.header.kind() {
                return false;
            }

            // Don't touch always reported spans
            if a.when.report_always() && b.when.report_always() {
                return false;
            }

            // From/To | Always | Delayed | Hidden
            // --------|--------|---------|--------
            // Always  | Keep   | From    | From
            // Delayed | To     | Keep    | From
            // Hidden  | To     | To      | To
            match a.when.cmp(&b.when) {
                // Always occlude lesser whens
                Ordering::Less => {
                    debug_assert!(
                        !(a.when == ReportWhen::FirstAlways && b.when == ReportWhen::Always)
                    );

                    true
                }
                // Only occlude when eq for `Hidden` whens
                Ordering::Equal if a.when == ReportWhen::Hidden => true,
                // Otherwise, keep both
                _ => false,
            }
        });

        self.messages.reverse();
    }

    /// Tests if this message bundle contains any error messages
    pub fn has_errors(&self) -> bool {
        self.messages
            .iter()
            .any(|msg| matches!(msg.kind(), AnnotateKind::Error))
    }

    /// Asserts that there aren't any delayed reports present
    pub fn assert_no_delayed_reports(&self) {
        let any_delayed = self
            .messages
            .iter()
            .find(|msg| msg.when == ReportWhen::Delayed);

        if let Some(delayed) = any_delayed {
            panic!(
                "detected delayed report message at {:?}, should have been reported over\n\nOriginal message:\n{:#?}\n",
                delayed.span(),
                delayed
            );
        }
    }
}

impl<L: Location> Default for MessageBundle<L> {
    fn default() -> Self {
        Self::empty()
    }
}

/// A reported message
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReportMessage<L: Location = Span> {
    pub(crate) header: SourceAnnotation<L>,
    pub(crate) annotations: Vec<SourceAnnotation<L>>,
    pub(crate) footer: Vec<Annotation>,
    pub(crate) when: ReportWhen,
}

impl<L: Location> ReportMessage<L> {
    /// Gets the kind of message reported
    pub fn kind(&self) -> AnnotateKind {
        self.header.kind()
    }

    /// Gets the reported message
    pub fn message(&self) -> &str {
        self.header.message()
    }

    /// Gets the span of text the message covers
    pub fn span(&self) -> L {
        self.header.span()
    }

    /// Gets any associated annotations
    pub fn annotations(&self) -> &[SourceAnnotation<L>] {
        &self.annotations
    }

    /// Gets any footer annotations (any annotation without a location in the source)
    pub fn footer(&self) -> &[Annotation] {
        &self.footer
    }

    pub(crate) fn map_spans<M: Location, F: Fn(L) -> M>(self, map: F) -> ReportMessage<M> {
        ReportMessage {
            header: self.header.map_spans(&map),
            annotations: self
                .annotations
                .into_iter()
                .map(|m| m.map_spans(&map))
                .collect(),
            footer: self.footer,
            when: self.when,
        }
    }
}

impl<L: Location> fmt::Display for ReportMessage<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.header)?;

        // Report any annotations
        for annotation in &self.annotations {
            write!(f, "\n| {annotation:#}")?;
        }

        // Report any footer messages
        for annotation in &self.footer {
            write!(f, "\n| {annotation:#}")?;
        }

        Ok(())
    }
}

/// When a [`ReportMessage`] should be reported
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReportWhen {
    /// Allow this message to be occluded by later messages.
    /// Used when a later stage improves the message at this span
    Hidden,
    /// Require this message to be occluded by a later message.
    /// Panic if one is encountered during the final reporting.
    Delayed,
    /// Always have this message reported, but only if it's the first at this span.
    /// Used for parser error messages, where earlier messages are more useful than later ones.
    FirstAlways,
    /// Always have this message reported (not occluded by other spans).
    /// This is the default option.
    Always,
}

impl ReportWhen {
    fn report_always(self) -> bool {
        matches!(self, Self::FirstAlways | Self::Always)
    }
}

impl Default for ReportWhen {
    fn default() -> Self {
        Self::Always
    }
}

#[cfg(test)]
mod tests {
    use toc_span::Span;

    use crate::*;

    #[test]
    fn collapse_hidden() {
        let mut reporter = MessageSink::default();

        reporter
            .error_detailed("first", Span::default())
            .report_hidden()
            .finish();
        reporter
            .error_detailed("second", Span::default())
            .report_hidden()
            .finish();
        reporter
            .error_detailed("third", Span::default())
            .report_hidden()
            .finish();

        let mut msgs = reporter.finish();
        msgs.make_uniform();

        assert_eq!(msgs.messages.len(), 1);
        assert_eq!(msgs.messages[0].header.message(), "third");
    }

    #[test]
    fn keep_delayed() {
        let mut reporter = MessageSink::default();

        reporter
            .error_detailed("first", Span::default())
            .report_delayed()
            .finish();
        reporter
            .error_detailed("second", Span::default())
            .report_delayed()
            .finish();
        reporter
            .error_detailed("third", Span::default())
            .report_delayed()
            .finish();

        let mut msgs = reporter.finish();
        let before = msgs.messages.clone();
        msgs.make_uniform();

        // Should be unchanged
        assert_eq!(msgs.messages.len(), 3);
        assert_eq!(before, msgs.messages);
    }

    #[test]
    fn delayed_occludes_hidden() {
        let mut reporter = MessageSink::<FileRange>::default();

        reporter
            .error_detailed("delayed", Default::default())
            .report_delayed()
            .finish();
        reporter
            .error_detailed("hidden", Default::default())
            .report_hidden()
            .finish();

        let mut msgs = reporter.finish();
        msgs.make_uniform();

        assert_eq!(msgs.messages.len(), 1);
        assert_eq!(msgs.messages[0].header.annotation.msg, "delayed");
    }

    #[test]
    fn always_occludes_both() {
        let mut reporter = MessageSink::<FileRange>::default();

        reporter
            .error_detailed("delayed", Default::default())
            .report_delayed()
            .finish();
        reporter
            .error_detailed("hidden", Default::default())
            .report_hidden()
            .finish();
        reporter
            .error_detailed("always", Default::default())
            .report_always()
            .finish();

        let mut msgs = reporter.finish();
        msgs.make_uniform();

        assert_eq!(msgs.messages.len(), 1);
        assert_eq!(msgs.messages[0].header.annotation.msg, "always");
    }

    #[test]
    fn keep_always_and_first_always() {
        let mut reporter = MessageSink::<FileRange>::default();

        reporter
            .error_detailed("always", Default::default())
            .report_always()
            .finish();
        reporter
            .error_detailed("first always", Default::default())
            .report_first_always()
            .finish();

        let mut msgs = reporter.finish();
        msgs.make_uniform();

        // `FirstAlways` before `Always`
        assert_eq!(msgs.messages.len(), 2);
        assert_eq!(msgs.messages[0].header.annotation.msg, "first always");
        assert_eq!(msgs.messages[1].header.annotation.msg, "always");
    }

    #[test]
    fn dedup_first_always() {
        let mut reporter = MessageSink::default();

        reporter
            .error_detailed("first", Span::default())
            .report_first_always()
            .finish();
        reporter
            .error_detailed("second", Span::default())
            .report_first_always()
            .finish();
        reporter
            .error_detailed("third", Span::default())
            .report_first_always()
            .finish();

        let mut msgs = reporter.finish();
        msgs.make_uniform();

        // Collapses to first
        assert_eq!(msgs.messages.len(), 1);
        assert_eq!(msgs.messages[0].header.message(), "first");
    }

    #[test]
    #[should_panic]
    fn assert_no_delayed_reports() {
        let mut reporter = MessageSink::<FileRange>::default();

        reporter
            .error_detailed("oops", Default::default())
            .report_delayed()
            .finish();

        let msgs = reporter.finish();
        msgs.assert_no_delayed_reports();
    }
}
