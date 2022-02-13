//! Constructed messages

use std::{collections::HashSet, fmt};

use toc_span::Span;

use crate::{AnnotateKind, Annotation, SourceAnnotation};

/// A bundle of messages
///
/// Messages are already sorted by file, then by starting location
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MessageBundle {
    pub(crate) messages: Vec<ReportMessage>,
}

impl MessageBundle {
    pub(crate) fn from_messages(messages: Vec<ReportMessage>) -> Self {
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
        self.messages.extend(other.messages.into_iter());
        self.make_uniform();
        self
    }

    /// Aggregates messages from another bundle
    pub fn aggregate(&mut self, other: &Self) {
        self.messages.extend_from_slice(&other.messages);
        self.make_uniform();
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ ReportMessage> + '_ {
        // Perform message deduplication
        // Earlier messages get reported over later ones
        // Annotate kinds are considered separately from each other
        // Spans that cover each-other are considered different

        let mut reported_at = HashSet::new();
        self.messages.iter().filter(move |msg| {
            // Only report the first one at a given span & kind
            reported_at.insert((msg.span(), msg.kind()))
        })
    }

    /// Puts the message bundle into a consistent state (sorted + all other metadata is correct)
    fn make_uniform(&mut self) {
        self.messages.sort_by_key(|item| item.span());
    }

    /// Tests if this message bundle contains any error messages
    pub fn has_errors(&self) -> bool {
        self.messages
            .iter()
            .any(|msg| matches!(msg.kind(), AnnotateKind::Error))
    }
}

/// A reported message
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReportMessage {
    // FIXME: These don't need to be pub crate
    pub(crate) header: SourceAnnotation,
    pub(crate) annotations: Vec<SourceAnnotation>,
    pub(crate) footer: Vec<Annotation>,
}

impl ReportMessage {
    /// Gets the kind of message reported
    pub fn kind(&self) -> AnnotateKind {
        self.header.kind()
    }

    /// Gets the reported message
    pub fn message(&self) -> &str {
        self.header.message()
    }

    /// Gets the span of text the message covers
    pub fn span(&self) -> Span {
        self.header.span()
    }

    /// Gets any associated annotations
    pub fn annotations(&self) -> &[SourceAnnotation] {
        &self.annotations
    }

    /// Gets any footer annotations (any annotation without a location in the source)
    pub fn footer(&self) -> &[Annotation] {
        &self.footer
    }
}

impl fmt::Display for ReportMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.header)?;

        // Report any annotations
        for annotation in &self.annotations {
            write!(f, "\n| {:#}", annotation)?;
        }

        // Report any footer messages
        for annotation in &self.footer {
            write!(f, "\n| {:#}", annotation)?;
        }

        Ok(())
    }
}
