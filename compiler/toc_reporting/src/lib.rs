//! Common message reporting for all compiler libraries
use std::fmt;

use toc_span::TextRange;

/// Type of message reported.
/// Kinds are ordered by severity (from least severe, to most severe).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessageKind {
    Warning,
    Error,
}

/// Type of annotation added to a message
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotateKind {
    /// Information related to the main message.
    /// May be context specific.
    Note,
    /// More detailed related to the message.
    Info,
}

/// A reported message
#[derive(Debug)]
pub struct ReportMessage {
    kind: MessageKind,
    msg: String,
    range: TextRange,
    annotations: Vec<Annotation>,
}

impl ReportMessage {
    /// Gets the kind of message reported
    pub fn kind(&self) -> MessageKind {
        self.kind
    }

    /// Gets the reported message
    pub fn message(&self) -> &str {
        &self.msg
    }

    /// Gets the range of text the message covers
    pub fn text_range(&self) -> TextRange {
        self.range
    }

    /// Gets any associated annotations
    pub fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }
}

impl fmt::Display for ReportMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (start, end) = (u32::from(self.range.start()), u32::from(self.range.end()));
        let kind = match self.kind() {
            MessageKind::Warning => "warn",
            MessageKind::Error => "error",
        };

        write!(f, "{} at {}..{}: {}", kind, start, end, self.msg)?;

        // Report any annotations
        for annotation in &self.annotations {
            write!(f, "\n| {}", annotation)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Annotation {
    kind: AnnotateKind,
    msg: String,
    range: Option<TextRange>,
}

impl Annotation {
    /// Gets the kind of annotation reported
    pub fn kind(&self) -> AnnotateKind {
        self.kind
    }

    /// Gets the annotation's message
    pub fn message(&self) -> &str {
        &self.msg
    }

    /// Gets the range of text the message covers.
    /// If `None`, no range should be reported either
    pub fn text_range(&self) -> Option<TextRange> {
        self.range
    }
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self.kind() {
            AnnotateKind::Info => "info",
            AnnotateKind::Note => "note",
        };

        if let Some(range) = self.range {
            let (start, end) = (u32::from(range.start()), u32::from(range.end()));
            write!(f, "{} for {}..{}: {}", kind, start, end, self.msg)
        } else {
            write!(f, "{}: {}", kind, self.msg)
        }
    }
}

/// Sink for message reports
#[derive(Debug, Default)]
pub struct MessageSink {
    messages: Vec<ReportMessage>,
}

impl MessageSink {
    pub fn new() -> Self {
        Self { messages: vec![] }
    }

    /// Merges an existing set of messages into a new MessageSink
    pub fn with_messages(messages: Vec<ReportMessage>) -> Self {
        Self { messages }
    }

    /// Reports a message
    ///
    /// Does not add any annotations to the message
    pub fn report(&mut self, kind: MessageKind, message: &str, range: TextRange) {
        MessageBuilder::new(self, kind, message, range).finish();
    }

    /// Reports a detailed message
    ///
    /// Returns a builder for adding annotations
    pub fn report_detailed(
        &mut self,
        kind: MessageKind,
        message: &str,
        range: TextRange,
    ) -> MessageBuilder {
        MessageBuilder::new(self, kind, message, range)
    }

    /// Removes any subsequent messages that share the same text range
    pub fn dedup_shared_ranges(&mut self) {
        self.messages
            .dedup_by(|a, b| a.range == b.range && a.kind == b.kind)
    }

    /// Finishes reporting any messages, giving back the final message list
    pub fn finish(self) -> Vec<ReportMessage> {
        self.messages
    }
}

/// Builder for detailed messages
#[derive(Debug)]
pub struct MessageBuilder<'a> {
    drop_bomb: drop_bomb::DropBomb,
    reporter: &'a mut MessageSink,
    kind: MessageKind,
    message: String,
    range: TextRange,
    annotations: Vec<Annotation>,
}

impl<'a> MessageBuilder<'a> {
    pub fn new(
        reporter: &'a mut MessageSink,
        kind: MessageKind,
        message: &str,
        range: TextRange,
    ) -> Self {
        Self {
            drop_bomb: drop_bomb::DropBomb::new("Missing `finish()` for MessageBuilder"),
            reporter,
            kind,
            message: message.to_string(),
            range,
            annotations: vec![],
        }
    }

    pub fn with_annotation<R>(mut self, kind: AnnotateKind, message: &str, range: R) -> Self
    where
        R: Into<Option<TextRange>>,
    {
        self.annotations.push(Annotation {
            kind,
            msg: message.to_string(),
            range: range.into(),
        });

        self
    }

    pub fn with_note<R>(self, message: &str, range: R) -> Self
    where
        R: Into<Option<TextRange>>,
    {
        self.with_annotation(AnnotateKind::Note, message, range)
    }

    pub fn with_info<R>(self, message: &str, range: R) -> Self
    where
        R: Into<Option<TextRange>>,
    {
        self.with_annotation(AnnotateKind::Info, message, range)
    }

    pub fn finish(self) {
        let MessageBuilder {
            mut drop_bomb,
            reporter,
            kind,
            message,
            range,
            annotations,
        } = self;

        // Defuse bomb now
        drop_bomb.defuse();

        reporter.messages.push(ReportMessage {
            kind,
            msg: message,
            range,
            annotations,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn report_message() {
        let mut sink = MessageSink::new();
        sink.report(
            MessageKind::Error,
            "an error message",
            TextRange::new(1.into(), 3.into()),
        );

        sink.report(
            MessageKind::Warning,
            "a warning message",
            TextRange::new(3.into(), 5.into()),
        );

        let msgs = sink.finish();
        assert_eq!(
            (msgs[0].message(), msgs[0].text_range()),
            ("an error message", TextRange::new(1.into(), 3.into()))
        );

        assert_eq!(
            (msgs[1].message(), msgs[1].text_range()),
            ("a warning message", TextRange::new(3.into(), 5.into()))
        );
    }
}
