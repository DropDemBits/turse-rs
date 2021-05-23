//! Common message reporting for all compiler libraries
use std::fmt;

use toc_span::Span;

/// Type of message reported.
/// Kinds are ordered by severity (from least severe, to most severe).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessageKind {
    Warning,
    Error,
}

impl fmt::Display for MessageKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            MessageKind::Warning => "warn",
            MessageKind::Error => "error",
        })
    }
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

impl fmt::Display for AnnotateKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            AnnotateKind::Note => "note",
            AnnotateKind::Info => "info",
        })
    }
}

/// A reported message
#[derive(Debug)]
pub struct ReportMessage {
    kind: MessageKind,
    msg: String,
    span: Span,
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

    /// Gets the span of text the message covers
    pub fn span(&self) -> Span {
        self.span
    }

    /// Gets any associated annotations
    pub fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }
}

impl fmt::Display for ReportMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file_id = self.span.file;
        let (start, end) = (
            u32::from(self.span.range.start()),
            u32::from(self.span.range.end()),
        );

        write!(f, "{}", self.kind())?;

        if let Some(file_id) = file_id {
            write!(f, " in file {:?}", file_id)?;
        }

        write!(f, " at {}..{}: {}", start, end, self.msg)?;

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
    span: Option<Span>,
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

    /// Gets the span of text the message covers.
    /// If `None`, no range should be reported either
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self.kind() {
            AnnotateKind::Info => "info",
            AnnotateKind::Note => "note",
        };

        if let Some(span) = self.span {
            let file_id = span.file;
            let (start, end) = (u32::from(span.range.start()), u32::from(span.range.end()));

            write!(f, "{}", self.kind())?;

            if let Some(file_id) = file_id {
                write!(f, " in file {:?}", file_id)?;
            }

            write!(f, " for {}..{}: {}", start, end, self.msg)?;

            Ok(())
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
    pub fn report(&mut self, kind: MessageKind, message: &str, span: Span) {
        MessageBuilder::new(self, kind, message, span).finish();
    }

    /// Reports an error message
    pub fn error(&mut self, message: &str, span: Span) {
        self.report(MessageKind::Error, message, span)
    }

    /// Reports a detailed error message
    pub fn error_detailed(&mut self, message: &str, span: Span) -> MessageBuilder {
        self.report_detailed(MessageKind::Error, message, span)
    }

    /// Reports a warning message
    pub fn warn(&mut self, message: &str, span: Span) {
        self.report(MessageKind::Warning, message, span)
    }

    /// Reports a detailed warning message
    pub fn warn_detailed(&mut self, message: &str, span: Span) -> MessageBuilder {
        self.report_detailed(MessageKind::Warning, message, span)
    }

    /// Reports a detailed message
    ///
    /// Returns a builder for adding annotations
    #[must_use = "message is not reported until `finish()` is called"]
    pub fn report_detailed(
        &mut self,
        kind: MessageKind,
        message: &str,
        span: Span,
    ) -> MessageBuilder {
        MessageBuilder::new(self, kind, message, span)
    }

    /// Removes any subsequent messages that share the same text range
    pub fn dedup_shared_ranges(&mut self) {
        self.messages
            .dedup_by(|a, b| a.span == b.span && a.kind == b.kind)
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
    span: Span,
    annotations: Vec<Annotation>,
}

impl<'a> MessageBuilder<'a> {
    pub fn new(
        reporter: &'a mut MessageSink,
        kind: MessageKind,
        message: &str,
        span: Span,
    ) -> Self {
        Self {
            drop_bomb: drop_bomb::DropBomb::new("Missing `finish()` for MessageBuilder"),
            reporter,
            kind,
            message: message.to_string(),
            span,
            annotations: vec![],
        }
    }

    pub fn with_annotation<R>(mut self, kind: AnnotateKind, message: &str, span: R) -> Self
    where
        R: Into<Option<Span>>,
    {
        self.annotations.push(Annotation {
            kind,
            msg: message.to_string(),
            span: span.into(),
        });

        self
    }

    pub fn with_note<S>(self, message: &str, span: S) -> Self
    where
        S: Into<Option<Span>>,
    {
        self.with_annotation(AnnotateKind::Note, message, span)
    }

    pub fn with_info<S>(self, message: &str, span: S) -> Self
    where
        S: Into<Option<Span>>,
    {
        self.with_annotation(AnnotateKind::Info, message, span)
    }

    pub fn finish(self) {
        let MessageBuilder {
            mut drop_bomb,
            reporter,
            kind,
            message,
            span,
            annotations,
        } = self;

        // Defuse bomb now
        drop_bomb.defuse();

        reporter.messages.push(ReportMessage {
            kind,
            msg: message,
            span,
            annotations,
        });
    }
}

#[cfg(test)]
mod tests {
    use toc_span::TextRange;

    use super::*;

    #[test]
    fn report_message() {
        let mut sink = MessageSink::new();
        sink.report(
            MessageKind::Error,
            "an error message",
            Span::new(None, TextRange::new(1.into(), 3.into())),
        );

        sink.report(
            MessageKind::Warning,
            "a warning message",
            Span::new(None, TextRange::new(3.into(), 5.into())),
        );

        let msgs = sink.finish();
        assert_eq!(
            (msgs[0].message(), msgs[0].span().range),
            ("an error message", TextRange::new(1.into(), 3.into()))
        );

        assert_eq!(
            (msgs[1].message(), msgs[1].span().range),
            ("a warning message", TextRange::new(3.into(), 5.into()))
        );
    }
}
