//! Common message reporting for all compiler libraries
use std::fmt;

use toc_span::Span;

// TODO: Create a common compiler message bundle

/// Type of annotation added to a message
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotateKind {
    /// Information related to the main message.
    /// May be context specific.
    Note,
    /// More detailed related to the message.
    Info,
    /// Warning annotation
    Warning,
    /// Error annotation
    Error,
}

impl fmt::Display for AnnotateKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            AnnotateKind::Note => "note",
            AnnotateKind::Info => "info",
            AnnotateKind::Warning => "warn",
            AnnotateKind::Error => "error",
        })
    }
}

/// A reported message
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReportMessage {
    header: SourceAnnotation,
    annotations: Vec<SourceAnnotation>,
    footer: Vec<Annotation>,
}

impl ReportMessage {
    /// Gets the kind of message reported
    pub fn kind(&self) -> AnnotateKind {
        self.header.annotation.kind
    }

    /// Gets the reported message
    pub fn message(&self) -> &str {
        self.header.annotation.message()
    }

    /// Gets the span of text the message covers
    pub fn span(&self) -> Span {
        self.header.span
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceAnnotation {
    annotation: Annotation,
    span: Span,
}

impl SourceAnnotation {
    /// Gets the kind of annotation reported
    pub fn kind(&self) -> AnnotateKind {
        self.annotation.kind()
    }

    /// Gets the annotation's message
    pub fn message(&self) -> &str {
        self.annotation.message()
    }

    /// Gets the span of text the annotation covers.
    pub fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for SourceAnnotation {
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

        if f.alternate() {
            write!(f, " for {}..{}: {}", start, end, self.message())?;
        } else {
            write!(f, " at {}..{}: {}", start, end, self.message())?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    kind: AnnotateKind,
    msg: String,
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
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind(), self.msg)
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

    /// Reports an error message
    pub fn error(&mut self, message: &str, span: Span) {
        self.report(AnnotateKind::Error, message, span)
    }

    /// Reports a detailed error message
    pub fn error_detailed(&mut self, message: &str, span: Span) -> MessageBuilder {
        self.report_detailed(AnnotateKind::Error, message, span)
    }

    /// Reports a warning message
    pub fn warn(&mut self, message: &str, span: Span) {
        self.report(AnnotateKind::Warning, message, span)
    }

    /// Reports a detailed warning message
    pub fn warn_detailed(&mut self, message: &str, span: Span) -> MessageBuilder {
        self.report_detailed(AnnotateKind::Warning, message, span)
    }

    /// Reports a message
    ///
    /// Does not add any annotations to the message
    fn report(&mut self, kind: AnnotateKind, message: &str, span: Span) {
        MessageBuilder::new(self, kind, message, span).finish();
    }

    /// Reports a detailed message
    ///
    /// Returns a builder for adding annotations
    #[must_use = "message is not reported until `finish()` is called"]
    fn report_detailed(&mut self, kind: AnnotateKind, message: &str, span: Span) -> MessageBuilder {
        MessageBuilder::new(self, kind, message, span)
    }

    /// Removes any subsequent messages that share the same text range
    pub fn dedup_shared_ranges(&mut self) {
        self.messages
            .dedup_by(|a, b| a.header.span == b.header.span && a.kind() == b.kind())
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
    kind: AnnotateKind,
    message: String,
    span: Span,
    annotations: Vec<SourceAnnotation>,
    footer: Vec<Annotation>,
}

impl<'a> MessageBuilder<'a> {
    pub fn new(
        reporter: &'a mut MessageSink,
        kind: AnnotateKind,
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
            footer: vec![],
        }
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

    fn with_annotation<R>(mut self, kind: AnnotateKind, message: &str, span: R) -> Self
    where
        R: Into<Option<Span>>,
    {
        match span.into() {
            Some(span) => self.annotations.push(SourceAnnotation {
                annotation: Annotation {
                    kind,
                    msg: message.to_string(),
                },
                span,
            }),
            None => self.footer.push(Annotation {
                kind,
                msg: message.to_string(),
            }),
        }

        self
    }

    pub fn finish(self) {
        let MessageBuilder {
            mut drop_bomb,
            reporter,
            kind,
            message,
            span,
            annotations,
            footer,
        } = self;

        // Defuse bomb now
        drop_bomb.defuse();

        reporter.messages.push(ReportMessage {
            header: SourceAnnotation {
                annotation: Annotation { kind, msg: message },
                span,
            },
            annotations,
            footer,
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
            AnnotateKind::Error,
            "an error message",
            Span::new(None, TextRange::new(1.into(), 3.into())),
        );

        sink.report(
            AnnotateKind::Warning,
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
