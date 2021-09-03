//! Message generation

use toc_span::Span;

use crate::{AnnotateKind, Annotation, MessageBundle, ReportMessage, SourceAnnotation};

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

    /* /// Removes any subsequent messages that share the same text range
    pub fn dedup_shared_ranges(&mut self) {
        self.messages
            .dedup_by(|a, b| a.header.span == b.header.span && a.kind() == b.kind())
    } */

    /// Finishes reporting any messages, giving back the final message list
    pub fn finish(self) -> MessageBundle {
        MessageBundle::from_messages(self.messages)
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
        // Test message sorting as well
        let mut sink = MessageSink::new();
        sink.report(
            AnnotateKind::Warning,
            "a warning message",
            Span::new(None, TextRange::new(3.into(), 5.into())),
        );

        sink.report(
            AnnotateKind::Error,
            "an error message",
            Span::new(None, TextRange::new(1.into(), 3.into())),
        );

        let msgs = sink.finish();
        let mut iter = msgs.iter();
        let msg = iter.next().unwrap();
        assert_eq!(
            (msg.message(), msg.span().range),
            ("an error message", TextRange::new(1.into(), 3.into()))
        );

        let msg = iter.next().unwrap();
        assert_eq!(
            (msg.message(), msg.span().range),
            ("a warning message", TextRange::new(3.into(), 5.into()))
        );
    }
}
