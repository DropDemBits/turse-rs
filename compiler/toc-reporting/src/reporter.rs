//! Message generation

use toc_span::Span;

use crate::{
    AnnotateKind, Annotation, Location, MessageBundle, ReportMessage, ReportWhen, SourceAnnotation,
};

/// Sink for message reports
#[derive(Debug)]
pub struct MessageSink<L: Location = Span> {
    messages: Vec<ReportMessage<L>>,
}

impl<L: Location> MessageSink<L> {
    pub fn new() -> Self {
        Self { messages: vec![] }
    }

    /// Reports an error message
    pub fn error(&mut self, message: impl Into<String>, at_span: impl Into<String>, span: L) {
        self.report(AnnotateKind::Error, message.into(), span)
            .with_error(at_span.into(), span)
            .finish()
    }

    /// Reports a detailed error message
    pub fn error_detailed(&mut self, message: impl Into<String>, span: L) -> MessageBuilder<'_, L> {
        self.report(AnnotateKind::Error, message.into(), span)
    }

    /// Reports a warning message
    pub fn warn(&mut self, message: impl Into<String>, at_span: impl Into<String>, span: L) {
        self.report(AnnotateKind::Warning, message.into(), span)
            .with_warn(at_span.into(), span)
            .finish()
    }

    /// Reports a detailed warning message
    pub fn warn_detailed(&mut self, message: impl Into<String>, span: L) -> MessageBuilder<'_, L> {
        self.report(AnnotateKind::Warning, message.into(), span)
    }

    /// Reports a detailed message
    ///
    /// Returns a builder for adding annotations
    #[must_use = "message is not reported until `finish()` is called"]
    fn report(
        &mut self,
        kind: AnnotateKind,
        message: impl Into<String>,
        span: L,
    ) -> MessageBuilder<'_, L> {
        MessageBuilder::new(self, kind, message.into(), span)
    }

    /// Finishes reporting any messages, giving back the final message list
    pub fn finish(self) -> MessageBundle<L> {
        MessageBundle::from_messages(self.messages)
    }
}

impl<L: Location> Default for MessageSink<L> {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for detailed messages
#[derive(Debug)]
pub struct MessageBuilder<'a, L: Location = Span> {
    drop_bomb: drop_bomb::DropBomb,
    reporter: &'a mut MessageSink<L>,
    kind: AnnotateKind,
    message: String,
    span: L,
    annotations: Vec<SourceAnnotation<L>>,
    footer: Vec<Annotation>,
    when: ReportWhen,
}

impl<'a, L: Location> MessageBuilder<'a, L> {
    pub fn new(
        reporter: &'a mut MessageSink<L>,
        kind: AnnotateKind,
        message: impl Into<String>,
        span: L,
    ) -> Self {
        Self {
            drop_bomb: drop_bomb::DropBomb::new("Missing `finish()` for MessageBuilder"),
            reporter,
            kind,
            message: message.into(),
            span,
            annotations: vec![],
            footer: vec![],
            when: Default::default(),
        }
    }

    /// Each inserted info is treated as a separate line
    pub fn with_info(self, message: impl Into<String>) -> Self {
        self.with_annotation(AnnotateKind::Info, message.into(), None)
    }

    pub fn with_note(self, message: impl Into<String>, span: L) -> Self {
        self.with_annotation(AnnotateKind::Note, message.into(), span)
    }

    pub fn with_warn(self, message: impl Into<String>, span: L) -> Self {
        self.with_annotation(AnnotateKind::Warning, message.into(), span)
    }

    pub fn with_error(self, message: impl Into<String>, span: L) -> Self {
        self.with_annotation(AnnotateKind::Error, message.into(), span)
    }

    fn with_annotation<R>(mut self, kind: AnnotateKind, message: String, span: R) -> Self
    where
        R: Into<Option<L>>,
    {
        let annotation = Annotation { kind, msg: message };

        match span.into() {
            Some(span) => self.annotations.push(SourceAnnotation { annotation, span }),
            None => self.footer.push(annotation),
        }

        self
    }

    pub fn report_always(self) -> Self {
        self.with_when(ReportWhen::Always)
    }

    pub fn report_first_always(self) -> Self {
        self.with_when(ReportWhen::FirstAlways)
    }

    pub fn report_delayed(self) -> Self {
        self.with_when(ReportWhen::Delayed)
    }

    pub fn report_hidden(self) -> Self {
        self.with_when(ReportWhen::Hidden)
    }

    fn with_when(mut self, when: ReportWhen) -> Self {
        self.when = when;

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
            when,
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
            when,
        });
    }
}

#[cfg(test)]
mod tests {

    use crate::FileRange;

    use super::*;

    #[test]
    fn report_message() {
        // Test message sorting as well
        let mut sink = MessageSink::<FileRange>::new();
        sink.report(
            AnnotateKind::Warning,
            "a warning message",
            FileRange::new(3.into(), 5.into()),
        )
        .finish();

        sink.report(
            AnnotateKind::Error,
            "an error message",
            FileRange::new(1.into(), 3.into()),
        )
        .finish();

        let msgs = sink.finish();
        let mut iter = msgs.iter();
        let msg = iter.next().unwrap();
        assert_eq!(
            (msg.message(), msg.span()),
            ("an error message", FileRange::new(1.into(), 3.into()))
        );

        let msg = iter.next().unwrap();
        assert_eq!(
            (msg.message(), msg.span()),
            ("a warning message", FileRange::new(3.into(), 5.into()))
        );
    }
}
