//! Common message reporting for all compiler libraries
use std::fmt;

pub use text_size::{TextRange, TextSize};

/// Type of message reported
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum MessageKind {
    Warning,
    Error,
}

/// A reported message
#[derive(Debug)]
pub struct ReportMessage {
    kind: MessageKind,
    msg: String,
    range: TextRange,
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
}

impl fmt::Display for ReportMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (start, end) = (u32::from(self.range.start()), u32::from(self.range.end()));
        let kind = match self.kind() {
            MessageKind::Warning => "warn",
            MessageKind::Error => "error",
        };

        write!(f, "{} at {}..{}: {}", kind, start, end, self.msg)
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
    pub fn report(&mut self, kind: MessageKind, message: &str, range: TextRange) {
        self.messages.push(ReportMessage {
            kind,
            msg: message.to_string(),
            range,
        });
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
