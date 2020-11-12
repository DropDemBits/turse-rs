//! Compile context things
use toc_core::{MessageSource, ReportMessage};

/// Context for the current compilation session
#[derive(Debug)]
pub struct CompileContext {
    // message aggregator unit source maps, etc.
    /// All aggregated messages
    messages: Vec<ReportMessage>,
}

impl CompileContext {
    pub fn new() -> Self {
        Self { messages: vec![] }
    }

    /// Takes reported messages from the given message source
    pub fn aggregate_messages(&mut self, message_source: &mut impl MessageSource) {
        self.messages
            .append(&mut message_source.take_reported_messages())
    }

    /// Gives a reference to all of the aggregated messages
    pub fn messages(&self) -> &Vec<ReportMessage> {
        &self.messages
    }
}

impl Default for CompileContext {
    fn default() -> Self {
        Self::new()
    }
}
