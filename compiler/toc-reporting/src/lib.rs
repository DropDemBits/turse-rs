//! Common message reporting for all compiler crates
use std::sync::Arc;

mod annotate;
mod message;
mod reporter;

pub use annotate::{AnnotateKind, Annotation, SourceAnnotation};
pub use message::{MessageBundle, ReportMessage, ReportWhen};
pub use reporter::{MessageBuilder, MessageSink};

/// A compilation result, including a bundle of associated messages
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileResult<T> {
    result: T,
    messages: Arc<MessageBundle>,
}

impl<T> CompileResult<T> {
    pub fn new(result: T, messages: MessageBundle) -> Self {
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
    pub fn messages(&self) -> &MessageBundle {
        &self.messages
    }

    pub fn bundle_messages(&self, bundle: &mut MessageBundle) {
        bundle.aggregate(self.messages())
    }

    /// Destructures the result into its component parts
    ///
    /// There must not be any other clones to self
    pub fn take(mut self) -> (T, MessageBundle) {
        let messages =
            Arc::get_mut(&mut self.messages).expect("other clones of CompileResult exists");
        let messages = std::mem::take(messages);
        (self.result, messages)
    }

    /// Maps a `CompileResult<T>` into a `CompileResult<U>`
    pub fn map<F, U>(self, f: F) -> CompileResult<U>
    where
        F: FnOnce(T) -> U,
    {
        CompileResult {
            result: f(self.result),
            messages: self.messages,
        }
    }
}
