//! Events produced during parsing
use crate::parser::ParseMessage;

use toc_syntax::SyntaxKind;

#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    StartNode {
        kind: SyntaxKind,
        /// Relative offset to the parent node event
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    Message(ParseMessage),
    Placeholder,
    /// Dropped start node
    Tombstone,
}
