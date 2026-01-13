//! Events produced during parsing
use toc_syntax::SyntaxKind;

#[derive(Debug, PartialEq)]
#[derive(Default)]
pub(crate) enum Event {
    StartNode {
        kind: SyntaxKind,
        /// Relative offset to the parent node event
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    #[default]
    Placeholder,
    /// Dropped start node
    Tombstone,
}

