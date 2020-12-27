//! Events produced during parsing
use toc_syntax::SyntaxKind;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Event {
    StartNode {
        kind: SyntaxKind,
        /// Relative offset to the parent node event
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    Placeholder,
}