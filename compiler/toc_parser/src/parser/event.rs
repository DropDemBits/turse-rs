//! Events produced during parsing
use crate::syntax::SyntaxKind;

use rowan::SmolStr;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Event {
    StartNode {
        kind: SyntaxKind,
        /// Relative offset to the parent node event
        forward_parent: Option<usize>,
    },
    AddToken {
        kind: SyntaxKind,
        text: SmolStr,
    },
    FinishNode,
    Placeholder,
}
