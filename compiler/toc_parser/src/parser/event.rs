//! Events produced during parsing
use crate::syntax::SyntaxKind;

use rowan::SmolStr;

#[derive(Debug, Clone)]
pub(crate) enum Event {
    StartNode { kind: SyntaxKind },
    StartNodeAt { kind: SyntaxKind, checkpoint: usize },
    AddToken { kind: SyntaxKind, text: SmolStr },
    FinishNode,
}
