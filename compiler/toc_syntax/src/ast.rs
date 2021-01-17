//! AST nodes
mod nodes;

use super::SyntaxNode;
pub use nodes::*;

mod helper {
    use crate::{ast::AstNode, SyntaxKind, SyntaxNode, SyntaxToken};

    pub(super) fn token(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        node.children_with_tokens()
            .filter_map(|thing| thing.into_token())
            .find(|tk| tk.kind() == kind)
    }

    pub(super) fn node<N: AstNode>(node: &SyntaxNode) -> Option<N> {
        node.children().find_map(N::cast)
    }

    pub(super) fn nodes<'n, N: AstNode + 'n>(node: &'n SyntaxNode) -> impl Iterator<Item = N> + 'n {
        node.children().filter_map(N::cast)
    }
}

pub trait AstNode: Sized {
    fn cast(syntax: SyntaxNode) -> Option<Self>;

    fn can_cast(syntax: &SyntaxNode) -> bool;

    fn syntax(&self) -> &SyntaxNode;
}
