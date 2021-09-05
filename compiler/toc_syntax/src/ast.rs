//! AST nodes

// Taken from rust-analyzer's syntax crate
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:ident) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = $crate::ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

#[rustfmt::skip] // formatted during codegen
mod nodes;
mod nodes_ext;

use super::SyntaxNode;
pub use nodes::*;
pub use nodes_ext::ExternalItemOwner;

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

