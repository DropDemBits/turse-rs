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

use std::marker::PhantomData;

pub use nodes::*;

mod helper {
    use crate::{SyntaxKind, SyntaxNode, SyntaxToken, ast::AstNode};

    pub(super) fn token(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        node.children_with_tokens()
            .filter_map(|thing| thing.into_token())
            .find(|tk| tk.kind() == kind)
    }

    pub(super) fn node<N: AstNode<Language = crate::Lang>>(node: &SyntaxNode) -> Option<N> {
        node.children().find_map(N::cast)
    }

    pub(super) fn nodes<'n, N: AstNode<Language = crate::Lang> + 'n>(
        node: &'n SyntaxNode,
    ) -> impl Iterator<Item = N> + 'n {
        node.children().filter_map(N::cast)
    }
}

pub use rowan::ast::AstNode;

use crate::{SyntaxKind, SyntaxNode, SyntaxNodePtr};

pub trait ExternalItemOwner: AstNode<Language = crate::Lang> {
    fn external_items(&self) -> Vec<ExternalItem> {
        self.syntax()
            .descendants()
            .filter_map(ExternalItem::cast)
            .collect()
    }
}

/// Like [`rowan::ast::AstPtr`], but there's a `try_from_raw`.
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> std::fmt::Debug for AstPtr<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("AstPtr").field(&self.raw).finish()
    }
}

impl<N: AstNode> Copy for AstPtr<N> {}
impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> AstPtr<N> {
        *self
    }
}

impl<N: AstNode> Eq for AstPtr<N> {}

impl<N: AstNode> PartialEq for AstPtr<N> {
    fn eq(&self, other: &AstPtr<N>) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> std::hash::Hash for AstPtr<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<N: AstNode<Language = crate::Lang>> AstPtr<N> {
    pub fn new(node: &N) -> AstPtr<N> {
        AstPtr {
            raw: SyntaxNodePtr::new(node.syntax()),
            _ty: PhantomData,
        }
    }

    pub fn to_node(&self, root: &SyntaxNode) -> N {
        let syntax_node = self.raw.to_node(root);
        N::cast(syntax_node).unwrap()
    }

    pub fn syntax_node_ptr(&self) -> SyntaxNodePtr {
        self.raw
    }

    pub fn text_range(&self) -> rowan::TextRange {
        self.raw.text_range()
    }

    pub fn cast<U: AstNode<Language = crate::Lang>>(self) -> Option<AstPtr<U>> {
        if !U::can_cast(self.raw.kind()) {
            return None;
        }
        Some(AstPtr {
            raw: self.raw,
            _ty: PhantomData,
        })
    }

    pub fn kind(&self) -> SyntaxKind {
        self.raw.kind()
    }

    pub fn upcast<M: AstNode>(self) -> AstPtr<M>
    where
        N: Into<M>,
    {
        AstPtr {
            raw: self.raw,
            _ty: PhantomData,
        }
    }

    /// Like `SyntaxNodePtr::cast` but the trait bounds work out.
    pub fn try_from_raw(raw: SyntaxNodePtr) -> Option<AstPtr<N>> {
        N::can_cast(raw.kind()).then_some(AstPtr {
            raw,
            _ty: PhantomData,
        })
    }

    pub fn wrap_left<R>(self) -> AstPtr<either::Either<N, R>>
    where
        either::Either<N, R>: AstNode,
    {
        AstPtr {
            raw: self.raw,
            _ty: PhantomData,
        }
    }

    pub fn wrap_right<L>(self) -> AstPtr<either::Either<L, N>>
    where
        either::Either<L, N>: AstNode,
    {
        AstPtr {
            raw: self.raw,
            _ty: PhantomData,
        }
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr {
    fn from(ptr: AstPtr<N>) -> SyntaxNodePtr {
        ptr.raw
    }
}
