//! Hash implementations

use toc_syntax::{
    SyntaxKind, SyntaxNode,
    ast::{self, AstNode},
    match_ast,
};

use crate::ast_id::{AstIdKind, ErasedAstId};

use super::{AstIdGen, AstIdNode, MaybeAstIdNode};

#[derive(Hash)]
#[repr(u8)]
#[allow(unused)]
enum FunctionLikeKind {
    Procedure,
    Function,
    Process,
}

#[derive(Hash)]
struct ErasedNamedAstNode<'a> {
    kind: SyntaxKind,
    name: &'a str,
}

#[derive(Hash)]
struct ErasedAssocAstNode<'a> {
    parent: Option<ErasedAstId>,
    assoc: ErasedNamedAstNode<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ErasedBlockStmtNode {
    parent: Option<ErasedAstId>,
}

macro_rules! named_ast_id_impls {
    (impl AstIdNode for $($variant_ident:ident => $ast_ident:ident . $name_method:ident),+ $(,)? ) => {
        $(
            impl AstIdNode for ast::$ast_ident {}
        )+

        pub(super) fn named_ast_id(node: &SyntaxNode, id_gen: &mut AstIdGen) -> Option<ErasedAstId> {
            let kind = node.kind();
            match_ast! {
                match node {
                    $(
                        ast::$ast_ident(node) => {
                            let name = node.$name_method();
                            let name = name.as_ref().map_or("", |it| it.text_immutable());
                            Some(id_gen.next(AstIdKind::$variant_ident, ErasedNamedAstNode{
                                kind,
                                name,
                            }))
                        },
                    )*
                    _ => None,
                }
            }
        }

        pub(super) fn might_alloc_named(node: &SyntaxNode) -> bool {
            let kind = node.kind();
            false $( || ast::$ast_ident::can_cast(kind) )*
        }
    };
}

named_ast_id_impls! {
    impl AstIdNode for
        Type => TypeDecl.decl_name,
        ModuleDecl => ModuleDecl.name,
        ClassDecl => ClassDecl.name,
}

macro_rules! assoc_ast_id_impls {
    (impl AstIdNode for $($variant_ident:ident => $ast_ident:ident . $name_method:ident),+ $(,)?  ) => {
        $(
            impl AstIdNode for ast::$ast_ident {}
        )+

        pub(super) fn assoc_ast_id(node: &SyntaxNode, id_gen: &mut AstIdGen, parent: Option<ErasedAstId>) -> Option<ErasedAstId> {
            let kind = node.kind();
            match_ast! {
                match node {
                    $(
                        ast::$ast_ident(node) => {
                            let name = node.$name_method();
                            let name = name.as_ref().map_or("", |it| it.text_immutable());
                            Some(id_gen.next(AstIdKind::$variant_ident, ErasedAssocAstNode {
                                parent,
                                assoc: ErasedNamedAstNode {
                                    kind,
                                    name,
                                }
                            }))
                        },
                    )*
                    _ => None,
                }
            }
        }

        pub(super) fn might_alloc_assoc(node: &SyntaxNode) -> bool {
            let kind = node.kind();
            false $( || ast::$ast_ident::can_cast(kind) )*
        }
    };
}

assoc_ast_id_impls! {
    impl AstIdNode for
        EnumVariant => EnumVariant.name,
        RecordField => RecordFieldName.name,
}

impl MaybeAstIdNode for ast::StmtList {}

pub(crate) fn block_ast_id(
    node: SyntaxNode,
    id_gen: &mut AstIdGen,
    parent: Option<ErasedAstId>,
) -> Option<ErasedAstId> {
    let Some(_) = ast::StmtList::cast(node) else {
        return None;
    };

    Some(id_gen.next(AstIdKind::StmtList, ErasedBlockStmtNode { parent }))
}

impl MaybeAstIdNode for ast::ConstVarDeclName {}
impl MaybeAstIdNode for ast::BindItem {}

pub(super) fn maybe_assoc_ast_id(
    node: &SyntaxNode,
    id_gen: &mut AstIdGen,
    parent: Option<ErasedAstId>,
) -> Option<ErasedAstId> {
    match node.kind() {
        SyntaxKind::ConstVarDeclName => {
            let Some(node) = ast::ConstVarDeclName::cast(node.clone()) else {
                return None;
            };

            let name = node.name();
            let name = name.as_ref().map_or("", |it| it.text_immutable());
            Some(id_gen.next(
                AstIdKind::ConstVar,
                ErasedAssocAstNode {
                    parent,
                    assoc: ErasedNamedAstNode {
                        kind: node.syntax().kind(),
                        name,
                    },
                },
            ))
        }
        SyntaxKind::BindItem => {
            let Some(node) = ast::BindItem::cast(node.clone()) else {
                return None;
            };

            let name = node.bind_as();
            let name = name.as_ref().map_or("", |it| it.text_immutable());
            Some(id_gen.next(
                AstIdKind::ConstVar,
                ErasedAssocAstNode {
                    parent,
                    assoc: ErasedNamedAstNode {
                        kind: node.syntax().kind(),
                        name,
                    },
                },
            ))
        }
        _ => None,
    }
}
