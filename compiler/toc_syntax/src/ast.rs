//! AST nodes
use super::{SyntaxKind, SyntaxNode, SyntaxToken};

macro_rules! ast_node {
    ($ast:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(SyntaxNode);

        impl $ast {
            pub fn cast(syntax: SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    SyntaxKind::$ast => Some(Self(syntax)),
                    _ => None,
                }
            }

            pub fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
}

macro_rules! ast_node_group {
    ($node_group:ident, $($kind:pat => $ast:ident),+ $(,)?) => {
        #[derive(Debug, PartialEq, Eq, Hash)]
        pub enum $node_group {
            $($ast($ast),)+
        }
        impl $node_group {
            pub fn cast(syntax: SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    $($kind => Some(Self::$ast($ast::cast(syntax)?)),)+
                    _ => None
                }
            }

            pub fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$ast(node) => &node.syntax(),)+
                }
            }
        }
    };
}

// Basic defs

ast_node!(Source);
ast_node!(Name);
ast_node!(NameList);
ast_node!(StmtList);

// Stmt
ast_node!(ConstVarDecl);
ast_node!(TypeDecl);

ast_node!(IfStmt);
ast_node!(ElseifStmt);
ast_node!(ElseStmt);
ast_node!(BlockStmt);
ast_node!(AssignStmt);
ast_node!(CallStmt);
ast_node!(ImportStmt);

ast_node_group!(Stmt,
    SyntaxKind::ConstVarDecl => ConstVarDecl,
    SyntaxKind::TypeDecl => TypeDecl,
    SyntaxKind::IfStmt => IfStmt,
    SyntaxKind::ElseifStmt => ElseifStmt,
    SyntaxKind::ElseStmt => ElseStmt,
    SyntaxKind::BlockStmt => BlockStmt,
    SyntaxKind::AssignStmt => AssignStmt,
    SyntaxKind::CallStmt => CallStmt,
    SyntaxKind::ImportStmt => ImportStmt,
);

// Expr
ast_node!(NameExpr);
ast_node!(FieldExpr);
ast_node!(DerefExpr);
ast_node!(ArrowExpr);
ast_node!(IndirectExpr);
ast_node!(BitsExpr);
ast_node!(LiteralExpr);
ast_node!(CheatExpr);
ast_node!(ObjClassExpr);
ast_node!(InitExpr);
ast_node!(BinaryExpr);
ast_node!(UnaryExpr);
ast_node!(ParenExpr);
ast_node!(CallExpr);

ast_node_group!(Expr,
    SyntaxKind::NameExpr => NameExpr,
    SyntaxKind::FieldExpr => FieldExpr,
    SyntaxKind::DerefExpr => DerefExpr,
    SyntaxKind::ArrowExpr => ArrowExpr,
    SyntaxKind::IndirectExpr => IndirectExpr,
    SyntaxKind::BitsExpr => BitsExpr,
    SyntaxKind::LiteralExpr => LiteralExpr,
    SyntaxKind::CheatExpr => CheatExpr,
    SyntaxKind::ObjClassExpr => ObjClassExpr,
    SyntaxKind::InitExpr => InitExpr,
    SyntaxKind::BinaryExpr => BinaryExpr,
    SyntaxKind::UnaryExpr => UnaryExpr,
    SyntaxKind::ParenExpr => ParenExpr,
    SyntaxKind::CallExpr => CallExpr,
);

// Type
ast_node!(PrimType);
ast_node!(NameType);
ast_node!(RangeType);
ast_node!(EnumType);
ast_node!(ArrayType);
ast_node!(SetType);
ast_node!(RecordType);
ast_node!(UnionType);
ast_node!(PointerType);
ast_node!(SubprogType);
ast_node!(CollectionType);
ast_node!(ConditionType);

ast_node_group!(Type,
    SyntaxKind::PrimType => PrimType,
    SyntaxKind::NameType => NameType,
    SyntaxKind::RangeType => RangeType,
    SyntaxKind::EnumType => EnumType,
    SyntaxKind::ArrayType => ArrayType,
    SyntaxKind::SetType => SetType,
    SyntaxKind::RecordType => RecordType,
    SyntaxKind::UnionType => UnionType,
    SyntaxKind::PointerType => PointerType,
    SyntaxKind::SubprogType => SubprogType,
    SyntaxKind::CollectionType => CollectionType,
    SyntaxKind::ConditionType => ConditionType,
);

impl Source {
    pub fn unit(&self) -> Option<SyntaxToken> {
        self.0
            .first_token()
            .filter(|token| matches!(token.kind(), SyntaxKind::KwUnit))
    }

    pub fn import_stmt(&self) -> Option<ImportStmt> {
        self.0.first_child().and_then(ImportStmt::cast)
    }

    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl StmtList {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        self.0.children().filter_map(Stmt::cast)
    }
}
