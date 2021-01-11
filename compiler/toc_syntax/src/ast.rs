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
    ($node_group:ident, $($kind:pat => $variant:ident:$ast:ident),+ $(,)?) => {
        #[derive(Debug, PartialEq, Eq, Hash)]
        pub enum $node_group {
            $($variant($ast),)+
        }
        impl $node_group {
            pub fn cast(syntax: SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    $($kind => Some(Self::$variant($ast::cast(syntax)?)),)+
                    _ => None
                }
            }

            pub fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$variant(node) => &node.syntax(),)+
                }
            }
        }
    };
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

// Attrs
ast_node!(UnqualifiedAttr);
ast_node!(PervasiveAttr);
ast_node!(RegisterAttr);
ast_node!(ConstAttr);
ast_node!(VarAttr);
ast_node!(CheatAttr);
ast_node!(ForwardAttr);
ast_node!(OpaqueAttr);

// Preprocs
ast_node_group!(PreprocGlob,
    SyntaxKind::PPInclude => Include:PPInclude,
    SyntaxKind::PPIf => If:PPIf,
    SyntaxKind::PPElseif => Elseif:PPElseif,
    SyntaxKind::PPElse => Else:PPElse,
    SyntaxKind::PPEndIf => EndIf:PPEndIf,
);

ast_node!(PPInclude);
ast_node!(PPIf);
ast_node!(PPElseif);
ast_node!(PPElse);
ast_node!(PPEndIf);

ast_node_group!(PPExpr,
    SyntaxKind::PPBinaryExpr => BinaryExpr:PPBinaryExpr,
    SyntaxKind::PPUnaryExpr => UnaryExpr:PPUnaryExpr,
    SyntaxKind::PPNameExpr => NameExpr:PPNameExpr,
    SyntaxKind::PPParenExpr => ParenExpr:PPParenExpr,
);

ast_node!(PPBinaryExpr);
ast_node!(PPUnaryExpr);
ast_node!(PPNameExpr);
ast_node!(PPParenExpr);

ast_node!(PPTokenBody);

// Stmt
ast_node_group!(Stmt,
    SyntaxKind::PreprocGlob => PreprocGlob,
    SyntaxKind::ConstVarDecl => ConstVarDecl,
    SyntaxKind::TypeDecl => TypeDecl,
    SyntaxKind::BindDecl => BindDecl,
    SyntaxKind::ProcDecl => ProcDecl,
    SyntaxKind::FcnDecl => FcnDecl,
    SyntaxKind::ProcessDecl => ProcessDecl,
    SyntaxKind::ExternalDecl => ExternalDecl,
    SyntaxKind::ForwardDecl => ForwardDecl,
    SyntaxKind::DeferredDecl => DeferredDecl,
    SyntaxKind::BodyDecl => BodyDecl,
    SyntaxKind::ModuleDecl => ModuleDecl,
    SyntaxKind::ClassDecl => ClassDecl,
    SyntaxKind::MonitorDecl => MonitorDecl,
    SyntaxKind::AssignStmt => AssignStmt,
    SyntaxKind::OpenStmt => OpenStmt,
    SyntaxKind::CloseStmt => CloseStmt,
    SyntaxKind::PutStmt => PutStmt,
    SyntaxKind::GetStmt => GetStmt,
    SyntaxKind::ReadStmt => ReadStmt,
    SyntaxKind::WriteStmt => WriteStmt,
    SyntaxKind::SeekStmt => SeekStmt,
    SyntaxKind::TellStmt => TellStmt,
    SyntaxKind::ForStmt => ForStmt,
    SyntaxKind::LoopStmt => LoopStmt,
    SyntaxKind::ExitStmt => ExitStmt,
    SyntaxKind::IfStmt => IfStmt,
    SyntaxKind::CaseStmt => CaseStmt,
    SyntaxKind::BlockStmt => BlockStmt,
    SyntaxKind::InvariantStmt => InvariantStmt,
    SyntaxKind::AssertStmt => AssertStmt,
    SyntaxKind::CallStmt => CallStmt,
    SyntaxKind::ReturnStmt => ReturnStmt,
    SyntaxKind::ResultStmt => ResultStmt,
    SyntaxKind::NewStmt => NewStmt,
    SyntaxKind::FreeStmt => FreeStmt,
    SyntaxKind::TagStmt => TagStmt,
    SyntaxKind::ForkStmt => ForkStmt,
    SyntaxKind::SignalStmt => SignalStmt,
    SyntaxKind::PauseStmt => PauseStmt,
    SyntaxKind::QuitStmt => QuitStmt,
    SyntaxKind::BreakStmt => BreakStmt,
    SyntaxKind::CheckednessStmt => CheckednessStmt,
);

ast_node!(StmtList);

ast_node!(ConstVarDecl);
ast_node!(TypeDecl);
ast_node!(BindDecl);
ast_node!(ProcDecl);
ast_node!(FcnDecl);
ast_node!(ProcessDecl);
ast_node!(ExternalDecl);
ast_node!(ForwardDecl);
ast_node!(DeferredDecl);
ast_node!(BodyDecl);
ast_node!(ModuleDecl);
ast_node!(ClassDecl);
ast_node!(MonitorDecl);

ast_node!(AssignStmt);
ast_node!(OpenStmt);
ast_node!(CloseStmt);
ast_node!(PutStmt);
ast_node!(GetStmt);
ast_node!(ReadStmt);
ast_node!(WriteStmt);
ast_node!(SeekStmt);
ast_node!(TellStmt);
ast_node!(ForStmt);
ast_node!(LoopStmt);
ast_node!(ExitStmt);
ast_node!(IfStmt);
ast_node!(CaseStmt);
ast_node!(BlockStmt);
ast_node!(InvariantStmt);
ast_node!(AssertStmt);
ast_node!(CallStmt);
ast_node!(ReturnStmt);
ast_node!(ResultStmt);
ast_node!(NewStmt);
ast_node!(FreeStmt);
ast_node!(TagStmt);
ast_node!(ForkStmt);
ast_node!(SignalStmt);
ast_node!(PauseStmt);
ast_node!(QuitStmt);
ast_node!(BreakStmt);
ast_node!(CheckednessStmt);
ast_node!(PreStmt);
ast_node!(InitStmt);
ast_node!(PostStmt);
ast_node!(HandlerStmt);
ast_node!(InheritStmt);
ast_node!(ImplementStmt);
ast_node!(ImplementByStmt);
ast_node!(ImportStmt);
ast_node!(ExportStmt);

ast_node!(ProcHeader);
ast_node!(DeviceSpec);
ast_node!(FcnHeader);
ast_node!(FcnResult);

ast_node_group!(SubprogHeader,
    SyntaxKind::ProcHeader => ProcHeader,
    SyntaxKind::FcnHeader => FcnHeader,
);

ast_node_group!(BodyKind,
    SyntaxKind::PlainHeader => PlainHeader,
    SyntaxKind::ProcHeader => ProcHeader,
    SyntaxKind::FcnHeader => FcnHeader,
);
ast_node!(PlainHeader);

ast_node_group!(ExternalKind,
    SyntaxKind::FcnHeader => ExternalFcn:FcnHeader,
    SyntaxKind::ProcHeader => ExternalProc:ProcHeader,
    SyntaxKind::ExternalVar => ExternalVar:ExternalVar,
);
ast_node!(ExternalVar);

ast_node!(AsnOp); // AsnOp is special (in impl)

ast_node_group!(OpenKind,
    SyntaxKind::OldOpen => OldOpen,
    SyntaxKind::NewOpen => NewOpen,
);
ast_node!(OldOpen);
ast_node!(OpenPath);
ast_node!(OpenMode);

ast_node!(NewOpen);
ast_node!(IoCap); // IoCap is special (in impl)

ast_node_group!(CloseKind,
    SyntaxKind::OldClose => OldClose,
    SyntaxKind::NewClose => NewClose,
);

ast_node!(OldClose);
ast_node!(NewClose);

ast_node!(PutItem); // give put kind?
ast_node!(PutOpt);

ast_node!(GetItem); // give get kind?
ast_node!(GetWidth);

ast_node!(StreamNum);

ast_node!(BinaryIO);
ast_node!(BinaryItem);
ast_node!(RequestSize);
ast_node!(ActualSize);

ast_node!(ForBounds);
ast_node!(StepBy);

ast_node!(IfBody);

ast_node!(CaseArm);

ast_node!(ForkStatus);
ast_node!(StackSize);
ast_node!(ProcessDesc);

ast_node!(QuitCause); // special

ast_node!(InitVar);

ast_node!(ImportList);
ast_node!(ImportItem);
ast_node_group!(ImportAttr,
    SyntaxKind::VarAttr => VarAttr,
    SyntaxKind::ConstAttr => ConstAttr,
    SyntaxKind::ForwardAttr => ForwardAttr,
);

ast_node!(ExportItem);
ast_node_group!(ExportAttr,
    SyntaxKind::VarAttr => VarAttr,
    SyntaxKind::UnqualifiedAttr => UnqualifiedAttr,
    SyntaxKind::PervasiveAttr => PervasiveAttr,
    SyntaxKind::OpaqueAttr => OpaqueAttr,
);

ast_node!(ExternalItem);

// Expr
ast_node!(LiteralExpr);
ast_node!(ObjClassExpr);
ast_node!(InitExpr);
ast_node!(NilExpr);
ast_node!(SizeOfExpr);
ast_node!(BinaryExpr);
ast_node!(UnaryExpr);
ast_node!(ParenExpr);
ast_node!(NameExpr);
ast_node!(SelfExpr);
ast_node!(FieldExpr);
ast_node!(DerefExpr);
ast_node!(CheatExpr);
ast_node!(NatCheatExpr);
ast_node!(ArrowExpr);
ast_node!(IndirectExpr);
ast_node!(BitsExpr);
ast_node!(CallExpr);

ast_node_group!(Expr,
    SyntaxKind::LiteralExpr => LiteralExpr,
    SyntaxKind::ObjClassExpr => ObjClassExpr,
    SyntaxKind::InitExpr => InitExpr,
    SyntaxKind::NilExpr => NilExpr,
    SyntaxKind::SizeOfExpr => SizeOfExpr,
    SyntaxKind::BinaryExpr => BinaryExpr,
    SyntaxKind::UnaryExpr => UnaryExpr,
    SyntaxKind::ParenExpr => ParenExpr,
    SyntaxKind::NameExpr => NameExpr,
    SyntaxKind::SelfExpr => SelfExpr,
    SyntaxKind::FieldExpr => FieldExpr,
    SyntaxKind::DerefExpr => DerefExpr,
    SyntaxKind::CheatExpr => CheatExpr,
    SyntaxKind::NatCheatExpr => NatCheatExpr,
    SyntaxKind::ArrowExpr => ArrowExpr,
    SyntaxKind::IndirectExpr => IndirectExpr,
    SyntaxKind::BitsExpr => BitsExpr,
    SyntaxKind::CallExpr => CallExpr,
);

ast_node_group!(Reference,
    SyntaxKind::NameExpr => NameExpr,
    SyntaxKind::SelfExpr => SelfExpr,
    SyntaxKind::FieldExpr => FieldExpr,
    SyntaxKind::DerefExpr => DerefExpr,
    SyntaxKind::CheatExpr => CheatExpr,
    SyntaxKind::NatCheatExpr => NatCheatExpr,
    SyntaxKind::ArrowExpr => ArrowExpr,
    SyntaxKind::IndirectExpr => IndirectExpr,
    SyntaxKind::BitsExpr => BitsExpr,
    SyntaxKind::CallExpr => CallExpr,
);

ast_node!(SizeSpec);
ast_node_group!(IndirectTy,
    SyntaxKind::PrimType => PrimTy:PrimType,
    SyntaxKind::Reference => NameTy:Reference,
);

ast_node_group!(BitRange,
    SyntaxKind::Expr => Expr,
    SyntaxKind::RangeSpec => RangeSpec,
);

ast_node!(ParamList);
ast_node_group!(Param,
    SyntaxKind::Expr => Expr,
    SyntaxKind::AllItem => AllItem,
    SyntaxKind::RangeItem => RangeItem,
);

ast_node!(AllItem);
ast_node!(RangeItem);
ast_node_group!(RangeBound,
    SyntaxKind::RelativeBound => RelativeBound,
    SyntaxKind::Expr => Expr,
);
ast_node!(RelativeBound);

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
ast_node!(FcnType);
ast_node!(ProcType);
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
    SyntaxKind::FcnType => FcnType,
    SyntaxKind::ProcType => ProcType,
    SyntaxKind::CollectionType => CollectionType,
    SyntaxKind::ConditionType => ConditionType,
);

ast_node!(SizedCharType);
ast_node!(SizedStringType);
ast_node!(SeqLength); // special???

ast_node!(RangeSpec);
ast_node!(RangeList);

ast_node!(UnionVariant);
ast_node!(RecordField);
ast_node!(Checkedness);

ast_node_group!(SubprogType,
    SyntaxKind::FcnType => FcnType,
    SyntaxKind::ProcType => ProcType,
);

ast_node_group!(ParamDecl,
    SyntaxKind::ConstVarParam => ConstVarParam:ConstVarParam,
    SyntaxKind::SubprogType => SubprogParam:SubprogType,
);
ast_node!(ConstVarParam);

ast_node!(ConditionKind);

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
