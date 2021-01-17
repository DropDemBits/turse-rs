//! AST nodes
use super::{SyntaxKind, SyntaxNode, SyntaxToken};

mod helper {
    use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

    pub(super) fn first_token(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        node.first_token().filter(|token| token.kind() == kind)
    }

    pub(super) fn token(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        node.children_with_tokens()
            .filter_map(|thing| match thing {
                rowan::NodeOrToken::Token(tk) if tk.kind() == kind => Some(tk),
                _ => None,
            })
            .next()
    }
}

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
    ($node_group:ident, $($kind:pat => $variant:ident : $ast:ident),+ $(,)?) => {
        #[derive(Debug, PartialEq, Eq, Hash)]
        pub enum $node_group {
            $($variant($ast),)+
        }
        impl $node_group {
            pub fn cast(syntax: SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    $($kind => Some(Self::$variant($ast::cast(syntax)?)),)+
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

// Basic node defs

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
ast_node!(PreprocGlob);

ast_node_group!(PreprocKind,
    SyntaxKind::PPIf => If:PPIf,
    SyntaxKind::PPElseif => Elseif:PPElseif,
    SyntaxKind::PPElse => Else:PPElse,
    SyntaxKind::PPEndIf => EndIf:PPEndIf,
    _ => Include:PPInclude,
);

ast_node!(PPInclude);
ast_node!(PPIf);
ast_node!(PPElseif);
ast_node!(PPElse);
ast_node!(PPEndIf);

ast_node_group!(PPExpr,
    SyntaxKind::PPBinaryExpr => BinaryExpr:PPBinaryExpr,
    SyntaxKind::PPUnaryExpr => UnaryExpr:PPUnaryExpr,
    SyntaxKind::PPParenExpr => ParenExpr:PPParenExpr,
    _ => NameExpr:PPNameExpr,
);

ast_node!(PPBinaryExpr);
ast_node!(PPUnaryExpr);
ast_node!(PPNameExpr);
ast_node!(PPParenExpr);

ast_node!(PPTokenBody);

// Stmt
ast_node_group!(Stmt,
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
    SyntaxKind::ElseifStmt => ElseifStmt,
    SyntaxKind::ElseStmt => ElseStmt,
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
    SyntaxKind::WaitStmt => WaitStmt,
    SyntaxKind::PauseStmt => PauseStmt,
    SyntaxKind::QuitStmt => QuitStmt,
    SyntaxKind::BreakStmt => BreakStmt,
    SyntaxKind::CheckednessStmt => CheckednessStmt,
    _ => PreprocGlob,
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
ast_node!(ElseifStmt);
ast_node!(ElseStmt);
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
ast_node!(WaitStmt);
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
    SyntaxKind::FcnHeader => FcnHeader,
    _ => ProcHeader,
);

ast_node_group!(BodyKind,
    SyntaxKind::ProcHeader => ProcHeader,
    SyntaxKind::FcnHeader => FcnHeader,
    _ => PlainHeader,
);
ast_node!(PlainHeader);

ast_node_group!(ExternalKind,
    SyntaxKind::FcnHeader => ExternalFcn:FcnHeader,
    SyntaxKind::ProcHeader => ExternalProc:ProcHeader,
    _ => ExternalVar:ExternalVar,
);
ast_node!(ExternalVar);

ast_node!(AsnOp); // AsnOp is special (in impl)

ast_node_group!(OpenKind,
    SyntaxKind::NewOpen => NewOpen,
    _ => OldOpen,
);
ast_node!(OldOpen);
ast_node!(OpenPath);
ast_node!(OpenMode);

ast_node!(NewOpen);
ast_node!(IoCap); // IoCap is special (in impl)

ast_node_group!(CloseKind,
    SyntaxKind::NewClose => NewClose,
    _ => OldClose,
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
    _ => ForwardAttr,
);

ast_node!(ExportItem);
ast_node_group!(ExportAttr,
    SyntaxKind::VarAttr => VarAttr,
    SyntaxKind::UnqualifiedAttr => UnqualifiedAttr,
    SyntaxKind::PervasiveAttr => PervasiveAttr,
    _ => OpaqueAttr,
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
    _ => CallExpr,
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
    _ => CallExpr,
);

ast_node!(SizeSpec);
ast_node_group!(IndirectTy,
    SyntaxKind::PrimType => PrimTy:PrimType,
    _ => NameTy:Reference,
);

ast_node_group!(BitRange,
    SyntaxKind::RangeSpec => RangeSpec,
    _ => Expr,
);

ast_node!(ParamList);
ast_node!(Param);
ast_node_group!(ParamKind,
    SyntaxKind::AllItem => AllItem,
    SyntaxKind::RangeItem => RangeItem,
    _ => Expr,
);

ast_node!(AllItem);
ast_node!(RangeItem);
ast_node_group!(RangeBound,
    SyntaxKind::RelativeBound => RelativeBound,
    _ => Expr,
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
    _ => ConditionType,
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
    _ => ProcType,
);

ast_node_group!(ParamDecl,
    SyntaxKind::ConstVarParam => ConstVarParam:ConstVarParam,
    _ => SubprogParam:SubprogType,
);
ast_node!(ConstVarParam);

ast_node!(ConditionKind);

// Misc
ast_node!(EndGroup);

impl Name {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        helper::first_token(&self.0, SyntaxKind::Identifier)
    }
}

impl NameList {
    pub fn names(&self) -> impl Iterator<Item = Name> + '_ {
        self.0.children().filter_map(Name::cast)
    }
}

impl PreprocGlob {
    pub fn directive(&self) -> Option<PreprocKind> {
        self.0.children().find_map(PreprocKind::cast)
    }
}

impl Source {
    pub fn unit_token(&self) -> Option<SyntaxToken> {
        helper::first_token(&self.0, SyntaxKind::KwUnit)
    }

    pub fn import_stmt(&self) -> Option<ImportStmt> {
        self.0.children().find_map(ImportStmt::cast)
    }

    pub fn stmt_list(&self) -> Option<StmtList> {
        self.0.children().find_map(StmtList::cast)
    }
}

impl StmtList {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        self.0.children().filter_map(Stmt::cast)
    }
}

impl ModuleDecl {
    pub fn module_token(&self) -> Option<SyntaxToken> {
        helper::token(&self.0, SyntaxKind::KwModule)
    }

    pub fn name(&self) -> Option<Name> {
        self.0.children().find_map(Name::cast)
    }

    pub fn end_group(&self) -> Option<EndGroup> {
        self.0.children().find_map(EndGroup::cast)
    }
}

impl ClassDecl {
    pub fn monitor_token(&self) -> Option<SyntaxToken> {
        helper::token(&self.0, SyntaxKind::KwMonitor)
    }

    pub fn class_token(&self) -> Option<SyntaxToken> {
        helper::token(&self.0, SyntaxKind::KwClass)
    }

    pub fn name(&self) -> Option<Name> {
        self.0.children().find_map(Name::cast)
    }

    pub fn device_spec(&self) -> Option<DeviceSpec> {
        self.0.children().find_map(DeviceSpec::cast)
    }

    pub fn end_group(&self) -> Option<EndGroup> {
        self.0.children().find_map(EndGroup::cast)
    }
}

impl MonitorDecl {
    pub fn monitor_token(&self) -> Option<SyntaxToken> {
        helper::token(&self.0, SyntaxKind::KwMonitor)
    }

    pub fn name(&self) -> Option<Name> {
        self.0.children().find_map(Name::cast)
    }

    pub fn device_spec(&self) -> Option<DeviceSpec> {
        self.0.children().find_map(DeviceSpec::cast)
    }

    pub fn end_group(&self) -> Option<EndGroup> {
        self.0.children().find_map(EndGroup::cast)
    }
}

impl EndGroup {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        helper::token(&self.0, SyntaxKind::Identifier)
    }
}
