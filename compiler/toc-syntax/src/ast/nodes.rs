// This is a generated file, do not touch
use crate::{
    ast::{helper, AstNode},
    SyntaxKind, SyntaxNode, SyntaxToken,
};
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Name(SyntaxNode);
impl AstNode for Name {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Name => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::Name => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl Name {
    pub fn identifier_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Identifier) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NameRef(SyntaxNode);
impl AstNode for NameRef {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NameRef => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NameRef => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NameRef {
    pub fn identifier_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Identifier) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UnqualifiedAttr(SyntaxNode);
impl AstNode for UnqualifiedAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UnqualifiedAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::UnqualifiedAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl UnqualifiedAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PervasiveAttr(SyntaxNode);
impl AstNode for PervasiveAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PervasiveAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PervasiveAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PervasiveAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RegisterAttr(SyntaxNode);
impl AstNode for RegisterAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RegisterAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RegisterAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RegisterAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConstAttr(SyntaxNode);
impl AstNode for ConstAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConstAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct VarAttr(SyntaxNode);
impl AstNode for VarAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::VarAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::VarAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl VarAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CheatAttr(SyntaxNode);
impl AstNode for CheatAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CheatAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CheatAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CheatAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForwardAttr(SyntaxNode);
impl AstNode for ForwardAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ForwardAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ForwardAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ForwardAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct OpaqueAttr(SyntaxNode);
impl AstNode for OpaqueAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OpaqueAttr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OpaqueAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl OpaqueAttr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Source(SyntaxNode);
impl AstNode for Source {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Source => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::Source => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl Source {
    pub fn unit_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwUnit) }
    pub fn import_stmt(&self) -> Option<ImportStmt> { helper::node(&self.0) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ImportStmt(SyntaxNode);
impl AstNode for ImportStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ImportStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ImportStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ImportStmt {
    pub fn import_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwImport) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn imports(&self) -> Option<ImportList> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct StmtList(SyntaxNode);
impl AstNode for StmtList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::StmtList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::StmtList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl StmtList {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PreprocGlob(SyntaxNode);
impl AstNode for PreprocGlob {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PreprocGlob => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PreprocGlob => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PreprocGlob {
    pub fn directive(&self) -> Option<PreprocKind> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPInclude(SyntaxNode);
impl AstNode for PPInclude {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPInclude => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPInclude => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPInclude {
    pub fn include_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwInclude) }
    pub fn path(&self) -> Option<LiteralExpr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPIf(SyntaxNode);
impl AstNode for PPIf {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPIf => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPIf => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPIf {
    pub fn pp_if_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::PPKwIf) }
    pub fn condition(&self) -> Option<PPExpr> { helper::node(&self.0) }
    pub fn then_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwThen) }
    pub fn pp_token_body(&self) -> Option<PPTokenBody> { helper::node(&self.0) }
    pub fn false_branch(&self) -> Option<PPFalseBranch> { helper::node(&self.0) }
    pub fn pp_end_if(&self) -> Option<PPEndIf> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPElseif(SyntaxNode);
impl AstNode for PPElseif {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPElseif => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPElseif => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPElseif {
    pub fn pp_elseif_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::PPKwElseif) }
    pub fn pp_elsif_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::PPKwElsif) }
    pub fn condition(&self) -> Option<PPExpr> { helper::node(&self.0) }
    pub fn then_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwThen) }
    pub fn pp_token_body(&self) -> Option<PPTokenBody> { helper::node(&self.0) }
    pub fn false_branch(&self) -> Option<PPFalseBranch> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPElse(SyntaxNode);
impl AstNode for PPElse {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPElse => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPElse => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPElse {
    pub fn pp_else_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::PPKwElse) }
    pub fn pp_token_body(&self) -> Option<PPTokenBody> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPEndIf(SyntaxNode);
impl AstNode for PPEndIf {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPEndIf => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPEndIf => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPEndIf {
    pub fn pp_end_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::PPKwEnd) }
    pub fn if_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwIf) }
    pub fn pp_endif_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::PPKwEndIf) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct LiteralExpr(SyntaxNode);
impl AstNode for LiteralExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LiteralExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::LiteralExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl LiteralExpr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPTokenBody(SyntaxNode);
impl AstNode for PPTokenBody {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPTokenBody => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPTokenBody => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPTokenBody {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPBinaryExpr(SyntaxNode);
impl AstNode for PPBinaryExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPBinaryExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPBinaryExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPBinaryExpr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPUnaryExpr(SyntaxNode);
impl AstNode for PPUnaryExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPUnaryExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPUnaryExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPUnaryExpr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPNameExpr(SyntaxNode);
impl AstNode for PPNameExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPNameExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPNameExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPNameExpr {
    pub fn name_ref(&self) -> Option<NameRef> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PPParenExpr(SyntaxNode);
impl AstNode for PPParenExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPParenExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPParenExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PPParenExpr {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn pp_expr(&self) -> Option<PPExpr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConstVarDeclName(SyntaxNode);
impl AstNode for ConstVarDeclName {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarDeclName => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarDeclName => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConstVarDeclName {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeDecl(SyntaxNode);
impl AstNode for TypeDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TypeDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::TypeDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl TypeDecl {
    pub fn type_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwType) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn decl_name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn forward_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwForward) }
    pub fn named_ty(&self) -> Option<Type> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BindItem(SyntaxNode);
impl AstNode for BindItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BindItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BindItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BindItem {
    pub fn as_var(&self) -> Option<VarAttr> { helper::node(&self.0) }
    pub fn to_register(&self) -> Option<RegisterAttr> { helper::node(&self.0) }
    pub fn bind_as(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn to_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwTo) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ProcDecl(SyntaxNode);
impl AstNode for ProcDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ProcDecl {
    pub fn proc_header(&self) -> Option<ProcHeader> { helper::node(&self.0) }
    pub fn subprog_body(&self) -> Option<SubprogBody> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FcnDecl(SyntaxNode);
impl AstNode for FcnDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FcnDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FcnDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl FcnDecl {
    pub fn fcn_header(&self) -> Option<FcnHeader> { helper::node(&self.0) }
    pub fn subprog_body(&self) -> Option<SubprogBody> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ProcessDecl(SyntaxNode);
impl AstNode for ProcessDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcessDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcessDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ProcessDecl {
    pub fn process_header(&self) -> Option<ProcessHeader> { helper::node(&self.0) }
    pub fn subprog_body(&self) -> Option<SubprogBody> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExternalDecl(SyntaxNode);
impl AstNode for ExternalDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExternalDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExternalDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExternalDecl {
    pub fn external_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwExternal) }
    pub fn external_spec(&self) -> Option<CompTimeExpr> { helper::node(&self.0) }
    pub fn external_kind(&self) -> Option<ExternalKind> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForwardDecl(SyntaxNode);
impl AstNode for ForwardDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ForwardDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ForwardDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ForwardDecl {
    pub fn forward_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwForward) }
    pub fn subprog_header(&self) -> Option<SubprogHeader> { helper::node(&self.0) }
    pub fn import_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwImport) }
    pub fn import_list(&self) -> Option<ImportList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct DeferredDecl(SyntaxNode);
impl AstNode for DeferredDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::DeferredDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::DeferredDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl DeferredDecl {
    pub fn deferred_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwDeferred) }
    pub fn subprog_header(&self) -> Option<SubprogHeader> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BodyDecl(SyntaxNode);
impl AstNode for BodyDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BodyDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BodyDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BodyDecl {
    pub fn body_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBody) }
    pub fn body_kind(&self) -> Option<BodyKind> { helper::node(&self.0) }
    pub fn subprog_body(&self) -> Option<SubprogBody> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ModuleDecl(SyntaxNode);
impl AstNode for ModuleDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ModuleDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ModuleDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ModuleDecl {
    pub fn module_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwModule) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn implement_stmt(&self) -> Option<ImplementStmt> { helper::node(&self.0) }
    pub fn implement_by_stmt(&self) -> Option<ImplementByStmt> { helper::node(&self.0) }
    pub fn import_stmt(&self) -> Option<ImportStmt> { helper::node(&self.0) }
    pub fn export_stmt(&self) -> Option<ExportStmt> { helper::node(&self.0) }
    pub fn pre_stmt(&self) -> Option<PreStmt> { helper::node(&self.0) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn post_stmt(&self) -> Option<PostStmt> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ClassDecl(SyntaxNode);
impl AstNode for ClassDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ClassDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ClassDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ClassDecl {
    pub fn monitor_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwMonitor) }
    pub fn class_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwClass) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn device_spec(&self) -> Option<DeviceSpec> { helper::node(&self.0) }
    pub fn inherit_stmt(&self) -> Option<InheritStmt> { helper::node(&self.0) }
    pub fn implement_stmt(&self) -> Option<ImplementStmt> { helper::node(&self.0) }
    pub fn implement_by_stmt(&self) -> Option<ImplementByStmt> { helper::node(&self.0) }
    pub fn import_stmt(&self) -> Option<ImportStmt> { helper::node(&self.0) }
    pub fn export_stmt(&self) -> Option<ExportStmt> { helper::node(&self.0) }
    pub fn pre_stmt(&self) -> Option<PreStmt> { helper::node(&self.0) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn post_stmt(&self) -> Option<PostStmt> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct MonitorDecl(SyntaxNode);
impl AstNode for MonitorDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::MonitorDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::MonitorDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl MonitorDecl {
    pub fn monitor_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwMonitor) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn device_spec(&self) -> Option<DeviceSpec> { helper::node(&self.0) }
    pub fn implement_stmt(&self) -> Option<ImplementStmt> { helper::node(&self.0) }
    pub fn implement_by_stmt(&self) -> Option<ImplementByStmt> { helper::node(&self.0) }
    pub fn import_stmt(&self) -> Option<ImportStmt> { helper::node(&self.0) }
    pub fn export_stmt(&self) -> Option<ExportStmt> { helper::node(&self.0) }
    pub fn pre_stmt(&self) -> Option<PreStmt> { helper::node(&self.0) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn post_stmt(&self) -> Option<PostStmt> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConstVarDecl(SyntaxNode);
impl AstNode for ConstVarDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConstVarDecl {
    pub fn var_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwVar) }
    pub fn const_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwConst) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn register_attr(&self) -> Option<RegisterAttr> { helper::node(&self.0) }
    pub fn constvar_names(&self) -> Option<ConstVarDeclNameList> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn type_spec(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn assign_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Assign) }
    pub fn init(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BindDecl(SyntaxNode);
impl AstNode for BindDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BindDecl => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BindDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BindDecl {
    pub fn bind_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBind) }
    pub fn bindings(&self) -> impl Iterator<Item = BindItem> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct AssignStmt(SyntaxNode);
impl AstNode for AssignStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::AssignStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::AssignStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl AssignStmt {
    pub fn asn_op(&self) -> Option<AsnOp> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct OpenStmt(SyntaxNode);
impl AstNode for OpenStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OpenStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OpenStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl OpenStmt {
    pub fn open_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwOpen) }
    pub fn open_kind(&self) -> Option<OpenKind> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CloseStmt(SyntaxNode);
impl AstNode for CloseStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CloseStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CloseStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CloseStmt {
    pub fn close_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwClose) }
    pub fn close_kind(&self) -> Option<CloseKind> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PutStmt(SyntaxNode);
impl AstNode for PutStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PutStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PutStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PutStmt {
    pub fn put_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPut) }
    pub fn stream_num(&self) -> Option<StreamNum> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn items(&self) -> impl Iterator<Item = PutItem> + '_ { helper::nodes(&self.0) }
    pub fn range_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Range) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GetStmt(SyntaxNode);
impl AstNode for GetStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::GetStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::GetStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl GetStmt {
    pub fn get_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwGet) }
    pub fn stream_num(&self) -> Option<StreamNum> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn items(&self) -> impl Iterator<Item = GetItem> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ReadStmt(SyntaxNode);
impl AstNode for ReadStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ReadStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ReadStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ReadStmt {
    pub fn read_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwRead) }
    pub fn binary_io(&self) -> Option<BinaryIO> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct WriteStmt(SyntaxNode);
impl AstNode for WriteStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::WriteStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::WriteStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl WriteStmt {
    pub fn write_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwWrite) }
    pub fn binary_io(&self) -> Option<BinaryIO> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SeekStmt(SyntaxNode);
impl AstNode for SeekStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SeekStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SeekStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SeekStmt {
    pub fn seek_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSeek) }
    pub fn stream_num(&self) -> Option<StreamNum> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn star_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Star) }
    pub fn seek_to(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TellStmt(SyntaxNode);
impl AstNode for TellStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TellStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::TellStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl TellStmt {
    pub fn tell_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwTell) }
    pub fn stream_num(&self) -> Option<StreamNum> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn tell_to(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForStmt(SyntaxNode);
impl AstNode for ForStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ForStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ForStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ForStmt {
    pub fn for_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFor) }
    pub fn decreasing_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwDecreasing) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn for_bounds(&self) -> Option<ForBounds> { helper::node(&self.0) }
    pub fn steps(&self) -> Option<StepBy> { helper::node(&self.0) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct LoopStmt(SyntaxNode);
impl AstNode for LoopStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LoopStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::LoopStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl LoopStmt {
    pub fn loop_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwLoop) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExitStmt(SyntaxNode);
impl AstNode for ExitStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExitStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExitStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExitStmt {
    pub fn exit_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwExit) }
    pub fn when_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwWhen) }
    pub fn condition(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IfStmt(SyntaxNode);
impl AstNode for IfStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IfStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::IfStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl IfStmt {
    pub fn if_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwIf) }
    pub fn if_body(&self) -> Option<IfBody> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CaseStmt(SyntaxNode);
impl AstNode for CaseStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CaseStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CaseStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CaseStmt {
    pub fn case_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwCase) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn of_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwOf) }
    pub fn case_arm(&self) -> impl Iterator<Item = CaseArm> + '_ { helper::nodes(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BlockStmt(SyntaxNode);
impl AstNode for BlockStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BlockStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BlockStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BlockStmt {
    pub fn begin_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBegin) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InvariantStmt(SyntaxNode);
impl AstNode for InvariantStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::InvariantStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::InvariantStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl InvariantStmt {
    pub fn invariant_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwInvariant) }
    pub fn condition(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct AssertStmt(SyntaxNode);
impl AstNode for AssertStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::AssertStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::AssertStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl AssertStmt {
    pub fn assert_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwAssert) }
    pub fn condition(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CallStmt(SyntaxNode);
impl AstNode for CallStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CallStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CallStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CallStmt {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ReturnStmt(SyntaxNode);
impl AstNode for ReturnStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ReturnStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ReturnStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ReturnStmt {
    pub fn return_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwReturn) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ResultStmt(SyntaxNode);
impl AstNode for ResultStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ResultStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ResultStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ResultStmt {
    pub fn result_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwResult) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NewStmt(SyntaxNode);
impl AstNode for NewStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NewStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NewStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NewStmt {
    pub fn new_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwNew) }
    pub fn expr_list(&self) -> Option<ExprList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FreeStmt(SyntaxNode);
impl AstNode for FreeStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FreeStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FreeStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl FreeStmt {
    pub fn free_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFree) }
    pub fn expr_list(&self) -> Option<ExprList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TagStmt(SyntaxNode);
impl AstNode for TagStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TagStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::TagStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl TagStmt {
    pub fn tag_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwTag) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForkStmt(SyntaxNode);
impl AstNode for ForkStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ForkStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ForkStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ForkStmt {
    pub fn fork_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFork) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn param_list(&self) -> Option<ParamList> { helper::node(&self.0) }
    pub fn fork_status(&self) -> Option<ForkStatus> { helper::node(&self.0) }
    pub fn stack_size(&self) -> Option<StackSize> { helper::node(&self.0) }
    pub fn process_desc(&self) -> Option<ProcessDesc> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SignalStmt(SyntaxNode);
impl AstNode for SignalStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SignalStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SignalStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SignalStmt {
    pub fn signal_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSignal) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PauseStmt(SyntaxNode);
impl AstNode for PauseStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PauseStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PauseStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PauseStmt {
    pub fn pause_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPause) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct QuitStmt(SyntaxNode);
impl AstNode for QuitStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::QuitStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::QuitStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl QuitStmt {
    pub fn quit_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwQuit) }
    pub fn reason(&self) -> Option<QuitCause> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn quit_code(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BreakStmt(SyntaxNode);
impl AstNode for BreakStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BreakStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BreakStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BreakStmt {
    pub fn break_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBreak) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CheckednessStmt(SyntaxNode);
impl AstNode for CheckednessStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CheckednessStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CheckednessStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CheckednessStmt {
    pub fn checkedness(&self) -> Option<Checkedness> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PreStmt(SyntaxNode);
impl AstNode for PreStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PreStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PreStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PreStmt {
    pub fn pre_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPre) }
    pub fn condition(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InitStmt(SyntaxNode);
impl AstNode for InitStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::InitStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::InitStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl InitStmt {
    pub fn init_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwInit) }
    pub fn init_var(&self) -> impl Iterator<Item = InitVar> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PostStmt(SyntaxNode);
impl AstNode for PostStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PostStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PostStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PostStmt {
    pub fn post_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPost) }
    pub fn condition(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct HandlerStmt(SyntaxNode);
impl AstNode for HandlerStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::HandlerStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::HandlerStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl HandlerStmt {
    pub fn handler_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwHandler) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
    pub fn stmts(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InheritStmt(SyntaxNode);
impl AstNode for InheritStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::InheritStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::InheritStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl InheritStmt {
    pub fn inherit_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwInherit) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn external_item(&self) -> Option<ExternalItem> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ImplementStmt(SyntaxNode);
impl AstNode for ImplementStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ImplementStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ImplementStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ImplementStmt {
    pub fn implement_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwImplement) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn external_item(&self) -> Option<ExternalItem> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ImplementByStmt(SyntaxNode);
impl AstNode for ImplementByStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ImplementByStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ImplementByStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ImplementByStmt {
    pub fn implement_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwImplement) }
    pub fn by_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBy) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn external_item(&self) -> Option<ExternalItem> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExportStmt(SyntaxNode);
impl AstNode for ExportStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExportStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExportStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExportStmt {
    pub fn export_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwExport) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn exports(&self) -> impl Iterator<Item = ExportItem> + '_ { helper::nodes(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConstVarDeclNameList(SyntaxNode);
impl AstNode for ConstVarDeclNameList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarDeclNameList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarDeclNameList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConstVarDeclNameList {
    pub fn names(&self) -> impl Iterator<Item = ConstVarDeclName> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ProcHeader(SyntaxNode);
impl AstNode for ProcHeader {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcHeader => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcHeader => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ProcHeader {
    pub fn procedure_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwProcedure) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn params(&self) -> Option<ParamSpec> { helper::node(&self.0) }
    pub fn device_spec(&self) -> Option<DeviceSpec> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SubprogBody(SyntaxNode);
impl AstNode for SubprogBody {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SubprogBody => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SubprogBody => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SubprogBody {
    pub fn import_stmt(&self) -> Option<ImportStmt> { helper::node(&self.0) }
    pub fn pre_stmt(&self) -> Option<PreStmt> { helper::node(&self.0) }
    pub fn init_stmt(&self) -> Option<InitStmt> { helper::node(&self.0) }
    pub fn post_stmt(&self) -> Option<PostStmt> { helper::node(&self.0) }
    pub fn handler_stmt(&self) -> Option<HandlerStmt> { helper::node(&self.0) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct EndGroup(SyntaxNode);
impl AstNode for EndGroup {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::EndGroup => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::EndGroup => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl EndGroup {
    pub fn end_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwEnd) }
    pub fn identifier_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Identifier) }
    pub fn case_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwCase) }
    pub fn for_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFor) }
    pub fn if_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwIf) }
    pub fn loop_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwLoop) }
    pub fn handler_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwHandler) }
    pub fn union_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwUnion) }
    pub fn record_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwRecord) }
    pub fn endcase_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwEndCase) }
    pub fn endfor_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwEndFor) }
    pub fn endif_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwEndIf) }
    pub fn endloop_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwEndLoop) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ParamSpec(SyntaxNode);
impl AstNode for ParamSpec {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ParamSpec => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ParamSpec => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ParamSpec {
    pub fn param_decl(&self) -> impl Iterator<Item = ParamDecl> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct DeviceSpec(SyntaxNode);
impl AstNode for DeviceSpec {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::DeviceSpec => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::DeviceSpec => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl DeviceSpec {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn comp_time_expr(&self) -> Option<CompTimeExpr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CompTimeExpr(SyntaxNode);
impl AstNode for CompTimeExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CompTimeExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CompTimeExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CompTimeExpr {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FcnHeader(SyntaxNode);
impl AstNode for FcnHeader {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FcnHeader => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FcnHeader => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl FcnHeader {
    pub fn function_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFunction) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn params(&self) -> Option<ParamSpec> { helper::node(&self.0) }
    pub fn fcn_result(&self) -> Option<FcnResult> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FcnResult(SyntaxNode);
impl AstNode for FcnResult {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FcnResult => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FcnResult => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl FcnResult {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn ty(&self) -> Option<Type> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ImportList(SyntaxNode);
impl AstNode for ImportList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ImportList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ImportList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ImportList {
    pub fn import_item(&self) -> impl Iterator<Item = ImportItem> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PlainHeader(SyntaxNode);
impl AstNode for PlainHeader {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PlainHeader => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PlainHeader => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PlainHeader {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn params(&self) -> Option<ParamSpec> { helper::node(&self.0) }
    pub fn fcn_result(&self) -> Option<FcnResult> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ProcessHeader(SyntaxNode);
impl AstNode for ProcessHeader {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcessHeader => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcessHeader => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ProcessHeader {
    pub fn process_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwProcess) }
    pub fn pervasive_attr(&self) -> Option<PervasiveAttr> { helper::node(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn params(&self) -> Option<ParamSpec> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn stack_size(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExternalVar(SyntaxNode);
impl AstNode for ExternalVar {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExternalVar => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExternalVar => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExternalVar {
    pub fn var_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwVar) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn ty(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn assign_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Assign) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct AsnOp(SyntaxNode);
impl AstNode for AsnOp {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::AsnOp => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::AsnOp => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl AsnOp {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct OldOpen(SyntaxNode);
impl AstNode for OldOpen {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OldOpen => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OldOpen => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl OldOpen {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn open_path(&self) -> Option<OpenPath> { helper::node(&self.0) }
    pub fn open_mode(&self) -> Option<OpenMode> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NewOpen(SyntaxNode);
impl AstNode for NewOpen {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NewOpen => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NewOpen => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NewOpen {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn open_path(&self) -> Option<OpenPath> { helper::node(&self.0) }
    pub fn io_caps(&self) -> impl Iterator<Item = IoCap> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct OpenPath(SyntaxNode);
impl AstNode for OpenPath {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OpenPath => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OpenPath => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl OpenPath {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct OpenMode(SyntaxNode);
impl AstNode for OpenMode {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OpenMode => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OpenMode => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl OpenMode {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IoCap(SyntaxNode);
impl AstNode for IoCap {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IoCap => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::IoCap => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl IoCap {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct OldClose(SyntaxNode);
impl AstNode for OldClose {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OldClose => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OldClose => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl OldClose {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NewClose(SyntaxNode);
impl AstNode for NewClose {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NewClose => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NewClose => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NewClose {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct StreamNum(SyntaxNode);
impl AstNode for StreamNum {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::StreamNum => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::StreamNum => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl StreamNum {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PutItem(SyntaxNode);
impl AstNode for PutItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PutItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PutItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PutItem {
    pub fn skip_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSkip) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PutOpt(SyntaxNode);
impl AstNode for PutOpt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PutOpt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PutOpt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PutOpt {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GetItem(SyntaxNode);
impl AstNode for GetItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::GetItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::GetItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl GetItem {
    pub fn skip_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSkip) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn get_width(&self) -> Option<GetWidth> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GetWidth(SyntaxNode);
impl AstNode for GetWidth {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::GetWidth => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::GetWidth => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl GetWidth {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn star_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Star) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BinaryIO(SyntaxNode);
impl AstNode for BinaryIO {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BinaryIO => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BinaryIO => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BinaryIO {
    pub fn stream_num(&self) -> Option<StreamNum> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn status(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn items(&self) -> impl Iterator<Item = BinaryItem> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BinaryItem(SyntaxNode);
impl AstNode for BinaryItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BinaryItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BinaryItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BinaryItem {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn request_size(&self) -> Option<RequestSize> { helper::node(&self.0) }
    pub fn actual_size(&self) -> Option<ActualSize> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RequestSize(SyntaxNode);
impl AstNode for RequestSize {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RequestSize => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RequestSize => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RequestSize {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ActualSize(SyntaxNode);
impl AstNode for ActualSize {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ActualSize => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ActualSize => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ActualSize {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForBounds(SyntaxNode);
impl AstNode for ForBounds {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ForBounds => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ForBounds => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ForBounds {
    pub fn range_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Range) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct StepBy(SyntaxNode);
impl AstNode for StepBy {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::StepBy => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::StepBy => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl StepBy {
    pub fn by_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBy) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IfBody(SyntaxNode);
impl AstNode for IfBody {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IfBody => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::IfBody => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl IfBody {
    pub fn condition(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn then_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwThen) }
    pub fn true_branch(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn false_branch(&self) -> Option<FalseBranch> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ElseifStmt(SyntaxNode);
impl AstNode for ElseifStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ElseifStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ElseifStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ElseifStmt {
    pub fn elsif_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwElsif) }
    pub fn elseif_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwElseif) }
    pub fn if_body(&self) -> Option<IfBody> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ElseStmt(SyntaxNode);
impl AstNode for ElseStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ElseStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ElseStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ElseStmt {
    pub fn else_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwElse) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CaseArm(SyntaxNode);
impl AstNode for CaseArm {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CaseArm => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CaseArm => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CaseArm {
    pub fn label_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwLabel) }
    pub fn select(&self) -> Option<CompTimeExprList> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn stmt_list(&self) -> Option<StmtList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CompTimeExprList(SyntaxNode);
impl AstNode for CompTimeExprList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CompTimeExprList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CompTimeExprList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CompTimeExprList {
    pub fn exprs(&self) -> impl Iterator<Item = CompTimeExpr> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExprList(SyntaxNode);
impl AstNode for ExprList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExprList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExprList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExprList {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ParamList(SyntaxNode);
impl AstNode for ParamList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ParamList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ParamList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ParamList {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn param(&self) -> impl Iterator<Item = Param> + '_ { helper::nodes(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForkStatus(SyntaxNode);
impl AstNode for ForkStatus {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ForkStatus => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ForkStatus => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ForkStatus {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct StackSize(SyntaxNode);
impl AstNode for StackSize {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::StackSize => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::StackSize => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl StackSize {
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ProcessDesc(SyntaxNode);
impl AstNode for ProcessDesc {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcessDesc => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcessDesc => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ProcessDesc {
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct WaitStmt(SyntaxNode);
impl AstNode for WaitStmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::WaitStmt => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::WaitStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl WaitStmt {
    pub fn wait_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwWait) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct QuitCause(SyntaxNode);
impl AstNode for QuitCause {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::QuitCause => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::QuitCause => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl QuitCause {
    pub fn at_caller(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Less) }
    pub fn bubble_up(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Greater) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Checkedness(SyntaxNode);
impl AstNode for Checkedness {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Checkedness => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::Checkedness => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl Checkedness {
    pub fn checked_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwChecked) }
    pub fn unchecked_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwUnchecked) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InitVar(SyntaxNode);
impl AstNode for InitVar {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::InitVar => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::InitVar => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl InitVar {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn assign_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Assign) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ImportItem(SyntaxNode);
impl AstNode for ImportItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ImportItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ImportItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ImportItem {
    pub fn attrs(&self) -> impl Iterator<Item = ImportAttr> + '_ { helper::nodes(&self.0) }
    pub fn external_item(&self) -> Option<ExternalItem> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExternalItem(SyntaxNode);
impl AstNode for ExternalItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExternalItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExternalItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExternalItem {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn in_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwIn) }
    pub fn path(&self) -> Option<LiteralExpr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExportItem(SyntaxNode);
impl AstNode for ExportItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExportItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExportItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ExportItem {
    pub fn attrs(&self) -> impl Iterator<Item = ExportAttr> + '_ { helper::nodes(&self.0) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn all_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwAll) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ObjClassExpr(SyntaxNode);
impl AstNode for ObjClassExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ObjClassExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ObjClassExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ObjClassExpr {
    pub fn objectclass_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwObjectClass) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InitExpr(SyntaxNode);
impl AstNode for InitExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::InitExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::InitExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl InitExpr {
    pub fn init_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwInit) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn comp_time_expr_list(&self) -> Option<CompTimeExprList> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NilExpr(SyntaxNode);
impl AstNode for NilExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NilExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NilExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NilExpr {
    pub fn nil_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwNil) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SizeOfExpr(SyntaxNode);
impl AstNode for SizeOfExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SizeOfExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SizeOfExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SizeOfExpr {
    pub fn sizeof_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSizeOf) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn ty_size(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn ref_size(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BinaryExpr(SyntaxNode);
impl AstNode for BinaryExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BinaryExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BinaryExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BinaryExpr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UnaryExpr(SyntaxNode);
impl AstNode for UnaryExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UnaryExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::UnaryExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl UnaryExpr {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ParenExpr(SyntaxNode);
impl AstNode for ParenExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ParenExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ParenExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ParenExpr {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NameExpr(SyntaxNode);
impl AstNode for NameExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NameExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NameExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NameExpr {
    pub fn name_ref(&self) -> Option<NameRef> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SelfExpr(SyntaxNode);
impl AstNode for SelfExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SelfExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SelfExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SelfExpr {
    pub fn self_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSelf) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FieldExpr(SyntaxNode);
impl AstNode for FieldExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FieldExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FieldExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl FieldExpr {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn dot_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Dot) }
    pub fn name_ref(&self) -> Option<NameRef> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct DerefExpr(SyntaxNode);
impl AstNode for DerefExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::DerefExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::DerefExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl DerefExpr {
    pub fn caret_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Caret) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CheatExpr(SyntaxNode);
impl AstNode for CheatExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CheatExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CheatExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CheatExpr {
    pub fn cheat_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwCheat) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn ty(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn size_spec(&self) -> Option<SizeSpec> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NatCheatExpr(SyntaxNode);
impl AstNode for NatCheatExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NatCheatExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NatCheatExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NatCheatExpr {
    pub fn pound_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Pound) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ArrowExpr(SyntaxNode);
impl AstNode for ArrowExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ArrowExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ArrowExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ArrowExpr {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn arrow_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Arrow) }
    pub fn name_ref(&self) -> Option<NameRef> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IndirectExpr(SyntaxNode);
impl AstNode for IndirectExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IndirectExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::IndirectExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl IndirectExpr {
    pub fn indirect_ty(&self) -> Option<IndirectTy> { helper::node(&self.0) }
    pub fn at_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::At) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BitsExpr(SyntaxNode);
impl AstNode for BitsExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BitsExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::BitsExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl BitsExpr {
    pub fn bits_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwBits) }
    pub fn param_list(&self) -> Option<ParamList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CallExpr(SyntaxNode);
impl AstNode for CallExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CallExpr => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CallExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CallExpr {
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
    pub fn param_list(&self) -> Option<ParamList> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NotEq(SyntaxNode);
impl AstNode for NotEq {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NotEq => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NotEq => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NotEq {
    pub fn not_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwNot) }
    pub fn tilde_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Tilde) }
    pub fn equ_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Equ) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NotIn(SyntaxNode);
impl AstNode for NotIn {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NotIn => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NotIn => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NotIn {
    pub fn not_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwNot) }
    pub fn tilde_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Tilde) }
    pub fn in_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwIn) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SizeSpec(SyntaxNode);
impl AstNode for SizeSpec {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SizeSpec => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SizeSpec => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SizeSpec {
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn size(&self) -> Option<CompTimeExpr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PrimType(SyntaxNode);
impl AstNode for PrimType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PrimType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PrimType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PrimType {}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NameType(SyntaxNode);
impl AstNode for NameType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NameType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::NameType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl NameType {
    pub fn comp_time_expr(&self) -> Option<CompTimeExpr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Param(SyntaxNode);
impl AstNode for Param {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Param => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::Param => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl Param {
    pub fn param_kind(&self) -> Option<ParamKind> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct AllItem(SyntaxNode);
impl AstNode for AllItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::AllItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::AllItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl AllItem {
    pub fn all_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwAll) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RangeItem(SyntaxNode);
impl AstNode for RangeItem {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RangeItem => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RangeItem => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RangeItem {
    pub fn range_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Range) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RelativeBound(SyntaxNode);
impl AstNode for RelativeBound {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RelativeBound => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RelativeBound => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RelativeBound {
    pub fn star_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Star) }
    pub fn minus_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Minus) }
    pub fn expr(&self) -> Option<Expr> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RangeType(SyntaxNode);
impl AstNode for RangeType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RangeType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RangeType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RangeType {
    pub fn packed_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPacked) }
    pub fn range_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Range) }
    pub fn size_spec(&self) -> Option<SizeSpec> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct EnumType(SyntaxNode);
impl AstNode for EnumType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::EnumType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::EnumType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl EnumType {
    pub fn packed_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPacked) }
    pub fn enum_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwEnum) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn fields(&self) -> Option<EnumVariantList> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
    pub fn size_spec(&self) -> Option<SizeSpec> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ArrayType(SyntaxNode);
impl AstNode for ArrayType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ArrayType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ArrayType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ArrayType {
    pub fn packed_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPacked) }
    pub fn flexible_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFlexible) }
    pub fn array_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwArray) }
    pub fn range_list(&self) -> Option<RangeList> { helper::node(&self.0) }
    pub fn of_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwOf) }
    pub fn elem_ty(&self) -> Option<Type> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SetType(SyntaxNode);
impl AstNode for SetType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SetType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SetType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SetType {
    pub fn packed_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPacked) }
    pub fn set_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwSet) }
    pub fn of_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwOf) }
    pub fn elem_ty(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn size_spec(&self) -> Option<SizeSpec> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RecordType(SyntaxNode);
impl AstNode for RecordType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RecordType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RecordType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RecordType {
    pub fn packed_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPacked) }
    pub fn record_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwRecord) }
    pub fn record_field(&self) -> impl Iterator<Item = RecordField> + '_ { helper::nodes(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UnionType(SyntaxNode);
impl AstNode for UnionType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UnionType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::UnionType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl UnionType {
    pub fn packed_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPacked) }
    pub fn union_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwUnion) }
    pub fn tag_name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn range_ty(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn of_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwOf) }
    pub fn union_variant(&self) -> impl Iterator<Item = UnionVariant> + '_ { helper::nodes(&self.0) }
    pub fn end_group(&self) -> Option<EndGroup> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PointerType(SyntaxNode);
impl AstNode for PointerType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PointerType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PointerType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl PointerType {
    pub fn is_checked(&self) -> Option<Checkedness> { helper::node(&self.0) }
    pub fn pointer_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPointer) }
    pub fn to_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwTo) }
    pub fn caret_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Caret) }
    pub fn to_ty(&self) -> Option<Type> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FcnType(SyntaxNode);
impl AstNode for FcnType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FcnType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FcnType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl FcnType {
    pub fn function_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwFunction) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn params(&self) -> Option<ParamSpec> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn ty(&self) -> Option<Type> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ProcType(SyntaxNode);
impl AstNode for ProcType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ProcType {
    pub fn procedure_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwProcedure) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn params(&self) -> Option<ParamSpec> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CollectionType(SyntaxNode);
impl AstNode for CollectionType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CollectionType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::CollectionType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl CollectionType {
    pub fn collection_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwCollection) }
    pub fn of_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwOf) }
    pub fn elem_ty(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn forward_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwForward) }
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConditionType(SyntaxNode);
impl AstNode for ConditionType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConditionType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConditionType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConditionType {
    pub fn condition_kind(&self) -> Option<ConditionKind> { helper::node(&self.0) }
    pub fn condition_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwCondition) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SizedCharType(SyntaxNode);
impl AstNode for SizedCharType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SizedCharType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SizedCharType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SizedCharType {
    pub fn char_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwChar) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn seq_length(&self) -> Option<SeqLength> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SizedStringType(SyntaxNode);
impl AstNode for SizedStringType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SizedStringType => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SizedStringType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SizedStringType {
    pub fn string_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwString) }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::LeftParen) }
    pub fn seq_length(&self) -> Option<SeqLength> { helper::node(&self.0) }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::RightParen) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SeqLength(SyntaxNode);
impl AstNode for SeqLength {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SeqLength => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::SeqLength => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl SeqLength {
    pub fn comp_time_expr(&self) -> Option<CompTimeExpr> { helper::node(&self.0) }
    pub fn star_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Star) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UnsizedBound(SyntaxNode);
impl AstNode for UnsizedBound {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UnsizedBound => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::UnsizedBound => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl UnsizedBound {
    pub fn star_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Star) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct EnumVariantList(SyntaxNode);
impl AstNode for EnumVariantList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::EnumVariantList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::EnumVariantList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl EnumVariantList {
    pub fn enum_variant(&self) -> impl Iterator<Item = EnumVariant> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct EnumVariant(SyntaxNode);
impl AstNode for EnumVariant {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::EnumVariant => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::EnumVariant => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl EnumVariant {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RangeList(SyntaxNode);
impl AstNode for RangeList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RangeList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RangeList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RangeList {
    pub fn ranges(&self) -> impl Iterator<Item = Type> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RecordField(SyntaxNode);
impl AstNode for RecordField {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RecordField => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RecordField => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RecordField {
    pub fn field_names(&self) -> Option<RecordFieldNameList> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn field_ty(&self) -> Option<Type> { helper::node(&self.0) }
    pub fn semicolon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Semicolon) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UnionVariant(SyntaxNode);
impl AstNode for UnionVariant {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UnionVariant => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::UnionVariant => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl UnionVariant {
    pub fn label_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwLabel) }
    pub fn selectors(&self) -> Option<CompTimeExprList> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn record_field(&self) -> impl Iterator<Item = RecordField> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RecordFieldNameList(SyntaxNode);
impl AstNode for RecordFieldNameList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RecordFieldNameList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RecordFieldNameList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RecordFieldNameList {
    pub fn names(&self) -> impl Iterator<Item = RecordFieldName> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RecordFieldName(SyntaxNode);
impl AstNode for RecordFieldName {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RecordFieldName => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RecordFieldName => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl RecordFieldName {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConstVarParam(SyntaxNode);
impl AstNode for ConstVarParam {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarParam => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarParam => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConstVarParam {
    pub fn pass_as_ref(&self) -> Option<VarAttr> { helper::node(&self.0) }
    pub fn bind_to_register(&self) -> Option<RegisterAttr> { helper::node(&self.0) }
    pub fn param_names(&self) -> Option<ParamNameList> { helper::node(&self.0) }
    pub fn colon_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Colon) }
    pub fn coerce_type(&self) -> Option<CheatAttr> { helper::node(&self.0) }
    pub fn param_ty(&self) -> Option<Type> { helper::node(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ParamNameList(SyntaxNode);
impl AstNode for ParamNameList {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ParamNameList => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ParamNameList => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ParamNameList {
    pub fn names(&self) -> impl Iterator<Item = ParamName> + '_ { helper::nodes(&self.0) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ParamName(SyntaxNode);
impl AstNode for ParamName {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ParamName => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ParamName => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ParamName {
    pub fn name(&self) -> Option<Name> { helper::node(&self.0) }
    pub fn comma_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::Comma) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConditionKind(SyntaxNode);
impl AstNode for ConditionKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConditionKind => Some(Self(syntax)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConditionKind => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode { &self.0 }
}
impl ConditionKind {
    pub fn priority_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwPriority) }
    pub fn deferred_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwDeferred) }
    pub fn timeout_token(&self) -> Option<SyntaxToken> { helper::token(&self.0, SyntaxKind::KwTimeout) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum PreprocKind {
    PPInclude(PPInclude),
    PPIf(PPIf),
    PPElseif(PPElseif),
    PPElse(PPElse),
    PPEndIf(PPEndIf),
}
impl AstNode for PreprocKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPInclude => Some(Self::PPInclude(AstNode::cast(syntax)?)),
            SyntaxKind::PPIf => Some(Self::PPIf(AstNode::cast(syntax)?)),
            SyntaxKind::PPElseif => Some(Self::PPElseif(AstNode::cast(syntax)?)),
            SyntaxKind::PPElse => Some(Self::PPElse(AstNode::cast(syntax)?)),
            SyntaxKind::PPEndIf => Some(Self::PPEndIf(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPInclude => true,
            SyntaxKind::PPIf => true,
            SyntaxKind::PPElseif => true,
            SyntaxKind::PPElse => true,
            SyntaxKind::PPEndIf => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PPInclude(node) => node.syntax(),
            Self::PPIf(node) => node.syntax(),
            Self::PPElseif(node) => node.syntax(),
            Self::PPElse(node) => node.syntax(),
            Self::PPEndIf(node) => node.syntax(),
        }
    }
}
impl From<PPInclude> for PreprocKind {
    fn from(variant: PPInclude) -> Self { Self::PPInclude(variant) }
}
impl From<PPIf> for PreprocKind {
    fn from(variant: PPIf) -> Self { Self::PPIf(variant) }
}
impl From<PPElseif> for PreprocKind {
    fn from(variant: PPElseif) -> Self { Self::PPElseif(variant) }
}
impl From<PPElse> for PreprocKind {
    fn from(variant: PPElse) -> Self { Self::PPElse(variant) }
}
impl From<PPEndIf> for PreprocKind {
    fn from(variant: PPEndIf) -> Self { Self::PPEndIf(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum PPExpr {
    PPBinaryExpr(PPBinaryExpr),
    PPUnaryExpr(PPUnaryExpr),
    PPNameExpr(PPNameExpr),
    PPParenExpr(PPParenExpr),
}
impl AstNode for PPExpr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPBinaryExpr => Some(Self::PPBinaryExpr(AstNode::cast(syntax)?)),
            SyntaxKind::PPUnaryExpr => Some(Self::PPUnaryExpr(AstNode::cast(syntax)?)),
            SyntaxKind::PPNameExpr => Some(Self::PPNameExpr(AstNode::cast(syntax)?)),
            SyntaxKind::PPParenExpr => Some(Self::PPParenExpr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPBinaryExpr => true,
            SyntaxKind::PPUnaryExpr => true,
            SyntaxKind::PPNameExpr => true,
            SyntaxKind::PPParenExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PPBinaryExpr(node) => node.syntax(),
            Self::PPUnaryExpr(node) => node.syntax(),
            Self::PPNameExpr(node) => node.syntax(),
            Self::PPParenExpr(node) => node.syntax(),
        }
    }
}
impl From<PPBinaryExpr> for PPExpr {
    fn from(variant: PPBinaryExpr) -> Self { Self::PPBinaryExpr(variant) }
}
impl From<PPUnaryExpr> for PPExpr {
    fn from(variant: PPUnaryExpr) -> Self { Self::PPUnaryExpr(variant) }
}
impl From<PPNameExpr> for PPExpr {
    fn from(variant: PPNameExpr) -> Self { Self::PPNameExpr(variant) }
}
impl From<PPParenExpr> for PPExpr {
    fn from(variant: PPParenExpr) -> Self { Self::PPParenExpr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum PPFalseBranch {
    PPElseif(PPElseif),
    PPElse(PPElse),
}
impl AstNode for PPFalseBranch {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PPElseif => Some(Self::PPElseif(AstNode::cast(syntax)?)),
            SyntaxKind::PPElse => Some(Self::PPElse(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PPElseif => true,
            SyntaxKind::PPElse => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PPElseif(node) => node.syntax(),
            Self::PPElse(node) => node.syntax(),
        }
    }
}
impl From<PPElseif> for PPFalseBranch {
    fn from(variant: PPElseif) -> Self { Self::PPElseif(variant) }
}
impl From<PPElse> for PPFalseBranch {
    fn from(variant: PPElse) -> Self { Self::PPElse(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Stmt {
    ConstVarDecl(ConstVarDecl),
    TypeDecl(TypeDecl),
    BindDecl(BindDecl),
    ProcDecl(ProcDecl),
    FcnDecl(FcnDecl),
    ProcessDecl(ProcessDecl),
    ExternalDecl(ExternalDecl),
    ForwardDecl(ForwardDecl),
    DeferredDecl(DeferredDecl),
    BodyDecl(BodyDecl),
    ModuleDecl(ModuleDecl),
    ClassDecl(ClassDecl),
    MonitorDecl(MonitorDecl),
    AssignStmt(AssignStmt),
    OpenStmt(OpenStmt),
    CloseStmt(CloseStmt),
    PutStmt(PutStmt),
    GetStmt(GetStmt),
    ReadStmt(ReadStmt),
    WriteStmt(WriteStmt),
    SeekStmt(SeekStmt),
    TellStmt(TellStmt),
    ForStmt(ForStmt),
    LoopStmt(LoopStmt),
    ExitStmt(ExitStmt),
    IfStmt(IfStmt),
    CaseStmt(CaseStmt),
    BlockStmt(BlockStmt),
    InvariantStmt(InvariantStmt),
    AssertStmt(AssertStmt),
    CallStmt(CallStmt),
    ReturnStmt(ReturnStmt),
    ResultStmt(ResultStmt),
    NewStmt(NewStmt),
    FreeStmt(FreeStmt),
    TagStmt(TagStmt),
    ForkStmt(ForkStmt),
    SignalStmt(SignalStmt),
    PauseStmt(PauseStmt),
    QuitStmt(QuitStmt),
    BreakStmt(BreakStmt),
    CheckednessStmt(CheckednessStmt),
    PreStmt(PreStmt),
    InitStmt(InitStmt),
    PostStmt(PostStmt),
    HandlerStmt(HandlerStmt),
    InheritStmt(InheritStmt),
    ImplementStmt(ImplementStmt),
    ImplementByStmt(ImplementByStmt),
    ImportStmt(ImportStmt),
    ExportStmt(ExportStmt),
    PreprocGlob(PreprocGlob),
}
impl AstNode for Stmt {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarDecl => Some(Self::ConstVarDecl(AstNode::cast(syntax)?)),
            SyntaxKind::TypeDecl => Some(Self::TypeDecl(AstNode::cast(syntax)?)),
            SyntaxKind::BindDecl => Some(Self::BindDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ProcDecl => Some(Self::ProcDecl(AstNode::cast(syntax)?)),
            SyntaxKind::FcnDecl => Some(Self::FcnDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ProcessDecl => Some(Self::ProcessDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ExternalDecl => Some(Self::ExternalDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ForwardDecl => Some(Self::ForwardDecl(AstNode::cast(syntax)?)),
            SyntaxKind::DeferredDecl => Some(Self::DeferredDecl(AstNode::cast(syntax)?)),
            SyntaxKind::BodyDecl => Some(Self::BodyDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ModuleDecl => Some(Self::ModuleDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ClassDecl => Some(Self::ClassDecl(AstNode::cast(syntax)?)),
            SyntaxKind::MonitorDecl => Some(Self::MonitorDecl(AstNode::cast(syntax)?)),
            SyntaxKind::AssignStmt => Some(Self::AssignStmt(AstNode::cast(syntax)?)),
            SyntaxKind::OpenStmt => Some(Self::OpenStmt(AstNode::cast(syntax)?)),
            SyntaxKind::CloseStmt => Some(Self::CloseStmt(AstNode::cast(syntax)?)),
            SyntaxKind::PutStmt => Some(Self::PutStmt(AstNode::cast(syntax)?)),
            SyntaxKind::GetStmt => Some(Self::GetStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ReadStmt => Some(Self::ReadStmt(AstNode::cast(syntax)?)),
            SyntaxKind::WriteStmt => Some(Self::WriteStmt(AstNode::cast(syntax)?)),
            SyntaxKind::SeekStmt => Some(Self::SeekStmt(AstNode::cast(syntax)?)),
            SyntaxKind::TellStmt => Some(Self::TellStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ForStmt => Some(Self::ForStmt(AstNode::cast(syntax)?)),
            SyntaxKind::LoopStmt => Some(Self::LoopStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ExitStmt => Some(Self::ExitStmt(AstNode::cast(syntax)?)),
            SyntaxKind::IfStmt => Some(Self::IfStmt(AstNode::cast(syntax)?)),
            SyntaxKind::CaseStmt => Some(Self::CaseStmt(AstNode::cast(syntax)?)),
            SyntaxKind::BlockStmt => Some(Self::BlockStmt(AstNode::cast(syntax)?)),
            SyntaxKind::InvariantStmt => Some(Self::InvariantStmt(AstNode::cast(syntax)?)),
            SyntaxKind::AssertStmt => Some(Self::AssertStmt(AstNode::cast(syntax)?)),
            SyntaxKind::CallStmt => Some(Self::CallStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ReturnStmt => Some(Self::ReturnStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ResultStmt => Some(Self::ResultStmt(AstNode::cast(syntax)?)),
            SyntaxKind::NewStmt => Some(Self::NewStmt(AstNode::cast(syntax)?)),
            SyntaxKind::FreeStmt => Some(Self::FreeStmt(AstNode::cast(syntax)?)),
            SyntaxKind::TagStmt => Some(Self::TagStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ForkStmt => Some(Self::ForkStmt(AstNode::cast(syntax)?)),
            SyntaxKind::SignalStmt => Some(Self::SignalStmt(AstNode::cast(syntax)?)),
            SyntaxKind::PauseStmt => Some(Self::PauseStmt(AstNode::cast(syntax)?)),
            SyntaxKind::QuitStmt => Some(Self::QuitStmt(AstNode::cast(syntax)?)),
            SyntaxKind::BreakStmt => Some(Self::BreakStmt(AstNode::cast(syntax)?)),
            SyntaxKind::CheckednessStmt => Some(Self::CheckednessStmt(AstNode::cast(syntax)?)),
            SyntaxKind::PreStmt => Some(Self::PreStmt(AstNode::cast(syntax)?)),
            SyntaxKind::InitStmt => Some(Self::InitStmt(AstNode::cast(syntax)?)),
            SyntaxKind::PostStmt => Some(Self::PostStmt(AstNode::cast(syntax)?)),
            SyntaxKind::HandlerStmt => Some(Self::HandlerStmt(AstNode::cast(syntax)?)),
            SyntaxKind::InheritStmt => Some(Self::InheritStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ImplementStmt => Some(Self::ImplementStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ImplementByStmt => Some(Self::ImplementByStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ImportStmt => Some(Self::ImportStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ExportStmt => Some(Self::ExportStmt(AstNode::cast(syntax)?)),
            SyntaxKind::PreprocGlob => Some(Self::PreprocGlob(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarDecl => true,
            SyntaxKind::TypeDecl => true,
            SyntaxKind::BindDecl => true,
            SyntaxKind::ProcDecl => true,
            SyntaxKind::FcnDecl => true,
            SyntaxKind::ProcessDecl => true,
            SyntaxKind::ExternalDecl => true,
            SyntaxKind::ForwardDecl => true,
            SyntaxKind::DeferredDecl => true,
            SyntaxKind::BodyDecl => true,
            SyntaxKind::ModuleDecl => true,
            SyntaxKind::ClassDecl => true,
            SyntaxKind::MonitorDecl => true,
            SyntaxKind::AssignStmt => true,
            SyntaxKind::OpenStmt => true,
            SyntaxKind::CloseStmt => true,
            SyntaxKind::PutStmt => true,
            SyntaxKind::GetStmt => true,
            SyntaxKind::ReadStmt => true,
            SyntaxKind::WriteStmt => true,
            SyntaxKind::SeekStmt => true,
            SyntaxKind::TellStmt => true,
            SyntaxKind::ForStmt => true,
            SyntaxKind::LoopStmt => true,
            SyntaxKind::ExitStmt => true,
            SyntaxKind::IfStmt => true,
            SyntaxKind::CaseStmt => true,
            SyntaxKind::BlockStmt => true,
            SyntaxKind::InvariantStmt => true,
            SyntaxKind::AssertStmt => true,
            SyntaxKind::CallStmt => true,
            SyntaxKind::ReturnStmt => true,
            SyntaxKind::ResultStmt => true,
            SyntaxKind::NewStmt => true,
            SyntaxKind::FreeStmt => true,
            SyntaxKind::TagStmt => true,
            SyntaxKind::ForkStmt => true,
            SyntaxKind::SignalStmt => true,
            SyntaxKind::PauseStmt => true,
            SyntaxKind::QuitStmt => true,
            SyntaxKind::BreakStmt => true,
            SyntaxKind::CheckednessStmt => true,
            SyntaxKind::PreStmt => true,
            SyntaxKind::InitStmt => true,
            SyntaxKind::PostStmt => true,
            SyntaxKind::HandlerStmt => true,
            SyntaxKind::InheritStmt => true,
            SyntaxKind::ImplementStmt => true,
            SyntaxKind::ImplementByStmt => true,
            SyntaxKind::ImportStmt => true,
            SyntaxKind::ExportStmt => true,
            SyntaxKind::PreprocGlob => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ConstVarDecl(node) => node.syntax(),
            Self::TypeDecl(node) => node.syntax(),
            Self::BindDecl(node) => node.syntax(),
            Self::ProcDecl(node) => node.syntax(),
            Self::FcnDecl(node) => node.syntax(),
            Self::ProcessDecl(node) => node.syntax(),
            Self::ExternalDecl(node) => node.syntax(),
            Self::ForwardDecl(node) => node.syntax(),
            Self::DeferredDecl(node) => node.syntax(),
            Self::BodyDecl(node) => node.syntax(),
            Self::ModuleDecl(node) => node.syntax(),
            Self::ClassDecl(node) => node.syntax(),
            Self::MonitorDecl(node) => node.syntax(),
            Self::AssignStmt(node) => node.syntax(),
            Self::OpenStmt(node) => node.syntax(),
            Self::CloseStmt(node) => node.syntax(),
            Self::PutStmt(node) => node.syntax(),
            Self::GetStmt(node) => node.syntax(),
            Self::ReadStmt(node) => node.syntax(),
            Self::WriteStmt(node) => node.syntax(),
            Self::SeekStmt(node) => node.syntax(),
            Self::TellStmt(node) => node.syntax(),
            Self::ForStmt(node) => node.syntax(),
            Self::LoopStmt(node) => node.syntax(),
            Self::ExitStmt(node) => node.syntax(),
            Self::IfStmt(node) => node.syntax(),
            Self::CaseStmt(node) => node.syntax(),
            Self::BlockStmt(node) => node.syntax(),
            Self::InvariantStmt(node) => node.syntax(),
            Self::AssertStmt(node) => node.syntax(),
            Self::CallStmt(node) => node.syntax(),
            Self::ReturnStmt(node) => node.syntax(),
            Self::ResultStmt(node) => node.syntax(),
            Self::NewStmt(node) => node.syntax(),
            Self::FreeStmt(node) => node.syntax(),
            Self::TagStmt(node) => node.syntax(),
            Self::ForkStmt(node) => node.syntax(),
            Self::SignalStmt(node) => node.syntax(),
            Self::PauseStmt(node) => node.syntax(),
            Self::QuitStmt(node) => node.syntax(),
            Self::BreakStmt(node) => node.syntax(),
            Self::CheckednessStmt(node) => node.syntax(),
            Self::PreStmt(node) => node.syntax(),
            Self::InitStmt(node) => node.syntax(),
            Self::PostStmt(node) => node.syntax(),
            Self::HandlerStmt(node) => node.syntax(),
            Self::InheritStmt(node) => node.syntax(),
            Self::ImplementStmt(node) => node.syntax(),
            Self::ImplementByStmt(node) => node.syntax(),
            Self::ImportStmt(node) => node.syntax(),
            Self::ExportStmt(node) => node.syntax(),
            Self::PreprocGlob(node) => node.syntax(),
        }
    }
}
impl From<ConstVarDecl> for Stmt {
    fn from(variant: ConstVarDecl) -> Self { Self::ConstVarDecl(variant) }
}
impl From<TypeDecl> for Stmt {
    fn from(variant: TypeDecl) -> Self { Self::TypeDecl(variant) }
}
impl From<BindDecl> for Stmt {
    fn from(variant: BindDecl) -> Self { Self::BindDecl(variant) }
}
impl From<ProcDecl> for Stmt {
    fn from(variant: ProcDecl) -> Self { Self::ProcDecl(variant) }
}
impl From<FcnDecl> for Stmt {
    fn from(variant: FcnDecl) -> Self { Self::FcnDecl(variant) }
}
impl From<ProcessDecl> for Stmt {
    fn from(variant: ProcessDecl) -> Self { Self::ProcessDecl(variant) }
}
impl From<ExternalDecl> for Stmt {
    fn from(variant: ExternalDecl) -> Self { Self::ExternalDecl(variant) }
}
impl From<ForwardDecl> for Stmt {
    fn from(variant: ForwardDecl) -> Self { Self::ForwardDecl(variant) }
}
impl From<DeferredDecl> for Stmt {
    fn from(variant: DeferredDecl) -> Self { Self::DeferredDecl(variant) }
}
impl From<BodyDecl> for Stmt {
    fn from(variant: BodyDecl) -> Self { Self::BodyDecl(variant) }
}
impl From<ModuleDecl> for Stmt {
    fn from(variant: ModuleDecl) -> Self { Self::ModuleDecl(variant) }
}
impl From<ClassDecl> for Stmt {
    fn from(variant: ClassDecl) -> Self { Self::ClassDecl(variant) }
}
impl From<MonitorDecl> for Stmt {
    fn from(variant: MonitorDecl) -> Self { Self::MonitorDecl(variant) }
}
impl From<AssignStmt> for Stmt {
    fn from(variant: AssignStmt) -> Self { Self::AssignStmt(variant) }
}
impl From<OpenStmt> for Stmt {
    fn from(variant: OpenStmt) -> Self { Self::OpenStmt(variant) }
}
impl From<CloseStmt> for Stmt {
    fn from(variant: CloseStmt) -> Self { Self::CloseStmt(variant) }
}
impl From<PutStmt> for Stmt {
    fn from(variant: PutStmt) -> Self { Self::PutStmt(variant) }
}
impl From<GetStmt> for Stmt {
    fn from(variant: GetStmt) -> Self { Self::GetStmt(variant) }
}
impl From<ReadStmt> for Stmt {
    fn from(variant: ReadStmt) -> Self { Self::ReadStmt(variant) }
}
impl From<WriteStmt> for Stmt {
    fn from(variant: WriteStmt) -> Self { Self::WriteStmt(variant) }
}
impl From<SeekStmt> for Stmt {
    fn from(variant: SeekStmt) -> Self { Self::SeekStmt(variant) }
}
impl From<TellStmt> for Stmt {
    fn from(variant: TellStmt) -> Self { Self::TellStmt(variant) }
}
impl From<ForStmt> for Stmt {
    fn from(variant: ForStmt) -> Self { Self::ForStmt(variant) }
}
impl From<LoopStmt> for Stmt {
    fn from(variant: LoopStmt) -> Self { Self::LoopStmt(variant) }
}
impl From<ExitStmt> for Stmt {
    fn from(variant: ExitStmt) -> Self { Self::ExitStmt(variant) }
}
impl From<IfStmt> for Stmt {
    fn from(variant: IfStmt) -> Self { Self::IfStmt(variant) }
}
impl From<CaseStmt> for Stmt {
    fn from(variant: CaseStmt) -> Self { Self::CaseStmt(variant) }
}
impl From<BlockStmt> for Stmt {
    fn from(variant: BlockStmt) -> Self { Self::BlockStmt(variant) }
}
impl From<InvariantStmt> for Stmt {
    fn from(variant: InvariantStmt) -> Self { Self::InvariantStmt(variant) }
}
impl From<AssertStmt> for Stmt {
    fn from(variant: AssertStmt) -> Self { Self::AssertStmt(variant) }
}
impl From<CallStmt> for Stmt {
    fn from(variant: CallStmt) -> Self { Self::CallStmt(variant) }
}
impl From<ReturnStmt> for Stmt {
    fn from(variant: ReturnStmt) -> Self { Self::ReturnStmt(variant) }
}
impl From<ResultStmt> for Stmt {
    fn from(variant: ResultStmt) -> Self { Self::ResultStmt(variant) }
}
impl From<NewStmt> for Stmt {
    fn from(variant: NewStmt) -> Self { Self::NewStmt(variant) }
}
impl From<FreeStmt> for Stmt {
    fn from(variant: FreeStmt) -> Self { Self::FreeStmt(variant) }
}
impl From<TagStmt> for Stmt {
    fn from(variant: TagStmt) -> Self { Self::TagStmt(variant) }
}
impl From<ForkStmt> for Stmt {
    fn from(variant: ForkStmt) -> Self { Self::ForkStmt(variant) }
}
impl From<SignalStmt> for Stmt {
    fn from(variant: SignalStmt) -> Self { Self::SignalStmt(variant) }
}
impl From<PauseStmt> for Stmt {
    fn from(variant: PauseStmt) -> Self { Self::PauseStmt(variant) }
}
impl From<QuitStmt> for Stmt {
    fn from(variant: QuitStmt) -> Self { Self::QuitStmt(variant) }
}
impl From<BreakStmt> for Stmt {
    fn from(variant: BreakStmt) -> Self { Self::BreakStmt(variant) }
}
impl From<CheckednessStmt> for Stmt {
    fn from(variant: CheckednessStmt) -> Self { Self::CheckednessStmt(variant) }
}
impl From<PreStmt> for Stmt {
    fn from(variant: PreStmt) -> Self { Self::PreStmt(variant) }
}
impl From<InitStmt> for Stmt {
    fn from(variant: InitStmt) -> Self { Self::InitStmt(variant) }
}
impl From<PostStmt> for Stmt {
    fn from(variant: PostStmt) -> Self { Self::PostStmt(variant) }
}
impl From<HandlerStmt> for Stmt {
    fn from(variant: HandlerStmt) -> Self { Self::HandlerStmt(variant) }
}
impl From<InheritStmt> for Stmt {
    fn from(variant: InheritStmt) -> Self { Self::InheritStmt(variant) }
}
impl From<ImplementStmt> for Stmt {
    fn from(variant: ImplementStmt) -> Self { Self::ImplementStmt(variant) }
}
impl From<ImplementByStmt> for Stmt {
    fn from(variant: ImplementByStmt) -> Self { Self::ImplementByStmt(variant) }
}
impl From<ImportStmt> for Stmt {
    fn from(variant: ImportStmt) -> Self { Self::ImportStmt(variant) }
}
impl From<ExportStmt> for Stmt {
    fn from(variant: ExportStmt) -> Self { Self::ExportStmt(variant) }
}
impl From<PreprocGlob> for Stmt {
    fn from(variant: PreprocGlob) -> Self { Self::PreprocGlob(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Item {
    ConstVarDeclName(ConstVarDeclName),
    TypeDecl(TypeDecl),
    BindItem(BindItem),
    ProcDecl(ProcDecl),
    FcnDecl(FcnDecl),
    ProcessDecl(ProcessDecl),
    ExternalDecl(ExternalDecl),
    ForwardDecl(ForwardDecl),
    DeferredDecl(DeferredDecl),
    BodyDecl(BodyDecl),
    ModuleDecl(ModuleDecl),
    ClassDecl(ClassDecl),
    MonitorDecl(MonitorDecl),
}
impl AstNode for Item {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarDeclName => Some(Self::ConstVarDeclName(AstNode::cast(syntax)?)),
            SyntaxKind::TypeDecl => Some(Self::TypeDecl(AstNode::cast(syntax)?)),
            SyntaxKind::BindItem => Some(Self::BindItem(AstNode::cast(syntax)?)),
            SyntaxKind::ProcDecl => Some(Self::ProcDecl(AstNode::cast(syntax)?)),
            SyntaxKind::FcnDecl => Some(Self::FcnDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ProcessDecl => Some(Self::ProcessDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ExternalDecl => Some(Self::ExternalDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ForwardDecl => Some(Self::ForwardDecl(AstNode::cast(syntax)?)),
            SyntaxKind::DeferredDecl => Some(Self::DeferredDecl(AstNode::cast(syntax)?)),
            SyntaxKind::BodyDecl => Some(Self::BodyDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ModuleDecl => Some(Self::ModuleDecl(AstNode::cast(syntax)?)),
            SyntaxKind::ClassDecl => Some(Self::ClassDecl(AstNode::cast(syntax)?)),
            SyntaxKind::MonitorDecl => Some(Self::MonitorDecl(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarDeclName => true,
            SyntaxKind::TypeDecl => true,
            SyntaxKind::BindItem => true,
            SyntaxKind::ProcDecl => true,
            SyntaxKind::FcnDecl => true,
            SyntaxKind::ProcessDecl => true,
            SyntaxKind::ExternalDecl => true,
            SyntaxKind::ForwardDecl => true,
            SyntaxKind::DeferredDecl => true,
            SyntaxKind::BodyDecl => true,
            SyntaxKind::ModuleDecl => true,
            SyntaxKind::ClassDecl => true,
            SyntaxKind::MonitorDecl => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ConstVarDeclName(node) => node.syntax(),
            Self::TypeDecl(node) => node.syntax(),
            Self::BindItem(node) => node.syntax(),
            Self::ProcDecl(node) => node.syntax(),
            Self::FcnDecl(node) => node.syntax(),
            Self::ProcessDecl(node) => node.syntax(),
            Self::ExternalDecl(node) => node.syntax(),
            Self::ForwardDecl(node) => node.syntax(),
            Self::DeferredDecl(node) => node.syntax(),
            Self::BodyDecl(node) => node.syntax(),
            Self::ModuleDecl(node) => node.syntax(),
            Self::ClassDecl(node) => node.syntax(),
            Self::MonitorDecl(node) => node.syntax(),
        }
    }
}
impl From<ConstVarDeclName> for Item {
    fn from(variant: ConstVarDeclName) -> Self { Self::ConstVarDeclName(variant) }
}
impl From<TypeDecl> for Item {
    fn from(variant: TypeDecl) -> Self { Self::TypeDecl(variant) }
}
impl From<BindItem> for Item {
    fn from(variant: BindItem) -> Self { Self::BindItem(variant) }
}
impl From<ProcDecl> for Item {
    fn from(variant: ProcDecl) -> Self { Self::ProcDecl(variant) }
}
impl From<FcnDecl> for Item {
    fn from(variant: FcnDecl) -> Self { Self::FcnDecl(variant) }
}
impl From<ProcessDecl> for Item {
    fn from(variant: ProcessDecl) -> Self { Self::ProcessDecl(variant) }
}
impl From<ExternalDecl> for Item {
    fn from(variant: ExternalDecl) -> Self { Self::ExternalDecl(variant) }
}
impl From<ForwardDecl> for Item {
    fn from(variant: ForwardDecl) -> Self { Self::ForwardDecl(variant) }
}
impl From<DeferredDecl> for Item {
    fn from(variant: DeferredDecl) -> Self { Self::DeferredDecl(variant) }
}
impl From<BodyDecl> for Item {
    fn from(variant: BodyDecl) -> Self { Self::BodyDecl(variant) }
}
impl From<ModuleDecl> for Item {
    fn from(variant: ModuleDecl) -> Self { Self::ModuleDecl(variant) }
}
impl From<ClassDecl> for Item {
    fn from(variant: ClassDecl) -> Self { Self::ClassDecl(variant) }
}
impl From<MonitorDecl> for Item {
    fn from(variant: MonitorDecl) -> Self { Self::MonitorDecl(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    PrimType(PrimType),
    NameType(NameType),
    RangeType(RangeType),
    EnumType(EnumType),
    ArrayType(ArrayType),
    SetType(SetType),
    RecordType(RecordType),
    UnionType(UnionType),
    PointerType(PointerType),
    FcnType(FcnType),
    ProcType(ProcType),
    CollectionType(CollectionType),
    ConditionType(ConditionType),
}
impl AstNode for Type {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PrimType => Some(Self::PrimType(AstNode::cast(syntax)?)),
            SyntaxKind::NameType => Some(Self::NameType(AstNode::cast(syntax)?)),
            SyntaxKind::RangeType => Some(Self::RangeType(AstNode::cast(syntax)?)),
            SyntaxKind::EnumType => Some(Self::EnumType(AstNode::cast(syntax)?)),
            SyntaxKind::ArrayType => Some(Self::ArrayType(AstNode::cast(syntax)?)),
            SyntaxKind::SetType => Some(Self::SetType(AstNode::cast(syntax)?)),
            SyntaxKind::RecordType => Some(Self::RecordType(AstNode::cast(syntax)?)),
            SyntaxKind::UnionType => Some(Self::UnionType(AstNode::cast(syntax)?)),
            SyntaxKind::PointerType => Some(Self::PointerType(AstNode::cast(syntax)?)),
            SyntaxKind::FcnType => Some(Self::FcnType(AstNode::cast(syntax)?)),
            SyntaxKind::ProcType => Some(Self::ProcType(AstNode::cast(syntax)?)),
            SyntaxKind::CollectionType => Some(Self::CollectionType(AstNode::cast(syntax)?)),
            SyntaxKind::ConditionType => Some(Self::ConditionType(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PrimType => true,
            SyntaxKind::NameType => true,
            SyntaxKind::RangeType => true,
            SyntaxKind::EnumType => true,
            SyntaxKind::ArrayType => true,
            SyntaxKind::SetType => true,
            SyntaxKind::RecordType => true,
            SyntaxKind::UnionType => true,
            SyntaxKind::PointerType => true,
            SyntaxKind::FcnType => true,
            SyntaxKind::ProcType => true,
            SyntaxKind::CollectionType => true,
            SyntaxKind::ConditionType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PrimType(node) => node.syntax(),
            Self::NameType(node) => node.syntax(),
            Self::RangeType(node) => node.syntax(),
            Self::EnumType(node) => node.syntax(),
            Self::ArrayType(node) => node.syntax(),
            Self::SetType(node) => node.syntax(),
            Self::RecordType(node) => node.syntax(),
            Self::UnionType(node) => node.syntax(),
            Self::PointerType(node) => node.syntax(),
            Self::FcnType(node) => node.syntax(),
            Self::ProcType(node) => node.syntax(),
            Self::CollectionType(node) => node.syntax(),
            Self::ConditionType(node) => node.syntax(),
        }
    }
}
impl From<PrimType> for Type {
    fn from(variant: PrimType) -> Self { Self::PrimType(variant) }
}
impl From<NameType> for Type {
    fn from(variant: NameType) -> Self { Self::NameType(variant) }
}
impl From<RangeType> for Type {
    fn from(variant: RangeType) -> Self { Self::RangeType(variant) }
}
impl From<EnumType> for Type {
    fn from(variant: EnumType) -> Self { Self::EnumType(variant) }
}
impl From<ArrayType> for Type {
    fn from(variant: ArrayType) -> Self { Self::ArrayType(variant) }
}
impl From<SetType> for Type {
    fn from(variant: SetType) -> Self { Self::SetType(variant) }
}
impl From<RecordType> for Type {
    fn from(variant: RecordType) -> Self { Self::RecordType(variant) }
}
impl From<UnionType> for Type {
    fn from(variant: UnionType) -> Self { Self::UnionType(variant) }
}
impl From<PointerType> for Type {
    fn from(variant: PointerType) -> Self { Self::PointerType(variant) }
}
impl From<FcnType> for Type {
    fn from(variant: FcnType) -> Self { Self::FcnType(variant) }
}
impl From<ProcType> for Type {
    fn from(variant: ProcType) -> Self { Self::ProcType(variant) }
}
impl From<CollectionType> for Type {
    fn from(variant: CollectionType) -> Self { Self::CollectionType(variant) }
}
impl From<ConditionType> for Type {
    fn from(variant: ConditionType) -> Self { Self::ConditionType(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    LiteralExpr(LiteralExpr),
    ObjClassExpr(ObjClassExpr),
    InitExpr(InitExpr),
    NilExpr(NilExpr),
    SizeOfExpr(SizeOfExpr),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    ParenExpr(ParenExpr),
    NameExpr(NameExpr),
    SelfExpr(SelfExpr),
    FieldExpr(FieldExpr),
    DerefExpr(DerefExpr),
    CheatExpr(CheatExpr),
    NatCheatExpr(NatCheatExpr),
    ArrowExpr(ArrowExpr),
    IndirectExpr(IndirectExpr),
    BitsExpr(BitsExpr),
    CallExpr(CallExpr),
}
impl AstNode for Expr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LiteralExpr => Some(Self::LiteralExpr(AstNode::cast(syntax)?)),
            SyntaxKind::ObjClassExpr => Some(Self::ObjClassExpr(AstNode::cast(syntax)?)),
            SyntaxKind::InitExpr => Some(Self::InitExpr(AstNode::cast(syntax)?)),
            SyntaxKind::NilExpr => Some(Self::NilExpr(AstNode::cast(syntax)?)),
            SyntaxKind::SizeOfExpr => Some(Self::SizeOfExpr(AstNode::cast(syntax)?)),
            SyntaxKind::BinaryExpr => Some(Self::BinaryExpr(AstNode::cast(syntax)?)),
            SyntaxKind::UnaryExpr => Some(Self::UnaryExpr(AstNode::cast(syntax)?)),
            SyntaxKind::ParenExpr => Some(Self::ParenExpr(AstNode::cast(syntax)?)),
            SyntaxKind::NameExpr => Some(Self::NameExpr(AstNode::cast(syntax)?)),
            SyntaxKind::SelfExpr => Some(Self::SelfExpr(AstNode::cast(syntax)?)),
            SyntaxKind::FieldExpr => Some(Self::FieldExpr(AstNode::cast(syntax)?)),
            SyntaxKind::DerefExpr => Some(Self::DerefExpr(AstNode::cast(syntax)?)),
            SyntaxKind::CheatExpr => Some(Self::CheatExpr(AstNode::cast(syntax)?)),
            SyntaxKind::NatCheatExpr => Some(Self::NatCheatExpr(AstNode::cast(syntax)?)),
            SyntaxKind::ArrowExpr => Some(Self::ArrowExpr(AstNode::cast(syntax)?)),
            SyntaxKind::IndirectExpr => Some(Self::IndirectExpr(AstNode::cast(syntax)?)),
            SyntaxKind::BitsExpr => Some(Self::BitsExpr(AstNode::cast(syntax)?)),
            SyntaxKind::CallExpr => Some(Self::CallExpr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::LiteralExpr => true,
            SyntaxKind::ObjClassExpr => true,
            SyntaxKind::InitExpr => true,
            SyntaxKind::NilExpr => true,
            SyntaxKind::SizeOfExpr => true,
            SyntaxKind::BinaryExpr => true,
            SyntaxKind::UnaryExpr => true,
            SyntaxKind::ParenExpr => true,
            SyntaxKind::NameExpr => true,
            SyntaxKind::SelfExpr => true,
            SyntaxKind::FieldExpr => true,
            SyntaxKind::DerefExpr => true,
            SyntaxKind::CheatExpr => true,
            SyntaxKind::NatCheatExpr => true,
            SyntaxKind::ArrowExpr => true,
            SyntaxKind::IndirectExpr => true,
            SyntaxKind::BitsExpr => true,
            SyntaxKind::CallExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::LiteralExpr(node) => node.syntax(),
            Self::ObjClassExpr(node) => node.syntax(),
            Self::InitExpr(node) => node.syntax(),
            Self::NilExpr(node) => node.syntax(),
            Self::SizeOfExpr(node) => node.syntax(),
            Self::BinaryExpr(node) => node.syntax(),
            Self::UnaryExpr(node) => node.syntax(),
            Self::ParenExpr(node) => node.syntax(),
            Self::NameExpr(node) => node.syntax(),
            Self::SelfExpr(node) => node.syntax(),
            Self::FieldExpr(node) => node.syntax(),
            Self::DerefExpr(node) => node.syntax(),
            Self::CheatExpr(node) => node.syntax(),
            Self::NatCheatExpr(node) => node.syntax(),
            Self::ArrowExpr(node) => node.syntax(),
            Self::IndirectExpr(node) => node.syntax(),
            Self::BitsExpr(node) => node.syntax(),
            Self::CallExpr(node) => node.syntax(),
        }
    }
}
impl From<LiteralExpr> for Expr {
    fn from(variant: LiteralExpr) -> Self { Self::LiteralExpr(variant) }
}
impl From<ObjClassExpr> for Expr {
    fn from(variant: ObjClassExpr) -> Self { Self::ObjClassExpr(variant) }
}
impl From<InitExpr> for Expr {
    fn from(variant: InitExpr) -> Self { Self::InitExpr(variant) }
}
impl From<NilExpr> for Expr {
    fn from(variant: NilExpr) -> Self { Self::NilExpr(variant) }
}
impl From<SizeOfExpr> for Expr {
    fn from(variant: SizeOfExpr) -> Self { Self::SizeOfExpr(variant) }
}
impl From<BinaryExpr> for Expr {
    fn from(variant: BinaryExpr) -> Self { Self::BinaryExpr(variant) }
}
impl From<UnaryExpr> for Expr {
    fn from(variant: UnaryExpr) -> Self { Self::UnaryExpr(variant) }
}
impl From<ParenExpr> for Expr {
    fn from(variant: ParenExpr) -> Self { Self::ParenExpr(variant) }
}
impl From<NameExpr> for Expr {
    fn from(variant: NameExpr) -> Self { Self::NameExpr(variant) }
}
impl From<SelfExpr> for Expr {
    fn from(variant: SelfExpr) -> Self { Self::SelfExpr(variant) }
}
impl From<FieldExpr> for Expr {
    fn from(variant: FieldExpr) -> Self { Self::FieldExpr(variant) }
}
impl From<DerefExpr> for Expr {
    fn from(variant: DerefExpr) -> Self { Self::DerefExpr(variant) }
}
impl From<CheatExpr> for Expr {
    fn from(variant: CheatExpr) -> Self { Self::CheatExpr(variant) }
}
impl From<NatCheatExpr> for Expr {
    fn from(variant: NatCheatExpr) -> Self { Self::NatCheatExpr(variant) }
}
impl From<ArrowExpr> for Expr {
    fn from(variant: ArrowExpr) -> Self { Self::ArrowExpr(variant) }
}
impl From<IndirectExpr> for Expr {
    fn from(variant: IndirectExpr) -> Self { Self::IndirectExpr(variant) }
}
impl From<BitsExpr> for Expr {
    fn from(variant: BitsExpr) -> Self { Self::BitsExpr(variant) }
}
impl From<CallExpr> for Expr {
    fn from(variant: CallExpr) -> Self { Self::CallExpr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SubprogHeader {
    ProcHeader(ProcHeader),
    FcnHeader(FcnHeader),
}
impl AstNode for SubprogHeader {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ProcHeader => Some(Self::ProcHeader(AstNode::cast(syntax)?)),
            SyntaxKind::FcnHeader => Some(Self::FcnHeader(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ProcHeader => true,
            SyntaxKind::FcnHeader => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ProcHeader(node) => node.syntax(),
            Self::FcnHeader(node) => node.syntax(),
        }
    }
}
impl From<ProcHeader> for SubprogHeader {
    fn from(variant: ProcHeader) -> Self { Self::ProcHeader(variant) }
}
impl From<FcnHeader> for SubprogHeader {
    fn from(variant: FcnHeader) -> Self { Self::FcnHeader(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BodyKind {
    PlainHeader(PlainHeader),
    ProcHeader(ProcHeader),
    FcnHeader(FcnHeader),
}
impl AstNode for BodyKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PlainHeader => Some(Self::PlainHeader(AstNode::cast(syntax)?)),
            SyntaxKind::ProcHeader => Some(Self::ProcHeader(AstNode::cast(syntax)?)),
            SyntaxKind::FcnHeader => Some(Self::FcnHeader(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PlainHeader => true,
            SyntaxKind::ProcHeader => true,
            SyntaxKind::FcnHeader => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PlainHeader(node) => node.syntax(),
            Self::ProcHeader(node) => node.syntax(),
            Self::FcnHeader(node) => node.syntax(),
        }
    }
}
impl From<PlainHeader> for BodyKind {
    fn from(variant: PlainHeader) -> Self { Self::PlainHeader(variant) }
}
impl From<ProcHeader> for BodyKind {
    fn from(variant: ProcHeader) -> Self { Self::ProcHeader(variant) }
}
impl From<FcnHeader> for BodyKind {
    fn from(variant: FcnHeader) -> Self { Self::FcnHeader(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExternalKind {
    ExternalFcn(FcnHeader),
    ExternalProc(ProcHeader),
    ExternalVar(ExternalVar),
}
impl AstNode for ExternalKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FcnHeader => Some(Self::ExternalFcn(AstNode::cast(syntax)?)),
            SyntaxKind::ProcHeader => Some(Self::ExternalProc(AstNode::cast(syntax)?)),
            SyntaxKind::ExternalVar => Some(Self::ExternalVar(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FcnHeader => true,
            SyntaxKind::ProcHeader => true,
            SyntaxKind::ExternalVar => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ExternalFcn(node) => node.syntax(),
            Self::ExternalProc(node) => node.syntax(),
            Self::ExternalVar(node) => node.syntax(),
        }
    }
}
impl From<FcnHeader> for ExternalKind {
    fn from(variant: FcnHeader) -> Self { Self::ExternalFcn(variant) }
}
impl From<ProcHeader> for ExternalKind {
    fn from(variant: ProcHeader) -> Self { Self::ExternalProc(variant) }
}
impl From<ExternalVar> for ExternalKind {
    fn from(variant: ExternalVar) -> Self { Self::ExternalVar(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum OpenKind {
    OldOpen(OldOpen),
    NewOpen(NewOpen),
}
impl AstNode for OpenKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OldOpen => Some(Self::OldOpen(AstNode::cast(syntax)?)),
            SyntaxKind::NewOpen => Some(Self::NewOpen(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OldOpen => true,
            SyntaxKind::NewOpen => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::OldOpen(node) => node.syntax(),
            Self::NewOpen(node) => node.syntax(),
        }
    }
}
impl From<OldOpen> for OpenKind {
    fn from(variant: OldOpen) -> Self { Self::OldOpen(variant) }
}
impl From<NewOpen> for OpenKind {
    fn from(variant: NewOpen) -> Self { Self::NewOpen(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum CloseKind {
    OldClose(OldClose),
    NewClose(NewClose),
}
impl AstNode for CloseKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::OldClose => Some(Self::OldClose(AstNode::cast(syntax)?)),
            SyntaxKind::NewClose => Some(Self::NewClose(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::OldClose => true,
            SyntaxKind::NewClose => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::OldClose(node) => node.syntax(),
            Self::NewClose(node) => node.syntax(),
        }
    }
}
impl From<OldClose> for CloseKind {
    fn from(variant: OldClose) -> Self { Self::OldClose(variant) }
}
impl From<NewClose> for CloseKind {
    fn from(variant: NewClose) -> Self { Self::NewClose(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum FalseBranch {
    ElseifStmt(ElseifStmt),
    ElseStmt(ElseStmt),
}
impl AstNode for FalseBranch {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ElseifStmt => Some(Self::ElseifStmt(AstNode::cast(syntax)?)),
            SyntaxKind::ElseStmt => Some(Self::ElseStmt(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ElseifStmt => true,
            SyntaxKind::ElseStmt => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ElseifStmt(node) => node.syntax(),
            Self::ElseStmt(node) => node.syntax(),
        }
    }
}
impl From<ElseifStmt> for FalseBranch {
    fn from(variant: ElseifStmt) -> Self { Self::ElseifStmt(variant) }
}
impl From<ElseStmt> for FalseBranch {
    fn from(variant: ElseStmt) -> Self { Self::ElseStmt(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ImportAttr {
    VarAttr(VarAttr),
    ConstAttr(ConstAttr),
    ForwardAttr(ForwardAttr),
}
impl AstNode for ImportAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::VarAttr => Some(Self::VarAttr(AstNode::cast(syntax)?)),
            SyntaxKind::ConstAttr => Some(Self::ConstAttr(AstNode::cast(syntax)?)),
            SyntaxKind::ForwardAttr => Some(Self::ForwardAttr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::VarAttr => true,
            SyntaxKind::ConstAttr => true,
            SyntaxKind::ForwardAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::VarAttr(node) => node.syntax(),
            Self::ConstAttr(node) => node.syntax(),
            Self::ForwardAttr(node) => node.syntax(),
        }
    }
}
impl From<VarAttr> for ImportAttr {
    fn from(variant: VarAttr) -> Self { Self::VarAttr(variant) }
}
impl From<ConstAttr> for ImportAttr {
    fn from(variant: ConstAttr) -> Self { Self::ConstAttr(variant) }
}
impl From<ForwardAttr> for ImportAttr {
    fn from(variant: ForwardAttr) -> Self { Self::ForwardAttr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExportAttr {
    VarAttr(VarAttr),
    UnqualifiedAttr(UnqualifiedAttr),
    PervasiveAttr(PervasiveAttr),
    OpaqueAttr(OpaqueAttr),
}
impl AstNode for ExportAttr {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::VarAttr => Some(Self::VarAttr(AstNode::cast(syntax)?)),
            SyntaxKind::UnqualifiedAttr => Some(Self::UnqualifiedAttr(AstNode::cast(syntax)?)),
            SyntaxKind::PervasiveAttr => Some(Self::PervasiveAttr(AstNode::cast(syntax)?)),
            SyntaxKind::OpaqueAttr => Some(Self::OpaqueAttr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::VarAttr => true,
            SyntaxKind::UnqualifiedAttr => true,
            SyntaxKind::PervasiveAttr => true,
            SyntaxKind::OpaqueAttr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::VarAttr(node) => node.syntax(),
            Self::UnqualifiedAttr(node) => node.syntax(),
            Self::PervasiveAttr(node) => node.syntax(),
            Self::OpaqueAttr(node) => node.syntax(),
        }
    }
}
impl From<VarAttr> for ExportAttr {
    fn from(variant: VarAttr) -> Self { Self::VarAttr(variant) }
}
impl From<UnqualifiedAttr> for ExportAttr {
    fn from(variant: UnqualifiedAttr) -> Self { Self::UnqualifiedAttr(variant) }
}
impl From<PervasiveAttr> for ExportAttr {
    fn from(variant: PervasiveAttr) -> Self { Self::PervasiveAttr(variant) }
}
impl From<OpaqueAttr> for ExportAttr {
    fn from(variant: OpaqueAttr) -> Self { Self::OpaqueAttr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum IndirectTy {
    PrimType(PrimType),
    NameType(NameType),
}
impl AstNode for IndirectTy {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PrimType => Some(Self::PrimType(AstNode::cast(syntax)?)),
            SyntaxKind::NameType => Some(Self::NameType(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::PrimType => true,
            SyntaxKind::NameType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PrimType(node) => node.syntax(),
            Self::NameType(node) => node.syntax(),
        }
    }
}
impl From<PrimType> for IndirectTy {
    fn from(variant: PrimType) -> Self { Self::PrimType(variant) }
}
impl From<NameType> for IndirectTy {
    fn from(variant: NameType) -> Self { Self::NameType(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ParamKind {
    AllItem(AllItem),
    RangeItem(RangeItem),
    Expr(Expr),
}
impl AstNode for ParamKind {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::AllItem => Some(Self::AllItem(AstNode::cast(syntax)?)),
            SyntaxKind::RangeItem => Some(Self::RangeItem(AstNode::cast(syntax)?)),
            _ if Expr::can_cast(syntax.kind()) => Some(Self::Expr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::AllItem => true,
            SyntaxKind::RangeItem => true,
            _ if Expr::can_cast(kind) => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::AllItem(node) => node.syntax(),
            Self::RangeItem(node) => node.syntax(),
            Self::Expr(node) => node.syntax(),
        }
    }
}
impl From<AllItem> for ParamKind {
    fn from(variant: AllItem) -> Self { Self::AllItem(variant) }
}
impl From<RangeItem> for ParamKind {
    fn from(variant: RangeItem) -> Self { Self::RangeItem(variant) }
}
impl From<Expr> for ParamKind {
    fn from(variant: Expr) -> Self { Self::Expr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RangeBound {
    RelativeBound(RelativeBound),
    Expr(Expr),
}
impl AstNode for RangeBound {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RelativeBound => Some(Self::RelativeBound(AstNode::cast(syntax)?)),
            _ if Expr::can_cast(syntax.kind()) => Some(Self::Expr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::RelativeBound => true,
            _ if Expr::can_cast(kind) => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::RelativeBound(node) => node.syntax(),
            Self::Expr(node) => node.syntax(),
        }
    }
}
impl From<RelativeBound> for RangeBound {
    fn from(variant: RelativeBound) -> Self { Self::RelativeBound(variant) }
}
impl From<Expr> for RangeBound {
    fn from(variant: Expr) -> Self { Self::Expr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum EndBound {
    UnsizedBound(UnsizedBound),
    CompTimeExpr(CompTimeExpr),
}
impl AstNode for EndBound {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UnsizedBound => Some(Self::UnsizedBound(AstNode::cast(syntax)?)),
            SyntaxKind::CompTimeExpr => Some(Self::CompTimeExpr(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::UnsizedBound => true,
            SyntaxKind::CompTimeExpr => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::UnsizedBound(node) => node.syntax(),
            Self::CompTimeExpr(node) => node.syntax(),
        }
    }
}
impl From<UnsizedBound> for EndBound {
    fn from(variant: UnsizedBound) -> Self { Self::UnsizedBound(variant) }
}
impl From<CompTimeExpr> for EndBound {
    fn from(variant: CompTimeExpr) -> Self { Self::CompTimeExpr(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SubprogType {
    FcnType(FcnType),
    ProcType(ProcType),
}
impl AstNode for SubprogType {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FcnType => Some(Self::FcnType(AstNode::cast(syntax)?)),
            SyntaxKind::ProcType => Some(Self::ProcType(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::FcnType => true,
            SyntaxKind::ProcType => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::FcnType(node) => node.syntax(),
            Self::ProcType(node) => node.syntax(),
        }
    }
}
impl From<FcnType> for SubprogType {
    fn from(variant: FcnType) -> Self { Self::FcnType(variant) }
}
impl From<ProcType> for SubprogType {
    fn from(variant: ProcType) -> Self { Self::ProcType(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ParamDecl {
    ConstVarParam(ConstVarParam),
    SubprogType(SubprogType),
}
impl AstNode for ParamDecl {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ConstVarParam => Some(Self::ConstVarParam(AstNode::cast(syntax)?)),
            _ if SubprogType::can_cast(syntax.kind()) => Some(Self::SubprogType(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ConstVarParam => true,
            _ if SubprogType::can_cast(kind) => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ConstVarParam(node) => node.syntax(),
            Self::SubprogType(node) => node.syntax(),
        }
    }
}
impl From<ConstVarParam> for ParamDecl {
    fn from(variant: ConstVarParam) -> Self { Self::ConstVarParam(variant) }
}
impl From<SubprogType> for ParamDecl {
    fn from(variant: SubprogType) -> Self { Self::SubprogType(variant) }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExternalRef {
    ExternalItem(ExternalItem),
    PPInclude(PPInclude),
}
impl AstNode for ExternalRef {
    type Language = crate::Lang;
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ExternalItem => Some(Self::ExternalItem(AstNode::cast(syntax)?)),
            SyntaxKind::PPInclude => Some(Self::PPInclude(AstNode::cast(syntax)?)),
            _ => None,
        }
    }
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            SyntaxKind::ExternalItem => true,
            SyntaxKind::PPInclude => true,
            _ => false,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ExternalItem(node) => node.syntax(),
            Self::PPInclude(node) => node.syntax(),
        }
    }
}
impl From<ExternalItem> for ExternalRef {
    fn from(variant: ExternalItem) -> Self { Self::ExternalItem(variant) }
}
impl From<PPInclude> for ExternalRef {
    fn from(variant: PPInclude) -> Self { Self::PPInclude(variant) }
}
