//! Crate for lowering the CST into a HIR node tree

// ???: Who handles preprocessor statement expansion?
// HIR lowering should handle preprocessor expansion, but requires a file database
// mapping paths to parsed CST's
// HIR lowering should also evaluate preprocessor expressions, provided a set of
// predefined identifiers

// Root file:
// - Parse CST
// - Gather dependencies
// - Request dependencies from file DB
//   - May load them from disk, or fetch from loaded cache
// ----
// Once all deps are parsed, then lower into HIR for all of them
//
// FileDB should only care about giving unique file handes corresponding to text sources.
// Other DBs will deal with what they map to (e.g. a separate DB managing all of the CSTs)

use toc_hir::{expr, stmt, ty, Database, Unit};
use toc_reporting::{MessageSink, ReportMessage};
use toc_syntax::{
    ast::{self, AstNode},
    SyntaxNode,
};

#[cfg(test)]
mod test;

pub struct HirLowerResult {
    pub database: Database,
    pub unit: Unit,
    messages: Vec<ReportMessage>,
}

impl HirLowerResult {
    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }
}

pub fn lower_ast(root_node: SyntaxNode) -> HirLowerResult {
    let mut ctx = LoweringCtx::new();
    let root = ast::Source::cast(root_node).unwrap();

    let unit = ctx.lower_root(root);

    let LoweringCtx { database, messages } = ctx;
    let messages = messages.finish();
    HirLowerResult {
        database,
        unit,
        messages,
    }
}

struct LoweringCtx {
    database: Database,
    messages: MessageSink,
}

impl LoweringCtx {
    fn new() -> Self {
        Self {
            database: Database::new(),
            messages: MessageSink::new(),
        }
    }

    fn lower_root(&mut self, root: ast::Source) -> toc_hir::Unit {
        // TODO: deal with root import statement
        let _is_child_unit = root.unit_token().is_some();

        let stmts = if let Some(stmts) = root.stmt_list() {
            stmts
                .stmts()
                .filter_map(|stmt| {
                    self.lower_stmt(stmt)
                        .map(|(node, span)| self.database.stmt_nodes.alloc_spanned(node, span))
                })
                .collect()
        } else {
            vec![]
        };

        toc_hir::Unit { stmts }
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> Option<(stmt::Stmt, toc_reporting::TextRange)> {
        let span = stmt.syntax().text_range();

        match stmt {
            ast::Stmt::ConstVarDecl(decl) => {
                let is_pervasive = decl.pervasive_attr().is_some();
                let is_register = decl.register_attr().is_some();
                let is_const = decl.const_token().is_some();

                // ???: Should DefIds be used instead of raw names?
                let names = Self::lower_name_list(decl.decl_list())?;

                let type_spec = decl
                    .type_spec()
                    .and_then(|ty| self.lower_type(ty))
                    .map(|(node, span)| self.database.type_nodes.alloc_spanned(node, span));
                let init_expr = decl
                    .init()
                    .map(|expr| self.lower_expr(expr))
                    .map(|(node, span)| self.database.expr_nodes.alloc_spanned(node, span));

                Some((
                    stmt::Stmt::ConstVar {
                        is_pervasive,
                        is_register,
                        is_const,

                        names,
                        type_spec,
                        init_expr,
                    },
                    span,
                ))
            }
            ast::Stmt::TypeDecl(_) => todo!(),
            ast::Stmt::BindDecl(_) => todo!(),
            ast::Stmt::ProcDecl(_) => todo!(),
            ast::Stmt::FcnDecl(_) => todo!(),
            ast::Stmt::ProcessDecl(_) => todo!(),
            ast::Stmt::ExternalDecl(_) => todo!(),
            ast::Stmt::ForwardDecl(_) => todo!(),
            ast::Stmt::DeferredDecl(_) => todo!(),
            ast::Stmt::BodyDecl(_) => todo!(),
            ast::Stmt::ModuleDecl(_) => todo!(),
            ast::Stmt::ClassDecl(_) => todo!(),
            ast::Stmt::MonitorDecl(_) => todo!(),
            ast::Stmt::AssignStmt(stmt) => {
                let op = stmt
                    .asn_op()
                    .and_then(|op| op.asn_kind())
                    .map(|op| syntax_to_hir_asn_op(op))
                    .unwrap_or(stmt::AssignOp::None);

                let lhs = self.lower_expr(stmt.reference()?.as_expr());
                let lhs = self.database.expr_nodes.alloc_spanned(lhs.0, lhs.1);

                let rhs = self.lower_expr(stmt.expr()?);
                let rhs = self.database.expr_nodes.alloc_spanned(rhs.0, rhs.1);

                Some((stmt::Stmt::Assign { lhs, op, rhs }, span))
            }
            ast::Stmt::OpenStmt(_) => todo!(),
            ast::Stmt::CloseStmt(_) => todo!(),
            ast::Stmt::PutStmt(_) => todo!(),
            ast::Stmt::GetStmt(_) => todo!(),
            ast::Stmt::ReadStmt(_) => todo!(),
            ast::Stmt::WriteStmt(_) => todo!(),
            ast::Stmt::SeekStmt(_) => todo!(),
            ast::Stmt::TellStmt(_) => todo!(),
            ast::Stmt::ForStmt(_) => todo!(),
            ast::Stmt::LoopStmt(_) => todo!(),
            ast::Stmt::ExitStmt(_) => todo!(),
            ast::Stmt::IfStmt(_) => todo!(),
            ast::Stmt::CaseStmt(_) => todo!(),
            ast::Stmt::BlockStmt(_) => todo!(),
            ast::Stmt::InvariantStmt(_) => todo!(),
            ast::Stmt::AssertStmt(_) => todo!(),
            ast::Stmt::CallStmt(_) => todo!(),
            ast::Stmt::ReturnStmt(_) => todo!(),
            ast::Stmt::ResultStmt(_) => todo!(),
            ast::Stmt::NewStmt(_) => todo!(),
            ast::Stmt::FreeStmt(_) => todo!(),
            ast::Stmt::TagStmt(_) => todo!(),
            ast::Stmt::ForkStmt(_) => todo!(),
            ast::Stmt::SignalStmt(_) => todo!(),
            ast::Stmt::PauseStmt(_) => todo!(),
            ast::Stmt::QuitStmt(_) => todo!(),
            ast::Stmt::BreakStmt(_) => todo!(),
            ast::Stmt::CheckednessStmt(_) => todo!(),
            ast::Stmt::PreStmt(_) => todo!(),
            ast::Stmt::InitStmt(_) => todo!(),
            ast::Stmt::PostStmt(_) => todo!(),
            ast::Stmt::HandlerStmt(_) => todo!(),
            ast::Stmt::InheritStmt(_) => todo!(),
            ast::Stmt::ImplementStmt(_) => todo!(),
            ast::Stmt::ImplementByStmt(_) => todo!(),
            ast::Stmt::ImportStmt(_) => todo!(),
            ast::Stmt::ExportStmt(_) => todo!(),
        }
    }

    fn lower_type(&mut self, ty: ast::Type) -> Option<(ty::Type, toc_reporting::TextRange)> {
        let _span = ty.syntax().text_range();

        match ty {
            ast::Type::PrimType(_) => todo!(),
            ast::Type::NameType(_) => todo!(),
            ast::Type::RangeType(_) => todo!(),
            ast::Type::EnumType(_) => todo!(),
            ast::Type::ArrayType(_) => todo!(),
            ast::Type::SetType(_) => todo!(),
            ast::Type::RecordType(_) => todo!(),
            ast::Type::UnionType(_) => todo!(),
            ast::Type::PointerType(_) => todo!(),
            ast::Type::FcnType(_) => todo!(),
            ast::Type::ProcType(_) => todo!(),
            ast::Type::CollectionType(_) => todo!(),
            ast::Type::ConditionType(_) => todo!(),
        }
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> (expr::Expr, toc_reporting::TextRange) {
        let span = expr.syntax().text_range();

        let expr = match expr {
            ast::Expr::LiteralExpr(_) => todo!(),
            ast::Expr::ObjClassExpr(_) => todo!(),
            ast::Expr::InitExpr(_) => todo!(),
            ast::Expr::NilExpr(_) => todo!(),
            ast::Expr::SizeOfExpr(_) => todo!(),
            ast::Expr::BinaryExpr(_) => todo!(),
            ast::Expr::UnaryExpr(_) => todo!(),
            ast::Expr::ParenExpr(_) => todo!(),
            ast::Expr::NameExpr(expr) => {
                if let Some(name) = expr.name().and_then(|name| name.identifier_token()) {
                    expr::Expr::Name(expr::Name::Name(name.text().to_string()))
                } else {
                    expr::Expr::Missing
                }
            }
            ast::Expr::SelfExpr(_) => todo!(),
            ast::Expr::FieldExpr(_) => todo!(),
            ast::Expr::DerefExpr(_) => todo!(),
            ast::Expr::CheatExpr(_) => todo!(),
            ast::Expr::NatCheatExpr(_) => todo!(),
            ast::Expr::ArrowExpr(_) => todo!(),
            ast::Expr::IndirectExpr(_) => todo!(),
            ast::Expr::BitsExpr(_) => todo!(),
            ast::Expr::CallExpr(_) => todo!(),
        };

        (expr, span)
    }

    /// Lowers a name list, holding up the invariant that it always contains
    /// at least one identifier
    fn lower_name_list(name_list: Option<ast::NameList>) -> Option<Vec<String>> {
        let names = name_list?
            .names()
            .filter_map(|name| {
                name.identifier_token()
                    .map(|token| token.text().to_string())
            })
            .collect::<Vec<_>>();

        // Invariant: Names list must contain at least one name
        Some(names).filter(|names| !names.is_empty())
    }
}

fn syntax_to_hir_asn_op(op: toc_syntax::AssignOp) -> stmt::AssignOp {
    match op {
        toc_syntax::AssignOp::None => stmt::AssignOp::None,
        toc_syntax::AssignOp::Add => stmt::AssignOp::Add,
        toc_syntax::AssignOp::Sub => stmt::AssignOp::Sub,
        toc_syntax::AssignOp::Mul => stmt::AssignOp::Mul,
        toc_syntax::AssignOp::Div => stmt::AssignOp::Div,
        toc_syntax::AssignOp::RealDiv => stmt::AssignOp::RealDiv,
        toc_syntax::AssignOp::Mod => stmt::AssignOp::Mod,
        toc_syntax::AssignOp::Rem => stmt::AssignOp::Rem,
        toc_syntax::AssignOp::Exp => stmt::AssignOp::Exp,
        toc_syntax::AssignOp::And => stmt::AssignOp::And,
        toc_syntax::AssignOp::Or => stmt::AssignOp::Or,
        toc_syntax::AssignOp::Xor => stmt::AssignOp::Xor,
        toc_syntax::AssignOp::Shl => stmt::AssignOp::Shl,
        toc_syntax::AssignOp::Shr => stmt::AssignOp::Shr,
        toc_syntax::AssignOp::Imply => stmt::AssignOp::Imply,
    }
}
