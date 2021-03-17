//! Lowering into `Stmt` HIR nodes
use toc_hir::{stmt, symbol};
use toc_syntax::ast::{self, AstNode};

impl super::LoweringCtx {
    pub(super) fn lower_stmt(&mut self, stmt: ast::Stmt) -> Option<stmt::StmtIdx> {
        let span = stmt.syntax().text_range();

        let stmt = match stmt {
            ast::Stmt::ConstVarDecl(decl) => self.lower_constvar_decl(decl),
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
            ast::Stmt::AssignStmt(stmt) => self.lower_assign_stmt(stmt),
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
            ast::Stmt::BlockStmt(stmt) => self.lower_block_stmt(stmt),
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
            ast::Stmt::PreprocGlob(_) => todo!(),
        }?;

        Some(self.database.stmt_nodes.alloc_spanned(stmt, span))
    }

    fn lower_constvar_decl(&mut self, decl: ast::ConstVarDecl) -> Option<stmt::Stmt> {
        let is_pervasive = decl.pervasive_attr().is_some();
        let is_register = decl.register_attr().is_some();
        let is_const = decl.const_token().is_some();

        let type_spec = decl.type_spec().and_then(|ty| self.lower_type(ty));
        let init_expr = decl.init().map(|expr| self.lower_expr(expr));

        // Declare names after uses to prevent def-use cycles
        let names = self.lower_name_list(decl.decl_list(), is_pervasive)?;

        Some(stmt::Stmt::ConstVar {
            is_pervasive,
            is_register,
            is_const,

            names,
            type_spec,
            init_expr,
        })
    }

    fn lower_assign_stmt(&mut self, stmt: ast::AssignStmt) -> Option<stmt::Stmt> {
        let op = stmt
            .asn_op()
            .and_then(|op| op.asn_kind())
            .map(|op| syntax_to_hir_asn_op(op))
            .unwrap_or(stmt::AssignOp::None);

        let lhs = self.lower_expr(stmt.reference()?.as_expr());
        let rhs = self.lower_expr(stmt.expr()?);

        Some(stmt::Stmt::Assign { lhs, op, rhs })
    }

    fn lower_block_stmt(&mut self, stmt: ast::BlockStmt) -> Option<stmt::Stmt> {
        self.scopes.push_scope(false);

        let stmts = if let Some(stmts) = stmt.stmt_list() {
            stmts
                .stmts()
                .filter_map(|stmt| self.lower_stmt(stmt))
                .collect()
        } else {
            vec![]
        };

        self.scopes.pop_scope();

        Some(stmt::Stmt::Block { stmts })
    }

    /// Lowers a name list, holding up the invariant that it always contains
    /// at least one identifier
    fn lower_name_list(
        &mut self,
        name_list: Option<ast::NameList>,
        is_pervasive: bool,
    ) -> Option<Vec<symbol::DefId>> {
        let names = name_list?
            .names()
            .filter_map(|name| {
                name.identifier_token().map(|token| {
                    self.scopes.def_sym(
                        token.text(),
                        token.text_range(),
                        symbol::SymbolKind::Declared,
                        is_pervasive,
                    )
                })
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
