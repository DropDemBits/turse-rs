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
            ast::Stmt::PutStmt(stmt) => self.lower_put_stmt(stmt),
            ast::Stmt::GetStmt(stmt) => self.lower_get_stmt(stmt),
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

        let lhs = self.lower_expr(stmt.lhs()?.as_expr());
        let rhs = self.lower_expr(stmt.rhs()?);

        Some(stmt::Stmt::Assign { lhs, op, rhs })
    }

    fn lower_put_stmt(&mut self, stmt: ast::PutStmt) -> Option<stmt::Stmt> {
        let stream_num = self.try_lower_expr(stmt.stream_num().and_then(|s| s.expr()));
        let items = stmt
            .items()
            .filter_map(|item| {
                if item.skip_token().is_some() {
                    Some(stmt::Skippable::Skip)
                } else if let Some(expr) = item.expr() {
                    let expr = self.lower_expr(expr);
                    let width = self.try_lower_expr(item.width().and_then(|o| o.expr()));
                    let precision = self.try_lower_expr(item.fraction().and_then(|o| o.expr()));
                    let exponent_width =
                        self.try_lower_expr(item.exp_width().and_then(|o| o.expr()));

                    let item = match (width, precision, exponent_width) {
                        (Some(width), Some(precision), Some(exponent_width)) => {
                            stmt::PutItem::with_exponent_width(
                                expr,
                                width,
                                precision,
                                exponent_width,
                            )
                        }
                        (Some(width), Some(precision), None) => {
                            stmt::PutItem::with_precision(expr, width, precision)
                        }
                        (Some(width), None, None) => stmt::PutItem::with_width(expr, width),
                        (None, None, None) => stmt::PutItem::new(expr),
                        _ => unreachable!(), // Invariants are being broken
                    };

                    Some(stmt::Skippable::Item(item))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        // Presence means newline should be omitted
        let append_newline = !stmt.range_token().is_some();

        if items.is_empty() {
            // there must be at least one item present
            None
        } else {
            Some(stmt::Stmt::Put {
                stream_num,
                items,
                append_newline,
            })
        }
    }

    fn lower_get_stmt(&mut self, stmt: ast::GetStmt) -> Option<stmt::Stmt> {
        let stream_num = self.try_lower_expr(stmt.stream_num().and_then(|s| s.expr()));
        let items = stmt
            .items()
            .filter_map(|item| {
                if item.skip_token().is_some() {
                    Some(stmt::Skippable::Skip)
                } else if let Some(ref_expr) = item.reference() {
                    let expr = self.lower_expr(ref_expr.as_expr());
                    let width = match item.get_width() {
                        None => stmt::GetWidth::Token,
                        Some(width) if width.star_token().is_some() => stmt::GetWidth::Line,
                        Some(width) => {
                            // Should be an expr for the count
                            stmt::GetWidth::Chars(self.lower_required_expr(width.expr()))
                        }
                    };

                    Some(stmt::Skippable::Item(stmt::GetItem { expr, width }))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if items.is_empty() {
            // there must be at least one item present
            None
        } else {
            Some(stmt::Stmt::Get { stream_num, items })
        }
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
