//! Lowering into `Stmt` HIR nodes
use toc_hir::stmt::{Assign, ConstVar};
use toc_hir::{stmt, symbol};
use toc_reporting::MessageKind;
use toc_span::{Spanned, TextRange};
use toc_syntax::ast::{self, AstNode};

impl super::LoweringCtx {
    pub(super) fn lower_stmt(&mut self, stmt: ast::Stmt) -> Option<stmt::StmtIdx> {
        let span = stmt.syntax().text_range();

        let stmt = match stmt {
            ast::Stmt::ConstVarDecl(decl) => self.lower_constvar_decl(decl),
            ast::Stmt::TypeDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::BindDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ProcDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::FcnDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ProcessDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ExternalDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ForwardDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::DeferredDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::BodyDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ModuleDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ClassDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::MonitorDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::AssignStmt(stmt) => self.lower_assign_stmt(stmt),
            ast::Stmt::OpenStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::CloseStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PutStmt(stmt) => self.lower_put_stmt(stmt),
            ast::Stmt::GetStmt(stmt) => self.lower_get_stmt(stmt),
            ast::Stmt::ReadStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::WriteStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::SeekStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::TellStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ForStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::LoopStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ExitStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::IfStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::CaseStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::BlockStmt(stmt) => self.lower_block_stmt(stmt),
            ast::Stmt::InvariantStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::AssertStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::CallStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ReturnStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ResultStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::NewStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::FreeStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::TagStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ForkStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::SignalStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PauseStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::QuitStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::BreakStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::CheckednessStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PreStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::InitStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PostStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::HandlerStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::InheritStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ImplementStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ImplementByStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ImportStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ExportStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PreprocGlob(_) => self.unsupported_stmt(span),
        }?;

        Some(self.database.stmt_nodes.alloc_spanned(stmt, span))
    }

    fn unsupported_stmt(&mut self, span: TextRange) -> Option<stmt::Stmt> {
        self.messages
            .report(MessageKind::Error, "unsupported statement", span);
        None
    }

    fn lower_constvar_decl(&mut self, decl: ast::ConstVarDecl) -> Option<stmt::Stmt> {
        let is_pervasive = decl.pervasive_attr().is_some();
        let is_register = decl.register_attr().is_some();
        let is_const = decl.const_token().is_some();

        let type_spec = decl.type_spec().and_then(|ty| self.lower_type(ty));
        let init_expr = decl.init().map(|expr| self.lower_expr(expr));
        let tail = match (type_spec, init_expr) {
            (Some(type_spec), None) => stmt::ConstVarTail::TypeSpec(type_spec),
            (None, Some(init_expr)) => stmt::ConstVarTail::InitExpr(init_expr),
            (Some(type_spec), Some(init_expr)) => stmt::ConstVarTail::Both(type_spec, init_expr),
            // Captured by the parser, no error needs to be reported
            (None, None) => return None,
        };

        // Declare names after uses to prevent def-use cycles
        let names = self.lower_name_list(decl.decl_list(), is_pervasive)?;

        Some(stmt::Stmt::ConstVar(ConstVar {
            is_register,
            is_const,

            names,
            tail,
        }))
    }

    fn lower_assign_stmt(&mut self, stmt: ast::AssignStmt) -> Option<stmt::Stmt> {
        let op = {
            let asn_op = stmt.asn_op()?;

            let span = asn_op.asn_node()?.text_range();
            let op_kind = asn_op
                .asn_kind()
                .map(syntax_to_hir_asn_op)
                .unwrap_or(stmt::AssignOp::None);

            Spanned::new(op_kind, span)
        };

        let lhs = self.lower_expr(stmt.lhs()?);
        let rhs = self.lower_expr(stmt.rhs()?);

        Some(stmt::Stmt::Assign(Assign { lhs, op, rhs }))
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

                    let opts = {
                        let width = item.width().and_then(|o| o.expr());
                        let precision = item.fraction().and_then(|o| o.expr());
                        let exponent_width = item.exp_width().and_then(|o| o.expr());

                        if let Some(exponent_width) = exponent_width {
                            stmt::PutOpts::WithExponentWidth {
                                width: self.lower_required_expr(width),
                                precision: self.lower_required_expr(precision),
                                exponent_width: self.lower_required_expr(Some(exponent_width)),
                            }
                        } else if let Some(precision) = precision {
                            stmt::PutOpts::WithPrecision {
                                width: self.lower_required_expr(width),
                                precision: self.lower_required_expr(Some(precision)),
                            }
                        } else if let Some(width) = width {
                            stmt::PutOpts::WithWidth {
                                width: self.lower_required_expr(Some(width)),
                            }
                        } else {
                            stmt::PutOpts::None
                        }
                    };

                    let item = stmt::PutItem { expr, opts };

                    Some(stmt::Skippable::Item(item))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        // Presence means newline should be omitted
        let append_newline = stmt.range_token().is_none();

        if items.is_empty() {
            // there must be at least one item present
            None
        } else {
            Some(stmt::Stmt::Put(stmt::Put {
                stream_num,
                items,
                append_newline,
            }))
        }
    }

    fn lower_get_stmt(&mut self, stmt: ast::GetStmt) -> Option<stmt::Stmt> {
        let stream_num = self.try_lower_expr(stmt.stream_num().and_then(|s| s.expr()));
        let items = stmt
            .items()
            .filter_map(|item| {
                if item.skip_token().is_some() {
                    Some(stmt::Skippable::Skip)
                } else if let Some(ref_expr) = item.expr() {
                    let expr = self.lower_expr(ref_expr);
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
            Some(stmt::Stmt::Get(stmt::Get { stream_num, items }))
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

        Some(stmt::Stmt::Block(stmt::Block { stmts }))
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
