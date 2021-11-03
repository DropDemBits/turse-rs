//! Lowering into `Stmt` HIR nodes
use toc_hir::stmt::Assign;
use toc_hir::{expr, item, stmt, symbol};
use toc_span::{SpanId, Spanned};
use toc_syntax::ast::{self, AstNode};

use crate::lower::LoweredStmt;

impl super::BodyLowering<'_, '_> {
    pub(super) fn lower_stmt(&mut self, stmt: ast::Stmt) -> Option<LoweredStmt> {
        let span = self.ctx.intern_range(stmt.syntax().text_range());

        let kind = match stmt {
            // `ConstVarDecl` is the only decl that can produce multiple stmts
            ast::Stmt::ConstVarDecl(decl) => return self.lower_constvar_decl(decl),
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
        };

        kind.map(LoweredStmt::Single)
    }

    fn unsupported_stmt(&mut self, span: SpanId) -> Option<stmt::StmtKind> {
        let span = self.ctx.library.lookup_span(span);
        self.ctx.messages.error(
            "unsupported statement",
            "this statement is not handled yet",
            span,
        );
        None
    }

    fn lower_constvar_decl(&mut self, decl: ast::ConstVarDecl) -> Option<LoweredStmt> {
        // is actually an item
        let span = self.ctx.mk_span(decl.syntax().text_range());
        let span = self.ctx.library.intern_span(span);

        let is_pervasive = decl.pervasive_attr().is_some();
        let is_register = decl.register_attr().is_some();
        let is_const = decl.const_token().is_some();

        let type_spec = decl.type_spec().and_then(|ty| self.lower_type(ty));
        let init_expr = decl.init().map(|expr| self.ctx.lower_expr_body(expr));
        let tail = match (type_spec, init_expr) {
            (Some(type_spec), None) => item::ConstVarTail::TypeSpec(type_spec),
            (None, Some(init_expr)) => item::ConstVarTail::InitExpr(init_expr),
            (Some(type_spec), Some(init_expr)) => item::ConstVarTail::Both(type_spec, init_expr),
            // Captured by the parser, no error needs to be reported
            (None, None) => return None,
        };

        // Declare names after uses to prevent def-use cycles
        let names = self.lower_name_list(decl.decl_list(), is_pervasive)?;
        let mutability = if is_const {
            item::Mutability::Const
        } else {
            item::Mutability::Var
        };

        let stmts: Vec<_> = names
            .into_iter()
            .map(|def_id| {
                let const_var = item::ConstVar {
                    mutability,
                    is_register,
                    tail,
                };

                let item_id = self.ctx.library.add_item(item::Item {
                    kind: item::ItemKind::ConstVar(const_var),
                    def_id,
                    span,
                });

                stmt::StmtKind::Item(item_id)
            })
            .collect();

        let lowered = if stmts.len() == 1 {
            // Single declaration
            // Just a little mutable reborrow, as a treat
            let mut stmts = stmts;
            LoweredStmt::Single(stmts.pop().unwrap())
        } else {
            // Multiple declarations, pack it into a multiple
            LoweredStmt::Multiple(stmts)
        };

        Some(lowered)
    }

    fn lower_assign_stmt(&mut self, stmt: ast::AssignStmt) -> Option<stmt::StmtKind> {
        let (op, asn_span) = {
            let asn_op = stmt.asn_op()?;
            let span = self.ctx.intern_range(asn_op.syntax().text_range());

            (asn_op.asn_kind().and_then(asn_to_bin_op), span)
        };

        let lhs = self.lower_expr(stmt.lhs()?);
        let rhs = self.lower_expr(stmt.rhs()?);

        let rhs = if let Some(op) = op {
            // Insert binary expression
            let op = Spanned::new(op, asn_span);
            let kind = expr::ExprKind::Binary(expr::Binary { lhs, op, rhs });
            // Take the span of the assignment stmt
            let span = self.ctx.intern_range(stmt.syntax().text_range());

            self.body.add_expr(expr::Expr { kind, span })
        } else {
            rhs
        };

        Some(stmt::StmtKind::Assign(Assign {
            lhs,
            asn: asn_span,
            rhs,
        }))
    }

    fn lower_put_stmt(&mut self, stmt: ast::PutStmt) -> Option<stmt::StmtKind> {
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
            Some(stmt::StmtKind::Put(stmt::Put {
                stream_num,
                items,
                append_newline,
            }))
        }
    }

    fn lower_get_stmt(&mut self, stmt: ast::GetStmt) -> Option<stmt::StmtKind> {
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
            Some(stmt::StmtKind::Get(stmt::Get { stream_num, items }))
        }
    }

    fn lower_block_stmt(&mut self, stmt: ast::BlockStmt) -> Option<stmt::StmtKind> {
        let stmt_list = stmt.stmt_list()?;

        self.ctx.scopes.push_scope(false);
        let stmts = self.lower_stmt_list(stmt_list);
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::Block(stmt::Block {
            kind: stmt::BlockKind::Normal,
            stmts,
        }))
    }

    /// Lowers a name list, holding up the invariant that it always contains
    /// at least one identifier
    fn lower_name_list(
        &mut self,
        name_list: Option<ast::NameList>,
        is_pervasive: bool,
    ) -> Option<Vec<symbol::LocalDefId>> {
        let names = name_list?
            .names()
            .filter_map(|name| {
                name.identifier_token().map(|token| {
                    let span = self.ctx.intern_range(token.text_range());
                    let def_id =
                        self.ctx
                            .library
                            .add_def(token.text(), span, symbol::SymbolKind::Declared);
                    self.ctx.scopes.def_sym(token.text(), def_id, is_pervasive)
                })
            })
            .collect::<Vec<_>>();

        // Invariant: Names list must contain at least one name
        Some(names).filter(|names| !names.is_empty())
    }
}

fn asn_to_bin_op(op: toc_syntax::AssignOp) -> Option<expr::BinaryOp> {
    Some(match op {
        toc_syntax::AssignOp::None => return None,
        toc_syntax::AssignOp::Add => expr::BinaryOp::Add,
        toc_syntax::AssignOp::Sub => expr::BinaryOp::Sub,
        toc_syntax::AssignOp::Mul => expr::BinaryOp::Mul,
        toc_syntax::AssignOp::Div => expr::BinaryOp::Div,
        toc_syntax::AssignOp::RealDiv => expr::BinaryOp::RealDiv,
        toc_syntax::AssignOp::Mod => expr::BinaryOp::Mod,
        toc_syntax::AssignOp::Rem => expr::BinaryOp::Rem,
        toc_syntax::AssignOp::Exp => expr::BinaryOp::Exp,
        toc_syntax::AssignOp::And => expr::BinaryOp::And,
        toc_syntax::AssignOp::Or => expr::BinaryOp::Or,
        toc_syntax::AssignOp::Xor => expr::BinaryOp::Xor,
        toc_syntax::AssignOp::Shl => expr::BinaryOp::Shl,
        toc_syntax::AssignOp::Shr => expr::BinaryOp::Shr,
        toc_syntax::AssignOp::Imply => expr::BinaryOp::Imply,
    })
}
