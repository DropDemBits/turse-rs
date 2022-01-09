//! Lowering into `Stmt` HIR nodes
use toc_hir::stmt::Assign;
use toc_hir::symbol::ForwardKind;
use toc_hir::{
    expr, item, stmt,
    symbol::{self, SymbolKind},
};
use toc_span::{SpanId, Spanned};
use toc_syntax::ast::{self, AstNode};

use crate::lower::LoweredStmt;
use crate::scopes::ScopeKind;

impl super::BodyLowering<'_, '_> {
    pub(super) fn lower_stmt(&mut self, stmt: ast::Stmt) -> Option<LoweredStmt> {
        let span = self.ctx.intern_range(stmt.syntax().text_range());

        let kind = match stmt {
            // `ConstVarDecl` is the only decl that can produce multiple stmts
            ast::Stmt::ConstVarDecl(decl) => return self.lower_constvar_decl(decl),
            ast::Stmt::TypeDecl(decl) => self.lower_type_decl(decl),

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

            ast::Stmt::ForStmt(stmt) => self.lower_for_stmt(stmt),
            ast::Stmt::LoopStmt(stmt) => self.lower_loop_stmt(stmt),
            ast::Stmt::ExitStmt(stmt) => self.lower_exit_stmt(stmt),
            ast::Stmt::IfStmt(stmt) => self.lower_if_stmt(stmt),
            ast::Stmt::CaseStmt(stmt) => self.lower_case_stmt(stmt),
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
        let span = self.ctx.intern_range(decl.syntax().text_range());

        let is_pervasive = decl.pervasive_attr().is_some();
        let is_register = decl.register_attr().is_some();
        let is_const = decl.const_token().is_some();

        let type_spec = decl.type_spec().and_then(|ty| self.lower_type(ty));
        let init_expr = decl.init().map(|expr| self.ctx.lower_expr_body(expr));

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
                    is_register,
                    mutability,
                    def_id,
                    type_spec,
                    init_expr,
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

    fn lower_type_decl(&mut self, decl: ast::TypeDecl) -> Option<stmt::StmtKind> {
        // Procedure for resolving forward declarations (general)
        // - declare them as a forward
        //   - ScopeTracker records that there's an unresolved forward decl here
        //   - Duplicate forward reported as error, but kept track of as eventually resolving to the same thing
        // - when declaring a resolution
        //   - resolve any forwards in the same scope level
        //   - If there's already a resolution, change error report based on what the old decl was
        //     - Resolved / Declared -> Duplicate def
        //     - Forward -> Must resolve in the same scope

        let is_pervasive = decl.pervasive_attr().is_some();

        let (type_def, sym_kind) = if let Some(forward) = decl.forward_token() {
            let token_span = self.ctx.intern_range(forward.text_range());
            (
                item::DefinedType::Forward(token_span),
                SymbolKind::Forward(ForwardKind::Type, None),
            )
        } else {
            let ty = self.lower_required_type(decl.named_ty());
            (
                item::DefinedType::Alias(ty),
                SymbolKind::Resolved(ForwardKind::Type),
            )
        };

        // Declare name after type to prevent def-use cycles
        let def_id = self.lower_name_def(decl.decl_name()?, sym_kind, is_pervasive);

        let span = self.ctx.intern_range(decl.syntax().text_range());

        let item_id = self.ctx.library.add_item(item::Item {
            kind: item::ItemKind::Type(item::Type { def_id, type_def }),
            def_id,
            span,
        });

        Some(stmt::StmtKind::Item(item_id))
    }

    fn lower_assign_stmt(&mut self, stmt: ast::AssignStmt) -> Option<stmt::StmtKind> {
        let (op, asn_span) = {
            let asn_op = stmt.asn_op()?;
            let span = self.ctx.intern_range(asn_op.syntax().text_range());

            (asn_op.asn_kind().and_then(asn_to_bin_op), span)
        };

        // Only lhs is required to generate a node
        let lhs = self.lower_expr(stmt.lhs()?);
        let rhs = self.lower_required_expr(stmt.rhs());

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

        if items.is_empty() && stream_num.is_none() {
            // there must be something present
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

        if items.is_empty() && stream_num.is_none() {
            // there must be something present
            None
        } else {
            Some(stmt::StmtKind::Get(stmt::Get { stream_num, items }))
        }
    }

    fn lower_for_stmt(&mut self, stmt: ast::ForStmt) -> Option<stmt::StmtKind> {
        let is_decreasing = stmt.decreasing_token().is_some();
        let name = stmt.name();

        let for_bounds = {
            let for_bounds = stmt.for_bounds().unwrap();

            if for_bounds.range_token().is_some() {
                // Explicit bounds
                stmt::ForBounds::Full(
                    self.lower_required_expr(for_bounds.start()),
                    self.lower_required_expr(for_bounds.end()),
                )
            } else if let Some(start) = for_bounds.start() {
                // Implicit bounds
                stmt::ForBounds::Implicit(self.lower_expr(start))
            } else {
                // Make a dummy for-loop bounds
                // Full bounds satisfies all cases
                stmt::ForBounds::Full(
                    self.lower_required_expr(None),
                    self.lower_required_expr(None),
                )
            }
        };

        let step_by = stmt
            .steps()
            .and_then(|step_by| step_by.expr())
            .map(|expr| self.lower_expr(expr));

        let counter_def;
        let body_stmts;

        self.ctx.scopes.push_scope(ScopeKind::Loop);
        {
            // counter is only available inside of the loop body
            counter_def =
                name.map(|name| self.lower_name_def(name, symbol::SymbolKind::Declared, false));
            body_stmts = self.lower_stmt_list(stmt.stmt_list().unwrap());
        }
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::For(stmt::For {
            is_decreasing,
            counter_def,
            bounds: for_bounds,
            step_by,
            stmts: body_stmts,
        }))
    }

    fn lower_loop_stmt(&mut self, stmt: ast::LoopStmt) -> Option<stmt::StmtKind> {
        self.ctx.scopes.push_scope(ScopeKind::Loop);
        let stmts = self.lower_stmt_list(stmt.stmt_list().unwrap());
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::Loop(stmt::Loop { stmts }))
    }

    fn lower_exit_stmt(&mut self, stmt: ast::ExitStmt) -> Option<stmt::StmtKind> {
        Some(stmt::StmtKind::Exit(stmt::Exit {
            when_condition: self.try_lower_expr(stmt.condition()),
        }))
    }

    fn lower_if_stmt(&mut self, stmt: ast::IfStmt) -> Option<stmt::StmtKind> {
        Some(self.lower_if_stmt_body(stmt.if_body().unwrap()))
    }

    fn lower_if_stmt_body(&mut self, if_body: ast::IfBody) -> stmt::StmtKind {
        let condition = self.lower_required_expr(if_body.condition());

        // Create block stmt for the true branch
        self.ctx.scopes.push_scope(ScopeKind::Block);
        let true_branch =
            self.lower_stmt_list_to_block(if_body.true_branch().unwrap(), ScopeKind::Block);
        self.ctx.scopes.pop_scope();

        // Handle the false branch
        let false_branch = if_body.false_branch().map(|branch| match branch {
            ast::FalseBranch::ElseStmt(stmt) => {
                // Simple, just lower to a stmt block
                self.ctx.scopes.push_scope(ScopeKind::Block);
                let else_branch =
                    self.lower_stmt_list_to_block(stmt.stmt_list().unwrap(), ScopeKind::Block);
                self.ctx.scopes.pop_scope();

                else_branch
            }
            ast::FalseBranch::ElseifStmt(stmt) => {
                // Also simple, just reuse our lowering of if bodies
                let range = stmt.syntax().text_range();
                let span = self.ctx.library.intern_span(self.ctx.mk_span(range));
                let kind = self.lower_if_stmt_body(stmt.if_body().unwrap());

                self.body.add_stmt(stmt::Stmt { kind, span })
            }
        });

        stmt::StmtKind::If(stmt::If {
            condition,
            true_branch,
            false_branch,
        })
    }

    fn lower_case_stmt(&mut self, stmt: ast::CaseStmt) -> Option<stmt::StmtKind> {
        let discriminant = self.lower_required_expr(stmt.expr());
        let case_arms = stmt
            .case_arm()
            .map(|arm| {
                let selectors: Vec<_> = arm
                    .select()
                    .map(|selectors| {
                        selectors
                            .exprs()
                            .map(|expr| self.lower_expr(expr))
                            .collect()
                    })
                    .unwrap_or_default();

                let selectors = if selectors.is_empty() {
                    stmt::CaseSelector::Default
                } else {
                    stmt::CaseSelector::Exprs(selectors)
                };

                self.ctx.scopes.push_scope(ScopeKind::Block);
                let stmts = self.lower_stmt_list(arm.stmt_list().unwrap());
                self.ctx.scopes.pop_scope();

                stmt::CaseArm { selectors, stmts }
            })
            .collect();

        Some(stmt::StmtKind::Case(stmt::Case {
            discriminant,
            arms: case_arms,
        }))
    }

    fn lower_block_stmt(&mut self, stmt: ast::BlockStmt) -> Option<stmt::StmtKind> {
        let stmt_list = stmt.stmt_list()?;

        self.ctx.scopes.push_scope(ScopeKind::Block);
        let stmts = self.lower_stmt_list(stmt_list);
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::Block(stmt::Block {
            kind: stmt::BlockKind::Normal,
            stmts,
        }))
    }

    fn lower_stmt_list_to_block(
        &mut self,
        stmt_list: ast::StmtList,
        as_kind: ScopeKind,
    ) -> stmt::StmtId {
        let range = stmt_list.syntax().text_range();

        self.ctx.scopes.push_scope(as_kind);
        let stmts = self.lower_stmt_list(stmt_list);
        self.ctx.scopes.pop_scope();

        let kind = stmt::StmtKind::Block(stmt::Block {
            kind: stmt::BlockKind::Normal,
            stmts,
        });
        let span = self.ctx.library.intern_span(self.ctx.mk_span(range));

        self.body.add_stmt(toc_hir::stmt::Stmt { kind, span })
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
            .map(|name| self.lower_name_def(name, symbol::SymbolKind::Declared, is_pervasive))
            .collect::<Vec<_>>();

        // Invariant: Names list must contain at least one name
        Some(names).filter(|names| !names.is_empty())
    }

    fn lower_name_def(
        &mut self,
        name: ast::Name,
        kind: symbol::SymbolKind,
        is_pervasive: bool,
    ) -> symbol::LocalDefId {
        // Can't declare an undefined symbol from a name def
        assert_ne!(kind, SymbolKind::Undeclared);

        let token = name.identifier_token().unwrap();
        let span = self.ctx.intern_range(token.text_range());
        let def_id = self.ctx.library.add_def(token.text(), span, kind);

        // Bring into scope
        let old_def = self
            .ctx
            .scopes
            .def_sym(token.text(), def_id, kind, is_pervasive);

        // Resolve any associated forward decls
        if let SymbolKind::Resolved(resolve_kind) = kind {
            let forward_list = self
                .ctx
                .scopes
                .take_resolved_forwards(token.text(), resolve_kind);

            if let Some(forward_list) = forward_list {
                // Point all of these local defs to this one
                for forward_def in forward_list {
                    let def_info = self.ctx.library.local_def_mut(forward_def);

                    match &mut def_info.kind {
                        SymbolKind::Forward(_, resolve_to) => *resolve_to = Some(def_id),
                        _ => unreachable!("not a forward def"),
                    }
                }
            }
        }

        if let Some(old_def) = old_def {
            // Report redeclares, specializing based on what kind of declaration it is
            let old_def_info = self.ctx.library.local_def(old_def);

            let old_span = self.ctx.library.lookup_span(old_def_info.name.span());
            let new_span = self.ctx.library.lookup_span(span);

            // Just use the name from the old def for both, since by definition they are the same
            let name = old_def_info.name.item();

            match (old_def_info.kind, kind) {
                (SymbolKind::Undeclared, _) | (_, SymbolKind::Undeclared) => {
                    // Always ok to declare over undeclared symbols
                }
                (SymbolKind::Forward(other_kind, _), SymbolKind::Forward(this_kind, _))
                    if other_kind == this_kind =>
                {
                    // Duplicate forward declare
                    self.ctx
                        .messages
                        .error_detailed(
                            &format!("`{}` is already a forward declaration", name),
                            new_span,
                        )
                        .with_note("previous forward declaration here", old_span)
                        .with_error("new one here", new_span)
                        .finish();
                }
                (SymbolKind::Forward(other_kind, resolve_to), SymbolKind::Resolved(this_kind))
                    if other_kind == this_kind =>
                {
                    // resolve_to: none -> didn't change
                    // resolve_to: some(== def_id) -> did change, this resolved it
                    // resolve_to: some(!= def_id) -> didn't change, this didn't resolve it (ok to note as redeclare)

                    if resolve_to.is_none() {
                        // Forwards must be resolved to the same scope
                        // This declaration didn't resolve it, which only happens if they're not in the same scope
                        self.ctx
                            .messages
                            .error_detailed(
                                &format!("`{}` must be resolved in the same scope", name),
                                new_span,
                            )
                            .with_note(&format!("forward declaration of `{}` here", name), old_span)
                            .with_error(
                                &format!("resolution of `{}` is not in the same scope", name),
                                new_span,
                            )
                            .finish();
                    } else {
                        // This shouldn't ever fail, since if a symbol is moving from
                        // forward to resolved, this is the declaration that should've
                        // done it.
                        assert_eq!(
                            resolve_to,
                            Some(def_id),
                            "encountered a forward not resolved by this definition"
                        );
                    }
                }
                _ => {
                    // Redeclaring over an older def
                    self.ctx
                        .messages
                        .error_detailed(
                            &format!("`{}` is already declared in this scope", name),
                            new_span,
                        )
                        .with_note(&format!("`{}` previously declared here", name), old_span)
                        .with_error(&format!("`{}` redeclared here", name), new_span)
                        .finish();
                }
            }
        }

        def_id
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
