//! Lowering from AST expressions and statements into a Body

use toc_ast_db::ast_id::{AstId, AstIdMap};
use toc_hir_expand::{SemanticFile, SemanticLoc, SemanticNodePtr, UnstableSemanticLoc};
use toc_syntax::{
    LiteralValue,
    ast::{self},
};

use crate::{
    Db, Symbol,
    expr::{self, LocalExpr},
    item::{self, HasItems},
    stmt::{self, LocalStmt},
};

use super::{Body, BodyContents, BodySpans};

pub(crate) fn module_body<'db>(
    db: &'db dyn Db,
    body: Body<'db>,
    file: SemanticFile<'db>,
    stmt_list: ast::StmtList,
    child_items: item::ChildItems<'db>,
) -> (BodyContents<'db>, BodySpans<'db>, Vec<BodyLowerError<'db>>) {
    let ast_id_map = file.ast_id_map(db);
    BodyLower::new(db, file, ast_id_map, Some(child_items), body).lower_from_stmts(stmt_list)
}

struct BodyLower<'db> {
    db: &'db dyn Db,
    file: SemanticFile<'db>,
    ast_id_map: &'db AstIdMap,
    child_items: Vec<item::ChildItems<'db>>,

    body: Body<'db>,
    contents: BodyContents<'db>,
    spans: BodySpans<'db>,

    errors: Vec<BodyLowerError<'db>>,
}

impl<'db> BodyLower<'db> {
    fn new(
        db: &'db dyn Db,
        file: SemanticFile<'db>,
        ast_id_map: &'db AstIdMap,
        child_items: Option<item::ChildItems<'db>>,
        body: Body<'db>,
    ) -> Self {
        Self {
            db,
            file,
            ast_id_map,
            body,
            child_items: child_items.into_iter().collect(),
            contents: Default::default(),
            spans: Default::default(),
            errors: Default::default(),
        }
    }

    fn lower_from_stmts(
        mut self,
        root: ast::StmtList,
    ) -> (BodyContents<'db>, BodySpans<'db>, Vec<BodyLowerError<'db>>) {
        let module_block = self
            .ast_id_map
            .lookup_for_maybe(&root)
            .map(|ast_id| item::ModuleBlock::new(self.db, SemanticLoc::new(self.file, ast_id)));

        if let Some(module_block) = module_block {
            self.contents.root_block = Some(module_block);
            self.child_items.push(module_block.child_items(self.db));
        }

        let mut top_level = vec![];
        for stmt in root.stmts() {
            let Some(new_stmt) = self.lower_statement(stmt, &mut top_level) else {
                continue;
            };
            top_level.push(new_stmt);
        }
        let top_level = top_level
            .into_iter()
            .map(|it| it.in_body(self.body))
            .collect::<Vec<_>>();

        let Some(_) = self.child_items.pop() else {
            unreachable!("left a module block that we already left")
        };
        assert!(
            self.child_items.is_empty(),
            "did not leave enough module blocks"
        );

        let BodyLower {
            mut contents,
            mut spans,
            errors,
            ..
        } = self;

        contents.exprs.shrink_to_fit();
        contents.stmts.shrink_to_fit();
        contents.top_level = top_level.into();

        spans.exprs.shrink_to_fit();
        spans.stmts.shrink_to_fit();

        (contents, spans, errors)
    }

    fn lower_statement(
        &mut self,
        stmt: ast::Stmt,
        stmts: &mut Vec<LocalStmt<'db>>,
    ) -> Option<LocalStmt<'db>> {
        // only needed for pointing to unhandled statements
        let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &stmt).into();

        let id = match stmt {
            ast::Stmt::ConstVarDecl(node) => {
                self.lower_constvar_init(node, stmts)?;
                return None;
            }

            // these decls only introduce new definitions in scope,
            // and that's handled by nameres later on
            ast::Stmt::TypeDecl(_)
            | ast::Stmt::BindDecl(_)
            | ast::Stmt::ProcDecl(_)
            | ast::Stmt::FcnDecl(_)
            | ast::Stmt::ProcessDecl(_)
            | ast::Stmt::ExternalDecl(_)
            | ast::Stmt::ForwardDecl(_)
            | ast::Stmt::DeferredDecl(_)
            | ast::Stmt::BodyDecl(_)
            | ast::Stmt::ModuleDecl(_)
            | ast::Stmt::ClassDecl(_)
            | ast::Stmt::MonitorDecl(_) => {
                return None;
            }

            ast::Stmt::AssignStmt(node) => Some(self.lower_assign_stmt(node)),
            ast::Stmt::OpenStmt(_) => None,
            ast::Stmt::CloseStmt(_) => None,
            ast::Stmt::PutStmt(node) => Some(self.lower_put_stmt(node)),
            ast::Stmt::GetStmt(_) => None,
            ast::Stmt::ReadStmt(_) => None,
            ast::Stmt::WriteStmt(_) => None,
            ast::Stmt::SeekStmt(_) => None,
            ast::Stmt::TellStmt(_) => None,
            ast::Stmt::ForStmt(_) => None,
            ast::Stmt::LoopStmt(_) => None,
            ast::Stmt::ExitStmt(_) => None,
            ast::Stmt::IfStmt(_) => None,
            ast::Stmt::CaseStmt(_) => None,
            ast::Stmt::BlockStmt(node) => Some(self.lower_block_stmt(node)),
            ast::Stmt::InvariantStmt(_) => None,
            ast::Stmt::AssertStmt(_) => None,
            ast::Stmt::CallStmt(_) => None,
            ast::Stmt::ReturnStmt(_) => None,
            ast::Stmt::ResultStmt(_) => None,
            ast::Stmt::NewStmt(_) => None,
            ast::Stmt::FreeStmt(_) => None,
            ast::Stmt::TagStmt(_) => None,
            ast::Stmt::ForkStmt(_) => None,
            ast::Stmt::SignalStmt(_) => None,
            ast::Stmt::PauseStmt(_) => None,
            ast::Stmt::QuitStmt(_) => None,
            ast::Stmt::BreakStmt(_) => None,
            ast::Stmt::CheckednessStmt(_) => None,
            ast::Stmt::PreStmt(_) => None,
            ast::Stmt::InitStmt(_) => None,
            ast::Stmt::PostStmt(_) => None,
            ast::Stmt::HandlerStmt(_) => None,
            ast::Stmt::InheritStmt(_) => None,
            ast::Stmt::ImplementStmt(_) => None,
            ast::Stmt::ImplementByStmt(_) => None,
            ast::Stmt::ImportStmt(_) => None,
            ast::Stmt::ExportStmt(_) => None,
            ast::Stmt::PreprocGlob(_) => None,
        };

        if id.is_none() {
            self.errors
                .push(BodyLowerError::UnhandledStatement { stmt: ptr });
        }

        id
    }

    fn lookup_item<T: item::ItemAstId<'db>>(&self, ast_id: AstId<T>) -> T::Item {
        let Some(child_items) = self.child_items.last() else {
            panic!("not in a scope with items")
        };
        child_items.get_item(self.db, SemanticLoc::new(self.file, ast_id))
    }

    fn lower_constvar_init(
        &mut self,
        node: ast::ConstVarDecl,
        stmts: &mut Vec<LocalStmt<'db>>,
    ) -> Option<()> {
        // for now: treat all constvars and items the same (get referenced as stmts)
        // and then lower them differently later
        //
        // The initializer expression is always the same
        let names = node.constvar_names()?;
        let init = self.lower_expr_opt(node.init());

        for name in names.names() {
            // Initializer gets a consistent place for items
            match self.ast_id_map.lookup_for_maybe(&name) {
                Some(ast_id) => {
                    let item = self.lookup_item(ast_id);
                    let stmt_id =
                        self.alloc_stmt(stmt::Stmt::InitializeConstVar(item, init), node.clone());
                    stmts.push(stmt_id);
                }
                None => {
                    // FIXME: Alloc local
                    return None;
                }
            }
        }

        Some(())
    }

    fn lower_assign_stmt(&mut self, node: ast::AssignStmt) -> LocalStmt<'db> {
        let op = node
            .asn_op()
            .and_then(|op| op.asn_kind())
            .and_then(|op| op.as_binary_op());

        let lhs = self.lower_expr_opt(node.lhs());
        let rhs = self.lower_expr_opt(node.rhs());

        self.alloc_stmt(stmt::Stmt::Assign(stmt::Assign { lhs, op, rhs }), node)
    }

    fn lower_put_stmt(&mut self, node: ast::PutStmt) -> LocalStmt<'db> {
        let stream_num = node
            .stream_num()
            .and_then(|stream| Some(self.lower_expr(stream.expr()?)));

        let items = node
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
                                width: self.lower_expr_opt(width),
                                precision: self.lower_expr_opt(precision),
                                exponent_width: self.lower_expr_opt(Some(exponent_width)),
                            }
                        } else if let Some(precision) = precision {
                            stmt::PutOpts::WithPrecision {
                                width: self.lower_expr_opt(width),
                                precision: self.lower_expr_opt(Some(precision)),
                            }
                        } else if let Some(width) = width {
                            stmt::PutOpts::WithWidth {
                                width: self.lower_expr_opt(Some(width)),
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
        let append_newline = node.range_token().is_none();

        // Note: we're being a little more lenient than the original one since
        // - the parser should make an error if there isn't any items
        // - is easier to just make it always
        self.alloc_stmt(
            stmt::Stmt::Put(stmt::Put {
                stream_num,
                items,
                append_newline,
            }),
            node,
        )
    }

    fn lower_block_stmt(&mut self, node: ast::BlockStmt) -> LocalStmt<'db> {
        let stmts = node.stmt_list().unwrap();

        let module_block = self
            .ast_id_map
            .lookup_for_maybe(&stmts)
            .map(|ast_id| item::ModuleBlock::new(self.db, SemanticLoc::new(self.file, ast_id)));
        if let Some(module_block) = module_block {
            self.child_items.push(module_block.child_items(self.db));
        }

        // Lower child statements
        let mut child_stmts = Vec::with_capacity(stmts.stmts().count());

        for stmt in stmts.stmts() {
            let Some(stmt_id) = self.lower_statement(stmt, &mut child_stmts) else {
                continue;
            };
            child_stmts.push(stmt_id);
        }

        assert_eq!(
            self.child_items.pop(),
            module_block.map(|block| block.child_items(self.db))
        );

        self.alloc_stmt(
            stmt::Stmt::Block(stmt::Block {
                module_block,
                kind: stmt::BlockKind::Normal,
                stmts: child_stmts.into(),
            }),
            node,
        )
    }

    fn alloc_stmt(&mut self, stmt: stmt::Stmt<'db>, node: impl Into<ast::Stmt>) -> LocalStmt<'db> {
        let place = self.contents.stmts.alloc(stmt);
        self.spans
            .stmts
            .insert(place, UnstableSemanticLoc::new(self.file, &node.into()));

        LocalStmt(place)
    }

    fn lower_expr_opt(&mut self, node: Option<ast::Expr>) -> LocalExpr<'db> {
        match node {
            Some(expr) => self.lower_expr(expr),
            None => self.missing_expr(),
        }
    }

    fn missing_expr(&mut self) -> LocalExpr<'db> {
        // Missing exprs don't have a matching node to attach to
        let place = self.contents.exprs.alloc(expr::Expr::Missing);
        LocalExpr(place)
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> LocalExpr<'db> {
        // only needed for pointing to unhandled expressions
        let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &expr).into();

        let id = match expr {
            ast::Expr::LiteralExpr(node) => Some(self.lower_literal_expr(node)),
            ast::Expr::ObjClassExpr(_) => None,
            ast::Expr::InitExpr(_) => None,
            ast::Expr::NilExpr(_) => None,
            ast::Expr::SizeOfExpr(_) => None,
            ast::Expr::BinaryExpr(_) => None,
            ast::Expr::UnaryExpr(_) => None,
            ast::Expr::ParenExpr(_) => None,
            ast::Expr::NameExpr(node) => Some(self.lower_name_expr(node)),
            ast::Expr::SelfExpr(_) => None,
            ast::Expr::FieldExpr(_) => None,
            ast::Expr::DerefExpr(_) => None,
            ast::Expr::CheatExpr(_) => None,
            ast::Expr::NatCheatExpr(_) => None,
            ast::Expr::ArrowExpr(_) => None,
            ast::Expr::IndirectExpr(_) => None,
            ast::Expr::BitsExpr(_) => None,
            ast::Expr::CallExpr(_) => None,
        };

        match id {
            Some(id) => id,
            None => {
                self.errors
                    .push(BodyLowerError::UnhandledExpression { expr: ptr });
                self.missing_expr()
            }
        }
    }

    fn lower_literal_expr(&mut self, node: ast::LiteralExpr) -> LocalExpr<'db> {
        let (value, errs) = node.literal().expect("aaaa");

        if let Some(errors) = errs {
            let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &node).into();

            self.errors
                .push(BodyLowerError::LiteralParseError { expr: ptr, errors })
        }

        let value = match value {
            LiteralValue::Int(v) => expr::Literal::Integer(v),
            LiteralValue::Real(v) => expr::Literal::Real(v.into()),
            LiteralValue::Char(v) if v.is_empty() => return self.missing_expr(),
            LiteralValue::Char(v) if v.len() == 1 => expr::Literal::Char(v.chars().next().unwrap()),
            LiteralValue::Char(v) => expr::Literal::CharSeq(v),
            LiteralValue::String(v) => expr::Literal::String(v),
            LiteralValue::Boolean(v) => expr::Literal::Boolean(v),
        };

        self.alloc_expr(expr::Expr::Literal(value), node)
    }

    fn lower_name_expr(&mut self, node: ast::NameExpr) -> LocalExpr<'db> {
        let Some(name) = node
            .name_ref()
            .and_then(|name_ref| name_ref.identifier_token())
        else {
            return self.missing_expr();
        };

        self.alloc_expr(
            expr::Expr::Name(expr::Name::Name(Symbol::new(
                self.db,
                name.text().to_owned(),
            ))),
            node,
        )
    }

    fn alloc_expr(&mut self, expr: expr::Expr<'db>, node: impl Into<ast::Expr>) -> LocalExpr<'db> {
        let place = self.contents.exprs.alloc(expr);
        self.spans
            .exprs
            .insert(place, UnstableSemanticLoc::new(self.file, &node.into()));

        LocalExpr(place)
    }
}

// Errors encountered during the body lowering process
pub enum BodyLowerError<'db> {
    /// This statement isn't lowered yet
    UnhandledStatement { stmt: SemanticNodePtr<'db> },
    /// This expression isn't lowered yet
    UnhandledExpression { expr: SemanticNodePtr<'db> },
    /// Error while parsing a literal expression
    LiteralParseError {
        expr: SemanticNodePtr<'db>,
        errors: toc_syntax::LiteralParseError,
    },
}
