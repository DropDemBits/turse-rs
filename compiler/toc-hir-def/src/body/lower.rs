//! Lowering from AST expressions and statements into a Body

use toc_hir_expand::{
    AstLocations, SemanticFile, SemanticLoc, SemanticNodePtr, UnstableSemanticLoc,
};
use toc_syntax::{
    LiteralValue,
    ast::{self, AstNode},
};

use crate::{
    Db, Symbol,
    body::ModuleBlock,
    expr::{self, LocalExpr},
    stmt::{self, LocalStmt},
};

use super::{Body, BodyContents, BodySpans};

pub(crate) fn module_body(
    db: &dyn Db,
    body: Body,
    stmt_list: SemanticLoc<ast::StmtList>,
) -> (BodyContents, BodySpans, Vec<BodyLowerError>) {
    let file = stmt_list.file(db.up());
    let ast_locations = file.ast_locations(db.up());
    let stmt_list = stmt_list.to_node(db.up());
    BodyLower::new(db, file, ast_locations, body, BodyMode::Module).lower_from_stmts(stmt_list)
}

pub(crate) enum BodyMode {
    Module,
    Function,
}

struct BodyLower<'db> {
    db: &'db dyn Db,
    file: SemanticFile,
    ast_locations: &'db AstLocations,

    body: Body,
    mode: BodyMode,
    contents: BodyContents,
    spans: BodySpans,

    errors: Vec<BodyLowerError>,
}

impl<'db> BodyLower<'db> {
    fn new(
        db: &'db dyn Db,
        file: SemanticFile,
        ast_locations: &'db AstLocations,
        body: Body,
        mode: BodyMode,
    ) -> Self {
        Self {
            db,
            ast_locations,
            file,
            body,
            mode,
            contents: Default::default(),
            spans: Default::default(),
            errors: Default::default(),
        }
    }

    fn lower_from_stmts(
        mut self,
        root: ast::StmtList,
    ) -> (BodyContents, BodySpans, Vec<BodyLowerError>) {
        let has_items = root.stmts().any(|node| {
            let kind = node.syntax().kind();
            ast::Item::can_cast(kind) || ast::PreprocGlob::can_cast(kind)
        });

        self.contents.root_block = has_items.then(|| {
            let loc = self.ast_locations.get(&root);
            ModuleBlock::new(self.db, loc)
        });

        let mut top_level = vec![];
        for stmt in root.stmts() {
            let new_stmts = self.lower_statement(stmt);
            top_level.extend(new_stmts.into_iter().map(|id| id.in_body(self.body)));
        }

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

    fn lower_statement(&mut self, stmt: ast::Stmt) -> Option<LocalStmt> {
        // only needed for pointing to unhandled statements
        let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &stmt).into();

        let id = match stmt {
            ast::Stmt::ConstVarDecl(node) => Some(self.lower_constvar_init(node)),

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

    fn lower_constvar_init(&mut self, node: ast::ConstVarDecl) -> LocalStmt {
        // for now: treat all constvars and items the same (get referenced as stmts)
        // and then lower them differently later

        // Initializer gets a consistent place for items
        let loc = self.ast_locations.get(&node);

        // The initializer expression is always the same however
        let init = self.lower_expr_opt(node.init());

        self.alloc_stmt(stmt::Stmt::InitializeConstVar(loc, init), node)
    }

    fn lower_assign_stmt(&mut self, node: ast::AssignStmt) -> LocalStmt {
        let op = node
            .asn_op()
            .and_then(|op| op.asn_kind())
            .and_then(|op| op.as_binary_op());

        let lhs = self.lower_expr_opt(node.lhs());
        let rhs = self.lower_expr_opt(node.rhs());

        self.alloc_stmt(stmt::Stmt::Assign(stmt::Assign { lhs, op, rhs }), node)
    }

    fn lower_put_stmt(&mut self, node: ast::PutStmt) -> LocalStmt {
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

    fn lower_block_stmt(&mut self, node: ast::BlockStmt) -> LocalStmt {
        let stmts = node.stmt_list().unwrap();

        let has_items = stmts.stmts().any(|node| {
            let kind = node.syntax().kind();
            ast::Item::can_cast(kind) || ast::PreprocGlob::can_cast(kind)
        });

        let module_block = has_items.then(|| {
            let loc = self.ast_locations.get(&stmts);
            ModuleBlock::new(self.db, loc)
        });

        // Lower child statements
        let child_stmts = stmts
            .stmts()
            .flat_map(|stmt| self.lower_statement(stmt))
            .collect::<Vec<_>>();

        self.alloc_stmt(
            stmt::Stmt::Block(stmt::Block {
                module_block,
                kind: stmt::BlockKind::Normal,
                stmts: child_stmts.into(),
            }),
            node,
        )
    }

    fn alloc_stmt(&mut self, stmt: stmt::Stmt, node: impl Into<ast::Stmt>) -> LocalStmt {
        let place = self.contents.stmts.alloc(stmt);
        self.spans
            .stmts
            .insert(place, UnstableSemanticLoc::new(self.file, &node.into()));

        LocalStmt(place)
    }

    fn lower_expr_opt(&mut self, node: Option<ast::Expr>) -> LocalExpr {
        match node {
            Some(expr) => self.lower_expr(expr),
            None => self.missing_expr(),
        }
    }

    fn missing_expr(&mut self) -> LocalExpr {
        // Missing exprs don't have a matching node to attach to
        let place = self.contents.exprs.alloc(expr::Expr::Missing);
        LocalExpr(place)
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> LocalExpr {
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

    fn lower_literal_expr(&mut self, node: ast::LiteralExpr) -> LocalExpr {
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

    fn lower_name_expr(&mut self, node: ast::NameExpr) -> LocalExpr {
        let Some(name) = node
            .name_ref()
            .and_then(|name_ref| name_ref.identifier_token())
        else {
            return self.missing_expr();
        };

        self.alloc_expr(
            expr::Expr::Name(expr::Name::Name(Symbol::new(self.db, name.text().into()))),
            node,
        )
    }

    fn alloc_expr(&mut self, expr: expr::Expr, node: impl Into<ast::Expr>) -> LocalExpr {
        let place = self.contents.exprs.alloc(expr);
        self.spans
            .exprs
            .insert(place, UnstableSemanticLoc::new(self.file, &node.into()));

        LocalExpr(place)
    }
}

// Errors encountered during the body lowering process
pub enum BodyLowerError {
    /// This statement isn't lowered yet
    UnhandledStatement { stmt: SemanticNodePtr },
    /// This expression isn't lowered yet
    UnhandledExpression { expr: SemanticNodePtr },
    /// Error while parsing a literal expression
    LiteralParseError {
        expr: SemanticNodePtr,
        errors: toc_syntax::LiteralParseError,
    },
}
