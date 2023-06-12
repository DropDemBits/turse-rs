use std::sync::Arc;

use la_arena::{Arena, ArenaMap};
use toc_hir_expand::{SemanticLoc, UnstableSemanticLoc};
use toc_syntax::ast;

use crate::{
    expr,
    stmt::{self, StmtId},
    Db,
};

pub(crate) mod lower {
    use toc_hir_expand::{
        AstLocations, SemanticFile, SemanticLoc, SemanticNodePtr, UnstableSemanticLoc,
    };
    use toc_syntax::ast;

    use crate::{stmt::LocalStmt, Db};

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

        fn lower_statement(&mut self, stmt: ast::Stmt) -> Vec<LocalStmt> {
            // only needed for pointing to unhandled statements
            let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &stmt).into();

            let ids = match stmt {
                _item_stmt @ ast::Stmt::ConstVarDecl(_) => {
                    // for now: treat all constvars and items the same (get referenced as stmts)
                    // and then lower them differently later
                    return vec![];
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
                    return vec![];
                }

                ast::Stmt::AssignStmt(_) => None,
                ast::Stmt::OpenStmt(_) => None,
                ast::Stmt::CloseStmt(_) => None,
                ast::Stmt::PutStmt(_) => None,
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
                ast::Stmt::BlockStmt(_) => None,
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

            match ids {
                Some(ids) => ids,
                None => {
                    self.errors
                        .push(BodyLowerError::UnhandledStatement { stmt: ptr });
                    vec![]
                }
            }
        }
    }

    // Errors encountered during the body lowering process
    pub enum BodyLowerError {
        /// This statement isn't lowered yet
        UnhandledStatement { stmt: SemanticNodePtr },
    }
}

#[salsa::tracked]
pub struct Body {
    origin: BodyOrigin,
}

// either: attached to a module-like, or a function-like
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BodyOrigin {
    ModuleBody(SemanticLoc<ast::StmtList>),
    FunctionBody(SemanticLoc<ast::StmtList>),
}

// Lowered body contents
#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct BodyContents {
    exprs: Arena<expr::Expr>,
    stmts: Arena<stmt::Stmt>,
    top_level: Box<[StmtId]>,
}

// Per-item spans
#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct BodySpans {
    exprs: ArenaMap<expr::ExprIndex, UnstableSemanticLoc<ast::Expr>>,
    stmts: ArenaMap<stmt::StmtIndex, UnstableSemanticLoc<ast::Stmt>>,
}

#[salsa::tracked]
impl Body {
    // Top-level stmts
    #[salsa::tracked(return_ref)]
    pub fn top_level_stmts(self, db: &dyn Db) -> Box<[StmtId]> {
        self.contents(db).top_level.clone()
    }

    #[salsa::tracked(return_ref)]
    pub(crate) fn contents(self, db: &dyn Db) -> Arc<BodyContents> {
        self.lower_contents(db).0.clone()
    }

    // Parts we wanna extract:
    // - Stmts + Exprs
    // - SpanMap<Expr> & SpanMap<Stmt>
    #[salsa::tracked]
    pub(crate) fn lower_contents(self, db: &dyn Db) -> (Arc<BodyContents>, Arc<BodySpans>) {
        // FIXME: Accumulate errors
        let (contents, spans, _errors) = match self.origin(db) {
            BodyOrigin::ModuleBody(stmts) => lower::module_body(db, self, stmts),
            // Note that if we're an FnBody then we can have ConstVars as locals
            BodyOrigin::FunctionBody(_stmts) => todo!(),
        };

        (Arc::new(contents), Arc::new(spans))
    }
}
