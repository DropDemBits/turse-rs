//! Lowering from AST expressions and statements into a Body

use toc_hir_expand::{
    AstLocations, SemanticFile, SemanticLoc, SemanticNodePtr, UnstableSemanticLoc,
};
use toc_syntax::ast::{self, AstNode};

use crate::{
    body::ModuleBlock,
    stmt::{self, LocalStmt},
    Db,
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

    fn lower_statement(&mut self, stmt: ast::Stmt) -> Vec<LocalStmt> {
        // only needed for pointing to unhandled statements
        let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &stmt).into();

        let ids = match stmt {
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

        match ids {
            Some(ids) => ids,
            None => {
                self.errors
                    .push(BodyLowerError::UnhandledStatement { stmt: ptr });
                vec![]
            }
        }
    }

    fn lower_constvar_init(&mut self, node: ast::ConstVarDecl) -> Vec<LocalStmt> {
        // for now: treat all constvars and items the same (get referenced as stmts)
        // and then lower them differently later

        // Initializer gets a consistent place for items
        let loc = self.ast_locations.get(&node);
        let place = self
            .contents
            .stmts
            .alloc(stmt::Stmt::InitializeConstVar(loc));
        self.spans.stmts.insert(
            place,
            loc.map_unstable(self.db.up(), |node| ast::Stmt::ConstVarDecl(node)),
        );

        vec![LocalStmt(place)]
    }

    fn lower_block_stmt(&mut self, node: ast::BlockStmt) -> Vec<LocalStmt> {
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
        let mut child_stmts = vec![];
        {
            for stmt in stmts.stmts() {
                let new_stmts = self.lower_statement(stmt);
                child_stmts.extend(new_stmts.into_iter());
            }
        }

        let place = self.contents.stmts.alloc(stmt::Stmt::Block(stmt::Block {
            module_block,
            kind: stmt::BlockKind::Normal,
            stmts: child_stmts.into(),
        }));
        self.spans.stmts.insert(
            place,
            UnstableSemanticLoc::new(self.file, &ast::Stmt::BlockStmt(node)),
        );

        vec![LocalStmt(place)]
    }
}

// Errors encountered during the body lowering process
pub enum BodyLowerError {
    /// This statement isn't lowered yet
    UnhandledStatement { stmt: SemanticNodePtr },
}
