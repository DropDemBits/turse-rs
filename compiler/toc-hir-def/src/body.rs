use toc_hir_expand::{SemanticLoc, UnstableSemanticLoc};
use toc_salsa_collections::arena::SalsaArena;
use toc_syntax::ast;

use crate::{
    Db, expr,
    stmt::{self, StmtId},
};

pub(crate) mod lower;
pub mod pretty;

/// Executable block of code
#[salsa::tracked(debug)]
pub struct Body<'db> {
    origin: BodyOrigin<'db>,
}

/// Where a `Body` comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum BodyOrigin<'db> {
    /// Attached to a module-like item
    ModuleBody(SemanticLoc<'db, ast::StmtList>),
    /// Attached to a function-like item
    FunctionBody(SemanticLoc<'db, ast::StmtList>),
}

/// Lowered body contents
#[derive(Debug, Default, PartialEq, Eq, Hash, salsa::Update)]
pub struct BodyContents<'db> {
    exprs: SalsaArena<expr::Expr<'db>>,
    stmts: SalsaArena<stmt::Stmt<'db>>,
    top_level: Box<[StmtId<'db>]>,
    root_block: Option<ModuleBlock<'db>>,
}

impl<'db> BodyContents<'db> {
    pub fn expr(&self, expr: expr::LocalExpr<'db>) -> &expr::Expr<'db> {
        &self.exprs[expr.0]
    }

    pub fn stmt(&self, stmt: stmt::LocalStmt<'db>) -> &stmt::Stmt<'db> {
        &self.stmts[stmt.0]
    }

    pub fn root_block(&self) -> Option<ModuleBlock<'db>> {
        self.root_block
    }
}

/// Spans of
#[derive(Debug, Default, PartialEq, Eq, salsa::Update, Hash)]
pub(crate) struct BodySpans<'db> {
    exprs: expr::ExprMap<'db, UnstableSemanticLoc<ast::Expr>>,
    stmts: stmt::StmtMap<'db, UnstableSemanticLoc<ast::Stmt>>,
}

#[salsa::tracked(debug)]
pub(crate) struct BodyLowerResult<'db> {
    #[tracked]
    #[return_ref]
    contents: BodyContents<'db>,
    #[tracked]
    #[return_ref]
    spans: BodySpans<'db>,
}

#[salsa::tracked]
impl<'db> Body<'db> {
    // // Top-level stmts
    pub fn top_level_stmts(self, db: &'db dyn Db) -> &'db [StmtId<'db>] {
        &self.contents(db).top_level
    }

    // FIXME: should be pub(crate), after making StmtId::get and ExprId::get
    pub fn contents(self, db: &'db dyn Db) -> &'db BodyContents<'db> {
        self.lower_contents(db).contents(db)
    }

    // Parts we wanna extract:
    // - Stmts + Exprs
    // - SpanMap<Expr> & SpanMap<Stmt>
    #[salsa::tracked]
    pub(crate) fn lower_contents(self, db: &'db dyn Db) -> BodyLowerResult<'db> {
        // FIXME: Accumulate errors
        let (contents, spans, _errors) = match self.origin(db) {
            BodyOrigin::ModuleBody(stmts) => lower::module_body(db, self, stmts),
            // Note that if we're an FnBody then we can have ConstVars as locals
            BodyOrigin::FunctionBody(_stmts) => todo!(),
        };

        BodyLowerResult::new(db, contents, spans)
    }
}

/// A block that contains items
#[salsa::tracked(debug)]
pub struct ModuleBlock<'db> {
    origin: SemanticLoc<'db, ast::StmtList>,
}

impl<'db> ModuleBlock<'db> {
    pub(crate) fn stmt_list(self, db: &'db dyn Db) -> SemanticLoc<'db, ast::StmtList> {
        self.origin(db)
    }
}
