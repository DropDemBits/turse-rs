use std::sync::Arc;

use la_arena::Arena;
use toc_hir_expand::{SemanticLoc, UnstableSemanticLoc};
use toc_syntax::ast;

use crate::{expr, stmt, Db};

pub(crate) mod lower;
pub mod pretty;

/// Executable block of code
#[salsa::tracked]
pub struct Body {
    origin: BodyOrigin,
}

/// Where a `Body` comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BodyOrigin {
    /// Attached to a module-like item
    ModuleBody(SemanticLoc<ast::StmtList>),
    /// Attached to a function-like item
    FunctionBody(SemanticLoc<ast::StmtList>),
}

/// Lowered body contents
#[derive(Debug, Default, PartialEq, Eq)]
pub struct BodyContents {
    exprs: Arena<expr::Expr>,
    stmts: Arena<stmt::Stmt>,
    top_level: Box<[stmt::LocalStmt]>,
    root_block: Option<ModuleBlock>,
}

impl BodyContents {
    pub fn expr(&self, expr: expr::LocalExpr) -> &expr::Expr {
        &self.exprs[expr.0]
    }

    pub fn stmt(&self, stmt: stmt::LocalStmt) -> &stmt::Stmt {
        &self.stmts[stmt.0]
    }

    pub fn root_block(&self) -> Option<ModuleBlock> {
        self.root_block
    }
}

/// Spans of
#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct BodySpans {
    exprs: expr::ExprMap<UnstableSemanticLoc<ast::Expr>>,
    stmts: stmt::StmtMap<UnstableSemanticLoc<ast::Stmt>>,
}

#[salsa::tracked]
impl Body {
    // Top-level stmts
    #[salsa::tracked(return_ref)]
    pub fn top_level_stmts(self, db: &dyn Db) -> Box<[stmt::LocalStmt]> {
        self.contents(db).top_level.clone()
    }

    // FIXME: should be pub(crate), after making StmtId::get and ExprId::get
    #[salsa::tracked(return_ref)]
    pub fn contents(self, db: &dyn Db) -> Arc<BodyContents> {
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

/// A block that contains items
#[salsa::tracked]
pub struct ModuleBlock {
    origin: SemanticLoc<ast::StmtList>,
}

impl ModuleBlock {
    pub(crate) fn stmt_list(self, db: &dyn Db) -> SemanticLoc<ast::StmtList> {
        self.origin(db)
    }
}
