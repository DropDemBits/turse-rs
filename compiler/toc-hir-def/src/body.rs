use std::sync::Arc;

use la_arena::{Arena, ArenaMap};
use toc_hir_expand::{SemanticLoc, UnstableSemanticLoc};
use toc_syntax::ast;

use crate::{
    expr,
    stmt::{self, StmtId},
    Db,
};

pub(crate) mod lower;

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
