//! Body structures

use std::fmt;

use la_arena::Arena;
use toc_span::SpanId;

use crate::{expr, stmt, symbol::LocalDefId};

pub use crate::ids::BodyId;

/// A bundle of executable code.
/// Represents either a statement group (fn body),
/// or an expression group (constvar init or type expr).
#[derive(PartialEq, Eq)]
pub struct Body {
    pub kind: BodyKind,
    /// Span covering the whole body
    pub span: SpanId,
    pub(crate) exprs: Arena<expr::Expr>,
    pub(crate) stmts: Arena<stmt::Stmt>,
}

impl fmt::Debug for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Body")
            .field("kind", &self.kind)
            .field("span", &self.span)
            .finish_non_exhaustive()
    }
}

impl Body {
    pub fn expr(&self, expr_id: expr::ExprId) -> &expr::Expr {
        &self.exprs[expr_id.into()]
    }

    pub fn stmt(&self, stmt_id: stmt::StmtId) -> &stmt::Stmt {
        &self.stmts[stmt_id.into()]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BodyKind {
    /// Bundle of statements, with the given statement list, parameter definition list,
    /// and optional result name (e.g. for module initializers, or function bodies).
    ///
    /// For BodyDecl, the parameter list would be cloned from the associated definition if
    /// not specified.
    Stmts(Vec<stmt::StmtId>, Vec<LocalDefId>, Option<LocalDefId>),
    /// Bundle of expressions, with the given expression root
    /// (e.g. for a `ConstVar` initializer, or an expression in a type).
    Exprs(expr::ExprId),
}
