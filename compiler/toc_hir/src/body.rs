//! Body structures

use std::fmt;

use la_arena::{Arena, ArenaMap, Idx};
use toc_span::SpanId;

use crate::{expr, ids::BodyIndex, item, stmt, symbol::LocalDefId, ty};

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
        &self.exprs[Idx::from(expr_id)]
    }

    pub fn stmt(&self, stmt_id: stmt::StmtId) -> &stmt::Stmt {
        &self.stmts[Idx::from(stmt_id)]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BodyKind {
    /// Bundle of statements (e.g. for module initializers, or function bodies)
    /// with the given statement list, parameter definition list, and optional result name.
    ///
    /// For BodyDecl, the parameter list would be cloned from the associated definition if
    /// not specified.
    Stmts(Vec<stmt::StmtId>, Vec<LocalDefId>, Option<LocalDefId>),
    /// Bundle of expressions, with the given expression root
    /// (e.g. for a `ConstVar` initializer, or an expression in a type).
    Exprs(expr::ExprId),
}

/// Owner of a [`Body`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BodyOwner {
    Item(item::ItemId),
    Type(ty::TypeId),
    Expr(expr::BodyExpr),
    // Stmts can be body owners, but `case` currently users normal exprs
}

/// Mapping between a [`BodyId`] and the corresponding [`BodyOwner`]
#[derive(Debug, Default, PartialEq, Eq)]
pub struct BodyTable {
    body_owners: ArenaMap<BodyIndex, BodyOwner>,
}

impl BodyTable {
    pub fn add_owner(&mut self, body_id: BodyId, owner: BodyOwner) {
        self.body_owners.insert(body_id.0, owner);
    }

    pub fn get_owner(&self, body_id: BodyId) -> Option<BodyOwner> {
        self.body_owners.get(body_id.0).copied()
    }
}
