//! Id structs used in the HIR tree

use std::fmt;

use crate::{body, expr, item, stmt, symbol};

/// A reference to a library in the library graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LibraryId(pub(crate) LibraryIndex);
pub(crate) type LibraryIndex = u32;

crate::arena_id_wrapper!(
    /// A library local reference to a definition.
    pub struct LocalDefId(symbol::DefInfo);
    /// Alias for the definition arena index
    pub(crate) type LocalDefIndex = Index;
);

impl LocalDefId {
    /// Creates a new `LocalDefId`
    ///
    /// Only to be used during testing
    pub fn new(id: u32) -> Self {
        let raw = la_arena::RawIdx::from(id);
        Self(la_arena::Idx::from_raw(raw))
    }
}

/// A library independent reference to a definition
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub LibraryId, pub LocalDefId);

impl fmt::Debug for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(lib_id, local_def) = self;
        f.write_fmt(format_args!("DefId({lib_id:?}, {local_def:?})"))
    }
}

crate::arena_id_wrapper!(
    /// A library local reference to an item.
    pub struct ItemId(item::Item);
);

crate::arena_id_wrapper!(
    /// A library local reference to a body.
    pub struct BodyId(body::Body);
    /// Alias for the body arena index
    pub(crate) type BodyIndex = Index;
);

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to an expression.
    ///
    /// [`Body`]: crate::body::Body
    pub struct ExprId(expr::Expr);
);

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to a statement.
    ///
    /// [`Body`]: crate::body::Body
    pub struct StmtId(stmt::Stmt);
);

/// Uniquely identifies a statement within a library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyStmt(pub body::BodyId, pub StmtId);

/// Uniquely identifies an expression within a library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyExpr(pub BodyId, pub ExprId);

impl ExprId {
    pub fn in_body(self, body: BodyId) -> BodyExpr {
        BodyExpr(body, self)
    }
}

impl StmtId {
    pub fn in_body(self, body: body::BodyId) -> BodyStmt {
        BodyStmt(body, self)
    }
}

/// An interned reference to a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeId(pub(crate) TypeIndex);
pub(crate) type TypeIndex = std::num::NonZeroU32;
