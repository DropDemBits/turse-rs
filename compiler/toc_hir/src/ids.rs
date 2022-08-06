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

impl DefId {
    pub fn library(self) -> LibraryId {
        self.0
    }

    pub fn local(self) -> LocalDefId {
        self.1
    }
}

impl fmt::Debug for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(lib_id, local_def) = self;
        f.write_fmt(format_args!("DefId({lib_id:?}, {local_def:?})"))
    }
}

crate::arena_id_wrapper!(
    /// A library local reference to an item.
    pub struct ItemId(item::Item);
    /// Alias for the item arena index
    pub(crate) type ItemIndex = Index;
);

/// A library local reference to a specific module-like item (e.g. [`Module`])
///
/// [`Module`]: crate::item::Module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ModuleId(pub(crate) ItemId);

impl ModuleId {
    /// Makes a new [`ModuleId`], run-time verified to only accept module-likes
    ///
    /// ## Panics
    /// If the [`Item`] from [`ItemId`] isn't a [`ModuleLike`]
    ///
    /// [`Item`]: crate::item::Item
    /// [`ModuleLike`]: crate::item::ModuleLike
    pub fn new(in_library: &crate::library::Library, item_id: ItemId) -> Self {
        Self::try_new(in_library, item_id).expect("only module-likes accepted")
    }

    /// Makes a new [`ModuleId`], returning `None` if it doesn't point to a module-like
    ///
    /// [`ModuleLike`]: crate::item::ModuleLike
    pub fn try_new(in_library: &crate::library::Library, item_id: ItemId) -> Option<Self> {
        if !matches!(
            in_library.item(item_id).kind,
            crate::item::ItemKind::Module(_)
        ) {
            return None;
        }

        Some(Self(item_id))
    }

    pub fn item_id(self) -> ItemId {
        self.0
    }
}

/// A module-local reference to a specific export
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ExportId(pub usize);

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

impl BodyStmt {
    pub fn with_stmt(self, stmt: StmtId) -> Self {
        BodyStmt(self.0, stmt)
    }

    pub fn body(self) -> BodyId {
        self.0
    }

    pub fn stmt(self) -> StmtId {
        self.1
    }
}

/// Uniquely identifies an expression within a library
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyExpr(pub BodyId, pub ExprId);

impl BodyExpr {
    pub fn with_expr(self, expr: ExprId) -> Self {
        BodyExpr(self.0, expr)
    }

    pub fn body(self) -> BodyId {
        self.0
    }

    pub fn expr(self) -> ExprId {
        self.1
    }
}

impl ExprId {
    pub fn in_body(self, body_id: BodyId) -> BodyExpr {
        BodyExpr(body_id, self)
    }
}

impl StmtId {
    pub fn in_body(self, body_id: body::BodyId) -> BodyStmt {
        BodyStmt(body_id, self)
    }
}

/// An interned reference to a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeId(pub(crate) TypeIndex);
pub(crate) type TypeIndex = std::num::NonZeroU32;

/// A type-local reference to a specific field
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FieldId(pub usize);
