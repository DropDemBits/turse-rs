//! Analysis query system definitions

use toc_hir::{
    body::BodyId,
    expr::{BodyExpr, ExprId},
    item::ItemId,
    library::{InLibrary, LibraryId},
    symbol,
    symbol::DefId,
    ty::TypeId as HirTypeId,
};
use toc_salsa::salsa;

use crate::ty;

#[salsa::query_group(TypeInternStorage)]
pub trait TypeIntern: toc_hir_db::db::HirDatabase {
    /// Interns the given type.
    #[salsa::interned]
    fn intern_type(&self, ty: ty::TypeData) -> ty::TypeId;
}

/// Type database
#[salsa::query_group(TypeDatabaseStorage)]
pub trait TypeDatabase: TypeIntern + TypeInternExt {
    /// Converts the HIR type into an analysis form
    #[salsa::invoke(ty::query::from_hir_type)]
    fn from_hir_type(&self, type_id: InLibrary<HirTypeId>) -> ty::TypeId;

    /// Gets the type of the given type source.
    #[salsa::invoke(ty::query::type_of)]
    fn type_of(&self, source: TypeSource) -> ty::TypeId;

    #[salsa::invoke(ty::query::value_produced)]
    fn value_produced(&self, source: ValueSource) -> Result<ValueKind, NotValue>;
}

/// Helpers for working with the type interner
pub trait TypeInternExt {
    // Helper creators
    fn mk_error(&self) -> ty::TypeId;
    fn mk_boolean(&self) -> ty::TypeId;
    fn mk_int(&self, kind: ty::IntSize) -> ty::TypeId;
    fn mk_nat(&self, kind: ty::NatSize) -> ty::TypeId;
    fn mk_real(&self, kind: ty::RealSize) -> ty::TypeId;
    fn mk_integer(&self) -> ty::TypeId;
    fn mk_char(&self) -> ty::TypeId;
    fn mk_string(&self) -> ty::TypeId;
    fn mk_char_n(&self, seq_size: ty::SeqSize) -> ty::TypeId;
    fn mk_string_n(&self, seq_size: ty::SeqSize) -> ty::TypeId;
    fn mk_alias(&self, def_id: DefId, base_ty: ty::TypeId) -> ty::TypeId;
    fn mk_forward(&self) -> ty::TypeId;
    fn mk_subprogram(
        &self,
        kind: symbol::SubprogramKind,
        params: Option<Vec<ty::Param>>,
        result: ty::TypeId,
    ) -> ty::TypeId;
    fn mk_void(&self) -> ty::TypeId;
}

/// Anything which can produce a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeSource {
    Def(DefId),
    Item(LibraryId, ItemId),
    BodyExpr(LibraryId, BodyExpr),
    Body(LibraryId, BodyId),
}

impl From<DefId> for TypeSource {
    fn from(id: DefId) -> Self {
        Self::Def(id)
    }
}

impl From<InLibrary<ItemId>> for TypeSource {
    fn from(id: InLibrary<ItemId>) -> Self {
        Self::Item(id.0, id.1)
    }
}

impl From<InLibrary<BodyExpr>> for TypeSource {
    fn from(id: InLibrary<BodyExpr>) -> Self {
        Self::BodyExpr(id.0, id.1)
    }
}

impl From<InLibrary<BodyId>> for TypeSource {
    fn from(id: InLibrary<BodyId>) -> Self {
        Self::Body(id.0, id.1)
    }
}

impl From<(LibraryId, BodyExpr)> for TypeSource {
    fn from(id: (LibraryId, BodyExpr)) -> Self {
        Self::BodyExpr(id.0, id.1)
    }
}

impl From<(LibraryId, BodyId, ExprId)> for TypeSource {
    fn from(id: (LibraryId, BodyId, ExprId)) -> Self {
        Self::BodyExpr(id.0, BodyExpr(id.1, id.2))
    }
}

impl From<(LibraryId, BodyId)> for TypeSource {
    fn from(id: (LibraryId, BodyId)) -> Self {
        Self::Body(id.0, id.1)
    }
}

impl From<(LibraryId, ItemId)> for TypeSource {
    fn from(id: (LibraryId, ItemId)) -> Self {
        Self::Item(id.0, id.1)
    }
}

/// Value produced from a [`ValueSource`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Scalar,
    Register(symbol::Mutability),
    Reference(symbol::Mutability),
}

impl ValueKind {
    pub fn is_value(self) -> bool {
        // no-op, but for matching semantics of other predicates
        true
    }

    pub fn is_ref(self) -> bool {
        matches!(self, Self::Reference(_) | Self::Register(_))
    }

    pub fn is_ref_mut(self) -> bool {
        matches!(
            self,
            Self::Reference(symbol::Mutability::Var) | Self::Register(symbol::Mutability::Var)
        )
    }

    pub fn is_storage_backed(self) -> bool {
        matches!(self, Self::Reference(_))
    }

    pub fn is_storage_backed_mut(self) -> bool {
        matches!(self, Self::Reference(symbol::Mutability::Var))
    }

    pub fn is_register(self) -> bool {
        matches!(self, Self::Register(_))
    }
}

/// Not values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NotValue {
    NotValue,
    Missing,
}

/// Anything that can produce a value
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueSource {
    DefId(DefId),
    Body(LibraryId, BodyId),
    BodyExpr(LibraryId, BodyExpr),
}

impl From<symbol::DefId> for ValueSource {
    fn from(def_id: symbol::DefId) -> Self {
        Self::DefId(def_id)
    }
}

impl From<(LibraryId, BodyId)> for ValueSource {
    fn from((lib_id, body): (LibraryId, BodyId)) -> Self {
        Self::Body(lib_id, body)
    }
}

impl From<(LibraryId, BodyExpr)> for ValueSource {
    fn from(expr: (LibraryId, BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(LibraryId, BodyId, ExprId)> for ValueSource {
    fn from(expr: (LibraryId, BodyId, ExprId)) -> Self {
        Self::BodyExpr(expr.0, BodyExpr(expr.1, expr.2))
    }
}

pub trait NotValueErrExt {
    fn or_missing(self) -> bool;
}

impl NotValueErrExt for Result<bool, NotValue> {
    fn or_missing(self) -> bool {
        self.unwrap_or_else(|err| matches!(err, NotValue::Missing))
    }
}
