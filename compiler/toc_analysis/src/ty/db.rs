//! Analysis query system definitions

use toc_hir::{
    body::BodyId,
    expr::{BodyExpr, ExprId},
    library::{InLibrary, LibraryId},
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
    fn mk_ref(&self, mutability: ty::Mutability, to: ty::TypeId) -> ty::TypeId;
}

/// Anything which can produce a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeSource {
    Def(DefId),
    BodyExpr(LibraryId, BodyExpr),
    Body(LibraryId, BodyId),
}

impl From<DefId> for TypeSource {
    fn from(id: DefId) -> Self {
        Self::Def(id)
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
