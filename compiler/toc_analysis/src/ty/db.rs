//! Analysis query system definitions

use std::sync::Arc;

use toc_hir::{
    body::BodyId,
    expr::{self, BodyExpr, ExprId},
    item::{self, ItemId},
    library::{InLibrary, LibraryId},
    symbol::{self, DefId},
    ty::TypeId as HirTypeId,
};
use toc_salsa::salsa;

use crate::{const_eval, ty};

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

    /// Gets what kind of value a [`ValueSource`] produces, or a [`NotValue`] if it doesn't produce one
    #[salsa::invoke(ty::query::value_produced)]
    fn value_produced(&self, source: ValueSource) -> Result<ValueKind, NotValue>;

    /// Gets the corresponding definition from the given [`BindingSource`], or `None` if there isn't one.
    /// This also performs definition resolution, so resolving the resultant [`DefId`] is unnecessary.
    #[salsa::invoke(ty::query::binding_def)]
    fn binding_def(&self, bind_src: BindingSource) -> Option<DefId>;

    /// Like [`Self::binding_def`], but does not perform definition resolution.
    /// Unless looking at the immediate def is necessary (e.g. determining if it's an import),
    /// then [`Self::binding_def`] should always be prefered.
    #[salsa::invoke(ty::query::unresolved_binding_def)]
    fn unresolved_binding_def(&self, bind_src: BindingSource) -> Option<DefId>;

    /// Gets the fields from the given [`FieldSource`]
    #[salsa::invoke(ty::query::fields_of)]
    fn fields_of(&self, source: FieldSource) -> Option<Arc<item::Fields>>;

    /// Finds the associated exporting def from the given [`BindingSource`], or `None` if there isn't any
    #[salsa::invoke(ty::query::find_exported_def)]
    fn exporting_def(&self, source: BindingSource) -> Option<DefId>;
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
    fn mk_opaque(&self, def_id: DefId, base_ty: ty::TypeId) -> ty::TypeId;
    fn mk_forward(&self) -> ty::TypeId;
    fn mk_constrained(
        &self,
        base_ty: ty::TypeId,
        start: const_eval::Const,
        end: ty::EndBound,
    ) -> ty::TypeId;
    fn mk_array(
        &self,
        sizing: ty::ArraySizing,
        ranges: Vec<ty::TypeId>,
        elem_ty: ty::TypeId,
    ) -> ty::TypeId;
    fn mk_enum(&self, with_def: ty::WithDef, variants: Vec<DefId>) -> ty::TypeId;
    fn mk_set(&self, with_def: ty::WithDef, elem_ty: ty::TypeId) -> ty::TypeId;
    fn mk_pointer(&self, checked: ty::Checked, target_ty: ty::TypeId) -> ty::TypeId;
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

/// Anything that can produce a reference to a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingSource {
    DefId(DefId),
    Body(LibraryId, BodyId),
    BodyExpr(LibraryId, BodyExpr),
}

impl From<symbol::DefId> for BindingSource {
    fn from(def_id: DefId) -> Self {
        Self::DefId(def_id)
    }
}

impl From<(LibraryId, BodyId)> for BindingSource {
    fn from((lib_id, body): (LibraryId, BodyId)) -> Self {
        Self::Body(lib_id, body)
    }
}

impl From<(LibraryId, expr::BodyExpr)> for BindingSource {
    fn from(expr: (LibraryId, expr::BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(LibraryId, BodyId, expr::ExprId)> for BindingSource {
    fn from(expr: (LibraryId, BodyId, expr::ExprId)) -> Self {
        Self::BodyExpr(expr.0, expr::BodyExpr(expr.1, expr.2))
    }
}

impl From<ValueSource> for BindingSource {
    fn from(src: ValueSource) -> Self {
        match src {
            ValueSource::Def(def_id) => Self::DefId(def_id),
            ValueSource::Body(lib_id, body_id) => Self::Body(lib_id, body_id),
            ValueSource::BodyExpr(lib_id, body_expr) => Self::BodyExpr(lib_id, body_expr),
        }
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

    pub fn mutability(self) -> Option<symbol::Mutability> {
        match self {
            ValueKind::Scalar => None,
            ValueKind::Register(m) | ValueKind::Reference(m) => Some(m),
        }
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
    Def(DefId),
    Body(LibraryId, BodyId),
    BodyExpr(LibraryId, BodyExpr),
}

impl ValueSource {
    pub fn span_of<DB>(self, db: &DB) -> toc_span::Span
    where
        DB: ?Sized + toc_hir_db::db::HirDatabase,
    {
        match self {
            ValueSource::Def(_def_id) => {
                unimplemented!("not sure if we need this")
            }
            ValueSource::Body(lib_id, body_id) => {
                let library = db.library(lib_id);
                library.body(body_id).span.lookup_in(&library)
            }
            ValueSource::BodyExpr(lib_id, BodyExpr(body_id, expr_id)) => {
                let library = db.library(lib_id);
                library.body(body_id).expr(expr_id).span.lookup_in(&library)
            }
        }
    }
}

impl From<DefId> for ValueSource {
    fn from(def_id: DefId) -> Self {
        Self::Def(def_id)
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

impl From<ValueSource> for TypeSource {
    fn from(src: ValueSource) -> Self {
        match src {
            ValueSource::Body(lib_id, body_id) => Self::Body(lib_id, body_id),
            ValueSource::BodyExpr(lib_id, body_id) => Self::BodyExpr(lib_id, body_id),
            ValueSource::Def(def_id) => TypeSource::Def(def_id),
        }
    }
}

pub trait NotValueErrExt {
    /// If it satisfies `is_predicate`, or is a [`NotValue::Missing`]
    fn is_missing_or(&self, is_predicate: impl FnOnce(ValueKind) -> bool) -> bool;
    /// If it isn't a [`NotValue::Missing`]
    fn is_any_value(&self) -> bool;
}

impl NotValueErrExt for Result<ValueKind, NotValue> {
    fn is_missing_or(&self, is_predicate: impl FnOnce(ValueKind) -> bool) -> bool {
        self.map_or_else(|err| matches!(err, NotValue::Missing), is_predicate)
    }

    fn is_any_value(&self) -> bool {
        matches!(self, Err(NotValue::NotValue))
    }
}

/// Any place we'd want to get fields from
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldSource {
    /// From the definition, with context on the module the def is accessed from
    DefId(DefId, item::ModuleId),
    /// Associated with the original definition of a type,
    /// with context on the module the type is accessed from
    TypeAssociated(ty::TypeId, item::ModuleId),
    /// On an instance of a type (e.g as the type of a `var`),
    /// with context on the module the type is accessed from
    TypeInstance(ty::TypeId, item::ModuleId),
    /// On an expression
    BodyExpr(LibraryId, expr::BodyExpr),
}

impl From<(DefId, item::ModuleId)> for FieldSource {
    fn from((def_id, in_mod): (DefId, item::ModuleId)) -> Self {
        Self::DefId(def_id, in_mod)
    }
}

impl From<(LibraryId, BodyExpr)> for FieldSource {
    fn from(expr: (LibraryId, BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(LibraryId, BodyId, ExprId)> for FieldSource {
    fn from(expr: (LibraryId, BodyId, ExprId)) -> Self {
        Self::BodyExpr(expr.0, BodyExpr(expr.1, expr.2))
    }
}
