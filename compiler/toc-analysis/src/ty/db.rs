//! Analysis query system definitions

use std::sync::Arc;

use toc_hir::{
    body::BodyId,
    expr::{self, BodyExpr, ExprId},
    item::{self, ItemId},
    package::{InPackage, PackageId},
    symbol::{self, DefId},
    ty::TypeId as HirTypeId,
};
use upcast::{Upcast, UpcastFrom};

use crate::ty::{self, query};

#[salsa::jar(db = TypeDatabase)]
pub struct TypeJar(ty::TypeId);

/// Type database
pub trait TypeDatabase:
    salsa::DbWithJar<TypeJar> + toc_hir_db::Db + Upcast<dyn toc_hir_db::Db>
{
    /// Converts the HIR type into an analysis form
    fn lower_hir_type(&self, type_id: InPackage<HirTypeId>) -> ty::TypeId;

    /// Gets the type of the given type source.
    fn type_of(&self, source: TypeSource) -> ty::TypeId;

    /// Gets what kind of value a [`ValueSource`] produces, or a [`NotValue`] if it doesn't produce one
    fn value_produced(&self, source: ValueSource) -> Result<ValueKind, NotValue>;

    /// Gets the corresponding definition from the given [`BindingSource`], or `None` if there isn't one.
    /// This also performs definition resolution, so resolving the resultant [`DefId`] is unnecessary.
    fn binding_def(&self, bind_src: BindingSource) -> Option<DefId>;

    /// Like [`Self::binding_def`], but does not perform definition resolution.
    /// Unless looking at the immediate def is necessary (e.g. determining if it's an import),
    /// then [`Self::binding_def`] should always be prefered.
    fn unresolved_binding_def(&self, bind_src: BindingSource) -> Option<DefId>;

    /// Gets the fields from the given [`FieldSource`]
    fn fields_of(&self, source: FieldSource) -> Option<Arc<item::Fields>>;

    /// Finds the associated exporting def from the given [`BindingSource`], or `None` if there isn't any
    fn exporting_def(&self, source: BindingSource) -> Option<DefId>;
}

impl<DB> TypeDatabase for DB
where
    DB: salsa::DbWithJar<TypeJar> + toc_hir_db::Db + Upcast<dyn toc_hir_db::Db>,
{
    fn lower_hir_type(&self, type_id: InPackage<HirTypeId>) -> super::TypeId {
        query::lower_hir_type(self, type_id)
    }

    fn type_of(&self, source: TypeSource) -> super::TypeId {
        query::type_of(self, source)
    }

    fn value_produced(&self, source: ValueSource) -> Result<ValueKind, NotValue> {
        query::value_produced(self, source)
    }

    fn binding_def(&self, bind_src: BindingSource) -> Option<DefId> {
        query::binding_def(self, bind_src)
    }

    fn unresolved_binding_def(&self, bind_src: BindingSource) -> Option<DefId> {
        query::unresolved_binding_def(self, bind_src)
    }

    fn fields_of(&self, source: FieldSource) -> Option<Arc<item::Fields>> {
        query::fields_of(self, source)
    }

    fn exporting_def(&self, source: BindingSource) -> Option<DefId> {
        query::exporting_def(self, source)
    }
}

impl<'db, DB: TypeDatabase + 'db> UpcastFrom<DB> for dyn TypeDatabase + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}

/// Anything which can produce a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeSource {
    Def(DefId),
    Item(PackageId, ItemId),
    BodyExpr(PackageId, BodyExpr),
    Body(PackageId, BodyId),
}

impl From<DefId> for TypeSource {
    fn from(id: DefId) -> Self {
        Self::Def(id)
    }
}

impl From<InPackage<ItemId>> for TypeSource {
    fn from(id: InPackage<ItemId>) -> Self {
        Self::Item(id.0, id.1)
    }
}

impl From<InPackage<BodyExpr>> for TypeSource {
    fn from(id: InPackage<BodyExpr>) -> Self {
        Self::BodyExpr(id.0, id.1)
    }
}

impl From<InPackage<BodyId>> for TypeSource {
    fn from(id: InPackage<BodyId>) -> Self {
        Self::Body(id.0, id.1)
    }
}

impl From<(PackageId, BodyExpr)> for TypeSource {
    fn from(id: (PackageId, BodyExpr)) -> Self {
        Self::BodyExpr(id.0, id.1)
    }
}

impl From<(PackageId, BodyId, ExprId)> for TypeSource {
    fn from(id: (PackageId, BodyId, ExprId)) -> Self {
        Self::BodyExpr(id.0, BodyExpr(id.1, id.2))
    }
}

impl From<(PackageId, BodyId)> for TypeSource {
    fn from(id: (PackageId, BodyId)) -> Self {
        Self::Body(id.0, id.1)
    }
}

impl From<(PackageId, ItemId)> for TypeSource {
    fn from(id: (PackageId, ItemId)) -> Self {
        Self::Item(id.0, id.1)
    }
}

/// Anything that can produce a reference to a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingSource {
    DefId(DefId),
    Body(PackageId, BodyId),
    BodyExpr(PackageId, BodyExpr),
}

impl From<symbol::DefId> for BindingSource {
    fn from(def_id: DefId) -> Self {
        Self::DefId(def_id)
    }
}

impl From<(PackageId, BodyId)> for BindingSource {
    fn from((pkg_id, body): (PackageId, BodyId)) -> Self {
        Self::Body(pkg_id, body)
    }
}

impl From<(PackageId, expr::BodyExpr)> for BindingSource {
    fn from(expr: (PackageId, expr::BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(PackageId, BodyId, expr::ExprId)> for BindingSource {
    fn from(expr: (PackageId, BodyId, expr::ExprId)) -> Self {
        Self::BodyExpr(expr.0, expr::BodyExpr(expr.1, expr.2))
    }
}

impl From<ValueSource> for BindingSource {
    fn from(src: ValueSource) -> Self {
        match src {
            ValueSource::Def(def_id) => Self::DefId(def_id),
            ValueSource::Body(pkg_id, body_id) => Self::Body(pkg_id, body_id),
            ValueSource::BodyExpr(pkg_id, body_expr) => Self::BodyExpr(pkg_id, body_expr),
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
    Body(PackageId, BodyId),
    BodyExpr(PackageId, BodyExpr),
}

impl ValueSource {
    pub fn span_of(self, db: &dyn toc_hir_db::Db) -> toc_span::Span {
        match self {
            ValueSource::Def(_def_id) => {
                unimplemented!("not sure if we need this")
            }
            ValueSource::Body(pkg_id, body_id) => {
                let package = db.package(pkg_id);
                package.body(body_id).span.lookup_in(&package)
            }
            ValueSource::BodyExpr(pkg_id, BodyExpr(body_id, expr_id)) => {
                let package = db.package(pkg_id);
                package.body(body_id).expr(expr_id).span.lookup_in(&package)
            }
        }
    }
}

impl From<DefId> for ValueSource {
    fn from(def_id: DefId) -> Self {
        Self::Def(def_id)
    }
}

impl From<(PackageId, BodyId)> for ValueSource {
    fn from((pkg_id, body): (PackageId, BodyId)) -> Self {
        Self::Body(pkg_id, body)
    }
}

impl From<(PackageId, BodyExpr)> for ValueSource {
    fn from(expr: (PackageId, BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(PackageId, BodyId, ExprId)> for ValueSource {
    fn from(expr: (PackageId, BodyId, ExprId)) -> Self {
        Self::BodyExpr(expr.0, BodyExpr(expr.1, expr.2))
    }
}

impl From<ValueSource> for TypeSource {
    fn from(src: ValueSource) -> Self {
        match src {
            ValueSource::Body(pkg_id, body_id) => Self::Body(pkg_id, body_id),
            ValueSource::BodyExpr(pkg_id, body_id) => Self::BodyExpr(pkg_id, body_id),
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
        !matches!(self, Err(NotValue::NotValue))
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
    BodyExpr(PackageId, expr::BodyExpr),
}

impl From<(DefId, item::ModuleId)> for FieldSource {
    fn from((def_id, in_mod): (DefId, item::ModuleId)) -> Self {
        Self::DefId(def_id, in_mod)
    }
}

impl From<(PackageId, BodyExpr)> for FieldSource {
    fn from(expr: (PackageId, BodyExpr)) -> Self {
        Self::BodyExpr(expr.0, expr.1)
    }
}

impl From<(PackageId, BodyId, ExprId)> for FieldSource {
    fn from(expr: (PackageId, BodyId, ExprId)) -> Self {
        Self::BodyExpr(expr.0, BodyExpr(expr.1, expr.2))
    }
}
