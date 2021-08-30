//! Analysis query system definitions

use toc_hir::{
    body::BodyId,
    expr::{self, BodyExpr},
    library::InLibrary,
    symbol::DefId,
    ty::TypeId as HirTypeId,
};
use toc_salsa::salsa;

use crate::ty;

#[salsa::query_group(TypeInternStorage)]
pub trait TypeIntern: toc_hir_db::HirDatabase {
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

    /// Gets the type of the given definition.
    #[salsa::invoke(ty::query::type_of)]
    fn type_of(&self, def_id: DefId) -> ty::TypeId;

    /// Gets the type produced by the given expression.
    #[salsa::invoke(ty::query::eval_ty_of)]
    fn eval_ty_of(&self, expr: InLibrary<BodyExpr>) -> ty::TypeId;

    /// Gets the type produced by the expression body.
    #[salsa::invoke(ty::query::eval_ty_of_body)]
    fn eval_ty_of_body(&self, body: InLibrary<BodyId>) -> ty::TypeId;

    /// Checks if the binary op can be applied with the given types
    #[salsa::invoke(ty::rules::check_binary_op)]
    fn check_binary_op(
        &self,
        left: ty::TypeId,
        op: expr::BinaryOp,
        right: ty::TypeId,
    ) -> Result<ty::TypeId, ty::rules::InvalidBinaryOp>;

    /// Checks if the unary op can be applied with the given types
    #[salsa::invoke(ty::rules::check_unary_op)]
    fn check_unary_op(
        &self,
        op: expr::UnaryOp,
        right: ty::TypeId,
    ) -> Result<ty::TypeId, ty::rules::InvalidUnaryOp>;
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
