//! Inference for items.
//!
//! Note: Aside from ConstVar decls, we don't actually perform type inference
//! and instead lower the type from the type specification, if any.

use toc_hir_def::item;

use crate::{Db, ty};

/// Infers the type of a [`ConstVar`] item.
///
/// Tries to use the explicit type specification if present, otherwise infers the type from
/// the initializer.
///
/// [`ConstVar`]: item::ConstVar
#[salsa_macros::tracked]
pub(crate) fn type_of_constvar<'db>(db: &'db dyn Db, constvar: item::ConstVar<'db>) -> ty::Ty<'db> {
    // TODO: Fallback to inferring from the initializer if not present.
    let ty = if let Some(lowered) = constvar.explicit_ty_cons(db) {
        ty::lower::from_cons(db, lowered.cons(db))
    } else {
        // Nothing to infer a type off of, just use the plain error type.
        ty::make::mk_error(db)
    };

    // ConstVars always create a place type.
    ty::Ty::new(
        db,
        ty::TyKind::Place(Box::new(ty.kind(db).clone()), constvar.mutability(db)),
    )
}
