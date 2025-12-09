//! Constructs a type from the HIR representation into the type system.

use toc_hir_def::ty_cons;

use crate::{
    Db,
    ty::{self, make},
};

#[salsa_macros::tracked]
pub(crate) fn from_cons<'db>(db: &'db dyn Db, cons: ty_cons::TyCons<'db>) -> ty::Ty<'db> {
    // FIXME: Figure out a way to pipe resolutions in?
    let root = cons.root(db);

    match cons.node(db, root) {
        ty_cons::TyKind::Missing => make::mk_error(db),
        ty_cons::TyKind::Boolean => make::mk_boolean(db),
        ty_cons::TyKind::Int(ty_cons::IntSize::Int1) => make::mk_int1(db),
        ty_cons::TyKind::Int(ty_cons::IntSize::Int2) => make::mk_int2(db),
        ty_cons::TyKind::Int(ty_cons::IntSize::Int4) => make::mk_int4(db),
        ty_cons::TyKind::Int(ty_cons::IntSize::Int) => make::mk_int(db),
        ty_cons::TyKind::Nat(ty_cons::NatSize::Nat1) => make::mk_nat1(db),
        ty_cons::TyKind::Nat(ty_cons::NatSize::Nat2) => make::mk_nat2(db),
        ty_cons::TyKind::Nat(ty_cons::NatSize::Nat4) => make::mk_nat4(db),
        ty_cons::TyKind::Nat(ty_cons::NatSize::Nat) => make::mk_nat(db),
        ty_cons::TyKind::Nat(ty_cons::NatSize::AddressInt) => make::mk_addressint(db),
        ty_cons::TyKind::Real(ty_cons::RealSize::Real4) => make::mk_real4(db),
        ty_cons::TyKind::Real(ty_cons::RealSize::Real8) => make::mk_real8(db),
        ty_cons::TyKind::Real(ty_cons::RealSize::Real) => make::mk_real(db),
        ty_cons::TyKind::Char => make::mk_char(db),
        ty_cons::TyKind::String => make::mk_string(db),
    }
}
