//! Type debug formatting & pretty printing

use std::fmt;

use crate::{
    db,
    ty::{IntSize, NatSize, RealSize, TypeKind},
};

use super::{Mutability, TypeId};

pub(super) fn debug_ty(db: &dyn db::TypeDatabase, type_id: TypeId) -> String {
    let mut out = String::new();

    emit_ty(db, &mut out, type_id).expect("failed to debug fmt ty");

    out
}

fn emit_ty(db: &dyn db::TypeDatabase, out: &mut dyn fmt::Write, type_id: TypeId) -> fmt::Result {
    let ty_dat = type_id.lookup(db);

    let prefix = match ty_dat.kind() {
        TypeKind::Error => "<error>",
        TypeKind::Boolean => "boolean",
        TypeKind::Int(IntSize::Int) => "int",
        TypeKind::Int(IntSize::Int1) => "int1",
        TypeKind::Int(IntSize::Int2) => "int2",
        TypeKind::Int(IntSize::Int4) => "int4",
        TypeKind::Nat(NatSize::Nat) => "nat",
        TypeKind::Nat(NatSize::Nat1) => "nat1",
        TypeKind::Nat(NatSize::Nat2) => "nat2",
        TypeKind::Nat(NatSize::Nat4) => "nat4",
        TypeKind::Nat(NatSize::AddressInt) => "addressint",
        TypeKind::Real(RealSize::Real) => "real",
        TypeKind::Real(RealSize::Real4) => "real4",
        TypeKind::Real(RealSize::Real8) => "real8",
        TypeKind::Integer => "{integer}",
        TypeKind::Char => "char",
        TypeKind::String => "string",
        TypeKind::CharN(_) => "char_n",
        TypeKind::StringN(_) => "string_n",
        TypeKind::Ref(Mutability::Const, _) => "ref",
        TypeKind::Ref(Mutability::Var, _) => "ref_mut",
    };

    out.write_str(prefix)?;

    // Extra bits
    match ty_dat.kind() {
        TypeKind::StringN(seq) | TypeKind::CharN(seq) => {
            out.write_fmt(format_args!(" {:?}", seq))?
        }
        TypeKind::Ref(_, to) => {
            out.write_char(' ')?;
            emit_ty(db, out, *to)?
        }
        _ => {}
    }

    Ok(())
}
