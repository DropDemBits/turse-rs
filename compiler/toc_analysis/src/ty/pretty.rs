//! Type debug formatting & pretty printing

use std::fmt;

use toc_span::Span;

use crate::{
    db,
    ty::{IntSize, NatSize, RealSize, TyRef, TypeKind},
};

use super::{Mutability, TypeId};

impl<'db, DB> fmt::Debug for TyRef<'db, DB>
where
    DB: db::TypeDatabase + ?Sized + 'db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        emit_debug_ty(self.db, f, self.id)
    }
}

impl<'db, DB> fmt::Display for TyRef<'db, DB>
where
    DB: db::ConstEval + ?Sized + 'db,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        emit_display_ty(self.db, f, self.id)
    }
}

impl TypeKind {
    pub fn prefix(&self) -> &str {
        match self {
            // Sized charseqs use the parent type as the basename
            TypeKind::CharN(_) => "char",
            TypeKind::StringN(_) => "string",
            // Refs are not shown to the user
            TypeKind::Ref(_, _) => "",
            _ => self.debug_prefix(),
        }
    }

    pub fn debug_prefix(&self) -> &str {
        match self {
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
        }
    }
}

fn emit_debug_ty<'db, DB>(db: &'db DB, out: &mut dyn fmt::Write, type_id: TypeId) -> fmt::Result
where
    DB: db::TypeDatabase + ?Sized + 'db,
{
    let ty = type_id.in_db(db);
    let ty_kind = &*ty.kind();

    out.write_str(ty_kind.debug_prefix())?;

    // Extra bits
    match ty_kind {
        TypeKind::StringN(seq) | TypeKind::CharN(seq) => {
            out.write_fmt(format_args!(" {:?}", seq))?
        }
        TypeKind::Ref(_, to) => {
            out.write_char(' ')?;
            emit_debug_ty(db, out, *to)?
        }
        _ => {}
    }

    Ok(())
}

fn emit_display_ty<'db, DB>(db: &'db DB, out: &mut dyn fmt::Write, type_id: TypeId) -> fmt::Result
where
    DB: db::ConstEval + ?Sized + 'db,
{
    let ty = type_id.in_db(db);
    let ty_kind = &*ty.kind();

    out.write_str(ty_kind.prefix())?;

    // Extra bits
    match ty_kind {
        TypeKind::StringN(seq) | TypeKind::CharN(seq) => {
            out.write_char('(')?;
            match seq.fixed_len(db, Span::default()) {
                Ok(None) => out.write_char('*')?,
                Ok(Some(v)) => out.write_fmt(format_args!("{}", v))?,
                Err(_) => unreachable!("should not show errors!"),
            }
            out.write_char(')')?;
        }
        TypeKind::Ref(_, to) => emit_display_ty(db, out, *to)?,
        _ => {}
    }

    Ok(())
}
