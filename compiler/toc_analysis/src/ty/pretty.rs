//! Type debug formatting & pretty printing

use std::fmt;

use toc_hir::symbol::{self, SubprogramKind};
use toc_span::Span;

use crate::{
    db,
    ty::{IntSize, NatSize, RealSize, TyRef, TypeKind},
};

use super::{NotFixedLen, PassBy, TypeId};

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
        emit_display_ty(self.db, f, self.id, PokeAliases::No)
    }
}

impl TypeKind {
    pub fn prefix(&self) -> &str {
        match self {
            // Sized charseqs use the parent type as the basename
            TypeKind::CharN(_) => "char",
            TypeKind::StringN(_) => "string",
            TypeKind::Alias(_, _) => "",
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
            TypeKind::Alias(_, _) => "alias",
            TypeKind::Forward => "forward",
            TypeKind::Subprogram(SubprogramKind::Function, _, _) => "function",
            TypeKind::Subprogram(SubprogramKind::Procedure, _, _) => "procedure",
            TypeKind::Subprogram(SubprogramKind::Process, _, _) => "process",
            TypeKind::Void => "void",
        }
    }
}

fn emit_debug_ty<'db, DB>(db: &'db DB, out: &mut dyn fmt::Write, type_id: TypeId) -> fmt::Result
where
    DB: db::TypeDatabase + ?Sized + 'db,
{
    let ty = type_id.in_db(db);
    out.write_str(ty.kind().debug_prefix())?;

    // Extra bits
    match ty.kind() {
        TypeKind::StringN(seq) | TypeKind::CharN(seq) => out.write_fmt(format_args!(" {seq:?}"))?,
        TypeKind::Alias(def_id, to) => {
            out.write_fmt(format_args!("[{def_id:?}] of "))?;
            emit_debug_ty(db, out, *to)?
        }
        TypeKind::Subprogram(_kind, params, result) => {
            if let Some(params) = params {
                write!(out, " ( ")?;
                for param in params {
                    // pass_by register : cheat type
                    let pass_by = match param.pass_by {
                        PassBy::Value => "pass(value) ",
                        PassBy::Reference(symbol::Mutability::Const) => "pass(const ref) ",
                        PassBy::Reference(symbol::Mutability::Var) => "pass(var ref) ",
                    };
                    let register = param.is_register.then(|| "register ").unwrap_or_default();
                    let cheat = param.coerced_type.then(|| "cheat ").unwrap_or_default();
                    write!(out, "{pass_by}{register}{cheat}")?;
                    emit_debug_ty(db, out, param.param_ty)?;
                    write!(out, ", ")?;
                }
                write!(out, ")")?;
            }
            write!(out, " -> ")?;
            emit_debug_ty(db, out, *result)?
        }
        _ => {}
    }

    Ok(())
}

fn emit_display_ty<'db, DB>(
    db: &'db DB,
    out: &mut dyn fmt::Write,
    type_id: TypeId,
    poke_aliases: PokeAliases,
) -> fmt::Result
where
    DB: db::ConstEval + ?Sized + 'db,
{
    let ty = {
        let ty = type_id.in_db(db);

        match (poke_aliases, ty.kind()) {
            (PokeAliases::Yes, TypeKind::Alias(_, ty)) => ty.in_db(db),
            _ => ty,
        }
    };

    out.write_str(ty.kind().prefix())?;

    // Extra bits
    match ty.kind() {
        TypeKind::StringN(seq) | TypeKind::CharN(seq) => {
            out.write_char('(')?;
            match seq.fixed_len(db, Span::default()) {
                Ok(v) => out.write_fmt(format_args!("{v}"))?,
                Err(NotFixedLen::DynSize) => out.write_char('*')?,
                Err(NotFixedLen::ConstError(_)) => unreachable!("should not show errors!"),
            }
            out.write_char(')')?;
        }
        TypeKind::Alias(def_id, to) => {
            let library = db.library(def_id.0);
            let name = library.local_def(def_id.1).name.item();
            out.write_fmt(format_args!("{name} (alias of "))?;
            emit_display_ty(db, out, *to, PokeAliases::Yes)?;
            out.write_char(')')?;
        }
        TypeKind::Subprogram(kind, params, result) => {
            // Format:
            // function (int) : int
            // function (int, cheat int) : int
            // function (int, register : cheat int) : int
            // function (int, var register : cheat int) : int
            // procedure

            // Poke aliases so that we don't get long types
            if let Some(params) = params {
                write!(out, " (")?;
                let mut first = true;

                for param in params {
                    let with_separator =
                        param.is_register || !matches!(param.pass_by, PassBy::Value);

                    let pass_by = match param.pass_by {
                        PassBy::Value => "",
                        PassBy::Reference(symbol::Mutability::Const) => "const ",
                        PassBy::Reference(symbol::Mutability::Var) => "var ",
                    };
                    let register = param.is_register.then(|| "register ").unwrap_or_default();
                    let separator = with_separator.then(|| ": ").unwrap_or_default();
                    let cheat = param.coerced_type.then(|| "cheat ").unwrap_or_default();

                    if !first {
                        write!(out, ", ")?;
                    }
                    write!(out, "{pass_by}{register}{separator}{cheat}")?;
                    emit_display_ty(db, out, param.param_ty, PokeAliases::Yes)?;

                    first = false;
                }
                write!(out, ")")?;
            }

            if matches!(kind, SubprogramKind::Function) {
                write!(out, " : ")?;
                emit_display_ty(db, out, *result, PokeAliases::Yes)?;
            }
        }
        TypeKind::Void => unreachable!("`void` should never be user visible"),
        _ => {}
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PokeAliases {
    No,
    Yes,
}
