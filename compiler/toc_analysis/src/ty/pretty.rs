//! Type debug formatting & pretty printing

use std::fmt;

use toc_hir::symbol::{self, SubprogramKind};
use toc_span::Span;

use crate::ty::Checked;
use crate::{
    db,
    ty::{IntSize, NatSize, RealSize, TyRef, TypeKind},
};

use super::{ArraySizing, EndBound, NotFixedLen, PassBy, TypeId, WithDef};

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
            TypeKind::Alias(_, _) | TypeKind::Opaque(_, _) | TypeKind::Constrained(..) => "",
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
            TypeKind::Opaque(_, _) => "opaque",
            TypeKind::Forward => "forward",
            TypeKind::Constrained(..) => "range of",
            TypeKind::Array(ArraySizing::Flexible, ..) => "flexible array",
            TypeKind::Array(..) => "array",
            TypeKind::Enum(..) => "enum",
            TypeKind::Set(..) => "set",
            TypeKind::Pointer(Checked::Checked, _) => "pointer to",
            TypeKind::Pointer(Checked::Unchecked, _) => "unchecked pointer to",
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
        TypeKind::Opaque(def_id, to) => {
            out.write_fmt(format_args!("[{def_id:?}] type to "))?;
            emit_debug_ty(db, out, *to)?
        }
        TypeKind::Constrained(base_ty, start, end) => {
            let base_ty = base_ty.in_db(db);
            out.write_fmt(format_args!(" `{base_ty:?}` ({start:?} .. {end:?})"))?;
        }
        TypeKind::Array(_is_flexible, ranges, elem) => {
            write!(out, " ( ")?;
            for &range in ranges {
                emit_debug_ty(db, out, range)?;
                write!(out, ", ")?;
            }
            write!(out, ") of ")?;
            emit_debug_ty(db, out, *elem)?;
        }
        TypeKind::Enum(with_def, variants) => {
            let def_id = match with_def {
                WithDef::Named(def_id) => def_id,
                WithDef::Anonymous(def_id) => def_id,
            };
            out.write_fmt(format_args!("[{def_id:?}] "))?;

            out.write_str("( ")?;
            if !variants.is_empty() {
                let library = db.library(def_id.0);

                for variant in variants {
                    let def_info = library.local_def(variant.1);
                    let name = def_info.name;
                    let span = def_info.def_at.lookup_in(&library);

                    write!(out, "{name:?}@{span:?}, ")?;
                }
            }
            out.write_char(')')?;
        }
        TypeKind::Set(with_def, to) => {
            let def_id = match with_def {
                WithDef::Named(def_id) => def_id,
                WithDef::Anonymous(def_id) => def_id,
            };
            out.write_fmt(format_args!("[{def_id:?}] of "))?;
            emit_debug_ty(db, out, *to)?
        }
        TypeKind::Pointer(_, to) => {
            out.write_char(' ')?;
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
    // FIXME: Change set display into `{Alias}` (`set of int`)
    // FIXME: Move `'s into here so that we can format like this: "`{Alias}` (alias of {type})"
    match ty.kind() {
        TypeKind::StringN(seq) | TypeKind::CharN(seq) => {
            out.write_char('(')?;
            match seq.fixed_len(db, Span::default()) {
                Ok(v) => out.write_fmt(format_args!("{v}"))?,
                Err(NotFixedLen::AnySize) => out.write_char('*')?,
                Err(NotFixedLen::ConstError(err)) if err.is_not_compile_time() => {
                    out.write_str("{dynamic}")?
                }
                Err(NotFixedLen::ConstError(err)) if err.is_missing() => {
                    out.write_str("{unknown}")?
                }
                Err(NotFixedLen::ConstError(err)) => {
                    unreachable!("should not show errors! {err:?}")
                }
            }
            out.write_char(')')?;
        }
        TypeKind::Alias(def_id, to) => {
            let library = db.library(def_id.0);
            let name = library.local_def(def_id.1).name;
            out.write_fmt(format_args!("{name} (alias of "))?;
            emit_display_ty(db, out, *to, PokeAliases::Yes)?;
            out.write_char(')')?;
        }
        TypeKind::Opaque(def_id, _) => {
            let library = db.library(def_id.0);
            let name = library.local_def(def_id.1).name;
            out.write_fmt(format_args!("{name} (an opaque type)"))?;
        }
        TypeKind::Constrained(_, start, end) => {
            // FIXME: Use the correct eval params
            let eval_params = crate::const_eval::EvalParams::default();

            match db.evaluate_const(start.clone(), eval_params) {
                Ok(v) => write!(out, "{v}", v = v.display(db))?,
                Err(err) if err.is_not_compile_time() => out.write_str("{dynamic}")?,
                Err(err) if err.is_missing() => out.write_str("{unknown}")?,
                Err(err) => {
                    unreachable!("should not show errors! ({err:?})")
                }
            };

            write!(out, " .. ")?;

            // don't need to worry about checking `AllowDyn` since that's more for type
            // checking than pretty printing
            match end {
                EndBound::Expr(end, _) => match db.evaluate_const(end.clone(), eval_params) {
                    Ok(v) => write!(out, "{v}", v = v.display(db))?,
                    Err(err) if err.is_not_compile_time() => out.write_str("{dynamic}")?,
                    Err(err) if err.is_missing() => out.write_str("{unknown}")?,
                    Err(err) => {
                        unreachable!("should not show errors! ({err:?})")
                    }
                },
                EndBound::Unsized(_) | EndBound::Any => write!(out, "*")?,
            }
        }
        TypeKind::Array(_is_flexible, ranges, elem_ty) => {
            // `{range_ty} of {elem_ty}`, or
            // `{range_ty}, {range_ty} of {elem_ty}`

            // intersperse comma
            let mut ranges = ranges.iter();
            if let Some(range_ty) = ranges.next() {
                write!(out, " ")?;
                emit_display_ty(db, out, *range_ty, PokeAliases::Yes)?;
            }
            for range_ty in ranges {
                write!(out, ", ")?;
                emit_display_ty(db, out, *range_ty, PokeAliases::Yes)?;
            }

            write!(out, " of ")?;
            emit_display_ty(db, out, *elem_ty, PokeAliases::No)?;
        }
        TypeKind::Enum(with_def, _) => {
            let def_id = match with_def {
                WithDef::Named(def_id) => def_id,
                WithDef::Anonymous(def_id) => def_id,
            };
            let library = db.library(def_id.0);
            let name = library.local_def(def_id.1).name;
            out.write_fmt(format_args!(" {name}"))?;
        }
        TypeKind::Set(with_def, to) => {
            let def_id = match with_def {
                WithDef::Named(def_id) => def_id,
                WithDef::Anonymous(def_id) => def_id,
            };
            let library = db.library(def_id.0);
            let name = library.local_def(def_id.1).name;
            out.write_fmt(format_args!(" {name}"))?;

            if poke_aliases == PokeAliases::No {
                out.write_str(" (of ")?;
                emit_display_ty(db, out, *to, PokeAliases::Yes)?;
                out.write_char(')')?;
            }
        }
        TypeKind::Pointer(_, to) => {
            out.write_char(' ')?;
            emit_display_ty(db, out, *to, PokeAliases::Yes)?
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
