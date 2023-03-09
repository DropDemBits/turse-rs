//! Constructors for interned types

use toc_hir::symbol::{self, DefId};

use crate::{
    const_eval,
    ty::{self, db, TypeId, TypeKind},
};

pub fn error(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::Error)
}

pub fn boolean(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::Boolean)
}

pub fn int(db: &dyn db::TypeDatabase, kind: ty::IntSize) -> TypeId {
    TypeId::new(db, TypeKind::Int(kind))
}

pub fn nat(db: &dyn db::TypeDatabase, kind: ty::NatSize) -> TypeId {
    TypeId::new(db, TypeKind::Nat(kind))
}

pub fn real(db: &dyn db::TypeDatabase, kind: ty::RealSize) -> TypeId {
    TypeId::new(db, TypeKind::Real(kind))
}

pub fn integer(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::Integer)
}

pub fn char(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::Char)
}

pub fn string(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::String)
}

pub fn char_n(db: &dyn db::TypeDatabase, seq_size: ty::SeqSize) -> TypeId {
    TypeId::new(db, TypeKind::CharN(seq_size))
}

pub fn string_n(db: &dyn db::TypeDatabase, seq_size: ty::SeqSize) -> TypeId {
    TypeId::new(db, TypeKind::StringN(seq_size))
}

pub fn alias(db: &dyn db::TypeDatabase, def_id: DefId, base_ty: TypeId) -> TypeId {
    TypeId::new(db, TypeKind::Alias(def_id, base_ty))
}

pub fn opaque(db: &dyn db::TypeDatabase, def_id: DefId, base_ty: TypeId) -> TypeId {
    TypeId::new(db, TypeKind::Opaque(def_id, base_ty))
}

pub fn forward(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::Forward)
}

pub fn constrained(
    db: &dyn db::TypeDatabase,
    base_ty: TypeId,
    start: const_eval::Const,
    end: ty::EndBound,
) -> TypeId {
    TypeId::new(db, TypeKind::Constrained(base_ty, start, end))
}

pub fn array(
    db: &dyn db::TypeDatabase,
    sizing: ty::ArraySizing,
    ranges: Vec<TypeId>,
    elem_ty: TypeId,
) -> super::TypeId {
    TypeId::new(db, TypeKind::Array(sizing, ranges, elem_ty))
}

pub fn enum_(db: &dyn db::TypeDatabase, with_def: ty::WithDef, variants: Vec<DefId>) -> TypeId {
    TypeId::new(db, TypeKind::Enum(with_def, variants))
}

pub fn set(db: &dyn db::TypeDatabase, with_def: ty::WithDef, elem_ty: TypeId) -> TypeId {
    TypeId::new(db, TypeKind::Set(with_def, elem_ty))
}

pub fn pointer(db: &dyn db::TypeDatabase, checked: ty::Checked, target_ty: TypeId) -> TypeId {
    TypeId::new(db, TypeKind::Pointer(checked, target_ty))
}

pub fn subprogram(
    db: &dyn db::TypeDatabase,
    kind: symbol::SubprogramKind,
    params: Option<Vec<ty::Param>>,
    result: TypeId,
) -> TypeId {
    TypeId::new(db, TypeKind::Subprogram(kind, params, result))
}

pub fn void(db: &dyn db::TypeDatabase) -> TypeId {
    TypeId::new(db, TypeKind::Void)
}
