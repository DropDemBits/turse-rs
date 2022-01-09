//! Type-related query implementation

use toc_hir::{
    body::BodyId,
    expr::BodyExpr,
    library::{InLibrary, WrapInLibrary},
    symbol::{DefId, DefOwner},
    ty::TypeId as HirTypeId,
};

use crate::db;

use super::lower;
use super::{IntSize, Mutability, NatSize, RealSize, SeqSize, Type, TypeId, TypeKind};

pub(crate) fn from_hir_type(db: &dyn db::TypeDatabase, type_id: InLibrary<HirTypeId>) -> TypeId {
    lower::ty_from_hir_ty(db, type_id)
}

pub(crate) fn type_of(db: &dyn db::TypeDatabase, source: db::TypeSource) -> TypeId {
    let ty = db.aliased_type_of(source).in_db(db);

    // punch through any aliases
    if let TypeKind::Ref(mutability, real_ty) = ty.kind() {
        // Carry the ref through
        db.mk_ref(*mutability, real_ty.in_db(db).peel_aliases().id())
    } else {
        ty.peel_aliases().id()
    }
}

pub(crate) fn aliased_type_of(db: &dyn db::TypeDatabase, source: db::TypeSource) -> TypeId {
    match source {
        db::TypeSource::Def(def_id) => ty_of_def(db, def_id),
        db::TypeSource::BodyExpr(id, expr) => ty_of_expr(db, InLibrary(id, expr)),
        db::TypeSource::Body(id, body) => ty_of_body(db, InLibrary(id, body)),
    }
}

fn ty_of_def(db: &dyn db::TypeDatabase, def_id: DefId) -> TypeId {
    if let Some(owner) = db.def_owner(def_id) {
        match owner {
            DefOwner::Item(item_id) => lower::ty_from_item(db, InLibrary(def_id.0, item_id)),
            DefOwner::Stmt(stmt_id) => lower::ty_from_stmt(db, InLibrary(def_id.0, stmt_id)),
        }
    } else {
        // No actual definition owner
        db.intern_type(
            Type {
                kind: TypeKind::Error,
            }
            .into(),
        )
    }
}

fn ty_of_expr(db: &dyn db::TypeDatabase, expr: InLibrary<BodyExpr>) -> TypeId {
    let InLibrary(lib_id, BodyExpr(body_id, expr_id)) = expr;

    let library = db.library(lib_id);
    let body = library.body(body_id);

    lower::ty_from_expr(db, body.in_library(lib_id), expr_id)
}

fn ty_of_body(db: &dyn db::TypeDatabase, body_id: InLibrary<BodyId>) -> TypeId {
    let library = db.library(body_id.0);
    let body = library.body(body_id.1);

    match &body.kind {
        toc_hir::body::BodyKind::Stmts(_, _) => db.mk_error(), // This is where we'd evaluate a const fn
        toc_hir::body::BodyKind::Exprs(expr) => db.type_of((body_id.0, body_id.1, *expr).into()),
    }
}

impl<T> db::TypeInternExt for T
where
    T: db::TypeIntern,
{
    fn mk_error(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Error,
            }
            .into(),
        )
    }

    fn mk_boolean(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Boolean,
            }
            .into(),
        )
    }

    fn mk_int(&self, kind: IntSize) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Int(kind),
            }
            .into(),
        )
    }

    fn mk_nat(&self, kind: NatSize) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Nat(kind),
            }
            .into(),
        )
    }

    fn mk_real(&self, kind: RealSize) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Real(kind),
            }
            .into(),
        )
    }

    fn mk_integer(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Integer,
            }
            .into(),
        )
    }

    fn mk_char(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Char,
            }
            .into(),
        )
    }

    fn mk_string(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::String,
            }
            .into(),
        )
    }

    fn mk_char_n(&self, seq_size: SeqSize) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::CharN(seq_size),
            }
            .into(),
        )
    }

    fn mk_string_n(&self, seq_size: SeqSize) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::StringN(seq_size),
            }
            .into(),
        )
    }

    fn mk_alias(&self, def_id: DefId, base_ty: TypeId) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Alias(def_id, base_ty),
            }
            .into(),
        )
    }

    fn mk_forward(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Forward,
            }
            .into(),
        )
    }

    fn mk_ref(&self, mutability: Mutability, to: TypeId) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Ref(mutability, to),
            }
            .into(),
        )
    }
}
