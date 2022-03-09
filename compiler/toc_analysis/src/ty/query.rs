//! Type-related query implementation

use toc_hir::symbol::BindingTo;
use toc_hir::{
    body::BodyId,
    expr::BodyExpr,
    library::{InLibrary, WrapInLibrary},
    symbol,
    symbol::{DefId, DefOwner},
    ty::TypeId as HirTypeId,
};

use crate::db;

use super::{lower, IntSize, NatSize, Param, RealSize, SeqSize, Type, TypeId, TypeKind};

pub(crate) fn from_hir_type(db: &dyn db::TypeDatabase, type_id: InLibrary<HirTypeId>) -> TypeId {
    lower::ty_from_hir_ty(db, type_id)
}

pub(crate) fn type_of(db: &dyn db::TypeDatabase, source: db::TypeSource) -> TypeId {
    match source {
        db::TypeSource::Def(def_id) => ty_of_def(db, def_id),
        db::TypeSource::Item(library_id, item_id) => {
            lower::ty_from_item(db, InLibrary(library_id, item_id))
        }
        db::TypeSource::BodyExpr(id, expr) => ty_of_expr(db, InLibrary(id, expr)),
        db::TypeSource::Body(id, body) => ty_of_body(db, InLibrary(id, body)),
    }
}

fn ty_of_def(db: &dyn db::TypeDatabase, def_id: DefId) -> TypeId {
    if let Some(owner) = db.def_owner(def_id) {
        match owner {
            DefOwner::Item(item_id) => lower::ty_from_item(db, InLibrary(def_id.0, item_id)),
            DefOwner::ItemParam(item_id, param_def) => {
                lower::ty_from_item_param(db, InLibrary(def_id.0, (item_id, param_def)))
            }
            DefOwner::Stmt(stmt_id) => lower::ty_from_stmt(db, InLibrary(def_id.0, stmt_id)),
        }
    } else {
        // No actual definition owner
        db.mk_error()
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
        toc_hir::body::BodyKind::Stmts(..) => {
            lower::ty_from_body_owner(db, body_id.0, db.body_owner(body_id))
        }
        toc_hir::body::BodyKind::Exprs(expr) => db.type_of((body_id.0, body_id.1, *expr).into()),
    }
}

pub(super) fn value_produced(
    db: &dyn db::TypeDatabase,
    value_src: db::ValueSource,
) -> Result<db::ValueKind, db::NotValue> {
    use db::{NotValue, ValueKind, ValueSource};

    match value_src {
        ValueSource::DefId(def_id) => {
            let kind = db.binding_to(def_id.into());

            // Take from the binding kind
            match kind {
                Ok(BindingTo::Storage(muta)) => Ok(ValueKind::Reference(muta)),
                Ok(BindingTo::Register(muta)) => Ok(ValueKind::Register(muta)),
                // Subprogram names are aliases of address constants
                Ok(BindingTo::Subprogram(..)) => Ok(ValueKind::Scalar),
                Err(symbol::NotBinding::Missing) => Err(NotValue::Missing),
                _ => Err(NotValue::NotValue),
            }
        }
        ValueSource::Body(lib_id, body_id) => {
            let library = db.library(lib_id);
            let body = library.body(body_id);

            // Take from the main body expr
            match body.kind {
                toc_hir::body::BodyKind::Stmts(_, _, _) => Err(NotValue::NotValue),
                toc_hir::body::BodyKind::Exprs(expr_id) => {
                    db.value_produced((lib_id, body_id, expr_id).into())
                }
            }
        }
        ValueSource::BodyExpr(lib_id, body_expr) => {
            let library = db.library(lib_id);

            match &library.body(body_expr.0).expr(body_expr.1).kind {
                toc_hir::expr::ExprKind::Missing => Err(NotValue::Missing),
                toc_hir::expr::ExprKind::Name(name) => match name {
                    toc_hir::expr::Name::Name(def_id) => {
                        // Defer to binding
                        db.value_produced(DefId(lib_id, *def_id).into())
                    }
                    toc_hir::expr::Name::Self_ => unimplemented!(),
                },
                toc_hir::expr::ExprKind::Literal(literal) => match literal {
                    toc_hir::expr::Literal::CharSeq(_) | toc_hir::expr::Literal::String(_) => {
                        Ok(ValueKind::Reference(symbol::Mutability::Const))
                    }
                    _ => Ok(ValueKind::Scalar),
                },
                _ => {
                    // Take from the expr's type (always produces a value)
                    let expr_ty = db.type_of((lib_id, body_expr).into());
                    let expr_ty_ref = expr_ty.in_db(db).to_base_type();

                    match expr_ty_ref.kind() {
                        kind if kind.is_scalar() => Ok(ValueKind::Scalar),
                        kind if !kind.is_error() => {
                            Ok(ValueKind::Reference(symbol::Mutability::Const))
                        }
                        _ => Err(NotValue::Missing),
                    }
                }
            }
        }
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

    fn mk_subprogram(
        &self,
        kind: symbol::SubprogramKind,
        params: Option<Vec<Param>>,
        result: TypeId,
    ) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Subprogram(kind, params, result),
            }
            .into(),
        )
    }

    fn mk_void(&self) -> super::TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Void,
            }
            .into(),
        )
    }
}
