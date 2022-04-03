//! Type-related query implementation

use std::sync::Arc;

use toc_hir::item::{self, ParameterInfo};
use toc_hir::symbol::{BindingTo, Mutability, NotBinding};
use toc_hir::{body, expr, stmt};
use toc_hir::{
    body::BodyId,
    expr::BodyExpr,
    library::{InLibrary, WrapInLibrary},
    symbol,
    symbol::{DefId, DefOwner},
    ty as hir_ty,
    ty::TypeId as HirTypeId,
};

use crate::db::{self, BindingSource, TypeDatabase};
use crate::ty::WithDef;

use super::{lower, Checked, IntSize, NatSize, Param, RealSize, SeqSize, Type, TypeId, TypeKind};

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
            DefOwner::ItemExport(item_id, export_idx) => {
                // Refer to the corresponding exported item
                let library = db.library(def_id.0);

                if let item::ItemKind::Module(module) = &library.item(item_id).kind {
                    let export_item = module.exports.get(export_idx).expect("bad export index");
                    let def_id = DefId(def_id.0, library.item(export_item.item_id).def_id);

                    db.type_of(def_id.into())
                } else {
                    // Only applicable to module-likes
                    unreachable!()
                }
            }
            DefOwner::Stmt(stmt_id) => lower::ty_from_stmt(db, InLibrary(def_id.0, stmt_id)),
        }
    } else {
        // No actual definition owner
        db.mk_error()
    }
}

fn ty_of_expr(db: &dyn db::TypeDatabase, expr: InLibrary<BodyExpr>) -> TypeId {
    let InLibrary(lib_id, body_expr @ BodyExpr(body_id, _)) = expr;

    let library = db.library(lib_id);
    let body = library.body(body_id);

    lower::ty_from_expr(db, body.in_library(lib_id), body_expr)
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

pub(crate) fn binding_def(db: &dyn TypeDatabase, bind_src: BindingSource) -> Option<DefId> {
    lookup_binding_def(db, bind_src).ok()
}

pub(crate) fn binding_to(
    db: &dyn TypeDatabase,
    bind_src: BindingSource,
) -> Result<BindingTo, NotBinding> {
    // Also find the canonical binding
    let def_id = db.resolve_def(lookup_binding_def(db, bind_src)?);

    // Take the binding kind from the def owner
    let def_owner = db.def_owner(def_id);
    let library = db.library(def_id.0);

    match def_owner {
        Some(DefOwner::Item(item_id)) => Ok(match &library.item(item_id).kind {
            item::ItemKind::ConstVar(item) if item.is_register => {
                BindingTo::Register(item.mutability)
            }
            item::ItemKind::Binding(item) if item.is_register => {
                BindingTo::Register(item.mutability)
            }
            item::ItemKind::ConstVar(item) => BindingTo::Storage(item.mutability),
            item::ItemKind::Binding(item) => BindingTo::Storage(item.mutability),
            item::ItemKind::Subprogram(item) => BindingTo::Subprogram(item.kind),
            item::ItemKind::Type(_) => BindingTo::Type,
            item::ItemKind::Module(_) => BindingTo::Module,
        }),
        Some(DefOwner::ItemParam(item_id, param_def)) => {
            // Lookup the arg
            let item = match &library.item(item_id).kind {
                item::ItemKind::Subprogram(item) => item,
                _ => unreachable!(),
            };

            Ok(match item.lookup_param_info(param_def) {
                ParameterInfo::Param(param_info) => {
                    // This is the real parameters

                    // Pass-by value parameters are always const
                    // Register parameters become register bindings
                    match (param_info.pass_by, param_info.is_register) {
                        (hir_ty::PassBy::Value, true) => BindingTo::Register(Mutability::Const),
                        (hir_ty::PassBy::Value, false) => BindingTo::Storage(Mutability::Const),
                        (hir_ty::PassBy::Reference(mutability), true) => {
                            BindingTo::Register(mutability)
                        }
                        (hir_ty::PassBy::Reference(mutability), false) => {
                            BindingTo::Storage(mutability)
                        }
                    }
                }
                ParameterInfo::Result => {
                    // This is the result parameter
                    // Always const storage, only modifiable by `result` stmts
                    BindingTo::Storage(Mutability::Const)
                }
            })
        }
        Some(DefOwner::ItemExport(..)) => {
            unreachable!("already resolved defs to canon form")
        }
        Some(DefOwner::Stmt(stmt_id)) => {
            match &library.body(stmt_id.0).stmt(stmt_id.1).kind {
                stmt::StmtKind::Item(_) => {
                    unreachable!("item def owners shouldn't be stmt def owners")
                }
                // for-loop counter var is an immutable ref
                stmt::StmtKind::For(_) => Ok(BindingTo::Storage(Mutability::Const)),
                _ => Err(NotBinding::NotBinding),
            }
        }
        // From an undeclared identifier, not purely a binding
        None => Err(NotBinding::Missing),
    }
}

fn lookup_binding_def(db: &dyn TypeDatabase, bind_src: BindingSource) -> Result<DefId, NotBinding> {
    match bind_src {
        // Trivial, def bindings are bindings to themselves
        // ???: Do we want to perform canonicalization / symbol resolution here?
        BindingSource::DefId(it) => Ok(it),
        BindingSource::Body(lib_id, body) => {
            let library = db.library(lib_id);

            match &library.body(body).kind {
                // Stmt bodies never produce bindings
                body::BodyKind::Stmts(..) => Err(NotBinding::NotBinding),
                // Defer to expr form
                body::BodyKind::Exprs(expr) => lookup_binding_def(db, (lib_id, body, *expr).into()),
            }
        }
        BindingSource::BodyExpr(lib_id, BodyExpr(body_id, expr_id)) => {
            // Traverse nodes until we encounter a valid binding
            let library = db.library(lib_id);

            // Only name exprs and fields can produce a binding
            match &library.body(body_id).expr(expr_id).kind {
                expr::ExprKind::Missing => Err(NotBinding::Missing),
                expr::ExprKind::Name(name) => match name {
                    expr::Name::Name(def_id) => Ok(DefId(lib_id, *def_id)),
                    expr::Name::Self_ => todo!(),
                },
                expr::ExprKind::Field(field) => {
                    // Look up field's corresponding def, or treat as missing if not there
                    let def_id = db
                        .fields_of((lib_id, body_id, field.lhs).into())
                        .and_then(|fields| {
                            fields.lookup(field.field.item()).map(|info| info.def_id)
                        })
                        .ok_or(NotBinding::Missing)?;

                    // Defer to the item's def
                    db.binding_def(def_id.into()).ok_or(NotBinding::Missing)
                }
                _ => Err(NotBinding::NotBinding),
            }
        }
    }
}

pub(super) fn value_produced(
    db: &dyn db::TypeDatabase,
    value_src: db::ValueSource,
) -> Result<db::ValueKind, db::NotValue> {
    use db::{NotValue, ValueKind, ValueSource};

    fn value_kind_from_binding(
        db: &dyn TypeDatabase,
        def_id: DefId,
    ) -> Result<ValueKind, NotValue> {
        let kind = db.binding_to(def_id.into());

        match kind {
            Ok(BindingTo::Storage(muta)) => Ok(ValueKind::Reference(muta)),
            Ok(BindingTo::Register(muta)) => Ok(ValueKind::Register(muta)),
            // Subprogram names are aliases of address constants
            Ok(BindingTo::Subprogram(..)) => Ok(ValueKind::Scalar),
            Err(symbol::NotBinding::Missing) => Err(NotValue::Missing),
            _ => Err(NotValue::NotValue),
        }
    }

    match value_src {
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
        ValueSource::BodyExpr(lib_id, body_expr @ BodyExpr(body_id, expr_id)) => {
            let library = db.library(lib_id);

            match &library.body(body_id).expr(expr_id).kind {
                toc_hir::expr::ExprKind::Missing => Err(NotValue::Missing),
                toc_hir::expr::ExprKind::Name(name) => match name {
                    toc_hir::expr::Name::Name(def_id) => {
                        let def_id = DefId(lib_id, *def_id);

                        if let Some(DefOwner::ItemExport(item_id, export_idx)) =
                            db.def_owner(def_id)
                        {
                            // Keep track of export mutability
                            let mutability =
                                if let item::ItemKind::Module(item) = &library.item(item_id).kind {
                                    let export =
                                        item.exports.get(export_idx).expect("bad export index");

                                    export.mutability
                                } else {
                                    unreachable!("not from a module-like")
                                };

                            // Take initially from the binding kind
                            let kind = value_kind_from_binding(db, def_id)?;

                            // Apply appropriate mutability
                            // Both must be mutable to be applicable as mutable
                            Ok(match (mutability, kind) {
                                (_, ValueKind::Scalar) => ValueKind::Scalar,
                                (Mutability::Var, ValueKind::Register(Mutability::Var)) => {
                                    ValueKind::Register(Mutability::Var)
                                }
                                (Mutability::Var, ValueKind::Reference(Mutability::Var)) => {
                                    ValueKind::Reference(Mutability::Var)
                                }
                                (_, ValueKind::Register(_)) => {
                                    ValueKind::Register(Mutability::Const)
                                }
                                (_, ValueKind::Reference(_)) => {
                                    ValueKind::Reference(Mutability::Const)
                                }
                            })
                        } else {
                            // Take directly from the binding kind
                            value_kind_from_binding(db, def_id)
                        }
                    }
                    toc_hir::expr::Name::Self_ => unimplemented!(),
                },
                toc_hir::expr::ExprKind::Field(field) => {
                    // Look up field's corresponding def & mutability, or treat as missing if not there
                    let (def_id, mutability) = db
                        .fields_of((lib_id, body_id, field.lhs).into())
                        .and_then(|fields| {
                            fields
                                .lookup(field.field.item())
                                .map(|info| (info.def_id, info.mutability))
                        })
                        .ok_or(NotValue::Missing)?;

                    // FIXME: For composite types, take into account the mutability of the reference
                    // Defer to the field's def
                    let def_id = db.binding_def(def_id.into()).ok_or(NotValue::Missing)?;
                    let kind = value_kind_from_binding(db, def_id)?;

                    // Apply appropriate mutability
                    // Both must be mutable to be applicable as mutable
                    Ok(match (mutability, kind) {
                        (_, ValueKind::Scalar) => ValueKind::Scalar,
                        (Mutability::Var, ValueKind::Register(Mutability::Var)) => {
                            ValueKind::Register(Mutability::Var)
                        }
                        (Mutability::Var, ValueKind::Reference(Mutability::Var)) => {
                            ValueKind::Reference(Mutability::Var)
                        }
                        (_, ValueKind::Register(_)) => ValueKind::Register(Mutability::Const),
                        (_, ValueKind::Reference(_)) => ValueKind::Reference(Mutability::Const),
                    })
                }
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

pub(crate) fn fields_of(
    db: &dyn db::TypeDatabase,
    source: db::FieldsSource,
) -> Option<Arc<item::Fields>> {
    match source {
        db::FieldsSource::DefId(def_id) => {
            let InLibrary(library_id, item_id) = db.item_of(def_id)?;
            let library = db.library(library_id);

            match &library.item(item_id).kind {
                item::ItemKind::Module(item) => {
                    // Build from the exports
                    let fields = item
                        .exports
                        .iter()
                        .map(|export| {
                            let local_def = library.item(export.item_id).def_id;
                            let field_name = library.local_def(local_def).name.item().clone();
                            let def_id = DefId(library_id, local_def);

                            let info = item::FieldInfo {
                                def_id,
                                mutability: export.mutability,
                                is_opaque: export.is_opaque,
                            };

                            (field_name, info)
                        })
                        .collect();

                    Some(Arc::new(item::Fields { fields }))
                }
                _ => {
                    // Defer to the corresponding type
                    let ty_id = db.type_of(def_id.into());
                    db.fields_of(ty_id.into())
                }
            }
        }
        db::FieldsSource::Type(_) => {
            // Current types do not have fields
            None
        }
        db::FieldsSource::BodyExpr(lib_id, body_expr) => {
            if let BindingTo::Module = db.binding_to((lib_id, body_expr).into()).ok()? {
                // Defer to the corresponding def
                let def_id = db.binding_def((lib_id, body_expr).into())?;
                db.fields_of(def_id.into())
            } else {
                // Defer to the corresponding type
                let ty_id = db.type_of((lib_id, body_expr).into());
                db.fields_of(ty_id.into())
            }
        }
    }
}

pub(crate) fn find_exported_def(
    db: &dyn TypeDatabase,
    value_src: db::ValueSource,
) -> Option<DefId> {
    let (library_id, library);
    let (body_id, expr_id) = match value_src {
        db::ValueSource::Body(lib_id, body_id) => {
            library_id = lib_id;
            library = db.library(lib_id);

            match &library.body(body_id).kind {
                body::BodyKind::Stmts(..) => return None,
                body::BodyKind::Exprs(expr_id) => (body_id, *expr_id),
            }
        }
        db::ValueSource::BodyExpr(lib_id, expr::BodyExpr(body_id, expr_id)) => {
            library_id = lib_id;
            library = db.library(lib_id);
            (body_id, expr_id)
        }
    };

    // Only name & field exprs provide access to exported defs
    match &library.body(body_id).expr(expr_id).kind {
        expr::ExprKind::Name(expr) => {
            match expr {
                expr::Name::Name(local_def) => {
                    // Take from the def owner
                    if let Some(DefOwner::ItemExport(item_id, export_idx)) =
                        db.def_owner(DefId(library_id, *local_def))
                    {
                        if let item::ItemKind::Module(item) = &library.item(item_id).kind {
                            let export = item.exports.get(export_idx).expect("bad export index");

                            Some(DefId(library_id, export.def_id))
                        } else {
                            unreachable!("not from a module-like")
                        }
                    } else {
                        // Not an item export
                        None
                    }
                }
                expr::Name::Self_ => None,
            }
        }
        expr::ExprKind::Field(expr) => {
            let lhs_def = db.binding_def((library_id, body_id, expr.lhs).into())?;

            if let Some(DefOwner::Item(item_id)) = db.def_owner(lhs_def) {
                if let item::ItemKind::Module(module) = &library.item(item_id).kind {
                    // Find matching export
                    module.exports.iter().find_map(|export| {
                        (library.local_def(export.def_id).name.item() == expr.field.item())
                            .then(|| DefId(library_id, export.def_id))
                    })
                } else {
                    // Not from a module-like item
                    None
                }
            } else {
                // Not from a def
                None
            }
        }
        _ => None,
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

    fn mk_set(&self, with_def: WithDef, elem_ty: TypeId) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Set(with_def, elem_ty),
            }
            .into(),
        )
    }

    fn mk_pointer(&self, checked: Checked, target_ty: TypeId) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Pointer(checked, target_ty),
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

    fn mk_void(&self) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Void,
            }
            .into(),
        )
    }
}
