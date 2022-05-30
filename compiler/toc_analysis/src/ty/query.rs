//! Type-related query implementation

use std::sync::Arc;

use toc_hir::item::{self, ImportMutability, ParameterInfo};
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

use crate::const_eval;
use crate::db::{self, BindingSource, TypeDatabase};
use crate::ty::{EndBound, WithDef};

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
            DefOwner::Export(module_id, export_id) => {
                // Refer to the corresponding exported item
                let library = db.library(def_id.0);

                let module = library.module_item(module_id);
                let export = module.export(export_id);
                let def_id = DefId(def_id.0, library.item(export.item_id).def_id);

                db.type_of(def_id.into())
            }
            DefOwner::Import(..) => {
                // Get to the canonical def
                let def_id = db.resolve_def(def_id);
                db.type_of(def_id.into())
            }
            DefOwner::Field(type_id, field_id) => {
                lower::ty_from_ty_field(db, InLibrary(def_id.0, type_id), field_id)
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
        Some(DefOwner::Export(..)) | Some(DefOwner::Import(..)) => {
            unreachable!("already resolved defs to canon form")
        }
        Some(DefOwner::Field(type_id, _field_id)) => match &library.lookup_type(type_id).kind {
            hir_ty::TypeKind::Enum(..) => Ok(BindingTo::EnumField),
            _ => unreachable!("field def owner on a non-field type"),
        },
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
                            fields.lookup(*field.field.item()).map(|info| info.def_id)
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
            // Enum fields (right now) are equivalent to associated consts
            Ok(BindingTo::EnumField) => Ok(ValueKind::Reference(Mutability::Const)),
            Err(symbol::NotBinding::Missing) => Err(NotValue::Missing),
            _ => Err(NotValue::NotValue),
        }
    }

    fn expr_fallback(
        db: &dyn TypeDatabase,
        lib_id: toc_hir::library::LibraryId,
        body_expr: BodyExpr,
    ) -> Result<db::ValueKind, db::NotValue> {
        let expr_ty = db.type_of((lib_id, body_expr).into());
        let expr_ty_ref = expr_ty.in_db(db).to_base_type();

        match expr_ty_ref.kind() {
            kind if kind.is_scalar() => Ok(ValueKind::Scalar),
            kind if !kind.is_error() => Ok(ValueKind::Reference(symbol::Mutability::Const)),
            _ => Err(NotValue::Missing),
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
                expr::ExprKind::Missing => Err(NotValue::Missing),
                expr::ExprKind::Name(name) => match name {
                    expr::Name::Name(def_id) => {
                        let def_id = DefId(lib_id, *def_id);

                        match db.def_owner(def_id) {
                            Some(DefOwner::Export(mod_id, export_id)) => {
                                // Keep track of export mutability
                                let mutability =
                                    library.module_item(mod_id).export(export_id).mutability;

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
                            }
                            Some(DefOwner::Import(item_id, import_id)) => {
                                // Keep track of import mutability
                                let mutability = match &library.item(item_id).kind {
                                    item::ItemKind::Subprogram(item) => {
                                        item.body.imports[import_id.0].mutability
                                    }
                                    item::ItemKind::Module(item) => {
                                        item.imports[import_id.0].mutability
                                    }
                                    _ => unreachable!(),
                                };

                                // Take initially from the binding kind
                                let kind = value_kind_from_binding(db, def_id)?;

                                // Apply appropriate mutability
                                // We're trusting that the appropriate mutability is applicable
                                // FIXME: Do the same thing for exports
                                Ok(match kind {
                                    ValueKind::Scalar => ValueKind::Scalar,
                                    ValueKind::Reference(_) => match mutability {
                                        ImportMutability::SameAsItem => kind,
                                        ImportMutability::Explicit(muta) => {
                                            ValueKind::Reference(muta)
                                        }
                                    },
                                    ValueKind::Register(_) => match mutability {
                                        ImportMutability::SameAsItem => kind,
                                        ImportMutability::Explicit(muta) => {
                                            ValueKind::Register(muta)
                                        }
                                    },
                                })
                            }
                            _ => {
                                // Take directly from the binding kind
                                value_kind_from_binding(db, def_id)
                            }
                        }
                    }
                    expr::Name::Self_ => unimplemented!(),
                },
                expr::ExprKind::Field(field) => {
                    // Look up field's corresponding def & mutability, or treat as missing if not there
                    let (def_id, mutability) = db
                        .fields_of((lib_id, body_id, field.lhs).into())
                        .and_then(|fields| {
                            fields
                                .lookup(*field.field.item())
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
                expr::ExprKind::Deref(_) => {
                    // Always produces a mutable reference
                    // Non-ptr types are handled by normal typeck
                    Ok(ValueKind::Reference(Mutability::Var))
                }
                expr::ExprKind::Literal(literal) => match literal {
                    toc_hir::expr::Literal::CharSeq(_) | toc_hir::expr::Literal::String(_) => {
                        Ok(ValueKind::Reference(symbol::Mutability::Const))
                    }
                    _ => Ok(ValueKind::Scalar),
                },
                expr::ExprKind::Call(expr) => {
                    use toc_hir::symbol::BindingResultExt;
                    // The following does always produces a const reference:
                    // - set cons
                    //
                    // The following does produce references, depending on the inherited
                    // mutability:
                    // - subprog call
                    // - array indexing
                    // - pointer ascription

                    // Note: yoinked from typeck
                    // FIXME: really extract this into a `ty::rules` fn
                    let lhs_expr = (lib_id, body_id, expr.lhs);
                    let lhs_mut = match db.value_produced(lhs_expr.into()) {
                        Ok(ValueKind::Reference(muta)) | Ok(ValueKind::Register(muta)) => {
                            Some(muta)
                        }
                        _ => None,
                    };

                    // Fetch type of lhs
                    // Always try to do it by `DefId` first, so that we can properly support paren-less functions
                    // We still need to defer to expression type lookup, since things like `expr::Deref` can produce
                    // references to subprograms.
                    let (lhs_ty, from_ty_binding) =
                        if let Some(def_id) = db.binding_def(lhs_expr.into()) {
                            // From an item
                            let is_ty_binding = db
                                .binding_to(def_id.into())
                                .map(|a| a.is_type())
                                .or_missing();
                            (db.type_of(def_id.into()), is_ty_binding)
                        } else {
                            // From an actual expression
                            (db.type_of(lhs_expr.into()), false)
                        };
                    let in_module = db.inside_module(lhs_expr.into());
                    let lhs_tyref = lhs_ty.in_db(db).peel_opaque(in_module).to_base_type();

                    // Bail on error types
                    if lhs_tyref.kind().is_error() {
                        return Err(NotValue::Missing);
                    }

                    enum CallKind<'db> {
                        SubprogramCall,
                        SetCons(TypeId),
                        ArrayIndexing(&'db [TypeId]),
                    }

                    // Check if lhs is callable
                    let has_parens = true;
                    let call_kind = match lhs_tyref.kind() {
                        TypeKind::Subprogram(toc_hir::symbol::SubprogramKind::Process, ..) => None,
                        // Parens are only potentially optional in subprograms
                        TypeKind::Subprogram(..) if !from_ty_binding => {
                            Some(CallKind::SubprogramCall)
                        }
                        // All the other kinds require parens
                        TypeKind::Set(_, elem_ty) if has_parens && from_ty_binding => {
                            Some(CallKind::SetCons(*elem_ty))
                        }
                        TypeKind::Array(_, ranges, _) if has_parens && !from_ty_binding => {
                            Some(CallKind::ArrayIndexing(ranges.as_slice()))
                        }
                        _ => None,
                    };

                    let call_kind = if let Some(call_kind) = call_kind {
                        call_kind
                    } else {
                        // Defer to the fallback
                        // This always produces some sort of value
                        //
                        // ???: Does this make sense? this is mostly here to prevent panicking
                        // when expecting a reference value, but we get a non-value
                        return expr_fallback(db, lib_id, body_expr);
                    };

                    match call_kind {
                        CallKind::SubprogramCall => {
                            // Defer to the default case
                            expr_fallback(db, lib_id, body_expr)
                        }
                        CallKind::SetCons(..) => Ok(ValueKind::Reference(Mutability::Const)),
                        CallKind::ArrayIndexing(..) => {
                            Ok(ValueKind::Reference(lhs_mut.unwrap_or(Mutability::Const)))
                        }
                    }
                }
                _ => {
                    // Take from the expr's type (always produces a value)
                    expr_fallback(db, lib_id, body_expr)
                }
            }
        }
    }
}

pub(crate) fn fields_of(
    db: &dyn db::TypeDatabase,
    source: db::FieldSource,
) -> Option<Arc<item::Fields>> {
    match source {
        db::FieldSource::DefId(def_id, in_module) => {
            // Defer to the owning item
            let InLibrary(library_id, item_id) = db.item_of(db.resolve_def(def_id))?;
            let library = db.library(library_id);
            let item = library.item(item_id);

            match &item.kind {
                item::ItemKind::Module(item) => {
                    // Build from the exports
                    let fields = item
                        .exports
                        .iter()
                        .map(|export| {
                            let local_def = library.item(export.item_id).def_id;
                            let field_name = library.local_def(local_def).name;
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
                    // Defer to the corresponding type (associated fields)
                    let ty_id = db.type_of(DefId(library_id, item.def_id).into());

                    db.fields_of(db::FieldSource::TypeAssociated(ty_id, in_module))
                }
            }
        }
        db::FieldSource::TypeAssociated(ty_id, in_module) => {
            // Fields associated with the type's definition
            let ty_ref = ty_id.in_db(db).peel_opaque(in_module).peel_aliases();

            // Only applicable for enums
            let (library, variants) = if let TypeKind::Enum(with_def, variants) = ty_ref.kind() {
                (db.library(with_def.def_id().0), variants)
            } else {
                return None;
            };

            let fields = variants
                .iter()
                .map(|&def_id| {
                    let def_info = library.local_def(def_id.1);
                    let field_info = item::FieldInfo {
                        def_id,
                        mutability: Mutability::Const,
                        is_opaque: false,
                    };

                    (def_info.name, field_info)
                })
                .collect();

            Some(Arc::new(item::Fields { fields }))
        }
        db::FieldSource::TypeInstance(ty_id, in_module) => {
            // Fields on an instance of a type
            let ty_ref = ty_id.in_db(db).peel_opaque(in_module).peel_aliases();

            match ty_ref.kind() {
                // While an enum does have fields, it's attached to the type
                // binding itself, not to anything with an enum type
                TypeKind::Enum(..) => None,
                // The rest of the types do not have fields
                _ => None,
            }
        }
        db::FieldSource::BodyExpr(lib_id, body_expr) => {
            let in_module = db.inside_module((lib_id, body_expr).into());
            let binding_to = db.binding_to((lib_id, body_expr).into()).ok()?;

            match binding_to {
                BindingTo::Module => {
                    // Exports from a given module
                    // Defer to the corresponding def
                    let def_id = db.binding_def((lib_id, body_expr).into())?;
                    db.fields_of((def_id, in_module).into())
                }
                BindingTo::Type => {
                    // Fields associated with the type
                    let ty_id = db.type_of((lib_id, body_expr).into());

                    db.fields_of(db::FieldSource::TypeAssociated(ty_id, in_module))
                }
                BindingTo::Storage(_) | BindingTo::Register(_) => {
                    // To some storage
                    // Get fields based off of the type (instance fields)
                    let ty_id = db.type_of((lib_id, body_expr).into());

                    db.fields_of(db::FieldSource::TypeInstance(ty_id, in_module))
                }
                _ => None,
            }
        }
    }
}

pub(crate) fn find_exported_def(
    db: &dyn TypeDatabase,
    bind_src: db::BindingSource,
) -> Option<DefId> {
    let (library_id, library);
    let (body_id, expr_id) = match bind_src {
        db::BindingSource::DefId(def_id @ DefId(lib_id, _)) => {
            library_id = lib_id;
            library = db.library(lib_id);

            // Take from the def owner
            let export_def = if let Some(DefOwner::Export(mod_id, export_id)) = db.def_owner(def_id)
            {
                let export_def = library.module_item(mod_id).export(export_id).def_id;
                Some(DefId(library_id, export_def))
            } else {
                // Not an item export
                None
            };

            return export_def;
        }
        db::BindingSource::Body(lib_id, body_id) => {
            library_id = lib_id;
            library = db.library(lib_id);

            match &library.body(body_id).kind {
                body::BodyKind::Stmts(..) => return None,
                body::BodyKind::Exprs(expr_id) => (body_id, *expr_id),
            }
        }
        db::BindingSource::BodyExpr(lib_id, expr::BodyExpr(body_id, expr_id)) => {
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
                    // Take from the def
                    db.exporting_def(DefId(library_id, *local_def).into())
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
                        (library.local_def(export.def_id).name == *expr.field.item())
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

    fn mk_opaque(&self, def_id: DefId, base_ty: TypeId) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Opaque(def_id, base_ty),
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

    fn mk_constrained(&self, base_ty: TypeId, start: const_eval::Const, end: EndBound) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Constrained(base_ty, start, end),
            }
            .into(),
        )
    }

    fn mk_array(
        &self,
        sizing: super::ArraySizing,
        ranges: Vec<super::TypeId>,
        elem_ty: super::TypeId,
    ) -> super::TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Array(sizing, ranges, elem_ty),
            }
            .into(),
        )
    }

    fn mk_enum(&self, with_def: WithDef, variants: Vec<DefId>) -> TypeId {
        self.intern_type(
            Type {
                kind: TypeKind::Enum(with_def, variants),
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
