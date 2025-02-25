//! Type-related query implementation

use std::sync::Arc;

use toc_hir::item::{self, ImportMutability};
use toc_hir::symbol::{IsRegister, Mutability, SymbolKind};
use toc_hir::ty::PassBy;
use toc_hir::{OrMissingExt, body, expr};
use toc_hir::{
    body::BodyId,
    expr::BodyExpr,
    package::{InPackage, WrapInPackage},
    symbol,
    symbol::{DefId, DefOwner},
    ty::TypeId as HirTypeId,
};

use crate::db::{self, BindingSource, TypeDatabase};
use crate::ty::{TypeId, TypeKind, lower, make};

pub(crate) fn lower_hir_type(db: &dyn db::TypeDatabase, type_id: InPackage<HirTypeId>) -> TypeId {
    lower::ty_from_hir_ty(db, type_id)
}

pub(crate) fn type_of(db: &dyn db::TypeDatabase, source: db::TypeSource) -> TypeId {
    match source {
        db::TypeSource::Def(def_id) => ty_of_def(db, def_id),
        db::TypeSource::Item(package_id, item_id) => {
            lower::ty_from_item(db, InPackage(package_id, item_id))
        }
        db::TypeSource::BodyExpr(id, expr) => ty_of_expr(db, InPackage(id, expr)),
        db::TypeSource::Body(id, body) => ty_of_body(db, InPackage(id, body)),
    }
}

fn ty_of_def(db: &dyn db::TypeDatabase, def_id: DefId) -> TypeId {
    if let Some(owner) = db.def_owner(def_id) {
        match owner {
            DefOwner::Item(item_id) => lower::ty_from_item(db, InPackage(def_id.0, item_id)),
            DefOwner::ItemParam(item_id, param_def) => {
                lower::ty_from_item_param(db, InPackage(def_id.0, (item_id, param_def)))
            }
            DefOwner::Export(module_id, export_id) => {
                // Refer to the corresponding exported item
                let package = db.package(def_id.0);

                let module = package.module_item(module_id);
                let export = module.export(export_id);
                let def_id = DefId(def_id.0, export.exported_def);

                db.type_of(def_id.into())
            }
            DefOwner::Field(type_id, field_id) => {
                lower::ty_from_ty_field(db, InPackage(def_id.0, type_id), field_id)
            }
            DefOwner::Stmt(stmt_id) => lower::ty_from_stmt(db, InPackage(def_id.0, stmt_id)),
        }
    } else {
        // No actual definition owner
        make::error(db)
    }
}

fn ty_of_expr(db: &dyn db::TypeDatabase, expr: InPackage<BodyExpr>) -> TypeId {
    let InPackage(pkg_id, body_expr @ BodyExpr(body_id, _)) = expr;

    let package = db.package(pkg_id);
    let body = package.body(body_id);

    lower::ty_from_expr(db, body.in_package(pkg_id), body_expr)
}

fn ty_of_body(db: &dyn db::TypeDatabase, body_id: InPackage<BodyId>) -> TypeId {
    let package = db.package(body_id.0);
    let body = package.body(body_id.1);

    match &body.kind {
        toc_hir::body::BodyKind::Stmts(..) => {
            lower::ty_from_body_owner(db, body_id.0, db.body_owner(body_id))
        }
        toc_hir::body::BodyKind::Exprs(expr) => db.type_of((body_id.0, body_id.1, *expr).into()),
    }
}

pub(crate) fn unresolved_binding_def(
    db: &dyn TypeDatabase,
    bind_src: BindingSource,
) -> Option<DefId> {
    lookup_binding_def(db, bind_src)
}

pub(crate) fn binding_def(db: &dyn TypeDatabase, bind_src: BindingSource) -> Option<DefId> {
    unresolved_binding_def(db, bind_src).and_then(|def_id| db.resolve_def(def_id).ok())
}

fn lookup_binding_def(db: &dyn TypeDatabase, bind_src: BindingSource) -> Option<DefId> {
    match bind_src {
        // Trivial, def bindings are bindings to themselves
        // We don't want to perform sym-res here, since that means
        // we can't figure out if something refers to an import or not
        BindingSource::DefId(it) => Some(it),
        BindingSource::Body(pkg_id, body) => {
            let package = db.package(pkg_id);

            match &package.body(body).kind {
                // Stmt bodies never produce bindings
                body::BodyKind::Stmts(..) => None,
                // Defer to expr form
                body::BodyKind::Exprs(expr) => lookup_binding_def(db, (pkg_id, body, *expr).into()),
            }
        }
        BindingSource::BodyExpr(pkg_id, BodyExpr(body_id, expr_id)) => {
            // Traverse nodes until we encounter a valid binding
            let package = db.package(pkg_id);

            // Only name exprs and fields can produce a binding
            match &package.body(body_id).expr(expr_id).kind {
                expr::ExprKind::Missing => None,
                expr::ExprKind::Name(name) => match name {
                    expr::Name::Name(binding) => match package.binding_resolve(*binding) {
                        symbol::Resolve::Def(local_def) => Some(DefId(pkg_id, local_def)),
                        symbol::Resolve::Err => None,
                    },
                    expr::Name::Self_ => todo!(),
                },
                expr::ExprKind::Field(field) => {
                    // Look up field's corresponding def, or treat as missing if not there
                    db.fields_of((pkg_id, body_id, field.lhs).into())
                        .and_then(|fields| {
                            fields.lookup(*field.field.item()).map(|info| info.def_id)
                        })
                }
                _ => None,
            }
        }
    }
}

pub(super) fn value_produced(
    db: &dyn db::TypeDatabase,
    value_src: db::ValueSource,
) -> Result<db::ValueKind, db::NotValue> {
    use db::{NotValue, ValueKind, ValueSource};

    fn value_with_mutability(muta: Mutability, reg: IsRegister) -> ValueKind {
        match reg {
            symbol::IsRegister::No => ValueKind::Reference(muta),
            symbol::IsRegister::Yes => ValueKind::Register(muta),
        }
    }

    fn value_kind_from_binding(
        db: &dyn TypeDatabase,
        def_id: DefId,
    ) -> Result<ValueKind, NotValue> {
        let def_id = db.resolve_def(def_id).map_err(|_| NotValue::Missing)?;
        let kind = db.symbol_kind(def_id);

        match kind {
            Some(
                SymbolKind::ConstVar(muta, reg)
                | SymbolKind::Binding(muta, reg)
                | SymbolKind::Param(PassBy::Reference(muta), reg),
            ) => Ok(value_with_mutability(muta, reg)),
            Some(SymbolKind::Param(PassBy::Value, reg)) => {
                Ok(value_with_mutability(Mutability::Const, reg))
            }
            // Subprogram names are aliases of address constants
            Some(SymbolKind::Subprogram(..)) => Ok(ValueKind::Scalar),
            // Enum variants (right now) are equivalent to associated consts
            Some(SymbolKind::EnumVariant) => Ok(ValueKind::Reference(Mutability::Const)),
            None => Err(NotValue::Missing),
            _ => Err(NotValue::NotValue),
        }
    }

    fn expr_fallback(
        db: &dyn TypeDatabase,
        pkg_id: toc_hir::package::PackageId,
        body_expr: BodyExpr,
    ) -> Result<db::ValueKind, db::NotValue> {
        let expr_ty = db.type_of((pkg_id, body_expr).into());
        let expr_ty_ref = expr_ty.to_base_type(db);

        match expr_ty_ref.kind(db) {
            kind if kind.is_scalar() => Ok(ValueKind::Scalar),
            kind if !kind.is_error() => Ok(ValueKind::Reference(symbol::Mutability::Const)),
            _ => Err(NotValue::Missing),
        }
    }

    match value_src {
        ValueSource::Def(def_id) => value_kind_from_binding(db, def_id),
        ValueSource::Body(pkg_id, body_id) => {
            let package = db.package(pkg_id);
            let body = package.body(body_id);

            // Take from the main body expr
            match body.kind {
                toc_hir::body::BodyKind::Stmts(_, _, _) => Err(NotValue::NotValue),
                toc_hir::body::BodyKind::Exprs(expr_id) => {
                    db.value_produced((pkg_id, body_id, expr_id).into())
                }
            }
        }
        ValueSource::BodyExpr(pkg_id, body_expr @ BodyExpr(body_id, expr_id)) => {
            let package = db.package(pkg_id);

            match &package.body(body_id).expr(expr_id).kind {
                expr::ExprKind::Missing => Err(NotValue::Missing),
                expr::ExprKind::Name(name) => match name {
                    expr::Name::Name(binding) => {
                        let def_id = match package.binding_resolve(*binding) {
                            symbol::Resolve::Def(local_def) => DefId(pkg_id, local_def),
                            symbol::Resolve::Err => return Err(NotValue::Missing),
                        };

                        match db.def_owner(def_id) {
                            Some(DefOwner::Export(mod_id, export_id)) => {
                                // Keep track of export mutability
                                let mutability =
                                    package.module_item(mod_id).export(export_id).mutability;

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
                            Some(DefOwner::Item(item_id)) => {
                                match &package.item(item_id).kind {
                                    // Special case for imports
                                    item::ItemKind::Import(item) => {
                                        let mutability = item.mutability;

                                        // Take initially from the binding kind
                                        let kind = value_kind_from_binding(db, def_id)?;

                                        // Apply appropriate mutability
                                        // We're trusting that the appropriate mutability is applicable
                                        // FIXME: Do the same thing for exports
                                        Ok(match kind {
                                            ValueKind::Scalar => ValueKind::Scalar,
                                            ValueKind::Reference(_) => match mutability {
                                                ImportMutability::SameAsItem => kind,
                                                ImportMutability::Explicit(muta, _) => {
                                                    ValueKind::Reference(muta)
                                                }
                                            },
                                            ValueKind::Register(_) => match mutability {
                                                ImportMutability::SameAsItem => kind,
                                                ImportMutability::Explicit(muta, _) => {
                                                    ValueKind::Register(muta)
                                                }
                                            },
                                        })
                                    }
                                    // Take directly from the binding kind
                                    _ => value_kind_from_binding(db, def_id),
                                }
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
                        .fields_of((pkg_id, body_id, field.lhs).into())
                        .and_then(|fields| {
                            fields
                                .lookup(*field.field.item())
                                .map(|info| (info.def_id, info.mutability))
                        })
                        .ok_or(NotValue::Missing)?;

                    // FIXME: For composite types, take into account the mutability of the reference
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
                    // The following does always produces a const reference:
                    // - set cons
                    //
                    // The following does produce references, depending on the inherited
                    // mutability:
                    // - subprog call
                    // - array indexing
                    // - pointer ascription
                    let lhs_expr = (pkg_id, body_id, expr.lhs);
                    let lhs_mut = db
                        .value_produced(lhs_expr.into())
                        .ok()
                        .and_then(ValueKind::mutability);

                    // Note: the following yoinked from typeck_call
                    // FIXME: really extract this into a `ty::rules` fn

                    // Fetch type of lhs
                    // Always try to do it by `DefId` first, so that we can properly support paren-less functions
                    // We still need to defer to expression type lookup, since things like `expr::Deref` can produce
                    // references to subprograms.
                    let (lhs_ty, from_ty_binding) =
                        if let Some(def_id) = db.binding_def(lhs_expr.into()) {
                            // From an item
                            (
                                db.type_of(def_id.into()),
                                db.symbol_kind(def_id).is_missing_or(SymbolKind::is_type),
                            )
                        } else {
                            // From an actual expression
                            (db.type_of(lhs_expr.into()), false)
                        };
                    let in_module = db.inside_module(lhs_expr.into());
                    let lhs_tyref = lhs_ty.peel_opaque(db, in_module).to_base_type(db);

                    // Bail on error types
                    if lhs_tyref.kind(db).is_error() {
                        return Err(NotValue::Missing);
                    }

                    enum CallKind<'db> {
                        SubprogramCall,
                        SetCons(TypeId),
                        ArrayIndexing(&'db [TypeId]),
                    }

                    // Check if lhs is callable
                    let has_parens = true;
                    let call_kind = match lhs_tyref.kind(db) {
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

                    let Some(call_kind) = call_kind else {
                        // Defer to the fallback
                        // This always produces some sort of value
                        //
                        // ???: Does this make sense? this is mostly here to prevent panicking
                        // when expecting a reference value, but we get a non-value
                        return expr_fallback(db, pkg_id, body_expr);
                    };

                    match call_kind {
                        CallKind::SubprogramCall => {
                            // Defer to the default case
                            expr_fallback(db, pkg_id, body_expr)
                        }
                        CallKind::SetCons(..) => Ok(ValueKind::Reference(Mutability::Const)),
                        CallKind::ArrayIndexing(..) => {
                            Ok(ValueKind::Reference(lhs_mut.unwrap_or(Mutability::Const)))
                        }
                    }
                }
                _ => {
                    // Take from the expr's type (always produces a value)
                    expr_fallback(db, pkg_id, body_expr)
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
            let def_id = db.resolve_def(def_id).ok()?;
            let InPackage(package_id, item_id) = db.item_of(def_id)?;
            let package = db.package(package_id);
            let item = package.item(item_id);

            match &item.kind {
                item::ItemKind::Module(item) => {
                    // Build from the exports
                    let fields = item
                        .exports
                        .iter()
                        .map(|export| {
                            let local_def = export.exported_def;
                            let field_name = package.local_def(local_def).name;
                            let def_id = DefId(package_id, local_def);

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
                    let ty_id = db.type_of(DefId(package_id, item.def_id).into());

                    db.fields_of(db::FieldSource::TypeAssociated(ty_id, in_module))
                }
            }
        }
        db::FieldSource::TypeAssociated(ty_id, in_module) => {
            // Fields associated with the type's definition
            let ty_ref = ty_id.peel_opaque(db, in_module).peel_aliases(db);

            // Only applicable for enums
            let TypeKind::Enum(with_def, variants) = ty_ref.kind(db) else {
                return None;
            };
            let package = db.package(with_def.def_id().package());

            let fields = variants
                .iter()
                .map(|&def_id| {
                    let def_info = package.local_def(def_id.1);
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
            let ty_ref = ty_id.peel_opaque(db, in_module).peel_aliases(db);

            match ty_ref.kind(db) {
                // While an enum does have fields, it's attached to the type
                // binding itself, not to anything with an enum type
                TypeKind::Enum(..) => None,
                // The rest of the types do not have fields
                _ => None,
            }
        }
        db::FieldSource::BodyExpr(pkg_id, body_expr) => {
            let in_module = db.inside_module((pkg_id, body_expr).into());
            let binding_def = db.binding_def((pkg_id, body_expr).into())?;
            let binding_to = db.symbol_kind(binding_def)?;

            match binding_to {
                SymbolKind::Module(_) => {
                    // Exports from a given module
                    // Defer to the corresponding def
                    db.fields_of((binding_def, in_module).into())
                }
                SymbolKind::Type => {
                    // Fields associated with the type
                    let ty_id = db.type_of((pkg_id, body_expr).into());

                    db.fields_of(db::FieldSource::TypeAssociated(ty_id, in_module))
                }
                kind if kind.is_ref() => {
                    // To any reference
                    // Get fields based off of the type (instance fields)
                    let ty_id = db.type_of((pkg_id, body_expr).into());

                    db.fields_of(db::FieldSource::TypeInstance(ty_id, in_module))
                }
                _ => None,
            }
        }
    }
}

pub(crate) fn exporting_def(db: &dyn TypeDatabase, bind_src: db::BindingSource) -> Option<DefId> {
    let (package_id, package);
    let (body_id, expr_id) = match bind_src {
        db::BindingSource::DefId(def_id @ DefId(pkg_id, _)) => {
            package_id = pkg_id;
            package = db.package(pkg_id);

            // Take from the def owner
            let export_def = if let Some(DefOwner::Export(mod_id, export_id)) = db.def_owner(def_id)
            {
                let export_def = package.module_item(mod_id).export(export_id).def_id;
                Some(DefId(package_id, export_def))
            } else {
                // Not an item export
                None
            };

            return export_def;
        }
        db::BindingSource::Body(pkg_id, body_id) => {
            package_id = pkg_id;
            package = db.package(pkg_id);

            match &package.body(body_id).kind {
                body::BodyKind::Stmts(..) => return None,
                body::BodyKind::Exprs(expr_id) => (body_id, *expr_id),
            }
        }
        db::BindingSource::BodyExpr(pkg_id, expr::BodyExpr(body_id, expr_id)) => {
            package_id = pkg_id;
            package = db.package(pkg_id);
            (body_id, expr_id)
        }
    };

    // Only name & field exprs provide access to exported defs
    match &package.body(body_id).expr(expr_id).kind {
        expr::ExprKind::Name(expr) => {
            match expr {
                expr::Name::Name(binding) => {
                    // Take from the def
                    let def_id = match package.binding_resolve(*binding) {
                        symbol::Resolve::Def(local_def) => DefId(package_id, local_def),
                        symbol::Resolve::Err => return None,
                    };

                    db.exporting_def(def_id.into())
                }
                expr::Name::Self_ => None,
            }
        }
        expr::ExprKind::Field(expr) => {
            // Peek at the def referenced by lhs
            let lhs_def = db.binding_def((package_id, body_id, expr.lhs).into())?;

            if let Some(DefOwner::Item(item_id)) = db.def_owner(lhs_def) {
                if let item::ItemKind::Module(module) = dbg!(&package.item(item_id).kind) {
                    // Find matching export
                    module.exports.iter().find_map(|export| {
                        let found = package.local_def(export.def_id).name == *expr.field.item();
                        found.then_some(DefId(package_id, export.def_id))
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
