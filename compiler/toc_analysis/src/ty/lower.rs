//! Lowering HIR entities into analysis types

use std::convert::TryInto;

use toc_hir::{
    body, expr, item,
    library::{InLibrary, LibraryId, WrapInLibrary},
    stmt,
    symbol::{self, BindingResultExt, BindingTo, DefId, LocalDefId},
    ty as hir_ty,
};

use crate::{
    const_eval::{Const, ConstInt},
    db::TypeDatabase,
    ty::{self, Param, TypeId, TypeKind},
};

use super::{IntSize, NatSize, RealSize, SeqSize};

pub(crate) fn ty_from_hir_ty(db: &dyn TypeDatabase, hir_id: InLibrary<hir_ty::TypeId>) -> TypeId {
    let library = db.library(hir_id.0);
    let hir_ty = library.lookup_type(hir_id.1);

    match &hir_ty.kind {
        hir_ty::TypeKind::Missing => db.mk_error(),
        hir_ty::TypeKind::Primitive(ty) => primitive_ty(db, hir_id, ty),
        hir_ty::TypeKind::Alias(ty) => alias_ty(db, hir_id, ty),
        hir_ty::TypeKind::Subprogram(ty) => subprogram_ty(db, hir_id, ty),
        hir_ty::TypeKind::Void => db.mk_void(),
    }
}

fn primitive_ty(
    db: &dyn TypeDatabase,
    hir_id: InLibrary<hir_ty::TypeId>,
    ty: &hir_ty::Primitive,
) -> TypeId {
    // Create the correct type based off of the base primitive type
    match ty {
        hir_ty::Primitive::Int => db.mk_int(IntSize::Int),
        hir_ty::Primitive::Int1 => db.mk_int(IntSize::Int1),
        hir_ty::Primitive::Int2 => db.mk_int(IntSize::Int2),
        hir_ty::Primitive::Int4 => db.mk_int(IntSize::Int4),
        hir_ty::Primitive::Nat => db.mk_nat(NatSize::Nat),
        hir_ty::Primitive::Nat1 => db.mk_nat(NatSize::Nat1),
        hir_ty::Primitive::Nat2 => db.mk_nat(NatSize::Nat2),
        hir_ty::Primitive::Nat4 => db.mk_nat(NatSize::Nat4),
        hir_ty::Primitive::Real => db.mk_real(RealSize::Real),
        hir_ty::Primitive::Real4 => db.mk_real(RealSize::Real4),
        hir_ty::Primitive::Real8 => db.mk_real(RealSize::Real8),
        hir_ty::Primitive::Boolean => db.mk_boolean(),
        hir_ty::Primitive::AddressInt => db.mk_nat(NatSize::AddressInt),
        hir_ty::Primitive::Char => db.mk_char(),
        hir_ty::Primitive::String => db.mk_string(),
        hir_ty::Primitive::SizedChar(len) => db.mk_char_n(lower_seq_len(hir_id.0, *len)),
        hir_ty::Primitive::SizedString(len) => db.mk_string_n(lower_seq_len(hir_id.0, *len)),
    }
}

fn lower_seq_len(library: LibraryId, seq_len: hir_ty::SeqLength) -> SeqSize {
    match seq_len {
        hir_ty::SeqLength::Any => SeqSize::Any,
        hir_ty::SeqLength::Expr(body) => SeqSize::Fixed(Const::from_body(library, body)),
    }
}

fn alias_ty(
    db: &dyn TypeDatabase,
    hir_id: InLibrary<hir_ty::TypeId>,
    ty: &hir_ty::Alias,
) -> TypeId {
    let def_id = DefId(hir_id.0, ty.0);

    if db
        .binding_to(def_id.into())
        .map(BindingTo::is_type)
        .or_missing()
    {
        // Defer to the type's definition
        db.type_of(def_id.into())
    } else {
        // Not a reference to a type
        db.mk_error()
    }
}

fn subprogram_ty(
    db: &dyn TypeDatabase,
    hir_id: InLibrary<hir_ty::TypeId>,
    ty: &hir_ty::Subprogram,
) -> TypeId {
    let library_id = hir_id.0;
    let params = subprogram_param_list(db, library_id, ty.param_list.as_ref());
    let result = require_resolved_hir_type(db, ty.result_ty.in_library(library_id));

    // NOTE(ctc-divergence): This is one of the cases where we diverge from `ctc`
    // `ctc` always promotes param-less types to ones with no parameters,
    // whereas `toc` carries through the param-less property.
    //
    // This divergence becomes apparent when referring to a var with this type.
    // For example:
    // ```turing
    // var a : function thing : int
    // var target : int := a
    // ```
    //
    // - ctc: Fails to compile, cannot assign `target` with type `function (): int`
    // - toc: Compiles, since the reference to `a` invokes the function

    // FIXME: This divergent behaviour causes an incorrect report type
    //
    // ```turing
    //   var a : function : int
    //   function pie() : int result 1 end pie
    //   a := pie
    // % ^ this is of type `int`
    // ```
    //
    // Should be incorrect binding kind

    db.mk_subprogram(ty.kind, params, result)
}

fn subprogram_param_list(
    db: &dyn TypeDatabase,
    library_id: LibraryId,
    param_list: Option<&Vec<hir_ty::Parameter>>,
) -> Option<Vec<Param>> {
    let param_list = param_list?
        .iter()
        .map(|param| {
            let param_ty = require_resolved_hir_type(db, param.param_ty.in_library(library_id));
            ty::Param {
                is_register: param.is_register,
                pass_by: match param.pass_by {
                    hir_ty::PassBy::Value => ty::PassBy::Value,
                    hir_ty::PassBy::Reference(kind) => ty::PassBy::Reference(kind),
                },
                coerced_type: param.coerced_type,
                param_ty,
            }
        })
        .collect();

    Some(param_list)
}

pub(crate) fn ty_from_item(db: &dyn TypeDatabase, item_id: InLibrary<item::ItemId>) -> TypeId {
    let library = db.library(item_id.0);
    let item = library.item(item_id.1);

    match &item.kind {
        item::ItemKind::ConstVar(item) => constvar_ty(db, item_id, item),
        item::ItemKind::Type(item) => type_def_ty(db, item_id, item),
        item::ItemKind::Binding(item) => bind_def_ty(db, item_id, item),
        item::ItemKind::Subprogram(item) => subprogram_item_ty(db, item_id, item),
        item::ItemKind::Module(_) => db.mk_error(), // Modules can't be used as types directly
    }
}

fn constvar_ty(
    db: &dyn TypeDatabase,
    item_id: InLibrary<item::ItemId>,
    item: &item::ConstVar,
) -> TypeId {
    // type_collect
    // extract type for declared identifiers
    // if both are present, then typecheck as assignment
    let item_ty = match (item.type_spec, item.init_expr) {
        (Some(ty_spec), _) => {
            // From type_spec
            db.from_hir_type(ty_spec.in_library(item_id.0)).in_db(db)
        }
        (_, Some(body)) => {
            // From inferred init expr
            db.type_of((item_id.0, body).into()).in_db(db)
        }
        (None, None) => {
            // No place to infer from, make an error
            db.mk_error().in_db(db)
        }
    };

    // Make the type concrete
    if *item_ty.kind() == TypeKind::Integer {
        // Integer decomposes into a normal `int`
        db.mk_int(IntSize::Int)
    } else {
        require_resolved_type(db, item_ty.id())
    }
}

fn type_def_ty(
    db: &dyn TypeDatabase,
    item_id: InLibrary<item::ItemId>,
    item: &item::Type,
) -> TypeId {
    let def_id = DefId(item_id.0, item.def_id);

    match &item.type_def {
        item::DefinedType::Alias(to_ty) => {
            // Peel any aliases that are encountered
            let base_ty = db
                .from_hir_type((*to_ty).in_library(item_id.0))
                .in_db(db)
                .peel_aliases();

            // Specialize based on the kind
            // TODO: Specialize type when it's a record or union
            match base_ty.kind() {
                // Forward base types get propagated as errors
                ty::TypeKind::Forward => db.mk_error(),
                _ => db.mk_alias(def_id, base_ty.id()),
            }
        }
        item::DefinedType::Forward(_) => db.mk_alias(def_id, db.mk_forward()),
    }
}

fn bind_def_ty(
    db: &dyn TypeDatabase,
    item_id: InLibrary<item::ItemId>,
    item: &item::Binding,
) -> TypeId {
    // Takes the type from what it's bound to
    let item_ty = db.type_of((item_id.0, item.bind_to).into());
    require_resolved_type(db, item_ty)
}

fn subprogram_item_ty(
    db: &dyn TypeDatabase,
    item_id: InLibrary<item::ItemId>,
    item: &item::Subprogram,
) -> TypeId {
    let library_id = item_id.0;
    let param_ty = subprogram_param_list(
        db,
        library_id,
        item.param_list.as_ref().map(|params| &params.tys),
    );
    let result_ty = require_resolved_hir_type(db, item.result.ty.in_library(library_id));

    db.mk_subprogram(item.kind, param_ty, result_ty)
}

pub(crate) fn ty_from_item_param(
    db: &dyn TypeDatabase,
    param_id: InLibrary<(item::ItemId, LocalDefId)>,
) -> TypeId {
    // Only from subprogram items
    let InLibrary(library_id, (item_id, param_def)) = param_id;
    let library = db.library(library_id);

    let item = match &library.item(item_id).kind {
        item::ItemKind::Subprogram(item) => item,
        _ => unreachable!(),
    };

    let hir_ty = match item.lookup_param_info(param_def) {
        item::ParameterInfo::Param(param_info) => {
            // From parameter
            param_info.param_ty
        }
        item::ParameterInfo::Result => {
            // From named result type
            item.result.ty
        }
    };

    require_resolved_hir_type(db, hir_ty.in_library(library_id))
}

pub(crate) fn ty_from_stmt(db: &dyn TypeDatabase, stmt_id: InLibrary<stmt::BodyStmt>) -> TypeId {
    let library_id = stmt_id.0;
    let stmt_id = stmt_id.1;

    let library = db.library(library_id);
    let stmt = library.body(stmt_id.0).stmt(stmt_id.1);

    match &stmt.kind {
        stmt::StmtKind::For(stmt) => for_counter_ty(db, library_id, stmt_id, stmt),
        _ => unreachable!("not a def owner"),
    }
}

fn for_counter_ty(
    db: &dyn TypeDatabase,
    library_id: LibraryId,
    stmt_id: stmt::BodyStmt,
    stmt: &stmt::For,
) -> TypeId {
    // infer the counter type from the range bounds
    match stmt.bounds {
        stmt::ForBounds::Implicit(_expr) => {
            // We don't support implicit bounds yet, so make an error
            // TODO: Do the proper thing once range types & type aliases are lowered
            db.mk_error()
        }
        stmt::ForBounds::Full(start, end) => {
            // Always infer from the start type
            // We can usually ignore the end type, except if start is not a concrete type
            let start_ty = db.type_of((library_id, stmt_id.0, start).into()).in_db(db);
            let end_ty = db.type_of((library_id, stmt_id.0, end).into()).in_db(db);

            // Pick the concrete type
            let counter_ty = if *start_ty.kind() != TypeKind::Integer {
                start_ty.id()
            } else if *end_ty.kind() != TypeKind::Integer {
                end_ty.id()
            } else {
                // Integer decomposes into a normal `int`
                db.mk_int(IntSize::Int)
            };

            counter_ty
        }
    }
}

pub(crate) fn ty_from_expr(
    db: &dyn TypeDatabase,
    body: InLibrary<&body::Body>,
    body_expr: expr::BodyExpr,
) -> TypeId {
    // FIXME: Move to using `type_of` instead of referring back to `ty_from_expr`
    let expr_id = body_expr.1;

    match &body.1.expr(expr_id).kind {
        expr::ExprKind::Missing => {
            // Missing, treat as error
            db.mk_error()
        }
        expr::ExprKind::Literal(expr) => literal_ty(db, expr),
        expr::ExprKind::Binary(expr) => binary_ty(db, body, expr, body_expr),
        expr::ExprKind::Unary(expr) => unary_ty(db, body, expr, body_expr),
        expr::ExprKind::Name(expr) => name_ty(db, body, expr),
        expr::ExprKind::Field(expr) => field_ty(db, body, expr, body_expr),
        expr::ExprKind::Call(expr) => call_expr_ty(db, body, expr, body_expr),
    }
}

fn literal_ty(db: &dyn TypeDatabase, expr: &expr::Literal) -> TypeId {
    match expr {
        expr::Literal::Integer(_) => db.mk_integer(),
        expr::Literal::Real(_) => db.mk_real(RealSize::Real),
        expr::Literal::Char(_) => db.mk_char(),
        expr::Literal::CharSeq(s) => {
            let size = s.len().try_into().unwrap_or(u64::MAX);
            let size = ConstInt::from_unsigned(size, false);
            let seq_size = ty::SeqSize::Fixed(size.into());
            db.mk_char_n(seq_size)
        }
        expr::Literal::String(_) => db.mk_string(),
        expr::Literal::Boolean(_) => db.mk_boolean(),
    }
}

fn binary_ty(
    db: &dyn TypeDatabase,
    body: InLibrary<&body::Body>,
    expr: &expr::Binary,
    body_expr: expr::BodyExpr,
) -> TypeId {
    let left = ty_from_expr(db, body, expr::BodyExpr(body_expr.0, expr.lhs));
    let right = ty_from_expr(db, body, expr::BodyExpr(body_expr.0, expr.rhs));

    // Inference doesn't care about type errors too much
    // Just use the provided inferred type
    ty::rules::infer_binary_op(db, left, *expr.op.item(), right).extract_ty()
}

fn unary_ty(
    db: &dyn TypeDatabase,
    body: InLibrary<&body::Body>,
    expr: &expr::Unary,
    body_expr: expr::BodyExpr,
) -> TypeId {
    let right = ty_from_expr(db, body, expr::BodyExpr(body_expr.0, expr.rhs));

    // Inference doesn't care about type errors too much
    // Just use the provided inferred type
    ty::rules::infer_unary_op(db, *expr.op.item(), right).extract_ty()
}

fn name_ty(db: &dyn TypeDatabase, body: InLibrary<&body::Body>, expr: &expr::Name) -> TypeId {
    // If def-id, fetch type from def id map
    // If self, then fetch type from provided class def id?
    match expr {
        expr::Name::Name(def_id) => {
            // FIXME: Perform name resolution
            let def_id = DefId(body.0, *def_id);

            // Defer to result type if it's a paren-less function
            let ty = db.type_of(def_id.into()).in_db(db);

            if let TypeKind::Subprogram(symbol::SubprogramKind::Function, None, result) = ty.kind()
            {
                *result
            } else {
                ty.id()
            }
        }
        expr::Name::Self_ => {
            todo!()
        }
    }
}

fn field_ty(
    db: &dyn TypeDatabase,
    body: InLibrary<&body::Body>,
    expr: &expr::Field,
    body_expr: expr::BodyExpr,
) -> TypeId {
    db.fields_of((body.0, body_expr.0, expr.lhs).into())
        .and_then(|fields| {
            fields.lookup(expr.field.item()).map(|field| {
                // FIXME: Handle opaque types
                db.type_of(field.def_id.into())
            })
        })
        .unwrap_or_else(|| db.mk_error())
}

fn call_expr_ty(
    db: &dyn TypeDatabase,
    body: InLibrary<&body::Body>,
    expr: &expr::Call,
    body_expr: expr::BodyExpr,
) -> TypeId {
    let left = ty_from_expr(db, body, expr::BodyExpr(body_expr.0, expr.lhs));
    let subprog_ty = left.in_db(db).to_base_type();

    if let TypeKind::Subprogram(
        symbol::SubprogramKind::Procedure | symbol::SubprogramKind::Function,
        _params,
        result,
    ) = subprog_ty.kind()
    {
        // Is void result? Error! (proc call in expr position)
        let result = result.in_db(db);

        if matches!(result.kind(), TypeKind::Void) {
            db.mk_error()
        } else {
            // It's okay to always infer as the result type, since that gives
            // better diagnostics
            result.id()
        }
    } else {
        // Not a callable subprogram
        db.mk_error()
    }
}

pub(super) fn ty_from_body_owner(
    db: &dyn TypeDatabase,
    library_id: LibraryId,
    body_owner: Option<body::BodyOwner>,
) -> TypeId {
    let body_owner = if let Some(body_owner) = body_owner {
        body_owner
    } else {
        unreachable!("all bodies should have owners")
    };

    match body_owner {
        body::BodyOwner::Item(item_id) => {
            // Take from the item
            let library = db.library(library_id);
            let item = library.item(item_id);

            match &item.kind {
                item::ItemKind::Subprogram(subprog) => {
                    // From result type
                    db.from_hir_type(subprog.result.ty.in_library(library_id))
                }
                item::ItemKind::Module(_) => {
                    // Modules are always procedure-like bodies
                    db.mk_void()
                }
                _ => db.mk_error(),
            }
        }
        body::BodyOwner::Type(_) => db.mk_error(),
    }
}

fn require_resolved_hir_type(db: &dyn TypeDatabase, ty: InLibrary<hir_ty::TypeId>) -> TypeId {
    require_resolved_type(db, db.from_hir_type(ty))
}

/// Requires that a type is resolved at this point, otherwise produces
/// a type error
fn require_resolved_type(db: &dyn TypeDatabase, ty: TypeId) -> TypeId {
    if ty.in_db(db).peel_aliases().kind().is_forward() {
        db.mk_error()
    } else {
        ty
    }
}
