//! Lowering HIR entities into anaylsis types

use std::convert::TryInto;

use toc_hir::library::{LibraryId, WrapInLibrary};
use toc_hir::{body, expr, stmt};
use toc_hir::{item, library::InLibrary, symbol::DefId, ty as hir_ty};

use crate::const_eval::{Const, ConstInt};
use crate::db::TypeDatabase;
use crate::ty::{self, TypeId, TypeKind};

use super::{IntSize, Mutability, NatSize, RealSize, SeqSize};

pub(crate) fn ty_from_hir_ty(db: &dyn TypeDatabase, hir_id: InLibrary<hir_ty::TypeId>) -> TypeId {
    let library = db.library(hir_id.0);
    let hir_ty = library.lookup_type(hir_id.1);

    match &hir_ty.kind {
        hir_ty::TypeKind::Missing => db.mk_error(),
        hir_ty::TypeKind::Primitive(ty) => primitive_ty(db, hir_id, ty),
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
        hir_ty::SeqLength::Dynamic => SeqSize::Dynamic,
        hir_ty::SeqLength::Expr(body) => SeqSize::Fixed(Const::from_body(library, body)),
    }
}

pub(crate) fn ty_from_item(db: &dyn TypeDatabase, item_id: InLibrary<item::ItemId>) -> TypeId {
    let library = db.library(item_id.0);
    let item = library.item(item_id.1);

    match &item.kind {
        item::ItemKind::ConstVar(item) => constvar_ty(db, item_id, item),
        item::ItemKind::Module(_) => db.mk_error(), // TODO: lower module items into tys
    }
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

fn constvar_ty(
    db: &dyn TypeDatabase,
    item_id: InLibrary<item::ItemId>,
    item: &item::ConstVar,
) -> TypeId {
    // type_collect
    // extract type for declared identifiers
    // if both are present, then typecheck as assignment
    let item_ty = match item.tail {
        item::ConstVarTail::Both(ty_spec, _) | item::ConstVarTail::TypeSpec(ty_spec) => {
            // From type_spec
            db.from_hir_type(ty_spec.in_library(item_id.0)).in_db(db)
        }
        item::ConstVarTail::InitExpr(expr) => {
            // From inferred init expr
            // Peel any refs
            db.type_of((item_id.0, expr).into()).in_db(db).peel_ref()
        }
    };

    // Make the type concrete
    let item_ty = if *item_ty.kind() == TypeKind::Integer {
        // Integer decomposes into a normal `int`
        db.mk_int(IntSize::Int)
    } else {
        item_ty.id()
    };

    // Use the appropriate reference mutability
    let mutability = match item.mutability {
        item::Mutability::Const => Mutability::Const,
        item::Mutability::Var => Mutability::Var,
    };

    db.mk_ref(mutability, item_ty)
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
            let start_ty = db
                .type_of((library_id, stmt_id.0, start).into())
                .in_db(db)
                .peel_ref();
            let end_ty = db
                .type_of((library_id, stmt_id.0, end).into())
                .in_db(db)
                .peel_ref();

            // Pick the concrete type
            let counter_ty = if *start_ty.kind() != TypeKind::Integer {
                start_ty.id()
            } else if *end_ty.kind() != TypeKind::Integer {
                end_ty.id()
            } else {
                // Integer decomposes into a normal `int`
                db.mk_int(IntSize::Int)
            };

            db.mk_ref(Mutability::Const, counter_ty)
        }
    }
}

pub(crate) fn ty_from_expr(
    db: &dyn TypeDatabase,
    body: InLibrary<&body::Body>,
    expr: expr::ExprId,
) -> TypeId {
    match &body.1.expr(expr).kind {
        expr::ExprKind::Missing => {
            // Missing, treat as error
            db.mk_error()
        }
        expr::ExprKind::Literal(expr) => literal_ty(db, body, expr),
        expr::ExprKind::Binary(expr) => binary_ty(db, body, expr),
        expr::ExprKind::Unary(expr) => unary_ty(db, body, expr),
        expr::ExprKind::Name(expr) => name_ty(db, body, expr),
    }
}

fn literal_ty(
    db: &dyn TypeDatabase,
    _body: InLibrary<&body::Body>,
    expr: &expr::Literal,
) -> TypeId {
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

// was doing: figure out how to actually lower it
// - see ty::rules::check_{bin,unary}_op for details
fn binary_ty(db: &dyn TypeDatabase, body: InLibrary<&body::Body>, expr: &expr::Binary) -> TypeId {
    // TODO: do full binexpr typechecks
    let left = ty_from_expr(db, body, expr.lhs);
    let right = ty_from_expr(db, body, expr.rhs);

    ty::rules::check_binary_op(db, left, *expr.op.item(), right).unwrap_or_else(|_| db.mk_error())
}

fn unary_ty(db: &dyn TypeDatabase, body: InLibrary<&body::Body>, expr: &expr::Unary) -> TypeId {
    let right = ty_from_expr(db, body, expr.rhs);

    ty::rules::check_unary_op(db, *expr.op.item(), right).unwrap_or_else(|_| db.mk_error())
}

fn name_ty(db: &dyn TypeDatabase, body: InLibrary<&body::Body>, expr: &expr::Name) -> TypeId {
    // If def-id, fetch type from def id map
    // If self, then fetch type from provided class def id?
    match expr {
        expr::Name::Name(def_id) => {
            let def_id = DefId(body.0, *def_id);
            // TODO: Perform name resolution
            db.type_of(def_id.into())
        }
        expr::Name::Self_ => {
            todo!()
        }
    }
}
