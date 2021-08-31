//! Lowering HIR entities into anaylsis types

use toc_hir::library::WrapInLibrary;
use toc_hir::{body, expr};
use toc_hir::{item, library::InLibrary, symbol::DefId, ty as hir_ty};

use crate::db::TypeDatabase;
use crate::ty::{self, Type, TypeId, TypeKind};

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
    _hir_id: InLibrary<hir_ty::TypeId>,
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
        hir_ty::Primitive::SizedChar(len) => db.mk_char_n(lower_seq_len(*len)),
        hir_ty::Primitive::SizedString(len) => db.mk_string_n(lower_seq_len(*len)),
        // TODO: Migrate this code to the appropriate place
        // We don't eagerly create constants from bodies
        /*hir_ty::Primitive::SizedChar(len) => {
            // Note: 32768 is the minimum defined limit for the length on `n` for char(N)
            // ???: Do we want to add a config/feature option to change this?
            let size_limit = 32768;

            match self.lower_seq_len(*len, size_limit) {
                Ok(len) => ty::Type::CharN(len),
                Err(err) => {
                    err.report_to(&mut self.state().reporter);
                    ty::Type::Error
                }
            }
        }
        hir_ty::Primitive::SizedString(len) => {
            // 256 is the maximum defined limit for the length on `n` for string(N),
            // so no option of changing that (unless we have control over the interpreter code).
            // - Legacy interpreter has the assumption baked in that the max length of a string is 256,
            //   so we can't change it yet unless we use a new interpreter.
            let size_limit = 256;

            match self.lower_seq_len(*len, size_limit) {
                Ok(len) => ty::Type::StringN(len),
                Err(err) => {
                    err.report_to(&mut self.state().reporter);
                    ty::Type::Error
                }
            }
        }*/
    }
}

fn lower_seq_len(seq_len: hir_ty::SeqLength) -> SeqSize {
    match seq_len {
        hir_ty::SeqLength::Dynamic => SeqSize::Dynamic,
        hir_ty::SeqLength::Expr(body) => SeqSize::Fixed(body),
    }
}

/*
fn lower_seq_len(
    &self,
    seq_len: toc_hir::ty::SeqLength,
    size_limit: u32,
) -> Result<ty::SeqSize, SeqLenError> {
    let expr = match seq_len {
        hir_ty::SeqLength::Dynamic => return Ok(ty::SeqSize::Dynamic),
        hir_ty::SeqLength::Expr(expr) => expr,
    };

    // TODO: Migrate this code to the appropriate place

    // Never allow 64-bit ops (size is always less than 2^32)
    // Restrict to no type since we handle it here too
    let const_expr = self
        .const_eval
        .defer_expr(self.unit.id, expr, false, RestrictType::None);

    // Always eagerly evaluate the expr
    let value = self
        .const_eval
        .eval_expr(const_expr)
        .map_err(SeqLenError::ConstEval)?;

    let span = self.db.get_span(expr.into());

    // Check that the value is actually the correct type, and in the correct value range.
    // Size can only be in (0, 32768)
    let int = value.into_int(span).map_err(SeqLenError::ConstEval)?;

    // Convert into a size, within the given limit
    let size = int
        .into_u32()
        .and_then(NonZeroU32::new)
        .filter(|size| size.get() < size_limit)
        .ok_or_else(|| SeqLenError::WrongSize(Spanned::new(int, span), size_limit))?;

    Ok(ty::SeqSize::Fixed(size))
}
*/
pub(crate) fn ty_from_item(db: &dyn TypeDatabase, item_id: InLibrary<item::ItemId>) -> TypeId {
    let library = db.library(item_id.0);
    let item = library.item(item_id.1);

    match &item.kind {
        item::ItemKind::ConstVar(item) => constvar_ty(db, item_id, item),
        item::ItemKind::Module(_) => todo!("we don't lower module decls yet"),
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
    let item_ty = match &item.tail {
        item::ConstVarTail::Both(ty_spec, _) | item::ConstVarTail::TypeSpec(ty_spec) => {
            // From type_spec
            db.from_hir_type(ty_spec.in_library(item_id.0))
        }
        item::ConstVarTail::InitExpr(expr) => {
            // From inferred init expr
            db.type_of((item_id.0, *expr).into())
        }
    };

    // Make the type concrete
    let ty_ref = if item_ty.lookup(db).kind() == &TypeKind::Integer {
        // Integer decomposes into a normal `int`
        db.intern_type(
            Type {
                kind: TypeKind::Int(IntSize::Int),
            }
            .into(),
        )
    } else {
        item_ty
    };

    // Use the appropriate reference mutability
    let mutability = match item.mutability {
        item::Mutability::Const => Mutability::Const,
        item::Mutability::Var => Mutability::Var,
    };

    db.intern_type(
        Type {
            kind: TypeKind::Ref(mutability, ty_ref),
        }
        .into(),
    )
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
        expr::ExprKind::Paren(expr) => paren_ty(db, body, expr),
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
        expr::Literal::CharSeq(_s) => {
            // TODO: Shove in eagerly evaluated constant
            todo!()
            //let size = NonZeroU32::new(s.len().try_into().unwrap_or(u32::MAX)).unwrap();
            //let seq_size = ty::SeqSize::Fixed(size);
            //ty::Type::CharN(seq_size)
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

fn paren_ty(db: &dyn TypeDatabase, body: InLibrary<&body::Body>, expr: &expr::Paren) -> TypeId {
    // Same eval kind as the inner
    ty_from_expr(db, body, expr.expr)
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
