//! Const eval query implementations

use std::{convert::TryInto, sync::Arc};

use toc_hir::{
    expr,
    symbol::{DefId, Mutability},
};

use crate::{
    const_eval::{
        errors::{ErrorKind, NotConst},
        ops::ConstOp,
        ConstError, ConstInt,
    },
    ty,
};

use super::{db::ConstEval, Const, ConstResult, ConstValue, EvalParams};

pub(crate) fn evaluate_const(
    db: &dyn ConstEval,
    expr: Const,
    params: EvalParams,
) -> ConstResult<ConstValue> {
    #[derive(Debug)]
    enum Eval {
        Expr(expr::ExprId),
        Op(ConstOp, toc_span::Span),
    }

    let (package_id, body_id) = match expr {
        Const::Value(v) => return Ok(v),
        Const::Error(err) => return Err(err),
        Const::Unevaluated(package, body) => (package, body),
        Const::UnevaluatedExpr(package, expr) => (package, expr.0),
    };

    let package = db.package(package_id);
    let body = package.body(body_id);
    let span_map = &package;

    let root_expr = match expr {
        Const::Unevaluated(_, _) => match &body.kind {
            toc_hir::body::BodyKind::Stmts(..) => {
                return Err(ConstError::new(
                    ErrorKind::NotConstExpr(NotConst::Expr),
                    body.span.lookup_in(span_map),
                ))
            }
            toc_hir::body::BodyKind::Exprs(expr) => *expr,
        },
        Const::UnevaluatedExpr(_, expr) => expr.1,
        _ => unreachable!(),
    };
    let in_module = db.inside_module((package_id, body_id, root_expr).into());

    // Do the actual evaluation, as a stack machine
    let mut eval_stack = vec![Eval::Expr(root_expr)];
    let mut operand_stack = vec![];

    loop {
        let local_expr = match eval_stack.pop() {
            Some(Eval::Expr(expr)) => expr,
            Some(Eval::Op(op, span)) => {
                // Perform operation
                let result = op
                    .evaluate(&mut operand_stack, params.allow_64bit_ops)
                    .map_err(|err| err.change_span(span))?;
                operand_stack.push(result);
                continue;
            }
            None => break,
        };

        let expr = body.expr(local_expr);
        let expr_span = expr.span.lookup_in(span_map);

        // Regarding enum field accesses:
        // We have access to the typing context now, so it should be easy to support enums & the like
        match &expr.kind {
            expr::ExprKind::Missing => {
                // Bail out
                return Err(ConstError::new(ErrorKind::MissingExpr, expr_span));
            }
            expr::ExprKind::Literal(expr) => {
                // ???: How to deal with 32-bit vs 64-bit integers?
                // - Could yoink info from somewhere?
                // - Only need to know if ops need to be done in either 32 or 64 bit mode

                // Convert into a ConstValue
                let operand = match expr {
                    expr::Literal::Integer(v) => {
                        let v = ConstInt::from_unsigned(*v, params.allow_64bit_ops)
                            .map_err(|err| err.change_span(expr_span))?;
                        ConstValue::Integer(v)
                    }
                    expr::Literal::Real(v) => ConstValue::Real(*v),
                    expr::Literal::Boolean(v) => ConstValue::Bool(*v),
                    expr::Literal::Char(v) => ConstValue::Char(*v),
                    expr::Literal::CharSeq(v) => ConstValue::CharN(Arc::new(v.clone())),
                    expr::Literal::String(v) => ConstValue::String(Arc::new(v.clone())),
                };

                operand_stack.push(operand);
            }
            expr::ExprKind::Binary(expr) => {
                // Push both expression operands and the operation
                let (op, span) = (expr.op.try_into()?, expr.op.span());
                eval_stack.push(Eval::Op(op, span.lookup_in(span_map)));
                eval_stack.push(Eval::Expr(expr.rhs));
                eval_stack.push(Eval::Expr(expr.lhs));
            }
            expr::ExprKind::Unary(expr) => {
                // Push expr operand & operator
                let (op, span) = (expr.op.try_into()?, expr.op.span());
                eval_stack.push(Eval::Op(op, span.lookup_in(span_map)));
                eval_stack.push(Eval::Expr(expr.rhs));
            }
            expr::ExprKind::Name(name) => {
                // May or may not reference a constant expression
                match name {
                    expr::Name::Name(binding) => {
                        // Resolve to canonical def first so that we aren't looking at any exports
                        let canonical_def = match package.binding_resolve(*binding) {
                            toc_hir::symbol::Resolve::Def(local_def) => {
                                db.resolve_def(DefId(package_id, local_def)).ok()
                            }
                            toc_hir::symbol::Resolve::Err => None,
                        }
                        .ok_or_else(|| {
                            // Not a const expr
                            ConstError::new(
                                ErrorKind::NotConstExpr(NotConst::Binding(package_id, *binding)),
                                expr_span,
                            )
                        })?;

                        let package_id = canonical_def.package();
                        let package = db.package(package_id);

                        let body = db.item_of(canonical_def).and_then(|item| {
                            match &package.item(item.1).kind {
                                toc_hir::item::ItemKind::ConstVar(cv)
                                    if matches!(cv.mutability, Mutability::Const) =>
                                {
                                    cv.init_expr
                                }
                                _ => None,
                            }
                        });

                        let body = match body {
                            Some(body) => body,
                            None => {
                                // Not a const expr
                                return Err(ConstError::new(
                                    ErrorKind::NotConstExpr(NotConst::Def(canonical_def)),
                                    expr_span,
                                ));
                            }
                        };

                        let value =
                            db.evaluate_const(Const::from_body(package_id, body), params)?;

                        // Check that the produced value matches the real type of the def
                        let left_ty = db.type_of(canonical_def.into());
                        let right_ty = db.type_of((package_id, body).into());

                        // FIXME: add tests with opaque tys once module exports are in const eval
                        let left_ty = left_ty
                            .peel_opaque(db.up(), in_module)
                            .peel_aliases(db.up());
                        let right_ty = right_ty
                            .peel_opaque(db.up(), in_module)
                            .peel_aliases(db.up());

                        if !ty::rules::is_assignable(db, left_ty, right_ty) {
                            // Wrong types
                            let span = package.body(body).span.lookup_in(&package);
                            return Err(ConstError::new(ErrorKind::WrongResultType, span));
                        }

                        // If bounds of `left` is known, check if `right` is in the range
                        if let Some((min, max)) =
                            Option::zip(left_ty.min_int_of(db).ok(), left_ty.max_int_of(db).ok())
                        {
                            // Since right is assignable into left, we can treat right as ConstInt
                            let as_ordinal = value.ordinal().ok_or_else(|| {
                                // Definitely the wrong type
                                let span = package.body(body).span.lookup_in(&package);
                                ConstError::new(ErrorKind::WrongResultType, span)
                            })?;

                            if !(min..=max).contains(&as_ordinal) {
                                // Is outside of value range
                                let span = package.body(body).span.lookup_in(&package);
                                return Err(ConstError::new(ErrorKind::OutsideRange, span));
                            }
                        }

                        // Cast into the canonical value
                        let value = match left_ty.to_base_type(db.up()).kind(db.up()) {
                            ty::TypeKind::Char => value.cast_into_char().map_or_else(
                                |err| {
                                    // Definitely the wrong type
                                    let span = package.body(body).span.lookup_in(&package);
                                    Err(err.change_span(span))
                                },
                                |v| Ok(ConstValue::Char(v)),
                            )?,
                            _ => value,
                        };

                        // Push (cached) value to the operand stack
                        operand_stack.push(value);
                    }
                    expr::Name::Self_ => {
                        // Never a const expr
                        // TODO: Use the self's associated def_id
                        return Err(ConstError::new(
                            ErrorKind::NotConstExpr(NotConst::Expr),
                            expr_span,
                        ));
                    }
                }
            }
            expr::ExprKind::Field(expr) => {
                // Possible things:
                // - module export (constvar reference)
                // - enum variants (enum variant value)
                let lhs_expr = (package_id, body_id, expr.lhs);
                let lhs_tyref = db
                    .type_of(lhs_expr.into())
                    .peel_opaque(db.up(), in_module)
                    .peel_aliases(db.up());

                match lhs_tyref.kind(db.up()) {
                    ty::TypeKind::Enum(_, variants) => {
                        // Enum variants
                        let Some(package_id) = variants.first().map(|def| def.package()) else {
                            return Err(ConstError::new(
                                ErrorKind::NoFields(*expr.field.item()),
                                expr.field.span().lookup_in(&package),
                            ));
                        };
                        let package = db.package(package_id);

                        // Get variant ordinal
                        let ordinal = variants
                            .iter()
                            .enumerate()
                            .find_map(|(idx, def_id)| {
                                (package.local_def(def_id.1).name == *expr.field.item())
                                    .then_some(idx)
                            })
                            .ok_or_else(|| {
                                ConstError::new(
                                    ErrorKind::NoFields(*expr.field.item()),
                                    expr.field.span().lookup_in(&package),
                                )
                            })?;

                        operand_stack.push(ConstValue::EnumVariant(lhs_tyref, ordinal));
                    }
                    _ => {
                        // Defer to normal field lookup
                        // FIXME: Handle const-eval field lookups for modules
                        // Note: This will expose us to evaluation cycles, so those need to be handled as well
                        // when the time comes
                        return Err(ConstError::new(
                            ErrorKind::NotConstExpr(NotConst::Expr),
                            expr_span,
                        ));
                    }
                }
            }
            expr::ExprKind::Call(_) => {
                // There are some functions which are allowed to be const-fns, but they're all builtins
                // It's okay to treat it as not const-evaluable
                return Err(ConstError::new(
                    ErrorKind::NotConstExpr(NotConst::Expr),
                    expr_span,
                ));
            }
            _ => {
                // The rest of the exprs aren't ever evaluable at compile time
                return Err(ConstError::new(
                    ErrorKind::NotConstExpr(NotConst::Expr),
                    expr_span,
                ));
            }
        }
    }

    // Should be one value remaining
    let result = operand_stack
        .pop()
        .expect("All values popped off of operand stack");
    assert!(operand_stack.is_empty());

    Ok(result)
}
