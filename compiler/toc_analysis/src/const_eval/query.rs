//! Const eval query implementations

use std::{convert::TryInto, sync::Arc};

use toc_hir::{
    expr,
    symbol::{DefId, Mutability},
};

use crate::{
    const_eval::{errors::ErrorKind, ops::ConstOp, ConstError, ConstInt},
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

    let (library_id, body_id) = match expr {
        Const::Value(v) => return Ok(v),
        Const::Error(err) => return Err(err),
        Const::Unevaluated(library, body) => (library, body),
        Const::UnevaluatedExpr(library, expr) => (library, expr.0),
    };

    let library = db.library(library_id);
    let body = library.body(body_id);
    let span_map = &library.span_map;

    let root_expr = match expr {
        Const::Unevaluated(_, _) => match &body.kind {
            toc_hir::body::BodyKind::Stmts(..) => {
                return Err(ConstError::new(
                    ErrorKind::NotConstExpr(None),
                    body.span.lookup_in(span_map),
                ))
            }
            toc_hir::body::BodyKind::Exprs(expr) => *expr,
        },
        Const::UnevaluatedExpr(_, expr) => expr.1,
        _ => unreachable!(),
    };

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
                    expr::Name::Name(def_id) => {
                        // TODO: Resolve to canonical definition
                        // Would need to alter what library id is used
                        let def_id = *def_id;
                        let library_id = library_id;
                        let library = db.library(library_id);

                        let body = db.item_of(DefId(library_id, def_id)).and_then(|item| {
                            match &library.item(item.1).kind {
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
                                    ErrorKind::NotConstExpr(Some(DefId(library_id, def_id))),
                                    expr_span,
                                ));
                            }
                        };

                        let value =
                            db.evaluate_const(Const::from_body(library_id, body), params)?;

                        // Check that the produced value matches the real type of the def
                        let left = db.type_of(DefId(library_id, def_id).into());
                        let right = db.type_of((library_id, body).into());

                        if !ty::rules::is_assignable(db, left, right) {
                            // Wrong types
                            let span = library.body(body).span.lookup_in(&library.span_map);
                            return Err(ConstError::new(ErrorKind::WrongResultType, span));
                        }

                        // If bounds of `left` is known, check if `right` is in the range
                        let left_ty = left.in_db(db);

                        if let Some((min, max)) = left_ty.min_int_of().zip(left_ty.max_int_of()) {
                            // Since right is assignable into left, we can treat right as ConstInt
                            let as_ordinal = match &value {
                                ConstValue::Integer(val) => *val,
                                ConstValue::Bool(val) => {
                                    if *val {
                                        ConstInt::from_unsigned(0, false)
                                            .expect("const construction")
                                    } else {
                                        ConstInt::from_unsigned(1, false)
                                            .expect("const construction")
                                    }
                                }
                                ConstValue::Char(val) => {
                                    ConstInt::from_unsigned(u64::from(*val), false)
                                        .expect("const construction")
                                }
                                _ => unreachable!(),
                            };

                            if !(min..=max).contains(&as_ordinal) {
                                // Is outside of value range
                                let span = library.body(body).span.lookup_in(&library.span_map);
                                return Err(ConstError::new(ErrorKind::OutsideRange, span));
                            }
                        }

                        // Push (cached) value to the operand stack
                        operand_stack.push(value);
                    }
                    expr::Name::Self_ => {
                        // Never a const expr
                        // TODO: Use the self's associated def_id
                        return Err(ConstError::new(
                            ErrorKind::NotConstExpr(Default::default()),
                            expr_span,
                        ));
                    }
                }
            }
            expr::ExprKind::Call(_) => {
                // There are some functions which are allowed to be const-fns, but they're all builtins
                // It's okay to treat it as not const-evaluable
                return Err(ConstError::new(ErrorKind::NotConstExpr(None), expr_span));
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
