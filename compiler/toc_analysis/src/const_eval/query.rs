//! Const eval query implementations

use std::convert::TryInto;

use toc_hir::{expr, symbol::DefId, item::Mutability};

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
    };

    let library = db.library(library_id);
    let body = library.body(body_id);
    let span_map = &library.span_map;

    let root_expr = match &body.kind {
        toc_hir::body::BodyKind::Stmts(_, _) => {
            return Err(ConstError::new(
                ErrorKind::NotCompileEvaluable,
                body.span.lookup_in(span_map),
            ))
        }
        toc_hir::body::BodyKind::Exprs(expr) => *expr,
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
                    expr::Literal::Char(_)
                    | expr::Literal::CharSeq(_)
                    | expr::Literal::String(_) => {
                        // Unsupported const value
                        return Err(ConstError::new(ErrorKind::UnsupportedValue, expr_span));
                    }
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
            expr::ExprKind::Paren(expr) => {
                // Push the inner expr
                eval_stack.push(Eval::Expr(expr.expr));
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

                        let body = library.item_of(def_id).and_then(|item| {
                            match &library.item(item).kind {
                                toc_hir::item::ItemKind::ConstVar(cv) if matches!(cv.mutability, Mutability::Const) => cv.tail.init_expr(),
                                _ => None,
                            }
                        });

                        let body = match body {
                            Some(body) => body,
                            None => {
                                // Not a const expr
                                let def_span = library
                                    .local_def(def_id)
                                    .name
                                    .span()
                                    .lookup_in(&library.span_map);

                                return Err(ConstError::new(
                                    ErrorKind::NoConstExpr(def_span),
                                    expr_span,
                                ));
                            }
                        };

                        let value =
                            db.evaluate_const(Const::from_body(library_id, body), params)?;

                        // Check that the produced value matches the real type of the def
                        let left = db.type_of(DefId(library_id, def_id).into());
                        let right = db.type_of((library_id, body).into());

                        if !ty::rules::is_assignable(db, left, right, true)
                            .expect("ignores mutability")
                        {
                            // Wrong types
                            let span = library.body(body).span.lookup_in(&library.span_map);
                            return Err(ConstError::new(ErrorKind::WrongResultType, span));
                        }

                        // TODO: If `left` is a range type, check if `right` is in the range

                        // Push (cached) value to the operand stack
                        operand_stack.push(value);
                    }
                    expr::Name::Self_ => {
                        // Never a const expr
                        // TODO: Use the self's associated def_id
                        return Err(ConstError::new(
                            ErrorKind::NoConstExpr(Default::default()),
                            expr_span,
                        ));
                    }
                }
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
