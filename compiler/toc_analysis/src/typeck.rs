//! Type checking
#[cfg(test)]
mod test;

use std::collections::HashMap;
use std::num::NonZeroU32;
use std::sync::Arc;

use toc_hir::{expr, stmt, ty as hir_ty};
use toc_reporting::{MessageKind, MessageSink, ReportMessage};
use toc_span::Spanned;

use crate::const_eval::{ConstError, ConstEvalCtx, ConstInt, ConstValue, RestrictType};
use crate::ty::{self, DefKind, TyCtx, TyRef};

// ???: Can we build up a type ctx without doing type propogation?
// Type propogation is inferring of types from inputs
// E.g. this stmt involves some type propogation
// ```ignore
// const k := 3
// ```
// Type of DefId(0) is now `int` (after reifying `Integer`)

// ???: What is the purpose of a type ctx?
// TypeCtx primarily holds mappings from TypeId's to the underlying raw types
// ???: Does it map DefId to TypeId as well?
// The only place that DefId's types are required is when performing const eval
// Type building also needs to be aware of external DefId's, and the export attributes (e.g. if it's `opaque`)
// ???: What about mutability of exports?
// - Mutability in general falls under responsibility of assignability
// - Export mutability matters for mutation outside of the local unit scope, normal const/var rules apply for local units

pub fn typecheck_unit(
    unit: &toc_hir::Unit,
    const_eval: Arc<ConstEvalCtx>,
) -> (TyCtx, Vec<ReportMessage>) {
    TypeCheck::check_unit(unit, const_eval)
}

struct TypeCheck<'a> {
    unit: &'a toc_hir::Unit,
    ty_ctx: TyCtx,
    eval_kinds: HashMap<expr::ExprIdx, EvalKind>,
    const_eval: Arc<ConstEvalCtx>,
    reporter: toc_reporting::MessageSink,
}

impl<'a> TypeCheck<'a> {
    fn check_unit(
        unit: &'a toc_hir::Unit,
        const_eval: Arc<ConstEvalCtx>,
    ) -> (TyCtx, Vec<ReportMessage>) {
        let mut typeck = Self {
            unit,
            ty_ctx: TyCtx::new(),
            eval_kinds: HashMap::new(),
            const_eval,
            reporter: toc_reporting::MessageSink::new(),
        };

        // Walk tree
        unit.walk_nodes(&mut typeck);

        let Self {
            ty_ctx, reporter, ..
        } = typeck;

        (ty_ctx, reporter.finish())
    }

    fn require_expr_ty(&mut self, eval_kind: EvalKind) -> TyRef {
        if let Some(ty) = eval_kind.as_expr_ty() {
            ty
        } else {
            // TODO: Report this error once type decls are lowered
            self.ty_ctx.add_type(ty::Type::Error)
        }
    }

    fn lookup_eval_kind(&self, expr: expr::ExprIdx) -> (&expr::Expr, EvalKind) {
        let eval_kind = self
            .eval_kinds
            .get(&expr)
            .expect("Expr is missing eval kind");
        let expr = &self.unit.database[expr];
        (expr, *eval_kind)
    }

    fn typeck_constvar(&mut self, decl: &stmt::ConstVar) {
        let ty_ref = match &decl.tail {
            stmt::ConstVarTail::Both(ty_spec, _) | stmt::ConstVarTail::TypeSpec(ty_spec) => {
                // From type_spec
                self.ty_ctx.get_type(*ty_spec).unwrap()
            }
            stmt::ConstVarTail::InitExpr(expr) => {
                // From inferred init expr
                let eval_kind = *self.eval_kinds.get(expr).unwrap();
                self.require_expr_ty(eval_kind)
            }
        };

        if let stmt::ConstVarTail::Both(ty_spec, init_expr) = &decl.tail {
            let lvalue_ty = self.ty_ctx.get_type(*ty_spec).unwrap();

            let rvalue_eval = *self.eval_kinds.get(init_expr).unwrap();
            let rvalue_ty = self.require_expr_ty(rvalue_eval);

            if let Some(false) = ty::rules::is_ty_assignable_to(lvalue_ty, rvalue_ty) {
                // TODO: invalidate associated `ConstExpr` in the event of an incompatible type

                // Incompatible, report it
                let init_span = self.unit.database.expr_nodes.spans[init_expr];
                let spec_span = self.unit.database.type_nodes.spans[ty_spec];

                self.reporter
                    .report_detailed(MessageKind::Error, "mismatched types", init_span)
                    .with_note(
                        "initializer's type is incompatible with this type",
                        spec_span,
                    )
                    .finish();
            }
        }

        // Make the type concrete
        let ty_ref = if *ty_ref == ty::Type::Integer {
            // Integer decomposes into a normal `int`
            self.ty_ctx.add_type(ty::Type::Int(ty::IntSize::Int))
        } else {
            ty_ref
        };

        // Wrap type in the appropriate def kind
        let def_kind = if decl.is_const {
            DefKind::Const(ty_ref)
        } else {
            DefKind::Var(ty_ref)
        };

        for def in &decl.names {
            self.ty_ctx.map_def_id(*def, def_kind);
        }
    }

    fn typeck_assign(&mut self, stmt: &stmt::Assign) {
        let (_l_value, l_value_eval) = self.lookup_eval_kind(stmt.lhs);
        let (_r_value, r_value_eval) = self.lookup_eval_kind(stmt.rhs);

        // Check if we can even assign into the lvalue (i.e. is lhs mutable)
        let l_value_ty = if let Some(ty) = l_value_eval.as_mut_ref_ty() {
            ty
        } else {
            let l_value_span = self.unit.database.expr_nodes.spans[&stmt.lhs];

            // TODO: Stringify lhs for more clarity on the error location
            self.reporter
                .report_detailed(
                    MessageKind::Error,
                    "cannot assign into expression on left hand side",
                    stmt.op.span(),
                )
                .with_note(
                    "this expression cannot be used as a variable reference",
                    l_value_span,
                )
                .finish();

            // Bail out, nothing else to do anyways
            return;
        };
        let r_value_ty = {
            let r_value_ty = self.require_expr_ty(r_value_eval);

            if let Some(bin_op) = stmt.op.item().as_binary_op() {
                // Typecheck binary operands
                let new_ty = self.type_check_binary_op(
                    stmt.lhs,
                    Spanned::new(bin_op, stmt.op.span()),
                    stmt.rhs,
                );
                self.ty_ctx.add_type(new_ty)
            } else {
                r_value_ty
            }
        };

        // Check if types are assignable
        // Leave error types as "always assignable"
        let asn_able = ty::rules::is_ty_assignable_to(l_value_ty, r_value_ty);
        if !asn_able.unwrap_or(true) {
            // TODO: Report expected type vs found type
            // - Requires type stringification/display impl
            self.reporter
                .report(MessageKind::Error, "mismatched types", stmt.op.span());
        }
    }

    fn typeck_literal(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Literal) {
        let ty = match expr {
            toc_hir::expr::Literal::Integer(_) => ty::Type::Integer,
            toc_hir::expr::Literal::Real(_) => ty::Type::Real(ty::RealSize::Real),
            // TODO: Use sized char type for default string type
            toc_hir::expr::Literal::CharSeq(_) => ty::Type::String,
            toc_hir::expr::Literal::String(_) => ty::Type::String,
            toc_hir::expr::Literal::Boolean(_) => ty::Type::Boolean,
        };

        // Post the type of the literal to eval type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn typeck_binary(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Binary) {
        // TODO: do full binexpr typechecks
        let ty = self.type_check_binary_op(expr.lhs, expr.op, expr.rhs);

        // Post binexpr type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn typeck_unary(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Unary) {
        let ty = self.type_check_unary_op(expr.op, expr.rhs);

        // Post unexpr type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn typeck_paren(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Paren) {
        // Same eval kind as the inner
        let eval_kind = self.eval_kinds[&expr.expr];
        self.eval_kinds.insert(id, eval_kind);
    }

    fn typeck_name(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Name) {
        // If def-id, fetch type from def id map
        // If self, then fetch type from provided class def id?
        let (use_id, ty_ref) = match expr {
            toc_hir::expr::Name::Name(use_id) => {
                (use_id, self.ty_ctx.get_def_id_kind(use_id.as_def()))
            }
            toc_hir::expr::Name::Self_ => todo!(),
        };

        let eval_ty = if let Some(ty_ref) = ty_ref {
            ty_ref
        } else {
            // Nab symbol info
            let sym_name = &self.unit.symbol_table.get_symbol(use_id.as_def()).name;
            let expr_range = self.unit.database.expr_nodes.spans[&id];

            // Not declared, no type provided by any decls
            self.reporter.report(
                MessageKind::Error,
                &format!("`{}` is not declared", sym_name),
                expr_range,
            );

            // Build error type
            let err_ref = self.ty_ctx.add_type(ty::Type::Error);
            let def_kind = DefKind::Error(err_ref);
            self.ty_ctx.map_def_id(use_id.as_def(), def_kind);

            def_kind
        };

        // Evalutes to a reference
        self.eval_kinds.insert(id, EvalKind::Ref(eval_ty));
    }

    fn typeck_primitive(&mut self, id: hir_ty::TypeIdx, ty: &hir_ty::Primitive) {
        // Create the correct type based off of the base primitive type
        let ty = match ty {
            hir_ty::Primitive::Int => ty::Type::Int(ty::IntSize::Int),
            hir_ty::Primitive::Int1 => ty::Type::Int(ty::IntSize::Int1),
            hir_ty::Primitive::Int2 => ty::Type::Int(ty::IntSize::Int2),
            hir_ty::Primitive::Int4 => ty::Type::Int(ty::IntSize::Int4),
            hir_ty::Primitive::Nat => ty::Type::Nat(ty::NatSize::Nat),
            hir_ty::Primitive::Nat1 => ty::Type::Nat(ty::NatSize::Nat1),
            hir_ty::Primitive::Nat2 => ty::Type::Nat(ty::NatSize::Nat2),
            hir_ty::Primitive::Nat4 => ty::Type::Nat(ty::NatSize::Nat4),
            hir_ty::Primitive::Real => ty::Type::Real(ty::RealSize::Real),
            hir_ty::Primitive::Real4 => ty::Type::Real(ty::RealSize::Real4),
            hir_ty::Primitive::Real8 => ty::Type::Real(ty::RealSize::Real8),
            hir_ty::Primitive::Boolean => ty::Type::Boolean,
            hir_ty::Primitive::AddressInt => ty::Type::Nat(ty::NatSize::AddressInt),
            hir_ty::Primitive::Char => ty::Type::Char,
            hir_ty::Primitive::String => ty::Type::String,
            hir_ty::Primitive::SizedChar(len) => {
                // Note: 32768 is the minimum defined limit for the length on `n` for char(N)
                // ???: Do we want to add a config/feature option to change this?
                let size_limit = 32768;

                match self.lower_seq_len(*len, size_limit) {
                    Ok(len) => ty::Type::CharN(len),
                    Err(err) => {
                        err.report_to(&mut self.reporter);
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
                        err.report_to(&mut self.reporter);
                        ty::Type::Error
                    }
                }
            }
        };

        // Maps the type id to the current type node
        let ty_ref = self.ty_ctx.add_type(ty);
        self.ty_ctx.map_type(id, ty_ref);
    }

    fn lower_seq_len(
        &mut self,
        seq_len: toc_hir::ty::SeqLength,
        size_limit: u32,
    ) -> Result<ty::SeqSize, SeqLenError> {
        let expr = match seq_len {
            hir_ty::SeqLength::Dynamic => return Ok(ty::SeqSize::Dynamic),
            hir_ty::SeqLength::Expr(expr) => expr,
        };

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

        let span = self.unit.database.expr_nodes.spans[&expr];

        // Check that the value is actually the correct type, and in the correct value range.
        // Size can only be in (0, 32768)
        let int = value
            .as_int()
            .ok_or_else(|| SeqLenError::WrongType(Spanned::new(value, span)))?;

        // Convert into a size, within the given limit
        let size = int
            .into_u32()
            .and_then(NonZeroU32::new)
            .filter(|size| size.get() < size_limit)
            .ok_or_else(|| SeqLenError::WrongSize(Spanned::new(int, span), size_limit))?;

        Ok(ty::SeqSize::Fixed(size))
    }

    fn get_ty_ref_from_expr(&mut self, expr: expr::ExprIdx) -> Spanned<ty::TyRef> {
        let ty_ref = self.require_expr_ty(self.eval_kinds[&expr]);
        let span = self.unit.database.expr_nodes.spans[&expr];
        Spanned::new(ty_ref, span)
    }

    fn type_check_binary_op(
        &mut self,
        lhs_id: expr::ExprIdx,
        op: Spanned<expr::BinaryOp>,
        rhs_id: expr::ExprIdx,
    ) -> ty::Type {
        let lhs_ty_ref = self.get_ty_ref_from_expr(lhs_id);
        let rhs_ty_ref = self.get_ty_ref_from_expr(rhs_id);

        match ty::rules::check_binary_operands(lhs_ty_ref, op, rhs_ty_ref) {
            Ok(ty) => ty,
            Err(err) => {
                ty::rules::report_binary_typecheck_error(err, &mut self.reporter);
                ty::Type::Error
            }
        }
    }

    fn type_check_unary_op(
        &mut self,
        op: Spanned<expr::UnaryOp>,
        rhs_id: expr::ExprIdx,
    ) -> ty::Type {
        let rhs_ty_ref = self.get_ty_ref_from_expr(rhs_id);

        match ty::rules::check_unary_operands(op, rhs_ty_ref) {
            Ok(ty) => ty,
            Err(err) => {
                ty::rules::report_unary_typecheck_error(err, &mut self.reporter);
                ty::Type::Error
            }
        }
    }
}

/// ???: Mapping concrete TypeId's back to ty::TypeIdx?
/// Pass in the ty::TypeIdx, which is mapped by the TyCtx
impl toc_hir::HirVisitor for TypeCheck<'_> {
    fn visit_constvar(&mut self, _id: stmt::StmtIdx, decl: &stmt::ConstVar) {
        self.typeck_constvar(decl);
    }

    fn visit_assign(&mut self, _id: stmt::StmtIdx, stmt: &stmt::Assign) {
        self.typeck_assign(stmt);
    }

    fn visit_literal(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Literal) {
        self.typeck_literal(id, expr);
    }

    fn visit_binary(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Binary) {
        self.typeck_binary(id, expr);
    }

    fn visit_unary(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Unary) {
        self.typeck_unary(id, expr);
    }

    fn visit_paren(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Paren) {
        self.typeck_paren(id, expr);
    }

    fn visit_name(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Name) {
        self.typeck_name(id, expr);
    }

    fn visit_primitive(&mut self, id: hir_ty::TypeIdx, ty: &hir_ty::Primitive) {
        self.typeck_primitive(id, ty);
    }
}

/// What an expression evaluates to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvalKind {
    Ref(DefKind),
    Value(TyRef),
}

impl EvalKind {
    fn as_expr_ty(self) -> Option<TyRef> {
        match self {
            EvalKind::Ref(DefKind::Const(ty))
            | EvalKind::Ref(DefKind::Var(ty))
            | EvalKind::Ref(DefKind::Error(ty))
            | EvalKind::Value(ty) => Some(ty),
            EvalKind::Ref(DefKind::Type(_)) => None,
        }
    }

    fn as_mut_ref_ty(self) -> Option<TyRef> {
        match self {
            EvalKind::Ref(DefKind::Var(ty)) | EvalKind::Ref(DefKind::Error(ty)) => Some(ty),
            _ => None,
        }
    }
}

enum SeqLenError {
    ConstEval(Spanned<ConstError>),
    WrongType(Spanned<ConstValue>),
    WrongSize(Spanned<ConstInt>, u32),
}

impl SeqLenError {
    fn report_to(&self, reporter: &mut MessageSink) {
        match self {
            SeqLenError::ConstEval(err) => {
                err.item().report_to(reporter, err.span());
            }
            SeqLenError::WrongType(value) => {
                reporter
                    .report_detailed(
                        MessageKind::Error,
                        "wrong type for character count",
                        value.span(),
                    )
                    .with_note(
                        &format!("expected integer value, found {}", value.item().type_name()),
                        value.span(),
                    )
                    .finish();
            }
            SeqLenError::WrongSize(int, size_limit) => {
                reporter
                    .report_detailed(
                        MessageKind::Error,
                        "invalid character count size",
                        int.span(),
                    )
                    .with_note(&format!("computed count is {}", int.item()), int.span())
                    .with_info(
                        &format!("valid sizes are between 1 to {}", size_limit - 1),
                        int.span(),
                    )
                    .finish();
            }
        }
    }
}
