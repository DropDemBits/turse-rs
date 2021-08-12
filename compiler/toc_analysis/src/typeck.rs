//! Type checking
#[cfg(test)]
mod test;

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::num::NonZeroU32;
use std::sync::Arc;

use toc_hir::{db, expr, stmt, ty as hir_ty, unit};
use toc_reporting::{CompileResult, MessageSink};
use toc_span::Spanned;

use crate::const_eval::{ConstError, ConstEvalCtx, ConstInt, RestrictType};
use crate::ty::{self, DefKind, TyCtx, TyRef};

// ???: Can we build up a type ctx without doing type propagation?
// Type propagation is inferring of types from inputs
// E.g. this stmt involves some type propagation
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
// - Mutability in general falls under responsibility of assignability (so by extension, typeck is responsible)
// - Export mutability matters for mutation outside of the local unit scope, normal const/var rules apply for local units

pub fn typecheck_unit(
    hir_db: db::HirDb,
    unit: &unit::Unit,
    const_eval: Arc<ConstEvalCtx>,
) -> CompileResult<TyCtx> {
    TypeCheck::check_unit(hir_db, unit, const_eval)
}

struct TypeCheck<'a> {
    // Shared state
    hir_db: db::HirDb,
    unit: &'a unit::Unit,
    const_eval: Arc<ConstEvalCtx>,

    // Main mutable state
    state: RefCell<TypeCheckState>,
}

/// Inner typecheck state
///
/// Now stored in a `RefCell` in `TypeCheck`, since before borrows into the HIR tree
/// were sourced indirectly through a shared reference to `unit::Unit`. borrowck was
/// able to reason that the `&mut self` and `&HirNode` refs were independent, and could
/// coexist.
///
/// With the borrows now being sourced from a `db::HirDb` field, the `&mut self` and
/// `&HirNode` borrows overlapped and calling any `&mut self` would trigger a borrowck error.
///
/// Hence the move to `&self` receiver combined with a `RefCell`.
struct TypeCheckState {
    ty_ctx: TyCtx,
    cached_expr_evals: HashMap<expr::ExprId, EvalKind>,
    reporter: toc_reporting::MessageSink,
}

impl<'a> TypeCheck<'a> {
    fn check_unit(
        hir_db: db::HirDb,
        unit: &'a unit::Unit,
        const_eval: Arc<ConstEvalCtx>,
    ) -> CompileResult<TyCtx> {
        let state = TypeCheckState {
            ty_ctx: TyCtx::new(),
            cached_expr_evals: HashMap::new(),
            reporter: toc_reporting::MessageSink::new(),
        };
        let state = RefCell::new(state);

        let typeck = Self {
            hir_db,
            unit,
            const_eval,

            state,
        };

        // Check root stmts
        for stmt in &unit.stmts {
            typeck.typeck_stmt(*stmt);
        }

        let state = typeck.state.into_inner();

        let TypeCheckState {
            ty_ctx, reporter, ..
        } = state;

        CompileResult::new(ty_ctx, reporter.finish())
    }

    fn state(&self) -> std::cell::RefMut<TypeCheckState> {
        self.state.borrow_mut()
    }

    fn typeck_stmt(&self, id: stmt::StmtId) {
        match self.hir_db.get_stmt(id) {
            stmt::Stmt::ConstVar(decl) => self.typeck_constvar(decl),
            stmt::Stmt::Assign(stmt) => self.typeck_assign(stmt),
            stmt::Stmt::Put(stmt) => self.typeck_put(stmt),
            stmt::Stmt::Get(stmt) => self.typeck_get(stmt),
            stmt::Stmt::Block(stmt) => self.typeck_block(stmt),
        }
    }

    fn typeck_expr(&self, id: expr::ExprId) -> EvalKind {
        // Contain the mut borrow of the state stuff
        {
            if let Some(cached_eval) = self.state().cached_expr_evals.get(&id) {
                return *cached_eval;
            }
        }

        // Walk through new the expression
        let eval_kind = match self.hir_db.get_expr(id) {
            expr::Expr::Missing => {
                // Missing, treat as an error type
                let err = self.state().ty_ctx.add_type(ty::Type::Error);
                EvalKind::Error(err)
            }
            expr::Expr::Literal(expr) => self.typeck_literal(expr),
            expr::Expr::Binary(expr) => self.typeck_binary(expr),
            expr::Expr::Unary(expr) => self.typeck_unary(expr),
            expr::Expr::Paren(expr) => self.typeck_paren(expr),
            expr::Expr::Name(expr) => self.typeck_name(expr),
        };

        // Cache the result
        self.state().cached_expr_evals.insert(id, eval_kind);

        eval_kind
    }

    fn lower_type(&self, id: hir_ty::TypeId) -> TyRef {
        // Contain the mut borrow of the state stuff
        {
            if let Some(cached_lower) = self.state().ty_ctx.get_type(id) {
                return cached_lower;
            }
        }

        let ty = match self.hir_db.get_type(id) {
            // Missing => treat as an error type
            hir_ty::Type::Missing => ty::Type::Error,
            hir_ty::Type::Primitive(ty) => self.typeck_primitive(ty),
        };

        // Add to ty_ctx cache
        let ty_ref = self.state().ty_ctx.add_type(ty);
        self.state().ty_ctx.map_type(id, ty_ref);
        ty_ref
    }

    fn get_spanned_expr_ty_ref(&self, id: expr::ExprId) -> Spanned<TyRef> {
        let ty_ref = self.typeck_expr(id).as_ty_ref();
        let span = self.hir_db.get_span(id.into());
        Spanned::new(ty_ref, span)
    }

    fn require_constvar_ref(&self, def_kind: DefKind) -> EvalKind {
        match def_kind {
            DefKind::Const(ty) => EvalKind::ConstRef(ty),
            DefKind::Var(ty) => EvalKind::VarRef(ty),
            DefKind::Error(err) => EvalKind::Error(err),
            DefKind::Type(_) => {
                // TODO: Report this error once type decls are lowered

                let err = self.state().ty_ctx.add_type(ty::Type::Error);
                EvalKind::Error(err)
            }
        }
    }

    fn typeck_constvar(&self, decl: &stmt::ConstVar) {
        // extract type for declared identifiers
        // if both are present, then typecheck as assignment
        let ty_ref = match &decl.tail {
            stmt::ConstVarTail::Both(ty_spec, _) | stmt::ConstVarTail::TypeSpec(ty_spec) => {
                // From type_spec
                self.lower_type(*ty_spec)
            }
            stmt::ConstVarTail::InitExpr(expr) => {
                // From inferred init expr
                self.typeck_expr(*expr).as_ty_ref()
            }
        };

        if let stmt::ConstVarTail::Both(ty_spec, init_expr) = &decl.tail {
            let l_value_ty = self.lower_type(*ty_spec);
            let r_value_ty = self.typeck_expr(*init_expr).as_ty_ref();

            if let Some(false) = ty::rules::is_ty_assignable_to(l_value_ty, r_value_ty) {
                // Incompatible, report it
                let init_span = self.hir_db.get_span(init_expr.into());
                let spec_span = self.hir_db.get_span(ty_spec.into());

                self.state()
                    .reporter
                    .error_detailed("mismatched types", init_span)
                    .with_note(
                        "initializer's type is incompatible with this type",
                        spec_span,
                    )
                    .finish();

                // Don't need to worry about ConstValue being anything,
                // since that should be handled by const eval type restrictions
                // However, there should still be an assert here
                // TODO: Add assert ensuring there is no valid ConstValue
            }
        }

        // Make the type concrete
        let ty_ref = if *ty_ref == ty::Type::Integer {
            // Integer decomposes into a normal `int`
            self.state()
                .ty_ctx
                .add_type(ty::Type::Int(ty::IntSize::Int))
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
            self.state().ty_ctx.map_def_id(*def, def_kind);
        }
    }

    fn typeck_assign(&self, stmt: &stmt::Assign) {
        let l_value_eval = self.typeck_expr(stmt.lhs);
        let r_value_eval = self.typeck_expr(stmt.rhs);

        // Check if we can even assign into the l_value (i.e. is lhs mutable)
        let l_value_ty = if let Some(ty) = l_value_eval.as_mut_ty_ref() {
            ty
        } else {
            let l_value_span = self.hir_db.get_span(stmt.lhs.into());

            // TODO: Stringify lhs for more clarity on the error location
            self.state()
                .reporter
                .error_detailed(
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
            let r_value_ty = r_value_eval.as_ty_ref();

            // Check binary op if present
            if let Some(bin_op) = stmt.op.item().as_binary_op() {
                let new_ty = self.type_check_binary_op(
                    stmt.lhs,
                    Spanned::new(bin_op, stmt.op.span()),
                    stmt.rhs,
                );
                self.state().ty_ctx.add_type(new_ty)
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
            self.state()
                .reporter
                .error_detailed("mismatched types", stmt.op.span())
                .finish();
        }
    }

    fn typeck_put(&self, stmt: &stmt::Put) {
        if let Some(stream) = stmt.stream_num {
            self.check_text_io_arg(stream);
        }

        let items = stmt.items.iter().filter_map(|item| match item {
            stmt::Skippable::Item(item) => Some(item),
            _ => None,
        });

        for item in items {
            let put_type = self.check_text_io_item(item.expr);
            if ty::rules::is_error(put_type.item()) {
                continue;
            }

            // Only the following are allowed to have precision & exponent options:
            // - Int
            // - Nat
            // - Real
            if !ty::rules::is_number(put_type.item()) && !ty::rules::is_error(put_type.item()) {
                if let Some(expr) = item.opts.precision() {
                    let span = self.hir_db.get_span(expr.into());

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            "cannot specify fraction width for this type",
                            put_type.span(),
                        )
                        .with_info(
                            "fraction width can only be specified for numeric put types",
                            None,
                        )
                        .finish();
                }

                if let Some(expr) = item.opts.exponent_width() {
                    let span = self.hir_db.get_span(expr.into());

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            "cannot specify exponent width for this type",
                            put_type.span(),
                        )
                        .with_info(
                            "exponent width can only be specified for numeric types",
                            None,
                        )
                        .finish();
                }
            }

            // Check that the parameters are all integers
            if let Some(width) = item.opts.width() {
                self.check_text_io_arg(width);
            }

            if let Some(precision) = item.opts.precision() {
                self.check_text_io_arg(precision);
            }

            if let Some(exp_width) = item.opts.exponent_width() {
                self.check_text_io_arg(exp_width);
            }
        }
    }

    fn typeck_get(&self, stmt: &stmt::Get) {
        if let Some(stream) = stmt.stream_num {
            self.check_text_io_arg(stream);
        }

        let items = stmt.items.iter().filter_map(|item| match item {
            stmt::Skippable::Item(item) => Some(item),
            _ => None,
        });

        for item in items {
            // Item expression must be a variable ref
            self.check_text_io_item(item.expr);
            let eval_kind = self.typeck_expr(item.expr);

            if !matches!(eval_kind, EvalKind::VarRef(_)) {
                let get_item_span = self.hir_db.get_span(item.expr.into());

                // TODO: Stringify item for more clarity on the error location
                self.state()
                    .reporter
                    .error_detailed("cannot assign into get item expression", get_item_span)
                    .with_note(
                        "this expression cannot be used as a variable reference",
                        get_item_span,
                    )
                    .finish();
            }

            if let stmt::GetWidth::Chars(expr) = item.width {
                self.check_text_io_arg(expr);
            }
        }
    }

    fn check_text_io_arg(&self, id: expr::ExprId) {
        let ty_ref = self.get_spanned_expr_ty_ref(id);

        self.check_integer_type(ty_ref);
    }

    fn check_integer_type(&self, ty_ref: Spanned<TyRef>) {
        if !ty::rules::is_integer(ty_ref.item()) && !ty::rules::is_error(ty_ref.item()) {
            // TODO: Stringify type for more clarity on the error
            self.state()
                .reporter
                .error_detailed("mismatched types", ty_ref.span())
                .with_note("expected integer type", ty_ref.span())
                .finish();
        }
    }

    fn check_text_io_item(&self, id: expr::ExprId) -> Spanned<TyRef> {
        let ty_ref = self.get_spanned_expr_ty_ref(id);

        // Must be a valid put/get type
        // Can be one of the following:
        // - Int
        // - Nat
        // - Real
        // - Char
        // - Char(N)
        // - String
        // - String(N)
        // - Boolean
        // - Enum

        // For now, all lowered types satisfy this condition
        match &*(*ty_ref.item()) {
            ty::Type::Error
            | ty::Type::Boolean
            | ty::Type::Int(_)
            | ty::Type::Nat(_)
            | ty::Type::Real(_)
            | ty::Type::Integer
            | ty::Type::Char
            | ty::Type::String
            | ty::Type::CharN(_)
            | ty::Type::StringN(_) => {}
        }

        ty_ref
    }

    fn typeck_block(&self, stmt: &stmt::Block) {
        for stmt in &stmt.stmts {
            self.typeck_stmt(*stmt)
        }
    }

    fn typeck_literal(&self, expr: &toc_hir::expr::Literal) -> EvalKind {
        let ty = match expr {
            toc_hir::expr::Literal::Integer(_) => ty::Type::Integer,
            toc_hir::expr::Literal::Real(_) => ty::Type::Real(ty::RealSize::Real),
            toc_hir::expr::Literal::Char(_) => ty::Type::Char,
            toc_hir::expr::Literal::CharSeq(s) => {
                let size = NonZeroU32::new(s.len().try_into().unwrap_or(u32::MAX)).unwrap();
                let seq_size = ty::SeqSize::Fixed(size);
                ty::Type::CharN(seq_size)
            }
            toc_hir::expr::Literal::String(_) => ty::Type::String,
            toc_hir::expr::Literal::Boolean(_) => ty::Type::Boolean,
        };

        // Evaluates to a value
        EvalKind::Value(self.state().ty_ctx.add_type(ty))
    }

    fn typeck_binary(&self, expr: &toc_hir::expr::Binary) -> EvalKind {
        // TODO: do full binexpr typechecks
        let ty = self.type_check_binary_op(expr.lhs, expr.op, expr.rhs);

        // Evaluates to a value
        EvalKind::Value(self.state().ty_ctx.add_type(ty))
    }

    fn typeck_unary(&self, expr: &toc_hir::expr::Unary) -> EvalKind {
        let ty = self.type_check_unary_op(expr.op, expr.rhs);

        // Evaluates to a value
        EvalKind::Value(self.state().ty_ctx.add_type(ty))
    }

    fn typeck_paren(&self, expr: &toc_hir::expr::Paren) -> EvalKind {
        // Same eval kind as the inner
        self.typeck_expr(expr.expr)
    }

    fn typeck_name(&self, expr: &toc_hir::expr::Name) -> EvalKind {
        // If def-id, fetch type from def id map
        // If self, then fetch type from provided class def id?
        let (use_id, ty_ref) = match expr {
            toc_hir::expr::Name::Name(use_id) => {
                (use_id, self.state().ty_ctx.get_def_id_kind(use_id.as_def()))
            }
            toc_hir::expr::Name::Self_ => {
                todo!()
            }
        };

        let name_def = if let Some(ty_ref) = ty_ref {
            ty_ref
        } else {
            // Nab symbol info
            let sym_name = &self.unit.symbol_table.get_symbol(use_id.as_def()).name;
            let expr_range = self.unit.symbol_table.get_use_span(*use_id);

            // Not declared, no type provided by any decls
            self.state()
                .reporter
                .error(&format!("`{}` is not declared", sym_name), expr_range);

            // Build error type
            let err_ref = self.state().ty_ctx.add_type(ty::Type::Error);
            let def_kind = DefKind::Error(err_ref);
            self.state().ty_ctx.map_def_id(use_id.as_def(), def_kind);

            def_kind
        };

        self.require_constvar_ref(name_def)
    }

    fn typeck_primitive(&self, ty: &hir_ty::Primitive) -> ty::Type {
        // Create the correct type based off of the base primitive type
        match ty {
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
            }
        }
    }

    fn lower_seq_len(
        &self,
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

        let span = self.hir_db.get_span(expr.into());

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

    fn type_check_binary_op(
        &self,
        lhs_id: expr::ExprId,
        op: Spanned<expr::BinaryOp>,
        rhs_id: expr::ExprId,
    ) -> ty::Type {
        let lhs_ty_ref = self.get_spanned_expr_ty_ref(lhs_id);
        let rhs_ty_ref = self.get_spanned_expr_ty_ref(rhs_id);

        match ty::rules::check_binary_operands(lhs_ty_ref, op, rhs_ty_ref) {
            Ok(ty) => ty,
            Err(err) => {
                ty::rules::report_binary_typecheck_error(err, &mut self.state().reporter);
                ty::Type::Error
            }
        }
    }

    fn type_check_unary_op(&self, op: Spanned<expr::UnaryOp>, rhs_id: expr::ExprId) -> ty::Type {
        let rhs_ty_ref = self.get_spanned_expr_ty_ref(rhs_id);

        match ty::rules::check_unary_operands(op, rhs_ty_ref) {
            Ok(ty) => ty,
            Err(err) => {
                ty::rules::report_unary_typecheck_error(err, &mut self.state().reporter);
                ty::Type::Error
            }
        }
    }
}

/// What an expression evaluates to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvalKind {
    VarRef(TyRef),
    ConstRef(TyRef),
    Value(TyRef),
    Error(TyRef),
}

impl EvalKind {
    fn as_ty_ref(self) -> TyRef {
        match self {
            EvalKind::ConstRef(ty) | EvalKind::VarRef(ty) | EvalKind::Value(ty) => ty,
            EvalKind::Error(err) => err,
        }
    }

    fn as_mut_ty_ref(self) -> Option<TyRef> {
        match self {
            EvalKind::VarRef(ty) => Some(ty),
            EvalKind::Error(err) => Some(err),
            _ => None,
        }
    }
}

enum SeqLenError {
    ConstEval(ConstError),
    WrongSize(Spanned<ConstInt>, u32),
}

impl SeqLenError {
    fn report_to(&self, reporter: &mut MessageSink) {
        match self {
            SeqLenError::ConstEval(err) => err.report_to(reporter),
            SeqLenError::WrongSize(int, size_limit) => {
                reporter
                    .error_detailed("invalid character count size", int.span())
                    .with_note(&format!("computed count is {}", int.item()), int.span())
                    .with_info(
                        &format!("valid sizes are between 1 to {}", size_limit - 1),
                        None,
                    )
                    .finish();
            }
        }
    }
}
