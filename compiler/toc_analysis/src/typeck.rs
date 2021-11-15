//! Type checking

#[cfg(test)]
mod test;

use std::cell::RefCell;

use toc_hir::expr::{self, BodyExpr};
use toc_hir::library::{self, LibraryId, WrapInLibrary};
use toc_hir::stmt::BodyStmt;
use toc_hir::symbol::{self, DefId};
use toc_hir::{body, item, stmt};
use toc_reporting::CompileResult;
use toc_span::Span;

use crate::const_eval::Const;
use crate::db::HirAnalysis;
use crate::ty;

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

pub(crate) fn typecheck_library(db: &dyn HirAnalysis, library: LibraryId) -> CompileResult<()> {
    TypeCheck::check_library(db, library)
}

struct TypeCheck<'db> {
    // Shared state
    db: &'db dyn HirAnalysis,
    library_id: library::LibraryId,
    library: library::LoweredLibrary,

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
    reporter: toc_reporting::MessageSink,
}

impl<'db> TypeCheck<'db> {
    fn check_library(db: &'db dyn HirAnalysis, library_id: LibraryId) -> CompileResult<()> {
        let state = TypeCheckState {
            reporter: toc_reporting::MessageSink::new(),
        };
        let state = RefCell::new(state);
        let library = db.library(library_id);

        let typeck = Self {
            db,
            library_id,
            library,

            state,
        };

        // Check bodies, starting from the root
        for body_id in db.bodies_of(library_id).iter().copied() {
            toc_hir::visitor::Walker::from_body(&typeck.library, body_id).visit_postorder(&typeck);
        }

        let state = typeck.state.into_inner();

        let TypeCheckState { reporter, .. } = state;

        CompileResult::new((), reporter.finish())
    }

    fn state(&self) -> std::cell::RefMut<TypeCheckState> {
        self.state.borrow_mut()
    }
}

impl toc_hir::visitor::HirVisitor for TypeCheck<'_> {
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        self.typeck_constvar(id, item);
    }

    fn visit_assign(&self, id: BodyStmt, stmt: &stmt::Assign) {
        self.typeck_assign(id.0, stmt);
    }

    fn visit_put(&self, id: BodyStmt, stmt: &stmt::Put) {
        self.typeck_put(id.0, stmt);
    }

    fn visit_get(&self, id: BodyStmt, stmt: &stmt::Get) {
        self.typeck_get(id.0, stmt);
    }

    fn visit_for(&self, id: BodyStmt, stmt: &stmt::For) {
        self.typeck_for(id.0, stmt);
    }

    fn visit_exit(&self, id: BodyStmt, stmt: &stmt::Exit) {
        self.typeck_exit(id.0, stmt);
    }

    fn visit_if(&self, id: BodyStmt, stmt: &stmt::If) {
        self.typeck_if(id.0, stmt);
    }

    fn visit_case(&self, id: BodyStmt, stmt: &stmt::Case) {
        self.typeck_case(id.0, stmt);
    }

    fn visit_binary(&self, id: BodyExpr, expr: &toc_hir::expr::Binary) {
        self.typeck_binary(id.0, expr);
    }

    fn visit_unary(&self, id: BodyExpr, expr: &toc_hir::expr::Unary) {
        self.typeck_unary(id.0, expr);
    }

    fn visit_name(&self, id: BodyExpr, expr: &expr::Name) {
        self.typeck_name(id, expr)
    }

    fn visit_primitive(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Primitive) {
        self.typeck_primitive(id, ty);
    }
}

impl TypeCheck<'_> {
    fn typeck_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        // Check the initializer expression
        let (ty_spec, init) = if let item::ConstVarTail::Both(ty_spec, initializer) = item.tail {
            (ty_spec, initializer)
        } else {
            // Inferred or already specified
            return;
        };

        let def_id = DefId(self.library_id, self.library.item(id).def_id);
        let left = self.db.type_of(def_id.into());
        let right = self.db.type_of((self.library_id, init).into());

        // Ignore mutability because we're checking an initializer
        let valid_asn = ty::rules::is_assignable(self.db, left, right, true);
        if !valid_asn.expect("lhs ty not from reference") {
            // Incompatible, report it
            let init_span = self
                .library
                .body(init)
                .span
                .lookup_in(&self.library.span_map);
            let spec_span = self
                .library
                .lookup_type(ty_spec)
                .span
                .lookup_in(&self.library.span_map);

            self.report_mismatched_assign_tys(left, right, init_span, spec_span, init_span);

            // Don't need to worry about ConstValue being anything,
            // since that should be handled by const eval type restrictions
            // However, there should still be an assert here
            // TODO: Add assert ensuring there is no valid ConstValue
        }
    }

    fn typeck_assign(&self, in_body: body::BodyId, item: &stmt::Assign) {
        let lib_id = self.library_id;
        let db = self.db;

        let left = db.type_of((lib_id, in_body, item.lhs).into());
        let right = db.type_of((lib_id, in_body, item.rhs).into());

        // Check if types are assignable
        // Leave error types as "always assignable"
        let asn_able = ty::rules::is_assignable(db, left, right, false);
        let asn_span = item.asn.lookup_in(&self.library.span_map);

        match asn_able {
            Some(true) => {} // Valid
            Some(false) => {
                // Invalid types!
                let body = self.library.body(in_body);
                let left_span = body.expr(item.lhs).span.lookup_in(&self.library.span_map);
                let right_span = body.expr(item.rhs).span.lookup_in(&self.library.span_map);

                self.report_mismatched_assign_tys(left, right, asn_span, left_span, right_span);
            }
            None => {
                // Not a mut ref
                let left_span = self
                    .library
                    .body(in_body)
                    .expr(item.lhs)
                    .span
                    .lookup_in(&self.library.span_map);

                // TODO: Stringify lhs for more clarity on the error location
                // TODO: If it's a ref expr, get the definition point
                self.state()
                    .reporter
                    .error_detailed("cannot assign into expression", asn_span)
                    .with_note("not a reference to a variable", left_span)
                    .finish();
            }
        }
    }

    fn report_mismatched_assign_tys(
        &self,
        left: ty::TypeId,
        right: ty::TypeId,
        asn_span: toc_span::Span,
        left_span: toc_span::Span,
        right_span: toc_span::Span,
    ) {
        let db = self.db;
        let left_ty = left.in_db(db).peel_ref();
        let right_ty = right.in_db(db).peel_ref();

        self.state()
            .reporter
            .error_detailed("mismatched types", asn_span)
            .with_note(
                &format!("this is of type `{right}`", right = right_ty),
                right_span,
            )
            .with_note(
                &format!("this is of type `{left}`", left = left_ty),
                left_span,
            )
            .with_info(&format!(
                "`{right}` is not assignable into `{left}`",
                left = left_ty,
                right = right_ty
            ))
            .finish();
    }

    fn typeck_put(&self, body_id: body::BodyId, stmt: &stmt::Put) {
        let body = self.library.body(body_id);

        if let Some(stream) = stmt.stream_num {
            self.check_text_io_arg(stream.in_body(body_id));
        }

        let items = stmt.items.iter().filter_map(|item| match item {
            stmt::Skippable::Item(item) => Some(item),
            _ => None,
        });

        for item in items {
            let put_ty = self
                .db
                .type_of((self.library_id, body_id, item.expr).into())
                .in_db(self.db)
                .peel_ref();
            let put_kind = put_ty.kind();

            if !self.is_text_io_item(put_ty.id()) {
                continue;
            }

            // Only the following are allowed to have precision & exponent options:
            // - Int
            // - Nat
            // - Real
            if !put_kind.is_number() {
                let item_span = body.expr(item.expr).span.lookup_in(&self.library.span_map);
                if let Some(expr) = item.opts.precision() {
                    let span = body.expr(expr).span.lookup_in(&self.library.span_map);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            &format!("cannot specify fraction width for `{}`", put_ty),
                            item_span,
                        )
                        .with_error("this is the invalid option", span)
                        .with_info("fraction width can only be specified for numeric put types")
                        .finish();
                }

                if let Some(expr) = item.opts.exponent_width() {
                    let span = body.expr(expr).span.lookup_in(&self.library.span_map);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            &format!("cannot specify exponent width for `{}`", put_ty),
                            item_span,
                        )
                        .with_error("this is the invalid option", span)
                        .with_info("exponent width can only be specified for numeric types")
                        .finish();
                }
            }

            // Check that the parameters are all integers
            if let Some(width) = item.opts.width() {
                self.check_text_io_arg(width.in_body(body_id));
            }

            if let Some(precision) = item.opts.precision() {
                self.check_text_io_arg(precision.in_body(body_id));
            }

            if let Some(exp_width) = item.opts.exponent_width() {
                self.check_text_io_arg(exp_width.in_body(body_id));
            }
        }
    }

    fn typeck_get(&self, body_id: body::BodyId, stmt: &stmt::Get) {
        let db = self.db;
        let body = self.library.body(body_id);

        if let Some(stream) = stmt.stream_num {
            self.check_text_io_arg(stream.in_body(body_id));
        }

        let items = stmt.items.iter().filter_map(|item| match item {
            stmt::Skippable::Item(item) => Some(item),
            _ => None,
        });

        for item in items {
            // Item expression must be a variable ref
            let body_expr = item.expr.in_body(body_id);
            let ty = db.type_of((self.library_id, body_expr).into()).in_db(db);

            if !self.is_text_io_item(ty.id()) {
                continue;
            }

            if ty.as_deref_mut().is_none() {
                let get_item_span = body.expr(item.expr).span.lookup_in(&self.library.span_map);

                // TODO: Stringify item for more clarity on the error location
                // TODO: If it's a ref expr, get the definition point
                self.state()
                    .reporter
                    .error_detailed("cannot assign into expression", get_item_span)
                    .with_error("not a reference to a variable", get_item_span)
                    .finish();
            }

            if let stmt::GetWidth::Chars(expr) = item.width {
                self.check_text_io_arg(expr.in_body(body_id));
            }
        }
    }

    fn check_text_io_arg(&self, expr: BodyExpr) {
        let ty_ref = self.db.type_of((self.library_id, expr).into());
        let span = self.library.body(expr.0).expr(expr.1).span;

        self.expect_integer_type(ty_ref, self.library.lookup_span(span));
    }

    fn is_text_io_item(&self, ty: ty::TypeId) -> bool {
        let db = self.db;
        let ty_dat = ty.in_db(db).peel_ref();

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
        match &*ty_dat.kind() {
            ty::TypeKind::Error
            | ty::TypeKind::Boolean
            | ty::TypeKind::Int(_)
            | ty::TypeKind::Nat(_)
            | ty::TypeKind::Real(_)
            | ty::TypeKind::Integer
            | ty::TypeKind::Char
            | ty::TypeKind::String
            | ty::TypeKind::CharN(_)
            | ty::TypeKind::StringN(_) => true,
            // Already deref'd
            ty::TypeKind::Ref(_, _) => unreachable!(),
        }
    }

    fn typeck_for(&self, body_id: body::BodyId, stmt: &stmt::For) {
        let db = self.db;

        match stmt.bounds {
            stmt::ForBounds::Implicit(expr) => {
                // These are not supported yet, until range types are lowered
                let expr_span = self.library.body(body_id).expr(expr).span;

                self.state().reporter.error(
                    "unsupported expression",
                    "implicit range bounds are not supported yet",
                    self.library.lookup_span(expr_span),
                )
            }
            stmt::ForBounds::Full(start, end) => {
                // Must both be index types
                let start_ty = db.type_of((self.library_id, body_id, start).into());
                let end_ty = db.type_of((self.library_id, body_id, end).into());

                // Wrap up both types
                let (start_ty, end_ty) =
                    (start_ty.in_db(db).peel_ref(), end_ty.in_db(db).peel_ref());

                let start_span = self.library.body(body_id).expr(start).span;
                let end_span = self.library.body(body_id).expr(end).span;

                let (start_span, end_span) = (
                    self.library.lookup_span(start_span),
                    self.library.lookup_span(end_span),
                );

                let bounds_span = start_span.cover(end_span);

                let is_index_ty = |ty_kind: ty::TyRefKind| ty_kind.is_index() || ty_kind.is_error();

                if !ty::rules::is_equivalent(db, start_ty.id(), end_ty.id()) {
                    // Bounds are not equivalent
                    self.state()
                        .reporter
                        .error_detailed("range bounds are not the same type", bounds_span)
                        .with_note(&format!("this is of type `{}`", start_ty), start_span)
                        .with_note(&format!("this is of type `{}`", end_ty), end_span)
                        .with_info(&format!("`{}` is not equivalent to `{}`", start_ty, end_ty))
                        .finish();
                } else if !is_index_ty(start_ty.kind()) || !is_index_ty(end_ty.kind()) {
                    // Neither is an index type
                    let mut state = self.state();
                    let mut builder = state
                        .reporter
                        .error_detailed("range bounds are not index types", bounds_span);

                    // Specialize when reporting a non-concrete type
                    if matches!(&*start_ty.kind(), ty::TypeKind::Integer)
                        || matches!(&*end_ty.kind(), ty::TypeKind::Integer)
                    {
                        builder = builder
                            .with_note(&format!("this is of type `{}`", start_ty), start_span)
                            .with_note(&format!("this is of type `{}`", end_ty), end_span);
                    } else {
                        builder = builder
                            .with_note(&format!("this is of type `{}`", start_ty), start_span)
                            .with_note(&format!("this is also of type `{}`", end_ty), end_span);
                    }

                    builder.with_info(&format!(
                            "expected an index type (an integer, `{boolean}`, `{chr}`, enumerated type, or a range)",
                            boolean = ty::TypeKind::Boolean.prefix(),
                            chr = ty::TypeKind::Char.prefix()
                        ))
                        .finish();
                }
            }
        }

        if let Some(step_by) = stmt.step_by {
            // `step_by` must evaluate to an integer type
            // ???: Does this make sense for other range types?
            let ty_id = db.type_of((self.library_id, body_id, step_by).into());
            let span = self.library.body(body_id).expr(step_by).span;

            self.expect_integer_type(ty_id, self.library.lookup_span(span));
        }
    }

    fn typeck_exit(&self, body_id: body::BodyId, stmt: &stmt::Exit) {
        if let Some(condition_expr) = stmt.when_condition {
            let condition_ty = self
                .db
                .type_of((self.library_id, body_id, condition_expr).into());
            let span = self.library.body(body_id).expr(condition_expr).span;

            self.expect_boolean_type(condition_ty, self.library.lookup_span(span));
        }
    }

    fn typeck_if(&self, body_id: body::BodyId, stmt: &stmt::If) {
        let condition = self
            .db
            .type_of((self.library_id, body_id, stmt.condition).into());
        let span = self.library.body(body_id).expr(stmt.condition).span;

        self.expect_boolean_type(condition, self.library.lookup_span(span));
    }

    fn typeck_case(&self, body_id: body::BodyId, stmt: &stmt::Case) {
        // Contracts:
        // - case discriminant must be one of the following types
        //   - integer
        //   - char
        //   - boolean
        //   - enum
        //   - string (? charseq)
        // - label selectors must be equivalent types to the case discriminant
        // - label selectors must be compile-time exprs
        let db = self.db;

        let discrim_ty = db
            .type_of((self.library_id, body_id, stmt.discriminant).into())
            .in_db(db)
            .peel_ref();
        let discrim_tykind = discrim_ty.kind();
        let discrim_span = self.library.body(body_id).expr(stmt.discriminant).span;
        let discrim_span = self.library.lookup_span(discrim_span);

        // Check discriminant type
        if !discrim_tykind.is_error()
            && !discrim_tykind.is_index()
            && !matches!(&*discrim_tykind, ty::TypeKind::String)
        {
            self.state()
                .reporter
                .error_detailed("mismatched types", discrim_span)
                .with_note(&format!("this is of type `{}`", discrim_ty), discrim_span)
                .with_info("`case` discriminant must be either:")
                .with_info(&format!(
                    "- an index type (an integer, `{boolean}`, `{chr}`, enumerated type, or a range), or",
                    boolean = ty::TypeKind::Boolean.prefix(),
                    chr = ty::TypeKind::Char.prefix()
                ))
                .with_info(&format!("- a `{}`", ty::TypeKind::String.prefix()))
                .finish();
        }

        // Check label selectors
        for &selector in stmt
            .arms
            .iter()
            .filter_map(|arm| {
                if let stmt::CaseSelector::Exprs(exprs) = &arm.selectors {
                    Some(exprs)
                } else {
                    None
                }
            })
            .flatten()
        {
            // was doing: adding tests
            let selector_ty = db.type_of((self.library_id, body_id, selector).into());
            let selector_span = self.library.body(body_id).expr(selector).span;
            let selector_span = self.library.lookup_span(selector_span);

            // Must match discriminant type
            if !ty::rules::is_equivalent(db, discrim_ty.id(), selector_ty) {
                let selector_ty = selector_ty.in_db(db).peel_ref();

                self.state()
                    .reporter
                    .error_detailed("mismatched types", selector_span)
                    .with_note(&format!("this is of type `{}`", selector_ty), selector_span)
                    .with_note(
                        &format!("discriminant is of type `{}`", discrim_ty),
                        discrim_span,
                    )
                    .with_info(&format!("`{}` is not a `{}`", selector_ty, discrim_ty))
                    .with_info("selector type must match discriminant type")
                    .finish();
            }

            // Must be a compile time expression
            let res = db
                .evaluate_const(
                    Const::from_expr(self.library_id, expr::BodyExpr(body_id, selector)),
                    Default::default(),
                )
                .err();

            if let Some(err) = res {
                err.report_to(db, &mut self.state().reporter);
            }
        }
    }

    fn typeck_binary(&self, body: body::BodyId, expr: &expr::Binary) {
        let db = self.db;
        let lib_id = self.library_id;
        let left = db.type_of((lib_id, body, expr.lhs).into());
        let right = db.type_of((lib_id, body, expr.rhs).into());

        if let Err(err) = ty::rules::check_binary_op(db, left, *expr.op.item(), right) {
            let op_span = self.library.lookup_span(expr.op.span());
            let body = self.library.body(body);
            let left_span = self.library.lookup_span(body.expr(expr.lhs).span);
            let right_span = self.library.lookup_span(body.expr(expr.rhs).span);

            ty::rules::report_invalid_bin_op(
                db,
                err,
                left_span,
                op_span,
                right_span,
                &mut self.state().reporter,
            );
        }
    }

    fn typeck_unary(&self, body: body::BodyId, expr: &expr::Unary) {
        let db = self.db;
        let lib_id = self.library_id;
        let right = db.type_of((lib_id, body, expr.rhs).into());

        if let Err(err) = ty::rules::check_unary_op(db, *expr.op.item(), right) {
            let op_span = self.library.lookup_span(expr.op.span());
            let body = self.library.body(body);
            let right_span = self.library.lookup_span(body.expr(expr.rhs).span);

            ty::rules::report_invalid_unary_op(
                db,
                err,
                op_span,
                right_span,
                &mut self.state().reporter,
            );
        }
    }

    fn typeck_name(&self, id: BodyExpr, expr: &expr::Name) {
        let def_id = match expr {
            expr::Name::Name(def_id) => *def_id,
            expr::Name::Self_ => todo!(), // TODO: Take def id from associated class
        };

        let def_info = self.library.local_def(def_id);

        if matches!(def_info.kind, symbol::SymbolKind::Undeclared) {
            let span = self
                .library
                .body(id.0)
                .expr(id.1)
                .span
                .lookup_in(&self.library.span_map);

            self.state().reporter.error(
                &format!("`{}` is not declared", def_info.name.item()),
                "not found in this scope",
                span,
            );
        }
    }

    fn typeck_primitive(&self, id: toc_hir::ty::TypeId, ty_node: &toc_hir::ty::Primitive) {
        let db = self.db;
        let ty = self
            .db
            .from_hir_type(id.in_library(self.library_id))
            .in_db(db);
        let ty_kind = &*ty.kind();

        let (expr_span, expr_ty) = match ty_node {
            toc_hir::ty::Primitive::SizedChar(toc_hir::ty::SeqLength::Expr(body))
            | toc_hir::ty::Primitive::SizedString(toc_hir::ty::SeqLength::Expr(body)) => {
                let expr_span = self
                    .library
                    .body(*body)
                    .span
                    .lookup_in(&self.library.span_map);
                let expr_ty = db.type_of((self.library_id, *body).into());

                (expr_span, expr_ty)
            }
            _ => return,
        };

        self.expect_integer_type(expr_ty, expr_span);

        // Check resultant size
        let (seq_size, size_limit) = match ty_kind {
            ty::TypeKind::CharN(seq_size @ ty::SeqSize::Fixed(_)) => {
                // Note: 32768 is the minimum defined limit for the length on `n` for char(N)
                // ???: Do we want to add a config/feature option to change this?
                (seq_size, 32768)
            }
            ty::TypeKind::StringN(seq_size @ ty::SeqSize::Fixed(_)) => {
                // 256 is the maximum defined limit for the length on `n` for string(N),
                // so no option of changing that (unless we have control over the interpreter code).
                // - Legacy interpreter has the assumption baked in that the max length of a string is 256,
                //   so we can't change it yet unless we use a new interpreter.
                (seq_size, 256)
            }
            // because of hir disambiguation above
            _ => unreachable!(),
        };

        let int = match seq_size.fixed_len(db, expr_span) {
            Ok(Some(v)) => v,
            Ok(None) => return, // dynamic, doesn't need checking
            Err(err) => {
                err.report_to(db, &mut self.state().reporter);
                return;
            }
        };

        // Convert into a size, checking if it's within the given limit
        if !int
            .into_u32()
            .map(|size| (1..size_limit).contains(&size))
            .unwrap_or(false)
        {
            self.state()
                .reporter
                .error_detailed("invalid character count size", expr_span)
                .with_error(&format!("computed count is {}", int), expr_span)
                .with_info(&format!("valid sizes are between 1 to {}", size_limit - 1))
                .finish();
        }
    }

    // TODO: Replace `expect_*_type` with `expect_type` once we have `ty::rules::is_equivalent`

    fn expect_integer_type(&self, type_id: ty::TypeId, span: Span) {
        let ty = type_id.in_db(self.db).peel_ref();
        let ty_kind = ty.kind();

        if !ty_kind.is_integer() && !ty_kind.is_error() {
            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_note(&format!("this is of type `{}`", ty), span)
                .with_info(&format!("`{}` is not an integer type", ty))
                .finish();
        }
    }

    fn expect_boolean_type(&self, type_id: ty::TypeId, span: Span) {
        let ty = type_id.in_db(self.db).peel_ref();
        let expected_ty = self.db.mk_boolean().in_db(self.db);
        let ty_kind = ty.kind();

        if !ty_kind.is_boolean() && !ty_kind.is_error() {
            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_note(&format!("this is of type `{}`", ty), span)
                .with_info(&format!(
                    "expected a `{}` type",
                    expected_ty.kind().prefix()
                ))
                .finish();
        }
    }
}
