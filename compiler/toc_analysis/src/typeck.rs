//! Type checking

#[cfg(test)]
mod test;

use std::cell::RefCell;

use toc_hir::expr::{self, BodyExpr};
use toc_hir::library::{self, LibraryId, WrapInLibrary};
use toc_hir::stmt::BodyStmt;
use toc_hir::symbol::{BindingKind, DefId, Mutability};
use toc_hir::{body, item, stmt};
use toc_reporting::CompileResult;
use toc_span::Span;

use crate::const_eval::{Const, ConstValue};
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

    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {
        self.typeck_type_decl(id, item);
    }

    fn visit_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        self.typeck_bind_decl(id, item);
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
        self.typeck_name_expr(id, expr);
    }

    fn visit_primitive(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Primitive) {
        self.typeck_primitive(id, ty);
    }

    fn visit_alias(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Alias) {
        self.typeck_alias(id, ty);
    }
}

impl TypeCheck<'_> {
    fn typeck_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        // Type spec must be resolved at this point
        if let Some(ty_spec) = item.type_spec {
            self.require_resolved_type(ty_spec);
        }

        // Check the initializer expression
        let (ty_spec, init) = if let Some(bundle) = item.type_spec.zip(item.init_expr) {
            bundle
        } else {
            // Inferred or already specified
            return;
        };

        let def_id = DefId(self.library_id, self.library.item(id).def_id);
        let left = self.db.type_of(def_id.into());
        let right = self.db.type_of((self.library_id, init).into());

        if !ty::rules::is_assignable(self.db, left, right) {
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

    fn typeck_type_decl(&self, _id: item::ItemId, item: &item::Type) {
        if let item::DefinedType::Alias(ty) = &item.type_def {
            self.require_resolved_type(*ty)
        }
    }

    fn typeck_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        let db = self.db;
        let bind_span = self.library.item(id).span;
        let bind_span = self.library.lookup_span(bind_span);

        let lib_id = self.library_id;
        let bind_to = (lib_id, item.bind_to);

        // Require that we're binding to (mutable) storage
        let predicate = match item.mutability {
            Mutability::Const => BindingKind::is_ref,
            Mutability::Var => BindingKind::is_ref_mut,
        };

        // ???: Rules on registers?

        if !db
            .binding_kind(bind_to.into())
            .map(predicate)
            .unwrap_or(false)
        {
            // Not a (mut) ref
            // Use var mutability for a simpler error message
            // ???: Does it matter to use the real mutability?
            let from = self.library.local_def(item.def_id).name.item();
            let bind_to_span = self
                .library
                .body(item.bind_to)
                .span
                .lookup_in(&self.library.span_map);

            self.report_mismatched_binding(
                BindingKind::Storage(Mutability::Var),
                bind_to.into(),
                bind_span,
                bind_to_span,
                |name| format!("cannot bind `{from}` to `{name}`"),
                || format!("cannot bind `{from}` to expression"),
            );
        }
    }

    fn typeck_assign(&self, in_body: body::BodyId, item: &stmt::Assign) {
        let lib_id = self.library_id;
        let db = self.db;
        let asn_span = item.asn.lookup_in(&self.library.span_map);

        let lhs = (lib_id, in_body, item.lhs);
        let rhs = (lib_id, in_body, item.rhs);

        // Check if we're assigning into a mut ref
        if !db
            .binding_kind(lhs.into())
            .map(BindingKind::is_ref_mut)
            .unwrap_or(false)
        {
            // Not a mut ref
            let left_span = self
                .library
                .body(in_body)
                .expr(item.lhs)
                .span
                .lookup_in(&self.library.span_map);

            self.report_mismatched_binding(
                BindingKind::Storage(Mutability::Var),
                lhs.into(),
                asn_span,
                left_span,
                |name| format!("cannot assign into `{name}`"),
                || "cannot assign into expression".to_string(),
            );
        } else {
            let left = db.type_of(lhs.into());
            let right = db.type_of(rhs.into());

            // Check if types are assignable
            // Leave error types as "always assignable"
            if !ty::rules::is_assignable(db, left, right) {
                // Invalid types!
                let body = self.library.body(in_body);
                let left_span = body.expr(item.lhs).span.lookup_in(&self.library.span_map);
                let right_span = body.expr(item.rhs).span.lookup_in(&self.library.span_map);

                self.report_mismatched_assign_tys(left, right, asn_span, left_span, right_span);
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
        let left_ty = left.in_db(db);
        let right_ty = right.in_db(db);

        self.state()
            .reporter
            .error_detailed("mismatched types", asn_span)
            .with_note(
                format!("this is of type `{right}`", right = right_ty),
                right_span,
            )
            .with_note(
                format!("this is of type `{left}`", left = left_ty),
                left_span,
            )
            .with_info(format!(
                "`{right}` is not assignable into `{left}`",
                left = left_ty.peel_aliases(),
                right = right_ty.peel_aliases()
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
                .in_db(self.db);

            if !self.is_text_io_item(put_ty.id()) {
                continue;
            }

            // Only the following are allowed to have precision & exponent options:
            // - Int
            // - Nat
            // - Real
            if !put_ty.kind().is_number() {
                let item_span = body.expr(item.expr).span.lookup_in(&self.library.span_map);
                if let Some(expr) = item.opts.precision() {
                    let span = body.expr(expr).span.lookup_in(&self.library.span_map);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            format!("cannot specify fraction width for `{}`", put_ty),
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
                            format!("cannot specify exponent width for `{}`", put_ty),
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

            if !db
                .binding_kind((self.library_id, body_expr).into())
                .map(BindingKind::is_ref_mut)
                .unwrap_or(false)
            {
                let get_item_span = body.expr(item.expr).span.lookup_in(&self.library.span_map);

                self.report_mismatched_binding(
                    BindingKind::Storage(Mutability::Var),
                    (self.library_id, body_expr).into(),
                    get_item_span,
                    get_item_span,
                    |name| format!("cannot assign into `{name}`"),
                    || "cannot assign into expression".to_string(),
                );
            }

            if !self.is_text_io_item(ty.id()) {
                continue;
            }

            // Verify that the item type can use the given width
            match &item.width {
                // All item types can use tokens
                stmt::GetWidth::Token => {}
                // Only strings can use lines
                stmt::GetWidth::Line => {
                    if !ty.kind().is_sized_string() {
                        // This is one of the times where a HIR -> AST conversion is useful,
                        // as it allows us to get this span without having to lower it down
                        // to the HIR level.
                        //
                        // For now, we'll just lower the span of the get width
                        // FIXME: Use the span of the line width token
                        let item_span = self.library.body(body_id).expr(item.expr).span;
                        let item_span = self.library.lookup_span(item_span);

                        self.state()
                            .reporter
                            .error_detailed("invalid get option used", item_span)
                            .with_error(
                                format!("cannot specify line width for `{ty}`", ty = ty),
                                item_span,
                            )
                            .with_info("line width can only be specified for `string` types")
                            .finish();
                    }
                }
                // Only strings and charseqs can use exact widths
                stmt::GetWidth::Chars(expr) => {
                    if !ty.kind().is_sized_charseq() {
                        let opt_span = self.library.body(body_id).expr(*expr).span;
                        let item_span = self.library.body(body_id).expr(item.expr).span;

                        let opt_span = self.library.lookup_span(opt_span);
                        let item_span = self.library.lookup_span(item_span);

                        self.state()
                            .reporter
                            .error_detailed("invalid get option", opt_span)
                            .with_note(format!("cannot specify character width for `{ty}`", ty = ty), item_span)
                            .with_error("this is the invalid option", opt_span)
                            .with_info("character width can only be specified for `string` and `char(N)` types")
                            .finish();
                    }

                    self.check_text_io_arg(expr.in_body(body_id));
                }
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
        let ty_dat = ty.in_db(db);

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
        match ty_dat.kind() {
            ty::TypeKind::Error
            | ty::TypeKind::Forward // accept since it's like error
            | ty::TypeKind::Boolean
            | ty::TypeKind::Int(_)
            | ty::TypeKind::Nat(_)
            | ty::TypeKind::Real(_)
            | ty::TypeKind::Integer
            | ty::TypeKind::Char
            | ty::TypeKind::String
            | ty::TypeKind::CharN(_)
            | ty::TypeKind::StringN(_) => true,
            // Already de-aliased
            ty::TypeKind::Alias(_, _) => unreachable!(),
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
                let (start_ty, end_ty) = (start_ty.in_db(db), end_ty.in_db(db));

                let start_span = self.library.body(body_id).expr(start).span;
                let end_span = self.library.body(body_id).expr(end).span;

                let (start_span, end_span) = (
                    self.library.lookup_span(start_span),
                    self.library.lookup_span(end_span),
                );

                let bounds_span = start_span.cover(end_span);

                // Only report if both bounds are not `Missing`
                if let Some(bounds_span) = bounds_span {
                    let base_start = start_ty.clone().to_base_type();
                    let base_end = end_ty.clone().to_base_type();

                    if !ty::rules::is_either_coercible(db, base_start.id(), base_end.id()) {
                        // Bounds are not equivalent for our purposes
                        self.state()
                            .reporter
                            .error_detailed("mismatched types", bounds_span)
                            .with_note(format!("this is of type `{}`", end_ty), end_span)
                            .with_note(format!("this is of type `{}`", start_ty), start_span)
                            .with_error(
                                format!(
                                    "`{}` is not equivalent to `{}`",
                                    end_ty.peel_aliases(),
                                    start_ty.peel_aliases(),
                                ),
                                bounds_span,
                            )
                            .with_info("range bounds types must be equivalent")
                            .finish();
                    } else if !base_start.kind().is_index() || !base_end.kind().is_index() {
                        // Neither is an index type
                        let mut state = self.state();
                        let mut builder = state
                            .reporter
                            .error_detailed("mismatched types", bounds_span);

                        // Specialize when reporting different types
                        let start_ty = format!("`{}`", start_ty);
                        let end_ty = format!("`{}`", end_ty);

                        builder = if start_ty != end_ty {
                            builder
                                .with_note(format!("this is of type {}", end_ty), end_span)
                                .with_note(format!("this is of type {}", start_ty), start_span)
                        } else {
                            builder
                                .with_note(format!("this is of type {}", end_ty), end_span)
                                .with_note(format!("this is also of type {}", start_ty), start_span)
                        };

                        builder.with_error("expected index types", bounds_span).with_info(format!(
                            "range bounds types must both be index types (an integer, `{boolean}`, `{chr}`, enumerated type, or a range)",
                            boolean = ty::TypeKind::Boolean.prefix(),
                            chr = ty::TypeKind::Char.prefix()
                        ), )
                        .finish();
                    }
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

        let discrim_display = db
            .type_of((self.library_id, body_id, stmt.discriminant).into())
            .in_db(db);
        let discrim_ty = discrim_display.clone().to_base_type();
        let discrim_span = self.library.body(body_id).expr(stmt.discriminant).span;
        let discrim_span = self.library.lookup_span(discrim_span);

        // Check discriminant type
        if !(discrim_ty.kind().is_error()
            || discrim_ty.kind().is_index()
            || matches!(discrim_ty.kind(), ty::TypeKind::String))
        {
            self.state()
                .reporter
                .error_detailed("mismatched types", discrim_span)
                .with_error(
                    format!(
                        "`{}` cannot be used as a case discriminant",
                        discrim_display
                    ),
                    discrim_span,
                )
                .with_info(format!(
                    "`case` discriminant must be either \
                    an index type (an integer, `{boolean}`, `{chr}`, enumerated type, or a range), \
                    or a `{string}`",
                    boolean = ty::TypeKind::Boolean.prefix(),
                    chr = ty::TypeKind::Char.prefix(),
                    string = ty::TypeKind::String.prefix()
                ))
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
            let selector_ty = db.type_of((self.library_id, body_id, selector).into());
            let selector_span = self.library.body(body_id).expr(selector).span;
            let selector_span = self.library.lookup_span(selector_span);

            // Must match discriminant type
            if !ty::rules::is_coercible_into(db, discrim_ty.id(), selector_ty) {
                let selector_ty = selector_ty.in_db(db);

                self.state()
                    .reporter
                    .error_detailed("mismatched types", selector_span)
                    .with_note(
                        format!("discriminant is of type `{}`", discrim_display),
                        discrim_span,
                    )
                    .with_note(
                        format!("selector is of type `{}`", selector_ty),
                        selector_span,
                    )
                    .with_error(
                        format!(
                            "`{}` is not a `{}`",
                            selector_ty.clone().peel_aliases(),
                            discrim_display.clone().peel_aliases()
                        ),
                        selector_span,
                    )
                    .with_info("selector type must match discriminant type")
                    .finish();
            }

            // Must be a compile time expression
            let res = db.evaluate_const(
                Const::from_expr(self.library_id, expr::BodyExpr(body_id, selector)),
                Default::default(),
            );

            match res {
                Err(err) => {
                    err.report_to(db, &mut self.state().reporter);
                }
                Ok(ConstValue::String(s)) if matches!(discrim_ty.kind(), ty::TypeKind::Char) => {
                    // Check that it's a length 1 string
                    if s.len() != 1 {
                        let selector_ty = selector_ty.in_db(db);

                        self.state()
                            .reporter
                            .error_detailed("mismatched types", selector_span)
                            .with_note(
                                format!("this is of type `{}`, of length {}", selector_ty, s.len()),
                                selector_span,
                            )
                            .with_note(
                                format!("discriminant is of type `{}`", discrim_display),
                                discrim_span,
                            )
                            .with_info(format!(
                                "`{}` only allows `{}` or `{}`s of length 1",
                                discrim_ty,
                                ty::TypeKind::Char.prefix(),
                                ty::TypeKind::String.prefix()
                            ))
                            .finish();
                    }
                }
                _ => (),
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

    fn typeck_name_expr(&self, id: expr::BodyExpr, expr: &expr::Name) {
        match expr {
            expr::Name::Name(def_id) => {
                // Validate it's a ref to a storage location
                let binding_kind = self
                    .db
                    .binding_kind(DefId(self.library_id, *def_id).into())
                    .expect("undecl defs are bindings");

                if !binding_kind.is_ref() {
                    let span = self.library.body(id.0).expr(id.1).span;
                    let span = self.library.lookup_span(span);
                    let name = self.library.local_def(*def_id).name.item();
                    let reason = match binding_kind {
                        BindingKind::Undeclared | BindingKind::Storage(_) => unreachable!(),
                        BindingKind::Type => format!("`{}` is a reference to a type", name),
                        BindingKind::Module => {
                            format!("`{}` is a reference to a module", name)
                        }
                    };

                    self.state().reporter.error(
                        format!("cannot use `{}` as an expression", name),
                        reason,
                        span,
                    );
                }
            }
            expr::Name::Self_ => todo!(),
        }
    }

    fn typeck_primitive(&self, id: toc_hir::ty::TypeId, ty_node: &toc_hir::ty::Primitive) {
        let db = self.db;
        let ty = self
            .db
            .from_hir_type(id.in_library(self.library_id))
            .in_db(db);

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
        let (seq_size, size_limit) = match ty.kind() {
            ty::TypeKind::CharN(seq_size @ ty::SeqSize::Fixed(_)) => {
                // Note: 32768 is the minimum defined limit for the length on `n` for char(N)
                // ???: Do we want to add a config/feature option to change this?
                (seq_size, ty::MAX_CHAR_N_LEN)
            }
            ty::TypeKind::StringN(seq_size @ ty::SeqSize::Fixed(_)) => {
                // 256 is the maximum defined limit for the length on `n` for string(N),
                // so no option of changing that (unless we have control over the interpreter code).
                // - Legacy interpreter has the assumption baked in that the max length of a string is 256,
                //   so we can't change it yet unless we use a new interpreter.
                (seq_size, ty::MAX_STRING_LEN)
            }
            // because of hir disambiguation above
            _ => unreachable!(),
        };

        let int = match seq_size.fixed_len(db, expr_span) {
            Ok(v) => v,
            Err(ty::NotFixedLen::DynSize) => return, // dynamic, doesn't need checking
            Err(ty::NotFixedLen::ConstError(err)) => {
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
                .with_error(format!("computed count is {}", int), expr_span)
                .with_info(format!("valid sizes are between 1 to {}", size_limit - 1))
                .finish();
        }
    }

    fn typeck_alias(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Alias) {
        let def_id = ty.0;
        let target = DefId(self.library_id, def_id);
        let binding_kind = self
            .db
            .binding_kind(target.into())
            .expect("undecl defs are bindings");

        if !binding_kind.is_type() {
            let span = self.library.lookup_type(id).span;
            let span = self.library.lookup_span(span);

            self.report_mismatched_binding(
                BindingKind::Type,
                target.into(),
                span,
                span,
                |name| format!("cannot use `{name}` as a type alias"),
                || unreachable!("never expr"),
            );
        }
    }

    fn report_mismatched_binding(
        &self,
        expected: BindingKind,
        binding_source: toc_hir_db::db::BindingSource,
        report_at: Span,
        binding_span: Span,
        from_def: impl FnOnce(&str) -> String,
        from_expr: impl FnOnce() -> String,
    ) {
        use toc_hir_db::db::BindingSource;

        // Looks like:
        // cannot use `{name}` as {expected_thing} -> from_def
        // `{name}` is a reference to {thing}, not {expected}
        // `{name}` defined here
        //
        // or
        //
        // cannot use expression as {expected_thing} -> from_expr
        // expression is not a reference to {thing}

        // Lookup the actual referred to binding
        // This pokes through reference exprs and gets the target binding
        //
        // (e.g. if we're passed an expr::Name, we defer to the def_id binding source)
        let binding_source = self
            .db
            .binding_to(binding_source)
            .map_or(binding_source, BindingSource::DefId);

        match binding_source {
            BindingSource::DefId(DefId(lib_id, local_def)) => {
                let library = self.db.library(lib_id);
                let def_info = library.local_def(local_def);

                let name = def_info.name.item().as_str();
                let def_at = library.lookup_span(def_info.name.span());

                let binding_kind = self
                    .db
                    .binding_kind(binding_source)
                    .expect("taken from a def_id");

                self.state()
                    .reporter
                    .error_detailed(from_def(name), report_at)
                    .with_error(
                        format!("`{name}` is a reference to {binding_kind}, not {expected}"),
                        binding_span,
                    )
                    .with_note(format!("`{name}` declared here"), def_at)
                    .finish();
            }
            BindingSource::Body(..) | BindingSource::BodyExpr(..) => self
                .state()
                .reporter
                .error_detailed(from_expr(), report_at)
                .with_error(format!("not a reference to {expected}"), binding_span)
                .finish(),
        }
    }

    // TODO: Replace `expect_*_type` with `expect_type` once we have `ty::rules::is_equivalent`

    fn expect_integer_type(&self, type_id: ty::TypeId, span: Span) {
        let ty = type_id.in_db(self.db);

        if !ty.kind().is_integer() && !ty.kind().is_error() {
            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_note(format!("this is of type `{}`", ty), span)
                .with_info(format!("`{}` is not an integer type", ty))
                .finish();
        }
    }

    fn expect_boolean_type(&self, type_id: ty::TypeId, span: Span) {
        let ty = type_id.in_db(self.db);
        let expected_ty = self.db.mk_boolean().in_db(self.db);

        if !ty.kind().is_boolean() && !ty.kind().is_error() {
            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_note(format!("this is of type `{}`", ty), span)
                .with_info(format!("expected a `{}` type", expected_ty.kind().prefix()))
                .finish();
        }
    }

    fn require_resolved_type(&self, ty: toc_hir::ty::TypeId) {
        let ty_ref = self
            .db
            .from_hir_type(ty.in_library(self.library_id))
            .in_db(self.db);

        if let ty::TypeKind::Alias(def_id, to_ty) = ty_ref.kind() {
            if to_ty.in_db(self.db).kind().is_forward() {
                let ty_span = self.library.lookup_type(ty).span;
                let ty_span = self.library.lookup_span(ty_span);

                let def_library = self.db.library(def_id.0);
                let name = def_library.local_def(def_id.1).name.item();

                self.state().reporter.error(
                    format!("`{}` has not been resolved at this point", name),
                    format!("`{}` is required to be resolved at this point", name),
                    ty_span,
                );
            }
        }
    }
}
