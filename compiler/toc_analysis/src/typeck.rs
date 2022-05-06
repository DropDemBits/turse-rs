//! Type checking

#[cfg(test)]
mod test;

use std::cell::RefCell;

use toc_hir::{
    body,
    expr::{self, BodyExpr},
    item,
    library::{self, LibraryId, WrapInLibrary},
    stmt,
    stmt::BodyStmt,
    symbol::{BindingResultExt, BindingTo, DefId, Mutability, NotBinding, SubprogramKind},
};
use toc_reporting::CompileResult;
use toc_span::Span;

use crate::{
    const_eval,
    ty::db::{NotValueErrExt, ValueKind},
};
use crate::{
    const_eval::{Const, ConstValue},
    db::HirAnalysis,
    ty,
};

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

    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        self.typeck_subprogram_decl(id, item);
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

    fn visit_call_stmt(&self, id: BodyStmt, stmt: &stmt::Call) {
        self.typeck_call_stmt(id.0, stmt);
    }

    fn visit_return_stmt(&self, id: BodyStmt, stmt: &stmt::Return) {
        self.typeck_return_stmt(id, stmt);
    }

    fn visit_result_stmt(&self, id: BodyStmt, stmt: &stmt::Result) {
        self.typeck_result_stmt(id, stmt);
    }

    fn visit_binary(&self, id: BodyExpr, expr: &toc_hir::expr::Binary) {
        self.typeck_binary(id.0, expr);
    }

    fn visit_unary(&self, id: BodyExpr, expr: &toc_hir::expr::Unary) {
        self.typeck_unary(id.0, expr);
    }

    // FIXME: typecheck end-relative range exprs

    fn visit_field(&self, id: BodyExpr, expr: &expr::Field) {
        self.typeck_field_expr(id, expr)
    }

    fn visit_deref(&self, id: BodyExpr, expr: &expr::Deref) {
        self.typeck_deref_expr(id, expr)
    }

    fn visit_call_expr(&self, id: BodyExpr, expr: &expr::Call) {
        self.typeck_call_expr(id, expr);
    }

    fn visit_primitive(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Primitive) {
        self.typeck_primitive(id, ty);
    }

    fn visit_alias(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Alias) {
        self.typeck_alias(id, ty);
    }

    fn visit_constrained(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Constrained) {
        self.typeck_constrained_ty(id, ty);
    }

    fn visit_array(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Array) {
        self.typeck_array_ty(id, ty);
    }

    fn visit_set(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Set) {
        self.typeck_set_ty(id, ty);
    }

    fn visit_pointer(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Pointer) {
        self.typeck_pointer_ty(id, ty);
    }

    fn visit_subprogram_ty(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Subprogram) {
        self.typeck_subprogram_ty(id, ty);
    }
}

impl TypeCheck<'_> {
    fn typeck_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        if let Some(ty_spec) = item.type_spec {
            // Type spec must be resolved at this point
            self.require_resolved_type(ty_spec);

            // Type must not be any-sized charseq, unsized, or non-positive sized
            self.require_known_positive_size(ty_spec, || {
                let maybe_const = match item.mutability {
                    Mutability::Const => "const",
                    Mutability::Var => "var",
                };

                format!("`{maybe_const}` declarations")
            });
        }

        if let Some(init_body) = item.init_expr {
            self.expect_expression((self.library_id, init_body).into());
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

        // Peel opaque just for assignability
        let in_module = self.db.inside_module((self.library_id, id).into());
        let left = left.in_db(self.db).peel_opaque(in_module).id();

        if !ty::rules::is_assignable(self.db, left, right) {
            // Incompatible, report it
            let init_span = self.library.body(init).span.lookup_in(&self.library);
            let spec_span = self
                .library
                .lookup_type(ty_spec)
                .span
                .lookup_in(&self.library);

            self.report_mismatched_assign_tys(left, right, init_span, spec_span, init_span, None);

            // Don't need to worry about ConstValue being anything,
            // since that should be handled by const eval type restrictions
            // However, there should still be an assert here
            // TODO: Add assert ensuring there is no valid ConstValue
        }
    }

    fn typeck_type_decl(&self, _id: item::ItemId, item: &item::Type) {
        if let item::DefinedType::Alias(ty) = &item.type_def {
            // Must be resoled & positive
            self.require_resolved_type(*ty);
            self.require_positive_size(*ty, || "`type` declarations".into());
        }
    }

    fn typeck_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        let db = self.db;
        let bind_span = self.library.item(id).span;
        let bind_span = bind_span.lookup_in(&self.library);

        let lib_id = self.library_id;
        let bind_to = (lib_id, item.bind_to);

        // Require that we're binding to (mutable) storage
        let expected_value = ExpectedValue::NonRegisterRef(item.mutability);
        let value_kind = db.value_produced(bind_to.into());
        let is_register = matches!(value_kind, Ok(ValueKind::Register(_)));

        let from = self.library.local_def(item.def_id).name;
        let bind_to_span = self
            .library
            .body(item.bind_to)
            .span
            .lookup_in(&self.library);

        self.expect_value_kind(
            expected_value,
            bind_to.into(),
            bind_span,
            bind_to_span,
            |thing| format!("cannot bind `{from}` to {thing}"),
            is_register
                .then(|| "registers don't have a location in memory, so they cannot be bound to"),
        );
    }

    fn typeck_subprogram_decl(&self, _id: item::ItemId, item: &item::Subprogram) {
        let in_where = || {
            let kind = match item.kind {
                SubprogramKind::Procedure => "procedure",
                SubprogramKind::Function => "function",
                SubprogramKind::Process => "process",
            };
            format!("`{kind}` declarations")
        };

        self.require_known_positive_size(item.result.ty, &in_where);

        if let Some(param_list) = &item.param_list {
            for param in &param_list.tys {
                self.require_positive_size(param.param_ty, &in_where);
            }
        }

        match item.extra {
            item::SubprogramExtra::None => {}
            item::SubprogramExtra::DeviceSpec(body_id)
            | item::SubprogramExtra::StackSize(body_id) => {
                self.expect_integer_value((self.library_id, body_id).into());
            }
        }
    }

    fn typeck_assign(&self, in_body: body::BodyId, item: &stmt::Assign) {
        let lib_id = self.library_id;
        let db = self.db;
        let asn_span = item.asn.lookup_in(&self.library);

        let lhs = (lib_id, in_body, item.lhs);
        let rhs = (lib_id, in_body, item.rhs);

        // Check if we're assigning into a mut ref, and from a value
        let is_lhs_ref_mut = {
            let left_span = self
                .library
                .body(in_body)
                .expr(item.lhs)
                .span
                .lookup_in(&self.library);

            self.expect_value_kind(
                ExpectedValue::Ref(Mutability::Var),
                lhs.into(),
                asn_span,
                left_span,
                |thing| format!("cannot assign into {thing}"),
                None,
            )
        };
        let is_rhs_value = self.expect_expression((self.library_id, in_body, item.rhs).into());

        // Only report type mismatches if it's from the correct values
        if is_lhs_ref_mut && is_rhs_value {
            let left = db.type_of(lhs.into());
            let right = db.type_of(rhs.into());

            // Check if types are assignable
            // Leave error types as "always assignable"
            if !ty::rules::is_assignable(db, left, right) {
                // Invalid types!
                let body = self.library.body(in_body);
                let left_span = body.expr(item.lhs).span.lookup_in(&self.library);
                let right_span = body.expr(item.rhs).span.lookup_in(&self.library);

                self.report_mismatched_assign_tys(
                    left, right, asn_span, left_span, right_span, None,
                );
            }
        }
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
            let put_tyref = self
                .db
                .type_of((self.library_id, body_id, item.expr).into())
                .in_db(self.db);
            let put_base_tyref = put_tyref.clone().to_base_type();
            let item_span = body.expr(item.expr).span.lookup_in(&self.library);

            if !self.expect_expression((self.library_id, body_id, item.expr).into()) {
                continue;
            }

            let is_item = self.expect_text_io_item(put_base_tyref.id(), || {
                self.state().reporter.error(
                    "invalid put type",
                    format!("cannot put a value of `{put_tyref}`"),
                    item_span,
                );
            });

            if !is_item {
                continue;
            }

            // Only the following are allowed to have precision & exponent options:
            // - Int
            // - Nat
            // - Real
            if !put_base_tyref.kind().is_number() {
                if let Some(expr) = item.opts.precision() {
                    let span = body.expr(expr).span.lookup_in(&self.library);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            format!("cannot specify fraction width for `{put_tyref}`"),
                            item_span,
                        )
                        .with_error("this is the invalid option", span)
                        .with_info("fraction width can only be specified for numeric put types")
                        .finish();
                }

                if let Some(expr) = item.opts.exponent_width() {
                    let span = body.expr(expr).span.lookup_in(&self.library);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note(
                            format!("cannot specify exponent width for `{put_tyref}`"),
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
            let base_ty = ty.clone().to_base_type();
            let item_span = body.expr(item.expr).span.lookup_in(&self.library);

            if !self.expect_value_kind(
                ExpectedValue::Ref(Mutability::Var),
                (self.library_id, body_expr).into(),
                item_span,
                item_span,
                |thing| format!("cannot assign into {thing}"),
                None,
            ) {
                // Can't assign into item
                continue;
            }

            let is_item = self.expect_text_io_item(ty.id(), || {
                self.state().reporter.error(
                    "invalid get type",
                    format!("cannot get a value of `{ty}`"),
                    item_span,
                );
            });

            if !is_item {
                continue;
            }

            // Verify that the item type can use the given width
            match &item.width {
                // All item types can use tokens
                stmt::GetWidth::Token => {}
                // Only strings can use lines
                stmt::GetWidth::Line => {
                    if !base_ty.kind().is_sized_string() {
                        // This is one of the times where a HIR -> AST conversion is useful,
                        // as it allows us to get this span without having to lower it down
                        // to the HIR level.
                        //
                        // For now, we'll just lower the span of the get width
                        // FIXME: Use the span of the line width token
                        let item_span = self.library.body(body_id).expr(item.expr).span;
                        let item_span = item_span.lookup_in(&self.library);

                        self.state()
                            .reporter
                            .error_detailed("invalid get option used", item_span)
                            .with_error(format!("cannot specify line width for `{ty}`"), item_span)
                            .with_info("line width can only be specified for `string` types")
                            .finish();
                    }
                }
                // Only strings and charseqs can use exact widths
                stmt::GetWidth::Chars(expr) => {
                    if !base_ty.kind().is_sized_charseq() {
                        let opt_span = self.library.body(body_id).expr(*expr).span;
                        let opt_span = opt_span.lookup_in(&self.library);

                        self.state()
                            .reporter
                            .error_detailed("invalid get option", opt_span)
                            .with_note(format!("cannot specify character width for `{ty}`"), item_span)
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
        self.expect_integer_value((self.library_id, expr).into());
    }

    fn expect_text_io_item(&self, ty: ty::TypeId, not_ty: impl FnOnce()) -> bool {
        let db = self.db;
        let ty_dat = ty.in_db(db).to_base_type();

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

        if ty_dat.kind().is_printable() {
            return true;
        } else if !matches!(ty_dat.kind(), ty::TypeKind::Error | ty::TypeKind::Forward) {
            debug_assert!(!matches!(ty_dat.kind(), ty::TypeKind::Alias(..)));
            not_ty()
        }

        false
    }

    fn typeck_for(&self, body_id: body::BodyId, stmt: &stmt::For) {
        let db = self.db;

        // Check step by first so that we can bail bounds typeck early
        if let Some(step_by) = stmt.step_by {
            // `step_by` must evaluate to an integer type
            // ???: Does this make sense for other range types?
            self.expect_integer_value((self.library_id, body_id, step_by).into());
        }

        match stmt.bounds {
            stmt::ForBounds::Implicit(expr) => {
                // Bounds implied from the expr

                // Cases:
                // - plain expr
                //   - iterable (i.e. arrays)
                //   - not iterable
                // - ty alias
                //   - index type (but not integer)
                //   - integer type
                //   - other
                // - other
                //   - error!

                let bounds_expr = (self.library_id, body_id, expr);
                let bounds_span = self
                    .library
                    .body(body_id)
                    .expr(expr)
                    .span
                    .lookup_in(&self.library);
                let in_module = db.inside_module(bounds_expr.into());

                if db
                    .value_produced(bounds_expr.into())
                    .map(ValueKind::is_value)
                    .or_missing()
                {
                    // for-each loop
                    // These are not supported yet, until after 0.1 tagging
                    let bounds_ty = db.type_of(bounds_expr.into());
                    let bounds_tyref = bounds_ty.in_db(db).peel_opaque(in_module);
                    let bounds_base_ty = bounds_tyref.clone().peel_aliases();

                    // FIXME(for-each): Specialize message for iterables
                    self.state()
                        .reporter
                        .error_detailed("mismatched types", bounds_span)
                        .with_note(format!("this is of type `{bounds_tyref}`"), bounds_span)
                        .with_error(format!("`{bounds_base_ty}` is not iterable"), bounds_span)
                        .with_info("only arrays types can be iterated over")
                        .finish();

                    return;
                }

                // Must be a type alias
                let binding_def = if let Some(def_id) = db.binding_def(bounds_expr.into()) {
                    def_id
                } else {
                    return;
                };

                if !db
                    .binding_to(binding_def.into())
                    .map(BindingTo::is_type)
                    .or_missing()
                {
                    self.report_mismatched_binding(
                        BindingTo::Type,
                        binding_def.into(),
                        bounds_span,
                        bounds_span,
                        |thing| format!("cannot use {thing} as a `for` bound"),
                        None,
                    );
                    return;
                }

                let bounds_ty = db.type_of(binding_def.into());
                let bounds_tyref = bounds_ty.in_db(db).peel_opaque(in_module);
                let bounds_base_ty = bounds_tyref.clone().to_base_type();

                // Should be an index type
                if !bounds_base_ty.kind().is_index() {
                    self.state()
                        .reporter
                        .error_detailed("mismatched types", bounds_span)
                        .with_note(format!("this is of type `{bounds_tyref}`"), bounds_span)
                        .with_error(
                            format!("`{bounds_base_ty}` is not an index type"),
                            bounds_span,
                        ).with_info(
                            "range bound type must be an index type (an integer, `boolean`, `char`, enumerated type, or a range)",
                        )
                        .finish();
                } else if bounds_tyref.clone().peel_aliases().kind().is_integer() {
                    // Is an integer type, but not from a range
                    // Range would be too big
                    self.state()
                        .reporter
                        .error_detailed("bound range is too large", bounds_span)
                        .with_error(
                            format!("a range over all `{bounds_tyref}` values is too large"),
                            bounds_span,
                        )
                        .with_info("use a range type to shrink the range of the bound")
                        .finish()
                }
            }
            stmt::ForBounds::Full(start, end) => {
                // Must both be values
                let is_start_value =
                    self.expect_expression((self.library_id, body_id, start).into());
                let is_end_value = self.expect_expression((self.library_id, body_id, end).into());

                // Must both be index types
                let start_ty = db.type_of((self.library_id, body_id, start).into());
                let end_ty = db.type_of((self.library_id, body_id, end).into());

                // Wrap up both types
                let (start_ty, end_ty) = (start_ty.in_db(db), end_ty.in_db(db));

                let start_span = self.library.body(body_id).expr(start).span;
                let end_span = self.library.body(body_id).expr(end).span;

                let (start_span, end_span) = (
                    start_span.lookup_in(&self.library),
                    end_span.lookup_in(&self.library),
                );

                let bounds_span = start_span.cover(end_span);

                if !(is_start_value && is_end_value) {
                    // Only report on types if they're both values
                } else if let Some(bounds_span) = bounds_span {
                    // Only report if both bounds are not `Missing`
                    let base_start = start_ty.clone().to_base_type();
                    let base_end = end_ty.clone().to_base_type();

                    if !ty::rules::is_either_coercible(db, base_start.id(), base_end.id()) {
                        // Bounds are not equivalent for our purposes
                        self.state()
                            .reporter
                            .error_detailed("mismatched types", bounds_span)
                            .with_note(format!("this is of type `{end_ty}`"), end_span)
                            .with_note(format!("this is of type `{start_ty}`"), start_span)
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
                        let start_ty = format!("`{start_ty}`");
                        let end_ty = format!("`{end_ty}`");

                        builder = if start_ty != end_ty {
                            builder
                                .with_note(format!("this is of type {end_ty}"), end_span)
                                .with_note(format!("this is of type {start_ty}"), start_span)
                        } else {
                            builder
                                .with_note(format!("this is of type {end_ty}"), end_span)
                                .with_note(format!("this is also of type {start_ty}"), start_span)
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
    }

    fn typeck_exit(&self, body_id: body::BodyId, stmt: &stmt::Exit) {
        if let Some(condition_expr) = stmt.when_condition {
            self.expect_boolean_value((self.library_id, body_id, condition_expr).into());
        }
    }

    fn typeck_if(&self, body_id: body::BodyId, stmt: &stmt::If) {
        self.expect_boolean_value((self.library_id, body_id, stmt.condition).into());
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
            .in_db(db);
        let discrim_base_ty = discrim_ty.clone().to_base_type();
        let discrim_span = self.library.body(body_id).expr(stmt.discriminant).span;
        let discrim_span = discrim_span.lookup_in(&self.library);

        if self.expect_expression((self.library_id, body_id, stmt.discriminant).into()) {
            // Check discriminant type
            if !(discrim_base_ty.kind().is_error()
                || discrim_base_ty.kind().is_index()
                || matches!(discrim_base_ty.kind(), ty::TypeKind::String))
            {
                self.state()
                    .reporter
                    .error_detailed("mismatched types", discrim_span)
                    .with_error(
                        format!("`{discrim_ty}` cannot be used as a case discriminant"),
                        discrim_span,
                    )
                    .with_info(
                        "`case` discriminant must be either \
                    an index type (an integer, `boolean`, `char`, enumerated type, or a range), \
                    or a `string`",
                    )
                    .finish();
            }
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
            let selector_span = selector_span.lookup_in(&self.library);

            // Must match discriminant type
            if !ty::rules::is_coercible_into(db, discrim_base_ty.id(), selector_ty) {
                let selector_ty = selector_ty.in_db(db);

                self.state()
                    .reporter
                    .error_detailed("mismatched types", selector_span)
                    .with_note(
                        format!("discriminant is of type `{discrim_ty}`"),
                        discrim_span,
                    )
                    .with_note(
                        format!("selector is of type `{selector_ty}`"),
                        selector_span,
                    )
                    .with_error(
                        format!(
                            "`{}` is not a `{}`",
                            selector_ty.clone().peel_aliases(),
                            discrim_ty.clone().peel_aliases()
                        ),
                        selector_span,
                    )
                    .with_info("selector type must match discriminant type")
                    .finish();
            }

            // Must be a value
            if !self.expect_expression((self.library_id, body_id, selector).into()) {
                continue;
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
                Ok(ConstValue::String(s))
                    if matches!(discrim_base_ty.kind(), ty::TypeKind::Char) =>
                {
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
                                format!("discriminant is of type `{discrim_ty}`"),
                                discrim_span,
                            )
                            .with_info(format!(
                                "`{discrim_base_ty}` only allows `char` or `string`s of length 1",
                            ))
                            .finish();
                    }
                }
                _ => (),
            }
        }
    }

    fn typeck_call_stmt(&self, body: body::BodyId, stmt: &stmt::Call) {
        self.typeck_call(body, stmt.lhs, stmt.arguments.as_ref(), false);
    }

    fn typeck_return_stmt(&self, id: stmt::BodyStmt, _stmt: &stmt::Return) {
        // Verify that we're in the correct statement
        let db = self.db;
        let body = id.0;
        let span = self.library.body(id.0).stmt(id.1).span;
        let span = span.lookup_in(&self.library);

        let result_ty = db.type_of((self.library_id, body).into());
        let result_ty_ref = result_ty.in_db(db).to_base_type();

        if result_ty_ref.kind().is_error() {
            self.state()
            .reporter
            .error("cannot use `return` here", "`return` statement is only allowed in subprogram bodies and module-kind declarations", span);
        } else if !matches!(result_ty_ref.kind(), ty::TypeKind::Void) {
            // Inside of function
            self.state().reporter.error(
                "cannot use `return` here",
                "`result` statement is used to return values in function bodies",
                span,
            );
        }
    }

    fn typeck_result_stmt(&self, id: stmt::BodyStmt, stmt: &stmt::Result) {
        // Verify matching result types
        let db = self.db;
        let body = id.0;
        let span = self.library.body(id.0).stmt(id.1).span;
        let span = span.lookup_in(&self.library);

        let result_ty = if let Some(owner) = self.db.body_owner(body.in_library(self.library_id)) {
            match owner {
                body::BodyOwner::Item(item) => {
                    let item = self.library.item(item);

                    // FIXME: Use the type from the `body`
                    // We currently need this route because we also need the span of the result type
                    if let item::ItemKind::Subprogram(subprog) = &item.kind {
                        Some(subprog.result.ty)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } else {
            unreachable!()
        };

        if let Some(hir_ty) = result_ty {
            // Peel any opaques first
            let in_module = db.inside_module((self.library_id, hir_ty).into());
            let result_ty = db.from_hir_type(hir_ty.in_library(self.library_id));
            let result_ty_ref = result_ty.in_db(db).peel_opaque(in_module);

            let result_ty = result_ty_ref.id();
            let result_ty_ref = result_ty_ref.to_base_type();

            if matches!(result_ty_ref.kind(), ty::TypeKind::Void) {
                // Not inside of function
                self.state().reporter.error(
                    "cannot use `result` here",
                    "`result` statement is only allowed in function bodies",
                    span,
                );
            } else {
                let value_expr = (self.library_id, body, stmt.expr);
                let value_ty = db.type_of(value_expr.into());

                // Check value compatibility
                if !self.expect_expression((self.library_id, body, stmt.expr).into()) {
                    // Not a value (already reported)
                } else if !ty::rules::is_assignable(db, result_ty, value_ty) {
                    // Not assignable into the return type
                    let ty_span = self.library.lookup_type(hir_ty).span;
                    let ty_span = ty_span.lookup_in(&self.library);

                    let value_span = self.library.body(body).expr(stmt.expr).span;
                    let value_span = value_span.lookup_in(&self.library);

                    self.report_mismatched_assign_tys(
                        result_ty,
                        value_ty,
                        value_span,
                        ty_span,
                        value_span,
                        Some(|ty| format!("function expects type {ty}")),
                    );
                }
            }
        } else {
            self.state().reporter.error(
                "cannot use `result` here",
                "`result` statement is only allowed in function bodies",
                span,
            );
        }
    }

    fn typeck_binary(&self, body: body::BodyId, expr: &expr::Binary) {
        let db = self.db;
        let lib_id = self.library_id;
        let left = db.type_of((lib_id, body, expr.lhs).into());
        let right = db.type_of((lib_id, body, expr.rhs).into());

        if let Err(err) = ty::rules::check_binary_op_values(db, self.library_id, body, expr) {
            ty::rules::report_invalid_bin_values(db, err, &mut self.state().reporter);
        } else if let Err(err) = ty::rules::check_binary_op(db, left, *expr.op.item(), right) {
            let op_span = expr.op.span().lookup_in(&self.library);
            let body = self.library.body(body);
            let left_span = body.expr(expr.lhs).span.lookup_in(&self.library);
            let right_span = body.expr(expr.rhs).span.lookup_in(&self.library);

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

        if let Err(err) = ty::rules::check_unary_op_values(db, self.library_id, body, expr) {
            ty::rules::report_invalid_unary_value(db, err, &mut self.state().reporter);
        } else if let Err(err) = ty::rules::check_unary_op(db, *expr.op.item(), right) {
            let op_span = expr.op.span().lookup_in(&self.library);
            let body = self.library.body(body);
            let right_span = body.expr(expr.rhs).span.lookup_in(&self.library);

            ty::rules::report_invalid_unary_op(
                db,
                err,
                op_span,
                right_span,
                &mut self.state().reporter,
            );
        }
    }

    fn typeck_field_expr(&self, id: expr::BodyExpr, expr: &expr::Field) {
        let db = self.db;

        // FIXME: Bail if lhs is an error type
        // FIXME: Point `in here` span at lhs / its binding def

        if let Some(fields) = db.fields_of((self.library_id, id.0, expr.lhs).into()) {
            if fields.lookup(*expr.field.item()).is_none() {
                // not a field
                let field_name = expr.field.item();
                self.state().reporter.error(
                    format!("no field named `{field_name}` in expression"),
                    format!("no field named `{field_name}` in here"),
                    expr.field.span().lookup_in(&self.library),
                );
            }
        } else {
            // no fields
            let field_name = expr.field.item();
            self.state().reporter.error(
                format!("no field named `{field_name}` in expression"),
                format!("no field named `{field_name}` in here"),
                expr.field.span().lookup_in(&self.library),
            );
        }
    }

    fn typeck_deref_expr(&self, id: expr::BodyExpr, expr: &expr::Deref) {
        let db = self.db;
        let rhs_expr = id.with_expr(expr.rhs);

        let ty = db.type_of((self.library_id, rhs_expr).into());
        let base_ty_ref = ty.in_db(db).to_base_type();

        if !self.expect_expression((self.library_id, rhs_expr).into()) {
            // don't proceed to type matching
        } else if !base_ty_ref.kind().is_pointer() && !base_ty_ref.kind().is_error() {
            let ty_ref = ty.in_db(db);
            let rhs_span = self
                .library
                .body(rhs_expr.0)
                .expr(rhs_expr.1)
                .span
                .lookup_in(&self.library);

            self.state()
                .reporter
                .error_detailed("mismatched types", rhs_span)
                .with_note(format!("this is of type `{ty_ref}`"), rhs_span)
                .with_error(format!("`{base_ty_ref}` is not a pointer type"), rhs_span)
                .finish();
        }
    }

    fn typeck_call_expr(&self, id: expr::BodyExpr, expr: &expr::Call) {
        self.typeck_call(id.0, expr.lhs, Some(&expr.arguments), true);
    }

    fn typeck_call(
        &self,
        body: body::BodyId,
        lhs: expr::ExprId,
        arg_list: Option<&expr::ArgList>,
        require_value: bool,
    ) {
        let db = self.db;
        let lhs_expr = (self.library_id, body, lhs);
        let lhs_span = self.library.body(body).expr(lhs).span;
        let lhs_span = lhs_span.lookup_in(&self.library);

        // Fetch type of lhs
        // Always try to do it by `DefId` first, so that we can properly support paren-less functions
        // We still need to defer to expression type lookup, since things like `expr::Deref` can produce
        // references to subprograms.
        let (lhs_ty, from_ty_binding) = if let Some(def_id) = db.binding_def(lhs_expr.into()) {
            // From an item
            let is_ty_binding = db
                .binding_to(def_id.into())
                .map(|a| a.is_type())
                .or_missing();
            (db.type_of(def_id.into()), is_ty_binding)
        } else {
            // From an actual expression
            (db.type_of(lhs_expr.into()), false)
        };
        let in_module = db.inside_module(lhs_expr.into());
        let lhs_tyref = lhs_ty.in_db(db).peel_opaque(in_module).to_base_type();

        // Bail on error types
        if lhs_tyref.kind().is_error() {
            return;
        }

        enum CallKind {
            SubprogramCall,
            SetCons(ty::TypeId),
        }

        // Check if lhs is callable
        let has_parens = arg_list.is_some();
        let call_kind = match lhs_tyref.kind() {
            ty::TypeKind::Subprogram(SubprogramKind::Process, ..) => None,
            // Parens are only potentially optional in subprograms
            ty::TypeKind::Subprogram(..) if !from_ty_binding => Some(CallKind::SubprogramCall),
            // All the other kinds require parens
            ty::TypeKind::Set(_, elem_ty) if has_parens && from_ty_binding => {
                Some(CallKind::SetCons(*elem_ty))
            }
            _ => None,
        };

        let call_kind = if let Some(kind) = call_kind {
            kind
        } else {
            // can't call expression
            let full_lhs_tyref = lhs_ty.in_db(db);
            let thing = match self.db.binding_def(lhs_expr.into()) {
                Some(def_id) => {
                    let library = self.db.library(def_id.0);
                    let def_info = library.local_def(def_id.1);
                    let name = def_info.name;
                    format!("`{name}`")
                }
                None => "expression".to_string(),
            };
            let extra_info = match lhs_tyref.kind() {
                ty::TypeKind::Set(..) if has_parens && !from_ty_binding => {
                    // Trying to construct a set, but from a variable
                    Some("sets can only be constructed from their type names")
                }
                ty::TypeKind::Subprogram(SubprogramKind::Process, ..) => {
                    Some("to start a new process, use a `fork` statement")
                }
                _ => None,
            };

            if has_parens {
                // Trying to call this expression
                let mut state = self.state();
                let mut builder = state
                    .reporter
                    .error_detailed(format!("cannot call or subscript {thing}"), lhs_span)
                    .with_note(format!("this is of type `{full_lhs_tyref}`"), lhs_span)
                    .with_error(format!("`{lhs_tyref}` is not callable"), lhs_span);

                if let Some(extra_info) = extra_info {
                    builder = builder.with_info(extra_info);
                }

                builder.finish();
            } else {
                // Just the expression by itself
                // FIXME: Improve error wording for callable types
                self.state().reporter.error(
                    format!("cannot use {thing} as a statement"),
                    format!("{thing} is not a statement"),
                    lhs_span,
                );
            }

            return;
        };

        if !matches!(call_kind, CallKind::SubprogramCall) && !require_value {
            // In statement position, which only accepts subprogram calls
            // Pointer casts are chained as part of exprs, and aren't directly in stmt position
            // FIXME: Refer to {thing} instead of "thing"
            self.state().reporter.error(
                "cannot use expression as a statement",
                "this is not a function or procedure",
                lhs_span,
            );
        }

        match call_kind {
            CallKind::SetCons(elem_ty) => {
                self.typeck_call_set_cons(lhs_expr, lhs_span, arg_list, body, elem_ty);
            }
            CallKind::SubprogramCall => self.typeck_call_subprogram(
                lhs_tyref,
                lhs_expr,
                lhs_span,
                arg_list,
                body,
                require_value,
            ),
        }
    }

    fn typeck_call_set_cons(
        &self,
        lhs_expr: (LibraryId, body::BodyId, expr::ExprId),
        lhs_span: Span,
        arg_list: Option<&expr::ArgList>,
        body_id: body::BodyId,
        elem_ty: ty::TypeId,
    ) {
        let arg_list = if let Some(arg_list) = arg_list {
            arg_list
        } else {
            // already checked to have an arg list
            unreachable!()
        };

        let body = self.library.body(body_id);
        let has_all = arg_list
            .iter()
            .enumerate()
            .find(|(_, arg)| matches!(body.expr(**arg).kind, expr::ExprKind::All));

        if let Some((idx, all_arg)) = has_all {
            // No others args must be present

            // FIXME: Change reporting spans to covering `before_all` and `after_all`
            let (before_all, after_all) = arg_list.split_at(idx);
            let after_all = &after_all[1..]; // don't include the `all` arg

            if !before_all.is_empty() || !after_all.is_empty() {
                let all_span = self
                    .library
                    .body(body_id)
                    .expr(*all_arg)
                    .span
                    .lookup_in(&self.library);

                self.state()
                    .reporter
                    .error_detailed("constructor call has extra arguments", lhs_span)
                    .with_error("call has extra arguments", lhs_span)
                    .with_note("this `all` also covers the rest of the arguments", all_span)
                    .finish();
            }
        }

        // Peel the elem ty's opaque
        let in_module = self.db.inside_module(lhs_expr.into());
        let elem_ty = elem_ty.in_db(self.db).peel_opaque(in_module).id();

        // All args must be coercible into the element type
        for arg in arg_list {
            if !self.expect_expression((self.library_id, body_id, *arg).into()) {
                continue;
            }

            let arg_ty = self.db.type_of((self.library_id, body_id, *arg).into());
            let arg_span = self.library.body(body_id).expr(*arg).span;
            let arg_span = arg_span.lookup_in(&self.library);

            // Check that it isn't a range expr
            // ???: Supporting range exprs in set cons calls (not end relative)?
            match &self.library.body(body_id).expr(*arg).kind {
                expr::ExprKind::All => {} // valid in set constructors
                expr::ExprKind::Range(_) => {
                    self.state().reporter.error(
                        "cannot use range expression here",
                        "range expressions aren't supported in set constructors",
                        arg_span,
                    );
                    continue;
                }
                _ => {}
            }

            if !ty::rules::is_assignable(self.db, elem_ty, arg_ty) {
                self.report_mismatched_param_tys(
                    elem_ty,
                    arg_ty,
                    arg_span,
                    arg_span,
                    arg_span,
                    ty::PassBy::Value,
                );
            }
        }
    }

    fn typeck_call_subprogram(
        &self,
        lhs_tyref: ty::TyRef<dyn HirAnalysis>,
        lhs_expr: (LibraryId, body::BodyId, expr::ExprId),
        lhs_span: Span,
        arg_list: Option<&expr::ArgList>,
        body: body::BodyId,
        require_value: bool,
    ) {
        // Arg list match?
        // - Arg ty?
        // - Arg binding?
        // - Arg count?

        let (kind, param_list) = if let ty::TypeKind::Subprogram(kind, params, _) = lhs_tyref.kind()
        {
            debug_assert_ne!(*kind, SubprogramKind::Process);
            (*kind, params.as_ref())
        } else {
            // Already checked that it's callable, or that it's an error
            return;
        };

        // Check if parens are required
        if param_list.is_some() && arg_list.is_none() {
            // Just referencing it bare
            let thing = match self.db.binding_def(lhs_expr.into()) {
                Some(def_id) => {
                    let library = self.db.library(def_id.0);
                    let def_info = library.local_def(def_id.1);
                    let name = def_info.name;
                    format!("`{name}`")
                }
                None => "this expression".to_string(),
            };

            self.state().reporter.error(
                format!("cannot use {thing} as a statement"),
                format!("{thing} is callable, but requires adding `()` after here"),
                lhs_span,
            );
        }

        let (empty_params, empty_args); // extends lifetime of empty lists
        let (param_list, arg_list) = match (param_list, arg_list) {
            (None, None) => {
                // both bare
                return;
            }
            (None, Some(arg_list)) => {
                empty_params = vec![];
                (&empty_params, arg_list)
            }
            (Some(param_list), None) => {
                empty_args = vec![];
                (param_list, &empty_args)
            }
            (Some(param_list), Some(arg_list)) => (param_list, arg_list),
        };
        let (mut params, mut args) = (param_list.iter(), arg_list.iter());

        fn arguments(count: usize) -> &'static str {
            if count == 1 {
                "argument"
            } else {
                "arguments"
            }
        }

        // For peeling opaques
        let in_module = self.db.inside_module(lhs_expr.into());

        loop {
            let (param, arg) = match (params.next(), args.next()) {
                (None, None) => {
                    // exact
                    break;
                }
                (Some(_), None) | (None, Some(_)) => {
                    // too few
                    let param_count = param_list.len();
                    let arg_count = arg_list.len();
                    let args = arguments(param_count);

                    // FIXME: Find the last non-missing argument, and use that as the span
                    let mut state = self.state();
                    let mut builder = state.reporter.error_detailed(
                        format!("expected {param_count} {args}, found {arg_count}",),
                        lhs_span,
                    );

                    let difference = if param_count > arg_count {
                        param_count - arg_count
                    } else {
                        arg_count - param_count
                    };
                    let diff_arguments = arguments(difference);

                    builder = if param_count > arg_count {
                        builder.with_error(
                            format!("call is missing {difference} {diff_arguments}"),
                            lhs_span,
                        )
                    } else {
                        builder.with_error(
                            format!("call has {difference} extra {diff_arguments}"),
                            lhs_span,
                        )
                    };
                    builder.finish();

                    break;
                }
                (Some(param), Some(arg)) => (param, arg),
            };

            // Check type & binding
            let arg_expr = (self.library_id, body, *arg);
            let arg_ty = self.db.type_of(arg_expr.into());
            let arg_value = self.db.value_produced(arg_expr.into());
            let arg_span = self.library.body(body).expr(*arg).span;
            let arg_span = arg_span.lookup_in(&self.library);

            // Check that it isn't `all` or a range expr
            match &self.library.body(body).expr(*arg).kind {
                expr::ExprKind::All => {
                    self.state().reporter.error(
                        "cannot use `all` here",
                        "`all` can't be used in subprogram calls",
                        arg_span,
                    );
                    continue;
                }
                expr::ExprKind::Range(_) => {
                    self.state().reporter.error(
                        "cannot use range expression here",
                        "range expressions can't be used in subprogram calls",
                        arg_span,
                    );
                    continue;
                }
                _ => {}
            }

            let (matches_pass_by, mutability) = match param.pass_by {
                ty::PassBy::Value => {
                    // Accept any expressions
                    (
                        arg_value.map(ValueKind::is_value).or_missing(),
                        Mutability::Var,
                    )
                }
                ty::PassBy::Reference(mutability) => {
                    // Only accept storage locations
                    let predicate = match mutability {
                        Mutability::Const => ValueKind::is_storage_backed,
                        Mutability::Var => ValueKind::is_storage_backed_mut,
                    };
                    (arg_value.map(predicate).or_missing(), mutability)
                }
            };

            if !matches_pass_by {
                self.report_mismatched_binding(
                    BindingTo::Storage(mutability),
                    arg_expr.into(),
                    arg_span,
                    arg_span,
                    |thing| format!("cannot pass {thing} to this parameter"),
                    None,
                );
            } else if !param.coerced_type {
                // Allow coercion for pass by value, but not for ref-args
                let predicate = match param.pass_by {
                    ty::PassBy::Value => ty::rules::is_assignable,
                    ty::PassBy::Reference(_) => ty::rules::is_coercible_into_param,
                };

                let param_ty = param.param_ty.in_db(self.db).peel_opaque(in_module).id();

                if !predicate(self.db, param_ty, arg_ty) {
                    self.report_mismatched_param_tys(
                        param.param_ty,
                        arg_ty,
                        arg_span,
                        arg_span,
                        arg_span,
                        param.pass_by,
                    );
                }
            }
        }

        if require_value && !matches!(kind, SubprogramKind::Function) {
            self.state().reporter.error(
                "cannot call procedure here",
                "procedure calls cannot be used as expressions",
                lhs_span,
            );
        }
    }

    fn typeck_primitive(&self, id: toc_hir::ty::TypeId, ty_node: &toc_hir::ty::Primitive) {
        let db = self.db;
        let ty = self
            .db
            .from_hir_type(id.in_library(self.library_id))
            .in_db(db);

        let (expr_body, expr_span) = match ty_node {
            toc_hir::ty::Primitive::SizedChar(toc_hir::ty::SeqLength::Expr(body))
            | toc_hir::ty::Primitive::SizedString(toc_hir::ty::SeqLength::Expr(body)) => {
                let expr_span = self.library.body(*body).span.lookup_in(&self.library);

                (*body, expr_span)
            }
            _ => return,
        };

        if !self.expect_integer_value((self.library_id, expr_body).into()) {
            return;
        }

        // Check resultant size
        let (seq_size, size_limit, allow_dyn_size) = match ty.kind() {
            ty::TypeKind::CharN(seq_size @ ty::SeqSize::Fixed(_)) => {
                // Note: 32768 is the minimum defined limit for the length on `n` for char(N)
                // ???: Do we want to add a config/feature option to change this?
                (seq_size, ty::MAX_CHAR_N_LEN, true)
            }
            ty::TypeKind::StringN(seq_size @ ty::SeqSize::Fixed(_)) => {
                // 256 is the maximum defined limit for the length on `n` for string(N),
                // so no option of changing that (unless we have control over the interpreter code).
                // - Legacy interpreter has the assumption baked in that the max length of a string is 256,
                //   so we can't change it yet unless we use a new interpreter.
                (seq_size, ty::MAX_STRING_LEN, false)
            }
            // because of hir disambiguation above
            _ => unreachable!(),
        };

        let int = match seq_size.fixed_len(db, expr_span) {
            Ok(v) => v,
            Err(ty::NotFixedLen::AnySize) => return, // any-sized, doesn't need checking
            Err(ty::NotFixedLen::ConstError(err)) => {
                // Allow non-compile time exprs in this position, if allowed
                if err.is_not_compile_time() && allow_dyn_size {
                    // Right now, is unsupported
                    let ty_span = self.library.lookup_type(id).span;
                    let ty_span = ty_span.lookup_in(&self.library);

                    self.state().reporter.error(
                        "unsupported type",
                        "dynamically sized `char(N)` isn't supported yet",
                        ty_span,
                    );
                } else {
                    err.report_to(db, &mut self.state().reporter);
                }
                return;
            }
        };

        // Convert into a size, checking if it's within the given limit
        if !int
            .into_u32()
            .map(|size| (1..size_limit).contains(&size))
            .unwrap_or(false)
        {
            let inclusive_limit = size_limit - 1;
            self.state()
                .reporter
                .error_detailed("invalid character count size", expr_span)
                .with_error(format!("computed count is {int}"), expr_span)
                .with_info(format!("valid sizes are between 1 to {inclusive_limit}"))
                .finish();
        }
    }

    fn typeck_alias(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Alias) {
        let Self {
            library,
            library_id,
            db,
            ..
        } = self;

        let in_module = db.inside_module(id.in_library(*library_id).into());
        let mut span = ty.base_def.span();
        let mut def_id = DefId(*library_id, *ty.base_def.item());

        for segment in &ty.segments {
            let fields = db.fields_of((def_id, in_module).into());
            let next_def =
                fields.and_then(|fields| fields.lookup(*segment.item()).map(|field| field.def_id));

            if let Some(next_def) = next_def {
                def_id = next_def;
                span = segment.span();
            } else {
                let library = db.library(def_id.0);
                let def_info = library.local_def(def_id.1);

                let thing = def_info.name;
                let field_name = segment.item();

                // ???: Include decl here?

                // No field
                self.state().reporter.error(
                    format!("no field named `{field_name}` in `{thing}`"),
                    format!("no field named `{field_name}` in here"),
                    segment.span().lookup_in(&self.library),
                );

                return;
            }
        }

        let binding_to = self.db.binding_to(def_id.into());

        if !binding_to.map(BindingTo::is_type).or_missing() {
            let span = span.lookup_in(library);

            self.report_mismatched_binding(
                BindingTo::Type,
                def_id.into(),
                span,
                span,
                |thing| format!("cannot use {thing} as a type alias"),
                None,
            );
        }
    }

    fn typeck_constrained_ty(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Constrained) {
        let db = self.db;
        let library = &self.library;
        let library_id = self.library_id;

        let ty_span = library.lookup_type(id).span.lookup_in(library);

        let cons_tyref = db.from_hir_type(id.in_library(library_id)).in_db(db);
        let (base_ty, start_bound, end_bound) = match cons_tyref.kind() {
            ty::TypeKind::Constrained(base_ty, start_bound, end_bound) => {
                (base_ty, start_bound, end_bound)
            }
            _ => unreachable!(),
        };
        let base_tyref = base_ty.in_db(db);

        let start_span = library.body(ty.start).span.lookup_in(library);
        let end_span = match ty.end {
            toc_hir::ty::ConstrainedEnd::Expr(end) => library.body(end).span.lookup_in(library),
            toc_hir::ty::ConstrainedEnd::Unsized(sz) => sz.span().lookup_in(library),
        };

        let end = match ty.end {
            toc_hir::ty::ConstrainedEnd::Expr(end) => Some(end),
            _ => None,
        };

        let start_ty = db.type_of(ty.start.in_library(library_id).into());
        let end_ty = end.map(|end| db.type_of(end.in_library(library_id).into()));

        if let Some(end_ty) = end_ty {
            if !ty::rules::is_equivalent(db, start_ty, end_ty) {
                // Bounds must be equivalent tys
                let start_tyref = start_ty.in_db(db);
                let end_tyref = end_ty.in_db(db);

                self.state()
                    .reporter
                    .error_detailed("mismatched types", ty_span)
                    .with_note(format!("this is of type `{end_tyref}`"), end_span)
                    .with_note(format!("this is of type `{start_tyref}`"), start_span)
                    .with_error(
                        format!(
                            "`{start_tyref}` is not equivalent to `{end_tyref}`",
                            start_tyref = start_tyref.peel_aliases(),
                            end_tyref = end_tyref.peel_aliases()
                        ),
                        ty_span,
                    )
                    .with_info("range bound types must be equivalent")
                    .finish();
            } else if !base_tyref.kind().is_index() && !base_tyref.kind().is_error() {
                // base_ty must be an index ty
                self.state().reporter.error_detailed("mismatched types", ty_span)
                .with_note(format!("bounds are of type `{base_tyref}`"), ty_span)
                .with_error(format!("`{base_tyref}` is not an index type"), ty_span)
                .with_info(
                    "an index type is an integer, a `boolean`, a `char`, an enumerated type, or a range of those types",
                ).finish()
            }
        }

        // Bounds must be compile-time exprs and within the value range of the type
        // FIXME: use the correct eval params
        let eval_params = Default::default();

        // Note: report type is specified to make lifetime disjoint from db
        let check_const_bound =
            |bound_const, allow_dyn, reporter: &mut toc_reporting::MessageSink| match db
                .evaluate_const(bound_const, eval_params)
            {
                Ok(v) => Some(v),
                Err(err) => {
                    // Only report if it's not a compile-time expr and we allow dynamic expressions
                    if !(err.is_not_compile_time() && matches!(allow_dyn, ty::AllowDyn::Yes)) {
                        err.report_to(db, reporter);
                    }

                    None
                }
            };

        let mut state = self.state();
        if let Some(value) =
            check_const_bound(start_bound.clone(), ty::AllowDyn::No, &mut state.reporter)
        {
            if let Some((ordinal, min_value)) =
                Option::zip(value.ordinal(), base_tyref.min_int_of().ok())
            {
                if ordinal < min_value {
                    state.reporter.error(
                        "computed value is outside the type's range",
                        format!(
                            "`{value}` is smaller than the smallest possible `{base_tyref}`",
                            value = value.display(db)
                        ),
                        start_span,
                    );
                }
            }
        }

        if let ty::EndBound::Expr(end_bound, allow_dyn) = end_bound {
            if let Some(value) =
                check_const_bound(end_bound.clone(), *allow_dyn, &mut state.reporter)
            {
                if let Some((ordinal, max_value)) =
                    value.ordinal().zip(base_tyref.max_int_of().ok())
                {
                    if max_value < ordinal {
                        state.reporter.error(
                            "computed value is outside the type's range",
                            format!(
                                "`{value}` is larger than the largest possible `{base_tyref}`",
                                value = value.display(db)
                            ),
                            end_span,
                        );
                    }
                }
            }
        }
    }

    fn typeck_array_ty(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Array) {
        let db = self.db;
        let library = &self.library;
        let library_id = self.library_id;

        let in_module = db.inside_module((self.library_id, id).into());
        let allow_zero_size = matches!(ty.sizing, toc_hir::ty::ArraySize::Flexible);

        // - Ranges must be index type
        //   - same index sizing restriction
        // - element count must be smaller than maximum element count
        // - must be positive size (or non-negative if flexible)
        for &range_hir_ty in &ty.ranges {
            let span = library.lookup_type(range_hir_ty).span.lookup_in(library);
            let range_ty = db.from_hir_type(range_hir_ty.in_library(library_id));
            let range_tyref = range_ty.in_db(db).peel_opaque(in_module);

            if !range_tyref.clone().to_base_type().kind().is_index() {
                // Not an index type
                // FIXME: Switch to common "mismatched types" convention (either altering the rest or just this)
                self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_error(format!("`{range_tyref}` is not an index type"), span)
                .with_info(
                    "an index type is an integer, a `boolean`, a `char`, an enumerated type, or a range",
                )
                .finish();
            } else if range_tyref.clone().peel_aliases().kind().is_integer() {
                // Is an integer type, but not from a range
                // Range would be too big
                self.state()
                    .reporter
                    .error_detailed("index range is too large", span)
                    .with_error(
                        format!("a range over all `{range_tyref}` values is too large"),
                        span,
                    )
                    .with_info("use a range type to shrink the range of elements")
                    .finish()
            }

            self.require_known_size(range_hir_ty, || "`array` types".into());
            self.require_non_negative_size(range_hir_ty, allow_zero_size, |is_zero_sized| {
                if is_zero_sized {
                    "`array` types that aren't `flexible`".into()
                } else {
                    "`array` types".into()
                }
            });
        }

        // TODO: Check that the element count is under 2^32 elements
    }

    fn typeck_set_ty(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Set) {
        let db = self.db;
        let in_module = db.inside_module((self.library_id, id).into());

        // elem tyref should be the visible one
        let elem_tyref = db
            .from_hir_type(ty.elem_ty.in_library(self.library_id))
            .in_db(db)
            .peel_opaque(in_module);
        let span = self
            .library
            .lookup_type(ty.elem_ty)
            .span
            .lookup_in(&self.library);

        if !elem_tyref.clone().to_base_type().kind().is_index() {
            // Not an index type
            // FIXME: Switch to common "mismatched types" convention (either altering the rest or just this)
            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_error(format!("`{elem_tyref}` is not an index type"), span)
                .with_info(
                    "an index type is an integer, a `boolean`, a `char`, an enumerated type, or a range",
                )
                .finish();
        } else if elem_tyref.clone().peel_aliases().kind().is_integer() {
            // Is an integer type, but not from a range
            // Range would be too big
            self.state()
                .reporter
                .error_detailed("element range is too large", span)
                .with_error(
                    format!("a range over all `{elem_tyref}` values is too large"),
                    span,
                )
                .with_info("use a range type to shrink the range of elements")
                .finish()
        }
        self.require_known_positive_size(ty.elem_ty, || "`set` element types".into());
    }

    fn typeck_pointer_ty(&self, _id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Pointer) {
        self.require_positive_size(ty.ty, || "pointer types".into());

        // FIXME: reject collection types as unchecked pointer targets
    }

    fn typeck_subprogram_ty(&self, _id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Subprogram) {
        let in_where = || {
            let kind = match ty.kind {
                SubprogramKind::Procedure => "procedure",
                SubprogramKind::Function => "function",
                SubprogramKind::Process => "process",
            };
            format!("`{kind}` types")
        };

        self.require_known_positive_size(ty.result_ty, &in_where);

        if let Some(param_list) = &ty.param_list {
            for param in param_list {
                self.require_positive_size(param.param_ty, &in_where);
            }
        }
    }

    // Checks both a size known at compile time, and a size that isn't zero
    fn require_known_positive_size(
        &self,
        ty_spec: toc_hir::ty::TypeId,
        in_where: impl Fn() -> String,
    ) {
        self.require_known_size(ty_spec, &in_where);
        self.require_positive_size(ty_spec, &in_where);
    }

    // Requirement that the size must be positive (greater than zero)
    fn require_positive_size(&self, ty_spec: toc_hir::ty::TypeId, in_where: impl Fn() -> String) {
        self.require_non_negative_size(ty_spec, false, |_| in_where())
    }

    fn require_non_negative_size(
        &self,
        ty_spec: toc_hir::ty::TypeId,
        allow_zero_size: bool,
        in_where: impl Fn(bool) -> String,
    ) {
        let db = self.db;
        let library_id = self.library_id;
        let library = &self.library;

        let ty_id = db.from_hir_type(ty_spec.in_library(library_id));
        let ty_ref = ty_id.in_db(db);

        // Only need to check constrained types
        // The rest are guaranteed to have a positive size
        if let ty::TypeKind::Constrained(_, _, _) = ty_ref.kind() {
            let ty_span = library.lookup_type(ty_spec).span.lookup_in(library);

            match ty_ref.element_count() {
                Ok(size) if size.is_positive() && !size.is_zero() => {}
                Ok(size) if size.is_zero() && allow_zero_size => {}
                Ok(size) => {
                    let place = in_where(size.is_zero());

                    let size_kind = if size.is_zero() {
                        "zero sized ranges"
                    } else {
                        "negative sized ranges"
                    };

                    // Zero or negative size
                    self.state()
                        .reporter
                        .error_detailed("element range is too small", ty_span)
                        .with_note(format!("computed range size is {size}"), ty_span)
                        .with_error(format!("{size_kind} cannot be used in {place}"), ty_span)
                        .finish();
                }
                Err(ty::NotInteger::ConstError(err)) => {
                    match err.kind() {
                        const_eval::ErrorKind::IntOverflow => {
                            // Overflow
                            self.state()
                                .reporter
                                .error_detailed("invalid range size", ty_span)
                                .with_error("range size is too large", ty_span)
                                .finish();
                        }
                        _ if err.is_not_compile_time() => {
                            // Checked during constrained ty typeck
                        }
                        _ => {
                            // Error during const-eval, already reported
                        }
                    }
                }
                Err(ty::NotInteger::NotInteger) => {
                    // Error during const-eval, already reported
                }
            }
        }
    }

    // For now, we only check for any-sized charseq
    fn require_known_size(&self, ty_spec: toc_hir::ty::TypeId, in_where: impl Fn() -> String) {
        let ty_id = self.db.from_hir_type(ty_spec.in_library(self.library_id));
        let ty_ref = ty_id.in_db(self.db);
        match ty_ref.kind() {
            ty::TypeKind::CharN(ty::SeqSize::Any) | ty::TypeKind::StringN(ty::SeqSize::Any) => {
                let ty_span = self.library.lookup_type(ty_spec).span;
                let ty_span = ty_span.lookup_in(&self.library);
                let place = in_where();

                let things = if matches!(ty_ref.kind(), ty::TypeKind::CharN(_)) {
                    "character sequences"
                } else {
                    "strings"
                };

                self.state()
                    .reporter
                    .error_detailed("invalid storage type", ty_span)
                    .with_error(
                        format!("cannot use `{ty_ref}` in {place}"),
                        ty_span,
                    )
                    .with_info(format!(
                        "`{ty_ref}`'s refer to {things} that do not have a fixed size known at compile-time"
                    ))
                    .finish()
            }
            _ => {}
        }
    }

    fn report_mismatched_assign_tys(
        &self,
        left: ty::TypeId,
        right: ty::TypeId,
        report_at: toc_span::Span,
        left_span: toc_span::Span,
        right_span: toc_span::Span,
        target_name: Option<fn(String) -> String>,
    ) {
        let db = self.db;
        let left_ty = left.in_db(db);
        let right_ty = right.in_db(db);
        let target_name = target_name.map_or_else(
            || format!("this is of type `{left_ty}`"),
            |f| f(format!("`{left_ty}`")),
        );

        self.state()
            .reporter
            .error_detailed("mismatched types", report_at)
            .with_note(format!("this is of type `{right_ty}`"), right_span)
            .with_note(target_name, left_span)
            .with_info(format!(
                "`{right}` is not assignable into `{left}`",
                left = left_ty.peel_aliases(),
                right = right_ty.peel_aliases()
            ))
            .finish();
    }

    fn report_mismatched_param_tys(
        &self,
        left: ty::TypeId,
        right: ty::TypeId,
        report_at: toc_span::Span,
        left_span: toc_span::Span,
        right_span: toc_span::Span,
        pass_by: ty::PassBy,
    ) {
        let db = self.db;
        let left_ty = left.in_db(db);
        let right_ty = right.in_db(db);
        let relation = match pass_by {
            ty::PassBy::Value => "assignable into",
            ty::PassBy::Reference(_) => "equivalent to",
        };

        self.state()
            .reporter
            .error_detailed("mismatched types", report_at)
            .with_note(format!("this is of type `{right_ty}`"), right_span)
            .with_note(format!("parameter expects type `{left_ty}`"), left_span)
            .with_info(format!(
                "`{right}` is not {relation} `{left}`",
                left = left_ty.peel_aliases(),
                right = right_ty.peel_aliases()
            ))
            .finish();

        // FIXME: Specialize message on anonymous types (saying from different def locations)
    }

    fn report_mismatched_binding(
        &self,
        expected: BindingTo,
        binding_source: crate::db::BindingSource,
        report_at: Span,
        binding_span: Span,
        from_thing: impl FnOnce(&str) -> String,
        additional_info: Option<&str>,
    ) {
        use crate::db::BindingSource;

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
            .binding_def(binding_source)
            .map_or(binding_source, BindingSource::DefId);

        let mut state = self.state();
        let mut builder = match binding_source {
            BindingSource::DefId(DefId(lib_id, local_def)) => {
                let library = self.db.library(lib_id);
                let def_info = library.local_def(local_def);

                let name = def_info.name;
                let def_at = def_info.def_at.lookup_in(&library);

                let binding_to = match self.db.binding_to(binding_source) {
                    Ok(kind) => kind,
                    Err(NotBinding::Missing) => return, // already covered by an undeclared def or missing expr error
                    Err(NotBinding::NotBinding) => unreachable!("taken from a def_id"),
                };

                state
                    .reporter
                    .error_detailed(from_thing(&format!("`{name}`")), report_at)
                    .with_error(
                        format!("`{name}` is a reference to {binding_to}, not {expected}"),
                        binding_span,
                    )
                    .with_note(format!("`{name}` declared here"), def_at)
            }
            BindingSource::Body(..) | BindingSource::BodyExpr(..) => state
                .reporter
                .error_detailed(from_thing("expression"), report_at)
                .with_error(format!("not a reference to {expected}"), binding_span),
        };

        if let Some(info) = additional_info {
            builder = builder.with_info(info);
        }

        builder.finish();
    }

    fn expect_expression(&self, value_src: crate::db::ValueSource) -> bool {
        let span = value_src.span_of(self.db);

        self.expect_value_kind(
            ExpectedValue::Value,
            value_src,
            span,
            span,
            |thing| format!("cannot use {thing} as an expression"),
            None,
        )
    }

    fn expect_value_kind(
        &self,
        expected_kind: ExpectedValue,
        value_src: ty::db::ValueSource,
        report_at: Span,
        value_span: Span,
        from_thing: impl FnOnce(&str) -> String,
        additional_info: Option<&str>,
    ) -> bool {
        // looks like (when checking for value):
        // "cannot use `{name}` as an expression"
        // "`{name}` is a reference to {binding_to}, not a variable"
        // "`{name}` declared here"
        //
        // or (when checking for ref mut or non-register ref mut)
        // "cannot bind `{from}` to {thing}"
        // "`{name}` is a reference to {binding_to}, not a variable" or "not a reference to a variable"
        // "`{name}` declared here"

        let value_kind = self.db.value_produced(value_src);
        let predicate = match expected_kind {
            ExpectedValue::Value => ValueKind::is_value,
            ExpectedValue::Ref(Mutability::Const) => ValueKind::is_ref,
            ExpectedValue::Ref(Mutability::Var) => ValueKind::is_ref_mut,
            ExpectedValue::NonRegisterRef(Mutability::Const) => ValueKind::is_storage_backed,
            ExpectedValue::NonRegisterRef(Mutability::Var) => ValueKind::is_storage_backed_mut,
        };
        let is_valid = value_kind.map(predicate).or_missing();
        let checking_for_value = matches!(expected_kind, ExpectedValue::Value);

        if !is_valid {
            let binding_to = self.db.binding_def(value_src.into());

            let (thing, def_info) = match binding_to {
                Some(def_id) => {
                    // From some def
                    let binding_to = match self.db.binding_to(def_id.into()) {
                        Ok(binding_to) => binding_to,
                        Err(NotBinding::Missing) => return true,
                        Err(NotBinding::NotBinding) => unreachable!("from a DefId"),
                    };
                    let def_library = self.db.library(def_id.0);
                    let def_info = def_library.local_def(def_id.1);
                    let name = def_info.name;
                    let def_at = def_info.def_at.lookup_in(&def_library);

                    (format!("`{name}`"), Some((def_id, def_at, binding_to)))
                }
                None => {
                    // From expr
                    ("expression".to_string(), None)
                }
            };

            let mut state = self.state();
            let mut builder = state.reporter.error_detailed(from_thing(&thing), report_at);

            builder = if let Some((def_id, def_at, binding_to)) = def_info {
                builder = if matches!(
                    binding_to,
                    BindingTo::Storage(Mutability::Var) | BindingTo::Register(Mutability::Var)
                ) {
                    // Originally was mutable
                    // Likely from an export
                    let exporting_def = self
                        .db
                        .exporting_def(value_src.into())
                        .expect("at mut storage but rejected it");
                    let exported_library = self.db.library(exporting_def.0);
                    let exported_span = exported_library
                        .local_def(exporting_def.1)
                        .def_at
                        .lookup_in(&*exported_library);

                    builder
                        .with_error(format!("{thing} is not exported as `var`"), value_span)
                        .with_note(format!("{thing} exported from here"), exported_span)
                } else {
                    builder
                        .with_error(
                            format!("{thing} is a reference to {binding_to}, not a variable"),
                            value_span,
                        )
                        .with_note(format!("{thing} declared here"), def_at)
                };

                if matches!(binding_to, BindingTo::Type)
                    && matches!(expected_kind, ExpectedValue::Value)
                {
                    // Check if this is a set type
                    let ty_ref = self.db.type_of(def_id.into()).in_db(self.db).to_base_type();

                    if ty_ref.kind().is_set() {
                        // Possibly trying to construct an empty set
                        builder = builder.with_note(
                            "to construct an empty set, add `()` after here",
                            value_span,
                        );
                    }
                }

                builder
            } else {
                // Only in here when checking for references
                debug_assert!(!checking_for_value);
                builder.with_error("not a reference to a variable", value_span)
            };

            if let Some(extra) = additional_info {
                builder = builder.with_info(extra);
            }

            builder.finish();
        }

        is_valid
    }

    fn expect_integer_value(&self, value_src: crate::db::ValueSource) -> bool {
        if !self.expect_expression(value_src) {
            return false;
        }

        let ty = self.db.type_of(value_src.into()).in_db(self.db);
        let base_ty = ty.clone().to_base_type();

        if !base_ty.kind().is_integer() && !base_ty.kind().is_error() {
            let span = value_src.span_of(self.db);

            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_note(format!("this is of type `{ty}`"), span)
                .with_info(format!("`{ty}` is not an integer type"))
                .finish();

            false
        } else {
            true
        }
    }

    fn expect_boolean_value(&self, value_src: crate::db::ValueSource) -> bool {
        if !self.expect_expression(value_src) {
            return false;
        }

        let ty = self.db.type_of(value_src.into()).in_db(self.db);
        let base_ty = ty.clone().to_base_type();

        if !base_ty.kind().is_boolean() && !base_ty.kind().is_error() {
            let span = value_src.span_of(self.db);

            self.state()
                .reporter
                .error_detailed("mismatched types", span)
                .with_note(format!("this is of type `{ty}`"), span)
                .with_info("expected a `boolean` type")
                .finish();

            false
        } else {
            true
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
                let ty_span = ty_span.lookup_in(&self.library);

                let def_library = self.db.library(def_id.0);
                let name = def_library.local_def(def_id.1).name;

                self.state().reporter.error(
                    format!("`{name}` has not been resolved at this point"),
                    format!("`{name}` is required to be resolved at this point"),
                    ty_span,
                );
            }
        }
    }
}

#[derive(Clone, Copy)]
enum ExpectedValue {
    Value,
    Ref(Mutability),
    NonRegisterRef(Mutability),
}
