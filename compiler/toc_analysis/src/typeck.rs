//! Type checking

// Needs to use common analysis database
//#[cfg(test)]
//mod test;

use std::cell::RefCell;

use toc_hir::expr::{self, BodyExpr};
use toc_hir::library::{self, LibraryId};
use toc_hir::stmt::BodyStmt;
use toc_hir::symbol::DefId;
use toc_hir::{body, item, stmt};
use toc_reporting::CompileResult;
use toc_span::SpanId;

use crate::ty::{self, TyCtx};
use crate::HirAnalysis;

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
    let res = TypeCheck::check_unit(db, library);
    CompileResult::new((), res.messages().to_owned())
}

/*
use std::sync::Arc;
use crate::const_eval::ConstEvalCtx;

pub fn analyze_unit(hir_db: db::HirDb, unit_id: unit::UnitId) -> CompileResult<()> {
    let unit = hir_db.get_unit(unit_id);

    let const_eval_ctx = Arc::new(ConstEvalCtx::new(hir_db.clone()));
    const_eval::collect_const_vars(hir_db.clone(), unit, const_eval_ctx.clone());

    let typecheck_res = typeck::typecheck_unit(hir_db.clone(), unit, const_eval_ctx.clone());

    eprintln!("{}", ty::pretty_dump_typectx(typecheck_res.result()));
    eprintln!("{:#?}", const_eval_ctx);

    let mut messages = vec![];
    typecheck_res.bundle_messages(&mut messages);

    CompileResult::new((), messages)
}
*/

pub fn typecheck_unit(db: &dyn HirAnalysis, library: LibraryId) -> CompileResult<TyCtx> {
    TypeCheck::check_unit(db, library)
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
    ty_ctx: TyCtx,
    reporter: toc_reporting::MessageSink,
}

impl<'db> TypeCheck<'db> {
    fn check_unit(db: &'db dyn HirAnalysis, library_id: LibraryId) -> CompileResult<TyCtx> {
        let state = TypeCheckState {
            ty_ctx: TyCtx::new(),
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
        let visitor = (&typeck) as &dyn toc_hir::visitor::HirVisitor;
        toc_hir::visitor::postorder_visit_library(&typeck.library, visitor);

        let state = typeck.state.into_inner();

        let TypeCheckState {
            ty_ctx, reporter, ..
        } = state;

        CompileResult::new(ty_ctx, reporter.finish())
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

    fn visit_binary(&self, id: BodyExpr, expr: &toc_hir::expr::Binary) {
        self.typeck_binary(id.0, expr);
    }

    fn visit_unary(&self, id: BodyExpr, expr: &toc_hir::expr::Unary) {
        self.typeck_unary(id.0, expr);
    }

    fn visit_primitive(&self, id: toc_hir::ty::TypeId, ty: &toc_hir::ty::Primitive) {
        // TODO: Check types for charN and stringN (this is where we'd report those errors)
    }
}

impl TypeCheck<'_> {
    fn typeck_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        // Check the initializer expression
        let (ty_spec, init) = if let item::ConstVarTail::Both(ty_spec, initializer) = &item.tail {
            (*ty_spec, *initializer)
        } else {
            // Inferred or already specified
            return;
        };

        let def_id = DefId(self.library_id, self.library.item(id).def_id);
        let left = self.db.type_of(def_id.into());
        let right = self.db.type_of((self.library_id, init).into());

        // Ignore mutability because we're checking an initializer
        let valid_asn = ty::rules::is_assignable(self.db, left, right, true);
        dbg!(valid_asn);
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
                // TODO: Report expected type vs found type
                // - Requires type stringification/display impl
                self.state()
                    .reporter
                    .error_detailed("mismatched types", asn_span)
                    .finish();
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
                self.state()
                    .reporter
                    .error_detailed("cannot assign into expression on left hand side", asn_span)
                    .with_note(
                        "this expression cannot be used as a variable reference",
                        left_span,
                    )
                    .finish();
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
            let put_ty = self
                .db
                .type_of((self.library_id, body_id, item.expr).into());

            if !self.is_text_io_item(put_ty) {
                continue;
            }

            let put_ty = put_ty.lookup(self.db);
            let put_kind = put_ty.kind();
            // Only the following are allowed to have precision & exponent options:
            // - Int
            // - Nat
            // - Real
            if !put_kind.is_number() && !put_kind.is_error() {
                let item_span = body.expr(item.expr).span.lookup_in(&self.library.span_map);
                if let Some(expr) = item.opts.precision() {
                    let span = body.expr(expr).span.lookup_in(&self.library.span_map);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note("cannot specify fraction width for this type", item_span)
                        .with_info(
                            "fraction width can only be specified for numeric put types",
                            None,
                        )
                        .finish();
                }

                if let Some(expr) = item.opts.exponent_width() {
                    let span = body.expr(expr).span.lookup_in(&self.library.span_map);

                    self.state()
                        .reporter
                        .error_detailed("invalid put option", span)
                        .with_note("cannot specify exponent width for this type", item_span)
                        .with_info(
                            "exponent width can only be specified for numeric types",
                            None,
                        )
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
            let ty = db.type_of((self.library_id, body_expr).into());

            if !self.is_text_io_item(ty) {
                continue;
            }

            if ty.as_deref_mut(db).is_none() {
                let get_item_span = body.expr(item.expr).span.lookup_in(&self.library.span_map);

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
                self.check_text_io_arg(expr.in_body(body_id));
            }
        }
    }

    fn check_text_io_arg(&self, expr: BodyExpr) {
        let ty_ref = self.db.type_of((self.library_id, expr).into());
        let span = self.library.body(expr.0).expr(expr.1).span;

        self.check_integer_type(ty_ref, span);
    }

    fn check_integer_type(&self, ty: ty::TypeId, span: SpanId) {
        let ty = ty.lookup(self.db);

        if !ty.kind().is_integer() && !ty.kind().is_error() {
            let ty_span = span.lookup_in(&self.library.span_map);

            // TODO: Stringify type for more clarity on the error
            self.state()
                .reporter
                .error_detailed("mismatched types", ty_span)
                .with_note("expected integer type", ty_span)
                .finish();
        }
    }

    fn is_text_io_item(&self, ty: ty::TypeId) -> bool {
        let db = self.db;
        let ty_dat = ty.peel_ref(db).lookup(db);

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

    fn typeck_binary(&self, body: body::BodyId, expr: &expr::Binary) {
        let db = self.db;
        let lib_id = self.library_id;
        let left = db.type_of((lib_id, body, expr.lhs).into());
        let right = db.type_of((lib_id, body, expr.rhs).into());

        if let Err(err) = ty::rules::check_binary_op(db, left, *expr.op.item(), right) {
            let op_span = self.library.lookup_span(expr.op.span());
            ty::rules::report_invalid_bin_op(err, op_span, &mut self.state().reporter);
        }
    }

    fn typeck_unary(&self, body: body::BodyId, expr: &expr::Unary) {
        let db = self.db;
        let lib_id = self.library_id;
        let right = db.type_of((lib_id, body, expr.rhs).into());

        if let Err(err) = ty::rules::check_unary_op(db, *expr.op.item(), right) {
            let op_span = self.library.lookup_span(expr.op.span());
            ty::rules::report_invalid_unary_op(err, op_span, &mut self.state().reporter);
        }
    }
}

// We don't check seq lengths yet
/*
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
*/
