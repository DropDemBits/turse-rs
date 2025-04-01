//! Various different lint passes that we perform

use std::cell::{RefCell, RefMut};

use toc_hir::{
    expr, package,
    package_graph::SourcePackage,
    visitor::{HirVisitor, Walker},
};
use toc_reporting::{CompileResult, MessageSink};

use crate::{db, ty};

#[cfg(test)]
mod test;

#[salsa::tracked]
pub(crate) fn lint_package(
    db: &dyn db::HirAnalysis,
    package_id: SourcePackage,
) -> CompileResult<()> {
    let package = db.package(package_id.into());
    let ctx = LintContext {
        _db: db,
        _package_id: package_id.into(),
        package,
        reporter: Default::default(),
    };

    Walker::from_package(&ctx.package).visit_postorder(&ImplLimitsLint { ctx: &ctx });

    CompileResult::new((), ctx.reporter.into_inner().finish())
}

struct LintContext<'db> {
    _db: &'db dyn db::HirAnalysis,
    _package_id: package::PackageId,
    package: package::LoweredPackage,
    reporter: RefCell<MessageSink>,
}

impl<'db> LintContext<'db> {
    // Helper for borrowing the reporter mutably
    fn reporter(&self) -> RefMut<MessageSink> {
        self.reporter.borrow_mut()
    }
}

struct ImplLimitsLint<'ctx> {
    ctx: &'ctx LintContext<'ctx>,
}

impl<'ctx> HirVisitor for ImplLimitsLint<'ctx> {
    fn visit_literal(&self, id: expr::BodyExpr, expr: &expr::Literal) {
        let span = self.ctx.package.body(id.0).expr(id.1).span;
        let span = span.lookup_in(&self.ctx.package);

        // Check that all literals meet the implementation defined limits
        match expr {
            expr::Literal::CharSeq(chars) => {
                if chars.len() >= ty::MAX_CHAR_N_LEN.try_into().unwrap() {
                    self.ctx.reporter().error(
                        "invalid char literal",
                        "this char literal is too long",
                        span,
                    );
                }
            }
            expr::Literal::String(chars) => {
                if chars.len() >= ty::MAX_STRING_LEN.try_into().unwrap() {
                    self.ctx.reporter().error(
                        "invalid string literal",
                        "this string literal is too long",
                        span,
                    );
                }
            }
            expr::Literal::Integer(value) => {
                // We check if the value of an integer is in bounds here because
                // we (eventually will) have implementation backends that don't
                // support 64-bit values.
                //
                // If we do choose to implement integer size suffixes, then this
                // check can be moved into HIR lowering as we'll default to representing
                // them as `int` types. This still won't take care of things like
                // overflowing unary negation or any overflowing operation, but
                // we'd need to use the const evaluator for that kind of reporting.
                if *value > u32::MAX.into() {
                    self.ctx
                        .reporter()
                        .error("invalid int literal", "number is too large", span);
                }
            }
            _ => {}
        }
    }
}
