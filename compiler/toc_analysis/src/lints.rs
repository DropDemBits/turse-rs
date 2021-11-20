//! Various different lint passes that we perform

use std::cell::{RefCell, RefMut};

use toc_hir::visitor::{HirVisitor, Walker};
use toc_hir::{expr, library};
use toc_reporting::{CompileResult, MessageSink};

use crate::{db, ty};

pub(crate) fn lint_library(
    db: &dyn db::HirAnalysis,
    library_id: library::LibraryId,
) -> CompileResult<()> {
    let library = db.library(library_id);
    let ctx = LintContext {
        _db: db,
        _library_id: library_id,
        library,
        reporter: Default::default(),
    };

    Walker::from_library(&ctx.library).visit_preorder(&ImplLimitsLint { ctx: &ctx });

    CompileResult::new((), ctx.reporter.into_inner().finish())
}

struct LintContext<'db> {
    _db: &'db dyn db::HirAnalysis,
    _library_id: library::LibraryId,
    library: library::LoweredLibrary,
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
        let span = self.ctx.library.body(id.0).expr(id.1).span;
        let span = self.ctx.library.lookup_span(span);

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
            _ => {}
        }
    }
}
