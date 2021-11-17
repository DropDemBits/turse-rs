//! Errors during constant evaluation
use toc_hir::symbol::DefId;
use toc_span::Span;

use crate::const_eval::db;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstError {
    kind: ErrorKind,
    span: Span,
}

impl ConstError {
    pub(super) fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub(super) fn without_span(kind: ErrorKind) -> Self {
        Self {
            kind,
            span: Span::default(),
        }
    }

    pub(super) fn change_span(self, new_span: Span) -> Self {
        Self {
            kind: self.kind,
            span: new_span,
        }
    }

    /// Reports the detailed version of the `ConstError` to the given reporter
    pub fn report_to<DB: db::ConstEval + ?Sized>(
        &self,
        db: &DB,
        reporter: &mut toc_reporting::MessageSink,
    ) {
        // Ignore already reported messages, or for missing expressions
        if matches!(self.kind, ErrorKind::Reported | ErrorKind::MissingExpr) {
            return;
        }

        // Report common message header
        match &self.kind {
            ErrorKind::NotConstExpr(Some(def_id)) => {
                // Report at the reference's definition spot
                let library = db.library(def_id.0);
                let def_info = library.local_def(def_id.1);
                let def_span = def_info.name.span().lookup_in(&library.span_map);

                reporter
                    .error_detailed("reference cannot be computed at compile-time", self.span)
                    .with_note(
                        &format!("`{}` declared here", def_info.name.item()),
                        def_span,
                    )
            }
            _ => reporter.error_detailed(&format!("{}", self.kind), self.span),
        }
        .finish();
    }
}

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq, Hash)]
pub(super) enum ErrorKind {
    // Traversal errors
    /// Encountered an evaluation cycle
    #[allow(dead_code)] // TODO: add cycle fixup when we can generate cycles
    #[error("detected a compile-time evaluation cycle")]
    EvalCycle,
    /// Missing expression operand
    #[error("operand is an invalid expression")]
    MissingExpr,
    /// Not a compile-time expression,
    ///
    /// If the expression comes from an identifier, then the provided [`DefId`]
    /// points to the symbol's definition location,
    // TODO: Add tests for the non-reference version
    // We don't generate the non-reference kind yet since we haven't lowered all exprs
    #[error("expression cannot be computed at compile-time")]
    NotConstExpr(Option<DefId>),
    /// Error is already reported
    #[allow(dead_code)] // TODO: Figure out how we can dedup errors
    #[error("compile-time evaluation error already reported")]
    Reported,

    // Computation errors
    /// Wrong operand type in eval expression
    #[error("wrong type for compile-time expression")]
    WrongOperandType,
    /// Wrong resultant type in eval expression
    #[error("wrong type for compile-time expression")]
    WrongResultType,
    /// Integer overflow
    #[error("integer overflow in compile-time expression")]
    IntOverflow,
    /// Floating point overflow
    #[error("real overflow in compile-time expression")]
    RealOverflow,
    /// Division by zero
    #[error("division by zero in compile-time expression")]
    DivByZero,
    /// Negative int exponent provided during power raising
    #[error("raising integer to a negative exponent")]
    NegativeIntExp,
    /// Negative int shift provided during bit shifting
    #[error("bit shifting integer by a negative amount")]
    NegativeIntShift,

    // Unsupported messages
    /// Currently unsupported const eval value
    #[allow(dead_code)] // We'll reuse this for set types once we lower those
    #[error("literal is currently not implemented for compile-time evaluation")]
    UnsupportedValue,
    /// Currently unsupported const eval operation
    #[error("operation is currently not implemented for compile-time evaluation")]
    UnsupportedOp,
}
