//! Errors during constant evaluation
use toc_hir::{
    package::PackageId,
    span::Spanned,
    symbol::{DefId, Resolve, Symbol},
};
use toc_span::Span;

use crate::const_eval::db;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstError {
    kind: ErrorKind,
    span: Span,
}

impl ConstError {
    pub(crate) fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub(crate) fn without_span(kind: ErrorKind) -> Self {
        Self {
            kind,
            span: Span::default(),
        }
    }

    pub(crate) fn change_span(self, new_span: Span) -> Self {
        Self {
            kind: self.kind,
            span: new_span,
        }
    }

    pub(crate) fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// Reports the detailed version of the `ConstError` to the given reporter
    pub fn report_to(&self, db: &dyn db::ConstEval, reporter: &mut toc_reporting::MessageSink) {
        self.make_report_to(db, reporter, false);
    }

    /// Reports the detailed version of the `ConstError` to the given reporter as a delayed report.
    /// This message should already be covered by other spans
    pub fn report_delayed_to(
        &self,
        db: &dyn db::ConstEval,
        reporter: &mut toc_reporting::MessageSink,
    ) {
        self.make_report_to(db, reporter, true);
    }

    fn make_report_to(
        &self,
        db: &dyn db::ConstEval,
        reporter: &mut toc_reporting::MessageSink,
        always_delayed: bool,
    ) {
        // FIXME: This error needs to be clarify where it came from
        //
        // In the following code:
        // ```diff
        // var a := 1
        // const b := a
        // const c := b
        // + var use : char(c)
        // ```
        //
        // `b` suddenly starts expecting `a` to be computed at compile-time
        // since it's used in `c`, which is used in a compile-time eval context.
        //
        // It would be useful to accumulate where the error comes from.

        // Ignore already reported messages, or for missing expressions
        if matches!(self.kind, ErrorKind::Reported | ErrorKind::MissingExpr) {
            return;
        }

        // Delayed reports need a real span
        if always_delayed && self.span.into_parts().is_none() {
            return;
        }

        let mut builder = match &self.kind {
            ErrorKind::NotConstExpr(NotConst::Def(def_id)) => {
                // Report at the reference's definition spot
                let bind_to = match db.symbol_kind(*def_id) {
                    Some(kind) => kind,
                    None => return, // taken from an undeclared ident or missing expr
                };
                let package = db.package(def_id.package());
                let def_info = package.local_def(def_id.local());
                let name = def_info.name;
                let def_span = def_info.def_at.lookup_in(&package);

                reporter
                    .error_detailed(
                        format!("cannot compute `{name}` at compile-time"),
                        self.span,
                    )
                    .with_error(
                        format!("`{name}` is a reference to {bind_to}, not a constant"),
                        self.span,
                    )
                    .with_note(format!("`{name}` declared here",), def_span)
            }
            ErrorKind::NotConstExpr(NotConst::Binding(package_id, binding)) => {
                let name = binding.item();
                let package = db.package(*package_id);

                if matches!(package.binding_resolve(*binding), Resolve::Err) {
                    // taken from an undeclared ident
                    return;
                }

                reporter
                    .error_detailed(
                        format!("cannot compute `{name}` at compile-time"),
                        self.span,
                    )
                    .with_error(format!("{}", self.kind), self.span)
            }
            // Report common message header
            _ => reporter
                .error_detailed("cannot compute expression at compile-time", self.span)
                .with_error(format!("{}", self.kind), self.span),
        };

        // These errors should be covered by earlier reporting
        if always_delayed
            || matches!(
                self.kind,
                ErrorKind::MissingExpr
                    | ErrorKind::WrongOperandType
                    | ErrorKind::WrongResultType
                    | ErrorKind::NoFields(..)
            )
        {
            builder = builder.report_delayed();
        }

        builder.finish();
    }

    pub fn is_not_compile_time(&self) -> bool {
        matches!(self.kind, ErrorKind::NotConstExpr(_))
    }
}

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq, Hash)]
pub(crate) enum ErrorKind {
    // Traversal errors
    /// Encountered an evaluation cycle
    #[allow(dead_code)] // TODO: add cycle fixup when we can generate cycles
    #[error("detected a compile-time evaluation cycle")]
    EvalCycle,
    /// Missing expression operand
    #[error("operand is an invalid expression")]
    MissingExpr,
    /// Not a compile-time expression. The included [`NotConst`] specifies what kind of
    /// not compile-time expression this is.
    #[error("expression cannot be computed at compile-time")]
    NotConstExpr(NotConst),
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
    /// Value to assign is outside of the type's supported range
    #[error("value is outside of the type's range")]
    OutsideRange,
    /// Division by zero
    #[error("division by zero in compile-time expression")]
    DivByZero,
    /// Negative int exponent provided during power raising
    #[error("raising integer to a negative exponent")]
    NegativeIntExp,
    /// Negative int shift provided during bit shifting
    #[error("bit shifting integer by a negative amount")]
    NegativeIntShift,
    /// Produced a string that is too big
    #[error("produced a string that is too large")]
    StringTooBig,
    /// Produced a char(n) that is too big
    #[error("produced a character sequence that is too large")]
    CharNTooBig,
    /// No field named `{sym}` on the given type.
    #[error("no field `{0}` in expression")]
    NoFields(Symbol),

    // Unsupported messages
    /// Currently unsupported const eval value
    #[allow(dead_code)] // We'll reuse this for set types once we lower those
    #[error("literal is currently not implemented for compile-time evaluation")]
    UnsupportedValue,
    /// Currently unsupported const eval operation
    #[error("operation is currently not implemented for compile-time evaluation")]
    UnsupportedOp,
}

/// What kind of expression is not a compile time one
// FIXME: Add tests for the non-reference version
// We don't generate the non-reference kind yet since we haven't lowered all exprs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NotConst {
    /// Known def, points to the associated definition.
    Def(DefId),
    /// Binding, not necessarily a known def.
    /// Points to the location of the binding
    Binding(PackageId, Spanned<Symbol>),
    /// Other expression, not nameable.
    Expr,
}
