//! Abstract syntax tree representation lowered from the initial KDL structure.
//!
//! Ensures that the syntax of the spec is valid before verifying semantics, and
//! provides access to equivalent span nodes to provide better error reporting.

use crate::PredicateOp;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Spanned<V>(V, miette::SourceSpan);

impl<V> Spanned<V> {
    pub(crate) fn new(value: V, span: miette::SourceSpan) -> Self {
        Self(value, span)
    }

    pub(crate) fn value(&self) -> &V {
        &self.0
    }

    pub(crate) fn span(&self) -> miette::SourceSpan {
        self.1
    }

    pub(crate) fn map<U>(self, f: impl FnOnce(V) -> U) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }

    pub(crate) fn into_inner(self) -> V {
        self.0
    }

    pub(crate) fn map_inner<U>(self, f: impl FnOnce(V) -> U) -> U {
        f(self.0)
    }
}

#[derive(Debug, Default)]
pub(crate) struct StackBeforeList<'src> {
    pub(crate) entries: Vec<Spanned<MaybeConditional<'src, StackBeforeOperand<'src>>>>,
}

#[derive(Debug)]
pub(crate) struct StackBeforeOperand<'src> {
    pub(crate) name: Spanned<&'src str>,
    pub(crate) ty: Spanned<&'src str>,
    pub(crate) description: Option<Spanned<&'src str>>,
    pub(crate) unused: Option<Spanned<bool>>,
    pub(crate) variadic: Option<Spanned<bool>>,
    pub(crate) computed: Option<Spanned<ComputedExpr<'src>>>,
    pub(crate) computed_offset: Option<Spanned<ComputedExpr<'src>>>,
}

#[derive(Debug, Default)]
pub(crate) struct StackAfterList<'src> {
    pub(crate) entries: Vec<Spanned<MaybeConditional<'src, StackAfterOperand<'src>>>>,
}

#[derive(Debug)]
pub(crate) struct StackAfterOperand<'src> {
    pub(crate) name: Spanned<&'src str>,
    pub(crate) ty: Spanned<&'src str>,
    pub(crate) description: Option<Spanned<&'src str>>,
    pub(crate) unused: Option<Spanned<bool>>,
    pub(crate) preserves: Option<Spanned<&'src str>>,
    pub(crate) computed: Option<Spanned<ComputedExpr<'src>>>,
    pub(crate) computed_offset: Option<Spanned<ComputedExpr<'src>>>,
}

#[derive(Debug)]
pub(crate) enum ComputedExpr<'src> {
    ImmediateOperand {
        operand: Spanned<&'src str>,
    },
    VariantRef {
        enum_ty: Spanned<&'src str>,
        variant: Spanned<&'src str>,
    },
    Op {
        lhs: Spanned<Box<Self>>,
        op: Spanned<ComputedOp>,
        rhs: Spanned<Box<Self>>,
    },
}

#[derive(Debug)]
pub(crate) enum ComputedOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub(crate) enum MaybeConditional<'src, V> {
    Conditional(Conditional<'src, V>),
    ConditionalCase(ConditionalCase<'src, V>),
    Operand(V),
}

#[derive(Debug)]
pub(crate) struct Conditional<'src, V> {
    pub(crate) predicates: Vec<Spanned<ConditionalPredicate<'src>>>,
    pub(crate) operands: Vec<Spanned<V>>,
}

#[derive(Debug)]
pub(crate) struct ConditionalPredicate<'src> {
    pub(crate) immediate_operand: Spanned<&'src str>,
    pub(crate) op: Spanned<PredicateOp>,
    pub(crate) value: Spanned<PredicateValue<'src>>,
}

#[derive(Debug)]
pub(crate) struct ConditionalCase<'src, V> {
    pub(crate) immediate_decode_match: Spanned<&'src str>,
    pub(crate) arms: Vec<Spanned<ConditonalArm<'src, V>>>,
}

#[derive(Debug)]
pub(crate) struct ConditonalArm<'src, V> {
    pub(crate) value: Spanned<PredicateValue<'src>>,
    pub(crate) operands: Vec<Spanned<V>>,
}

#[derive(Debug)]
pub(crate) enum PredicateValue<'src> {
    EnumVariant {
        enum_ty: Spanned<&'src str>,
        variant: Spanned<&'src str>,
    },
    Number(i128),
}
