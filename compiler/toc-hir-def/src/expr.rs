//! Expression nodes

use crate::{body::Body, Symbol};

use la_arena::ArenaMap;
pub use toc_syntax::{InfixOp as BinaryOp, PrefixOp as UnaryOp};

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to an expression.
    ///
    /// [`Body`]: crate::body::Body
    pub struct LocalExpr(Expr);
    /// Alias for the expr arena index
    pub(crate) type ExprIndex = Index;
);

impl LocalExpr {
    pub fn in_body(self, body: Body) -> ExprId {
        ExprId(body, self)
    }
}

/// Uniquely identifies an expression within a package
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExprId(Body, LocalExpr);

impl ExprId {
    pub fn with_expr(self, expr: LocalExpr) -> Self {
        Self(self.0, expr)
    }

    pub fn body(self) -> Body {
        self.0
    }

    pub fn expr(self) -> LocalExpr {
        self.1
    }
}

pub type ExprMap<V> = ArenaMap<ExprIndex, V>;

/// Expressions
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    /// Error expression, only used to represent invalid code
    Missing,
    /// Literal values
    Literal(Literal),
    //ObjClass(ObjClass),
    /// Aggregate initialization expression, containing initialization values
    Init(Init),
    //Nil(Nil),
    //SizeOf(SizeOf),
    Binary(Binary),
    Unary(Unary),
    /// `all` expression
    All,
    /// Range expression
    Range(Range),
    /// `self` is a special case of a name expression
    Name(Name),
    /// Field lookup
    Field(Field),
    /// Deref (`^`) Expression
    Deref(Deref),
    //Cheat(Cheat),
    //NatCheat(NatCheat),
    //Arrow(Arrow),
    //Indirect(Indirect),
    //Bits(Bits),
    /// Calling expression
    Call(Call),
}

/// Literal expression
///
/// Note: While this does implement `Eq`, equality testing for reals
/// treats `NaN`s of the same bitwise representation as equal.
/// This equality relation is primarily used for memoizing HIR trees in the
/// salsa db.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Integer(u64),
    Real(FloatBits),
    Char(char),
    /// Guaranteed to be a string of non-zero length
    CharSeq(String),
    String(String),
    Boolean(bool),
}

/// A wrapper around `f64` that implements `PartialEq` and `Eq` through
/// bitwise equality.
#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct FloatBits(pub f64);

impl PartialEq for FloatBits {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

// FloatBits satisfies the requirements for `Eq` by comparing
// the raw bit representations
impl Eq for FloatBits {}

impl From<f64> for FloatBits {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

/// Aggregate initialization
#[derive(Debug, PartialEq, Eq)]
pub struct Init {
    pub exprs: Box<[Body]>,
}

/// Binary operator expression
#[derive(Debug, PartialEq, Eq)]
pub struct Binary {
    pub lhs: LocalExpr,
    pub op: BinaryOp,
    pub rhs: LocalExpr,
}

// Unary operator expression
#[derive(Debug, PartialEq, Eq)]
pub struct Unary {
    pub op: UnaryOp,
    pub rhs: LocalExpr,
}

/// Name expression
#[derive(Debug, PartialEq, Eq)]
pub enum Name {
    /// Normal identifier reference
    Name(Symbol),
    /// Reference to `self`
    // FIXME: Resolve to the appropriate class DefId
    Self_,
}

/// Field lookup expression
#[derive(Debug, PartialEq, Eq)]
pub struct Field {
    /// Reference to lookup in
    pub lhs: LocalExpr,
    /// Field to lookup
    pub field: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Deref {
    /// Right-hand side
    pub rhs: LocalExpr,
}

/// Calling expression
#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    /// Reference to the calling expression
    pub lhs: LocalExpr,
    /// Arguments to the call
    pub arguments: ArgList,
}

/// Argument list
pub type ArgList = Box<[LocalExpr]>;

/// Range expression
#[derive(Debug, PartialEq, Eq)]
pub struct Range {
    pub start: RangeBound,
    pub end: Option<RangeBound>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeBound {
    /// Bound is relative to the start point (`expr`)
    FromStart(LocalExpr),
    /// Bound is relative to end point (`* - expr`)
    FromEnd(LocalExpr),
    /// Bound is at the end point (`*`)
    AtEnd,
}

impl RangeBound {
    pub fn expr(self) -> Option<LocalExpr> {
        match self {
            RangeBound::FromStart(expr) | RangeBound::FromEnd(expr) => Some(expr),
            RangeBound::AtEnd => None,
        }
    }
}
