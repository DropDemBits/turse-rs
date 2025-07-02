//! Expression nodes

use crate::{Symbol, body::Body};

use toc_salsa_collections::arena::SalsaArenaMap;
pub use toc_syntax::{InfixOp as BinaryOp, PrefixOp as UnaryOp};

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to an expression.
    ///
    /// [`Body`]: crate::body::Body
    pub struct LocalExpr<'db>(Expr<'db>);
    /// Alias for the stmt arena index
    pub(crate) type ExprIndex<'db> = Index;
);

impl<'db> LocalExpr<'db> {
    pub fn in_body(self, body: Body<'db>) -> ExprId<'db> {
        ExprId(body, self)
    }
}

/// Uniquely identifies an expression within a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct ExprId<'db>(Body<'db>, LocalExpr<'db>);

impl<'db> ExprId<'db> {
    pub fn with_expr(self, expr: LocalExpr<'db>) -> Self {
        Self(self.0, expr)
    }

    pub fn body(self) -> Body<'db> {
        self.0
    }

    pub fn expr(self) -> LocalExpr<'db> {
        self.1
    }
}

pub type ExprMap<'db, V> = SalsaArenaMap<ExprIndex<'db>, V>;

/// Expressions
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum Expr<'db> {
    /// Error expression, only used to represent invalid code
    Missing,
    /// Literal values
    Literal(Literal),
    //ObjClass(ObjClass),
    /// Aggregate initialization expression, containing initialization values
    Init(Init<'db>),
    //Nil(Nil),
    //SizeOf(SizeOf),
    Binary(Binary<'db>),
    Unary(Unary<'db>),
    /// `all` expression
    All,
    /// Range expression
    Range(Range<'db>),
    /// `self` is a special case of a name expression
    Name(Name<'db>),
    /// Field lookup
    Field(Field<'db>),
    /// Deref (`^`) Expression
    Deref(Deref<'db>),
    //Cheat(Cheat),
    //NatCheat(NatCheat),
    //Arrow(Arrow),
    //Indirect(Indirect),
    //Bits(Bits),
    /// Calling expression
    Call(Call<'db>),
}

/// Literal expression
///
/// Note: While this does implement `Eq`, equality testing for reals
/// treats `NaN`s of the same bitwise representation as equal.
/// This equality relation is primarily used for memoizing HIR trees in the
/// salsa db.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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

// FloatBits satisfies the requirements for `Eq` by hashing
// the raw bit representations
impl std::hash::Hash for FloatBits {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.to_bits());
    }
}

impl From<f64> for FloatBits {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

/// Aggregate initialization
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Init<'db> {
    pub exprs: Vec<Body<'db>>,
}

/// Binary operator expression
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Binary<'db> {
    pub lhs: LocalExpr<'db>,
    pub op: BinaryOp,
    pub rhs: LocalExpr<'db>,
}

// Unary operator expression
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Unary<'db> {
    pub op: UnaryOp,
    pub rhs: LocalExpr<'db>,
}

/// Name expression
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum Name<'db> {
    /// Normal identifier reference
    Name(Symbol<'db>),
    /// Reference to `self`
    // FIXME: Resolve to the appropriate class DefId
    Self_,
}

/// Field lookup expression
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Field<'db> {
    /// Reference to lookup in
    pub lhs: LocalExpr<'db>,
    /// Field to lookup
    pub field: Symbol<'db>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Deref<'db> {
    /// Right-hand side
    pub rhs: LocalExpr<'db>,
}

/// Calling expression
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Call<'db> {
    /// Reference to the calling expression
    pub lhs: LocalExpr<'db>,
    /// Arguments to the call
    pub arguments: ArgList<'db>,
}

/// Argument list
pub type ArgList<'db> = Vec<LocalExpr<'db>>;

/// Range expression
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Range<'db> {
    pub start: RangeBound<'db>,
    pub end: Option<RangeBound<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum RangeBound<'db> {
    /// Bound is relative to the start point (`expr`)
    FromStart(LocalExpr<'db>),
    /// Bound is relative to end point (`* - expr`)
    FromEnd(LocalExpr<'db>),
    /// Bound is at the end point (`*`)
    AtEnd,
}
