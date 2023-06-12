//! Expression nodes

use crate::{Body, Symbol};

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to an expression.
    ///
    /// [`Body`]: crate::body::Body
    pub struct ExprId(Expr);
    /// Alias for the expr arena index
    pub(crate) type ExprIndex = Index;
);

// impl ExprId {
//     pub fn in_body(self, body_id: BodyId) -> BodyExpr {
//         BodyExpr(body_id, self)
//     }
// }

// /// Uniquely identifies an expression within a package
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct BodyExpr(pub BodyId, pub ExprId);

// impl BodyExpr {
//     pub fn with_expr(self, expr: ExprId) -> Self {
//         BodyExpr(self.0, expr)
//     }

//     pub fn body(self) -> BodyId {
//         self.0
//     }

//     pub fn expr(self) -> ExprId {
//         self.1
//     }
// }

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
#[derive(Debug, Clone)]
pub enum Literal {
    Integer(u64),
    Real(f64),
    Char(char),
    /// Guaranteed to be a string of non-zero length
    CharSeq(String),
    String(String),
    Boolean(bool),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Bitwise compare for real literals
            (Self::Real(l0), Self::Real(r0)) => l0.to_bits() == r0.to_bits(),
            // Simple equality for everything else
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::CharSeq(l0), Self::CharSeq(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            _ => false,
        }
    }
}

// Literal::Real satisfies the requirements for `Eq` by comparing
// the raw bit representations
impl Eq for Literal {}

/// Aggregate initialization
#[derive(Debug, PartialEq, Eq)]
pub struct Init {
    pub exprs: Vec<Body>,
}

/// Binary operator expression
#[derive(Debug, PartialEq, Eq)]
pub struct Binary {
    pub lhs: ExprId,
    pub op: BinaryOp,
    pub rhs: ExprId,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BinaryOp {
    /// Addition / Set Union / String Concatenation (`+`)
    Add,
    /// Subtraction / Set Subtraction (`-`)
    Sub,
    /// Multiplication / Set Intersection (`*`)
    Mul,
    /// Integer Division (`div`)
    Div,
    /// Real Division (`/`)
    RealDiv,
    /// Modulo (`mod`)
    Mod,
    /// Remainder (`rem`)
    Rem,
    /// Exponentiation (`**`)
    Exp,
    /// Bitwise/boolean And (`and`)
    And,
    /// Bitwise/boolean Or (`or`)
    Or,
    /// Bitwise/boolean Exclusive-Or (`xor`)
    Xor,
    /// Logical Shift Left (`shl`)
    Shl,
    /// Logical Shift Right (`shr`)
    Shr,
    /// Less than (`<`)
    Less,
    /// Less than or Equal (`<=`)
    LessEq,
    /// Greater than (`>`)
    Greater,
    /// Greater than or Equal (`>=`)
    GreaterEq,
    /// Equality (`=` or `=`)
    Equal,
    /// Inequality (`not=` or `~=`)
    NotEqual,
    /// Set inclusion (`in`)
    In,
    /// Set exclusion (`not in`)
    NotIn,
    /// Material Implication (`=>`)
    Imply,
}

// Unary operator expression
#[derive(Debug, PartialEq, Eq)]
pub struct Unary {
    pub op: UnaryOp,
    pub rhs: ExprId,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UnaryOp {
    /// Binary/boolean negation operator (`not`)
    Not,
    /// Integer identity (`+`)
    Identity,
    /// Integer negation (`-`)
    Negate,
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
    pub lhs: ExprId,
    /// Field to lookup
    pub field: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Deref {
    /// Right-hand side
    pub rhs: ExprId,
}

/// Calling expression
#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    /// Reference to the calling expression
    pub lhs: ExprId,
    /// Arguments to the call
    pub arguments: ArgList,
}

/// Argument list
pub type ArgList = Vec<ExprId>;

/// Range expression
#[derive(Debug, PartialEq, Eq)]
pub struct Range {
    pub start: RangeBound,
    pub end: Option<RangeBound>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeBound {
    /// Bound is relative to the start point (`expr`)
    FromStart(ExprId),
    /// Bound is relative to end point (`* - expr`)
    FromEnd(ExprId),
    /// Bound is at the end point (`*`)
    AtEnd,
}
