//! Expression nodes
use la_arena::Idx;
use toc_span::Spanned;

use crate::symbol;

pub type ExprIdx = Idx<Expr>;

/// Expressions
#[derive(Debug)]
pub enum Expr {
    /// Error expression, only used to represent invalid code
    Missing,
    /// Literal values
    Literal(Literal),
    //ObjClass(ObjClass),
    //Init(Init),
    //Nil(Nil),
    //SizeOf(SizeOf),
    Binary(Binary),
    Unary(Unary),
    Paren(Paren),
    /// `self` is a special case of a name expression
    Name(Name),
    //Field(Field),
    //Deref(Deref),
    //Cheat(Cheat),
    //NatCheat(NatCheat),
    //Arrow(Arrow),
    //Indirect(Indirect),
    //Bits(Bits),
    //Call(Call),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Nat(u64),
    Integer(u64),
    Real(f64),
    CharSeq(String),
    String(String),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Binary {
    pub lhs: ExprIdx,
    pub op: Spanned<BinaryOp>,
    pub rhs: ExprIdx,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
    /// Arrow (`->`)
    Arrow,
    /// Dot (`.`)
    Dot,
    /// Call (`(`)
    Call,
}

#[derive(Debug)]
pub struct Unary {
    pub op: Spanned<UnaryOp>,
    pub rhs: ExprIdx,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    /// Binary/boolean negation operator (`not`)
    Not,
    /// Integer identity (`+`)
    Identity,
    /// Integer negation (`-`)
    Negate,
    /// Nat cheat (`#`)
    NatCheat,
    /// Pointer dereferencing operator (`^`)
    Deref,
}

#[derive(Debug)]
pub struct Paren {
    pub expr: ExprIdx,
}

/// Name expression
#[derive(Debug)]
pub enum Name {
    /// Normal identifier reference
    Name(symbol::UseId),
    /// Reference to `self`
    // TODO: Link a use-id to the appropriate class DefId
    Self_,
}
