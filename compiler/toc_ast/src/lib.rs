//! AST representation
#![allow(clippy::if_same_then_else)]
pub mod ast;
pub mod block;
pub mod scope;
pub mod types;
pub mod token;
pub mod value;

extern crate toc_core;

use std::fmt;

/// All valid operators usable in Turing code
#[derive(Debug)]
pub enum Operator {
    /// No Operation, used by simple assignment
    NoOp,

    // Unary Operators
    /// Unary Plus/Identity (`+`)
    UnaryPlus,
    /// Unary Minus/Negation (`-`)
    UnaryMinus,
    /// Nat Typecheat (`#`)
    NatCheat,
    /// Bitwise/Logical Not (`not`)
    Not,
    /// Pointer Dereference (`^`)
    PointerDeref,

    // Binary Operators
    /// Addition / Union (`+`)
    Add,
    /// Subtraction / Difference (`+`)
    Sub,
    /// Multiplication / Intersection (`*`)
    Mul,
    /// Real Division (`/`)
    RealDiv,
    /// Integer Division (`div`)
    IntDiv,
    /// Modulo (`mod`)
    Mod,
    /// Remainder (`rem`)
    Rem,
    /// Exponentiaion (`**`)
    Exp,
    /// Bitwise/Logical And (`and`)
    And,
    /// Bitwise/Logical Or (`or`)
    Or,
    /// Bitwise/Logical Xor (`xor`)
    Xor,
    /// Bitshift Left (`shl`)
    Shl,
    /// Bitshift Right (`shr`)
    Shr,
    /// Implication (`=>`)
    Imply,
    /// Less Than (`<`)
    Lt,
    /// Less Than or Equal (`<=`)
    Le,
    /// Greater Than (`>`)
    Gt,
    /// Greater Than or Equal (`>=`)
    Ge,
    /// Equal (`=`)
    Equ,
    /// Not Equal (`~=`)
    NotEqu,
}

impl Operator {
    /// Converts a `TokenType` into the corresponding unary `Operator`
    pub fn from_unary(tok_type: &token::TokenType) -> Self {
        use token::TokenType;

        match tok_type {
            TokenType::Plus => Operator::UnaryPlus,
            TokenType::Minus => Operator::UnaryMinus,
            TokenType::Pound => Operator::NatCheat,
            TokenType::Not => Operator::Not,
            TokenType::Caret => Operator::PointerDeref,
            _ => panic!("No conversion into a unary operator for '{}'", tok_type),
        }
    }

    /// Converts a `TokenType` into the corresponding binary `Operator`
    pub fn from_binary(tok_type: &token::TokenType) -> Self {
        use token::TokenType;

        match tok_type {
            TokenType::Assign => Operator::NoOp,
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Sub,
            TokenType::Star => Operator::Mul,
            TokenType::Slash => Operator::RealDiv,
            TokenType::Div => Operator::IntDiv,
            TokenType::Mod => Operator::Mod,
            TokenType::Rem => Operator::Rem,
            TokenType::Exp => Operator::Exp,
            TokenType::And => Operator::And,
            TokenType::Or => Operator::Or,
            TokenType::Xor => Operator::Xor,
            TokenType::Shl => Operator::Shl,
            TokenType::Shr => Operator::Shr,
            TokenType::Imply => Operator::Imply,
            TokenType::Less => Operator::Lt,
            TokenType::LessEqu => Operator::Le,
            TokenType::Greater => Operator::Gt,
            TokenType::GreaterEqu => Operator::Ge,
            TokenType::Equ => Operator::Equ,
            TokenType::NotEqu => Operator::NotEqu,
            _ => panic!("No conversion into a binary operator for '{}'", tok_type),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::NoOp => f.write_str("nop"),
            Operator::UnaryPlus => f.write_str("+"),
            Operator::UnaryMinus => f.write_str("-"),
            Operator::NatCheat => f.write_str("#"),
            Operator::Not => f.write_str("not"),
            Operator::PointerDeref => f.write_str("^"),
            Operator::Add => f.write_str("+"),
            Operator::Sub => f.write_str("-"),
            Operator::Mul => f.write_str("*"),
            Operator::RealDiv => f.write_str("/"),
            Operator::IntDiv => f.write_str("div"),
            Operator::Mod => f.write_str("mod"),
            Operator::Rem => f.write_str("rem"),
            Operator::Exp => f.write_str("exp"),
            Operator::And => f.write_str("and"),
            Operator::Or => f.write_str("or"),
            Operator::Xor => f.write_str("xor"),
            Operator::Shl => f.write_str("shl"),
            Operator::Shr => f.write_str("shr"),
            Operator::Imply => f.write_str("=>"),
            Operator::Lt => f.write_str("<"),
            Operator::Le => f.write_str("<="),
            Operator::Gt => f.write_str(">"),
            Operator::Ge => f.write_str(">="),
            Operator::Equ => f.write_str("="),
            Operator::NotEqu => f.write_str("~="),
        }
    }
}
