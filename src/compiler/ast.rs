//! AST structure definitions
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;

#[derive(Debug)]
pub enum Expr {
    BinaryOp {
        left: Box<Self>,
        op: Token,
        right: Box<Self>,
        types: (TypeRef, TypeRef),
    },
    UnaryOp {
        op: Token,
        right: Box<Self>,
        types: TypeRef,
    },
    Grouping {
        expr: Box<Self>,
        types: TypeRef,
    },
    Literal {
        value: Token,
        types: TypeRef,
    },
}

#[derive(Debug)]
pub enum Stmt {}
