//! AST structure definitions
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;

#[derive(Debug)]
pub enum Expr {
    BinaryOp {
        left: Box<Self>,
        op: Token,
        right: Box<Self>,
        eval_type: TypeRef,
    },
    UnaryOp {
        op: Token,
        right: Box<Self>,
        eval_type: TypeRef,
    },
    Grouping {
        expr: Box<Self>,
        eval_type: TypeRef,
    },
    Literal {
        value: Token,
        eval_type: TypeRef,
    },
}

#[derive(Debug)]
pub enum Stmt {}
