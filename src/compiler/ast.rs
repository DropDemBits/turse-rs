//! AST structure definitions
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;
use std::fmt;

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

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::compiler::token::TokenType;
        use Expr::*;

        match self {
            BinaryOp {
                left,
                op,
                right,
                eval_type: _,
            } => f.write_fmt(format_args!(
                "({:?} {:?} {:?})",
                &op.token_type, left, right
            )),
            UnaryOp {
                op,
                right,
                eval_type: _,
            } => f.write_fmt(format_args!("({:?} {:?})", &op.token_type, right)),
            Grouping { expr, eval_type: _ } => f.write_fmt(format_args!("({:?})", expr)),
            Literal {
                value,
                eval_type: _,
            } => match &value.token_type {
                TokenType::StringLiteral(s) => f.write_fmt(format_args!("\"{}\"", s)),
                TokenType::CharLiteral(s) => f.write_fmt(format_args!("'{}'", s)),
                TokenType::IntLiteral(n) => f.write_fmt(format_args!("int({})", n)),
                TokenType::RealLiteral(n) => f.write_fmt(format_args!("real({})", n)),
                TokenType::BoolLiteral(b) => f.write_fmt(format_args!("bool({})", b)),
                TokenType::Nil => f.write_fmt(format_args!("nil")),
                _ => f.write_fmt(format_args!("unk({:?})", value)),
            },
        }
    }
}

#[derive(Debug)]
pub enum Stmt {}
