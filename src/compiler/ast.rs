//! AST structure definitions
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;
use std::fmt;

/// Definition of an identifier
#[derive(Debug)]
pub struct Identifier {
    /// The token associated with the name
    pub token: Token,
    /// The name of the identifier
    pub name: String,
    /// The type for this identifier
    pub type_spec: TypeRef,
}

impl Identifier {
    pub fn new(token: &Token, type_spec: TypeRef, source: &str) -> Self {
        Self {
            token: token.clone(),
            name: token.location.get_lexeme(source).to_string(),
            type_spec,
        }
    }
}

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
    // Note: Some functions & procedures may be in the AST as pure references (in the middle of expressions)
    Reference {
        ident: Identifier,
    },
    Call {
        left: Box<Self>,
        op: Token,
        arg_list: Vec<Self>, // Parens may be omitted
        eval_type: TypeRef,
    },
    Dot {
        left: Box<Self>,
        ident: Identifier,
        eval_type: TypeRef,
    },
}

impl Expr {
    pub fn get_eval_type(&self) -> TypeRef {
        match self {
            Expr::BinaryOp { eval_type, .. } => *eval_type,
            Expr::UnaryOp { eval_type, .. } => *eval_type,
            Expr::Grouping { eval_type, .. } => *eval_type,
            Expr::Literal { eval_type, .. } => *eval_type,
            Expr::Call { eval_type, .. } => *eval_type,
            Expr::Dot { eval_type, .. } => *eval_type,
            Expr::Reference { ident } => ident.type_spec,
        }
    }
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
            Reference { ident } => f.write_fmt(format_args!("ref({})", ident.name)),
            Call {
                left,
                op: _,
                arg_list,
                eval_type: _,
            } => f.write_fmt(format_args!("{:?}({:?})", left, arg_list)),
            Dot {
                left,
                ident,
                eval_type: _,
            } => f.write_fmt(format_args!("(. {:?} {:?})", left, ident.name)), //f.write_fmt(format_args!("(. {:?} {:?})", left, name)),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    // Decls
    VarDecl {
        ident: Identifier,
        value: Option<Box<Expr>>,
        is_const: bool,
    },
    // Stmts
    Assign {
        var_ref: Box<Expr>,
        op: Token,
        value: Box<Expr>,
    },
    ProcedureCall {
        proc_ref: Box<Expr>,
    },
}

/// Mutable Visitor for a generated AST
pub trait ASTVisitor {
    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &mut Stmt);

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut Expr);
}
