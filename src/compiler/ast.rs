//! AST structure definitions
use crate::compiler::block::CodeBlock;
use crate::compiler::token::Token;
use crate::compiler::types::TypeRef;
use std::cell::RefCell;
use std::fmt;
use std::num::NonZeroU32;
use std::rc::Rc;

/// Definition of an identifier
#[derive(Debug, Clone)]
pub struct Identifier {
    /// The token associated with the name
    pub token: Token,
    /// The name of the identifier
    pub name: String,
    /// The type for this identifier
    pub type_spec: TypeRef,
    /// If the identifier backs a storage unit not mutable at runtime
    pub is_const: bool,
    /// If the identifier is the name for the type definition pointed to by
    /// `type_spec`
    pub is_typedef: bool,
    /// If the identifier has been declared in a declaration statement, or
    /// has been defined by reference to the name (used to keep track of undefined
    /// identifiers)
    pub is_declared: bool,
    /// The import index of the identifier
    /// If None, the identifier is local to the scope
    /// If Some, this is the index (plus one) to the corresponding entry into the
    /// current scope's import table
    pub import_index: Option<NonZeroU32>,
}

impl Identifier {
    /// Creates a new identifier
    /// `token` Location of the reference token
    pub fn new(
        token: Token,
        type_spec: TypeRef,
        name: String,
        is_const: bool,
        is_typedef: bool,
        is_declared: bool,
        import_index: u32,
    ) -> Self {
        Self {
            token,
            name,
            type_spec,
            is_const,
            is_typedef,
            is_declared,
            import_index: NonZeroU32::new(import_index),
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
        // Token is provided in case an error wants to be reported at the token location
        // String is provided as type information is only needed during type validation & compilation
        field: (Token, String),
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
                left, op, right, ..
            } => f.write_fmt(format_args!(
                "({:?} {:?} {:?})",
                &op.token_type, left, right
            )),
            UnaryOp { op, right, .. } => {
                f.write_fmt(format_args!("({:?} {:?})", &op.token_type, right))
            }
            Grouping { expr, eval_type: _ } => f.write_fmt(format_args!("({:?})", expr)),
            Literal { value, .. } => match &value.token_type {
                TokenType::StringLiteral(s) => f.write_fmt(format_args!("\"{}\"", s)),
                TokenType::CharLiteral(s) => f.write_fmt(format_args!("'{}'", s)),
                TokenType::IntLiteral(n) => f.write_fmt(format_args!("int({})", n)),
                TokenType::RealLiteral(n) => f.write_fmt(format_args!("real({})", n)),
                TokenType::BoolLiteral(b) => f.write_fmt(format_args!("bool({})", b)),
                TokenType::Nil => f.write_fmt(format_args!("nil")),
                _ => f.write_fmt(format_args!("unk({:?})", value)),
            },
            Reference { ident } => f.write_fmt(format_args!("ref({})", ident.name)),
            Call { left, arg_list, .. } => f.write_fmt(format_args!("{:?}({:?})", left, arg_list)),
            Dot {
                left,
                field: (_tok, name),
                ..
            } => f.write_fmt(format_args!("(. {:?} {:?})", left, name)),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    // Decls
    VarDecl {
        idents: Vec<Identifier>,
        type_spec: TypeRef,
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
    Block {
        block: Rc<RefCell<CodeBlock>>,
    },
}

/// Mutable Visitor for a generated AST
pub trait ASTVisitor {
    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &mut Stmt);

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut Expr);
}
