//! AST structure definitions
use crate::compiler::block::CodeBlock;
use crate::compiler::frontend::token::Token;
use crate::compiler::types::TypeRef;
use crate::compiler::Location;
use std::cell::RefCell;
use std::fmt;
use std::num::NonZeroU32;
use std::rc::Rc;

/// Type of the identifier instance
pub type IdentInstance = u16;

/// Definition of an identifier
#[derive(Debug, Clone)]
pub struct Identifier {
    /// The token associated with the name.
    pub token: Token,
    /// The name of the identifier.
    pub name: String,
    /// The type for this identifier.
    pub type_spec: TypeRef,
    /// If the identifier backs a storage unit not mutable at runtime.
    pub is_const: bool,
    /// If the identifier is the name for the type definition pointed to by
    /// `type_spec`.
    pub is_typedef: bool,
    /// If the identifier has been declared in a declaration statement, or
    /// has been defined by reference to the name (used to keep track of undefined
    /// identifiers).
    pub is_declared: bool,
    /// If the identifier references a value that can be evaluated at compile time.
    pub is_compile_eval: bool,
    /// The import index of the identifier.
    /// If None, the identifier is local to the scope.
    /// If Some, this is the index (plus one) to the corresponding entry into the
    /// current scope's import table.
    pub import_index: Option<NonZeroU32>,
    /// The instance of the identifier.
    ///
    /// Each redeclaration of an identifier or usage before declaration creates
    /// a new instance of the identifier, and this field distinguishes between
    /// the versions.
    ///
    /// The current scope stores all instances of the identifier so that the
    /// validator can grab the correct instance of the identifier, instead of
    /// always grabbing the latest declaration of the identifier.
    pub instance: IdentInstance,
}

impl Identifier {
    /// Creates a new identifier.
    /// Specifying an import index of '0' indicates that the identifier is not imported
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
            is_compile_eval: false,
            instance: 0, // All identifiers start with instance 0
        }
    }
}

/// Common expression node
#[derive(Clone)]
pub enum Expr {
    /// Empty expression, always evaluates to a type error.
    /// Only used as a placeholder in "init" expressions.
    Empty,
    /// Binary expression
    BinaryOp {
        /// Left operand for the binary operation
        left: Box<Self>,
        /// Operator of the binary expression
        op: Token,
        /// Right operand for the binary operation
        right: Box<Self>,
        /// The expression evaluation type
        eval_type: TypeRef,
        /// If the expression is compile-time evaluable
        is_compile_eval: bool,
        /// The span of the expression
        span: Location,
    },
    /// Unary expression
    UnaryOp {
        /// Operator of the unary expression
        op: Token,
        /// Operand for the unary operation
        right: Box<Self>,
        /// The expression evaluation type
        eval_type: TypeRef,
        /// If the expression is compile-time evaluable
        is_compile_eval: bool,
        /// The span of the expression
        span: Location,
    },
    /// Parenthetical expression
    Grouping {
        /// The inner grouping expression
        expr: Box<Self>,
        // The following are for caching purposes
        /// The expression evaluation type
        eval_type: TypeRef,
        /// If the expression is compile-time evaluable
        is_compile_eval: bool,
        /// The span of the expression
        span: Location,
    },
    /// Common literal value
    Literal {
        /// The literal value
        value: Token,
        /// The evaluation type
        eval_type: TypeRef,
    },
    // Note: Some functions & procedures may be in the AST as pure references (in the middle of expressions)
    // This is checked in the validator stage
    Reference {
        /// The identifier associated with this referenece
        ident: Identifier,
    },
    /// Funcion call expression
    Call {
        /// Expression evaluating to a reference
        left: Box<Self>,
        /// Token location
        op: Token,
        /// The argument list for the call
        arg_list: Vec<Self>, // Parens may be omitted, indicated by left's eval type
        /// The expression evaluation type
        eval_type: TypeRef,
        /// If the expression is compile-time evaluable
        is_compile_eval: bool,
        /// The span of the expression
        span: Location,
    },
    /// Dot/field expression
    Dot {
        /// Expression evaluating to a reference
        left: Box<Self>,
        /// Field to be referenced
        // While the type is an identifier, it is not a direct reference to an
        // identifier in the current scope
        field: Identifier,
        /// The expression evaluation type
        eval_type: TypeRef,
        /// If the expression is compile-time evaluable
        is_compile_eval: bool,
        /// The span of the expression
        span: Location,
    },
    /// "init" expression
    Init {
        /// Location of "init"
        init: Location,
        /// Expressions part of the "init"
        exprs: Vec<Self>,
        /// The span of the expression
        span: Location,
    },
}

impl Expr {
    /// Gets the evaluation type produced by the expression
    pub fn get_eval_type(&self) -> TypeRef {
        match self {
            Expr::Empty => TypeRef::TypeError,
            Expr::Init { .. } => TypeRef::TypeError, // Doesn't evaluate to anything
            Expr::BinaryOp { eval_type, .. } => *eval_type,
            Expr::UnaryOp { eval_type, .. } => *eval_type,
            Expr::Grouping { eval_type, .. } => *eval_type,
            Expr::Literal { eval_type, .. } => *eval_type,
            Expr::Call { eval_type, .. } => *eval_type,
            Expr::Dot { eval_type, .. } => *eval_type,
            Expr::Reference { ident } => ident.type_spec,
        }
    }

    /// Gets the span of the location
    pub fn get_span(&self) -> &Location {
        match self {
            Expr::Empty => panic!("Can't get span for empty expression"),
            Expr::Init { span, .. } => span,
            Expr::BinaryOp { span, .. } => span,
            Expr::UnaryOp { span, .. } => span,
            Expr::Grouping { span, .. } => span,
            Expr::Literal { value, .. } => &value.location,
            Expr::Call { span, .. } => span,
            Expr::Dot { span, .. } => span,
            Expr::Reference { ident, .. } => &ident.token.location,
        }
    }

    pub fn set_span(&mut self, at: Location) {
        match self {
            Expr::Empty => {}
            Expr::Init { span, .. } => *span = at,
            Expr::BinaryOp { span, .. } => *span = at,
            Expr::UnaryOp { span, .. } => *span = at,
            Expr::Grouping { span, .. } => *span = at,
            Expr::Literal { value, .. } => value.location = at,
            Expr::Call { span, .. } => *span = at,
            Expr::Dot { span, .. } => *span = at,
            Expr::Reference { ident } => ident.token.location = at,
        }
    }

    /// Gets the compile evaluability status of the expression
    pub fn is_compile_eval(&self) -> bool {
        match self {
            Expr::Empty => false,
            Expr::Init { .. } => false, // Never directly compilable at runtime
            Expr::BinaryOp {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::UnaryOp {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Grouping {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Literal { .. } => true, // Literals are already evaluated
            Expr::Call {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Dot {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Reference { ident } => ident.is_compile_eval,
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::compiler::frontend::token::TokenType;
        use Expr::*;

        match self {
            Empty => f.write_str("<empty>"),
            Init { exprs, .. } => f.write_fmt(format_args!("init({:?})", exprs)),
            BinaryOp {
                left, op, right, ..
            } => f.write_fmt(format_args!("({} {:?} {:?})", &op.token_type, left, right)),
            UnaryOp { op, right, .. } => {
                f.write_fmt(format_args!("({} {:?})", &op.token_type, right))
            }
            Grouping { expr, .. } => f.write_fmt(format_args!("({:?})", expr)),
            Literal { value, .. } => match &value.token_type {
                TokenType::StringLiteral(s) => f.write_fmt(format_args!("\"{}\"", s)),
                TokenType::CharLiteral(s) => f.write_fmt(format_args!("'{}'", s)),
                TokenType::NatLiteral(n) => f.write_fmt(format_args!("nat({})", n)),
                TokenType::IntLiteral(n) => f.write_fmt(format_args!("int({})", n)),
                TokenType::RealLiteral(n) => f.write_fmt(format_args!("real({})", n)),
                TokenType::BoolLiteral(b) => f.write_fmt(format_args!("bool({})", b)),
                TokenType::Nil => f.write_fmt(format_args!("nil")),
                _ => f.write_fmt(format_args!("unk({:?})", value)),
            },
            Reference { ident } => f.write_fmt(format_args!("ref({})", ident.name)),
            Call { left, arg_list, .. } => f.write_fmt(format_args!("{:?}({:?})", left, arg_list)),
            Dot { left, field, .. } => f.write_fmt(format_args!("(. {:?} {:?})", left, field.name)),
        }
    }
}

/// Common statement node
#[derive(Debug, Clone)]
pub enum Stmt {
    // Decls
    /// Variable & Constant declaration
    VarDecl {
        /// The identifier(s) declared
        idents: Vec<Identifier>,
        /// The type spec for all of the identifiers
        type_spec: TypeRef,
        /// The (semi-optional) initialization value
        value: Option<Box<Expr>>,
        /// If the declare is for a const declaration
        is_const: bool,
    },
    /// `type` statement declaration
    TypeDecl {
        /// The identifier associated with this type declare
        ident: Identifier,
        /// Resolved type for a forward type declare
        resolved_type: Option<TypeRef>,
        /// If the identifier actually declares a new identifier
        is_new_def: bool,
    },
    // Stmts
    /// Simple & Compound assignment expression
    Assign {
        /// The variable reference expression
        var_ref: Box<Expr>,
        /// The assignment operation
        op: Token,
        /// The value to assign
        value: Box<Expr>,
    },
    /// Procedure or function call
    ProcedureCall {
        /// The reference to the procedure or function variable
        proc_ref: Box<Expr>,
    },
    /// Block of statements
    Block {
        /// The associated code block
        block: Rc<RefCell<CodeBlock>>,
        /// Statements as part of the block
        stmts: Vec<Self>,
    },
}

/// Mutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait VisitorMut<St, Ex> {
    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut Expr) -> Ex;
}

/// Immutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait Visitor<St, Ex> {
    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &Expr) -> Ex;
}
