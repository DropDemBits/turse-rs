//! AST structure definitions
use crate::block::CodeBlock;
use crate::types::TypeRef;
use toc_core::Location;

use std::cell::RefCell;
use std::fmt;
use std::num::NonZeroU32;
use std::rc::Rc;

/// Type of the identifier instance
pub type IdentInstance = u16;

/// Definition of an identifier
#[derive(Debug, Clone)]
pub struct Identifier {
    /// The location of this identifier reference in the source code.
    pub location: Location,
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
        location: Location,
        type_spec: TypeRef,
        name: String,
        is_const: bool,
        is_typedef: bool,
        is_declared: bool,
        import_index: u32,
    ) -> Self {
        Self {
            location,
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

/// Binary operators
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    /// Addition / Set Union / String Concatenation (`+`)
    Add,
    /// Subtraction / Set Subtraction (`*`)
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
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => f.write_str("+"),
            BinaryOp::Sub => f.write_str("-"),
            BinaryOp::Mul => f.write_str("mul"),
            BinaryOp::Div => f.write_str("div"),
            BinaryOp::RealDiv => f.write_str("/"),
            BinaryOp::Mod => f.write_str("mod"),
            BinaryOp::Rem => f.write_str("rem"),
            BinaryOp::Exp => f.write_str("**"),
            BinaryOp::And => f.write_str("and"),
            BinaryOp::Or => f.write_str("or"),
            BinaryOp::Xor => f.write_str("xor"),
            BinaryOp::Shl => f.write_str("shl"),
            BinaryOp::Shr => f.write_str("shr"),
            BinaryOp::Less => f.write_str("<"),
            BinaryOp::LessEq => f.write_str("<="),
            BinaryOp::Greater => f.write_str(">"),
            BinaryOp::GreaterEq => f.write_str(">="),
            BinaryOp::Equal => f.write_str("="),
            BinaryOp::NotEqual => f.write_str("not ="),
            BinaryOp::In => f.write_str("in"),
            BinaryOp::NotIn => f.write_str("not in"),
            BinaryOp::Imply => f.write_str("=>"),
            BinaryOp::Arrow => f.write_str("->"),
            BinaryOp::Dot => f.write_str("."),
        }
    }
}

/// Unary operators
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    /// Binary/boolean negation operator (`not`)
    Not,
    /// Nat cheat (`#`)
    NatCheat,
    /// Integer identity (`+`)
    Identity,
    /// Integer negation (`-`)
    Negate,
    /// Pointer dereferencing operator (`^`)
    Deref,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => f.write_str("not"),
            UnaryOp::NatCheat => f.write_str("#"),
            UnaryOp::Identity => f.write_str("+"),
            UnaryOp::Negate => f.write_str("-"),
            UnaryOp::Deref => f.write_str("^"),
        }
    }
}

/// Literal values
#[derive(Debug, Clone)]
pub enum Literal {
    /// String sequence (`"abcd"`)
    StrSequence(String),
    /// Character sequence (`'abcd'`)
    CharSequence(String),
    /// Natural literal (`123456`)
    Nat(u64),
    /// Integer literal (`-123456`)
    Int(i64),
    /// Real literal (`0.1234`)
    Real(f64),
    /// Boolean Literal (`true` or `false`)
    Bool(bool),
    /// Nil literal
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::StrSequence(s) => f.write_fmt(format_args!("\"{}\"", s)),
            Literal::CharSequence(s) => f.write_fmt(format_args!("'{}'", s)),
            Literal::Nat(n) => f.write_fmt(format_args!("nat({})", n)),
            Literal::Int(n) => f.write_fmt(format_args!("int({})", n)),
            Literal::Real(n) => f.write_fmt(format_args!("real({})", n)),
            Literal::Bool(b) => f.write_fmt(format_args!("bool({})", b)),
            Literal::Nil => f.write_fmt(format_args!("nil")),
        }
    }
}

/// Common expression node
///
/// `eval_type` is the type produced after evaluating the expression.
/// For reference and dot expressions, the evaluation type may be different from the identifier type spec
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
        op: (BinaryOp, Location),
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
        op: (UnaryOp, Location),
        /// Operand for the unary operation
        right: Box<Self>,
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
        value: Literal,
        /// The evaluation type
        eval_type: TypeRef,
        /// The span of the expression
        span: Location,
    },
    // Note: Some functions & procedures may be in the AST as pure references (in the middle of expressions)
    // This is checked in the validator stage
    Reference {
        /// The identifier associated with this referenece
        ident: Identifier,
        /// The expression evaluation type
        eval_type: TypeRef,
    },
    /// Funcion call expression
    Call {
        /// Expression evaluating to a reference
        left: Box<Self>,
        /// Token location
        paren_at: Location,
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
    /// "Indirect" expression
    Indirect {
        // The reference type in the indirect expression.
        // If None, the type is a primitive and is contained in `eval_type`.
        // Must be a type reference.
        reference: Option<Box<Expr>>,
        /// The address section of the indirect expression
        addr: Box<Expr>,
        /// The type to read at the address.
        /// If TypeRef::Unknown, type is derived from the reference expression
        eval_type: TypeRef,
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
            Expr::Indirect { eval_type, .. } => *eval_type,
            Expr::BinaryOp { eval_type, .. } => *eval_type,
            Expr::UnaryOp { eval_type, .. } => *eval_type,
            Expr::Literal { eval_type, .. } => *eval_type,
            Expr::Call { eval_type, .. } => *eval_type,
            Expr::Dot { eval_type, .. } => *eval_type,
            Expr::Reference { eval_type, .. } => *eval_type,
        }
    }

    /// Gets the span of the location
    pub fn get_span(&self) -> &Location {
        match self {
            Expr::Empty => panic!("Can't get span for empty expression"),
            Expr::Indirect { span, .. } => span,
            Expr::Init { span, .. } => span,
            Expr::BinaryOp { span, .. } => span,
            Expr::UnaryOp { span, .. } => span,
            Expr::Literal { span, .. } => span,
            Expr::Call { span, .. } => span,
            Expr::Dot { span, .. } => span,
            Expr::Reference { ident, .. } => &ident.location,
        }
    }

    pub fn set_span(&mut self, at: Location) {
        match self {
            Expr::Empty => {}
            Expr::Init { span, .. } => *span = at,
            Expr::Indirect { span, .. } => *span = at,
            Expr::BinaryOp { span, .. } => *span = at,
            Expr::UnaryOp { span, .. } => *span = at,
            Expr::Literal { span, .. } => *span = at,
            Expr::Call { span, .. } => *span = at,
            Expr::Dot { span, .. } => *span = at,
            Expr::Reference { ident, .. } => ident.location = at,
        }
    }

    /// Gets the compile evaluability status of the expression
    pub fn is_compile_eval(&self) -> bool {
        match self {
            Expr::Empty => false,
            Expr::Init { .. } => false, // Never directly evaluable at compile-time
            Expr::Indirect { .. } => false, // Never compile-time evaluable
            Expr::BinaryOp {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::UnaryOp {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Literal { value, .. } => !matches!(&value, Literal::Nil), // Literals (except nil) are already evaluated
            Expr::Call {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Dot {
                is_compile_eval, ..
            } => *is_compile_eval,
            Expr::Reference { ident, .. } => ident.is_compile_eval,
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;

        match self {
            Empty => f.write_str("<empty>"),
            Init { exprs, .. } => f.write_fmt(format_args!("init({:?})", exprs)),
            Indirect {
                eval_type,
                reference,
                ..
            } => f.write_fmt(format_args!("({:?} @ ({:?}))", eval_type, reference)),
            BinaryOp {
                left, op, right, ..
            } => f.write_fmt(format_args!("({} {:?} {:?})", &op.0, left, right)),
            UnaryOp { op, right, .. } => f.write_fmt(format_args!("({} {:?})", &op.0, right)),
            Literal { value, .. } => f.write_fmt(format_args!("{}", value)),
            Reference { ident, .. } => f.write_fmt(format_args!("ref({:#?})", ident)),
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
    /// `type` statement declaration.
    /// The type_spec of `ident` is the declared type
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
        /// The (optional) assignment operation
        op: Option<BinaryOp>,
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
    /// Starts a visit to the tree. Allows the visitor to set itself up
    fn start_visit(&mut self) {}

    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &mut Expr) -> Ex;

    /// Ends a visit to the tree. Allows the visitor to perform any cleanup
    fn end_visit(&mut self) {}
}

/// Immutable Visitor for a generated AST.
/// `St` is the type returned from visiting statements, and `Ex` is the type
/// returned from visiting expressions.
pub trait Visitor<St, Ex> {
    /// Starts a visit to the tree. Allows the visitor to set itself up
    fn start_visit(&mut self) {}

    /// Visit a single statement in the tree
    fn visit_stmt(&mut self, stmt: &Stmt) -> St;

    /// Visit an expression in the tree
    fn visit_expr(&mut self, expr: &Expr) -> Ex;

    /// Ends a visit to the tree. Allows the visitor to perform any cleanup
    fn end_visit(&mut self) {}
}
