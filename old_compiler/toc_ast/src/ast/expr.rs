//! Expr AST Nodes
use crate::ast::ident::{IdentRef, RefKind};
use crate::ast::types::Type;
use crate::types::TypeRef;
use toc_core::Location;

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

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => f.write_str("+"),
            BinaryOp::Sub => f.write_str("-"),
            BinaryOp::Mul => f.write_str("*"),
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
            BinaryOp::NotEqual => f.write_str("not="),
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

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
#[derive(Debug, Clone, PartialEq)]
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

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    pub name: String,
    pub type_spec: TypeRef,
    pub ref_kind: RefKind,
}

/// Expression Node Kind
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Error expression, always evaluates to a type error.
    Error,
    /// Parentheses, only used to preserve operand order in AST dumping
    Parens {
        /// Inner parentheses expression
        inner: Box<Expr>,
    },
    /// Binary expression
    BinaryOp {
        /// Left operand for the binary operation
        left: Box<Expr>,
        /// Operator of the binary expression
        op: (BinaryOp, Location),
        /// Right operand for the binary operation
        right: Box<Expr>,
    },
    /// Unary expression
    UnaryOp {
        /// Operator of the unary expression
        op: (UnaryOp, Location),
        /// Operand for the unary operation
        right: Box<Expr>,
    },
    /// Common literal value
    Literal {
        /// The literal value
        value: Literal,
    },
    // Note: Some functions & procedures may be in the AST as pure references (in the middle of expressions)
    // This is checked in the validator stage
    Reference {
        /// The identifier associated with this referenece
        ident: IdentRef,
    },
    /// Funcion call expression
    Call {
        /// Expression evaluating to a reference
        left: Box<Expr>,
        /// Token location
        paren_at: Location,
        /// The argument list for the call
        arg_list: Vec<Expr>, // Parens may be omitted, indicated by left's eval type
    },
    /// Dot/field expression
    Dot {
        /// Expression evaluating to a reference
        left: Box<Expr>,
        /// Field to be referenced.
        ///
        /// A tuple of the field def, and the location of the field
        field: (FieldDef, Location),
    },
    /// Arrow expression
    Arrow {
        /// Expression evaluating to a reference
        left: Box<Expr>,
        /// Field to be referenced.
        ///
        /// A tuple of the field def, and the location of the field
        field: (FieldDef, Location),
    },
    /// "init" expression
    Init {
        /// Location of "init"
        init: Location,
        /// Expressions part of the "init"
        exprs: Vec<Expr>,
    },
    /// "Indirect" expression
    Indirect {
        /// The type to read at the address.
        indirect_type: Box<Type>,
        /// The address section of the indirect expression
        addr: Box<Expr>,
    },
}

/// Common expression node
///
/// `eval_type` is the type produced after evaluating the expression.
/// For reference and dot expressions, the evaluation type may be different from the identifier type spec
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    /// The expression evaluation type
    pub eval_type: TypeRef,
    /// If the expression is compile-time evaluable
    pub is_compile_eval: bool,
    /// The span of the expression
    pub span: Location,
}

impl Expr {
    /// Gets the evaluation type produced by the expression
    pub fn get_eval_type(&self) -> TypeRef {
        self.eval_type
    }

    /// Gets the span of the location
    pub fn get_span(&self) -> &Location {
        &self.span
    }

    pub fn set_span(&mut self, at: Location) {
        self.span = at;
    }

    /// Gets the compile evaluability status of the expression
    pub fn is_compile_eval(&self) -> bool {
        self.is_compile_eval
    }
}

mod pretty_print {
    use super::{Expr, ExprKind};
    use crate::pretty_print;
    use std::fmt;

    impl fmt::Display for ExprKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                ExprKind::Error => f.write_str("<error>"),
                ExprKind::Init { exprs, .. } => {
                    f.write_str("init(")?;
                    pretty_print::print_list(f, exprs.iter())?;
                    f.write_str(")")
                }
                ExprKind::Indirect {
                    indirect_type,
                    addr,
                    ..
                } => f.write_fmt(format_args!("[{}] @ ({})", indirect_type, addr)),
                ExprKind::Parens { inner } => f.write_fmt(format_args!("({})", inner)),
                ExprKind::BinaryOp {
                    left, op, right, ..
                } => f.write_fmt(format_args!("{0} {1} {2}", left, &op.0, right)),
                ExprKind::UnaryOp { op, right, .. } => {
                    if op.0 == super::UnaryOp::Not {
                        f.write_fmt(format_args!("{0} {1}", &op.0, right))
                    } else {
                        f.write_fmt(format_args!("{0}{1}", &op.0, right))
                    }
                }
                ExprKind::Literal { value, .. } => f.write_fmt(format_args!("{}", value)),
                ExprKind::Reference { ident, .. } => f.write_fmt(format_args!("ref({})", ident)),
                ExprKind::Call { left, arg_list, .. } => {
                    f.write_fmt(format_args!("{}(", left))?;
                    pretty_print::print_list(f, arg_list.iter())?;
                    f.write_str(")")
                }
                ExprKind::Dot { left, field, .. } => {
                    f.write_fmt(format_args!("{} . {}", left, field.0.name))
                }
                ExprKind::Arrow { left, field, .. } => {
                    f.write_fmt(format_args!("{} -> {}", left, field.0.name))
                }
            }
        }
    }

    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.kind.fmt(f)
        }
    }
}
