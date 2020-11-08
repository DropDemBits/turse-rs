//! Type AST Nodes
use crate::ast::expr::Expr;
use crate::ast::ident::IdentRef;
use crate::types::{ParamDef, PrimitiveType, TypeRef};

use toc_core::Location;

/// Type Node Variants.
/// Most of these type variants have a corresponding struction in `toc_ast::types::Type`
#[derive(Debug, Clone)]
pub enum TypeKind {
    /// Error type, for unparsable types
    /// (will be converted into a `TypeRef::TypeError`)
    Error,
    /// Primitive type, with the final primitive type
    Primitive(PrimitiveType),
    /// Sized CharN node, with an arbitrary compile-time expression
    CharN { size: Box<Expr> },
    /// Sized StringN node, with an arbitrary compile-time expression
    StringN { size: Box<Expr> },
    /// Reference to a named type.
    Reference { ident: IdentRef },
    /// Forward reference to a type
    Forward { is_resolved: bool },
    /// Pointer to another Type
    Pointer { to: Box<Type>, is_unchecked: bool },
    /// Set type
    Set { range: Box<Type> },
    /// Enumerated Type, with fields in order of declaration
    Enum { fields: Vec<String> },
    /// Inclusive range type.
    /// Does not contain the base type or the range size.
    Range {
        start: Box<Expr>,
        end: Option<Box<Expr>>,
    },
    /// Function / Procedure Type
    Function {
        params: Option<Vec<ParamDef>>,
        result: Option<Box<Type>>,
    },
    /// Array type
    Array {
        ranges: Vec<Type>,
        element_type: Box<Type>,
        is_flexible: bool,
        is_init_sized: bool,
    },
}

/// Type Node
#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    /// `TypeRef` that this type corresponds to
    pub type_ref: Option<TypeRef>,
    /// The span of the type node
    pub span: Location,
}
