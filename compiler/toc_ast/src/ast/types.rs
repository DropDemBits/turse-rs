//! Type AST Nodes
use crate::ast::expr::Expr;
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
    /// Arbitrary expression type, and may not be a valid reference type
    Reference { ref_expr: Box<Expr> },
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

mod pretty_print {
    use super::{Type, TypeKind};
    use crate::pretty_print;
    use std::fmt;

    impl fmt::Display for TypeKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                TypeKind::Error => f.write_str("{ error }")?,
                TypeKind::Reference { ref_expr } => {
                    f.write_fmt(format_args!("{{ ref_expr {} }}", ref_expr))?
                }
                TypeKind::Range { start, end } => {
                    f.write_str("{ range ")?;

                    if let Some(end) = end {
                        f.write_fmt(format_args!("{} .. {} ", start, end))?;
                    } else {
                        f.write_fmt(format_args!("{} .. * ", start))?;
                    }

                    f.write_str(" }}")?;
                }
                TypeKind::Array {
                    ranges,
                    element_type,
                    is_flexible,
                    ..
                } => {
                    f.write_str("{ ")?;
                    if *is_flexible {
                        f.write_str("flexible ")?;
                    }
                    f.write_str("array ")?;
                    pretty_print::print_list(f, ranges.iter())?;
                    f.write_fmt(format_args!(" of {} }}", element_type))?;
                }
                TypeKind::Enum { fields } => {
                    f.write_str("{ enum ( ")?;

                    for name in fields {
                        f.write_fmt(format_args!("{} ", name))?;
                    }

                    f.write_str(")")?;
                }
                TypeKind::Set { range } => f.write_fmt(format_args! {"{{ set of {} }}", range})?,
                TypeKind::Forward { is_resolved } => {
                    if *is_resolved {
                        f.write_str("{ resolved forward }")?;
                    } else {
                        f.write_str("{ forward }")?;
                    }
                }
                TypeKind::Pointer { to, is_unchecked } => {
                    if *is_unchecked {
                        f.write_fmt(format_args!("{{ unchecked pointer to {} }}", to))?
                    } else {
                        f.write_fmt(format_args!("{{ pointer to {} }}", to))?
                    }
                }
                TypeKind::Function { params, result } => {
                    if result.is_some() {
                        f.write_str("{ function ")?;
                    } else {
                        f.write_str("{ procedure ")?;
                    }

                    if let Some(params) = params {
                        f.write_str("(")?;
                        pretty_print::print_list(f, params.iter())?;
                        f.write_str(") ")?;
                    }

                    if let Some(result) = result {
                        f.write_fmt(format_args!("-> {} ", result))?
                    }

                    f.write_str("}")?;
                }
                TypeKind::Primitive(p) => f.write_fmt(format_args!("{{ prim {:?} }}", p))?,
                TypeKind::CharN { size } => f.write_fmt(format_args!("{{ char(*) {} }}", size))?,
                TypeKind::StringN { size } => {
                    f.write_fmt(format_args!("{{ string(*) {} }}", size))?
                }
            }

            Ok(())
        }
    }

    impl fmt::Display for Type {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.kind.fmt(f)
        }
    }
}
