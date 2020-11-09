//! Type AST Nodes
use crate::ast::expr::Expr;
use crate::types::{ParamInfo, PrimitiveType, TypeRef};

use toc_core::Location;

/// Sequence Size
#[derive(Debug, Clone)]
pub enum SeqSize {
    /// Size is based on other factors
    Any,
    /// Size is based on an expression
    Sized(Box<Expr>),
}

/// Type Node Variants.
/// Most of these type variants have a corresponding struction in `toc_ast::types::Type`
#[derive(Debug, Clone)]
pub enum TypeKind {
    /// Error type, for unparsable types
    /// (will be converted into a `TypeRef::TypeError`)
    Error,
    /// Primitive type, with the final primitive type
    Primitive(PrimitiveType),
    /// Sized CharN node, with a given sequence size
    CharN { size: SeqSize },
    /// Sized StringN node, with a given sequence size
    StringN { size: SeqSize },
    /// Reference to a named type.
    /// Arbitrary expression type, and may not be a valid reference type
    Reference { ref_expr: Box<Expr> },
    /// Forward reference to a type
    Forward,
    /// Pointer to another Type
    Pointer { to: Box<Type>, is_unchecked: bool },
    /// Set type
    Set { range: Box<Type> },
    /// Enumerated Type, with fields in order of declaration
    Enum { fields: Vec<String> },
    /// Inclusive range type.
    /// Does not contain the base type or the range size.
    Range { start: Box<Expr>, end: SeqSize },
    /// Function / Procedure Type
    Function {
        params: Option<Vec<(Box<Type>, ParamInfo)>>,
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

impl Type {
    pub fn new(kind: TypeKind, span: Location) -> Self {
        Self {
            kind,
            type_ref: None,
            span,
        }
    }

    pub fn type_ref(&self) -> &TypeRef {
        self.type_ref.as_ref().expect("unresolved type")
    }
}

mod pretty_print {
    use super::{SeqSize, Type, TypeKind};
    use crate::pretty_print;
    use std::fmt;

    impl fmt::Display for SeqSize {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                SeqSize::Any => f.write_str("*"),
                SeqSize::Sized(expr) => expr.fmt(f),
            }
        }
    }

    impl fmt::Display for TypeKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                TypeKind::Error => f.write_str("{ error }")?,
                TypeKind::Reference { ref_expr } => {
                    f.write_fmt(format_args!("{{ ref_expr {} }}", ref_expr))?
                }
                TypeKind::Range { start, end } => {
                    f.write_fmt(format_args!("{{ range {} .. {} }}", start, end))?;
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
                TypeKind::Forward => {
                    f.write_str("{ forward }")?;
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

                        let mut peek_enumerate = params.iter().peekable();

                        while let Some((tyref, info)) = peek_enumerate.next() {
                            f.write_fmt(format_args!("{} : {}", info, tyref))?;

                            if peek_enumerate.peek().is_some() {
                                f.write_str(", ")?;
                            }
                        }

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
