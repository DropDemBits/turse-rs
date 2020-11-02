//! Identifier related AST structures
use crate::types::TypeRef;
use toc_core::Location;

/// Identifier id, associated with a unique declaration of an identifier
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq)]
pub struct IdentId(pub u32);

/// A reference to an identifier in the AST.
///
/// Just a named reference to both
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct IdentRef {
    /// Id of the referenced identifier
    pub id: IdentId,
    /// Location of the reference
    pub location: Location,
}

impl IdentRef {
    pub fn new(id: IdentId, location: Location) -> Self {
        Self { id, location }
    }
}

/// Variant of identifier reference
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RefKind {
    /// Reference to a storage location mutable at runtime
    Var,
    /// Reference to a storage location not mutable at runtime
    Const,
    /// Reference to a type, not associated with any storage locations
    Type,
}

/// Definition of an identifier
#[derive(Debug, Clone)]
pub struct Identifier {
    /// The declaration location of this identifier in the source code.
    pub location: Location,
    /// The name of the identifier.
    pub name: String,
    /// The type for this identifier.
    pub type_spec: TypeRef,
    /// The reference kind of this identifier
    pub ref_kind: RefKind,
    /// If the identifier has been declared in a declaration statement, or
    /// has been defined by reference to the name (used to keep track of undefined
    /// identifiers).
    pub is_declared: bool,
    /// If the identifier references a value that can be evaluated at compile time.
    pub is_compile_eval: bool,
    /// If the identifier is pervasive and is able to be implicitly imported into
    /// child scopes
    pub is_pervasive: bool,
    /// The number of times this identifier has been used
    pub usages: usize,
}

impl Identifier {
    /// Creates a new identifier.
    /// Specifying an import index of '0' indicates that the identifier is not imported
    /// `token` Location of the reference token
    pub fn new(
        location: Location,
        type_spec: TypeRef,
        name: String,
        ref_kind: RefKind,
        is_declared: bool,
        is_pervasive: bool,
    ) -> Self {
        Self {
            location,
            name,
            type_spec,
            ref_kind,
            is_declared,
            is_pervasive,
            is_compile_eval: false,
            usages: 0,
        }
    }
}

mod pretty_print {
    use super::{IdentId, IdentRef, Identifier};
    use std::fmt;

    impl fmt::Display for IdentId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_fmt(format_args!("id:{}", self.0))
        }
    }

    impl fmt::Display for IdentRef {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.id.fmt(f)
        }
    }

    impl fmt::Display for Identifier {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_fmt(format_args!(
                "{{ {} ty: {}, used: {}, ",
                self.name, self.type_spec, self.usages
            ))?;

            match self.ref_kind {
                super::RefKind::Var => f.write_str("var")?,
                super::RefKind::Const => f.write_str("const")?,
                super::RefKind::Type => f.write_str("tydef")?,
            }

            let props = [
                ("decl", &self.is_declared),
                ("pervasive", &self.is_pervasive),
                ("comp_eval", &self.is_compile_eval),
            ];

            for (name, is_present) in props.iter() {
                if **is_present {
                    f.write_str(" ")?;
                    f.write_str(name)?;
                }
            }

            f.write_str(" }\n")
        }
    }
}
