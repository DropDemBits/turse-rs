//! Everything related to symbols.
//! `SymbolTable` construction with respect to scoping rules occurs in `toc_hir_lowering`.

use toc_span::Spanned;

pub use crate::ids::{DefId, LocalDefId};

/// Information associated with a `LocalDefId` or `DefId`.
#[derive(Debug, PartialEq, Eq)]
pub struct DefInfo {
    /// The name of the definition, along with the span of the identifer.
    pub name: Spanned<String>,
    /// The kind of symbol.
    pub kind: SymbolKind,
    // ...
    // probably include additional definition information such as
    // - bound to a register (unsure?)
    // - pervasive (maybe left over from construction)
    // - mutability/access (const, var, type/none)?
    // - is part of a forward resolution chain?
}

#[derive(Debug)]
pub struct Symbol {
    /// Name of the symbol.
    pub name: String,
    /// The kind of symbol.
    pub kind: SymbolKind,
    /// If the symbol is pervasive, and can implicitly cross import boundaries.
    pub is_pervasive: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    /// The symbol is undeclared at the point of definition.
    Undeclared,
    /// The symbol is a normal declaration at the point of definition.
    Declared,
    /// The symbol is a forward reference to a later declaration.
    Forward,
    /// The symbol is a resolution of a forward declaration, with a `DefId`
    /// pointing back to the original forward declaration symbol.
    Resolved(DefId),
}
