//! Everything related to symbols.
//! `SymbolTable` construction with respect to scoping rules occurs in `toc_hir_lowering`.

use la_arena::ArenaMap;
use toc_span::Spanned;

pub use crate::ids::{DefId, LocalDefId};
use crate::{
    ids::{ItemId, LocalDefIndex},
    stmt::BodyStmt,
};

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
    /// The symbol is a forward reference to a later declaration,
    /// with a [`LocaLDefId`] pointing to the resolving definition.
    Forward(ForwardKind, Option<LocalDefId>),
    /// The symbol is a resolution of a forward declaration.
    Resolved(ForwardKind),
}

/// Disambiguates between different forward declaration kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForwardKind {
    /// `type` forward declaration
    Type,
    /// `procedure` forward declaration
    // Only constructed in tests right now
    _Procedure,
}

/// Any HIR node that contains a definition
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefOwner {
    Item(ItemId),
    Stmt(BodyStmt),
}

/// Mapping between a [`LocalDefId`] and the corresponding [`DefOwner`]
#[derive(Debug, Default, PartialEq, Eq)]
pub struct DefTable {
    def_owners: ArenaMap<LocalDefIndex, DefOwner>,
}

impl DefTable {
    pub fn add_owner(&mut self, def_id: LocalDefId, owner: DefOwner) {
        self.def_owners.insert(def_id.0, owner);
    }

    pub fn get_owner(&self, def_id: LocalDefId) -> Option<DefOwner> {
        self.def_owners.get(def_id.0).copied()
    }
}
