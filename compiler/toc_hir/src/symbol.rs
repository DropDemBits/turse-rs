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
    /// The name of the definition, along with the span of the identifier.
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
    /// The symbol is declared, but is only usable in certain contexts
    LimitedDeclared(LimitedKind),
    /// The symbol is a forward reference to a later declaration,
    /// with a [`LocalDefId`] pointing to the resolving definition.
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

/// Specificity on why a symbol is limited in visibility
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LimitedKind {
    /// Only usable in post-condition statements
    PostCondition,
}

/// Any HIR node that contains a definition
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefOwner {
    /// Owned directly by an `Item`
    Item(ItemId),
    /// Parameter on a given `Item`
    ItemParam(ItemId, LocalDefId),
    /// Owned by a `Stmt`
    Stmt(BodyStmt),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Var,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// A binding to a storage location (e.g. from [`ConstVar`](crate::item::ConstVar))
    Storage(Mutability),
    /// A binding to a register
    Register(Mutability),
    /// Binding to a type
    Type,
    /// Binding to a module
    Module,
    /// Binding to a subprogram
    Subprogram(SubprogramKind),

    /// A binding that isn't attached to anything
    Undeclared,
}

impl BindingKind {
    // Undeclared bindings are treated as equivalent to all of the
    // other binding types, for error reporting purposes.
    //
    // While it's still an invalid state, it can theoretically be
    // any valid binding kind.

    /// If this is a binding to a value reference (mut or immutable, storage or register)
    pub fn is_ref(self) -> bool {
        matches!(
            self,
            Self::Undeclared | Self::Storage(_) | Self::Register(_)
        )
    }

    /// If this is a binding to a mutable value reference (storage or register)
    pub fn is_ref_mut(self) -> bool {
        matches!(
            self,
            Self::Undeclared | Self::Storage(Mutability::Var) | Self::Register(Mutability::Var)
        )
    }

    /// If this is a binding to a storage location (mut or immutable)
    pub fn is_storage(self) -> bool {
        matches!(self, Self::Undeclared | Self::Storage(_))
    }

    /// If this is a binding to a mutable storage location
    pub fn is_storage_mut(self) -> bool {
        matches!(self, Self::Undeclared | Self::Storage(Mutability::Var))
    }

    /// If this is a binding to a type
    pub fn is_type(self) -> bool {
        matches!(self, Self::Undeclared | Self::Type)
    }
}

impl std::fmt::Display for BindingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BindingKind::Undeclared => unreachable!("undecl bindings should never be reported"),
            BindingKind::Storage(Mutability::Var) => "a variable",
            BindingKind::Storage(Mutability::Const) => "a constant",
            BindingKind::Register(Mutability::Var) => "a register",
            BindingKind::Register(Mutability::Const) => "a constant register",
            BindingKind::Type => "a type",
            BindingKind::Module => "a module",
            BindingKind::Subprogram(SubprogramKind::Procedure) => "a procedure",
            BindingKind::Subprogram(SubprogramKind::Function) => "a function",
            BindingKind::Subprogram(SubprogramKind::Process) => "a process",
        };

        f.write_str(name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SubprogramKind {
    Procedure,
    Function,
    Process,
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
