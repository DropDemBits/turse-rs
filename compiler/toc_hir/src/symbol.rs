//! Everything related to symbols.
//! `SymbolTable` construction with respect to scoping rules occurs in `toc_hir_lowering`.

use std::fmt;

use la_arena::ArenaMap;
use toc_span::SpanId;

pub use crate::ids::{DefId, LocalDefId};
use crate::{
    ids::{ExportId, ItemId, LocalDefIndex, ModuleId},
    stmt::BodyStmt,
    ty::{FieldId, TypeId},
};

pub mod syms;

/// Reference to an interned symbol
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(internment::Intern<String>);

impl Symbol {
    pub fn new(name: impl Into<String>) -> Self {
        Self(internment::Intern::new(name.into()))
    }

    pub fn name(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.as_str().fmt(f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

// String -> Symbol

impl From<String> for Symbol {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Self::new(name)
    }
}

/// Information associated with a `LocalDefId` or `DefId`.
#[derive(Debug, PartialEq, Eq)]
pub struct DefInfo {
    /// The name of the definition
    pub name: Symbol,
    /// Where the def was defined at
    pub def_at: SpanId,
    /// The kind of symbol.
    pub kind: SymbolKind,
    // ...
    // probably include additional definition information such as
    // - bound to a register (unsure?)
    // - pervasive (maybe left over from construction)
    // - mutability/access (const, var, type/none)?
    // - is part of a forward resolution chain?
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
    /// The symbol is from an export of an item, with a [`LocalDefId`]
    /// pointing to the original item.
    ItemExport(LocalDefId),
    /// The symbol is of an imported item.
    ItemImport,
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
    /// Export from a given `Module`
    Export(ModuleId, ExportId),
    /// Field on a given `Type`
    Field(TypeId, FieldId),
    /// Owned by a `Stmt`
    Stmt(BodyStmt),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Var,
}

impl Mutability {
    pub fn from_is_mutable(is_var: bool) -> Mutability {
        match is_var {
            true => Mutability::Var,
            false => Mutability::Const,
        }
    }
}

/// What a definition binds to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingTo {
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
    /// Binding to an enum field
    EnumField,
}

impl BindingTo {
    // Undeclared bindings are treated as equivalent to all of the
    // other binding types, for error reporting purposes.
    //
    // While it's still an invalid state, it can theoretically be
    // any valid binding kind.

    /// If this is a binding to a value reference (mut or immutable, storage, register, subprogram)
    pub fn is_ref(self) -> bool {
        matches!(
            self,
            Self::Storage(_) | Self::Register(_) | Self::Subprogram(_) | Self::EnumField
        )
    }

    /// If this is a binding to a mutable value reference (storage or register)
    pub fn is_ref_mut(self) -> bool {
        matches!(
            self,
            Self::Storage(Mutability::Var) | Self::Register(Mutability::Var)
        )
    }

    /// If this is a binding to a storage location (mut or immutable)
    pub fn is_storage(self) -> bool {
        matches!(self, Self::Storage(_))
    }

    /// If this is a binding to a mutable storage location
    pub fn is_storage_mut(self) -> bool {
        matches!(self, Self::Storage(Mutability::Var))
    }

    /// If this is a binding to a type
    pub fn is_type(self) -> bool {
        matches!(self, Self::Type)
    }
}

impl std::fmt::Display for BindingTo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BindingTo::Storage(Mutability::Var) => "a variable",
            BindingTo::Storage(Mutability::Const) => "a constant",
            BindingTo::Register(Mutability::Var) => "a register",
            BindingTo::Register(Mutability::Const) => "a constant register",
            BindingTo::Type => "a type",
            BindingTo::Module => "a module",
            BindingTo::Subprogram(SubprogramKind::Procedure) => "a procedure",
            BindingTo::Subprogram(SubprogramKind::Function) => "a function",
            BindingTo::Subprogram(SubprogramKind::Process) => "a process",
            BindingTo::EnumField => "an enum variant",
        };

        f.write_str(name)
    }
}

/// From a failed binding lookup
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotBinding {
    /// From an undeclared definition or an error expression
    Missing,
    /// Not a binding at all (e.g. a plain value)
    NotBinding,
}

/// Helper trait to deal with [`NotBinding`] kind narrowing
pub trait BindingResultExt: seal_me::Sealed {
    /// Allows a missing definition to match the previous predicate
    fn or_missing(self) -> bool;
}

impl BindingResultExt for Result<bool, NotBinding> {
    // Treat error exprs as the same as error
    // It can be any kind of expression

    fn or_missing(self) -> bool {
        self.unwrap_or_else(|err| match err {
            NotBinding::Missing => true,
            NotBinding::NotBinding => false,
        })
    }
}

mod seal_me {
    pub trait Sealed {}
    impl Sealed for Result<bool, super::NotBinding> {}
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
