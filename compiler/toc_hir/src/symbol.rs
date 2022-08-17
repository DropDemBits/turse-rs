//! Everything related to symbols.
//! `SymbolTable` construction with respect to scoping rules occurs in `toc_hir_lowering`.

use std::fmt;

use indexmap::IndexMap;
use la_arena::{Arena, ArenaMap};
use toc_span::{SpanId, Spanned};

pub use crate::ids::{DefId, LocalDefId};
use crate::{
    ids::{ExportId, ItemId, LocalDefIndex, ModuleId},
    stmt::BodyStmt,
    ty::{FieldId, PassBy, TypeId},
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

// FIXME: This feels more like an AST or `toc_span` construct, move it elsewhere
/// The span of a specific AST node in a [`Library`](crate::library::Library)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeSpan(pub SpanId);

impl NodeSpan {
    pub fn span(self) -> SpanId {
        self.0
    }
}

/// Table of all [`DefInfo`]s in a [`Library`](crate::library::Library)
#[derive(Debug, Default, PartialEq, Eq)]
pub struct DefInfoTable {
    defs: Arena<DefInfo>,
}

impl DefInfoTable {
    /// Helper for declaring a new symbol.
    ///
    /// ## Parameters
    /// - `name`: The name of the symbol to define
    /// - `span`: The text span of the definition
    /// - `kind`: The kind of symbol to define, or None if it's undeclared
    ///
    /// ## Returns
    /// The [`LocalDefId`] associated with the definition
    ///
    /// [`LocalDefId`]: crate::symbol::LocalDefId
    pub fn add_def(
        &mut self,
        name: Symbol,
        span: SpanId,
        kind: Option<SymbolKind>,
        pervasive: IsPervasive,
    ) -> LocalDefId {
        let def = DefInfo {
            name,
            def_at: span,
            kind,
            pervasive,
        };
        let index = self.defs.alloc(def);
        LocalDefId(index)
    }

    pub fn get_info(&self, local_def: LocalDefId) -> &DefInfo {
        &self.defs[local_def.0]
    }

    pub fn iter(&self) -> impl Iterator<Item = (LocalDefId, &'_ DefInfo)> + '_ {
        self.defs.iter().map(|(id, info)| (LocalDefId(id), info))
    }
}

/// Information associated with a `LocalDefId` or `DefId`.
#[derive(Debug, PartialEq, Eq)]
pub struct DefInfo {
    /// The name of the definition
    pub name: Symbol,
    /// Where the def was defined at
    pub def_at: SpanId,
    /// What kind of declaration this definition refers to,
    /// or `None` if it's from an undeclared definition.
    pub kind: Option<SymbolKind>,

    /// Whether or not this def is automatically imported through
    /// child import boundaries.
    pub pervasive: IsPervasive,
}

/// What kind of item this symbol references.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)] // Exhaustive listing of variants
pub enum SymbolKind {
    /// From `const`, `var`, or the `for`-loop counter var, which might be bound to a register
    ConstVar(Mutability, IsRegister),
    /// From a `bind`, which might be bound to a register
    Binding(Mutability, IsRegister),
    /// From a `type`, or `collection of forward {name}`
    Type,
    /// From a `function`, `procedure`, or `process`
    Subprogram(SubprogramKind),
    /// Parameter on a subprogram (either in or out/result params)
    Param(PassBy, IsRegister),
    // ???: Could probably make this a prop of the sym
    /// From `external const` or `external var`
    ExternalConstVar(Mutability),
    // ???: Could probably make this a prop of the sym
    /// From an `external function` or `external procedure`
    ExternalSubprog(SubprogramKind),
    // ???: Could probably make this a prop of the sym
    /// Deferred subprogram, which has no body
    Deferred(SubprogramKind),
    /// From a `body`, which may know what it's a body of.
    /// Not resolved to what it's a body of, since that requires name resolution.
    Body(Option<SubprogramKind>),
    /// From a `module`, which may or may not be a monitor
    Module(IsMonitor),
    /// From a `class`, which may or may not be a monitor
    Class(IsMonitor),

    /// From an `import` item
    Import,
    /// From an `export` item
    Export,

    /// From an `enum`
    Enum,
    /// From a `set`
    Set,
    /// From a `record`
    Record,
    /// From a `union`
    Union,
    /// From a variant on an `enum`
    EnumVariant,
    /// From a field on a `record` or `union`
    RecordField,
}

impl SymbolKind {
    /// If this is can be used as a value reference
    pub fn is_ref(self) -> bool {
        matches!(
            self,
            Self::ConstVar(..)
                | Self::Binding(..)
                | Self::Subprogram(_)
                | Self::Param(..)
                | Self::RecordField
                | Self::EnumVariant
        )
    }

    /// If this is a binding to a mutable value reference (storage or register)
    pub fn is_ref_mut(self) -> bool {
        matches!(
            self,
            Self::ConstVar(muta, _)
            | Self::Binding(muta, _)
            | Self::Param(PassBy::Reference(muta), _) if muta == Mutability::Var
        )
    }

    /// If this is a binding to a storage location (mut or immutable)
    pub fn is_storage(self) -> bool {
        matches!(self, Self::ConstVar(..) | Self::Binding(..))
    }

    /// If this is a binding to a mutable storage location
    pub fn is_storage_mut(self) -> bool {
        matches!(
            self,
            Self::ConstVar(muta, reg)
            | Self::Binding(muta, reg)
            | Self::Param(PassBy::Reference(muta), reg) if muta == Mutability::Var && reg == IsRegister::No
        )
    }

    /// If this is a binding to a type
    pub fn is_type(self) -> bool {
        matches!(self, Self::Type)
    }
}

impl std::fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::ConstVar(muta, reg) | Self::Binding(muta, reg) => match (reg, muta) {
                (IsRegister::No, Mutability::Const) => "a constant",
                (IsRegister::No, Mutability::Var) => "a variable",
                (IsRegister::Yes, Mutability::Const) => "a constant register",
                (IsRegister::Yes, Mutability::Var) => "a register",
            },
            Self::Type => "a type",
            Self::Subprogram(SubprogramKind::Procedure) => "a procedure",
            Self::Subprogram(SubprogramKind::Function) => "a function",
            Self::Subprogram(SubprogramKind::Process) => "a process",
            Self::Param(pass_by, reg) => match (reg, pass_by) {
                (IsRegister::No, PassBy::Value | PassBy::Reference(Mutability::Const)) => {
                    "a constant"
                }
                (IsRegister::Yes, PassBy::Value | PassBy::Reference(Mutability::Const)) => {
                    "a constant register"
                }
                (IsRegister::No, PassBy::Reference(Mutability::Var)) => "a variable",
                (IsRegister::Yes, PassBy::Reference(Mutability::Var)) => "a register",
            },
            Self::ExternalConstVar(Mutability::Const) => "an external constant",
            Self::ExternalConstVar(Mutability::Var) => "an external variable",
            Self::ExternalSubprog(SubprogramKind::Procedure) => "an external procedure",
            Self::ExternalSubprog(SubprogramKind::Function) => "an external function",
            Self::ExternalSubprog(SubprogramKind::Process) => "an external process",
            Self::Deferred(SubprogramKind::Procedure) => "a deferred procedure",
            Self::Deferred(SubprogramKind::Function) => "a deferred function",
            Self::Deferred(SubprogramKind::Process) => "a deferred process",
            Self::Body(_) => "a subprogram body",
            Self::Module(IsMonitor::No) => "a module",
            Self::Module(IsMonitor::Yes) => "a monitor",
            Self::Class(IsMonitor::No) => "a class",
            Self::Class(IsMonitor::Yes) => "a monitor class",
            Self::Import => "an import",
            Self::Export => "an export",
            Self::Enum => "an enum",
            Self::Set => "a set",
            Self::Record => "a record",
            Self::Union => "a union",
            Self::EnumVariant => "an enum variant",
            Self::RecordField => "a record field",
        };

        f.write_str(name)
    }
}

crate::make_named_bool! {
    pub enum IsPervasive;
}

crate::make_named_bool! {
    pub enum IsRegister;
}

crate::make_named_bool! {
    pub enum IsMonitor;
}

/// Any HIR node that contains a definition
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefOwner {
    /// Owned directly by an [`Item`](crate::item::Item)
    Item(ItemId),
    /// Parameter on a given [`Item`](crate::item::Item)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SubprogramKind {
    Procedure,
    Function,
    Process,
}

/// What a binding might resolve to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolve {
    /// Resolves to a local def
    Def(LocalDefId),
    /// Doesn't resolve to any def, either because it's undeclared
    /// or not visible at the binding's resolution point.
    Err,
}

impl Resolve {
    /// Asserts that this is a [`Resolve::Def`], and extracts the associated
    /// [`LocalDefId`].
    ///
    /// ## Panics
    ///
    /// Panics if it's not a [`Resolve::Def`] (e.g. [`Resolve::Err`])
    #[track_caller]
    pub fn unwrap_def(self) -> LocalDefId {
        match self {
            Resolve::Def(local_def) => local_def,
            Resolve::Err => panic!("called `Resolve::unwrap_def` on a `Resolve::Err` value"),
        }
    }
}

/// What a def might resolve to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefResolve {
    /// Resolves to another local def
    Local(LocalDefId),
    /// Resolves to an external def
    External(DefId),
    /// No resolution was applicable, e.g. there wasn't any def that
    /// was applicable for being the import's target
    Err,
    /// This is already the canonical def.
    Canonical,
}

/// Library-local map of bindings to their corresponding [`Resolve`]
#[derive(Debug, PartialEq, Eq, Default)]
pub struct ResolutionMap {
    pub binding_resolves: IndexMap<Spanned<Symbol>, Resolve>,
    pub def_resolves: DefMap<DefResolve>,
}

// FIXME: Replace with a `DefMap<DefOwner>`
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

/// Mapping between a [`LocalDefId`] and the coresponding `T`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefMap<T> {
    map: ArenaMap<LocalDefIndex, T>,
}

impl<T> DefMap<T> {
    pub fn insert(&mut self, def_id: LocalDefId, value: T) {
        self.map.insert(def_id.0, value)
    }

    pub fn get(&self, def_id: LocalDefId) -> Option<&T> {
        self.map.get(def_id.0)
    }

    pub fn get_mut(&mut self, def_id: LocalDefId) -> Option<&mut T> {
        self.map.get_mut(def_id.0)
    }
}

impl<T> Default for DefMap<T> {
    fn default() -> Self {
        Self {
            map: Default::default(),
        }
    }
}
