//! Item definitions

use indexmap::IndexMap;

use crate::{
    body,
    ids::ItemIndex,
    span::SpanId,
    symbol::{self, Symbol},
    ty,
};

pub use crate::ids::{ExportId, ItemId, ModuleId};

/// An entity representing a declaration.
///
/// There is a unique mapping between an [`ItemId`] and a [`LocalDefId`],
/// i.e. there is only one [`LocalDefId`] corresponding to a [`ItemId`],
/// and vise versa. Multiple definitions coming from the same statement
/// are grouped together in a special block, indicating an item declaration
/// group.
///
/// [`LocalDefId`]: symbol::LocalDefId
#[derive(Debug, PartialEq, Eq)]
pub struct Item {
    pub kind: ItemKind,
    /// Associated definition info
    pub def_id: symbol::LocalDefId,
    /// Span covering the whole item
    pub span: SpanId,
    // ...
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemKind {
    // const + var decl, since both can be accessed during ctce
    // Contains body
    /// Combined representation for `const` and `var` declarations
    /// (disambiguated by `mutability`)
    ConstVar(ConstVar),
    /// Type alias & forward declaration
    Type(Type),
    /// Binding a definition as something else.
    Binding(Binding),
    /// General function-like, rolls up `function`, `procedure`, and `process`
    /// Distinguished by return type
    Subprogram(Subprogram),
    /// Maybe instead Extern(ConstVar) and Extern(Function)
    // ExternVar { .. },
    // ExternFunction { ..  },
    // Forward { .. },
    // Deferred { .. },
    // Body { .. },
    // aliased with monitor classes
    /*
    Class {
        as_monitor: bool,
        declares: Vec<ItemId>,
        exports: Vec<ExportItem>,
        body: BodyId,
        // ..
    },
    */
    // aliased with monitor (modules)
    Module(Module),
    /// Import introducing defs from the outer scope
    Import(Import),
    // FIXME: Also make Export into a real item
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstVar {
    pub is_register: bool,
    pub mutability: symbol::Mutability,
    pub def_id: symbol::LocalDefId,
    pub type_spec: Option<ty::TypeId>,
    pub init_expr: Option<body::BodyId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Type {
    pub def_id: symbol::LocalDefId,
    pub type_def: DefinedType,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DefinedType {
    /// Declaring an alias of a type
    Alias(ty::TypeId),
    /// Declaring a forward declaration of a type.
    /// Provided SpanId is the span of the token
    Forward(SpanId),
}

// Like constvar, bind is split up into multiple distinct items
#[derive(Debug, PartialEq, Eq)]
pub struct Binding {
    pub is_register: bool,
    /// Bind as a mutable location?
    pub mutability: symbol::Mutability,
    /// The definition to bind to
    pub def_id: symbol::LocalDefId,
    /// Expression to bind the definition to
    // ???: Why is this a body? Does it need to be (it can be a plain expr)?
    pub bind_to: body::BodyId,
}

/// A subprogram item
#[derive(Debug, PartialEq, Eq)]
pub struct Subprogram {
    /// The specific type of subprogram defined
    pub kind: symbol::SubprogramKind,
    /// Name of the function
    pub def_id: symbol::LocalDefId,

    /// Formal parameter list
    pub param_list: Option<ParamList>,
    /// Result info
    pub result: SubprogramResult,
    /// Extra info associated with the subprogram
    pub extra: SubprogramExtra,

    /// Executable portion of the subprogram
    pub body: SubprogramBody,
}

impl Subprogram {
    /// Looks up the associated parameter info
    pub fn lookup_param_info(&self, param_def: symbol::LocalDefId) -> ParameterInfo {
        // Try first in param list, then against result name
        self.param_list
            .as_ref()
            .and_then(|param_list| {
                param_list.names.iter().enumerate().find_map(|(idx, name)| {
                    (*name == param_def).then(|| ParameterInfo::Param(&param_list.tys[idx]))
                })
            })
            .unwrap_or_else(|| {
                assert_eq!(
                    self.result.name,
                    Some(param_def),
                    "not the named result param"
                );
                ParameterInfo::Result
            })
    }
}

#[derive(Debug)]
pub enum ParameterInfo<'a> {
    /// For a parameter
    Param(&'a ty::Parameter),
    /// For the named result
    Result,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParamList {
    pub names: Vec<symbol::LocalDefId>,
    pub tys: Vec<ty::Parameter>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SubprogramResult {
    pub name: Option<symbol::LocalDefId>,
    pub ty: ty::TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubprogramExtra {
    /// No extra subprogram info defined
    None,
    /// Specific device specification.
    /// Unused, and of unknown purpose (though checked for const-eval & correct type)
    DeviceSpec(body::BodyId),
    /// Stack size to allocate for the new process
    StackSize(body::BodyId),
}

impl SubprogramExtra {
    pub fn device_spec(self) -> Option<body::BodyId> {
        match self {
            SubprogramExtra::DeviceSpec(body) => Some(body),
            _ => None,
        }
    }

    pub fn stack_size(self) -> Option<body::BodyId> {
        match self {
            SubprogramExtra::StackSize(body) => Some(body),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SubprogramBody {
    pub body: body::BodyId,
    pub imports: Vec<ItemId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub as_monitor: bool,
    pub def_id: symbol::LocalDefId,
    pub declares: Vec<ItemId>,
    pub imports: Vec<ItemId>,
    /// What this module exports.
    /// There are no [`ExportItem`]s which share the same [`Symbol`].
    pub exports: Vec<ExportItem>,
    pub body: body::BodyId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Import {
    /// [`LocalDefId`](symbol::LocalDefId) of this specific import
    pub def_id: symbol::LocalDefId,
    pub mutability: ImportMutability,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportMutability {
    SameAsItem,
    /// Also has a span pointing to the applicable attribute
    Explicit(symbol::Mutability, SpanId),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExportItem {
    /// [`LocalDefId`](symbol::LocalDefId) to uniquely identify this specific export
    pub def_id: symbol::LocalDefId,
    pub mutability: symbol::Mutability,
    pub qualify_as: QualifyAs,
    pub is_opaque: bool,
    /// Associated def to export
    pub exported_def: symbol::LocalDefId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QualifyAs {
    Qualified,
    Unqualified,
    PervasiveUnqualified,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Fields {
    pub fields: IndexMap<Symbol, FieldInfo>,
}

impl Fields {
    pub fn lookup(&self, field: Symbol) -> Option<&FieldInfo> {
        self.fields.get(&field)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FieldInfo {
    /// Associated definition of this field
    pub def_id: symbol::DefId,
    /// Mutability of this field
    pub mutability: symbol::Mutability,
    /// If this field refers to an opaque type
    pub is_opaque: bool,
}

/// A module-like item (e.g. a `module`, a `class`, a `monitor`)
#[derive(Debug, PartialEq, Eq)]
pub enum ModuleLike<'a> {
    Module(&'a Module),
}

impl ModuleLike<'_> {
    pub fn export(&self, export_id: ExportId) -> &ExportItem {
        match self {
            ModuleLike::Module(module) => {
                module.exports.get(export_id.0).expect("bad export index")
            }
        }
    }

    pub fn exports_of(&self) -> &[ExportItem] {
        match self {
            ModuleLike::Module(module) => &module.exports,
        }
    }
}

/// Represents the item hierarchy, from leaf items to root items
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct ModuleTree {
    /// Which module is this item declared in.
    in_module: la_arena::ArenaMap<ItemIndex, ModuleId>,
}

impl ModuleTree {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn link_modules(&mut self, from: ModuleId, to: ModuleId) {
        assert_eq!(
            self.in_module.insert(to.item_id().0, from),
            None,
            "existing link for {from:?}, replaced with {to:?}"
        );
    }

    pub fn link_declared_item(&mut self, module_id: ModuleId, declares: ItemId) {
        assert_eq!(
            self.in_module.insert(declares.0, module_id),
            None,
            "existing link for {declares:?}, replaced with {module_id:?}"
        );
    }

    /// Gets the parent of this module.
    /// Can be missing as root modules don't have a parent.
    pub fn parent_of(&self, module: ModuleId) -> Option<ModuleId> {
        self.in_module.get(module.item_id().0).copied()
    }

    /// Gets which module declared this item.
    /// Can be missing as root modules aren't declared in another item
    pub fn module_of(&self, item_id: ItemId) -> Option<ModuleId> {
        self.in_module.get(item_id.0).copied()
    }
}
