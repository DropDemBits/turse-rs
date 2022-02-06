//! Item definitions

use toc_span::SpanId;

use crate::{body, symbol, ty};

pub use crate::ids::ItemId;

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
    // ???: parameters & result are shared between this and the function header, what do?
    // - We need to split the function header and function body portions
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParamList {
    pub names: Vec<symbol::LocalDefId>,
    pub tys: Vec<Parameter>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    /// How the parameter should be passed in as
    pub pass_by: PassBy,
    /// If the value should be bound to a register
    pub is_register: bool,
    /// If the passed in value should be coerced to the target's type
    pub coerced_type: bool,
    /// Parameter's type
    pub param_ty: ty::TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PassBy {
    /// Pass by value
    Value,
    /// Pass by reference, with the specified mutability
    Reference(symbol::Mutability),
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub as_monitor: bool,
    pub def_id: symbol::LocalDefId,
    pub declares: Vec<ItemId>,
    // not handling exports yet
    //exports: Vec<ExportItem>,
    pub body: body::BodyId,
}
