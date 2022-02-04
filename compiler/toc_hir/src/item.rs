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
    // Bind { .. },
    /// general function, rolls up function, procedure, and process
    /// distinguished by return type
    // Function { ty: TypeId, body: BodyId, },
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

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub as_monitor: bool,
    pub def_id: symbol::LocalDefId,
    pub declares: Vec<ItemId>,
    // not handling exports yet
    //exports: Vec<ExportItem>,
    pub body: body::BodyId,
}
