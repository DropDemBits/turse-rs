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
    // Type { .. },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Const,
    Var,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstVar {
    pub is_register: bool,
    pub mutability: Mutability,
    pub def_id: symbol::LocalDefId,
    pub type_spec: Option<ty::TypeId>,
    pub init_expr: Option<body::BodyId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstVarTail {
    /// Only the type spec is specified
    TypeSpec(ty::TypeId),
    /// Only the initializer is specified
    InitExpr(body::BodyId),
    /// Both the type spec and init expr are specified
    Both(ty::TypeId, body::BodyId),
}

impl ConstVarTail {
    pub fn type_spec(self) -> Option<ty::TypeId> {
        match self {
            ConstVarTail::TypeSpec(ty_spec) => Some(ty_spec),
            ConstVarTail::InitExpr(_) => None,
            ConstVarTail::Both(ty_spec, _) => Some(ty_spec),
        }
    }

    pub fn init_expr(self) -> Option<body::BodyId> {
        match self {
            ConstVarTail::TypeSpec(_) => None,
            ConstVarTail::InitExpr(init_expr) => Some(init_expr),
            ConstVarTail::Both(_, init_expr) => Some(init_expr),
        }
    }
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
