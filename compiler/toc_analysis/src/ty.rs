//! Representation of Turing types

use std::fmt::{self, Debug};
use std::ops::Deref;

use indexmap::IndexMap;

/// A type reference, for each unique type
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TyRef(internment::Intern<Type>);

impl fmt::Debug for TyRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Bypass internment node
        f.write_fmt(format_args!("TyRef({:?})", self.0.deref()))
    }
}

impl Deref for TyRef {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Type(TyRef),
    Const(TyRef),
    Var(TyRef),
    Error(TyRef),
}

/// Typing context for a given unit
#[derive(Debug)]
pub struct TyCtx {
    ty_table: IndexMap<toc_hir::ty::TypeIdx, TyRef>,
    // Store def id type here since it'll be needed for bytecode gen
    def_type: IndexMap<toc_hir::symbol::DefId, DefKind>,
}

impl TyCtx {
    pub fn new() -> Self {
        Self {
            ty_table: IndexMap::new(),
            def_type: IndexMap::new(),
        }
    }

    pub fn add_type(&mut self, ty: Type) -> TyRef {
        TyRef(internment::Intern::new(ty))
    }

    pub fn map_type(&mut self, id: toc_hir::ty::TypeIdx, ty_ref: TyRef) {
        self.ty_table.insert(id, ty_ref);
    }

    pub fn get_type(&self, id: toc_hir::ty::TypeIdx) -> Option<TyRef> {
        self.ty_table.get(&id).copied()
    }

    pub fn map_def_id(&mut self, def_id: toc_hir::symbol::DefId, kind: DefKind) {
        self.def_type.insert(def_id, kind);
    }

    pub fn get_def_id_kind(&self, def_id: toc_hir::symbol::DefId) -> Option<DefKind> {
        self.def_type.get(&def_id).copied()
    }
}

pub(crate) fn pretty_dump_typectx(ty_ctx: &TyCtx) -> String {
    let mut s = String::new();

    // Type Nodes
    s.push_str("ty_nodes:\n");
    for (k, v) in ty_ctx.ty_table.iter() {
        s.push_str(&format!("    {:?} {:?}\n", k, v))
    }

    // DefKinds
    s.push_str("def_kinds:\n");
    for (k, v) in ty_ctx.def_type.iter() {
        s.push_str(&format!("    {:?} {:?}\n", k, v))
    }

    s
}

/// General concrete type
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    /// Type Error, produced so that errors aren't duplicated
    Error,
    /// Boolean type
    Boolean,
    /// Signed integer types (e.g. `int4`, `int`).
    Int(IntSize),
    /// Unsigned integer types (e.g. `nat4`, `nat`).
    /// Also includes `addressint`, with the size being dependent on the target
    /// machine.
    Nat(NatSize),
    /// Floating point types (e.g. `real8`, `real`)
    Real(RealSize),
    /// General integer type. Infers to adjacent concrete types (in the case of
    /// binary expressions) or to `int`.
    Integer,
    /// Single character type
    Char,
    /// Simple string type
    String,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum IntSize {
    Int1,
    Int2,
    Int4,
    /// Initialization checked version of `Int4`
    Int,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum NatSize {
    Nat1,
    Nat2,
    Nat4,
    /// Initialization checked version of `Nat4`
    Nat,
    /// Address sized integer, dependent on target machine
    AddressInt,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RealSize {
    Real4,
    Real8,
    /// Initialization checked version of `Real8`
    Real,
}
