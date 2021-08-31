//! Representation of Turing types

use std::fmt::{self, Debug};
use std::ops::Deref;
use std::sync::Arc;

use indexmap::IndexMap;

use crate::db;

mod lower;
pub(crate) mod query;
pub mod rules;

toc_salsa::create_intern_key!(
    /// Id referencing an interned type.
    pub TypeId;
);

impl TypeId {
    /// Looks up the interned data in the given database
    pub fn lookup<DB>(self, db: &DB) -> TypeData
    where
        DB: ?Sized + db::TypeDatabase,
    {
        db.lookup_intern_type(self)
    }
}

/// Interened type data
// ???(ra bug): Can't rename
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeData {
    data: Arc<Type>,
}

impl std::ops::Deref for TypeData {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        self.data.deref()
    }
}

impl From<Type> for TypeData {
    fn from(ty: Type) -> Self {
        Self { data: Arc::new(ty) }
    }
}

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
    ty_table: IndexMap<toc_hir::ty::TypeId, TyRef>,
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

    pub fn map_type(&mut self, id: toc_hir::ty::TypeId, ty_ref: TyRef) {
        self.ty_table.insert(id, ty_ref);
    }

    pub fn get_type(&self, id: toc_hir::ty::TypeId) -> Option<TyRef> {
        self.ty_table.get(&id).copied()
    }

    pub fn map_def_id(&mut self, def_id: toc_hir::symbol::DefId, kind: DefKind) {
        self.def_type.insert(def_id, kind);
    }

    pub fn get_def_id_kind(&self, def_id: toc_hir::symbol::DefId) -> Option<DefKind> {
        self.def_type.get(&def_id).copied()
    }
}

impl Default for TyCtx {
    fn default() -> Self {
        Self::new()
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

// Constructible vs Well-formed (valid)
//
// Constructible, Well-formed  => Type itself is real, and all dependencies of it are real
// Constructible, ill-formed   => Type itself is real, but maybe not its dependencies
//                                e.g. `char(<invalid expr>)`, `set of <error>`
// Unconstructible, ill-formed => A type cannot be reified
//                                e.g. `<error>`, or from a TyKind::Missing

/// General concrete type
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
}

impl Type {
    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// Unconstructable type.
    /// Produced so that errors aren't duplicated.
    Error,
    /// Boolean type.
    Boolean,
    /// Signed integer types (e.g. `int4`, `int`).
    Int(IntSize),
    /// Unsigned integer types (e.g. `nat4`, `nat`).
    /// Also includes `addressint`, with the size being dependent on the target.
    /// machine.
    Nat(NatSize),
    /// Floating point types (e.g. `real8`, `real`).
    Real(RealSize),
    /// General integer type. Infers to adjacent concrete types (in the case of
    /// binary expressions) or to `int`.
    Integer,
    /// Single character type.
    Char,
    /// Simple string type.
    String,
    /// Fixed-size character type.
    CharN(SeqSize),
    /// Fixed-size string type.
    StringN(SeqSize),
    /// Reference type.
    ///
    /// This type does not appear in syntax (except for parameter binding),
    /// and is an implementation detail.
    Ref(Mutability, TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Var,
}

/// Size variant of an `Int`.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum IntSize {
    Int1,
    Int2,
    Int4,
    /// Initialization checked version of `Int4`
    Int,
}

/// Size variant of a Nat
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

/// Size variant of a Real
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RealSize {
    Real4,
    Real8,
    /// Initialization checked version of `Real8`
    Real,
}

/// Size of a CharSeq
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SeqSize {
    /// Runtime sized (only accepted for parameters)
    Dynamic,
    /// Fixed, compile-time size
    Fixed(toc_hir::body::BodyId),
}
