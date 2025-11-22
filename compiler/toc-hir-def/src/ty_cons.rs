//! Layout for how type constructors are defined.

use toc_hir_expand::UnstableSemanticLoc;
use toc_salsa_collections::arena::{SalsaArena, SalsaArenaMap};
use toc_syntax::ast;

pub(crate) mod lower;

pub use lower::TyLowerError;

use crate::Db;

/// The HIR version of a type constructor from the AST.
///
/// This is a pair of [`TyCons`] and a [`TyConsSpans`].
#[salsa_macros::tracked(debug)]
pub struct LoweredTyCons<'db> {
    /// The actual HIR type constructor.
    #[tracked]
    #[returns(copy)]
    pub cons: TyCons<'db>,

    /// Spans that map each node in a [`TyCons`] to the original AST span.
    #[tracked]
    #[returns(ref)]
    pub spans: TyConsSpans<'db>,

    #[tracked]
    #[returns(ref)]
    errors_inner: Vec<TyLowerError<'db>>,
}

impl<'db> LoweredTyCons<'db> {
    /// Errors encountered during type constructor lowering.
    pub fn errors(self, db: &'db dyn Db) -> &'db [TyLowerError<'db>] {
        self.errors_inner(db).as_slice()
    }
}

/// A type constructor within a syntax item.
#[salsa_macros::interned(debug)]
pub struct TyCons<'db> {
    #[returns(ref)]
    inner: TyConsInner,
}

impl<'db> TyCons<'db> {
    pub fn root(self, db: &'db dyn Db) -> TyNode {
        self.inner(db).root
    }

    pub fn node(self, db: &'db dyn Db, node: TyNode) -> &'db TyKind {
        &self.inner(db).types[node.0]
    }
}

/// Lowered type constructor contents
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct TyConsInner {
    // eventually we'll want an Option<Box<ExprStore>> in here to represent type-level expressions
    types: SalsaArena<TyKind>,

    root: TyNode,
}

/// Spans of a type constructor
#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct TyConsSpans<'db> {
    types: TyConsMap<UnstableSemanticLoc<'db, ast::Type>>,
}

impl<'db> TyConsSpans<'db> {
    fn shrink_to_fit(&mut self) {
        self.types.shrink_to_fit();
    }
}

pub type TyConsMap<V> = SalsaArenaMap<TyNodeIndex, V>;

crate::arena_id_wrapper!(
    /// A [`TyCons`] local reference to a type node.
    pub struct TyNode(TyKind);
    /// Alias for the type constructor arena index
    pub(crate) type TyNodeIndex = Index;
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum TyKind {
    /// A missing type node, if it is required to be present.
    Missing,
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
    /// Single character type.
    Char,
    /// Simple string type.
    String,
}

/// Variants of an integer-class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntSize {
    Int1,
    Int2,
    Int4,
    /// Initialization checked version of `Int4`
    Int,
}

/// Size variant of a natural-class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NatSize {
    Nat1,
    Nat2,
    Nat4,
    /// Initialization checked version of `Nat4`
    Nat,
    /// Address sized integer, dependent on target machine
    AddressInt,
}

/// Size variant of a number-class type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RealSize {
    Real4,
    Real8,
    /// Initialization checked version of `Real8`
    Real,
}
