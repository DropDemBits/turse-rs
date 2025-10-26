//! A type participant within the type system.

use std::marker::PhantomData;

use toc_hir_def::Mutability;

use crate::Db;

#[salsa_macros::interned(debug)]
pub struct Ty<'db> {
    #[returns(ref)]
    pub kind: TyKind,
}

/// A placeholder representing flexible vars can never be present in a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NeverFlexVar {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<Flex = NeverFlexVar> {
    /// A type hole derived from a type system error. This allows type inference
    /// to make forward progress and discover more type errors without having to
    /// stop at the first type error.
    Error,
    /// A placeholder flexible var used during type inference.
    FlexVar(Flex),
    /// Reference to a non-temporary memory value.
    Place(Box<TyKind<Flex>>, Mutability),
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
    /// Integer-variable type. This is a phony type that defaults to `int` if it
    /// is not unified with a concrete `int`, `nat`, or `real` type.
    Integer,
    /// Number-variable type. This is a phony type that defaults to `real` if it
    /// is not unified with a concrete `real` type.
    Number,
    /// Single character type.
    Char,
    /// Simple string type.
    String,
    /// Fixed-size character type.
    #[cfg(feature = "unimplemented")]
    CharN(SeqSize),
    /// Fixed-size string type.
    #[cfg(feature = "unimplemented")]
    StringN(SeqSize),
    /// Named alias to another type, pointing to the base (un-aliased) type
    #[cfg(feature = "unimplemented")]
    Alias(AliasTy),
    /// An alias exported as an opaque type. Points to the base (un-aliased) alias,
    /// with the [`DefId`] pointing to the original alias.
    #[cfg(feature = "unimplemented")]
    Opaque(DefId, TypeId),
    /// Constrained value type, with base type, range start, and range end.
    /// Base type is already de-aliased
    #[cfg(feature = "unimplemented")]
    Constrained(TypeId, Const, EndBound),
    /// Array type, with flexibility, types for each index, and the element type.
    #[cfg(feature = "unimplemented")]
    Array(ArraySizing, Vec<TypeId>, TypeId),
    /// An enumeration type, with associated definition point and variants.
    #[cfg(feature = "unimplemented")]
    Enum(WithDef, Vec<DefId>),
    /// Set type, with associated definition point
    #[cfg(feature = "unimplemented")]
    Set(WithDef, TypeId),
    /// Set type, with a given checkedness
    #[cfg(feature = "unimplemented")]
    Pointer(Checked, TypeId),
    /// Subprogram type, from (`procedure`, `function`, and `process`).
    #[cfg(feature = "unimplemented")]
    Subprogram(symbol::SubprogramKind, Option<Vec<Param>>, TypeId),
    /// Void type, returned from (`procedure` and `process`)
    #[cfg(feature = "unimplemented")]
    Void,
    // Other types to add:
    // - array
    // - range
    // - enum
    // - union
    // - record
    // - set
    // - pointer
    // - condition
    // - collection
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

impl<Var> TyKind<Var> {
    fn fold_vars<U>(self, mut f: impl FnMut(Var) -> TyKind<U>) -> TyKind<U> {
        match self {
            TyKind::Error => TyKind::Error,
            TyKind::FlexVar(v) => f(v),
            TyKind::Place(ty_kind, mutability) => {
                TyKind::Place(Box::new(ty_kind.fold_vars(f)), mutability)
            }
            TyKind::Boolean => TyKind::Boolean,
            TyKind::Int(int_size) => TyKind::Int(int_size),
            TyKind::Nat(nat_size) => TyKind::Nat(nat_size),
            TyKind::Real(real_size) => TyKind::Real(real_size),
            TyKind::Integer => TyKind::Integer,
            TyKind::Number => TyKind::Number,
            TyKind::Char => TyKind::Char,
            TyKind::String => TyKind::String,
        }
    }
}

impl<'db> TyKind<FlexVar<'db>> {
    /// Replaces any flexible type variables with concrete types and/or rigid type variables.
    pub fn substitute(self, sub: impl FnMut(FlexVar<'db>) -> TyKind) -> TyKind {
        self.fold_vars(sub)
    }
}

impl TyKind {
    /// Instantiates a type, replacing rigid type variables with flexible ones.
    pub fn instantiate<'db>(self) -> TyKind<FlexVar<'db>> {
        self.fold_vars(|v| match v {})
    }

    pub fn intern<'db>(self, db: &'db dyn Db) -> Ty<'db> {
        match self {
            TyKind::Error => make::mk_error(db),
            TyKind::FlexVar(var) => match var {},
            TyKind::Place(inner_ty, mutability) => Ty::new(db, TyKind::Place(inner_ty, mutability)),
            TyKind::Boolean => make::mk_boolean(db),
            TyKind::Int(IntSize::Int1) => make::mk_int1(db),
            TyKind::Int(IntSize::Int2) => make::mk_int2(db),
            TyKind::Int(IntSize::Int4) => make::mk_int4(db),
            TyKind::Int(IntSize::Int) => make::mk_int(db),
            TyKind::Nat(NatSize::Nat1) => make::mk_nat1(db),
            TyKind::Nat(NatSize::Nat2) => make::mk_nat2(db),
            TyKind::Nat(NatSize::Nat4) => make::mk_nat4(db),
            TyKind::Nat(NatSize::Nat) => make::mk_nat(db),
            TyKind::Nat(NatSize::AddressInt) => make::mk_addressint(db),
            TyKind::Real(RealSize::Real4) => make::mk_real4(db),
            TyKind::Real(RealSize::Real8) => make::mk_real8(db),
            TyKind::Real(RealSize::Real) => make::mk_real(db),
            TyKind::Integer => make::mk_integer(db),
            TyKind::Number => make::mk_number(db),
            TyKind::Char => make::mk_char(db),
            TyKind::String => make::mk_string(db),
        }
    }
}

/// A flexible inference variable that can be constrained to a more flexible type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FlexVar<'db>(u32, PhantomData<&'db ()>);

impl<'db> ena::unify::UnifyKey for FlexVar<'db> {
    type Value = Option<FlexTy<'db>>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        FlexVar(u, PhantomData)
    }

    fn tag() -> &'static str {
        "FlexVar"
    }
}

/// The corresponding type of a [`FlexVar`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FlexTy<'db> {
    /// Corresponds to another flexible type variable.
    Var(FlexVar<'db>),
    /// Type corresponds to a known concrete type.
    Concrete(Ty<'db>),
    /// Corresponds to a type that may have flexible vars.
    // FIXME: It'd be nice to describe a flexible type by its scheme (substs + structure) so that we can infer the structure
    Ty(Box<TyKind<FlexVar<'db>>>),
}

impl<'db> ena::unify::EqUnifyValue for FlexTy<'db> {}

impl<'db> From<Ty<'db>> for FlexTy<'db> {
    fn from(value: Ty<'db>) -> Self {
        Self::Concrete(value)
    }
}

impl<'db> From<FlexVar<'db>> for FlexTy<'db> {
    fn from(value: FlexVar<'db>) -> Self {
        Self::Var(value)
    }
}

impl<'db> FlexTy<'db> {
    pub fn into_ty_kind(self, db: &'db dyn Db) -> TyKind<FlexVar<'db>> {
        match self {
            FlexTy::Var(var) => TyKind::FlexVar(var),
            FlexTy::Concrete(ty) => ty.kind(db).clone().instantiate(),
            FlexTy::Ty(ty_kind) => *ty_kind,
        }
    }
}

pub mod make {
    //! Creates commonly used simple types.

    use crate::{
        Db,
        ty::{IntSize, NatSize, RealSize, Ty, TyKind},
    };

    /// Constructs a placeholder error type.
    #[salsa_macros::tracked]
    pub fn mk_error<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Error)
    }

    /// Constructs an integer flexible type.
    /// This defaults to a `int` type when not constrained by anything else.
    #[salsa_macros::tracked]
    pub fn mk_integer<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Integer)
    }

    /// Constructs a number flexible type.
    /// This defaults to a `real` type when not constrained by anything else.
    #[salsa_macros::tracked]
    pub fn mk_number<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Number)
    }

    /// Constructs a `boolean` type.
    #[salsa_macros::tracked]
    pub fn mk_boolean<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Boolean)
    }

    /// Constructs an `int1` type.
    #[salsa_macros::tracked]
    pub fn mk_int1<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Int(IntSize::Int1))
    }

    /// Constructs an `int2` type.
    #[salsa_macros::tracked]
    pub fn mk_int2<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Int(IntSize::Int2))
    }

    /// Constructs an `int4` type.
    #[salsa_macros::tracked]
    pub fn mk_int4<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Int(IntSize::Int4))
    }

    /// Constructs an `int` type.
    #[salsa_macros::tracked]
    pub fn mk_int<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Int(IntSize::Int))
    }

    /// Constructs a `nat1` type.
    #[salsa_macros::tracked]
    pub fn mk_nat1<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Nat(NatSize::Nat1))
    }

    /// Constructs a `nat2` type.
    #[salsa_macros::tracked]
    pub fn mk_nat2<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Nat(NatSize::Nat2))
    }

    /// Constructs a `nat4` type.
    #[salsa_macros::tracked]
    pub fn mk_nat4<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Nat(NatSize::Nat4))
    }

    /// Constructs a `nat` type.
    #[salsa_macros::tracked]
    pub fn mk_nat<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Nat(NatSize::Nat))
    }

    /// Constructs an `addressint` type.
    #[salsa_macros::tracked]
    pub fn mk_addressint<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Nat(NatSize::AddressInt))
    }

    /// Constructs a `real4` type.
    #[salsa_macros::tracked]
    pub fn mk_real4<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Real(RealSize::Real4))
    }

    /// Constructs a `real8` type.
    #[salsa_macros::tracked]
    pub fn mk_real8<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Real(RealSize::Real8))
    }

    /// Constructs a `real` type.
    #[salsa_macros::tracked]
    pub fn mk_real<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Real(RealSize::Real))
    }

    /// Constructs a `string` type.
    #[salsa_macros::tracked]
    pub fn mk_string<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::String)
    }

    /// Constructs a `char` type.
    #[salsa_macros::tracked]
    pub fn mk_char<'db>(db: &'db dyn Db) -> Ty<'db> {
        Ty::new(db, TyKind::Char)
    }
}
