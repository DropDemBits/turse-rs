//! Various representations of a type at different stages of type evaluation.

use std::marker::PhantomData;

use crate::ty::FlexVar;

/// How a type can be represented internally
pub trait TypeIr {
    type TyVar: Copy + Eq;
}

/// A placeholder representing flexible vars can never be present in a type.
pub type NeverFlexVar = core::convert::Infallible;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rigid(());

impl TypeIr for Rigid {
    type TyVar = NeverFlexVar;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Infer<'db>(PhantomData<&'db ()>);

impl<'db> TypeIr for Infer<'db> {
    type TyVar = FlexVar<'db>;
}
