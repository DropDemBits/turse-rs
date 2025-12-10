//! An implementation of cubic biunification.
//!
//! The core algorithm lives in [`unify`].

pub mod unify;

pub use unify::{BiunificationSolution, BiunificationTable, BiunificationVariables, UnifyKey};
