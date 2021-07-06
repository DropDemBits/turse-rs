//! Crate containing all of the HIR node representations
//!
//! Note: All `expr`, `stmt`, and `ty` nodes are to be used with the module's
//! prefix, e.g. `expr::Name` instead of importing the node directly

pub mod db;
pub mod expr;
pub mod stmt;
pub mod symbol;
pub mod ty;
pub mod unit;
mod unit_map;
pub mod visitor;

pub use db::HirId;
