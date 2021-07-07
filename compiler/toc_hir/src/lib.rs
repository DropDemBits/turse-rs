//! Crate containing all of the HIR node representations
//!
//! Note: All `expr`, `stmt`, and `ty` nodes are to be used with the module's
//! prefix, e.g. `expr::Name` instead of importing the node directly

pub(crate) mod internals {
    /// Helper for creating wrapper types of `db::HirId`.
    ///
    /// Only to be used inside of this crate.
    #[macro_export]
    macro_rules! hir_id_wrapper {
        ($id:ident) => {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            pub struct $id(pub(crate) $crate::db::HirId);

            impl From<$id> for $crate::db::HirId {
                fn from(id: $id) -> Self {
                    id.0
                }
            }

            impl From<&$id> for $crate::db::HirId {
                fn from(id: &$id) -> Self {
                    id.0
                }
            }
        };
    }
}

pub mod db;
pub mod expr;
pub mod stmt;
pub mod symbol;
pub mod ty;
pub mod unit;
pub mod visitor;

pub use db::HirId;
