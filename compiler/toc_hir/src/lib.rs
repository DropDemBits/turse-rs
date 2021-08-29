//! Crate containing all of the HIR node representations
//!
//! Note: All `expr`, `stmt`, and `ty` nodes are to be used with the module's
//! prefix, e.g. `expr::Name` instead of importing the node directly

pub(crate) mod internals {
    /// Helper for creating wrapper types of [`la_arena::Idx`].
    ///
    /// Only to be used inside of this crate.
    #[macro_export]
    macro_rules! arena_id_wrapper {
        (
            $(#[$attrs:meta])*
            $visi:vis struct $id:ident($wrap:path);
        ) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            $(#[$attrs])*
            $visi struct $id(pub(crate) ::la_arena::Idx<$wrap>);

            impl From<$id> for ::la_arena::Idx<$wrap> {
                fn from(id: $id) -> Self {
                    id.0
                }
            }

            impl From<&$id> for ::la_arena::Idx<$wrap> {
                fn from(id: &$id) -> Self {
                    id.0
                }
            }

            impl ::std::fmt::Debug for $id {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    let raw: u32 = self.0.into_raw().into();
                    f.debug_tuple(stringify!($id))
                        .field(&raw)
                        .finish()
                }
            }
        };
    }
}

pub mod body;
pub mod builder;
pub mod expr;
pub mod item;
pub mod library;
pub mod library_graph;
pub mod stmt;
pub mod symbol;
pub mod ty;
pub mod visitor;
