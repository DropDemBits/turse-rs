//! Crate containing all of the HIR node representations
//!
//! All `expr`, `stmt`, `item`, `ty`, and `body` entities are to be used with
//! the module's prefix, e.g. `expr::Name` instead of importing the node
//! directly.
//!
//! While all HIR entities implement `PartialEq` and `Eq`, they are only used
//! to see if the bit representation actually changed, which may diverge from
//! the actual equality semantics (e.g. see [`expr::Literal`] for such a case).

pub(crate) mod internals {
    /// Helper for creating wrapper types of [`la_arena::Idx`].
    ///
    /// Only to be used inside of this crate.
    #[macro_export]
    macro_rules! arena_id_wrapper {
        // Just a newtype for the index
        (
            $(#[$attrs:meta])*
            $vis:vis struct $id:ident($wrap:path);
        ) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            $(#[$attrs])*
            $vis struct $id(pub(crate) ::la_arena::Idx<$wrap>);

            $crate::arena_id_wrapper!(@impl_rest, $id, $wrap);
        };
        // Newtype + type alias for the index
        (
            $(#[$attrs_wrap:meta])*
            $vis_wrap:vis struct $id:ident($wrap:path);
            $(#[$attrs_alias:meta])*
            $vis_alias:vis type $index_alias:ident = Index;
        ) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            $(#[$attrs_wrap])*
            $vis_wrap struct $id(pub(crate) $index_alias);

            $(#[$attrs_alias])*
            $vis_alias type $index_alias = ::la_arena::Idx<$wrap>;

            $crate::arena_id_wrapper!(@impl_rest, $id, $wrap);
        };
        // Other impls
        (
            @impl_rest, $id:ident, $wrap:path
        ) => {
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

    /// Simple named boolean
    #[macro_export]
    macro_rules! make_named_bool {
        (
            $(#[$attrs:meta])*
            $vis:vis enum $ident:ident $(;)?
        ) => {
            $(#[$attrs])*
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            $vis enum $ident {
                No,
                Yes
            }

            impl ::std::convert::From<bool> for $ident {
                fn from(v: bool) -> Self {
                    match v {
                        false => Self::No,
                        true => Self::Yes,
                    }
                }
            }

            impl ::std::convert::From<$ident> for bool {
                fn from(v: $ident) -> bool {
                    matches!(v, $ident::Yes)
                }
            }
        };
    }
}

pub(crate) mod ids;

pub mod builder;
pub mod span;

pub mod item;
pub mod package;
pub mod package_graph;
pub mod symbol;

pub mod body;
pub mod expr;
pub mod stmt;
pub mod ty;
pub mod visitor;

pub use toc_hir_expand::{
    AstLocations, Jar as ExpandJar, SemanticFile, SemanticLoc, SemanticNodePtr, UnstableSemanticLoc,
};

pub use toc_hir_def::{
    body::{pretty::render_item_body, pretty::render_package_bodies, Body},
    expr::*,
    item::{
        pretty::render_item_tree, root_module, AnyItem, ConstVar, HasItems, IntoItem, Item, Module,
    },
    stmt::*,
    Db as DefDb, Jar as DefJar, Mutability,
};

/// Helper trait equivalent to `Option::map_or(predicate)`
pub trait OrMissingExt<T> {
    fn is_missing_or(&self, is_predicate: impl FnOnce(T) -> bool) -> bool;
}

impl<T> OrMissingExt<T> for Option<T>
where
    T: Copy,
{
    fn is_missing_or(&self, is_predicate: impl FnOnce(T) -> bool) -> bool {
        self.map_or(true, is_predicate)
    }
}
