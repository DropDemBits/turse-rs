//! Items and code bodies

use upcast::{Upcast, UpcastFrom};

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

pub mod body;
pub mod expr;
pub mod stmt;

pub mod item;

pub trait Db: salsa::DbWithJar<Jar> + toc_hir_expand::Db + Upcast<dyn toc_hir_expand::Db> {}

impl<DB> Db for DB where
    DB: salsa::DbWithJar<Jar> + toc_hir_expand::Db + Upcast<dyn toc_hir_expand::Db>
{
}

impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}

#[salsa::jar(db = Db)]
pub struct Jar(
    Symbol,
    item::root_module,
    item::ConstVar,
    item::ConstVar_mutability,
    item::Module,
    item::Module_items,
    item::Module_body,
    item::Module_stmt_list,
    body::Body,
    body::Body_top_level_stmts,
    body::Body_contents,
    body::Body_lower_contents,
);

#[salsa::interned]
pub struct Symbol {
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Var,
}

impl Mutability {
    pub fn from_is_mutable(is_var: bool) -> Mutability {
        match is_var {
            true => Mutability::Var,
            false => Mutability::Const,
        }
    }
}
