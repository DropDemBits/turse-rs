//! Items and code bodies

use std::fmt;

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

        // Newtype + type alias for the index with a lifetime
        (
            $(#[$attrs_wrap:meta])*
            $vis_wrap:vis struct $id:ident<$id_lt:lifetime>($wrap:path);
            $(#[$attrs_alias:meta])*
            $vis_alias:vis type $index_alias:ident<$index_lt:lifetime> = Index;
        ) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            $(#[$attrs_wrap])*
            $vis_wrap struct $id<$id_lt>(pub(crate) $index_alias<$id_lt>);

            $(#[$attrs_alias])*
            $vis_alias type $index_alias<$index_lt> = ::la_arena::Idx<$wrap>;

            $crate::arena_id_wrapper!(@impl_rest, $id, $wrap, $id_lt);
        };
        // Other impls
        (
            @impl_rest, $id:ident, $wrap:path $(, $id_lt:lifetime)?
        ) => {
            impl$(<$id_lt>)? From<$id$(::<$id_lt>)?> for ::la_arena::Idx<$wrap> {
                fn from(id: $id$(::<$id_lt>)?) -> Self {
                    id.0
                }
            }

            impl$(<$id_lt>)? From<&$id$(::<$id_lt>)?> for ::la_arena::Idx<$wrap> {
                fn from(id: &$id$(::<$id_lt>)?) -> Self {
                    id.0
                }
            }

            impl$(<$id_lt>)?::std::fmt::Debug for $id$(<$id_lt>)? {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    let raw: u32 = self.0.into_raw().into();
                    f.debug_tuple(stringify!($id))
                        .field(&raw)
                        .finish()
                }
            }

            unsafe impl$(<$id_lt>)? salsa::Update for $id$(<$id_lt>)? {
                unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
                    // SAFETY: old_pointer is required to satisfy both the safety and validity invariants
                    let old_id = unsafe { &mut *old_pointer };
                    let new_id = new_value;

                    if old_id.0 != new_id.0 {
                        old_id.0 = new_id.0;
                        true
                    } else {
                        false
                    }
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

#[cfg(test)]
mod test;

pub mod body;
pub mod expr;
pub mod stmt;

pub mod item;

#[salsa::db]
pub trait Db: toc_hir_expand::Db {}

#[salsa::db]
impl<DB> Db for DB where DB: toc_hir_expand::Db {}

// Not in salsa so we make one ourselves
pub trait DisplayWithDb<'db, Db: ?Sized> {
    fn display<'me>(&'me self, db: &'db Db) -> DisplayWith<'me, 'db, Db>
    where
        'db: 'me,
        Self: Sized + 'me,
    {
        DisplayWith { value: self, db }
    }

    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db) -> fmt::Result;
}

pub struct DisplayWith<'me, 'db: 'me, Db: ?Sized> {
    value: &'me (dyn DisplayWithDb<'db, Db> + 'me),
    db: &'db Db,
}

impl<'me, 'db: 'me, Db: ?Sized> fmt::Display for DisplayWith<'me, 'db, Db> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DisplayWithDb::fmt(self.value, f, self.db)
    }
}

#[salsa::interned(debug, no_lifetime)]
pub struct Symbol {
    #[returns(ref)]
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

crate::make_named_bool! {
    pub enum IsPervasive;
}

crate::make_named_bool! {
    pub enum IsRegister;
}

crate::make_named_bool! {
    pub enum IsMonitor;
}

bitflags::bitflags! {
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub(crate) struct ItemAttrs: u32 {
        const NONE = 0;
        const MUTABLE = 1 << 0;
        const PERVASIVE = 1 << 1;
        const REGISTER = 1 << 2;
        const MONITOR = 1 << 3;
    }
}

impl ItemAttrs {
    pub(crate) fn mutablity(self) -> Mutability {
        Mutability::from_is_mutable(self.contains(ItemAttrs::MUTABLE))
    }

    pub(crate) fn is_pervasive(self) -> IsPervasive {
        self.contains(ItemAttrs::PERVASIVE).into()
    }

    pub(crate) fn is_register(self) -> IsRegister {
        self.contains(ItemAttrs::REGISTER).into()
    }

    pub(crate) fn is_monitor(self) -> IsMonitor {
        self.contains(ItemAttrs::MONITOR).into()
    }
}
