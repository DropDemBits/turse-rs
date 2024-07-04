//! Items and code bodies

use std::fmt;

use upcast::{Upcast, UpcastFrom};

pub(crate) mod internals {
    /// Helper for creating wrapper types of [`la_arena::Idx`].
    ///
    /// Only to be used inside of this crate.
    #[macro_export]
    macro_rules! arena_id_wrapper {
        (
            $(#[$attrs_wrap:meta])*
            $vis_wrap:vis struct $id:ident$(<$($param:ident),+>)?($wrap:path);

            $(
            $(#[$attrs_alias:meta])*
            $vis_alias:vis type $index_alias:ident$(<$($alias_param:ident)+>)? = Index;
            )?
        ) => {
            #[repr(transparent)]
            $(#[$attrs_wrap])*
            $vis_wrap struct $id$(<$($param)+>)?(pub(crate) ::la_arena::Idx<$wrap>);

            $(
            $(#[$attrs_alias])*
            $vis_alias type $index_alias$(<$($alias_param)+>)? = ::la_arena::Idx<$wrap>;
            )?

            impl$(<$($param)+>)? From<$id$(<$($param)+>)?> for ::la_arena::Idx<$wrap> {
                fn from(id: $id$(<$($param)+>)?) -> Self {
                    id.0
                }
            }

            impl$(<$($param)+>)? From<&$id$(<$($param)+>)?> for ::la_arena::Idx<$wrap> {
                fn from(id: &$id$(<$($param)+>)?) -> Self {
                    id.0
                }
            }

            impl$(<$($param)+>)? ::std::fmt::Debug for $id$(<$($param)+>)? {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    let raw: u32 = self.0.into_raw().into();
                    f.debug_tuple(stringify!($id))
                        .field(&raw)
                        .finish()
                }
            }

            impl$(<$($param)+>)? ::std::clone::Clone for $id$(<$($param)+>)? {
                fn clone(&self) -> Self {
                    *self
                }
            }

            impl$(<$($param)+>)? ::std::marker::Copy for $id$(<$($param)+>)? {}

            impl$(<$($param)+>)? ::std::cmp::PartialEq for $id$(<$($param)+>)? {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0
                }
            }

            impl$(<$($param)+>)? ::std::cmp::Eq for $id$(<$($param)+>)? {}

            impl$(<$($param)+>)? ::std::hash::Hash for $id$(<$($param)+>)? {
                fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                    self.0.hash(state);
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

pub mod resolver;

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
    item::containing_item,
    item::ItemCollection,
    item::NestedItemCollection,
    item::BlockItems,
    item::BlockItems_collect_items,
    item::BlockItems_collect_nested_items,
    item::ConstVar,
    item::ConstVar_item_attrs,
    item::ConstVar_parent_constvar,
    item::root_module,
    item::RootModule,
    item::RootModule_body,
    item::RootModule_collect_items,
    item::RootModule_collect_nested_items,
    item::RootModule_stmt_list,
    item::UnitModule,
    item::UnitModule__stmt_list,
    item::Module,
    item::Module_body,
    item::Module_collect_items,
    item::Module_collect_nested_items,
    item::Module_stmt_list,
    item::Module_item_attrs,
    item::module_block_collect_items,
    item::module_block_collect_nested_items,
    body::Body,
    body::Body_top_level_stmts,
    body::Body_contents,
    body::Body_lower_contents,
    body::ModuleBlock,
);

// Not in salsa so we make one ourselves
pub trait DisplayWithDb<'db, Db: ?Sized + 'db> {
    fn display<'me>(&'me self, db: &'db Db) -> DisplayWith<'me, 'db, Db>
    where
        'db: 'me,
        Self: Sized + 'me,
    {
        DisplayWith { value: self, db }
    }

    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Db) -> fmt::Result;
}

pub struct DisplayWith<'me, 'db: 'me, Db: ?Sized + 'db> {
    value: &'me (dyn DisplayWithDb<'db, Db> + 'me),
    db: &'db Db,
}

impl<'me, 'db: 'me, Db: ?Sized> fmt::Display for DisplayWith<'me, 'db, Db> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DisplayWithDb::fmt(self.value, f, self.db)
    }
}

#[salsa::interned]
pub struct Symbol {
    #[return_ref]
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
