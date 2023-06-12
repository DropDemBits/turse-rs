//! Items and code bodies

use upcast::{Upcast, UpcastFrom};

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

pub mod body;
pub mod expr;
pub mod stmt;

pub mod item;
