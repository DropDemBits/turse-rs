//! Path manipulation and interning.
//! Eventually meant to mirror rust-analyzer's [paths] crate.
//!
//! [paths]: https://github.com/rust-lang/rust-analyzer/blob/27239fbb/crates/paths/src/lib.rs

mod expansion;
mod paths;

pub use expansion::{expand_path, BuiltinPrefix, PrefixExpansions};
pub use paths::RawPath;

#[salsa::jar(db = Db)]
pub struct Jar(
    paths::RawPath,
    expansion::PrefixExpansions,
    expansion::PrefixExpansions_builtin_expansion,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

impl<'db, DB: Db + 'db> upcast::UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}
