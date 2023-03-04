//! Path manipulation and interning.
//! Eventually meant to mirror rust-analyzer's [paths] crate.
//!
//! [paths]: https://github.com/rust-lang/rust-analyzer/blob/27239fbb/crates/paths/src/lib.rs

// FIXME(salsa-2022): consider eliding generated return ref if it's clear that it can be elided (or just add the allow)

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

pub trait Db: salsa::DbWithJar<Jar> {
    fn upcast_to_path_db(&self) -> &dyn Db;
}

impl<DB> Db for DB
where
    DB: salsa::DbWithJar<Jar>,
{
    fn upcast_to_path_db(&self) -> &dyn Db {
        self
    }
}
