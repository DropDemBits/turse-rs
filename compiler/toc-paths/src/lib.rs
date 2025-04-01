//! Path manipulation and interning.
//! Eventually meant to mirror rust-analyzer's [paths] crate.
//!
//! [paths]: https://github.com/rust-lang/rust-analyzer/blob/27239fbb/crates/paths/src/lib.rs

mod expansion;
mod paths;

pub use expansion::{BuiltinPrefix, PrefixExpansions, expand_path};
pub use paths::{RawOwnedPath, RawPath, RawRefPath};

#[salsa::db]
pub trait Db: salsa::Database {}

#[salsa::db]
impl<DB> Db for DB where DB: salsa::Database {}
