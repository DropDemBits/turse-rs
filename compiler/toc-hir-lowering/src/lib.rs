//! Crate for lowering the CST into a HIR node tree

// ???: Who handles preprocessor statement expansion?
// HIR lowering should handle preprocessor expansion, but requires a file database
// mapping paths to parsed CST's
// HIR lowering should also evaluate preprocessor expressions, provided a set of
// predefined identifiers

// Root file:
// - Parse CST
// - Gather dependencies
// - Request dependencies from file DB
//   - May load them from disk, or fetch from loaded cache
// ----
// Once all deps are parsed, then lower into HIR for all of them
//
// FileDB should only care about giving unique file handles corresponding to text sources.
// Other DBs will deal with what they map to (e.g. a separate DB managing all of the CSTs)

mod collector;
mod lower;
mod resolver;

use toc_hir::package::LoweredPackage;

pub use lower::{lower_package, lower_source_graph};

/// Trait representing a database that can store a lowered HIR tree
#[salsa::db]
pub trait Db: toc_source_graph::Db + toc_ast_db::Db {}

#[salsa::db]
impl<DB> Db for DB where DB: toc_source_graph::Db + toc_ast_db::Db {}
