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

use toc_hir::library::LoweredLibrary;

pub use lower::{lower_library, lower_source_graph};
use upcast::{Upcast, UpcastFrom};

#[salsa::jar(db = Db)]
pub struct Jar(lower::lower_library, lower::lower_source_graph);

/// Trait representing a database that can store a lowered HIR tree
pub trait Db:
    salsa::DbWithJar<Jar>
    + toc_source_graph::Db
    + toc_ast_db::Db
    + Upcast<dyn toc_source_graph::Db>
    + Upcast<dyn toc_ast_db::Db>
{
}

impl<DB> Db for DB where
    DB: salsa::DbWithJar<Jar>
        + toc_source_graph::Db
        + toc_ast_db::Db
        + Upcast<dyn toc_source_graph::Db>
        + Upcast<dyn toc_ast_db::Db>
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
