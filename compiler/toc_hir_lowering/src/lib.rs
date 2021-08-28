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

mod lower;
mod scopes;

use toc_hir::library;
use toc_reporting::CompileResult;
use toc_span::{FileId, SpanTable};

use crate::lower::{LoweringCtx, LoweringDb};

#[cfg(test)]
mod test;

/// Lowers the given file as the root of a HIR library.
///
/// ## Returns
///
/// Returns the [`Library`] of the newly lowered HIR tree, along with a
/// [`SpanTable`] containing the interned spans.
///
/// [`Library`]: library::Library
pub fn lower_library(
    ast_db: &dyn LoweringDb,
    file: FileId,
) -> CompileResult<(library::Library, SpanTable)> {
    LoweringCtx::new(ast_db).lower_library(file)
}
