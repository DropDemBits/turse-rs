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
mod resolver;

use toc_hir::{library::LoweredLibrary, library_graph::LibraryGraph};
use toc_reporting::CompileResult;
use toc_span::FileId;

/// Trait representing a database that can store a lowered HIR tree
//
// Note: So long as we have a split between `hir_lowering` and `hir_db`, we can't use salsa's interner
// with a trait we defined here, as that would require a foreign blanket impl on a foreign trait.
// (this is why symbol interning is handled by the `internment` crate.)
pub trait LoweringDb: toc_ast_db::db::SourceParser {
    /// Lowers the given file as the root of a HIR library.
    ///
    /// ## Returns
    ///
    /// Returns the [`Library`] of the newly lowered HIR tree, along with a
    /// [`SpanTable`] containing the interned spans.
    ///
    /// [`Library`]: toc_hir::library::Library
    /// [`SpanTable`]: toc_span::SpanTable
    fn lower_library(&self, file: FileId) -> CompileResult<LoweredLibrary>;

    /// Lowers the entire library graph
    fn lower_library_graph(&self) -> CompileResult<LibraryGraph>;
}
