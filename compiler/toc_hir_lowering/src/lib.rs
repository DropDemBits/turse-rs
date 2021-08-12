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

use toc_hir::db::HirBuilder;
use toc_hir::unit;
use toc_reporting::CompileResult;
use toc_span::FileId;
use toc_syntax::{
    ast::{self, AstNode},
    SyntaxNode,
};

use crate::lower::LoweringCtx;

#[cfg(test)]
mod test;

/// Lowers the AST into the HIR representation
///
/// ## Returns
///
/// Returns the [`UnitId`] of the newly lowered unit.
pub fn lower_ast(
    hir_db: HirBuilder,
    file: Option<FileId>,
    root_node: SyntaxNode,
) -> CompileResult<unit::UnitId> {
    let mut ctx = LoweringCtx::new(hir_db, file);
    let root = ast::Source::cast(root_node).unwrap();
    let unit_span = toc_span::Span::new(file, root.syntax().text_range());

    let stmts = ctx.lower_root(root);
    let LoweringCtx {
        database: hir_db,
        messages,
        scopes,
        ..
    } = ctx;
    let messages = messages.finish();

    let unit = hir_db.add_unit_with(
        move |id| {
            let symbol_table = scopes.finish();
            unit::Unit {
                id,
                stmts,
                symbol_table,
            }
        },
        unit_span,
    );

    CompileResult::new(unit, messages)
}
