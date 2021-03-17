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
// FileDB should only care about giving unique file handes corresponding to text sources.
// Other DBs will deal with what they map to (e.g. a separate DB managing all of the CSTs)

mod lower;
mod scopes;

use toc_hir::{Database, Unit};
use toc_reporting::ReportMessage;
use toc_syntax::{
    ast::{self, AstNode},
    SyntaxNode,
};

use crate::lower::LoweringCtx;

#[cfg(test)]
mod test;

pub struct HirLowerResult {
    pub database: Database,
    pub unit: Unit,
    messages: Vec<ReportMessage>,
}

impl HirLowerResult {
    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }
}

pub fn lower_ast(root_node: SyntaxNode) -> HirLowerResult {
    let mut ctx = LoweringCtx::new();
    let root = ast::Source::cast(root_node).unwrap();

    let stmts = ctx.lower_root(root);
    let LoweringCtx {
        database,
        messages,
        scopes,
    } = ctx;
    let messages = messages.finish();

    let unit = toc_hir::Unit {
        stmts,
        symbol_table: scopes.finish(),
    };

    HirLowerResult {
        database,
        unit,
        messages,
    }
}
