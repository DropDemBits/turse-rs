//! Lowering implementation.
//! Fragmentend into submodules by node class

mod expr;
mod stmt;
mod ty;

use std::convert::TryInto;

use toc_hir::stmt::StmtIdx;
use toc_hir::Database;
use toc_reporting::MessageSink;
use toc_span::{TextRange, TextSize};
use toc_syntax::ast;

use crate::scopes;

pub(super) struct LoweringCtx {
    pub(super) database: Database,
    pub(super) messages: MessageSink,
    pub(super) scopes: scopes::ScopeBuilder,
}

impl LoweringCtx {
    pub(super) fn new() -> Self {
        Self {
            database: Database::new(),
            messages: MessageSink::new(),
            scopes: scopes::ScopeBuilder::new(),
        }
    }

    pub(super) fn lower_root(&mut self, root: ast::Source) -> Vec<StmtIdx> {
        // TODO: deal with root import statement (i.e. build up import info)
        let _is_child_unit = root.unit_token().is_some();

        let stmts = if let Some(stmts) = root.stmt_list() {
            stmts
                .stmts()
                .filter_map(|stmt| self.lower_stmt(stmt))
                .collect()
        } else {
            vec![]
        };

        stmts
    }
}

/// Offsets the given range pair by `source_span`
fn offset_span(start: usize, end: usize, source_span: TextRange) -> TextRange {
    let (start, end): (TextSize, TextSize) = (
        start
            .try_into()
            .unwrap_or_else(|_| TextSize::from(u32::MAX)),
        end.try_into().unwrap_or_else(|_| TextSize::from(u32::MAX)),
    );

    TextRange::new(source_span.start() + start, source_span.start() + end)
}
