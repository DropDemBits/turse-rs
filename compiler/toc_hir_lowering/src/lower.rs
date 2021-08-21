//! Lowering implementation.
//! Fragmented into submodules by node class
#![allow(clippy::unnecessary_wraps)] // Top level lowering points also return Option

mod expr;
mod stmt;
mod ty;

use toc_hir::db::{self};
use toc_hir::stmt::StmtId;
use toc_reporting::MessageSink;
use toc_span::FileId;
use toc_syntax::ast;

use crate::scopes;

pub(super) struct LoweringCtx {
    pub(super) file: Option<FileId>,
    pub(super) database: db::HirBuilder,
    pub(super) messages: MessageSink,
    pub(super) scopes: scopes::ScopeBuilder,
}

impl LoweringCtx {
    pub(super) fn new(database: db::HirBuilder, file: Option<FileId>) -> Self {
        Self {
            file,
            database: database.clone(),
            messages: MessageSink::new(),
            scopes: scopes::ScopeBuilder::new(database),
        }
    }

    pub(super) fn lower_root(&mut self, root: ast::Source) -> Vec<StmtId> {
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
