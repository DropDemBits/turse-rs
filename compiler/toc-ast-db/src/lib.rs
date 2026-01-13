//! Database queries & structures for the AST level of compilation

pub mod ast_id;
pub mod db;
pub mod span;

mod source;

use toc_syntax::SyntaxNode;
use toc_vfs_db::SourceFile;

pub use crate::source::{
    file_link_of, file_links, parse_depends, parse_file, reachable_files, reachable_imported_files,
    validate_file,
};
pub use crate::span::{
    LineInfo, LineMapping, LspPosition,
    query::{
        line_mapping, map_byte_index, map_byte_index_to_character, map_byte_index_to_position,
    },
};

#[salsa::db]
pub trait Db: salsa::Database + toc_vfs_db::Db {}

#[salsa::db]
impl<DB> Db for DB where DB: toc_vfs_db::Db {}

pub trait IntoAst {
    type Db: ?Sized + Db;
    fn ast(self, db: &Self::Db) -> SyntaxNode;
}

impl IntoAst for SourceFile {
    type Db = dyn Db;
    fn ast(self, db: &Self::Db) -> SyntaxNode {
        parse_file(db, self).result().syntax()
    }
}

#[salsa::tracked(returns(ref))]
fn tracked_ast_id_map(db: &dyn Db, source_file: SourceFile) -> ast_id::AstIdMap {
    let root = source_file.ast(db);
    ast_id::AstIdMap::from_source(&root)
}

pub trait SourceFileExt {
    fn ast_id_map(self, db: &dyn Db) -> &ast_id::AstIdMap;
}

impl SourceFileExt for SourceFile {
    fn ast_id_map(self, db: &dyn Db) -> &ast_id::AstIdMap {
        tracked_ast_id_map(db, self)
    }
}
