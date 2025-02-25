//! Database queries & structures for the AST level of compilation

pub mod db;
pub mod span;

mod source;

use toc_syntax::SyntaxNode;
use toc_vfs_db::SourceFile;
use upcast::{Upcast, UpcastFrom};

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

#[salsa::jar(db = Db)]
pub struct Jar(
    // source stuff
    source::file_links,
    source::file_link_of,
    source::parse_file,
    source::validate_file,
    source::parse_depends,
    source::reachable_files,
    source::reachable_imported_files,
    // span stuff
    line_mapping,
    map_byte_index,
    map_byte_index_to_character,
    map_byte_index_to_position,
);

pub trait Db: salsa::DbWithJar<Jar> + toc_vfs_db::Db + Upcast<dyn toc_vfs_db::Db> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + toc_vfs_db::Db + Upcast<dyn toc_vfs_db::Db> {}

impl<'db, DB: Db + 'db> UpcastFrom<DB> for dyn Db + 'db {
    fn up_from(value: &DB) -> &Self {
        value
    }
    fn up_from_mut(value: &mut DB) -> &mut Self {
        value
    }
}

pub trait IntoAst {
    type Db<'db>: Db + ?Sized + 'db;
    fn ast(self, db: &Self::Db<'_>) -> SyntaxNode;
}

impl IntoAst for SourceFile {
    type Db<'db> = dyn Db + 'db;
    fn ast(self, db: &Self::Db<'_>) -> SyntaxNode {
        parse_file(db, self).result().syntax()
    }
}
