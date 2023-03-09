//! Database queries & structures for the AST level of compilation

pub mod db;
pub mod span;

mod source;

pub use crate::source::{
    file_link_of, file_links, parse_depends, parse_file, reachable_files, reachable_imported_files,
    validate_file,
};
pub use crate::span::{
    query::{
        line_mapping, map_byte_index, map_byte_index_to_character, map_byte_index_to_position,
    },
    LineInfo, LineMapping, LspPosition,
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

pub trait Db: salsa::DbWithJar<Jar> + toc_vfs_db::Db {
    fn upcast_to_source_db(&self) -> &dyn Db;
}

impl<DB> Db for DB
where
    DB: salsa::DbWithJar<Jar> + toc_vfs_db::Db + toc_paths::Db,
{
    fn upcast_to_source_db(&self) -> &dyn Db {
        self
    }
}
