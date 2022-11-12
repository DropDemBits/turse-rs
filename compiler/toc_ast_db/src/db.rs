//! AST Query system definitions

use std::{collections::BTreeSet, sync::Arc};

use toc_reporting::CompileResult;
use toc_salsa::salsa;
use toc_source_graph::{ExternalLinks, SourceGraph};
use toc_span::FileId;
use toc_vfs::HasVfs;
use toc_vfs_db::db::FileSystem;

use crate::{
    source, span,
    span::{LineInfo, LineMapping, LspPosition},
};

#[salsa::query_group(SourceParserStorage)]
pub trait SourceParser: FileSystem {
    /// Source graph of all of the libraries
    #[salsa::input]
    fn source_graph(&self) -> Arc<SourceGraph>;

    /// What other files a given file refers to
    #[salsa::input]
    fn file_links(&self, file: FileId) -> Arc<ExternalLinks>;

    /// Parses the given file
    #[salsa::invoke(source::parse_file)]
    fn parse_file(&self, file_id: FileId) -> CompileResult<toc_parser::ParseTree>;

    /// Validates the file according to grammar validation rules
    #[salsa::invoke(source::validate_file)]
    fn validate_file(&self, file_id: FileId) -> CompileResult<()>;

    /// Parse out the dependencies of a file
    #[salsa::invoke(source::parse_depends)]
    fn parse_depends(&self, file_id: FileId) -> CompileResult<Arc<toc_parser::FileDepends>>;

    /// Gets the [`ExternalLink`](toc_parser::ExternalLink)'s corresponding file
    #[salsa::invoke(source::file_link_of)]
    fn file_link_of(&self, file: FileId, link: toc_parser::ExternalLink) -> Option<FileId>;

    /// Gets the set of all the transient file dependencies of `file`
    #[salsa::invoke(source::reachable_files)]
    fn reachable_files(&self, file: FileId) -> Arc<BTreeSet<FileId>>;

    /// Gets the set of all the transient file imports of `file`
    #[salsa::invoke(source::reachable_imported_files)]
    fn reachable_imported_files(&self, file: FileId) -> Arc<BTreeSet<FileId>>;
}

#[salsa::query_group(SpanMappingStorage)]
pub trait SpanMapping: FileSystem + HasVfs {
    #[salsa::invoke(span::query::line_mapping)]
    fn line_mapping(&self, file_id: toc_span::FileId) -> Arc<LineMapping>;

    #[salsa::invoke(span::query::file_path)]
    fn file_path(&self, file_id: toc_span::FileId) -> Arc<String>;

    #[salsa::invoke(span::query::map_byte_index)]
    fn map_byte_index(&self, file: toc_span::FileId, index: usize) -> Option<LineInfo>;

    #[salsa::invoke(span::query::map_byte_index_to_position)]
    fn map_byte_index_to_position(
        &self,
        file: toc_span::FileId,
        index: usize,
    ) -> Option<LspPosition>;

    #[salsa::invoke(span::query::map_byte_index_to_character)]
    fn map_byte_index_to_character(&self, file: toc_span::FileId, index: usize) -> Option<usize>;
}

pub trait AstDatabaseExt: FileSystem + SourceParser {
    /// Reloads all files accessible from the source roots using the given file loader
    ///
    /// Also rebuilds all dependency graphs
    fn invalidate_source_graph(&mut self, loader: &dyn toc_vfs::FileLoader);
}
