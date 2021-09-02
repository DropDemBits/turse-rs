//! AST Query system definitions

use std::sync::Arc;

use toc_reporting::CompileResult;
use toc_salsa::salsa;
use toc_span::FileId;
use toc_vfs::db::FileSystem;

use crate::span::{LineInfo, LineMapping, LspPosition};
use crate::{source, span, SourceRoots};

#[salsa::query_group(SourceParserStorage)]
pub trait SourceParser: FileSystem {
    /// Source roots for all of the libraries
    #[salsa::input]
    fn source_roots(&self) -> SourceRoots;

    /// Parses the given file
    #[salsa::invoke(source::parse_file)]
    fn parse_file(&self, file_id: FileId) -> CompileResult<toc_parser::ParseTree>;

    /// Validates the file according to grammar validation rules
    #[salsa::invoke(source::validate_file)]
    fn validate_file(&self, file_id: FileId) -> CompileResult<()>;

    /// Parse out the dependencies of a file
    #[salsa::invoke(source::parse_depends)]
    fn parse_depends(&self, file_id: FileId) -> CompileResult<toc_parser::FileDepends>;
}

#[salsa::query_group(SpanMappingStorage)]
pub trait SpanMapping: toc_vfs::db::FileSystem {
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
}
