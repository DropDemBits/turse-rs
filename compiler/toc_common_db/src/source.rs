//! Source file interpretation queries

use std::sync::Arc;

use toc_salsa::salsa;
use toc_span::FileId;
use toc_vfs::query::FileSystem;

#[salsa::query_group(SourceParserStorage)]
pub trait SourceParser: FileSystem {
    /// Parses the given file
    fn parse_file(&self, file_id: FileId) -> Arc<toc_parser::ParseResult>;

    /// Validates the file according to grammar validation rules
    fn validate_file(&self, file_id: FileId) -> Arc<toc_validate::ValidateResult>;

    // TODO: Add dependency extraction query (take code from toc_driver)
}

fn parse_file(db: &dyn SourceParser, file_id: FileId) -> Arc<toc_parser::ParseResult> {
    let source = db.file_source(file_id);
    // FIXME: If a load error is present, then add it to the parse result / create a new one
    let res = toc_parser::parse(Some(file_id), &source.0);

    Arc::new(res)
}

fn validate_file(db: &dyn SourceParser, file_id: FileId) -> Arc<toc_validate::ValidateResult> {
    let cst = db.parse_file(file_id);
    let res = toc_validate::validate_ast(Some(file_id), cst.syntax());

    Arc::new(res)
}
