//! Source file interpretation queries

use toc_reporting::CompileResult;
use toc_span::FileId;

use crate::db;

pub(crate) fn parse_file(
    db: &dyn db::SourceParser,
    file_id: FileId,
) -> CompileResult<toc_parser::ParseTree> {
    let source = db.file_source(file_id);
    // FIXME: If a load error is present, then add it to the parse result / create a new one
    toc_parser::parse(Some(file_id), &source.0)
}

pub(crate) fn validate_file(db: &dyn db::SourceParser, file_id: FileId) -> CompileResult<()> {
    let cst = db.parse_file(file_id);
    toc_validate::validate_ast(Some(file_id), cst.result().syntax())
}

pub(crate) fn parse_depends(
    db: &dyn db::SourceParser,
    file_id: FileId,
) -> CompileResult<toc_parser::FileDepends> {
    let cst = db.parse_file(file_id);
    toc_parser::parse_depends(Some(file_id), cst.result().syntax())
}
