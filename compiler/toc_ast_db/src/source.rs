//! Source file interpretation queries

use std::sync::Arc;

use toc_reporting::CompileResult;
use toc_salsa::salsa;
use toc_span::FileId;
use toc_vfs::query::FileSystem;

#[salsa::query_group(SourceParserStorage)]
pub trait SourceParser: FileSystem {
    /// Source roots for all of the libraries
    #[salsa::input]
    fn source_roots(&self) -> SourceRoots;

    /// Parses the given file
    fn parse_file(&self, file_id: FileId) -> CompileResult<toc_parser::ParseTree>;

    /// Validates the file according to grammar validation rules
    fn validate_file(&self, file_id: FileId) -> CompileResult<()>;

    /// Parse out the dependencies of a file
    fn parse_depends(&self, file_id: FileId) -> CompileResult<toc_parser::FileDepends>;
}

fn parse_file(db: &dyn SourceParser, file_id: FileId) -> CompileResult<toc_parser::ParseTree> {
    let source = db.file_source(file_id);
    // FIXME: If a load error is present, then add it to the parse result / create a new one
    toc_parser::parse(Some(file_id), &source.0)
}

fn validate_file(db: &dyn SourceParser, file_id: FileId) -> CompileResult<()> {
    let cst = db.parse_file(file_id);
    toc_validate::validate_ast(Some(file_id), cst.result().syntax())
}

fn parse_depends(db: &dyn SourceParser, file_id: FileId) -> CompileResult<toc_parser::FileDepends> {
    let cst = db.parse_file(file_id);
    toc_parser::parse_depends(Some(file_id), cst.result().syntax())
}

/// Library source roots
#[derive(Debug, Clone)]
pub struct SourceRoots {
    roots: Arc<Vec<FileId>>,
}

impl SourceRoots {
    pub fn new(roots: Vec<FileId>) -> Self {
        Self {
            roots: Arc::new(roots),
        }
    }

    /// Iterates over the source roots, from the bottom of the dependency graph and up
    pub fn roots(&self) -> impl Iterator<Item = FileId> + '_ {
        self.roots.iter().copied()
    }
}
