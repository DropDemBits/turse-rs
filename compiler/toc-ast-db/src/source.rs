//! Source file interpretation queries

use std::{
    collections::{BTreeSet, VecDeque},
    sync::Arc,
};

use toc_parser::ExternalLinks;
use toc_reporting::CompileResult;
use toc_span::{FileId, Span};
use toc_vfs_db::SourceFile;

use crate::Db;

/// What other files a given file refers to
#[salsa::tracked]
pub fn file_links(db: &dyn Db, source: SourceFile) -> Arc<toc_parser::ExternalLinks> {
    let mut links = ExternalLinks::default();
    let deps = crate::parse_depends(db, source);
    for dep in deps.result().dependencies() {
        // Get the target file
        let child =
            toc_vfs_db::resolve_path(db.up(), source.path(db.up()), dep.relative_path.clone());

        links.bind(dep.link_from.clone(), child.into());
    }

    Arc::new(links)
}

/// Parses the given file
#[salsa::tracked]
pub fn parse_file(db: &dyn Db, source: SourceFile) -> CompileResult<toc_parser::ParseTree> {
    // FIXME: If a load error is present, then add it to the parse result / create a new one
    let file_id: FileId = source.path(db.up()).into();
    toc_parser::parse(source.contents(db.up())).remap_spans(|range| Span::new(file_id, range.0))
}

/// Validates the file according to grammar validation rules
#[salsa::tracked]
pub fn validate_file(db: &dyn Db, source: SourceFile) -> CompileResult<()> {
    let file_id: FileId = source.path(db.up()).into();
    let cst = crate::parse_file(db, source);
    toc_validate::validate_ast(cst.result().syntax())
        .remap_spans(|range| Span::new(file_id, range.0))
}

/// Parse out the dependencies of a file
#[salsa::tracked]
pub fn parse_depends(
    db: &dyn Db,
    source: SourceFile,
) -> CompileResult<Arc<toc_parser::FileDepends>> {
    let file_id: FileId = source.path(db.up()).into();
    let cst = crate::parse_file(db, source);
    toc_parser::parse_depends(cst.result().syntax())
        .remap_spans(|range| Span::new(file_id, range.0))
}

/// Gets the [`ExternalLink`](toc_parser::ExternalLink)'s corresponding file
#[salsa::tracked]
pub fn file_link_of(
    db: &dyn Db,
    source: SourceFile,
    link: toc_parser::ExternalLink,
) -> Option<FileId> {
    crate::file_links(db, source).links_to(link)
}

/// Gets the set of all the transient file dependencies of `root` (excluding itself)
#[salsa::tracked]
pub fn reachable_files(db: &dyn Db, root: SourceFile) -> Arc<BTreeSet<SourceFile>> {
    let mut files = BTreeSet::default();
    let mut pending_queue = VecDeque::default();

    pending_queue.push_back(root);

    while let Some(current_file) = pending_queue.pop_front() {
        // Skip if we've already explored this file
        if files.contains(&current_file) {
            continue;
        }
        files.insert(current_file);

        // add all of this file's linked bits to the queue
        pending_queue.extend(
            crate::file_links(db, current_file)
                .all_links()
                .map(|path| toc_vfs_db::source_of(db.up(), path.into_raw())),
        );
    }

    files.remove(&root);

    Arc::new(files)
}

/// Gets the set of all the transient file imports of `root` (excluding itself)
#[salsa::tracked]
pub fn reachable_imported_files(db: &dyn Db, root: SourceFile) -> Arc<BTreeSet<SourceFile>> {
    let mut files = BTreeSet::default();
    let mut pending_queue = VecDeque::default();

    pending_queue.push_back(root);

    while let Some(current_file) = pending_queue.pop_front() {
        // Skip if we've already explored this file
        if files.contains(&current_file) {
            continue;
        }
        files.insert(current_file);

        // add all of the file's import linked bits to the queue
        pending_queue.extend(
            crate::file_links(db, current_file)
                .import_links()
                .map(|path| toc_vfs_db::source_of(db.up(), path.into_raw())),
        );
    }

    files.remove(&root);

    Arc::new(files)
}
