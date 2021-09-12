//! Source file interpretation queries

use std::collections::VecDeque;

use toc_reporting::CompileResult;
use toc_span::FileId;
use toc_vfs::{db::VfsDatabaseExt, PathResolution};

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

impl<T> db::AstDatabaseExt for T
where
    T: toc_vfs::db::FileSystem + db::SourceParser,
{
    fn reload_source_roots(&mut self, loader: &dyn toc_vfs::FileLoader) {
        let db = self;
        let source_roots = db.source_roots();
        let mut reached_files = std::collections::HashSet::new();

        let mut pending_files: VecDeque<_> = source_roots.roots().collect();
        while let Some(current_file) = pending_files.pop_front() {
            if !reached_files.insert(current_file) {
                // We've already reached this file, don't enter into a cycle
                // TODO: Report that we've reached a cycle if we're dealing with include chains
                continue;
            }

            // Load in the file source
            let path = db.get_vfs().lookup_path(current_file);
            let res = loader.load_file(path);
            db.update_file(current_file, res);

            let deps = db.parse_depends(current_file);
            for dep in deps.result().dependencies() {
                let child = match db
                    .get_vfs()
                    .resolve_path(Some(current_file), &dep.relative_path)
                {
                    PathResolution::Interned(id) => id,
                    PathResolution::NewPath(path) => {
                        let intern_path = loader.normalize_path(&path).unwrap_or(path);
                        db.get_vfs_mut().intern_path(intern_path)
                    }
                };

                // Update the resolution path
                // ???: Any way to reduce changes to this?
                // Only needs to change if the parent file is modified
                db.set_resolve_path(current_file, dep.relative_path.clone(), child);
                pending_files.push_back(child);
            }
        }
    }
}
