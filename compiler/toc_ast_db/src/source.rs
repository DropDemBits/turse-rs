//! Source file interpretation queries

use std::collections::VecDeque;
use std::sync::Arc;

use toc_reporting::CompileResult;
use toc_source_graph::{DependGraph, SourceDepend, SourceKind};
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

pub(crate) fn depend_of(
    db: &dyn db::SourceParser,
    library: FileId,
    from: FileId,
    relative_path: String,
) -> (FileId, SourceKind) {
    db.depend_graph(library).depend_of(from, relative_path)
}

impl<T> db::AstDatabaseExt for T
where
    T: toc_vfs::HasVfs + toc_vfs::db::FileSystem + db::SourceParser,
{
    fn invalidate_source_graph(&mut self, loader: &dyn toc_vfs::FileLoader) {
        let db = self;
        let source_graph = db.source_graph();
        let mut pending_files: VecDeque<_> = vec![].into();

        for library_root in source_graph.library_roots() {
            let mut depend_graph = DependGraph::new(library_root);
            pending_files.push_back(library_root);

            while let Some(current_file) = pending_files.pop_front() {
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
                    let kind = match dep.kind {
                        toc_parser::DependencyKind::Include => SourceKind::Include,
                        toc_parser::DependencyKind::Import => SourceKind::Unit,
                    };

                    let info = SourceDepend {
                        relative_path: dep.relative_path.clone(),
                        kind,
                    };
                    depend_graph.add_source_dep(current_file, child, info);
                    pending_files.push_back(child);
                }
            }

            // Update the depend graph
            db.set_depend_graph(library_root, Arc::new(depend_graph));
        }
    }
}
