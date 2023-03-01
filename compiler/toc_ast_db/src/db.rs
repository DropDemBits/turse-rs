//! AST Query system definitions

pub trait AstDatabaseExt {
    /// Rebuilds the mapping of which files refer to which other files,
    /// loading new files  using the given file loader.
    fn rebuild_file_links(&mut self, loader: &dyn toc_vfs::FileLoader);
}
