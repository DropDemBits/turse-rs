//! The actual VFS structures

use std::collections::HashMap;
use std::convert::TryFrom;
use std::path::{Component, Path, PathBuf};

use toc_span::FileId;

use crate::intern::{PathInterner, PathResolution};
use crate::BuiltinPrefix;

// TODO: Unify path slashes to the platform preferred separator
// This should be dependent on if we have legacy-mode path transformations enabled

/// Concrete virtual filesystem interface
///
// TODO: This behaviour has changed, and the `Vfs` is now just a path tree abstraction
/// File sources are loaded into the main `Vfs` type, and file sources in
/// the form of raw binary blobs can come from anywhere.
/// For example, they can be generated from diffs provided from a
/// language client, or they can be loaded from the filesystem directly
/// through [`std::fs::read`].
#[derive(Debug, Default)]
pub struct Vfs {
    path_interner: PathInterner,
    builtin_expansions: HashMap<BuiltinPrefix, PathBuf>,
}

impl Vfs {
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolves a path relative to a given file.
    ///
    /// If `path` expands into an absolute path, then `relative_to` is ignored
    ///
    /// If `relative_to` is [`None`], then the expanded path will be treated as an absolute one.
    pub fn resolve_path(&self, relative_to: Option<FileId>, path: &str) -> PathResolution {
        // Convert `path` into an absolute one
        let expanded_path = self.expand_path(path);

        let full_path = if expanded_path.is_absolute() {
            // Already an absolute path
            expanded_path
        } else if let Some(relative_to) = relative_to {
            // Tack on the parent path
            let mut parent_path = self.path_interner.lookup_path(relative_to).to_owned();
            assert!(parent_path.pop(), "parent path for file was empty");

            // Join paths together, applying path de-dotting
            join_dedot(parent_path, &expanded_path)
        } else {
            // The only place that I can think that needs an absolute path is
            // for looking up the predef list, which always becomes an absolute path.
            //
            // However, even then the predef list is usually accessible via the `%oot`
            // prefix, so it's likely just bad practice that's happening here
            //
            // For now, treat as-is, but may or may not be an absolute path.
            expanded_path
        };

        // FIXME: Apply path normalization
        // Use a provided path normalizer to guarantee that we have a uniform form of path

        match self.path_interner.lookup_id(full_path.as_path()) {
            Some(id) => PathResolution::Interned(id),
            None => PathResolution::NewPath(full_path),
        }
    }

    /// Expands a path, dealing with any percent prefixes
    pub fn expand_path(&self, path: impl AsRef<Path>) -> PathBuf {
        let path = path.as_ref();

        // Check if the given path has a percent prefix
        let prefix_name = if_chain::if_chain! {
            if let Some(Component::Normal(comp)) = path.components().next();
            // If the given path component is not a valid unicode string, then it's safe to bail out
            // None of the builtin percent prefixes only contain alphabetic ascii characters.
            if let Some(comp) = comp.to_str();
            if let Some(prefix_name) = comp.strip_prefix('%');
            then {
                prefix_name
            }
            else {
                return path.to_owned();
            }
        };

        match BuiltinPrefix::try_from(prefix_name) {
            Ok(prefix_path) => {
                let base_path = self
                    .builtin_expansions
                    .get(&prefix_path)
                    .expect("missing path prefix");
                join_dedot(
                    base_path.to_owned(),
                    path.strip_prefix(prefix_path.to_string()).unwrap(),
                )
            }
            Err(_) => {
                // No corresponding path prefix, return the path as-is
                path.to_owned()
            }
        }
    }

    /// Sets the path to expand to for a given prefix
    pub fn set_prefix_expansion(&mut self, prefix: BuiltinPrefix, expansion: impl AsRef<Path>) {
        self.builtin_expansions
            .insert(prefix, expansion.as_ref().to_path_buf());
    }

    /// Interns the given path into the corresponding [`FileId`]
    ///
    /// # Panics
    ///
    /// Panics if there are too many paths that are interned
    ///
    /// # Returns
    ///
    /// The corresponding [`FileId`] for the path
    pub fn intern_path(&mut self, path: PathBuf) -> FileId {
        self.path_interner.intern_path(path)
    }

    /// Looks up the path corresponding to the given [`FileId`]
    pub fn lookup_path(&self, file_id: FileId) -> &std::path::Path {
        self.path_interner.lookup_path(file_id)
    }
}

/// Trait providing the query system access to the virtual file system
pub trait HasVfs {
    // Get access to the underlying virtual file system
    fn get_vfs(&self) -> &Vfs;

    // Get mutable access to the underlying virtual file system
    fn get_vfs_mut(&mut self) -> &mut Vfs;
}

/// Joins two paths together, applying `ParentDir` and `CurrentDir` components
///
/// `append` must be a relative path
fn join_dedot(mut path: PathBuf, append: &Path) -> PathBuf {
    assert!(append.is_relative());

    for comp in append.components() {
        match comp {
            Component::CurDir => {}
            Component::ParentDir => {
                // go up
                path.pop();
            }
            Component::Normal(comp) => {
                // append component
                path.push(comp);
            }
            // absolute components
            // should never be reachable since we check for absolute expanded paths
            Component::Prefix(_) | Component::RootDir => unreachable!(),
        }
    }

    path
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn resolve_dedots_expansion() {
        let mut vfs = Vfs::new();

        vfs.set_prefix_expansion(BuiltinPrefix::Oot, "/path/to/oot");
        let help = vfs.intern_path("/path/to/some/help".into());

        let resolve = vfs.resolve_path(None, "%oot/../other/.././oot/../some/help");
        assert_eq!(resolve, PathResolution::Interned(help));
    }

    #[test]
    fn resolve_dedots_relative() {
        let mut vfs = Vfs::new();

        let child = vfs.intern_path("/src/subdir/child".into());
        let main = vfs.intern_path("/src/main.t".into());

        let resolve = vfs.resolve_path(Some(child), "../././././main.t");
        assert_eq!(resolve, PathResolution::Interned(main));
    }
}
