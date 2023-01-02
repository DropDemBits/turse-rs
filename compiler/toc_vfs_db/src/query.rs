//! Query interface for the VFS system
//!
use std::{borrow::Cow, path::Path, sync::Arc};

use camino::{Utf8Component, Utf8PathBuf};
use toc_span::FileId;

use crate::db::{FileSystem, FilesystemExt, PathIntern, VfsDatabaseExt};
use toc_vfs::{BuiltinPrefix, ErrorKind, LoadError, LoadResult, LoadStatus};

// Non query stuff //

impl<T> FilesystemExt for T
where
    T: FileSystem + PathIntern,
{
    fn resolve_path(
        &self,
        relative_to: Option<FileId>,
        path: &str,
        loader: &dyn toc_vfs::FileLoader,
    ) -> FileId {
        // Convert `path` into an absolute one
        let expanded_path = self.expand_path(path.into());

        let full_path = if !path_helper::needs_joining(&expanded_path) {
            // Already an absolute path
            expanded_path
        } else if let Some(relative_to) = relative_to {
            // Tack on the parent path
            let mut parent_path = self.lookup_intern_path(relative_to);
            assert!(parent_path.pop(), "parent path for file was empty");

            // Join paths together, applying path de-dotting
            path_helper::join_dedot(parent_path, &expanded_path)
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

        // Use the provided path normalizer to guarantee that we have a uniform form of path
        let full_path = loader
            .normalize_path(full_path.as_path().as_std_path())
            .map_or(full_path, |aa| Utf8PathBuf::try_from(aa).unwrap());

        self.intern_path(full_path)
    }

    fn expand_path(&self, path: &camino::Utf8Path) -> Utf8PathBuf {
        // Check if the given path has a percent prefix
        let prefix_name = if let Some(Utf8Component::Normal(comp)) = path.components().next() {
            if let Some(prefix_name) = comp.strip_prefix('%') {
                prefix_name
            } else {
                return path.to_owned();
            }
        } else {
            return path.to_owned();
        };

        match BuiltinPrefix::try_from(prefix_name) {
            Ok(prefix_path) => {
                let base_path = self.prefix_expansion(prefix_path);

                path_helper::join_dedot(
                    base_path,
                    path.strip_prefix(prefix_path.to_string()).unwrap(),
                )
            }
            Err(_) => {
                // No corresponding path prefix, return the path as-is
                path.to_owned()
            }
        }
    }
}

impl<T> VfsDatabaseExt for T
where
    T: FileSystem + PathIntern,
{
    fn insert_file<P: AsRef<Path>>(&mut self, path: P, source: &str) -> FileId {
        // Intern the path, then add it to the db
        let file_id = self.intern_path(Utf8PathBuf::try_from(path.as_ref().to_owned()).unwrap());
        self.set_file_source(file_id, (Arc::new(source.into()), None));
        file_id
    }

    fn insert_fixture(&mut self, fixture: toc_vfs::FixtureFiles) {
        // Collect all of the interned paths first
        let files = fixture
            .files
            .into_iter()
            .map(|(path, src)| (self.intern_path(path.try_into().unwrap()), src))
            .collect::<Vec<_>>();

        for (file, source) in files {
            self.update_file(file, source);
        }
    }

    fn update_file(&mut self, file_id: FileId, result: LoadResult) {
        let result = match result {
            Ok(LoadStatus::Unchanged) => return,
            Ok(LoadStatus::Modified(byte_source)) => {
                // FIXME: Deal with different character encodings (per `VFS Interface.md`)
                // This is likely the place where we'd do it

                // Try converting the file into UTF-8
                let source = String::from_utf8_lossy(&byte_source);

                match source {
                    Cow::Borrowed(_source) => {
                        // Steal memory from the cloning process
                        (String::from_utf8(byte_source).unwrap(), None)
                    }
                    Cow::Owned(invalid) => {
                        // Non UTF-8 encoded characters
                        (
                            invalid,
                            Some(LoadError::new(
                                self.lookup_intern_path(file_id).as_std_path(),
                                ErrorKind::InvalidEncoding,
                            )),
                        )
                    }
                }
            }
            Err(err) => {
                // Error encountered during loading
                (String::new(), Some(err))
            }
        };
        let result = (Arc::new(result.0), result.1);

        self.set_file_source(file_id, result);
    }
}

/// ???: Do we need these (i.e. does camino handle it?)
mod path_helper {
    use camino::{Utf8Component, Utf8Path, Utf8PathBuf, Utf8Prefix};

    /// Joins two paths together, applying `ParentDir` and `CurrentDir` components
    ///
    /// `append` must be a path that needs joining
    pub(super) fn join_dedot(mut base_path: Utf8PathBuf, append: &Utf8Path) -> Utf8PathBuf {
        assert!(needs_joining(append));

        let mut comps = append.components().peekable();

        // Deal with the first component
        if let Some(first) = comps.peek() {
            match *first {
                Utf8Component::Prefix(prefix) => {
                    if let Utf8Prefix::Disk(_) = prefix.kind() {
                        assert!(!matches!(comps.next(), Some(Utf8Component::RootDir)));

                        // Check if we need to fixup the drive prefix
                        // Only need to do so if theres either a (different) drive prefix,
                        // or no prefix at all

                        let mut base_comps = base_path.components().peekable();

                        let needs_fixup = match base_comps.peek() {
                            // Do the fixup for drive prefix paths
                            // Don't change drive for non-drive prefix paths
                            Some(Utf8Component::Prefix(other_prefix)) => {
                                matches!(other_prefix.kind(), Utf8Prefix::Disk(_))
                            }
                            // Append the drive prefix for all other components
                            _ => true,
                        };

                        if needs_fixup {
                            // Strip off existing drive prefix and join the rest of the path back
                            let new_path = std::iter::once(Utf8Component::Prefix(prefix))
                                .chain(
                                    base_comps
                                        .skip_while(|c| matches!(c, Utf8Component::Prefix(_))),
                                )
                                .collect();
                            base_path = new_path;
                        }
                    } else {
                        // per the assert above
                        unreachable!()
                    }
                }
                Utf8Component::RootDir => unreachable!("tried to append on a path with a root dir"),
                _ => {}
            }
        }

        for comp in comps {
            match comp {
                Utf8Component::CurDir => {}
                Utf8Component::ParentDir => {
                    // go up
                    base_path.pop();
                }
                Utf8Component::Normal(comp) => {
                    // append component
                    base_path.push(comp);
                }
                // absolute component
                // should never be reachable since we should've already dealt with them
                Utf8Component::Prefix(_) | Utf8Component::RootDir => unreachable!(),
            }
        }

        base_path
    }

    /// Returns true if the path is needs to be joined onto another path
    pub(super) fn needs_joining(path: &Utf8Path) -> bool {
        let mut comps = path.components();
        let Some(first) = comps.next() else {
        // empty path
        return true;
    };

        match first {
            Utf8Component::Prefix(prefix) => {
                if matches!(prefix.kind(), Utf8Prefix::Disk(_)) {
                    // Only consider for matching if the next component isn't a root dir
                    !matches!(comps.next(), Some(Utf8Component::RootDir))
                } else {
                    // None of the other prefixes should be considered for joining
                    false
                }
            }
            // Doesn't need joining, driver directory will come from current drive
            Utf8Component::RootDir => false,
            // Definitely relative components
            Utf8Component::CurDir | Utf8Component::ParentDir | Utf8Component::Normal(_) => true,
        }
    }
}
