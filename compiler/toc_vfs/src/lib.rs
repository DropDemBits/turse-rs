//! Abstraction over interfacing the native filesystem

/*
Design:
FileSystem
|
| HasVFS
TargetDatabase
| VfsBackend
|
- Memory
- Storage
- LSP (need to bubble up updates to FileSystem)
*/

pub mod file_db;
pub mod fs_impl;
pub mod query;

pub use file_db::{FileDb, FileInfo};

// ???: Where do we map paths?
// - Need to be able to lookup paths from special root paths
//   - `%oot`: Root Turing path, has sources for all of the predef modules
//   - `%home`: User home path
//   - `%job`: Path with an unknown purpose (in current Turing editor, this is not set)
//   - `%help`: Alias of "%oot/help"
//   - `%tmp`: Generated temporary directory
//
// -> We store mapped paths as inputs in the `FileSystemDatabase`

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use crate::{
        fs_impl::{memory::MemoryFS, FsBackend},
        query::{FileSystem, FileSystemStorage, HasVFS, PathInternStorage},
    };

    #[salsa::database(FileSystemStorage, PathInternStorage)]
    struct VfsTestDB {
        storage: salsa::Storage<Self>,
        vfs: Arc<dyn FsBackend>,
    }

    impl salsa::Database for VfsTestDB {}

    impl HasVFS for VfsTestDB {
        fn get_vfs(&self) -> &dyn FsBackend {
            &*self.vfs
        }
    }

    impl VfsTestDB {
        fn new(backing_vfs: Arc<dyn FsBackend>) -> Self {
            Self {
                storage: Default::default(),
                vfs: backing_vfs,
            }
        }
    }

    #[test]
    fn test_file_retrieval() {
        const FILE_SOURCE: &str = r#"put "yee""#;

        let mut backend = MemoryFS::new();
        backend.add_file("src/main.t", FILE_SOURCE);

        let backing_vfs = Arc::new(backend);
        let mut database = VfsTestDB::new(backing_vfs);

        database.set_root_file_path("src/main.t".into());
        let root_file = database.root_file();

        let res = database.file_source(root_file);
        let (source, _load_error) = &*res;

        assert_eq!(source, FILE_SOURCE);
    }

    #[test]
    fn test_relative_path_resolve() {
        const FILE_SOURCES: &[&str] = &[r#"put "yee""#, r#"put "bam bam""#, r#"put "ye ye""#];

        let mut backend = MemoryFS::new();
        backend.add_file("src/main.t", FILE_SOURCES[0]);
        backend.add_file("src/foo/bar.t", FILE_SOURCES[1]);
        backend.add_file("src/foo/bap.t", FILE_SOURCES[2]);

        let backing_vfs = Arc::new(backend);
        let mut database = VfsTestDB::new(backing_vfs);

        database.set_root_file_path("src/main.t".into());

        // Lookup root file source
        {
            let root_file = database.root_file();
            let res = database.file_source(root_file);
            let (source, _load_error) = &*res;

            assert_eq!(source, FILE_SOURCES[0]);
        }

        // Lookup bar.t file source
        let bar_t = {
            let bar_t = database.resolve_path(database.root_file(), "foo/bar.t");
            let res = database.file_source(bar_t);
            let (source, _load_error) = &*res;

            assert_eq!(source, FILE_SOURCES[1]);
            bar_t
        };

        // Lookup bap.t from bar.t
        {
            let bap_t = database.resolve_path(bar_t, "bap.t");
            let res = database.file_source(bap_t);
            let (source, _load_error) = &*res;

            assert_eq!(source, FILE_SOURCES[2]);
        };
    }
}
