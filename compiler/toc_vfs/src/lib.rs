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

use std::convert::TryFrom;
use std::fmt;

pub use file_db::{FileDb, FileInfo};

/// Built-in prefixes for paths.
///
/// The [`FileSystem::set_prefix_expansion`][set_prefix_expansion] method
/// should be used to set the corresponding path the path expands into.
///
/// [set_prefix_expansion]: crate::query::FileSystem::set_prefix_expansion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinPrefix {
    /// %oot, should point to the Turing home directory
    /// (the directory containing "support" from an (Open)Turing installation).
    Oot,
    /// `%help`, should point to the Turing help directory
    /// (normally set as "%oot/support/help").
    Help,
    /// `%user`, should point to the current user's home directory
    /// (normally "$HOME" on Linux, or "C:/Users/%USER%" on Windows).
    UserHome,
    /// `%job`, can be specified by the user
    /// (set to a user provided path, or the temp directory root otherwise).
    Job,
    /// `%tmp`, should point to the temp directory
    /// (normally should be the path returned from [`env::temp_dir`][temp_dir])
    ///
    /// [temp_dir]: std::env::temp_dir
    Temp,
}

impl TryFrom<&str> for BuiltinPrefix {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "oot" => Self::Oot,
            "help" => Self::Help,
            "home" => Self::UserHome,
            "job" => Self::Job,
            "tmp" => Self::Temp,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for BuiltinPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BuiltinPrefix::Oot => "%oot",
            BuiltinPrefix::Help => "%help",
            BuiltinPrefix::UserHome => "%home",
            BuiltinPrefix::Job => "%job",
            BuiltinPrefix::Temp => "%tmp",
        })
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use crate::{
        fs_impl::{memory::MemoryFs, FsBackend},
        query::{FileSystem, FileSystemStorage, HasVFS, PathInternStorage},
        BuiltinPrefix,
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

    fn make_test_db(vfs: Arc<dyn FsBackend>) -> VfsTestDB {
        let mut database = VfsTestDB::new(vfs);

        // Setup the builtin prefixes to reasonable defaults
        database.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/".into());
        database.set_prefix_expansion(BuiltinPrefix::Help, "/help/".into());
        database.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/".into());
        database.set_prefix_expansion(BuiltinPrefix::Job, "/temp/".into());
        database.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/".into());

        database
    }

    #[test]
    fn file_retrieval() {
        const FILE_SOURCE: &str = r#"put "yee""#;

        let mut backend = MemoryFs::new();
        backend.add_file("/src/main.t", FILE_SOURCE);

        let backing_vfs = Arc::new(backend);
        let mut database = make_test_db(backing_vfs);

        database.set_root_file_path("/src/main.t".into());
        let root_file = database.root_file();

        let res = database.file_source(root_file);
        let (source, _load_error) = &*res;

        assert_eq!(source, FILE_SOURCE);
    }

    #[test]
    fn relative_path_resolve() {
        const FILE_SOURCES: &[&str] = &[r#"put "yee""#, r#"put "bam bam""#, r#"put "ye ye""#];

        let mut backend = MemoryFs::new();
        backend.add_file("/src/main.t", FILE_SOURCES[0]);
        backend.add_file("/src/foo/bar.t", FILE_SOURCES[1]);
        backend.add_file("/src/foo/bap.t", FILE_SOURCES[2]);

        let backing_vfs = Arc::new(backend);
        let mut database = make_test_db(backing_vfs);

        database.set_root_file_path("/src/main.t".into());

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

    #[test]
    fn path_expansion() {
        const FILE_SOURCES: &[&str] = &[
            r#"% not empty"#,
            r#""%oot/support/predefs/Net.tu""#,
            r#"unit module pervasive Net end Net"#,
            r#"a list of file keywords"#,
        ];

        let mut backend = MemoryFs::new();
        backend.add_file("/src/main.t", FILE_SOURCES[0]);
        backend.add_file("/oot/support/Predefs.lst", FILE_SOURCES[1]);
        backend.add_file("/oot/support/Net.tu", FILE_SOURCES[2]);
        backend.add_file("/oot/support/help/Keyword Lookup.txt", FILE_SOURCES[3]);
        backend.add_file("/home/special_file.t", FILE_SOURCES[0]);
        backend.add_file("/temp/pre/made/some-temp-item", FILE_SOURCES[0]);
        backend.add_file("/job/to/make/job-item", FILE_SOURCES[0]);

        let backing_vfs = Arc::new(backend);
        let mut database = make_test_db(backing_vfs);

        // Ensure that we are using the correct dirs
        database.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/".into());
        database.set_prefix_expansion(BuiltinPrefix::Help, "/oot/support/help".into());
        database.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/".into());
        database.set_prefix_expansion(BuiltinPrefix::Job, "/job/".into());
        database.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/".into());

        database.set_root_file_path("/src/main.t".into());

        // Lookup Predefs.lst file source
        let predefs_list = {
            let predefs_list =
                database.resolve_path(database.root_file(), "%oot/support/Predefs.lst");
            let res = database.file_source(predefs_list);
            assert_eq!(*res, (FILE_SOURCES[1].to_owned(), None));
            predefs_list
        };

        // Lookup Net.tu from Predefs.lst
        {
            let net_tu = database.resolve_path(predefs_list, "Net.tu");
            let res = database.file_source(net_tu);
            assert_eq!(*res, (FILE_SOURCES[2].to_owned(), None));
        };

        {
            let file = database.resolve_path(database.root_file(), "%help/Keyword Lookup.txt");
            let res = database.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[3].to_owned(), None));
        };

        {
            let file = database.resolve_path(database.root_file(), "%home/special_file.t");
            let res = database.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        };

        {
            let file = database.resolve_path(database.root_file(), "%tmp/pre/made/some-temp-item");
            let res = database.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        };

        {
            let file = database.resolve_path(database.root_file(), "%job/to/make/job-item");
            let res = database.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        };
    }
}
