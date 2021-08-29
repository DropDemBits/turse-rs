//! Abstraction over interfacing the native filesystem
//!
//! Path expansion operations, path interning, and intermediate file storage
//! are held in the concrete [`Vfs`] type, and file sources for the compiler
//! are accessed via the [`FileSystem::file_source`] query.
//!
//! Resolving paths into [`FileId`]s via the [`FileSystem::resolve_path`]
//! query is required during lowering the concrete syntax tree into HIR,
//! specifically during:
//!
//! - Include file expansion in order to get the file id for getting the parse tree
//! - Import dependency resolution for generating dependencies between units
//!
//! ## Note
//!
//! All paths must be encountered before executing any queries, since no paths are interned from queries
//!
//! [`Vfs`]: crate::query::Vfs
//! [`FileSystem::file_source`]: crate::query::FileSystem::file_source
//! [`FileSystem::resolve_path`]: crate::query::FileSystem::resolve_path
//! [`FileId`]: toc_span::FileId

// TODO: Flesh out documentation using VFS Interface.md
// TODO: Generate a test fixture VFS tree from a given source string

pub mod query;

use std::convert::TryFrom;
use std::fmt;

// TODO: migrate into here
pub use query::{HasVfs, Vfs};

/// Built-in prefixes for paths.
///
/// The [`Vfs::set_prefix_expansion`][set_prefix_expansion] method
/// should be used to set the corresponding path the path expands into.
///
/// [set_prefix_expansion]: crate::query::Vfs::set_prefix_expansion
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LoadError {
    /// File was not found
    NotFound,
    /// File is in an unsupported encoding
    InvalidEncoding,
    /// Other loading error
    Other(String),
}

#[cfg(test)]
mod test {
    use toc_salsa::salsa;

    use crate::query::VfsDatabaseExt;
    use crate::{
        query::{FileSystem, FileSystemStorage},
        BuiltinPrefix,
    };
    use crate::{HasVfs, LoadError, Vfs};

    #[salsa::database(FileSystemStorage)]
    struct VfsTestDB {
        storage: salsa::Storage<Self>,
        vfs: Vfs,
    }

    impl salsa::Database for VfsTestDB {}

    impl HasVfs for VfsTestDB {
        fn get_vfs(&self) -> &Vfs {
            &self.vfs
        }
    }

    impl VfsTestDB {
        fn new() -> Self {
            Self {
                storage: Default::default(),
                vfs: Default::default(),
            }
        }
    }

    fn make_test_db() -> VfsTestDB {
        let mut db = VfsTestDB::new();
        let vfs = &mut db.vfs;

        // Setup the builtin prefixes to reasonable defaults
        vfs.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/");
        vfs.set_prefix_expansion(BuiltinPrefix::Help, "/help/");
        vfs.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/");
        vfs.set_prefix_expansion(BuiltinPrefix::Job, "/temp/");
        vfs.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/");

        db
    }

    #[test]
    fn file_retrieval() {
        const FILE_SOURCE: &str = r#"put "yee""#;

        let mut db = make_test_db();
        let root_file = db.vfs.insert_file("/src/main.t", FILE_SOURCE);

        db.invalidate_files();

        let res = db.file_source(root_file);
        let (source, _load_error) = &*res;

        assert_eq!(source, FILE_SOURCE);
    }

    #[test]
    fn relative_path_resolve() {
        const FILE_SOURCES: &[&str] = &[r#"put "yee""#, r#"put "bam bam""#, r#"put "ye ye""#];

        let mut db = make_test_db();
        let root_file = db.vfs.insert_file("/src/main.t", FILE_SOURCES[0]);
        db.vfs.insert_file("/src/foo/bar.t", FILE_SOURCES[1]);
        db.vfs.insert_file("/src/foo/bap.t", FILE_SOURCES[2]);

        db.invalidate_files();

        // Lookup root file source
        {
            let res = db.file_source(root_file);
            let (source, _load_error) = &*res;

            assert_eq!(source, FILE_SOURCES[0]);
        }

        // Lookup bar.t file source
        let bar_t = {
            let bar_t = db.resolve_path(root_file, "foo/bar.t");
            let res = db.file_source(bar_t);
            let (source, _load_error) = &*res;

            assert_eq!(source, FILE_SOURCES[1]);
            bar_t
        };

        // Lookup bap.t from bar.t
        {
            let bap_t = db.resolve_path(bar_t, "bap.t");
            let res = db.file_source(bap_t);
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

        let mut db = make_test_db();
        let root_file = {
            let vfs = &mut db.vfs;
            let root_file = vfs.insert_file("/src/main.t", FILE_SOURCES[0]);
            vfs.insert_file("/oot/support/Predefs.lst", FILE_SOURCES[1]);
            vfs.insert_file("/oot/support/Net.tu", FILE_SOURCES[2]);
            vfs.insert_file("/oot/support/help/Keyword Lookup.txt", FILE_SOURCES[3]);
            vfs.insert_file("/home/special_file.t", FILE_SOURCES[0]);
            vfs.insert_file("/temp/pre/made/some-temp-item", FILE_SOURCES[0]);
            vfs.insert_file("/job/to/make/job-item", FILE_SOURCES[0]);

            // Ensure that we are using the correct dirs
            vfs.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/");
            vfs.set_prefix_expansion(BuiltinPrefix::Help, "/oot/support/help");
            vfs.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/");
            vfs.set_prefix_expansion(BuiltinPrefix::Job, "/job/");
            vfs.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/");

            db.invalidate_files();

            root_file
        };

        // Lookup Predefs.lst file source
        let predefs_list = {
            let predefs_list = db.resolve_path(root_file, "%oot/support/Predefs.lst");
            let res = db.file_source(predefs_list);
            assert_eq!(*res, (FILE_SOURCES[1].to_owned(), None));
            predefs_list
        };

        // Lookup Net.tu from Predefs.lst
        {
            let net_tu = db.resolve_path(predefs_list, "Net.tu");
            let res = db.file_source(net_tu);
            assert_eq!(*res, (FILE_SOURCES[2].to_owned(), None));
        };

        {
            let file = db.resolve_path(root_file, "%help/Keyword Lookup.txt");
            let res = db.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[3].to_owned(), None));
        };

        {
            let file = db.resolve_path(root_file, "%home/special_file.t");
            let res = db.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        };

        {
            let file = db.resolve_path(root_file, "%tmp/pre/made/some-temp-item");
            let res = db.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        };

        {
            let file = db.resolve_path(root_file, "%job/to/make/job-item");
            let res = db.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        };
    }

    #[test]
    fn file_update() {
        const FILE_SOURCES: &[&str] = &[r#"var tee : int := 1"#, r#"var shoe : int := 2"#];

        let mut db = make_test_db();
        let file = db.vfs.insert_file("/main.t", FILE_SOURCES[0]);

        db.invalidate_files();

        {
            let res = db.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[0].to_owned(), None));
        }

        db.update_file(file, Some(FILE_SOURCES[1].into()));
        {
            let res = db.file_source(file);
            assert_eq!(*res, (FILE_SOURCES[1].to_owned(), None));
        }

        db.update_file(file, None);
        {
            let res = db.file_source(file);
            assert_eq!(*res, (String::new(), Some(LoadError::NotFound)));
        }
    }
}
