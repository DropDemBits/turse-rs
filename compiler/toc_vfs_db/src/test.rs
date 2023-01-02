use std::ops::Deref;

use toc_salsa::salsa;
use toc_vfs::{BuiltinPrefix, DummyFileLoader, LoadError, LoadStatus};

use crate::db::{FileSystem, FileSystemStorage, FilesystemExt, PathInternStorage, VfsDatabaseExt};

#[salsa::database(FileSystemStorage, PathInternStorage)]
#[derive(Default)]
struct VfsTestDB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for VfsTestDB {}

impl VfsTestDB {
    fn new() -> Self {
        Self::default()
    }
}

fn make_test_db() -> VfsTestDB {
    let mut db = VfsTestDB::new();

    // Setup the builtin prefixes to reasonable defaults
    db.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/".into());
    db.set_prefix_expansion(BuiltinPrefix::Help, "/help/".into());
    db.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/".into());
    db.set_prefix_expansion(BuiltinPrefix::Job, "/temp/".into());
    db.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/".into());

    db
}

#[test]
fn file_retrieval() {
    const FILE_SOURCE: &str = r#"put "yee""#;

    let mut db = make_test_db();
    let root_file = db.insert_file("/src/main.t", FILE_SOURCE);

    let res = db.file_source(root_file);
    let (source, _load_error) = res;

    assert_eq!(source.deref(), FILE_SOURCE);
}

#[test]
fn relative_path_resolve() {
    const FILE_SOURCES: &[&str] = &[r#"put "yee""#, r#"put "bam bam""#, r#"put "ye ye""#];

    let mut db = make_test_db();
    let root_file = db.insert_file("/src/main.t", FILE_SOURCES[0]);
    db.insert_file("/src/foo/bar.t", FILE_SOURCES[1]);
    db.insert_file("/src/foo/bap.t", FILE_SOURCES[2]);

    // Lookup root file source
    {
        let res = db.file_source(root_file);
        let (source, _load_error) = res;

        assert_eq!(source.deref(), FILE_SOURCES[0]);
    }

    // Lookup bar.t file source
    let bar_t = {
        let bar_t = db.resolve_path(Some(root_file), "foo/bar.t", &DummyFileLoader);
        let res = db.file_source(bar_t);
        let (source, _load_error) = res;

        assert_eq!(source.deref(), FILE_SOURCES[1]);
        bar_t
    };

    // Lookup bap.t from bar.t
    {
        let bap_t = db.resolve_path(Some(bar_t), "bap.t", &DummyFileLoader);
        let res = db.file_source(bap_t);
        let (source, _load_error) = res;

        assert_eq!(source.deref(), FILE_SOURCES[2]);
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
        let root_file = db.insert_file("/src/main.t", FILE_SOURCES[0]);
        db.insert_file("/oot/support/Predefs.lst", FILE_SOURCES[1]);
        db.insert_file("/oot/support/Net.tu", FILE_SOURCES[2]);
        db.insert_file("/oot/support/help/Keyword Lookup.txt", FILE_SOURCES[3]);
        db.insert_file("/home/special_file.t", FILE_SOURCES[0]);
        db.insert_file("/temp/pre/made/some-temp-item", FILE_SOURCES[0]);
        db.insert_file("/job/to/make/job-item", FILE_SOURCES[0]);

        // Ensure that we are using the correct dirs
        db.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/".into());
        db.set_prefix_expansion(BuiltinPrefix::Help, "/oot/support/help".into());
        db.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/".into());
        db.set_prefix_expansion(BuiltinPrefix::Job, "/job/".into());
        db.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/".into());

        root_file
    };

    // Lookup Predefs.lst file source
    let predefs_list = {
        let predefs_list = db.resolve_path(
            Some(root_file),
            "%oot/support/Predefs.lst",
            &DummyFileLoader,
        );
        let res = db.file_source(predefs_list);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[1], None));
        predefs_list
    };

    // Lookup Net.tu from Predefs.lst
    {
        let net_tu = db.resolve_path(Some(predefs_list), "Net.tu", &DummyFileLoader);
        let res = db.file_source(net_tu);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[2], None));
    };

    {
        let file = db.resolve_path(
            Some(root_file),
            "%help/Keyword Lookup.txt",
            &DummyFileLoader,
        );
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[3], None));
    };

    {
        let file = db.resolve_path(Some(root_file), "%home/special_file.t", &DummyFileLoader);
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    };

    {
        let file = db.resolve_path(
            Some(root_file),
            "%tmp/pre/made/some-temp-item",
            &DummyFileLoader,
        );
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    };

    {
        let file = db.resolve_path(Some(root_file), "%job/to/make/job-item", &DummyFileLoader);
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    };
}

#[test]
fn file_update() {
    const FILE_SOURCES: &[&str] = &[r#"var tee : int := 1"#, r#"var shoe : int := 2"#];

    let mut db = make_test_db();
    let file = db.insert_file("/main.t", FILE_SOURCES[0]);

    {
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    }

    db.update_file(file, Ok(LoadStatus::Modified(FILE_SOURCES[1].into())));
    {
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[1], None));
    }

    db.update_file(file, Err(LoadError::new("", toc_vfs::ErrorKind::NotFound)));
    {
        let res = db.file_source(file);
        assert_eq!(
            (res.0.as_str(), res.1.unwrap().kind()),
            ("", &toc_vfs::ErrorKind::NotFound)
        );
    }
}
