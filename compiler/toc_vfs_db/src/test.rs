use std::ops::Deref;

use toc_salsa::salsa;
use toc_vfs::{impl_has_vfs, BuiltinPrefix, LoadError, LoadStatus, Vfs};

use crate::db::{FileSystem, FileSystemStorage, VfsDatabaseExt};

#[salsa::database(FileSystemStorage)]
struct VfsTestDB {
    storage: salsa::Storage<Self>,
    vfs: Vfs,
}

impl salsa::Database for VfsTestDB {}

impl_has_vfs!(VfsTestDB, vfs);

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
        let bar_t = db
            .vfs
            .resolve_path(Some(root_file), "foo/bar.t")
            .into_file_id();
        let res = db.file_source(bar_t);
        let (source, _load_error) = res;

        assert_eq!(source.deref(), FILE_SOURCES[1]);
        bar_t
    };

    // Lookup bap.t from bar.t
    {
        let bap_t = db.vfs.resolve_path(Some(bar_t), "bap.t").into_file_id();
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
        let vfs = &mut db.vfs;
        vfs.set_prefix_expansion(BuiltinPrefix::Oot, "/oot/");
        vfs.set_prefix_expansion(BuiltinPrefix::Help, "/oot/support/help");
        vfs.set_prefix_expansion(BuiltinPrefix::UserHome, "/home/");
        vfs.set_prefix_expansion(BuiltinPrefix::Job, "/job/");
        vfs.set_prefix_expansion(BuiltinPrefix::Temp, "/temp/");

        root_file
    };

    // Lookup Predefs.lst file source
    let predefs_list = {
        let predefs_list = db
            .vfs
            .resolve_path(Some(root_file), "%oot/support/Predefs.lst")
            .into_file_id();
        let res = db.file_source(predefs_list);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[1], None));
        predefs_list
    };

    // Lookup Net.tu from Predefs.lst
    {
        let net_tu = db
            .vfs
            .resolve_path(Some(predefs_list), "Net.tu")
            .into_file_id();
        let res = db.file_source(net_tu);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[2], None));
    };

    {
        let file = db
            .vfs
            .resolve_path(Some(root_file), "%help/Keyword Lookup.txt")
            .into_file_id();
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[3], None));
    };

    {
        let file = db
            .vfs
            .resolve_path(Some(root_file), "%home/special_file.t")
            .into_file_id();
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    };

    {
        let file = db
            .vfs
            .resolve_path(Some(root_file), "%tmp/pre/made/some-temp-item")
            .into_file_id();
        let res = db.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    };

    {
        let file = db
            .vfs
            .resolve_path(Some(root_file), "%job/to/make/job-item")
            .into_file_id();
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
