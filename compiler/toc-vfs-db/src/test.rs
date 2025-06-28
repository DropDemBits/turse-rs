use toc_paths::BuiltinPrefix;
use toc_vfs::{LoadError, LoadStatus};

use crate::{SourceTable, VfsBridge, VfsDbExt};

#[derive(Default, Clone)]
#[salsa::db]
struct VfsTestDB {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl salsa::Database for VfsTestDB {}

impl VfsTestDB {
    fn new() -> Self {
        Self::default()
    }
}

impl VfsBridge for VfsTestDB {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }
}

fn make_test_db() -> VfsTestDB {
    let db = VfsTestDB::new();

    // Setup the builtin prefixes to reasonable defaults
    let builtin_paths = vec![
        (BuiltinPrefix::Oot, "/oot/".into()),
        (BuiltinPrefix::Help, "/help/".into()),
        (BuiltinPrefix::UserHome, "/home/".into()),
        (BuiltinPrefix::Job, "/temp/".into()),
        (BuiltinPrefix::Temp, "/temp/".into()),
    ];

    toc_paths::PrefixExpansions::new(&db, builtin_paths.into_iter().collect());

    db
}

#[test]
fn file_retrieval() {
    const FILE_SOURCE: &str = r#"put "yee""#;

    let mut db = make_test_db();
    let root_file = db.insert_file("/src/main.t", FILE_SOURCE);

    let res = crate::source_of(&db, &root_file);

    assert_eq!(res.contents(&db), FILE_SOURCE);
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
        let res = crate::source_of(&db, &root_file);

        assert_eq!(res.contents(&db), FILE_SOURCES[0]);
    }

    // Lookup bar.t file source
    let bar_t = {
        let bar_t = crate::resolve_path(&db, &root_file, "foo/bar.t".into());
        let res = crate::source_of(&db, &bar_t);

        assert_eq!(res.contents(&db), FILE_SOURCES[1]);
        bar_t
    };

    // Lookup bap.t from bar.t
    {
        let bap_t = crate::resolve_path(&db, &bar_t, "bap.t".into());
        let res = crate::source_of(&db, &bap_t);

        assert_eq!(res.contents(&db), FILE_SOURCES[2]);
    };
}

#[test]
fn path_expansion() {
    use salsa::Setter;

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
        let builtin_paths = vec![
            (BuiltinPrefix::Oot, "/oot/".into()),
            (BuiltinPrefix::Help, "/oot/support/help".into()),
            (BuiltinPrefix::UserHome, "/home/".into()),
            (BuiltinPrefix::Job, "/job/".into()),
            (BuiltinPrefix::Temp, "/temp/".into()),
        ];

        toc_paths::PrefixExpansions::get(&db)
            .set_builtin_expansions(&mut db)
            .to(builtin_paths.into_iter().collect());

        root_file
    };

    // Lookup Predefs.lst file source
    let predefs_list = {
        let predefs_list = crate::resolve_path(&db, &root_file, "%oot/support/Predefs.lst".into());
        let res = crate::source_of(&db, &predefs_list);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[1], None)
        );
        predefs_list
    };

    // Lookup Net.tu from Predefs.lst
    {
        let net_tu = crate::resolve_path(&db, &predefs_list, "Net.tu".into());
        let res = crate::source_of(&db, &net_tu);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[2], None)
        );
    };

    {
        let file = crate::resolve_path(&db, &root_file, "%help/Keyword Lookup.txt".into());
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[3], None)
        );
    };

    {
        let file = crate::resolve_path(&db, &root_file, "%home/special_file.t".into());
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[0], None)
        );
    };

    {
        let file = crate::resolve_path(&db, &root_file, "%tmp/pre/made/some-temp-item".into());
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[0], None)
        );
    };

    {
        let file = crate::resolve_path(&db, &root_file, "%job/to/make/job-item".into());
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[0], None)
        );
    };
}

#[test]
fn file_update() {
    const FILE_SOURCES: &[&str] = &[r#"var tee : int := 1"#, r#"var shoe : int := 2"#];

    let mut db = make_test_db();
    let file = db.insert_file("/main.t", FILE_SOURCES[0]);

    {
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[0], None)
        );
    }

    db.update_file(&file, Ok(LoadStatus::Modified(FILE_SOURCES[1].into())));
    {
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db)),
            (FILE_SOURCES[1], None)
        );
    }

    db.update_file(&file, Err(LoadError::new("", toc_vfs::ErrorKind::NotFound)));
    {
        let res = crate::source_of(&db, &file);
        assert_eq!(
            (res.contents(&db).as_str(), res.errors(&db).unwrap().kind()),
            ("", &toc_vfs::ErrorKind::NotFound)
        );
    }
}
