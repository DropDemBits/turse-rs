//! Test fixture generation

use toc_span::FileId;

use crate::{HasVfs, LoadResult};
use crate::{LoadError, LoadStatus};

pub const FILE_DELIMITER_START: &str = "%%-";

/// Generates a VFS tree from the given text source, producing a set of [`FixtureFiles`]
///
/// There are two kinds of text sources:
///
/// - Single file, implied to be in the `src/main.t` file
/// - Multiple files, where each file source is specified
///
/// Files within a multi-file text source are delimited using the `%%-` characters.
/// All file names must be specified in a multi-file source, as there is no implied
/// `src/main.t` (it must be specified explicitly).
///
/// Specifying `removed` before the file path indicates that no file source should be added.
///
/// # Examples
///
/// Single file source:
///
/// ```rust,compile_fail
/// generate_vfs(db, r#"i := "i am in a single file""#);
/// ```
///
/// Multiple file source:
///
/// ```rust,compile_fail
/// generate_vfs(db, r#"
/// %%- /src/main.t
/// put "i am in one file"
/// %%- /src/test.t
/// put "and i am in another"
/// "#);
/// ```
#[must_use = "fixture files are not inserted into the given database (use `VfsDatabaseExt::insert_fixture`)"]
pub fn generate_vfs<DB: HasVfs>(db: &mut DB, source: &str) -> FixtureFiles {
    // ???: How we deal with inserting path resolutions
    // We just do it here for direct paths, let source graph deal with the others

    let mut fixture = FixtureFiles { files: vec![] };

    // Segment into lines
    let lines: Vec<_> = source.split('\n').collect();

    // Segment by starts, treat as endings
    let mut file_ends = lines
        .iter()
        .enumerate()
        .filter_map(|(idx, line)| {
            // Skip leading ws
            let line = line.trim_start();
            Some(idx).filter(|_| line.starts_with(FILE_DELIMITER_START))
        })
        .chain(Some(lines.len()));

    let mut file_start = 0;
    let mut file_end = 0;
    let files: Vec<_> = std::iter::from_fn({
        || {
            file_start = file_end;
            file_end = file_ends.next()?;
            Some(&lines[file_start..file_end])
        }
    })
    .skip_while(|file| {
        // skip leading whitespace
        file.iter().all(|line| line.trim_start().is_empty())
    })
    .collect();

    // Ensure that there's no text before the first line
    let is_single_file = if let Some(file) = files.first() {
        let first_line = file.iter().find(|line| !line.trim_start().is_empty());

        if let Some(first_line) = first_line {
            if first_line.trim_start().starts_with(FILE_DELIMITER_START) {
                // starts with the delimiter, proceed as normal
                false
            } else if files.len() > 1 {
                // is multi-file, but with leading text present
                // make sure that the leading text is just whitespace
                panic!("text present before first file (`src/main.t` must be explicitly defined)");
            } else {
                // is just a single file
                true
            }
        } else {
            // is empty, or a single file, in other words
            true
        }
    } else {
        // Guaranteed to be a single file source, since it's empty
        true
    };

    if is_single_file {
        let main = db.get_vfs_mut().intern_path("src/main.t".into());
        fixture
            .files
            .push((main, Ok(LoadStatus::Modified(source.into()))));
        return fixture;
    }

    // Build up the files
    for text in files {
        let (config, source) = text.split_first().expect("missing file info args");
        let (is_removed, path) = {
            let mut config = config
                .trim_start()
                .strip_prefix(FILE_DELIMITER_START)
                .expect("missing start delimiter");
            let is_removed = if let Some(rest) = config.trim_start().strip_prefix("removed") {
                if rest.starts_with('/') {
                    // part of a path, don't take it
                    false
                } else {
                    // is removed, eat it!
                    config = rest;
                    true
                }
            } else {
                false
            };
            let path = config.trim_start().trim_end();

            (is_removed, path)
        };

        let file = db.get_vfs_mut().intern_path(path.into());

        let source = if is_removed {
            Err(LoadError::NotFound)
        } else {
            // Rebuild the source!
            let mut source_lines = source.iter();
            let mut source = String::new();

            // this is essentially equivalent to itertools::intersperse
            source.push_str(source_lines.next().unwrap_or(&""));
            for line in source_lines {
                source.push('\n');
                source.push_str(line);
            }

            Ok(LoadStatus::Modified(source.into()))
        };

        fixture.files.push((file, source));
    }

    fixture
}

/// File tree generated by [`generate_vfs`]
#[derive(Debug)]
pub struct FixtureFiles {
    pub files: Vec<(FileId, LoadResult)>,
}

impl FixtureFiles {
    pub fn file_source(&self, file: FileId) -> (String, Option<LoadError>) {
        self.files
            .iter()
            .find(|info| info.0 == file)
            .map(|(_, res)| {
                res.as_ref()
                    .map(|sts| match sts {
                        LoadStatus::Unchanged => unreachable!(),
                        LoadStatus::Modified(src) => {
                            (String::from_utf8_lossy(src).to_string(), None)
                        }
                    })
                    .unwrap_or_else(|err| (String::new(), Some(err.clone())))
            })
            .unwrap()
    }
}

#[cfg(test)]
mod test {
    use crate::{impl_has_vfs, Vfs};

    use super::*;

    #[derive(Default)]
    struct VfsTestDB {
        vfs: Vfs,
    }

    impl_has_vfs!(VfsTestDB, vfs);

    #[test]
    fn single_file() {
        let mut db = VfsTestDB::default();
        let srcs = generate_vfs(&mut db, "single file, yay");

        let root_file = db.vfs.resolve_path(None, "src/main.t").into_file_id();
        let source = srcs.file_source(root_file).0;
        assert_eq!(source.as_str(), "single file, yay");
    }

    #[test]
    fn single_file_empty() {
        let mut db = VfsTestDB::default();
        let srcs = generate_vfs(&mut db, "");

        let root_file = db.vfs.resolve_path(None, "src/main.t").into_file_id();
        let res = srcs.file_source(root_file);
        assert_eq!((res.0.as_str(), res.1), ("", None));
    }

    #[test]
    fn single_file_many_lines() {
        let mut db = VfsTestDB::default();
        let srcs = generate_vfs(&mut db, "single file\nbut multiple lines");

        let root_file = db.vfs.resolve_path(None, "src/main.t").into_file_id();
        let source = srcs.file_source(root_file).0;
        assert_eq!(source.as_str(), "single file\nbut multiple lines");
    }

    #[test]
    fn multifile() {
        const FILE_SOURCES: &[&str] = &[
            "file 0\ni have lines\nbut that's it",
            "file 1\ni also have lines, and a trailing nl\n",
            "file 2\ni also have a trailing nl at the end of the list\n",
        ];
        let mut db = VfsTestDB::default();

        // Leading whitespace is okay
        let srcs = generate_vfs(
            &mut db,
            &format!(
                "     \n    %%- src/main.t\n{0}\n%%- src/file1.t\n{1}\n%%- removed removed/ya.t\n%%- src/file2.t\n{2}",
                FILE_SOURCES[0], FILE_SOURCES[1], FILE_SOURCES[2]
            ),
        );

        let file = db.vfs.resolve_path(None, "src/main.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), FILE_SOURCES[0]);
        let file = db.vfs.resolve_path(None, "src/file1.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), FILE_SOURCES[1]);
        let file = db.vfs.resolve_path(None, "src/file2.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), FILE_SOURCES[2]);
        let file = db.vfs.resolve_path(None, "removed/ya.t").into_file_id();
        let res = srcs.file_source(file);
        assert_eq!(
            (res.0.as_str(), res.1),
            ("", Some(crate::LoadError::NotFound))
        )
    }

    #[test]
    fn multifile_one_file() {
        const FILE_SOURCES: &[&str] = &["singular_file"];
        let mut db = VfsTestDB::default();
        let srcs = generate_vfs(&mut db, &format!("%%- one/file.t\n{0}", FILE_SOURCES[0]));

        let file = db.vfs.resolve_path(None, "one/file.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), FILE_SOURCES[0]);
    }

    #[test]
    fn multifile_removed_as_path() {
        const FILE_SOURCES: &[&str] = &["not actually empty"];
        let mut db = VfsTestDB::default();
        let srcs = generate_vfs(&mut db, &format!("%%- removed/ya.t\n{0}", FILE_SOURCES[0]));

        let file = db.vfs.resolve_path(None, "removed/ya.t").into_file_id();
        let res = srcs.file_source(file);
        assert_eq!((res.0.as_str(), res.1), (FILE_SOURCES[0], None));
    }

    #[test]
    fn multifile_empty_files() {
        let mut db = VfsTestDB::default();

        let srcs = generate_vfs(
            &mut db,
            &format!("%%- empty/file0.t\n%%- empty/file1.t\n%%- empty/file2.t"),
        );

        let file = db.vfs.resolve_path(None, "empty/file0.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), "");
        let file = db.vfs.resolve_path(None, "empty/file1.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), "");
        let file = db.vfs.resolve_path(None, "empty/file2.t").into_file_id();
        assert_eq!(srcs.file_source(file).0.as_str(), "");
    }

    #[test]
    #[should_panic = "text present before first file (`src/main.t` must be explicitly defined)"]
    fn multifile_with_leading_text() {
        let mut db = VfsTestDB::default();
        let _ = generate_vfs(&mut db, "        some text\n%%- src/a_file.t");
    }
}
