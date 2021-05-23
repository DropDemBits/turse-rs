//! Abstraction over interfacing the native filesystem

use std::num::NonZeroU32;
use std::{
    convert::TryFrom,
    sync::{Arc, RwLock},
};

use toc_span::FileId;

/// Information stored about a file
pub struct FileInfo {
    /// The full path to the file
    pub path: String,
    /// The source text for the file
    pub source: String,
}

/// File database, holding a mapping between `FileId`s and `FileInfo`s
pub struct FileDb {
    files: RwLock<Vec<Arc<FileInfo>>>,
}

impl FileDb {
    pub fn new() -> Self {
        Self {
            files: RwLock::new(Vec::new()),
        }
    }

    pub fn add_file(&self, path: &str, source: &str) -> FileId {
        // TODO: Dedup paths
        let mut files = self.files.write().unwrap();

        let id = u32::try_from(files.len() + 1).expect("Too many file ids");
        let id = NonZeroU32::new(id).unwrap();
        let id = FileId::new(id);
        files.push(Arc::new(FileInfo {
            path: path.to_owned(),
            source: source.to_owned(),
        }));

        id
    }

    pub fn get_file(&self, id: FileId) -> Arc<FileInfo> {
        self.files.read().unwrap()[(id.raw_id().get() - 1) as usize].clone()
    }

    pub fn files(&self) -> impl Iterator<Item = FileId> {
        (0..self.files.read().unwrap().len()).map(|i| {
            let id = u32::try_from(i + 1).ok();
            let id = id.and_then(NonZeroU32::new).unwrap();
            FileId::new(id)
        })
    }
}

impl Default for FileDb {
    fn default() -> Self {
        Self::new()
    }
}

// ???: Where do we map paths?
// - Need to be able to lookup paths from special root paths
//   - `%oot`: Root Turing path, has sources for all of the predef modules
//   - `%home`: User home path
//   - `%job`: Path with an unknown purpose (in current Turing editor, this is not set)
//   - `%help`: Alias of "%oot/help"
//   - `%tmp`: Generated temporary directory

#[test]
fn test_mut_across_file_add() {
    let db = FileDb::new();
    let first_file = db.add_file("some/path/to/there", "the_raw_text_source");
    let first_text = &db.get_file(first_file).source;
    let _second_file = db.add_file("some/path/to/elsewhere", "other_source");
    let _later_use = first_text;
}
