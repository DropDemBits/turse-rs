//! Path manipulation and interning

use camino::Utf8PathBuf;

#[salsa::jar(db = Db)]
pub struct Jar(RawPath);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

/// Interned path to a source, but not anchored to any other path
#[salsa::interned]
pub struct RawPath {
    #[return_ref]
    pub path: Utf8PathBuf,
}
