//! Core path entities
use camino::Utf8PathBuf;

pub type RawOwnedPath = camino::Utf8PathBuf;
pub type RawRefPath = camino::Utf8Path;

/// Interned path to a source, but not anchored to any other path
#[salsa::interned(debug, no_lifetime)]
pub struct RawPath {
    /// Path that may contain unexpanded percent prefixes
    #[return_ref]
    pub raw_path: Utf8PathBuf,
}

impl RawPath {
    // FIXME: Remove once we fully migrate to raw paths and the like
    // eg. printing the paths instead of the ids
    pub fn into_raw(self) -> u32 {
        self.0.as_u32()
    }
}
