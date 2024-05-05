//! Core path entities
use camino::Utf8PathBuf;

/// Interned path to a source, but not anchored to any other path
#[salsa::interned]
pub struct RawPath {
    /// Path that may contain unexpanded percent prefixes
    #[return_ref]
    pub raw_path: Utf8PathBuf,
}

impl RawPath {
    /// Creates a a new dummy [`RawPath`], for use in test
    /// as a dummy handle to pass things through.
    ///
    /// Do not use this outside of tests, as crashes will
    /// happen if used improperly
    pub fn dummy(id: u32) -> Self {
        use salsa::{AsId, Id};
        Self::from_id(Id::from_u32(id))
    }

    // FIXME: Remove once we fully migrate to raw paths and the like
    // eg. printing the paths instead of the ids
    pub fn into_raw(self) -> u32 {
        self.0.as_u32()
    }
}
