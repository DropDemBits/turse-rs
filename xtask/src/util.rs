use std::{env, path::Path};

use camino::Utf8PathBuf;

pub fn project_root() -> camino::Utf8PathBuf {
    // from rust-analyzer's xtask lib.rs file
    Utf8PathBuf::from_path_buf(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(1)
            .unwrap()
            .to_path_buf(),
    )
    .unwrap()
}

pub fn normalize_newlines(s: &str) -> String {
    s.replace("\r\n", "\n")
}

pub fn is_ci() -> bool {
    std::env::var("CI").is_ok()
}
