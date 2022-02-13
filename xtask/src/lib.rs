mod codegen;
mod package;

use std::{
    env,
    path::{Path, PathBuf},
};

pub fn project_root() -> PathBuf {
    // from rust-analyzer's xtask lib.rs file
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

pub use codegen::do_codegen;
pub use package::do_package;
