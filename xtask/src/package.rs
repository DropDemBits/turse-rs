//! Extension packaging

use std::fs;

use super::project_root;
use xshell::{cmd, pushd};

pub fn do_package() -> anyhow::Result<()> {
    package_server()?;
    package_client()?;

    Ok(())
}

fn package_server() -> anyhow::Result<()> {
    let manifest_path = project_root().join("Cargo.toml");

    // Build server binary
    cmd!("cargo build --release --bin lsp-server --manifest-path {manifest_path}").run()?;

    let binary = project_root()
        .join("target/release")
        .join(format!("lsp-server{}", binary_ext()));
    let dest_folder = project_root().join("lsp-client/vscode/server");
    let dest_file = dest_folder.join("turing-lsp-server");

    fs::create_dir_all(dest_folder)?;
    fs::copy(binary, dest_file)?;

    Ok(())
}

fn package_client() -> anyhow::Result<()> {
    let _dir = pushd("./lsp-client/vscode")?;

    cmd!("npm ci").run()?;
    cmd!("npx vsce package").run()?;

    Ok(())
}

fn binary_ext() -> &'static str {
    if cfg!(target_os = "windows") {
        ".exe"
    } else {
        ""
    }
}
