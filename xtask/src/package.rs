//! Extension packaging

use std::fs;

use crate::{flags, util::project_root};
use xshell::{cmd, Shell};

impl flags::Package {
    pub(crate) fn run(self, sh: &Shell) -> eyre::Result<()> {
        match self.subcommand {
            flags::PackageCmd::Lsp(cmd) => {
                if let Some(_) = cmd.server() {
                    package_server(sh)?;
                }

                if let Some(_) = cmd.client() {
                    package_client(sh)?;
                }

                Ok(())
            }
        }
    }
}

fn package_server(sh: &Shell) -> eyre::Result<()> {
    let manifest_path = project_root().join("Cargo.toml");

    // Build server binary
    cmd!(
        sh,
        "cargo build --release --bin toc-lsp-server --manifest-path {manifest_path}"
    )
    .run()?;

    let binary = project_root()
        .join("target/release")
        .join(format!("toc-lsp-server{}", binary_ext()));
    let dest_folder = project_root().join("lsp-client/vscode/server");
    let dest_file = dest_folder.join(format!("turing-lsp-server{}", binary_ext()));

    fs::create_dir_all(dest_folder)?;
    fs::copy(binary, dest_file)?;

    Ok(())
}

fn package_client(sh: &Shell) -> eyre::Result<()> {
    let _dir = sh.push_dir(project_root().join("lsp-client/vscode"));

    if cfg!(target_os = "windows") {
        cmd!(sh, "cmd /c npm ci").run()?;
        cmd!(sh, "cmd /c npx vsce package").run()?;
    } else {
        cmd!(sh, "npm ci").run()?;
        cmd!(sh, "npx vsce package").run()?;
    }

    Ok(())
}

fn binary_ext() -> &'static str {
    if cfg!(target_os = "windows") {
        ".exe"
    } else {
        ""
    }
}
