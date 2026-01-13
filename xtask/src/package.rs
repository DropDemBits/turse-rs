//! Extension packaging

use std::fs;

use crate::{flags, util::project_root};
use miette::IntoDiagnostic;
use xshell::{Shell, cmd};

impl flags::Package {
    pub(crate) fn run(self, sh: &Shell) -> miette::Result<()> {
        match self.subcommand {
            flags::PackageCmd::Lsp(cmd) => {
                if cmd.server().is_some() {
                    package_server(sh)?;
                }

                if cmd.client().is_some() {
                    package_client(sh)?;
                }

                Ok(())
            }
        }
    }
}

fn package_server(sh: &Shell) -> miette::Result<()> {
    let manifest_path = project_root().join("Cargo.toml");

    // Build server binary
    cmd!(
        sh,
        "cargo build --release --bin toc-lsp-server --manifest-path {manifest_path}"
    )
    .run()
    .into_diagnostic()?;

    let binary = project_root()
        .join("target/release")
        .join(format!("toc-lsp-server{}", binary_ext()));
    let dest_folder = project_root().join("lsp-client/vscode/server");
    let dest_file = dest_folder.join(format!("turing-lsp-server{}", binary_ext()));

    fs::create_dir_all(dest_folder).into_diagnostic()?;
    fs::copy(binary, dest_file).into_diagnostic()?;

    Ok(())
}

fn package_client(sh: &Shell) -> miette::Result<()> {
    let _dir = sh.push_dir(project_root().join("lsp-client/vscode"));

    if cfg!(target_os = "windows") {
        cmd!(sh, "cmd /c npm ci").run().into_diagnostic()?;
        cmd!(sh, "cmd /c npx vsce package")
            .run()
            .into_diagnostic()?;
    } else {
        cmd!(sh, "npm ci").run().into_diagnostic()?;
        cmd!(sh, "npx vsce package").run().into_diagnostic()?;
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
