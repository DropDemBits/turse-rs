use std::{fs, path::Path};

use miette::{IntoDiagnostic, WrapErr};
use xshell::{cmd, Shell};

use crate::{
    flags,
    util::{normalize_newlines, project_root},
};

mod syntax;

impl flags::Codegen {
    pub(crate) fn run(self, _sh: &Shell) -> miette::Result<()> {
        let check = self.check;

        match self.codegen_type.unwrap_or_default() {
            flags::CodegenType::All => {
                syntax::do_codegen(check)?;

                Ok(())
            }
            flags::CodegenType::Grammar => syntax::do_codegen(check),
            flags::CodegenType::TuringBytecode => {
                todo!()
            }
        }
    }
}

// from rust-analyzer xtask and test-utils crates

fn reformat(text: String) -> miette::Result<String> {
    let sh = Shell::new()
        .into_diagnostic()
        .wrap_err("while creating rustfmt shell")?;
    let rustfmt_toml = project_root().join("rustfmt.toml");

    // check for rustfmt
    let mut text = cmd!(
        sh,
        "rustup run stable rustfmt --config-path {rustfmt_toml} --config fn_single_line=true"
    )
    .stdin(text)
    .read()
    .into_diagnostic()
    .wrap_err("while formatting output")?;

    if !text.ends_with('\n') {
        text.push('\n');
    }

    Ok(text)
}

fn add_preamble(gen_ty: flags::CodegenType, mut text: String) -> String {
    let header = format!("//! Generated by `xtask codegen {gen_ty}`, do not edit by hand\n\n");
    text.insert_str(0, &header);
    text
}

/// Checks that the `file` has the specified `contents`. If that is not the
/// case, updates the file and then fails the test.
#[allow(clippy::print_stderr)]
fn ensure_file_contents(cg: flags::CodegenType, file: &Path, contents: &str, check: bool) -> bool {
    let contents = normalize_newlines(contents);
    if let Ok(old_contents) = fs::read_to_string(file) {
        if normalize_newlines(&old_contents) == contents {
            // File is already up to date.
            return false;
        }
    }

    let display_path = file.strip_prefix(project_root()).unwrap_or(file);
    if check {
        panic!(
            "{} was not up-to-date{}",
            file.display(),
            if std::env::var("CI").is_ok() {
                format!(
                    "\n    NOTE: run `cargo xtask codegen {cg}` locally and commit the updated files\n"
                )
            } else {
                "".to_owned()
            }
        );
    } else {
        eprintln!(
            "\n\x1b[31;1merror\x1b[0m: {} was not up-to-date, updating\n",
            display_path.display()
        );

        if let Some(parent) = file.parent() {
            let _ = fs::create_dir_all(parent);
        }
        fs::write(file, contents).unwrap();
        true
    }
}
