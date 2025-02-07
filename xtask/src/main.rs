mod codegen;
mod flags;
mod package;
mod util;

use miette::IntoDiagnostic;
use util::project_root;
use xshell::Shell;

fn main() -> miette::Result<()> {
    let flags = flags::Xtask::from_env_or_exit();

    let sh = &Shell::new().into_diagnostic()?;
    sh.change_dir(project_root());

    match flags.subcommand {
        flags::XtaskCmd::Codegen(cmd) => cmd.run(sh),
        flags::XtaskCmd::Package(cmd) => cmd.run(sh),
    }
}
