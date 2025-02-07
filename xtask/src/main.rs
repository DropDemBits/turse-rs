mod codegen;
mod flags;
mod package;
mod util;

use util::project_root;
use xshell::Shell;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let flags = flags::Xtask::from_env_or_exit();

    let sh = &Shell::new()?;
    sh.change_dir(project_root());

    match flags.subcommand {
        flags::XtaskCmd::Codegen(cmd) => cmd.run(sh),
        flags::XtaskCmd::Package(cmd) => cmd.run(sh),
    }
}
