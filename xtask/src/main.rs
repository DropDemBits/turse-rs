use std::env;

fn main() {
    let mut args = env::args();
    let task = args.nth(1);

    let res = match task.as_deref() {
        Some("codegen") => xtask::do_codegen(),
        Some("pack-ext") => xtask::do_package(),
        _ => {
            show_help();
            Ok(())
        }
    };

    if let Err(e) = res {
        eprintln!("Encountered error: {}", e);
        std::process::exit(-1);
    }
}

fn show_help() {
    println!(
        r#"Available tasks:

codegen         Performs necessary code generation
pack-ext        Packages the extension"#
    );
}
