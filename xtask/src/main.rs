use std::env;

fn main() {
    let mut args = env::args();
    let task = args.nth(1);

    let res = match task.as_ref().map(|s| s.as_str()) {
        Some("codegen") => xtask::do_codegen(),
        _ => show_help(),
    };

    if let Err(e) = res {
        eprintln!("Encountered error: {}", e);
        std::process::exit(-1);
    }
}

fn show_help() -> anyhow::Result<()> {
    println!(
        r#"Available tasks:

codegen         Performs necessary code generation"#
    );

    Ok(())
}
