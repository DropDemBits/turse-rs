//! Dummy bin for running the new scanner and parser

use std::{env, fs, io};

fn load_contents(path: &str) -> io::Result<String> {
    let contents = fs::read(path)?;
    let contents = String::from_utf8_lossy(&contents).to_string();
    Ok(contents)
}

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = load_contents(&path).expect("Unable to load file");

    // Parse root CST
    let parsed = toc_parser::parse(&contents);
    // TODO: Gather dependencies from root CST, and parse them

    println!("Parsed output: {}", parsed.debug_tree());
    let validate_res = toc_validate::validate_ast(parsed.syntax());
    let hir_res = toc_hir_lowering::lower_ast(parsed.syntax());

    let msgs = parsed
        .messages()
        .iter()
        .chain(validate_res.messages().iter())
        .chain(hir_res.messages().iter());

    for msg in msgs {
        println!("{}", msg);
    }

    println!("{:#?}", hir_res.database);

    std::process::exit(if parsed.has_errors() { -1 } else { 0 });
}
