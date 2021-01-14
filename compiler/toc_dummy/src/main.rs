//! Dummy bin for running the new scanner and parser

use std::{env, fs, io};

use toc_syntax::validate;

fn load_contents(path: &str) -> io::Result<String> {
    let contents = fs::read(path)?;
    let contents = String::from_utf8_lossy(&contents).to_string();
    Ok(contents)
}

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = load_contents(&path).expect("Unable to load file");
    let parsed = toc_parser::parse(&contents);

    println!("Parsed output: {}", parsed.debug_tree());
    let validate_res = validate::validate_ast(parsed.syntax());
    for msg in validate_res.errors() {
        let (start, end): (usize, usize) = (msg.1.start().into(), msg.1.end().into());
        println!("error at {}..{}: {}", start, end, msg.0);
    }

    std::process::exit(if parsed.has_errors() { -1 } else { 0 });
}
