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
    let parsed = toc_parser::parse(&contents);

    println!("Parsed output: {}", parsed.debug_tree());
    std::process::exit(if parsed.has_errors() { -1 } else { 0 });
}
