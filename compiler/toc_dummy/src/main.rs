//! Dummy bin for running the new scanner and parser

use std::{env, fs};

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = fs::read_to_string(path).expect("Unable to load file");
    let parsed = toc_parser::parse(&contents);

    println!("Parsed output: {}", parsed.debug_tree());
}
