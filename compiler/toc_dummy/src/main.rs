//! Dummy bin for running the new scanner and parser

use std::{env, fs};
use toc_syntax::ast::*;

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = fs::read_to_string(path).expect("Unable to load file");
    let parsed = toc_parser::parse(&contents);

    println!("Parsed output: {}", parsed.debug_tree());

    let source = Source::cast(parsed.syntax()).unwrap();

    for stmt in source.stmts() {
        let range = stmt.syntax().text_range();
        let slice = &contents[range.start().into()..range.end().into()];

        match stmt {
            Stmt::ConstVarDecl(_) => {
                print!("cv_decl [{}]", slice);
            }
            Stmt::TypeDecl(_) => {
                print!("cv_decl [{}]", slice);
            }
            Stmt::BlockStmt(_) => {
                print!("block_stmt [{}]", slice);
            }
            _ => {}
        }
    }
}
