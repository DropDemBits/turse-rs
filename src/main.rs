#![allow(dead_code)]
mod compiler;

fn main() {
    let mut scanner = compiler::scanner::Scanner::new("> >= => = :=");
    scanner.scan_tokens();

    println!("Tokens: {:#?}", &scanner.tokens);
}
