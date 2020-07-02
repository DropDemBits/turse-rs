#![allow(dead_code)]
mod compiler;

fn main() {
    let mut scanner =
        compiler::scanner::Scanner::new("tok mod rem 1 36#4ah519ASdgfd 1. 1.0 100.00e600");
    scanner.scan_tokens();

    println!("Tokens: {:#?}", &scanner.tokens);
}
