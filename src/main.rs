mod compiler;
mod status_reporter;

fn main() {
    let mut scanner =
        compiler::scanner::Scanner::new("tok mod rem 1 36#4ah519ASdgfd 1. 1.0 100.00e200");
    scanner.scan_tokens();

    if scanner.is_valid_scan() {
        println!("Tokens: {:#?}", &scanner.tokens);
    } else {
        eprintln!("Error occurred during scanning");
    }
}
