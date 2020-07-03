mod compiler;
mod status_reporter;

extern crate getopts;
use getopts::Options;
use std::env;
use std::fs;

fn show_usage(program_name: &String, opts: &Options) {
    let brief = format!("Usage: {} [options]", program_name);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.long_only(true);
    opts.optopt(
        "",
        "file",
        "Run a compiled Turing program from a bytecode file",
        "FILE_PATH",
    );
    opts.optopt(
        "",
        "run",
        "Compile and run a Turing program from a source file",
        "FILE_PATH",
    );
    opts.optflag("", "help", "Shows this help message");
    opts.optflag("", "repl", "Launches the REPL interpreter");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    if matches.opt_present("help") {
        // Show the help
        show_usage(&program, &opts);
        return;
    }

    if matches.opt_present("repl") {
        println!("Launching REPL interpreter (Not Supported Yet)");
    } else if let Some(source_path) = matches.opt_str("run") {
        compile_run_file(&source_path);
    } else if let Some(bytecode_path) = matches.opt_str("file") {
        println!(
            "Launching program from {} (Not Supported Yet)",
            bytecode_path
        );
    } else if matches.free.is_empty() {
        println!("Running from embedded file (Not Supported Yet)");
    } else {
        show_usage(&program, &opts);
    }
}

/// Compiles and runs the given file
fn compile_run_file(path: &str) {
    // Load file & exec
    let file_contents = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read in file: {}", e.to_string());
            return;
        }
    };

    let mut scanner = compiler::scanner::Scanner::new(&file_contents);
    scanner.scan_tokens();

    if scanner.is_valid_scan() {
        //println!("Tokens: {:#?}", &scanner.tokens);
        for token in scanner.tokens {
            let locate = token.location;
            let tok_width = locate.get_length(&file_contents);

            println!(
                "{}:{}-{} in {} {:?}",
                locate.line,
                locate.column,
                locate.column + tok_width,
                path,
                token.token_type
            );
        }
    } else {
        eprintln!("Error occurred during scanning");
    }
}
