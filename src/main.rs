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
        Err(f) => {
            eprintln!("{}", f.to_string());
            show_usage(&program, &opts);
            return;
        }
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

    // Build the main unit
    let code_unit = compiler::frontend::block::CodeUnit::new(true);

    let mut scanner = compiler::frontend::scanner::Scanner::new(&file_contents);
    scanner.scan_tokens();

    let mut parser =
        compiler::frontend::parser::Parser::new(scanner.tokens, &file_contents, code_unit);
    parser.parse();

    // Take the unit back from the parser
    let mut code_unit = parser.take_unit();
    let mut type_table = code_unit.take_types();

    // By this point, all decls local to the unit have been resolved, and can be made available to other units which need it
    // TODO: Provide external type resolution stage

    // Validate AST
    let mut validator =
        compiler::frontend::validator::Validator::new(code_unit.root_block(), &mut type_table);
    code_unit.visit_ast_mut(&mut validator);
    code_unit.put_types(type_table);

    println!("Unit:\n{:#?}", code_unit);
}
