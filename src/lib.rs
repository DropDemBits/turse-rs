mod compiler;
mod status_reporter;

use std::fs;

/// Compiles and runs the given file
pub fn compile_run_file(path: &str) {
    // Load file & exec
    let file_contents = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read in file: {}", e.to_string());
            return;
        }
    };

    compile_file(path, &file_contents);
}

/// Compiles a single file
pub fn compile_file(_path: &str, contents: &str) {
    // Build the main unit
    let code_unit = compiler::block::CodeUnit::new(true);

    let mut scanner = compiler::frontend::scanner::Scanner::new(contents);
    scanner.scan_tokens();

    let mut parser =
        compiler::frontend::parser::Parser::new(scanner.tokens, contents, code_unit);
    parser.parse();

    // Take the unit back from the parser
    let mut code_unit = parser.take_unit();
    let type_table = code_unit.take_types();

    // By this point, all decls local to the unit have been resolved, and can be made available to other units which need it
    // TODO: Provide external type resolution stage

    // Validate AST
    let mut validator =
        compiler::frontend::validator::Validator::new(code_unit.root_block(), type_table);
    code_unit.visit_ast_mut(&mut validator);
    code_unit.put_types(validator.take_types());

    // Validator must run successfully
    if validator.reporter.has_error() {
        return;
    }

    // Code Unit
    println!("Parsed Code Unit:\n{:#?}\n", code_unit);

    // Generate IR for the given unit
    /*let ir_builder = compiler::ir::IrBuilder::new(code_unit);
    let ir = ir_builder.generate_ir().unwrap();

    println!("Generated IR:\n{:#?}\n", ir);

    let main_func = ir.funcs.get("<init>").unwrap().entry_block;
    let edge_iter = ir.blocks.edges(main_func);

    for edge in edge_iter {
        println!("ed {:?}", edge);
    }*/
}

