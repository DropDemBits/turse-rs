//! Core compiler interface
extern crate toc_ast;
extern crate toc_core;
extern crate toc_frontend;
extern crate toc_ir;

use std::fs;
use std::{cell::RefCell, rc::Rc};
use toc_ast::block::CodeUnit;
use toc_frontend::{
    context::CompileContext, parser::Parser, scanner::Scanner, validator::Validator,
};

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

    let (unit, context) = compile_file(path, &file_contents);
    resolve_unit(unit, context);
}

/// Compiles a single file into a single code unit
pub fn compile_file(_path: &str, contents: &str) -> (CodeUnit, Rc<RefCell<CompileContext>>) {
    // Build the main unit
    let context = Rc::new(RefCell::new(CompileContext::new()));

    let scanner = Scanner::scan_source(contents, context.clone());
    let mut parser = Parser::new(scanner, contents, true, context.clone());

    parser.parse();

    // Take the parsed unit from the parser
    (parser.take_unit(), context)
}

/// Resolves the unit into the corresponding IR graph
pub fn resolve_unit(mut code_unit: CodeUnit, context: Rc<RefCell<CompileContext>>) {
    let type_table = code_unit.take_types();
    let unit_scope = code_unit.take_unit_scope();

    // TODO: Provide inter-unit type resolution stage
    // By this point, all decls local to the unit have been resolved, and can be made available to other units which need it

    // Validate AST
    let mut validator = Validator::new(unit_scope, type_table, context.clone());
    code_unit.visit_ast_mut(&mut validator);
    let (type_table, unit_scope) = validator.take_code_unit_parts();
    code_unit.put_types(type_table);
    code_unit.put_unit_scope(unit_scope);

    // Validator must run successfully
    if context.borrow().reporter.has_error() {
        return;
    }

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
