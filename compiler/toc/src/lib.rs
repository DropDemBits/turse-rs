//! Core compiler interface
extern crate toc_ast;
extern crate toc_core;
extern crate toc_frontend;
extern crate toc_ir;

use toc_ast::unit::CodeUnit;
use toc_frontend::context::{CompileSession, CompileStatus};

fn dump_info(unit: &CodeUnit, dump_out: &[String]) {
    if dump_out.iter().any(|elem| elem == "ast") {
        // Pretty-print AST
        println!("ast: {}", &unit.root_stmt);
    }

    if dump_out.iter().any(|elem| elem == "scope") {
        // Pretty-print unit scope
        println!("scope: {}", &unit.unit_scope);
    }

    if dump_out.iter().any(|elem| elem == "types") {
        // Pretty-print types
        println!("types: {}", &unit.type_table);
    }
}

/// Compiles the given file
///
/// # Returns
/// Returns whether compilation was successful or not
pub fn compile_file(
    path: &str,
    dump_out: Vec<String>,
    mute_warnings: bool,
    only_parser: bool,
) -> bool {
    let mut session = CompileSession::new();
    let (main_unit, status) = session.compile_source_file(path, only_parser, mute_warnings);

    // Dump info for the main unit (skip over empty file)
    if let Some(main_unit) = main_unit {
        if let Some((_, unit)) = session.units().find(|(id, _)| **id == main_unit) {
            dump_info(&unit, &dump_out);
        }
    }

    status == CompileStatus::Success
}
