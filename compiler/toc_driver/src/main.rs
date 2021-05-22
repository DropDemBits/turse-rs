//! Dummy bin for running the new scanner and parser

use std::{env, fs, io, sync::Arc};

fn load_contents(path: &str) -> io::Result<String> {
    let contents = fs::read(path)?;
    let contents = String::from_utf8_lossy(&contents).to_string();
    Ok(contents)
}

fn main() {
    let path: String = env::args().nth(1).expect("Missing path to source file");
    let contents = load_contents(&path).expect("Unable to load file");

    // Parse root CST
    let parsed = toc_parser::parse(&contents);
    let dependencies = toc_driver::gather_dependencies(parsed.syntax());
    // TODO: Gather dependencies from root CST, and parse them
    // Requires a FileDB to keep track of files, as well as some variation of a VFS
    // to resolve (relative) paths to files

    println!("Parsed output: {}", parsed.dump_tree());
    let validate_res = toc_validate::validate_ast(parsed.syntax());
    println!("Dependencies: {:#?}", dependencies);

    let mut unit_map = toc_hir::UnitMapBuilder::new();
    let hir_res = toc_hir_lowering::lower_ast(parsed.syntax(), &mut unit_map);

    let unit_map = Arc::new(unit_map.finish());
    let root_unit = unit_map.get_unit(hir_res.id);
    println!("{:#?}", root_unit);

    let analyze_res = toc_analysis::analyze_unit(hir_res.id, unit_map);

    let msgs = parsed
        .messages()
        .iter()
        .chain(validate_res.messages().iter())
        .chain(hir_res.messages().iter())
        .chain(analyze_res.messages().iter());

    let mut has_errors = false;

    for msg in msgs {
        has_errors |= matches!(msg.kind(), toc_reporting::MessageKind::Error);
        println!("{}", msg);
    }

    std::process::exit(if has_errors { -1 } else { 0 });
}
