mod lowering;

use std::path::{Path, PathBuf};
use std::{env, fs};

use anyhow::Result;

pub fn project_root() -> PathBuf {
    // from rust-analyzer's xtask lib.rs file
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

pub fn do_codegen() -> Result<()> {
    let grammar = project_root().join("compiler/toc_syntax/parsed_turing.ungram");
    let out_dir = project_root().join("compiler/toc_syntax/src/generated/");

    let grammar = fs::read_to_string(&grammar)?.parse()?;
    let lowered = lowering::lower_grammar(&grammar);

    println!("Used nodes");
    for nd in &lowered.node_data {
        println!("{:?}", nd);
    }

    println!("Used tokens");
    for tk in &lowered.token_data {
        println!("\t'{}'", tk);
    }

    Ok(())
}
