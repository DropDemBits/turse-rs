use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::{env, fs};

use anyhow::Result;
use ungrammar::Grammar;

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

/*
Structure:
Node
children: [
    Node(node_id)
    Token(token_id)
    List(NodeOrToken)
]
Group
[
    Alternate: [
        name: str
        to: NodeOrToken
    ]
    ...
]
*/

pub fn do_codegen() -> Result<()> {
    let grammar = project_root().join("compiler/toc_syntax/parsed_turing.ungram");
    let out_dir = project_root().join("compiler/toc_syntax/src/generated/");

    let grammar: Grammar = fs::read_to_string(&grammar)?.parse()?;

    let mut used_tokens = BTreeSet::new();

    for node in grammar.iter() {
        let dat = &grammar[node];
        match dat.rule {
            ungrammar::Rule::Labeled { label: _, rule: _ } => {}
            ungrammar::Rule::Node(_) => {}
            ungrammar::Rule::Token(token) => {
                let tk_dat = &grammar[token];
                used_tokens.insert(tk_dat.name.as_str());
            }
            ungrammar::Rule::Seq(_) => {}
            ungrammar::Rule::Alt(ref r) => {
                use ungrammar::Rule;

                #[derive(Debug, PartialEq)]
                enum Alternates {
                    Unknown,
                    Node,
                    Token,
                    Complicated,
                }

                let mut alterante_over = Alternates::Unknown;
                for rule in r {
                    let transition_to = match rule {
                        Rule::Labeled { rule, .. } => match &**rule {
                            Rule::Node(_) => Alternates::Node,
                            Rule::Token(_) => Alternates::Token,
                            _ => Alternates::Complicated,
                        },
                        Rule::Node(_) => Alternates::Node,
                        Rule::Token(_) => Alternates::Token,
                        _ => Alternates::Complicated,
                    };

                    alterante_over = match alterante_over {
                        Alternates::Token if transition_to == Alternates::Node => {
                            Alternates::Complicated
                        }
                        Alternates::Node if transition_to == Alternates::Token => {
                            Alternates::Complicated
                        }
                        Alternates::Complicated => Alternates::Complicated,
                        _ => transition_to,
                    };
                }
                assert_ne!(alterante_over, Alternates::Unknown);
                println!("{:?} {:?}", alterante_over, dat.name);
            }
            ungrammar::Rule::Opt(_) => {}
            ungrammar::Rule::Rep(_) => {}
        }
    }

    println!("Used tokens:");
    for tk in used_tokens {
        println!("\t'{}'", tk);
    }

    Ok(())
}
