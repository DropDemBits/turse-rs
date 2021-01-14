//! AST validation
///! Checking if things hold up to stricter syntax semantics
// fancy quotes: ‘’
use rowan::TextRange;

use crate::ast;
use crate::SyntaxNode;

// Taken from rust-analyzer's syntax crate
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:ident) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

pub struct SyntaxError(pub String, pub TextRange);

pub struct ValidateResult {
    errors: Vec<SyntaxError>,
}

impl ValidateResult {
    pub fn push_error(&mut self, msg: &str, range: TextRange) {
        self.errors.push(SyntaxError(msg.to_string(), range))
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

pub fn validate_ast(root: SyntaxNode) -> ValidateResult {
    let mut result = ValidateResult { errors: vec![] };

    for node in root.descendants() {
        match_ast!(match node {
            ast::PreprocGlob(pp_glob) => validate_preproc_glob(pp_glob, &mut result),
            _ => (),
        })
    }

    result
}

fn validate_preproc_glob(glob: ast::PreprocGlob, res: &mut ValidateResult) {
    let thing = glob.contained().unwrap();
    match_ast!(match thing {
        ast::PPElseif(nd) => without_matching(nd.syntax(), res),
        ast::PPElse(nd) => without_matching(nd.syntax(), res),
        ast::PPEndIf(nd) => without_matching(nd.syntax(), res),
        _ => (),
    });

    fn without_matching(node: &SyntaxNode, res: &mut ValidateResult) {
        let first = node.first_token().unwrap();
        res.push_error(
            &format!("found ‘{}’ without matching ‘#if’", first.text()),
            first.text_range(),
        );
    }
}
