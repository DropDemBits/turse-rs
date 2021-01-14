//! AST validation
///! Checking if things hold up to stricter syntax semantics
// fancy quotes: ‘’
use rowan::TextRange;
use toc_reporting::{MessageKind, MessageSink, ReportMessage};

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

pub struct ValidateResult {
    messages: Vec<ReportMessage>,
}

impl ValidateResult {
    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }
}

pub fn validate_ast(root: SyntaxNode) -> ValidateResult {
    let mut ctx = ValidateCtx {
        sink: MessageSink::new(),
    };

    for node in root.descendants() {
        match_ast!(match node {
            ast::PreprocGlob(pp_glob) => validate_preproc_glob(pp_glob, &mut ctx),
            _ => (),
        })
    }

    ctx.finish()
}
struct ValidateCtx {
    sink: MessageSink,
}

impl ValidateCtx {
    pub(crate) fn push_error(&mut self, msg: &str, range: TextRange) {
        self.sink.report(MessageKind::Error, msg, range);
    }

    fn finish(self) -> ValidateResult {
        ValidateResult {
            messages: self.sink.finish(),
        }
    }
}

fn validate_preproc_glob(glob: ast::PreprocGlob, res: &mut ValidateCtx) {
    let thing = glob.contained().unwrap();
    match_ast!(match thing {
        ast::PPElseif(nd) => without_matching(nd.syntax(), res),
        ast::PPElse(nd) => without_matching(nd.syntax(), res),
        ast::PPEndIf(nd) => without_matching(nd.syntax(), res),
        _ => (),
    });

    fn without_matching(node: &SyntaxNode, res: &mut ValidateCtx) {
        let first = node.first_token().unwrap();
        res.push_error(
            &format!("found ‘{}’ without matching ‘#if’", first.text()),
            first.text_range(),
        );
    }
}
