//! AST validation
//! Checking if things hold up to stricter syntax semantics
// fancy quotes: ‘’
#[cfg(test)]
mod test;

use toc_reporting::{MessageKind, MessageSink, ReportMessage, TextRange};
use toc_syntax::SyntaxNode;
use toc_syntax::{ast, SyntaxKind};

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

    if let Some(source) = ast::Source::cast(root) {
        validate_source(source, &mut ctx);
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

fn validate_source(src: ast::Source, ctx: &mut ValidateCtx) {
    let root = src.syntax();

    for node in root.descendants() {
        match_ast!(match node {
            ast::PreprocGlob(pp_glob) => validate_preproc_glob(pp_glob, ctx),
            ast::ElseStmt(stmt) => validate_else_stmt(stmt, ctx),
            ast::ElseifStmt(stmt) => validate_elseif_stmt(stmt, ctx),
            _ => (),
        })
    }
}

fn validate_preproc_glob(glob: ast::PreprocGlob, ctx: &mut ValidateCtx) {
    let thing = glob.contained().unwrap();
    match_ast!(match thing {
        ast::PPElseif(nd) => without_matching(nd.syntax(), "#if", ctx),
        ast::PPElse(nd) => without_matching(nd.syntax(), "#if", ctx),
        ast::PPEndIf(nd) => without_matching(nd.syntax(), "#if", ctx),
        _ => (),
    });
}

fn validate_else_stmt(stmt: ast::ElseStmt, ctx: &mut ValidateCtx) {
    let parent_kind = stmt.syntax().parent().map(|p| p.kind());

    if !matches!(parent_kind, Some(SyntaxKind::IfBody)) {
        without_matching(stmt.syntax(), "if", ctx);
    }
}

fn validate_elseif_stmt(stmt: ast::ElseifStmt, ctx: &mut ValidateCtx) {
    let parent_kind = stmt.syntax().parent().map(|p| p.kind());

    if !matches!(parent_kind, Some(SyntaxKind::IfBody)) {
        without_matching(stmt.syntax(), "if", ctx);
    }
}

fn without_matching(node: &SyntaxNode, thing: &str, ctx: &mut ValidateCtx) {
    let first = node.first_token().unwrap();
    ctx.push_error(
        &format!("found ‘{}’ without matching ‘{}’", first.text(), thing),
        first.text_range(),
    );
}

#[cfg(test)]
#[track_caller]
pub(crate) fn check(source: &str, expected: expect_test::Expect) {
    let res = toc_parser::parse(source);
    let validate_res = validate_ast(res.syntax());

    let mut buf = String::new();
    for msg in res.messages().iter().chain(validate_res.messages().iter()) {
        buf.push_str(&format!("{}\n", msg));
    }
    let trimmed = buf.trim_end();

    expected.assert_eq(trimmed);
}
