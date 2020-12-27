//! Parser for building the initial Concrete Syntax Tree
mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use source::Source;
use toc_scanner::Scanner;
use toc_syntax::SyntaxNode;

use rowan::GreenNode;

use crate::sink::Sink;

pub fn parse(source: &str) -> ParseResult {
    let tokens: Vec<_> = Scanner::new(source).collect();
    let source = Source::new(&tokens);
    let parser = parser::Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    ParseResult {
        node: sink.finish(),
    }
}

pub struct ParseResult {
    node: GreenNode,
}

impl ParseResult {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }
}

#[cfg(test)]
#[track_caller]
pub(crate) fn check(source: &str, expected: expect_test::Expect) {
    let res = parse(source);
    let syntax = res.syntax();
    let parsed_tree = format!("{:#?}", syntax);

    // chop off newline
    expected.assert_eq(&parsed_tree[0..parsed_tree.len() - 1]);
}

// Updating tests? Set `UPDATE_EXPECT=1` before running `cargo test`
#[cfg(test)]
mod test {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_empty_file() {
        check("", expect![[r#"Root@0..0"#]])
    }

    #[test]
    #[rustfmt::skip]
    fn parse_whitespace() {
        check("   \t\n   ", expect![[r#"
            Root@0..8
              Whitespace@0..8 "   \t\n   ""#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn parse_line_comment() {
        check("% hello", expect![[r#"
            Root@0..7
              Comment@0..7 "% hello""#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn parse_block_comment() {
        check("/* hello */", expect![[r#"
            Root@0..11
              Comment@0..11 "/* hello */""#]]);
    }
}
