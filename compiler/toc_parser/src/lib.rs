//! Parser for building the initial Concrete Syntax Tree
mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use source::Source;
use toc_reporting::{MessageKind, ReportMessage};
use toc_scanner::Scanner;
use toc_syntax::SyntaxNode;

use rowan::GreenNode;

use crate::sink::Sink;

pub fn parse(source: &str) -> ParseResult {
    let (tokens, scanner_msgs) = Scanner::new(source).collect_all();

    let source = Source::new(&tokens);
    let parser = parser::Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events, scanner_msgs);

    sink.finish()
}

pub struct ParseResult {
    node: GreenNode,
    messages: Vec<ReportMessage>,
}

impl ParseResult {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }

    pub fn has_errors(&self) -> bool {
        self.messages
            .iter()
            .any(|msg| msg.kind() == MessageKind::Error)
    }

    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }

    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let syntax_node = SyntaxNode::new_root(self.node.clone());
        let tree = format!("{:#?}", syntax_node);

        // trim trailing newline
        s.push_str(&tree[0..tree.len() - 1]);

        for err in &self.messages {
            s.push_str(&format!("\n{}", err));
        }

        s
    }
}

#[cfg(test)]
#[track_caller]
pub(crate) fn check(source: &str, expected: expect_test::Expect) {
    let res = parse(source);
    expected.assert_eq(&res.debug_tree());
}

// Updating tests? Set `UPDATE_EXPECT=1` before running `cargo test`
#[cfg(test)]
mod test {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_empty_file() {
        check("", expect![[r#"Source@0..0"#]])
    }

    #[test]
    #[rustfmt::skip]
    fn parse_whitespace() {
        check("   \t\n   ", expect![[r#"
            Source@0..8
              Whitespace@0..8 "   \t\n   ""#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn parse_line_comment() {
        check("% hello", expect![[r#"
            Source@0..7
              Comment@0..7 "% hello""#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn parse_block_comment() {
        check("/* hello */", expect![[r#"
            Source@0..11
              Comment@0..11 "/* hello */""#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn scanner_errors_into_parser_errors() {
        check("var e := 1e", expect![[r#"
            Source@0..11
              ConstVarDecl@0..11
                KwVar@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..6
                  Name@4..6
                    Identifier@4..5 "e"
                    Whitespace@5..6 " "
                Assign@6..8 ":="
                Whitespace@8..9 " "
                LiteralExpr@9..11
                  RealLiteral@9..11 "1e"
            error at 9..11: real literal is missing exponent digits"#]]);
    }
}
