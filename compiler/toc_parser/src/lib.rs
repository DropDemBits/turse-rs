//! Parser for building the initial Concrete Syntax Tree
mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use source::Source;
use toc_reporting::ReportMessage;
use toc_scanner::Scanner;
use toc_span::FileId;
use toc_syntax::SyntaxNode;

use rowan::GreenNode;

use crate::sink::Sink;

pub fn parse(file: Option<FileId>, source: &str) -> ParseResult {
    let (tokens, scanner_msgs) = Scanner::new(file, source).collect_all();

    let source = Source::new(&tokens);
    let parser = parser::Parser::new(file, source);
    let (events, mut parser_msgs) = parser.parse();
    parser_msgs.dedup_shared_ranges();
    let sink = Sink::new(&tokens, events, vec![scanner_msgs, parser_msgs]);

    sink.finish()
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseResult {
    node: GreenNode,
    messages: Vec<ReportMessage>,
}

impl ParseResult {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }

    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }

    pub fn copy_messages(&self, dest: &mut Vec<ReportMessage>) {
        dest.extend_from_slice(&self.messages);
    }

    pub fn dump_tree(&self) -> String {
        let mut s = String::new();

        let syntax_node = SyntaxNode::new_root(self.node.clone());
        let tree = format!("{:#?}", syntax_node);

        s.push_str(&tree);

        s
    }

    pub fn debug_tree(&self) -> String {
        let mut s = self.dump_tree();

        // trim trailing newline
        s.pop();

        for err in &self.messages {
            s.push_str(&format!("\n{}", err));
        }

        s
    }
}

#[cfg(test)]
#[track_caller]
pub(crate) fn check(source: &str, expected: expect_test::Expect) {
    let res = parse(None, source);
    expected.assert_eq(&res.debug_tree());
}

// Updating tests? Set `UPDATE_EXPECT=1` before running `cargo test`
#[cfg(test)]
mod test {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_empty_file() {
        check(
            "",
            expect![[r#"
            Source@0..0
              StmtList@0..0"#]],
        )
    }

    #[test]
    #[rustfmt::skip]
    fn parse_whitespace() {
        check("   \t\n   ", expect![[r#"
            Source@0..8
              Whitespace@0..8 "   \t\n   "
              StmtList@8..8"#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn parse_line_comment() {
        check("% hello", expect![[r#"
            Source@0..7
              Comment@0..7 "% hello"
              StmtList@7..7"#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn parse_block_comment() {
        check("/* hello */", expect![[r#"
            Source@0..11
              Comment@0..11 "/* hello */"
              StmtList@11..11"#]]);
    }

    #[test]
    #[rustfmt::skip]
    fn scanner_errors_into_parser_errors() {
        check("var e := 1e", expect![[r#"
            Source@0..11
              StmtList@0..11
                ConstVarDecl@0..11
                  KwVar@0..3 "var"
                  Whitespace@3..4 " "
                  NameList@4..5
                    Name@4..5
                      Identifier@4..5 "e"
                  Whitespace@5..6 " "
                  Assign@6..8 ":="
                  Whitespace@8..9 " "
                  LiteralExpr@9..11
                    RealLiteral@9..11 "1e""#]]);
    }
}
