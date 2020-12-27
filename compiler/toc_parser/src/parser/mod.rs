//! Main parser
mod event;
mod expr;
mod marker;
mod sink;
mod source;

use crate::parser::event::Event;
use crate::parser::marker::Marker;
use crate::parser::sink::Sink;
use crate::parser::source::Source;
use crate::syntax::{SyntaxKind, SyntaxNode};

use toc_scanner::{Scanner, Token, TokenKind};

use rowan::GreenNode;

pub fn parse(source: &str) -> ParseResult {
    let tokens: Vec<_> = Scanner::new(source).collect();
    let parser = Parser::new(&tokens);
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

struct Parser<'t, 'src> {
    source: Source<'t, 'src>,
    events: Vec<Event>,
}

impl<'t, 'src> Parser<'t, 'src> {
    pub fn new(tokens: &'t [Token<'src>]) -> Self {
        Self {
            source: Source::new(tokens),
            events: vec![],
        }
    }

    pub fn parse(mut self) -> Vec<Event> {
        let root = self.start();

        while self.peek().is_some() {
            expr::expr(&mut self);
        }

        root.complete(&mut self, SyntaxKind::Root);

        self.events
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Creates a new `Marker` at the current position
    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder); // Placeholder for future marker

        Marker::new(pos)
    }

    fn bump(&mut self) {
        self.source.next_token().expect("bump at the end of file");
        self.events.push(Event::AddToken);
    }
}

#[cfg(test)]
#[track_caller]
fn check(source: &str, expected: expect_test::Expect) {
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
