//! Main parser
mod event;
mod expr;
mod sink;
mod source;

use crate::parser::event::Event;
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
        self.start_node(SyntaxKind::Root);

        while self.peek().is_some() {
            expr::expr(&mut self);
        }

        self.finish_node();

        self.events
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode { kind });
    }

    fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
        self.events.push(Event::StartNodeAt { kind, checkpoint })
    }

    fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    fn bump(&mut self) {
        let token = self.source.next_token().unwrap();
        let kind = token.kind.into();
        let text = token.lexeme.into();

        self.events.push(Event::AddToken { kind, text });
    }

    fn bump_as(&mut self, kind: SyntaxKind) {
        let token = self.source.next_token().unwrap();
        let text = token.lexeme.into();

        self.events.push(Event::AddToken { kind, text });
    }

    fn checkpoint(&self) -> usize {
        self.events.len()
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
