//! Main parser
mod event;
mod expr;
mod sink;

use std::iter::Peekable;

use crate::parser::event::Event;
use crate::parser::sink::Sink;
use crate::syntax::{SyntaxKind, SyntaxNode};
use toc_scanner::{Scanner, TokenKind};

use rowan::GreenNode;

pub fn parse(source: &str) -> ParseResult {
    let parser = Parser::new(source);
    parser.parse()
}

pub struct ParseResult {
    node: GreenNode,
}

impl ParseResult {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }
}

struct Parser<'s> {
    scanner: Peekable<Scanner<'s>>,
    events: Vec<Event>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        let scanner = Scanner::new(source).peekable();

        Self {
            scanner,
            events: vec![],
        }
    }

    pub fn parse(mut self) -> ParseResult {
        self.start_node(SyntaxKind::Root);

        while self.peek().is_some() {
            expr::expr(&mut self);
        }

        self.finish_node();

        // Done with the events, build the actual tree
        let event_sink = Sink::new(self.events);

        ParseResult {
            node: event_sink.finish(),
        }
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.scanner.peek().map(|tok| tok.kind)
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
        let token = self.scanner.next().unwrap();

        self.events.push(Event::AddToken {
            kind: token.kind.into(),
            text: token.lexeme.into(),
        });
    }

    fn bump_as(&mut self, kind: SyntaxKind) {
        let token = self.scanner.next().unwrap();

        self.events.push(Event::AddToken {
            kind,
            text: token.lexeme.into(),
        });
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
}
