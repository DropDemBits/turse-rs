//! Sink for events
use crate::event::Event;
use crate::parser::ParseMessage;
use crate::ParseResult;

use rowan::GreenNodeBuilder;
use std::mem;
use toc_scanner::token::Token;
use toc_scanner::ScannerError;
use toc_syntax::SyntaxKind;

pub(super) struct Sink<'t, 'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'src>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParseMessage>,
}

impl<'t, 'src> Sink<'t, 'src> {
    pub(super) fn new(
        tokens: &'t [Token<'src>],
        events: Vec<Event>,
        scanner_errors: Vec<ScannerError>,
    ) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
            errors: scanner_errors.into_iter().map(|err| err.into()).collect(),
        }
    }

    pub(super) fn finish(mut self) -> ParseResult {
        for idx in 0..self.events.len() {
            match mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let mut look_at = forward_parent.map(|off| idx + off);
                    let mut parent_kinds = vec![kind];

                    while let Some(parent_at) = look_at {
                        // Pull forward parent node
                        let node = mem::replace(&mut self.events[parent_at], Event::Placeholder);

                        if let Event::StartNode {
                            kind,
                            forward_parent,
                        } = node
                        {
                            // Jump to the next parent
                            look_at = forward_parent.map(|off| parent_at + off);
                            parent_kinds.push(kind);
                        } else {
                            unreachable!();
                        }
                    }

                    // Push the nodes, from outermost (last) to innermost (first)
                    parent_kinds.into_iter().rev().for_each(|kind| {
                        self.builder.start_node(kind.into());
                    });
                }
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Message(err) => self.errors.push(err),
                Event::Tombstone | Event::Placeholder => {}
            }

            self.skip_trivia();
        }

        ParseResult {
            node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn token(&mut self) {
        let token = &self.tokens[self.cursor];
        let kind: SyntaxKind = token.kind.into();
        let text = token.lexeme.into();

        self.builder.token(kind.into(), text);
        self.cursor += 1;
    }

    /// Skips all whitespace, including comments
    fn skip_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            self.token()
        }
    }
}
