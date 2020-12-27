//! Sink for events
use crate::parser::event::Event;
use crate::syntax::SyntaxKind;

use rowan::{GreenNode, GreenNodeBuilder, SmolStr};
use toc_scanner::{Token, TokenKind};

pub(super) struct Sink<'t, 'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'src>],
    cursor: usize,
    events: Vec<Event>,
}

impl<'t, 'src> Sink<'t, 'src> {
    pub(super) fn new(tokens: &'t [Token<'src>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        // Rewrite event history to remove `StartNodeAt` by pulling back nodes
        let mut reordered_events = self.events.clone();

        for (idx, event) in self.events.iter().enumerate() {
            if let Event::StartNodeAt { kind, checkpoint } = event {
                reordered_events.remove(idx);
                reordered_events.insert(*checkpoint, Event::StartNode { kind: *kind });
            }
        }

        for event in reordered_events {
            match event {
                Event::StartNode { kind } => self.builder.start_node(kind.into()),
                Event::StartNodeAt { .. } => unreachable!(),
                Event::AddToken { kind, text } => self.token(kind, text),
                Event::FinishNode => self.builder.finish_node(),
            }

            self.skip_trivia();
        }

        self.builder.finish()
    }

    fn token(&mut self, kind: SyntaxKind, text: SmolStr) {
        self.builder.token(kind.into(), text);
        self.cursor += 1;
    }

    /// Skips all whitespace, including comments
    fn skip_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            self.token(token.kind.into(), token.lexeme.into())
        }
    }
}
