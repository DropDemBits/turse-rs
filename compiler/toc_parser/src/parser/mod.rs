//! Main parser
pub(crate) mod marker;

use crate::event::Event;
use crate::grammar;
use crate::parser::marker::Marker;
use crate::source::Source;

use toc_scanner::token::TokenKind;

pub(crate) struct Parser<'t, 'src> {
    source: Source<'t, 'src>,
    events: Vec<Event>,
}

impl<'t, 'src> Parser<'t, 'src> {
    pub(crate) fn new(source: Source<'t, 'src>) -> Self {
        Self {
            source,
            events: vec![],
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == Some(kind)
    }

    /// Creates a new `Marker` at the current position
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder); // Placeholder for future marker

        Marker::new(pos)
    }

    pub(crate) fn bump(&mut self) {
        self.source.next_token().expect("bump at the end of file");
        self.events.push(Event::AddToken);
    }
}
