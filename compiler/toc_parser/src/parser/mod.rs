//! Main parser
mod error;
pub(crate) mod marker;

pub(crate) use error::ParseError;

use crate::event::Event;
use crate::grammar;
use crate::parser::error::ErrorKind;
use crate::parser::marker::{Marker, MaybeMarker};
use crate::source::Source;

use std::mem;
use toc_scanner::token::{Token, TokenKind};
use toc_syntax::SyntaxKind;

const RECOVERY_SET: &[TokenKind] = &[TokenKind::Var, TokenKind::Const];

pub(crate) struct Parser<'t, 'src> {
    source: Source<'t, 'src>,
    events: Vec<Event>,
    expected_kinds: Vec<TokenKind>,
}

impl<'t, 'src> Parser<'t, 'src> {
    pub(crate) fn new(source: Source<'t, 'src>) -> Self {
        Self {
            source,
            events: vec![],
            expected_kinds: vec![],
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            // Nom on token, expected
            self.bump()
        } else {
            self.error();
        }
    }

    /// Reports an unexpected token error at the current token
    pub(crate) fn error(&mut self) {
        let err = self.start(); // start error node
        self.error_unexpected_at(err);
    }

    /// Reports an unexpected token error at the given marker
    pub(crate) fn error_unexpected_at<T>(&mut self, err_at: T)
    where
        T: Into<Marker>,
    {
        let current = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current {
            (Some(*kind), range.clone())
        } else {
            // Last token always exists in a non-empty file
            (None, self.source.last_token_range().unwrap())
        };

        // push error
        self.events.push(Event::Error(ParseError {
            kind: ErrorKind::UnexpectedToken {
                expected: mem::take(&mut self.expected_kinds),
                found,
                range,
            },
        }));

        if !self.at_set(&RECOVERY_SET) && !self.at_end() {
            // not in the recovery set? consume it! (wrap in error node)
            self.bump();
            err_at.into().complete(self, SyntaxKind::Error);
        }
    }

    /// Checks if the cursor is past the end
    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Checks if the current token is in the given set
    fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    /// Creates a new `MaybeMarker` at the current position
    pub(crate) fn start(&mut self) -> MaybeMarker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder); // Placeholder for future marker

        MaybeMarker::new(pos)
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().expect("bump at the end of file");
        self.events.push(Event::AddToken);
    }
}
