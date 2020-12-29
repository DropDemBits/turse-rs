//! Main parser
mod error;
pub(crate) mod marker;

pub(crate) use error::ParseError;

use crate::event::Event;
use crate::grammar;
use crate::parser::error::ErrorKind;
use crate::parser::marker::MaybeMarker;
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

    /// Expects the next token to be of `kind`, otherwise reports an error
    ///
    /// # Returns
    /// Returns `true` if the expected token was found
    pub(crate) fn expect(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            // Nom on token, expected
            self.bump();
            true
        } else {
            self.error();
            false
        }
    }

    /// Reports an unexpected token error at the next token
    pub(crate) fn error(&mut self) {
        let err = self.start(); // start error node
        self.error_unexpected_at(err, &[]);
    }

    /// Reports an unexpected token error at the next token, with an extra recovery set
    pub(crate) fn error_with_extra_recovery(&mut self, extra: &[TokenKind]) {
        let err = self.start(); // start error node
        self.error_unexpected_at(err, extra);
    }

    /// Reports an unexpected token error at the given marker
    ///
    /// Note: `err_at` can't be a `Marker` as the passed-in marker must
    /// be allowed to not be made so that a bump at the end of the file
    /// is not made
    pub(crate) fn error_unexpected_at(
        &mut self,
        err_at: MaybeMarker,
        extra_recovery: &[TokenKind],
    ) {
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
            },
            range,
        }));

        if !self.at_set(&RECOVERY_SET) && !self.at_set(&extra_recovery) && !self.at_end() {
            // not in the recovery set? consume it! (wrap in error node)
            self.bump();
            err_at.complete(self, SyntaxKind::Error);
        } else {
            // Cursor is at the end of file or is part of the recovery set
            // Error node does not need to be built, so forget the marker
            err_at.forget(self);
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

    /// Resets the expected token list
    ///
    /// Used when there are leftover tokens from previous calls to `at`,
    /// but the tokens are not required
    pub(crate) fn reset_expected_tokens(&mut self) {
        self.expected_kinds.clear();
    }

    /// Bumps a token onto the current syntax node
    pub(crate) fn bump(&mut self) {
        self.reset_expected_tokens();
        self.source.next_token().expect("bump at the end of file");
        self.events.push(Event::AddToken);
    }
}
