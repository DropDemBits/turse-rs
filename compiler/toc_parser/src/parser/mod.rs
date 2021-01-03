//! Main parser
mod error;
pub(crate) mod marker;

pub(crate) use error::{Expected, ParseError};

use crate::event::Event;
use crate::grammar;
use crate::parser::error::ErrorKind;
use crate::parser::marker::Marker;
use crate::source::Source;

use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use toc_scanner::token::{Token, TokenKind};
use toc_syntax::SyntaxKind;

const STMT_START_RECOVERY_SET: &[TokenKind] = &[
    TokenKind::Var,
    TokenKind::Const,
    TokenKind::Type,
    TokenKind::If,
    TokenKind::Elif,
    TokenKind::Elsif,
    TokenKind::Elseif,
    TokenKind::Else,
    TokenKind::Begin,
];

pub(crate) struct Parser<'t, 'src> {
    source: Source<'t, 'src>,
    events: Vec<Event>,
    expected_kinds: Vec<TokenKind>,
    // Invariant: can only be modified in `with_extra_recovery`
    extra_recovery: Rc<RefCell<Vec<TokenKind>>>,
}

impl<'t, 'src> Parser<'t, 'src> {
    pub(crate) fn new(source: Source<'t, 'src>) -> Self {
        Self {
            source,
            events: vec![],
            expected_kinds: vec![],
            extra_recovery: Rc::new(RefCell::new(vec![])),
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }

    /// Checks if the next token is of `kind`
    ///
    /// # Returns
    /// Returns `true` if the expected token is of `kind`
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    /// Eats the next token if it is of `kind`, otherwise does nothing
    ///
    /// # Returns
    /// Returns `true` if the token was eaten
    pub(crate) fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            // nom, expected
            self.bump();
            true
        } else {
            false
        }
    }

    /// Eats the next token if it is of `kind`, otherwise does nothing
    ///
    /// Does not add the token to the expected_kinds list
    ///
    /// # Returns
    /// Returns `true` if the token was eaten
    pub(crate) fn hidden_eat(&mut self, kind: TokenKind) -> bool {
        if !self.eat(kind) {
            // pop last added item
            self.expected_kinds.pop();
            false
        } else {
            true
        }
    }

    /// Expects the next token to be of `kind`, otherwise reports an error
    ///
    /// # Returns
    /// Returns `true` if the expected token was found
    pub(crate) fn expect(&mut self, kind: TokenKind) -> bool {
        if !self.eat(kind) {
            self.error(None);
            false
        } else {
            true
        }
    }

    /// Reports an unexpected token error at the next token
    pub(crate) fn error(&mut self, category: impl Into<Option<Expected>>) {
        self.error_unexpected_at(None, category);
    }

    /// Runs the parser with the extra recovery tokens
    ///
    /// Recovery tokens are accumulated in nested `with_extra_recovery` calls
    pub(crate) fn with_extra_recovery<T>(
        &mut self,
        extra: &[TokenKind],
        f: impl FnOnce(&mut Parser) -> T,
    ) -> T {
        let previous_set = self.extra_recovery.borrow().len();
        self.extra_recovery.borrow_mut().extend_from_slice(extra);

        let t = f(self);

        self.extra_recovery.borrow_mut().truncate(previous_set);

        t
    }

    /// Reports an unexpected token error at the given marker
    pub(crate) fn error_unexpected_at(
        &mut self,
        err_at: impl Into<Option<Marker>>,
        category: impl Into<Option<Expected>>,
    ) {
        let current = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current {
            (Some(*kind), range.clone())
        } else {
            // Last token always exists in a non-empty file
            (None, self.source.last_token_range().unwrap())
        };

        // push error
        debug_assert!(
            !self.expected_kinds.is_empty(),
            "Extra call to `error` or `error_unexpected_at`"
        );

        self.events.push(Event::Error(ParseError {
            kind: ErrorKind::UnexpectedToken {
                expected: mem::take(&mut self.expected_kinds),
                expected_category: category.into(),
                found,
            },
            range,
        }));

        // If the cursor is at the end of file or is part of the recovery set,
        // error node does not need to be built
        if !self.at_set(&STMT_START_RECOVERY_SET)
        && !self.at_set(&self.extra_recovery.clone().borrow()) // just cloning the Rc & reborrowing the contents
        && !self.at_end()
        {
            // not in the recovery set? consume it! (wrap in error node)
            let m = err_at.into().unwrap_or_else(|| self.start());
            self.bump();
            m.complete(self, SyntaxKind::Error);
        } else if let Some(err_at) = err_at.into() {
            // Always complete the provided marker,
            // since there's most likely some tokens already
            err_at.complete(self, SyntaxKind::Error);
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

    /// Creates a new `Marker` at the current position
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder); // Placeholder for future marker

        Marker::new(pos)
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
