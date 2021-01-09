//! Main parser
mod error;
pub(crate) mod marker;

use drop_bomb::DropBomb;
pub(crate) use error::{Expected, ParseMessage};

use crate::event::Event;
use crate::grammar;
use crate::parser::error::MessageKind;
use crate::parser::marker::Marker;
use crate::source::Source;

use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use toc_scanner::token::TokenKind;
use toc_syntax::SyntaxKind;

const STMT_START_RECOVERY_SET: &[TokenKind] = &[
    // Decls //
    TokenKind::Var,
    TokenKind::Const,
    TokenKind::Type,
    TokenKind::Bind,
    TokenKind::Procedure,
    TokenKind::Function,
    // Stmts //
    TokenKind::For,
    TokenKind::Loop,
    TokenKind::Exit,
    TokenKind::If,
    TokenKind::Elif,
    TokenKind::Elsif,
    TokenKind::Elseif,
    TokenKind::Else,
    TokenKind::Case,
    TokenKind::Begin,
    TokenKind::Invariant,
    TokenKind::Assert,
    TokenKind::Return,
    TokenKind::Result_,
    TokenKind::Signal,
    TokenKind::Pause,
    TokenKind::Checked,
    TokenKind::Unchecked,
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
        grammar::source(&mut self);
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
            self.error_unexpected().report();
            false
        } else {
            true
        }
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

    /// Provides a builder to report the unexpected token message
    pub(crate) fn error_unexpected<'p>(&'p mut self) -> UnexpectedBuilder<'p, 't, 'src> {
        UnexpectedBuilder::new(self)
    }

    /// Reports a token alias warning at the current token
    pub(crate) fn warn_alias(&mut self, normal: &str) {
        let current = self.source.peek_token();

        let (found, range) = current
            .map(|tok| (tok.kind, tok.range.clone()))
            .expect("warning of alias at end of file");

        self.events.push(Event::Message(ParseMessage {
            kind: MessageKind::OtherWarn(format!("{} found, assuming it to be {}", found, normal)),
            range,
        }));
    }

    /// Checks if the cursor is past the end of the file
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

pub(crate) struct UnexpectedBuilder<'p, 't, 's> {
    p: &'p mut Parser<'t, 's>,
    category: Option<Expected>,
    marker: Option<Marker>,
    bomb: DropBomb,
}

impl<'p, 't, 's> UnexpectedBuilder<'p, 't, 's> {
    fn new(p: &'p mut Parser<'t, 's>) -> Self {
        Self {
            p,
            category: None,
            marker: None,
            bomb: DropBomb::new("missing call to `report`"),
        }
    }

    /// Reports the unexpected token using the given category
    pub(crate) fn with_category(mut self, expected: Expected) -> Self {
        self.category = Some(expected);
        self
    }

    /// Reports the unexpected error with the specified marker
    pub(crate) fn with_marker(mut self, marker: Marker) -> Self {
        self.marker = Some(marker);
        self
    }

    /// Reports the error
    pub(crate) fn report(mut self) {
        self.bomb.defuse();

        let current = self.p.source.peek_token();

        let (found, range) = match current {
            Some(token) => (Some(token.kind), token.range.clone()),
            None => (None, self.p.source.last_token_range().unwrap()), // Last token always exists in a non-empty file
        };

        // push error
        debug_assert!(
            !self.p.expected_kinds.is_empty(),
            "Extra call to `error_unexpected`"
        );

        self.p.events.push(Event::Message(ParseMessage {
            kind: MessageKind::UnexpectedToken {
                expected: mem::take(&mut self.p.expected_kinds),
                expected_category: self.category,
                found,
            },
            range,
        }));

        // If the cursor is at the end of file or is part of the recovery set,
        // error node does not need to be built
        if !self.p.at_set(&STMT_START_RECOVERY_SET)
        && !self.p.at_set(&self.p.extra_recovery.clone().borrow()) // just cloning the Rc & reborrowing the contents
        && !self.p.at_end()
        {
            // not in the recovery set? consume it! (wrap in error node)
            let m = match self.marker {
                Some(marker) => marker,
                None => self.p.start(),
            };

            self.p.bump();

            m.complete(self.p, SyntaxKind::Error);
        } else if let Some(marker) = self.marker {
            // Always complete the provided marker, since there's
            // most likely some tokens already
            marker.complete(self.p, SyntaxKind::Error);
        }
    }
}
