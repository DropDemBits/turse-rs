//! Token Source

use toc_scanner::token::{Token, TokenKind, TokenRange};

pub(super) struct Source<'t, 'src> {
    tokens: &'t [Token<'src>],
    cursor: usize,
}

impl<'t, 'src> Source<'t, 'src> {
    pub(crate) fn new(tokens: &'t [Token<'src>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(crate) fn next_token(&mut self) -> Option<&Token> {
        self.skip_trivia();
        let token = self.tokens.get(self.cursor)?;

        self.cursor += 1;
        Some(token)
    }

    pub(crate) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_raw()
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        self.skip_trivia();
        self.peek_token_raw()
    }

    pub(crate) fn last_token_range(&self) -> Option<TokenRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.peek_token_raw().map(|tok| tok.kind)
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    /// Skips over all triva tokens
    fn skip_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().map_or(false, TokenKind::is_trivia)
    }
}
