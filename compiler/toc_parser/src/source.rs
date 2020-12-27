//! Token Source
use toc_scanner::token::{Token, TokenKind};
use toc_syntax::SyntaxKind;

pub(super) struct Source<'t, 'src> {
    tokens: &'t [Token<'src>],
    cursor: usize,
}

impl<'t, 'src> Source<'t, 'src> {
    pub(super) fn new(tokens: &'t [Token<'src>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(super) fn next_token(&mut self) -> Option<&Token> {
        self.skip_trivia();
        let token = self.tokens.get(self.cursor)?;

        self.cursor += 1;
        Some(token)
    }

    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_raw()
    }

    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.tokens.get(self.cursor).map(|tok| tok.kind)
    }

    /// Skips over all triva tokens
    fn skip_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw()
            .map_or(false, |kind| SyntaxKind::from(kind).is_trivia())
    }
}
