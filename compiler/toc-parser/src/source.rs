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

    pub(crate) fn next_token(&mut self) -> Option<&Token<'_>> {
        self.cursor = self.skip_trivia(self.cursor)?;
        let token = self.tokens.get(self.cursor)?;

        self.cursor += 1;
        Some(token)
    }

    /// Peeks at the next `TokenKind`
    pub(crate) fn peek_kind(&self) -> Option<TokenKind> {
        let peek_to = self.skip_trivia(self.cursor)?;
        self.token_kind_at(peek_to)
    }

    /// Peeks at the next `Token`
    pub(crate) fn peek_token(&self) -> Option<&Token<'_>> {
        let peek_to = self.skip_trivia(self.cursor)?;
        self.token_at(peek_to)
    }

    /// Gets the last non-trivia token, or looks up the last token
    pub(crate) fn last_non_trivia_token_range(&self) -> Option<TokenRange> {
        self.tokens
            .iter()
            .rev()
            .find(|tok| !tok.kind.is_trivia())
            .or_else(|| self.tokens.last())
            .map(|Token { range, .. }| *range)
    }

    fn token_kind_at(&self, cursor: usize) -> Option<TokenKind> {
        self.token_at(cursor).map(|tok| tok.kind)
    }

    fn token_at(&self, cursor: usize) -> Option<&Token<'_>> {
        self.tokens.get(cursor)
    }

    /// Skips over all trivia tokens starting from `usize`,
    /// returning the index of the first non-trivia token
    /// (or `None` if it's outside the token list)
    fn skip_trivia(&self, mut cursor: usize) -> Option<usize> {
        while self.at_trivia(cursor) {
            cursor += 1;
        }

        Some(cursor).filter(|cursor| *cursor < self.tokens.len())
    }

    fn at_trivia(&self, cursor: usize) -> bool {
        self.token_kind_at(cursor)
            .map_or(false, TokenKind::is_trivia)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn make_tokens(tokens: &[TokenKind]) -> Vec<Token> {
        tokens
            .iter()
            .map(|kind| Token {
                kind: *kind,
                lexeme: "",
                range: TokenRange::new(0.into(), 0.into()),
            })
            .collect()
    }

    fn to_kind(token: Option<&Token>) -> Option<TokenKind> {
        token.map(|tok| tok.kind)
    }

    #[test]
    fn skip_whitespace() {
        let tokens = make_tokens(&[
            TokenKind::Var,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Assign,
            TokenKind::Whitespace,
            TokenKind::IntLiteral,
        ]);
        let mut source = Source::new(&tokens);

        assert_eq!(to_kind(source.next_token()), Some(TokenKind::Var));
        assert_eq!(to_kind(source.next_token()), Some(TokenKind::Identifier));
        assert_eq!(to_kind(source.next_token()), Some(TokenKind::Assign));
        assert_eq!(to_kind(source.next_token()), Some(TokenKind::IntLiteral));
        assert_eq!(to_kind(source.next_token()), None);
    }
}
