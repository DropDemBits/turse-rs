//! Main parser
use crate::syntax::{SyntaxKind, SyntaxNode};
use toc_scanner::{Scanner, Token};

use rowan::{GreenNode, GreenNodeBuilder};
use std::fmt;

pub fn parse(source: &str) -> ParseResult {
    let parser = Parser::new(source);
    parser.parse()
}

pub struct ParseResult {
    node: GreenNode,
}

impl ParseResult {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }
}

struct Parser<'src> {
    scanner: Scanner<'src>,
    builder: GreenNodeBuilder<'static>,

    // scanner stuff
    previous: Option<(Token, &'src str)>,
    current: Option<(Token, &'src str)>,
    peek: Option<(Token, &'src str)>,
}

/// Converts a token into the corresponding rowan `SyntaxKind`
fn to_kind(token: Token) -> rowan::SyntaxKind {
    SyntaxKind::from_token(token).into()
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        let mut scanner = Scanner::new(source);
        let previous = None;
        let current = scanner.next();
        let peek = scanner.next();

        Self {
            scanner,
            builder: GreenNodeBuilder::new(),
            previous,
            current,
            peek,
        }
    }

    /// Gets the previous token in the stream
    fn previous(&self) -> Option<Token> {
        self.previous.map(|tk| tk.0)
    }

    /// Gets the current token in the stream
    fn current(&self) -> Option<Token> {
        self.current.map(|tk| tk.0)
    }

    /// Peeks at the next token in the stream
    /// `peek` is not affected by token stitching
    fn peek(&self) -> Option<Token> {
        self.peek.map(|tk| tk.0)
    }

    /// Advances `peek` without updating `current` and `previous`.
    ///
    /// Only used to support token stitching
    fn advance_peek(&mut self) {}

    /// Advances to the next token, returning the previous token
    fn next_token(&mut self) -> Option<Token> {
        std::mem::swap(&mut self.previous, &mut self.current);
        std::mem::swap(&mut self.current, &mut self.peek);
        self.peek = self.scanner.next();

        // Bump into node
        if let Some((previous, text)) = self.previous.clone() {
            self.builder
                .token(SyntaxKind::from_token(previous).into(), text.into());
            Some(previous)
        } else {
            None
        }
    }

    /// Checks if all of the tokens have been consumed yet
    fn is_at_end(&self) -> bool {
        self.current().is_none()
    }

    /// Expects a certain token to be next in the stream. \
    /// If the current token matches the expected token, the current token is consumed.
    /// Otherwise an error message is reported.
    fn expects(&mut self, expected_type: Token, message: fmt::Arguments) -> Result<Token, ()> {
        if self.current() == Some(expected_type) {
            Ok(self.next_token().unwrap()) // token should still be present
        } else {
            // TODO: Reporting!
            /*self.reporter
            .borrow_mut()
            .report_error(&self.current().location, message);*/

            Err(())
        }
    }

    /// Optionally expects a certain token to be next in the stream. \
    /// If the current token matches the expected token, the current token is
    /// consumed and true is returned.
    /// Otherwise, false is returned.
    fn optional(&mut self, optional_type: Token) -> bool {
        if self.current() == Some(optional_type) {
            self.next_token();
            true
        } else {
            false
        }
    }

    /// Skips to the next non-whitespace token (excluding newlines)
    fn skip_ws(&mut self) {
        while let Some(Token::Whitespace) = self.next_token() {}
    }

    /// Skips to the next non-whitespace token (including newlines)
    fn skip_ws_and_nl(&mut self) {
        while matches!(
            self.next_token(),
            Some(Token::Whitespace) | Some(Token::LineEnd)
        ) {}
    }

    pub fn parse(mut self) -> ParseResult {
        self.builder.start_node(to_kind(Token::Root));

        self.builder.finish_node();
        ParseResult {
            node: self.builder.finish(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_empty_file() {
        let res = parse("");
        assert_eq!(format!("{:?}", res.syntax()), "Root@0..0");
    }
}
