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
    SyntaxKind::from(token).into()
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
                .token(SyntaxKind::from(previous).into(), text.into());
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

    /// Skips to the next non-whitespace token (including newlines and comments)
    fn skip_ws(&mut self) {
        while matches!(
            self.current(),
            Some(Token::Whitespace) | Some(Token::Comment)
        ) {
            self.next_token();
        }
    }

    pub fn parse(mut self) -> ParseResult {
        self.builder.start_node(SyntaxKind::Root.into());
        self.skip_ws(); // go over preliminary stuff

        while let Some(token) = self.current() {
            match token {
                Token::Var | Token::Const => self.decl_var(),
                _ => {
                    // make error node
                    self.builder.start_node(to_kind(Token::Error));
                    while !matches!(self.next_token(), Some(Token::Var) | Some(Token::Const)) {}
                    self.builder.finish_node();
                }
            }
        }

        self.builder.finish_node();
        ParseResult {
            node: self.builder.finish(),
        }
    }

    fn name_list(&mut self) {
        self.builder.start_node(SyntaxKind::NameList.into());

        self.builder.start_node(SyntaxKind::Name.into());
        let _ = self.expects(
            Token::Identifier,
            format_args!("Expected name to start list of names"),
        );
        self.builder.finish_node();

        self.skip_ws();

        while self.optional(Token::Comma) {
            self.skip_ws();
            self.builder.start_node(SyntaxKind::Name.into());
            let _ = self.expects(Token::Identifier, format_args!("Expected name after ','"));
            self.builder.finish_node();
            self.skip_ws();
        }

        self.builder.finish_node();
    }

    fn decl_var(&mut self) {
        assert!(matches!(
            self.current(),
            Some(Token::Var) | Some(Token::Const)
        ));

        self.builder.start_node(SyntaxKind::ConstVarDecl.into());
        self.next_token(); // nom const or var
        self.skip_ws();

        self.name_list();
        self.skip_ws();

        if self.optional(Token::Colon) {
            self.skip_ws();
            // TODO: parse type spec
        }
        self.skip_ws();

        if self.optional(Token::Assign) {
            self.skip_ws();
            // TODO: parse init val
        }
        self.skip_ws();

        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::{expect, Expect};

    fn dump_tree(syntax: SyntaxNode) -> String {
        let mut s = format!("{:?}", syntax);
        s.push_str(" [ ");
        syntax.children().for_each(|child| {
            s.push_str(&dump_tree(child));
            s.push_str(", ")
        });
        s.push_str("]");
        s
    }

    #[track_caller]
    fn check(source: &str, expected: Expect) {
        let res = parse(source);
        let syntax = res.syntax();
        let parsed_tree = format!("{:#?}", syntax);

        // chop off newline
        expected.assert_eq(&parsed_tree[0..parsed_tree.len() - 1]);
    }

    #[test]
    fn test_empty_file() {
        check("", expect![[r#"Root@0..0"#]])
    }

    #[test]
    #[rustfmt::skip]
    fn test_decl_var() {
        check("var hello", expect![[r#"
            Root@0..9
              ConstVarDecl@0..9
                Var@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..9
                  Name@4..9
                    Identifier@4..9 "hello""#]]);
        check("const hello", expect![[r#"
            Root@0..11
              ConstVarDecl@0..11
                Const@0..5 "const"
                Whitespace@5..6 " "
                NameList@6..11
                  Name@6..11
                    Identifier@6..11 "hello""#]]);
        check("var hello : ", expect![[r#"
            Root@0..12
              ConstVarDecl@0..12
                Var@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..10
                  Name@4..9
                    Identifier@4..9 "hello"
                  Whitespace@9..10 " "
                Colon@10..11 ":"
                Whitespace@11..12 " ""#]]);
        check("const hello : ", expect![[r#"
            Root@0..14
              ConstVarDecl@0..14
                Const@0..5 "const"
                Whitespace@5..6 " "
                NameList@6..12
                  Name@6..11
                    Identifier@6..11 "hello"
                  Whitespace@11..12 " "
                Colon@12..13 ":"
                Whitespace@13..14 " ""#]]);
        check("var hello : :=", expect![[r#"
            Root@0..14
              ConstVarDecl@0..14
                Var@0..3 "var"
                Whitespace@3..4 " "
                NameList@4..10
                  Name@4..9
                    Identifier@4..9 "hello"
                  Whitespace@9..10 " "
                Colon@10..11 ":"
                Whitespace@11..12 " "
                Assign@12..14 ":=""#]]);
        check("const hello : :=", expect![[r#"
            Root@0..16
              ConstVarDecl@0..16
                Const@0..5 "const"
                Whitespace@5..6 " "
                NameList@6..12
                  Name@6..11
                    Identifier@6..11 "hello"
                  Whitespace@11..12 " "
                Colon@12..13 ":"
                Whitespace@13..14 " "
                Assign@14..16 ":=""#]]);
    }
}
