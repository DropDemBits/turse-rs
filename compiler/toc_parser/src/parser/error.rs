//! Parser Error Reports

use std::fmt;

use toc_scanner::token::TokenKind;

#[derive(Debug, PartialEq)]
pub(crate) enum ParseMessage {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        expected_category: Option<Expected>,
        found: Option<TokenKind>,
    },
}

impl fmt::Display for ParseMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::UnexpectedToken {
                expected,
                expected_category,
                found,
            } => {
                write!(f, "expected ")?;

                if let Some(category) = expected_category {
                    write!(f, "{}", category)?;
                } else {
                    // Base it off of the expected tokens
                    let expected_count = expected.len();
                    let is_first = |i| i == 0;
                    let is_last = |i| i == expected_count - 1;

                    for (idx, expected_kind) in expected.iter().enumerate() {
                        if is_first(idx) {
                            write!(f, "{}", expected_kind)?;
                        } else if is_last(idx) {
                            write!(f, " or {}", expected_kind)?;
                        } else {
                            write!(f, ", {}", expected_kind)?;
                        }
                    }
                }

                if let Some(found) = found {
                    write!(f, ", but found {}", found)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum Expected {
    Expression,
    PreprocCondition,
    Statement,
    Type,
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Expression => "expression",
            Self::PreprocCondition => "preprocessor condition",
            Self::Statement => "statement",
            Self::Type => "type specifier",
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check(info: ParseMessage, out: Expect) {
        out.assert_eq(&format!("{}", info));
    }

    #[test]
    fn single_expected_but_found() {
        check(
            ParseMessage::UnexpectedToken {
                expected: vec![TokenKind::Assign],
                expected_category: None,
                found: Some(TokenKind::Equ),
            },
            expect![[r#"expected ‘:=’, but found ‘=’"#]],
        );
    }

    #[test]
    fn single_expected_but_not_found() {
        check(
            ParseMessage::UnexpectedToken {
                expected: vec![TokenKind::Range],
                expected_category: None,
                found: None,
            },
            expect![[r#"expected ‘..’"#]],
        );
    }

    #[test]
    fn multiple_expected_but_found() {
        check(
            ParseMessage::UnexpectedToken {
                expected: vec![
                    TokenKind::Identifier,
                    TokenKind::Bits,
                    TokenKind::Cheat,
                    TokenKind::Caret,
                ],
                expected_category: None,
                found: Some(TokenKind::IntLiteral),
            },
            expect![[r#"expected identifier, ‘bits’, ‘cheat’ or ‘^’, but found int literal"#]],
        );
    }

    #[test]
    fn two_expected_but_found() {
        check(
            ParseMessage::UnexpectedToken {
                expected: vec![TokenKind::Const, TokenKind::Var],
                expected_category: None,
                found: Some(TokenKind::Colon),
            },
            expect![[r#"expected ‘const’ or ‘var’, but found ‘:’"#]],
        );
    }

    #[test]
    fn multiple_expected_but_not_found() {
        check(
            ParseMessage::UnexpectedToken {
                expected: vec![
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::Not,
                    TokenKind::In,
                ],
                expected_category: None,
                found: None,
            },
            expect![[r#"expected ‘+’, ‘-’, ‘not’ or ‘in’"#]],
        );
    }

    #[test]
    fn unexpected_category_over_list() {
        // category has preference over token list
        check(
            ParseMessage::UnexpectedToken {
                expected: vec![TokenKind::Pervasive],
                expected_category: Some(Expected::Expression),
                found: Some(TokenKind::Var),
            },
            expect![[r#"expected expression, but found ‘var’"#]],
        )
    }
}
