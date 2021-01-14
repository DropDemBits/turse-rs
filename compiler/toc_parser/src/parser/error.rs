//! Parser Error Reports

use std::fmt;

use toc_scanner::token::{TokenKind, TokenRange};

#[derive(Debug, PartialEq)]
pub(crate) struct ParseMessage {
    pub(super) info: MessageInfo,
    pub(super) range: TokenRange,
}

impl ParseMessage {
    pub(crate) fn category(&self) -> toc_reporting::MessageKind {
        match self.info {
            MessageInfo::UnexpectedToken { .. } | MessageInfo::OtherError(..) => {
                toc_reporting::MessageKind::Error
            }
            MessageInfo::OtherWarn(..) => toc_reporting::MessageKind::Warning,
        }
    }

    pub(crate) fn message(&self) -> String {
        format!("{}", self.info)
    }

    pub(crate) fn range(&self) -> toc_reporting::TextRange {
        self.range
    }
}

impl fmt::Display for ParseMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self.category() {
            toc_reporting::MessageKind::Warning => "warn",
            toc_reporting::MessageKind::Error => "error",
        };

        write!(
            f,
            "{} at {}..{}: ",
            kind,
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        self.info.fmt(f)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum MessageInfo {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        expected_category: Option<Expected>,
        found: Option<TokenKind>,
    },
    OtherError(String),
    OtherWarn(String),
}

impl fmt::Display for MessageInfo {
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
            Self::OtherError(msg) | Self::OtherWarn(msg) => {
                write!(f, "{}", msg)
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
    use std::ops::Range;

    #[track_caller]
    fn check(info: MessageInfo, range: Range<u32>, out: Expect) {
        let range = TokenRange::new(range.start.into(), range.end.into());

        let err = ParseMessage { info, range };

        out.assert_eq(&format!("{}", err));
    }

    #[test]
    fn single_expected_but_found() {
        check(
            MessageInfo::UnexpectedToken {
                expected: vec![TokenKind::Assign],
                expected_category: None,
                found: Some(TokenKind::Equ),
            },
            11..12,
            expect![[r#"error at 11..12: expected ‘:=’, but found ‘=’"#]],
        );
    }

    #[test]
    fn single_expected_but_not_found() {
        check(
            MessageInfo::UnexpectedToken {
                expected: vec![TokenKind::Range],
                expected_category: None,
                found: None,
            },
            5..6,
            expect![[r#"error at 5..6: expected ‘..’"#]],
        );
    }

    #[test]
    fn multiple_expected_but_found() {
        check(
            MessageInfo::UnexpectedToken {
                expected: vec![
                    TokenKind::Identifier,
                    TokenKind::Bits,
                    TokenKind::Cheat,
                    TokenKind::Caret,
                ],
                expected_category: None,
                found: Some(TokenKind::IntLiteral),
            },
            5..8,
            expect![[
                r#"error at 5..8: expected identifier, ‘bits’, ‘cheat’ or ‘^’, but found int literal"#
            ]],
        );
    }

    #[test]
    fn two_expected_but_found() {
        check(
            MessageInfo::UnexpectedToken {
                expected: vec![TokenKind::Const, TokenKind::Var],
                expected_category: None,
                found: Some(TokenKind::Colon),
            },
            2..3,
            expect![[r#"error at 2..3: expected ‘const’ or ‘var’, but found ‘:’"#]],
        );
    }

    #[test]
    fn multiple_expected_but_not_found() {
        check(
            MessageInfo::UnexpectedToken {
                expected: vec![
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::Not,
                    TokenKind::In,
                ],
                expected_category: None,
                found: None,
            },
            5..8,
            expect![[r#"error at 5..8: expected ‘+’, ‘-’, ‘not’ or ‘in’"#]],
        );
    }

    #[test]
    fn unexpected_category_over_list() {
        // category has preference over token list
        check(
            MessageInfo::UnexpectedToken {
                expected: vec![TokenKind::Pervasive],
                expected_category: Some(Expected::Expression),
                found: Some(TokenKind::Var),
            },
            3..6,
            expect![[r#"error at 3..6: expected expression, but found ‘var’"#]],
        )
    }

    #[test]
    fn other_error_message() {
        check(
            MessageInfo::OtherError("this is not a real error message".to_string()),
            1..3,
            expect![["error at 1..3: this is not a real error message"]],
        );
    }
}
