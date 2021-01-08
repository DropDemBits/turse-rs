//! Parser Error Reports

use std::fmt;

use toc_scanner::token::{TokenKind, TokenRange};
use toc_scanner::ScannerError;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    pub(super) kind: ErrorKind,
    pub(super) range: TokenRange,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        self.kind.fmt(f)
    }
}

impl From<ScannerError> for ParseError {
    fn from(ScannerError(msg, at): ScannerError) -> Self {
        Self {
            kind: ErrorKind::OtherError(msg),
            range: at,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum ErrorKind {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        expected_category: Option<Expected>,
        found: Option<TokenKind>,
    },
    #[allow(unused)]
    InvalidLiteral {
        kind: InvalidLiteral,
    },
    OtherError(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ErrorKind::UnexpectedToken {
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
            ErrorKind::InvalidLiteral { kind } => {
                write!(f, "{}", kind)
            }
            ErrorKind::OtherError(msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum Expected {
    Expression,
    Statement,
    Type,
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Expression => "expression",
            Self::Statement => "statement",
            Self::Type => "type specifier",
        })
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(unused)]
pub(crate) enum InvalidLiteral {
    // Int Literals
    /// Int literal is unrepresentable in normal Turing code
    IntUnrepresentable,

    // Real literals
    /// Real literal is unrepresentable in normal Turing code
    RealUnrepresentable,
    /// Real literal is missing exponent digits
    RealMissingExponentDigits,

    // Radix literals
    /// Radix literal base is outside of the accepted range (2..=36)
    RadixInvalidBase,
    /// Radix literal contains a digit outside of the base's range
    RadixInvalidDigit,
    /// Radix literal is missing the digits portion
    RadixNoDigits,

    // String/Char Literals
    /// CharSeq Literal is missing the terminator
    CharSeqMissingTerminator,
    /// Invalid escape character
    CharSeqInvalidEscape,
    /// Octal escape character is greater than 255
    CharSeqBadOctalEscape,
    /// Unicode escape character is greater than U+10FFFF
    CharSeqBadUnicodeEscape,
    /// Unicode escape character is a surrogate character
    CharSeqUnicodeSurrogateEscape,
    /// Escape character (x, u, U) is missing hex digits
    CharSeqEscapeMissingDigits,
}

impl fmt::Display for InvalidLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            InvalidLiteral::IntUnrepresentable => "invalid int literal",
            InvalidLiteral::RealUnrepresentable => "invalid real literal",
            InvalidLiteral::RealMissingExponentDigits => "real literal is missing exponent digits",
            InvalidLiteral::RadixInvalidBase => "the base is outside the accepted range of 2 - 36",
            InvalidLiteral::RadixInvalidDigit => {
                "a digit in the literal is not an accepted character for the base"
            }
            InvalidLiteral::RadixNoDigits => "literal is missing digits",
            InvalidLiteral::CharSeqMissingTerminator => {
                "literal is missing the terminating character"
            }
            InvalidLiteral::CharSeqInvalidEscape => "invalid escape character",
            InvalidLiteral::CharSeqBadOctalEscape => "octal escape character is greater than 255",
            InvalidLiteral::CharSeqBadUnicodeEscape => {
                "unicode escape character is greater than U+10FFFF"
            }
            InvalidLiteral::CharSeqUnicodeSurrogateEscape => {
                "unicode escape character encodes a surrogate"
            }
            InvalidLiteral::CharSeqEscapeMissingDigits => "escape sequence is missing hex digits",
        };

        write!(f, "{}", text)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::{expect, Expect};
    use std::ops::Range;

    #[track_caller]
    fn check(kind: ErrorKind, range: Range<u32>, out: Expect) {
        let range = TokenRange::new(range.start.into(), range.end.into());

        let err = ParseError { kind, range };

        out.assert_eq(&format!("{}", err));
    }

    #[test]
    fn single_expected_but_found() {
        check(
            ErrorKind::UnexpectedToken {
                expected: vec![TokenKind::Assign],
                expected_category: None,
                found: Some(TokenKind::Equ),
            },
            11..12,
            expect![["error at 11..12: expected ’:=’, but found ’=’"]],
        );
    }

    #[test]
    fn single_expected_but_not_found() {
        check(
            ErrorKind::UnexpectedToken {
                expected: vec![TokenKind::Range],
                expected_category: None,
                found: None,
            },
            5..6,
            expect![["error at 5..6: expected ’..’"]],
        );
    }

    #[test]
    fn multiple_expected_but_found() {
        check(
            ErrorKind::UnexpectedToken {
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
                r#"error at 5..8: expected identifier, ’bits’, ’cheat’ or ’^’, but found int literal"#
            ]],
        );
    }

    #[test]
    fn two_expected_but_found() {
        check(
            ErrorKind::UnexpectedToken {
                expected: vec![TokenKind::Const, TokenKind::Var],
                expected_category: None,
                found: Some(TokenKind::Colon),
            },
            2..3,
            expect![[r#"error at 2..3: expected ’const’ or ’var’, but found ’:’"#]],
        );
    }

    #[test]
    fn multiple_expected_but_not_found() {
        check(
            ErrorKind::UnexpectedToken {
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
            expect![[r#"error at 5..8: expected ’+’, ’-’, ’not’ or ’in’"#]],
        );
    }

    #[test]
    fn unexpected_category_over_list() {
        // category has preference over token list
        check(
            ErrorKind::UnexpectedToken {
                expected: vec![TokenKind::Pervasive],
                expected_category: Some(Expected::Expression),
                found: Some(TokenKind::Var),
            },
            3..6,
            expect![[r#"error at 3..6: expected expression, but found ’var’"#]],
        )
    }
}
