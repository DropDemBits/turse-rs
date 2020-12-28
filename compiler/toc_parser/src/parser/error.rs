//! Parser Error Reports

use std::fmt;

use toc_scanner::token::{TokenKind, TokenRange};

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    pub(super) kind: ErrorKind,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum ErrorKind {
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        range: TokenRange,
    },
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ErrorKind::UnexpectedToken {
                expected,
                found,
                range,
            } => {
                write!(
                    f,
                    "error at {}..{}: expected ",
                    u32::from(range.start()),
                    u32::from(range.end()),
                )?;

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

                if let Some(found) = found {
                    write!(f, ", but found {}", found)?;
                }

                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::{expect, Expect};
    use std::ops::Range;

    #[track_caller]
    fn check(expected: Vec<TokenKind>, found: Option<TokenKind>, range: Range<u32>, out: Expect) {
        let range = TokenRange::new(range.start.into(), range.end.into());

        let err = ParseError {
            kind: ErrorKind::UnexpectedToken {
                expected,
                found,
                range,
            },
        };

        out.assert_eq(&format!("{}", err));
    }

    #[test]
    fn single_expected_but_found() {
        check(
            vec![TokenKind::Assign],
            Some(TokenKind::Equ),
            11..12,
            expect![["error at 11..12: expected ’:=’, but found ’=’"]],
        );
    }

    #[test]
    fn single_expected_but_not_found() {
        check(
            vec![TokenKind::Range],
            None,
            5..6,
            expect![["error at 5..6: expected ’..’"]],
        );
    }

    #[test]
    fn multiple_expected_but_found() {
        check(
            vec![
                TokenKind::Identifier,
                TokenKind::Bits,
                TokenKind::Cheat,
                TokenKind::Caret,
            ],
            Some(TokenKind::IntLiteral),
            5..8,
            expect![[
                r#"error at 5..8: expected identifier, ’bits’, ’cheat’ or ’^’, but found int literal"#
            ]],
        );
    }

    #[test]
    fn two_expected_but_found() {
        check(
            vec![TokenKind::Const, TokenKind::Var],
            Some(TokenKind::Colon),
            2..3,
            expect![[r#"error at 2..3: expected ’const’ or ’var’, but found ’:’"#]],
        );
    }

    #[test]
    fn multiple_expected_but_not_found() {
        check(
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Not,
                TokenKind::In,
            ],
            None,
            5..8,
            expect![[r#"error at 5..8: expected ’+’, ’-’, ’not’ or ’in’"#]],
        );
    }
}
