//! Utilities for pretty printing
use std::fmt;

pub(crate) fn print_list<'list, L, I, W: fmt::Write>(f: &'list mut W, list: L) -> fmt::Result
where
    L: Iterator<Item = &'list I>,
    I: fmt::Display + 'list,
{
    let mut peek_enumerate = list.peekable();

    while let Some(item) = peek_enumerate.next() {
        f.write_fmt(format_args!("{}", item))?;

        if peek_enumerate.peek().is_some() {
            f.write_str(", ")?;
        }
    }

    Ok(())
}

/// Padding for pretty printing
/// Always does an indentation of 4
pub(crate) struct IndentWriter<'a, 'b: 'a> {
    // Always make indents at a newline
    make_indent: bool,
    // Link to enclosed formatter
    formatter: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> IndentWriter<'a, 'b> {
    pub fn new(formatter: &'a mut fmt::Formatter<'b>) -> Self {
        Self {
            make_indent: true,
            formatter,
        }
    }
}

impl fmt::Write for IndentWriter<'_, '_> {
    // Mostly taken from PadAdapter in Rust's standard library
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let mut s = s;

        while !s.is_empty() {
            if self.make_indent {
                self.formatter.write_str("    ")?;
                self.make_indent = false;
            }

            let to_newline = match s.find('\n') {
                Some(at) => {
                    // make indent, include in position
                    self.make_indent = true;
                    at + 1
                }
                None => s.len(), // Rest of slice
            };

            // Write portion & advance
            self.formatter.write_str(&s[..to_newline])?;
            s = &s[to_newline..];
        }

        Ok(())
    }
}
