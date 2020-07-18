pub(crate) mod ast;
pub(crate) mod block;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod scope;
pub(crate) mod token;
pub(crate) mod types;
pub(crate) mod validator;
pub(crate) mod value;

extern crate unicode_segmentation;

/// Location of a token in a file/text stream
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    /// Starting byte of a lexeme
    start: usize,
    /// Ending byte of a lexeme
    end: usize,
    /// Starting line number of the lexeme
    pub line: usize,
    /// Starting column of the lexeme
    pub column: usize,
    /// The column span of the lexeme, in columns
    pub width: usize,
    /// The line span of the lexeme, in lines
    pub line_span: usize,
}

impl Location {
    pub fn new() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
            width: 0,
            line_span: 0,
        }
    }

    /// Creates a new location spanning from this location to the other location, including the end location's token
    pub fn span_to(&self, other: &Self) -> Self {
        // Compute the correct span ends
        let line_span = other.line.saturating_sub(self.line);
        let width = if line_span > 0 {
            // More than one line, take the end span's column (plus the width, minus 1)
            other.column.saturating_add(other.width).saturating_sub(1)
        } else {
            // On the same line, span between the columns (including the widths)
            other
                .column
                .saturating_add(other.width)
                .saturating_sub(self.column)
        };

        Self {
            start: self.start,
            line: self.line,
            column: self.column,
            end: other.end,
            width,
            line_span,
        }
    }

    /// Advances the location to the next lexeme, beginning a new lexeme
    pub fn step(&mut self) {
        self.start = self.end;
        self.column += self.width;
        self.line += self.line_span;
        self.width = 0;
        self.line_span = 0;
    }

    /// Advances the column location by the give amount of steps
    pub fn columns(&mut self, steps: usize) {
        self.width += steps;
    }

    /// Advances the line location by the give amount of steps, as well as resetting the column
    pub fn lines(&mut self, steps: usize) {
        self.column = 1;
        self.width = 0;
        self.line_span += steps;
    }

    /// Moves the end of the lexeme to the given byte index
    pub fn current_to(&mut self, next_end: usize) {
        self.end = next_end;
    }

    /// Moves the end of the lexeme to the end of the given location
    pub fn current_to_other(&mut self, other: &Self) {
        self.end = other.end;
    }

    /// Gets the lexeme corresponding to this location
    pub fn get_lexeme<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}
