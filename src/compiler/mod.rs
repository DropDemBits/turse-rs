pub(crate) mod ast;
pub(crate) mod parser;
pub(crate) mod scanner;
pub(crate) mod token;
pub(crate) mod types;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

/// Location of a token in a file/text stream
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    /// Starting byte of a lexeme
    start: usize,
    /// Ending byte of a lexeme
    end: usize,
    /// Line number of the lexeme
    pub line: usize,
    /// Starting column of the lexeme
    pub column: usize,
}

impl Location {
    pub fn new() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }
    }

    /// Advances the location to the next lexeme, beginning a new lexeme
    pub fn step(&mut self) {
        self.start = self.end;
    }

    /// Advances the column location by the give amount of steps
    pub fn columns(&mut self, steps: usize) {
        self.column += steps;
    }

    /// Advances the line location by the give amount of steps, as well as resetting the column
    pub fn lines(&mut self, steps: usize) {
        self.column = 1;
        self.line += steps;
    }

    /// Moves the end of the lexeme to the given byte index
    pub fn current_to(&mut self, next_end: usize) {
        self.end = next_end;
    }

    /// Moves the end of the lexeme to the end of the given location
    pub fn current_to_other(&mut self, other: &Location) {
        self.end = other.end;
    }

    /// Gets the width of the location from the slice, in graphemes
    pub fn get_length(&self, source: &str) -> usize {
        if self.start == self.end {
            0
        } else {
            let slice = &source[self.start..self.end];
            UnicodeSegmentation::graphemes(slice, true).count()
        }
    }
}
