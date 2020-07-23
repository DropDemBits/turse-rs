//! Root module for all compiler facilities
pub(crate) mod ast;
pub(crate) mod block;
pub(crate) mod frontend;
pub(crate) mod scope;
pub(crate) mod types;
pub(crate) mod value;

#[cfg(test)]
extern crate rand;
extern crate unicode_segmentation;

use std::fmt;

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

/// All valid operators usable in Turing code
#[derive(Debug)]
pub enum Operator {
    // Unary Operators
    /// Unary Plus/Identity (`+`)
    UnaryPlus,
    /// Unary Minus/Negation (`-`)
    UnaryMinus,
    /// Nat Typecheat (`#`)
    NatCheat,
    /// Bitwise/Logical Not (`not`)
    Not,
    /// Pointer Dereference (`^`)
    PointerDeref,

    // Binary Operators
    /// Addition / Union (`+`)
    Add,
    /// Subtraction / Difference (`+`)
    Sub,
    /// Multiplication / Intersection (`*`)
    Mul,
    /// Real Division (`/`)
    RealDiv,
    /// Integer Division (`div`)
    IntDiv,
    /// Modulo (`mod`)
    Mod,
    /// Remainder (`rem`)
    Rem,
    /// Exponentiaion (`**`)
    Exp,
    /// Bitwise/Logical And (`and`)
    And,
    /// Bitwise/Logical Or (`or`)
    Or,
    /// Bitwise/Logical Xor (`xor`)
    Xor,
    /// Bitshift Left (`shl`)
    Shl,
    /// Bitshift Right (`shr`)
    Shr,
    /// Implication (`=>`)
    Imply,
    /// Less Than (`<`)
    Lt,
    /// Less Than or Equal (`<=`)
    Le,
    /// Greater Than (`>`)
    Gt,
    /// Greater Than or Equal (`>=`)
    Ge,
    /// Equal (`=`)
    Equ,
    /// Not Equal (`~=`)
    NotEqu,
}

impl Operator {
    /// Converts a `TokenType` into the corresponding unary `Operator`
    fn from_unary(tok_type: frontend::token::TokenType) -> Self {
        use frontend::token::TokenType;

        match tok_type {
            TokenType::Plus => Operator::UnaryPlus,
            TokenType::Minus => Operator::UnaryMinus,
            TokenType::Pound => Operator::NatCheat,
            TokenType::Not => Operator::Not,
            TokenType::Caret => Operator::PointerDeref,
            _ => panic!("No conversion into a unary operator for '{}'", tok_type),
        }
    }

    /// Converts a `TokenType` into the corresponding binary `Operator`
    fn from_binary(tok_type: frontend::token::TokenType) -> Self {
        use frontend::token::TokenType;

        match tok_type {
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Sub,
            TokenType::Star => Operator::Mul,
            TokenType::Slash => Operator::RealDiv,
            TokenType::Div => Operator::IntDiv,
            TokenType::Mod => Operator::Mod,
            TokenType::Rem => Operator::Rem,
            TokenType::Exp => Operator::Exp,
            TokenType::And => Operator::And,
            TokenType::Or => Operator::Or,
            TokenType::Xor => Operator::Xor,
            TokenType::Shl => Operator::Shl,
            TokenType::Shr => Operator::Shr,
            TokenType::Imply => Operator::Imply,
            TokenType::Less => Operator::Lt,
            TokenType::LessEqu => Operator::Le,
            TokenType::Greater => Operator::Gt,
            TokenType::GreaterEqu => Operator::Ge,
            TokenType::Equ => Operator::Equ,
            TokenType::NotEqu => Operator::NotEqu,
            _ => panic!("No conversion into a binary operator for '{}'", tok_type),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::UnaryPlus => f.write_str("+"),
            Operator::UnaryMinus => f.write_str("-"),
            Operator::NatCheat => f.write_str("#"),
            Operator::Not => f.write_str("not"),
            Operator::PointerDeref => f.write_str("^"),
            Operator::Add => f.write_str("+"),
            Operator::Sub => f.write_str("-"),
            Operator::Mul => f.write_str("*"),
            Operator::RealDiv => f.write_str("/"),
            Operator::IntDiv => f.write_str("div"),
            Operator::Mod => f.write_str("mod"),
            Operator::Rem => f.write_str("rem"),
            Operator::Exp => f.write_str("exp"),
            Operator::And => f.write_str("and"),
            Operator::Or => f.write_str("or"),
            Operator::Xor => f.write_str("xor"),
            Operator::Shl => f.write_str("shl"),
            Operator::Shr => f.write_str("shr"),
            Operator::Imply => f.write_str("=>"),
            Operator::Lt => f.write_str("<"),
            Operator::Le => f.write_str("<="),
            Operator::Gt => f.write_str(">"),
            Operator::Ge => f.write_str(">="),
            Operator::Equ => f.write_str("="),
            Operator::NotEqu => f.write_str("~="),
        }
    }
}
