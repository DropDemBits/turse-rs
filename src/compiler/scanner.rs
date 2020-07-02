//! Scanner for tokens
use crate::status_reporter::StatusReporter;
use std::num::ParseIntError;
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
}

/// Parsed token
#[derive(Debug, PartialEq)]
pub struct Token {
    /// Type of the token
    token_type: TokenType,
    /// Location of the lexeme in the file/text stream
    location: Location,
}

/// Valid tokens in Turing
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Character Tokens
    /// &
    Ampersand,
    /// ^
    At,
    /// ->
    Deref,
    /// ^
    Caret,
    /// :
    Colon,
    /// :=
    Assign,
    /// ,
    Comma,
    /// .
    Dot,
    /// ..
    Range,
    /// =
    Equ,
    /// >=
    GreaterEqu,
    /// >
    Greater,
    /// #
    Pound,
    /// =>
    Imply,
    /// <=
    LessEqu,
    /// (
    LeftParen,
    /// <
    Less,
    /// -
    Dash,
    /// |
    Bar,
    /// +
    Plus,
    /// )
    RightParen,
    /// ;
    Semicolon,
    /// /
    Slash,
    /// *
    Star,
    /// **
    Exp,
    /// ~
    Tilde,

    // Packed Operator-Assign
    // These are complicated by the fact that whitespace does not matter
    // between the operator and the equal
    // However, these cases are handled by the parser.

    // Keywords
    Addressint,
    All,
    And,
    Array,
    Asm,
    Assert,
    Begin,
    Bind,
    Body,
    Boolean,
    By,
    Case,
    Char,
    Checked,
    Class,
    Close,
    Collection,
    Condition,
    Const,
    Decreasing,
    Def,
    Deferred,
    Div,
    Else,
    Elsif,
    End,
    Enum,
    Exit,
    Export,
    External,
    False,
    Fcn,
    Flexible,
    For,
    Fork,
    Forward,
    Free,
    Function,
    Get,
    Handler,
    If,
    Implement,
    Import,
    In,
    Include,
    Inherit,
    Init,
    Int,
    Int1,
    Int2,
    Int4,
    Invariant,
    Label,
    Loop,
    Mod,
    Module,
    Monitor,
    Nat,
    Nat1,
    Nat2,
    Nat4,
    New,
    Nil,
    Not,
    Of,
    Opaque,
    Open,
    Or,
    Packed,
    Pause,
    Pervasive,
    Pointer,
    Post,
    Pre,
    Priority,
    Proc,
    Procedure,
    Process,
    Put,
    Quit,
    Read,
    Real,
    Real4,
    Real8,
    Record,
    Register,
    Rem,
    Result_,
    Return,
    Seek,
    Set,
    Shl,
    Shr,
    Signal,
    Skip,
    String_,
    Tag,
    Tell,
    Then,
    Timeout,
    To,
    True,
    Type,
    Unchecked,
    Union,
    Unqualified,
    Var,
    Wait,
    When,
    Write,
    Xor,

    // Literals
    Identifier(String),
    CharLiteral(String),
    StringLiteral(String),
    IntLiteral(u64),
    RealLiteral(f64),

    // Other
    Eof,
}

/// Scanner for tokens
pub struct Scanner<'a> {
    /// Scanning source
    source: &'a str,
    /// Status reporter
    reporter: StatusReporter,
    /// Vector of scanned tokens
    pub tokens: Vec<Token>,
    /// Iterator for char indicies
    next_indicies: std::str::CharIndices<'a>,
    /// Iterator for chars
    chars: std::str::Chars<'a>,

    /// Next char in stream
    peek: char,
    /// Next next char in stream
    peek_ahead: char,

    /// Current Location of the scanner
    cursor: Location,
}

impl<'s> Scanner<'s> {
    pub fn new(source: &'s str) -> Self {
        let mut next_indicies = source.char_indices();
        let mut chars = source.chars();

        // Skip over first char
        next_indicies.next();

        let peek = chars.next().unwrap_or('\0');
        let peek_ahead = chars.next().unwrap_or('\0');

        Self {
            source,
            reporter: StatusReporter::new(),
            tokens: vec![],
            next_indicies,
            chars,
            peek,
            peek_ahead,
            cursor: Location::new(),
        }
    }

    /// Checks if the scan was successfully performed
    pub fn is_valid_scan(&self) -> bool {
        !self.reporter.has_error()
    }

    /// Scans the source input for all tokens
    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.cursor.step();
            self.scan_token();
        }
    }

    fn get_source_slice(&self, locate: &Location) -> &str {
        &self.source[locate.start..locate.end]
    }

    // Checks if the end of the stream has been reached
    fn is_at_end(&self) -> bool {
        // Not at the end if the last added token is not Eof, or there are no tokens
        match self.tokens.last() {
            Some(ref tok) => tok.token_type == TokenType::Eof,
            None => false,
        }
    }

    /// Grabs the next char in the text stream
    fn next_char(&mut self) -> char {
        // Advance the peeks
        let next_chr = self.peek;
        self.peek = self.peek_ahead;
        self.peek_ahead = self.chars.next().unwrap_or('\0');

        // Advance the cursor
        let (lexeme_end, _) = self
            .next_indicies
            .next()
            .unwrap_or((self.source.len(), '\0'));

        self.cursor.current_to(lexeme_end);

        next_chr
    }

    /// Tries to match the next char
    /// If a match was found, the next char is consumed
    fn match_next(&mut self, expected: char) -> bool {
        if !self.is_at_end() && self.peek == expected {
            // Matched char, nom the char
            self.next_char();
            true
        } else {
            false
        }
    }

    // Scan a single token
    fn scan_token(&mut self) {
        let chr = self.next_char();

        match chr {
            '\0' => self.make_token(TokenType::Eof, 0),
            // Whitespace
            ' ' | '\t' => self.cursor.columns(1),
            '\r' => {}
            '\n' => self.cursor.lines(1),
            // Meaningful tokens
            '(' => self.make_token(TokenType::LeftParen, 1),
            ')' => self.make_token(TokenType::RightParen, 1),
            '&' => self.make_token(TokenType::Ampersand, 1),
            '@' => self.make_token(TokenType::At, 1),
            '^' => self.make_token(TokenType::Caret, 1),
            ',' => self.make_token(TokenType::Comma, 1),
            '#' => self.make_token(TokenType::Pound, 1),
            '-' => self.make_or_default('>', TokenType::Deref, 2, TokenType::Dash, 1),
            '|' => self.make_token(TokenType::Bar, 1),
            '+' => self.make_token(TokenType::Plus, 1),
            ';' => self.make_token(TokenType::Semicolon, 1),
            '/' => self.make_token(TokenType::Slash, 1),
            '~' => self.make_token(TokenType::Tilde, 1),
            '=' => self.make_or_default('>', TokenType::Imply, 2, TokenType::Equ, 1),
            ':' => self.make_or_default('=', TokenType::Assign, 2, TokenType::Colon, 1),
            '>' => self.make_or_default('=', TokenType::GreaterEqu, 2, TokenType::Greater, 1),
            '<' => self.make_or_default('=', TokenType::LessEqu, 2, TokenType::Less, 1),
            '.' => self.make_or_default('.', TokenType::Range, 2, TokenType::Dot, 1),
            '*' => self.make_or_default('*', TokenType::Exp, 2, TokenType::Star, 1),
            '"' => self.make_char_sequence(true),
            '\'' => self.make_char_sequence(false),
            '0'..='9' => self.make_number(),
            _ => {
                if is_ident_char(chr) {
                    self.make_ident();
                } else {
                    self.reporter.report_error(
                        &self.cursor,
                        format_args!("Unrecognized character '{}'", chr),
                    );
                }
            }
        }
    }

    /// Makes a token and adds it to the token list
    /// Also steps the cursor's columns
    fn make_token(&mut self, token_type: TokenType, steps: usize) {
        self.tokens.push(Token {
            token_type,
            location: self.cursor.clone(),
        });

        self.cursor.columns(steps);
    }

    /// Makes the `does_match` token if the char matched, otherwise makes the `no_match` token
    /// Also steps with the appropriate token
    fn make_or_default(
        &mut self,
        expect: char,
        does_match: TokenType,
        step_match: usize,
        no_match: TokenType,
        step_no_match: usize,
    ) {
        if self.match_next(expect) {
            self.make_token(does_match, step_match);
        } else {
            self.make_token(no_match, step_no_match);
        }
    }

    fn make_number(&mut self) {
        // 3 main number formats
        // numeric+
        // numeric+ '#' alphanumeric+
        // numeric+ '.' (numeric+)? ([eE] numeric+)?

        // Go over main digits first
        while matches!(self.peek, '0'..='9') {
            self.next_char();
        }

        let next_char = self.peek;
        match next_char {
            '.' | 'e' | 'E' => self.make_number_real(),
            '#' => self.make_number_radix(),
            _ => self.make_number_basic(),
        }
    }

    fn make_number_basic(&mut self) {
        // End normal IntLiteral
        let numerals = self.get_source_slice(&self.cursor);
        let numerals_len = numerals.len();
        let value = numerals.parse::<u64>();

        match value {
            Ok(num) => {
                self.make_token(TokenType::IntLiteral(num), numerals_len);
            }
            Err(e) if e.to_string() == "number too large to fit in target type" => {
                // Too large
                self.reporter
                    .report_error(&self.cursor, format_args!("Integer literal is too large"));
            }
            Err(_) => {
                // Bad!
                self.reporter.report_error(
                    &self.cursor,
                    format_args!("Failed to parse integer literal"),
                );
            }
        }

        // Done
        return;
    }

    fn make_number_radix(&mut self) {
        // Base has already been parsed
        let base_numerals = self.get_source_slice(&self.cursor).to_string();
        // Nom the '#'
        self.next_char();

        // Go over the rest of the digits
        let mut radix_locate = self.cursor.clone();
        radix_locate.step();

        while self.peek.is_ascii_alphanumeric() {
            self.next_char();
        }

        // Select the rest of the radix digits
        radix_locate.current_to_other(&self.cursor);
        let radix_numerals = self
            .get_source_slice(&radix_locate)
            .to_string()
            .to_ascii_lowercase();

        // Parse as a u64
        let base = match try_parse_int(&base_numerals, 10) {
            Ok(num) => num,
            Err(k) => match k {
                IntErrKind::Overflow(_) => 0, // Same error message, out of range
                IntErrKind::InvalidDigit(e) | IntErrKind::Other(e) => panic!(
                    "Failed to parse base for integer literal at {:?} ({})",
                    &self.cursor, e
                ),
            },
        };

        // Check if the base is in range
        if base < 2 || base > 36 {
            self.reporter.report_error(
                &self.cursor,
                format_args!("Base for integer literal is not between the range of 2 - 36"),
            );

            return;
        }

        // Check if there are any numeral digits
        if radix_numerals.is_empty() {
            self.reporter.report_error(
                &self.cursor,
                format_args!("Missing digits for integer literal"),
            );
            return;
        }

        // Check if the range contains digits outside of the range

        match try_parse_int(&radix_numerals, base as u32) {
            Ok(num) => {
                let literal_len = self.get_source_slice(&self.cursor).len();
                self.make_token(TokenType::IntLiteral(num), literal_len);
            }
            Err(k) => match k {
                IntErrKind::Overflow(_) => {
                    self.reporter
                        .report_error(&self.cursor, format_args!("Integer literal is too large"));
                    return;
                }
                IntErrKind::InvalidDigit(_) => {
                    self.reporter.report_error(
                        &self.cursor,
                        format_args!("Digit in integer literal is outside of the specified base's allowed digits"),
                    );
                    return;
                }
                IntErrKind::Other(e) => panic!(
                    "Failed to parse base for integer literal at {:?} ({})",
                    &self.cursor, e
                ),
            },
        }
    }

    fn make_number_real(&mut self) {
        if self.peek == '.' {
            // First part of significand has already been parsed
            // Nom the '.'
            self.next_char();

            // Get the rest of the significand
            while matches!(self.peek, '0'..='9') {
                self.next_char();
            }
        }

        if self.peek == 'e' || self.peek == 'E' {
            // Nom the 'e'
            self.next_char();

            // Parse the exponent digits
            while matches!(self.peek, '0'..='9') {
                self.next_char();
            }
        }

        // Try to parse the value
        let digits = self.get_source_slice(&self.cursor);
        let digits_len = digits.len();
        let value = digits.parse::<f64>();
        match value {
            Ok(num) if num.is_infinite() => {
                self.reporter
                    .report_error(&self.cursor, format_args!("Real literal is too large"));
            }
            Ok(num) if num.is_nan() => {
                // Capture NaNs (What impl does)
                self.reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal"));
            }
            Err(e) if e.to_string() == "invalid float literal" => {
                self.reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal"));
            }
            Err(e) => eprintln!("{}", e.to_string()),
            Ok(num) => self.make_token(TokenType::RealLiteral(num), digits_len),
        }
    }

    fn make_char_sequence(&mut self, is_str_literal: bool) {
        let mut text_locate = self.cursor.clone();
        let ending_delimiter = if is_str_literal { '"' } else { '\'' };

        // Step over starting delimiter
        text_locate.step();

        // Keep going along the string until the end of the line, or the delimiter
        // TODO: Handle char escapes (eg \n)
        while self.peek != ending_delimiter && self.peek != '\n' && self.peek != '\0' {
            self.next_char();
        }

        // Get the width of the lexeme
        let lexeme = self.get_source_slice(&self.cursor);
        let part_width = UnicodeSegmentation::graphemes(lexeme, true).count();

        match self.peek {
            '\n' => {
                self.reporter.report_error(
                    &self.cursor,
                    format_args!("String literal ends at the end of the line"),
                );
                self.cursor.columns(part_width);

                return;
            }
            '\0' => {
                self.reporter.report_error(
                    &self.cursor,
                    format_args!("String literal ends at the end of the file"),
                );
                self.cursor.columns(part_width);

                return;
            }
            _ => {
                assert!((self.peek == '\'' || self.peek == '"'));

                // End the string
                text_locate.current_to_other(&self.cursor);

                // Consume other delimiter
                self.next_char();

                let text_slice = self.get_source_slice(&text_locate).to_string();

                // Make it! (Adjust part_width by 1 to account for ending delimiter)
                if is_str_literal {
                    self.make_token(TokenType::StringLiteral(text_slice), part_width + 1);
                } else {
                    self.make_token(TokenType::CharLiteral(text_slice), part_width + 1);
                }
            }
        }
    }

    fn make_ident(&mut self) {
        // Consume all of the identifier digits
        while is_ident_char_or_digit(self.peek) {
            self.next_char();
        }

        // Produce the identifier
        let ident_slice = self.get_source_slice(&self.cursor);
        let ident = ident_slice.to_string();
        let len = UnicodeSegmentation::graphemes(ident_slice, true).count();

        self.make_token(TokenType::Identifier(ident), len);
    }
}

/// Checks if the given `chr` is a valid identifier character
fn is_ident_char(chr: char) -> bool {
    chr.is_alphabetic() || chr == '_'
}

/// Checks if the given `chr` is a valid identifier character or digit
fn is_ident_char_or_digit(chr: char) -> bool {
    is_ident_char(chr) || chr.is_numeric()
}

// We're not in nightly, so we'll have to make our own type
enum IntErrKind {
    Overflow(ParseIntError),
    InvalidDigit(ParseIntError),
    Other(ParseIntError),
}

fn try_parse_int(digits: &str, base: u32) -> Result<u64, IntErrKind> {
    match u64::from_str_radix(&digits, base) {
        Ok(num) => Ok(num),
        // Ugly, but we're not using nightly builds
        Err(e) if e.to_string() == "number too large to fit in target type" => {
            Err(IntErrKind::Overflow(e))
        }
        Err(e) if e.to_string() == "invalid digit found in string" => {
            Err(IntErrKind::InvalidDigit(e))
        }
        Err(e) => Err(IntErrKind::Other(e)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_invalid_chars() {
        // Invalid chars (outside of strings) in the current format:
        // Control chars
        // '[' ']' '{' '}' '!' '$' '?' '`' '\\'
        for c in "[]{}!$?`\\".chars() {
            let s = c.to_string();
            let mut scanner = Scanner::new(&s);
            scanner.scan_tokens();

            if scanner.is_valid_scan() {
                panic!("Invalid char {} passed as valid", c);
            }
        }
    }

    #[test]
    fn test_identifier() {
        // Valid ident
        let mut scanner = Scanner::new("_source_text");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::Identifier("_source_text".to_string())
        );

        // Skip over first digits
        let mut scanner = Scanner::new("0_separate");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_ne!(
            scanner.tokens[0].token_type,
            TokenType::Identifier("0123_separate".to_string())
        );

        // Invalid ident
        let mut scanner = Scanner::new("ba$e");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());
    }

    #[test]
    fn test_int_literal_basic() {
        // Basic integer literal
        let mut scanner = Scanner::new("01234560");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens[0].token_type, TokenType::IntLiteral(1234560));

        // Overflow
        let mut scanner = Scanner::new("99999999999999999999");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Digit cutoff
        let mut scanner = Scanner::new("999a999");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens[0].token_type, TokenType::IntLiteral(999));
    }

    #[test]
    fn test_int_literal_radix() {
        // Integer literal with base
        let mut scanner = Scanner::new("16#EABC");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens[0].token_type, TokenType::IntLiteral(0xEABC));

        // Overflow
        let mut scanner = Scanner::new("10#99999999999999999999");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // No digits
        let mut scanner = Scanner::new("30#");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Out of range (> 36)
        let mut scanner = Scanner::new("37#asda");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Out of range (= 0)
        let mut scanner = Scanner::new("0#0000");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Out of range (= 1)
        let mut scanner = Scanner::new("1#0000");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Invalid digit
        let mut scanner = Scanner::new("10#999a999");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());
    }

    #[test]
    fn test_real_literal() {
        // Real Literal
        let mut scanner = Scanner::new("1.");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());

        let mut scanner = Scanner::new("100.00");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());

        let mut scanner = Scanner::new("100.00e10");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());

        let mut scanner = Scanner::new("100.00e100");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());

        let mut scanner = Scanner::new("1e100");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());

        // Invalid format
        let mut scanner = Scanner::new("1e");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Too big
        let mut scanner = Scanner::new("1e600");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());
    }

    #[test]
    fn test_string_literal() {
        // String literal parsing
        let mut scanner = Scanner::new("\"abcd💖\"a");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::StringLiteral("abcd💖".to_string())
        );

        // Validate width
        assert_eq!(scanner.tokens[1].location.column, 8);

        // Ends at the end of line
        let mut scanner = Scanner::new("\"abcd\n");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Ends at the end of file
        let mut scanner = Scanner::new("\"abcd");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());
    }

    #[test]
    fn test_char_literal() {
        // Char(n) literal parsing
        let mut scanner = Scanner::new("'abcd'");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::CharLiteral("abcd".to_string())
        );

        // Ends at the end of line
        let mut scanner = Scanner::new("'abcd\n");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());

        // Ends at the end of file
        let mut scanner = Scanner::new("'abcd");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());
    }
}
