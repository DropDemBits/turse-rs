//! Scanner for tokens
use crate::context::CompileContext;
use crate::token::{Token, TokenType};
use toc_core::Location;

use std::cell::RefCell;
use std::char;
use std::num::ParseIntError;
use std::rc::Rc;
use unicode_segmentation::UnicodeSegmentation;

extern crate strtod;

/// Scanner for tokens
#[derive(Debug)]
pub struct Scanner<'a> {
    /// Scanning source
    source: &'a str,
    /// Compile context of the scanner
    context: Rc<RefCell<CompileContext>>,
    /// Vector of scanned tokens
    pub tokens: Vec<Token<'a>>,
    /// Iterator for char indicies
    next_indicies: std::str::CharIndices<'a>,
    /// Iterator for chars
    chars: std::str::Chars<'a>,
    /// Current character in stream
    current: char,
    /// Next char in stream
    peek: char,

    /// Current Location of the scanner
    cursor: Location,
}

impl<'s> Scanner<'s> {
    /// Creates a scanner for the given source and `CompileContext`
    pub fn scan_source(source: &'s str, context: Rc<RefCell<CompileContext>>) -> Self {
        let mut next_indicies = source.char_indices();
        let mut chars = source.chars();

        // Skip over first char
        next_indicies.next();

        let current = chars.next().unwrap_or('\0');
        let peek = chars.next().unwrap_or('\0');

        Self {
            source,
            context,
            tokens: vec![],
            next_indicies,
            chars,
            current,
            peek,
            cursor: Location::new(),
        }
    }

    // Checks if the end of the stream has been reached
    fn is_at_end(&self) -> bool {
        self.cursor.end >= self.source.len()
    }

    /// Grabs the next char in the text stream.
    ///
    /// Also advances `cursor` to the char boundary between `current` and `peek`.
    fn next_char(&mut self) -> char {
        // Advance the peek
        self.current = self.peek;
        self.peek = self.chars.next().unwrap_or('\0');

        // Advance the cursor
        let (lexeme_end, _) = self
            .next_indicies
            .next()
            .unwrap_or((self.source.len(), '\0'));

        self.cursor.current_to(lexeme_end);

        self.current
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

    /// Skips over all whitespace, returning the first non-whitespace character,
    /// or `None` if at the end of the file
    fn skip_whitespace(&mut self) -> char {
        loop {
            match self.current {
                // Whitespace
                ' ' => self.cursor.columns(1),
                '\t' => self.cursor.columns(4), // By default, 1 tab = 4 spaces
                '\r' => {}
                '\n' => self.cursor.lines(1),
                '%' => self.skip_line_comment(),
                '/' => {
                    if self.match_next('*') {
                        // Skip over a block comment
                        self.skip_block_comment()
                    } else {
                        // Give back a slash
                        break self.current;
                    }
                }
                chr => break chr,
            }

            // Step over whitespace
            self.next_char();
            self.cursor.step();
        }
    }

    /// Scan the input source, producing a single token
    ///
    /// No token fusing occurs at this stage
    fn scan_token(&mut self) -> Option<Token<'s>> {
        let chr = self.skip_whitespace();

        if chr == '\0' {
            // End of file
            None
        } else {
            let token = match chr {
                // Meaningful tokens
                '(' => self.make_and_advance(TokenType::LeftParen),
                ')' => self.make_and_advance(TokenType::RightParen),
                '@' => self.make_and_advance(TokenType::At),
                '^' => self.make_and_advance(TokenType::Caret),
                ',' => self.make_and_advance(TokenType::Comma),
                '#' => self.make_and_advance(TokenType::Pound),
                '+' => self.make_and_advance(TokenType::Plus),
                ';' => self.make_and_advance(TokenType::Semicolon),
                '~' => self.make_and_advance(TokenType::Tilde),
                '&' => self.make_and_advance(TokenType::And),
                '|' => self.make_and_advance(TokenType::Or),
                '/' => self.make_and_advance(TokenType::Slash),
                '-' => self.make_or_default('>', TokenType::Arrow, TokenType::Minus),
                '=' => self.make_or_default('>', TokenType::Imply, TokenType::Equ),
                ':' => self.make_or_default('=', TokenType::Assign, TokenType::Colon),
                '>' => self.make_or_default('=', TokenType::GreaterEqu, TokenType::Greater),
                '<' => self.make_or_default('=', TokenType::LessEqu, TokenType::Less),
                '.' if matches!(self.peek, '0'..='9') => {
                    // `self.make_number_real` expects there to be a '.' or 'e' at self.current
                    self.make_number_real(self.cursor)
                }
                '.' => self.make_or_default('.', TokenType::Range, TokenType::Dot),
                '*' => self.make_or_default('*', TokenType::Exp, TokenType::Star),
                '"' => self.make_char_sequence(true),
                '\'' => self.make_char_sequence(false),
                '0'..='9' => self.make_number(),
                _ => {
                    if is_ident_char(chr) {
                        self.make_ident()
                    } else {
                        self.context.borrow_mut().reporter.report_error(
                            &self.cursor,
                            format_args!("Unrecognized character '{}'", chr),
                        );

                        // Step over invalid char
                        self.next_char();

                        // Create an error token
                        self.make_token(TokenType::Error, 1)
                    }
                }
            };

            Some(token)
        }
    }

    /// Makes a token
    /// Also steps the cursor's columns
    fn make_token(&mut self, token_type: TokenType, steps: usize) -> Token<'s> {
        self.cursor.columns(steps);

        Token::new(token_type, self.cursor)
    }

    /// Makes a token and steps over the current character
    /// Also steps the cursor's columns
    fn make_and_advance(&mut self, token_type: TokenType) -> Token<'s> {
        self.next_char();
        self.make_token(token_type, 1)
    }

    /// Makes the `does_match` token if the char matched, otherwise makes the `no_match` token
    ///
    /// Also steps over the component characters
    fn make_or_default(
        &mut self,
        expect: char,
        does_match: TokenType,
        no_match: TokenType,
    ) -> Token<'s> {
        let (production, steps) = if self.match_next(expect) {
            (does_match, 2)
        } else {
            (no_match, 1)
        };

        // Nom the unnom'd char
        self.next_char();
        self.make_token(production, steps)
    }

    /// Skips over a block comment
    ///
    /// Returns false if the block comment ends at the end of the file
    fn skip_block_comment(&mut self) {
        // Block comment parsing
        let mut depth: usize = 1;

        while depth > 0 {
            // Consume char
            self.next_char();

            match self.current {
                '\0' => {
                    self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Block comment (starting here) ends at the end of the file"),
                    );

                    // No more parsing, will be handled by the return from `skip_whitespace`
                    return;
                }
                '*' => {
                    if self.peek == '/' {
                        // Decrease depth and consume '*/'
                        self.next_char();
                        depth = depth.saturating_sub(1);
                    }
                }
                '/' => {
                    if self.peek == '*' {
                        // Increase depth and consume '/*'
                        self.next_char();
                        depth = depth.saturating_add(1);
                    }
                }
                '\n' => {
                    // End the line and rebase lexeme start to the beginning of the line
                    self.cursor.lines(1);
                    self.cursor.step();
                }
                _ => {}
            }
        }

        // Handle column stuff
        let remaining_comment = self.cursor.get_lexeme(self.source);
        let end_at_column = UnicodeSegmentation::graphemes(remaining_comment, true).count();
        self.cursor.columns(end_at_column);
    }

    /// Skips over a line comment
    fn skip_line_comment(&mut self) {
        // Line comment
        while self.peek != '\n' && !self.is_at_end() {
            // Nom all the chars
            self.next_char();
        }
    }

    /// Scans a number literal
    fn make_number(&mut self) -> Token<'s> {
        // 3 main number formats
        // numeric+
        // numeric+ '#' alphanumeric+
        // numeric* '.' (numeric+)? ([eE] numeric+)?

        // Go over main digits first
        while matches!(self.current, '0'..='9') {
            self.next_char();
        }

        // Grab the base numerals before continuing
        let base_numerals = self.cursor;

        match self.current {
            '.' if self.peek != '.' => {
                // Don't consume a TokenType::Range accidentally
                self.make_number_real(base_numerals)
            }
            'e' | 'E' => self.make_number_real(base_numerals),
            '#' => self.make_number_radix(base_numerals),
            // No nom as `self.peek` may not be a valid base 10 numeral
            _ => self.make_number_basic(base_numerals),
        }
    }

    /// Scans a plain old `NatLiteral`, only accepting base 10 digits
    fn make_number_basic(&mut self, numerals: Location) -> Token<'s> {
        // End normal NatLiteral
        let numerals = numerals.get_lexeme(self.source);
        let numerals_len = numerals.len();

        match try_parse_int(numerals, 10) {
            Ok(num) => self.make_token(TokenType::NatLiteral(num), numerals_len),
            Err(e) => {
                match e {
                    IntErrKind::Overflow(_) => self
                        .context
                        .borrow_mut()
                        .reporter
                        .report_error(&self.cursor, format_args!("Integer literal is too large")),
                    IntErrKind::InvalidDigit(_) => self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Invalid digit found for a base 10 number"),
                    ),
                    IntErrKind::Other(e) => self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Failed to parse integer literal ({})", e),
                    ),
                }

                // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
                self.make_token(TokenType::NatLiteral(0), numerals_len)
            }
        }

        // Done
    }

    /// Scans an integer literal with a customizable base (between 2 - 36)
    fn make_number_radix(&mut self, base_numerals: Location) -> Token<'s> {
        // Nom '#'
        self.next_char();

        // Base has already been parsed
        let base_numerals = base_numerals.get_lexeme(self.source).to_string();

        // Go over the rest of the digits
        let mut radix_cursor = self.cursor;
        radix_cursor.step();

        while self.current.is_ascii_alphanumeric() {
            self.next_char();
        }

        // Select the rest of the radix digits
        radix_cursor.current_to_other(&self.cursor);
        let radix_numerals = radix_cursor
            .get_lexeme(self.source)
            .to_string()
            .to_ascii_lowercase();

        // Parse as a u64
        let base = match try_parse_int(&base_numerals, 10) {
            Ok(num) => {
                if num < 2 || num > 36 {
                    self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Base for integer literal is not between the range of 2 - 36"),
                    );

                    None
                } else {
                    // Valid parse
                    Some(num)
                }
            }
            Err(k) => {
                match k {
                    IntErrKind::Overflow(_) => {
                        self.context.borrow_mut().reporter.report_error(
                            &self.cursor,
                            format_args!(
                                "Base for integer literal is not between the range of 2 - 36"
                            ),
                        );
                    }
                    IntErrKind::InvalidDigit(_) => {
                        self.context.borrow_mut().reporter.report_error(
                            &self.cursor,
                            format_args!("Invalid digit found in the base specifier"),
                        );
                    } // Notify!
                    IntErrKind::Other(e) => self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Failed to parse base for integer literal ({})", e),
                    ),
                }

                None
            }
        };

        // Check if the base is in range
        if base.is_none() {
            // Error has been reported above
            // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
            return self.make_token(TokenType::NatLiteral(0), base_numerals.len());
        }

        let base = base.unwrap();

        // Check if there are any numeral digits
        if radix_numerals.is_empty() {
            self.context.borrow_mut().reporter.report_error(
                &self.cursor,
                format_args!("Missing digits for integer literal"),
            );

            // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
            return self.make_token(TokenType::NatLiteral(0), base_numerals.len());
        }

        // Check if the range contains digits outside of the range

        match try_parse_int(&radix_numerals, base as u32) {
            Ok(num) => {
                let literal_len = self.cursor.get_lexeme(self.source).len();
                self.make_token(TokenType::NatLiteral(num), literal_len)
            }
            Err(k) => {
                match k {
                    IntErrKind::Overflow(_) => {
                        self.context.borrow_mut().reporter.report_error(
                            &self.cursor,
                            format_args!("Integer literal is too large"),
                        );
                    }
                    IntErrKind::InvalidDigit(_) => {
                        self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Digit in integer literal is outside of the specified base's allowed digits"),
                    );
                    }
                    IntErrKind::Other(e) => self.context.borrow_mut().reporter.report_error(
                        &self.cursor,
                        format_args!("Failed to parse base for integer literal ({})", e),
                    ),
                }
                // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
                self.make_token(TokenType::NatLiteral(0), base_numerals.len())
            }
        }
    }

    /// Makes a `RealLiteral`, complete with parsing decimal digits and scientific notation exponents
    fn make_number_real(&mut self, numerals: Location) -> Token<'s> {
        // First part of significand has already been parsed

        if self.current == '.' {
            // Nom '.'
            self.next_char();

            // Get the rest of the significand
            while matches!(self.current, '0'..='9') {
                self.next_char();
            }
        }

        let requires_exponent_digits = if matches!(self.current, 'e' | 'E') {
            // Nom 'e'
            self.next_char();

            if matches!(self.current, '-' | '+') {
                // Consume exponent sign
                self.next_char();
            }

            // Parse the exponent digits
            let mut found_exponent_digits = false;

            while matches!(self.current, '0'..='9') {
                self.next_char();
                found_exponent_digits = true;
            }

            // Required, see if any were found
            Some(found_exponent_digits)
        } else {
            // No digits required
            None
        };

        // Try to parse the value
        let mut numerals = numerals;
        numerals.current_to_other(&self.cursor);
        let digits = numerals.get_lexeme(self.source);
        let digits_len = digits.len();
        let value = strtod::strtod(digits);

        // Setup the token width
        self.cursor.columns(digits_len);

        // A value is always produced
        let parsed_value = match value {
            Some(num) if num.is_infinite() => {
                self.context
                    .borrow_mut()
                    .reporter
                    .report_error(&self.cursor, format_args!("Real literal is too large"));
                0f64
            }
            Some(num) if num.is_nan() => {
                // Capture NaNs (What impl does)
                self.context
                    .borrow_mut()
                    .reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal (is NaN)"));
                0f64
            }
            Some(_num) if requires_exponent_digits.eq(&Some(false)) => {
                // Missing exponent digits
                self.context
                    .borrow_mut()
                    .reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal"));
                0f64
            }
            None => {
                self.context
                    .borrow_mut()
                    .reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal"));
                0f64
            }
            Some(num) => num,
        };

        self.make_token(TokenType::RealLiteral(parsed_value), 0)
    }

    /// Makes a char sequence, handling all of the escapes and stuff
    fn make_char_sequence(&mut self, is_str_literal: bool) -> Token<'s> {
        // Step over starting delimeter
        self.next_char();

        let ending_delimiter = if is_str_literal { '"' } else { '\'' };
        let literal_text = self.extract_char_sequence(ending_delimiter);

        // Get lexeme (the entire text including the starting delimiter) and lexeme width
        let lexeme = self.cursor.get_lexeme(self.source);
        let lexeme_partial_width = UnicodeSegmentation::graphemes(lexeme, true).count();

        // Advance column width to the correct location
        self.cursor.columns(lexeme_partial_width);

        // Get width adjustment based on ending delimeter
        let width_adjust = match self.current {
            '\r' | '\n' => {
                self.context.borrow_mut().reporter.report_error(
                    &self.cursor,
                    format_args!("String literal ends at the end of the line"),
                );

                0
            }
            '\0' => {
                self.context.borrow_mut().reporter.report_error(
                    &self.cursor,
                    format_args!("String literal ends at the end of the file"),
                );

                0
            }
            _ => {
                assert!(matches!(self.current, '\'' | '"'));

                // Consume other delimiter
                self.next_char();

                // Adjust width by 1 to account for ending delimiter
                1
            }
        };

        // Make it!
        let kind = if is_str_literal {
            TokenType::StringLiteral(literal_text)
        } else {
            TokenType::CharLiteral(literal_text)
        };

        self.make_token(kind, width_adjust)
    }

    /// Extracts the character sequence from the source, handling escape sequences
    fn extract_char_sequence(&mut self, ending_delimiter: char) -> String {
        let mut literal_text = String::with_capacity(256);

        // Note: Depending on the VM settings, this text may either be interpreted in
        // Turing's main character encodings (Windows-1252 and IBM / Code Page 437),
        // or as UTF-8 characters. Neither the scanner nor the compiler in general do
        // not have to deal with the character encoding nonsense, so all
        // characters are treated as if they were all UTF-8 characters.
        //
        // While this handling may cause issues when running the compiled version in
        // the original TProlog (via compilation to *.tbc), this should not be a major
        // issue as *.tbc compilation is more of a fun experiment rather than a major
        // feature.
        // If compatibility with TProlog is desired, the code generator can also solve
        // this issue by lossy converting the UTF-8 strings into ASCII strings.

        // Keep going along the string until the end of the line, or the delimiter
        while self.current != ending_delimiter && !matches!(self.current, '\r' | '\n' | '\0') {
            let chr = self.current;
            // Always nom on character
            self.next_char();

            match chr {
                '^' | '\\' if matches!(self.current, '\r' | '\n' | '\0') => break, // Reached the end of the literal
                '\\' => self.scan_slash_escape(&mut literal_text),
                '^' => self.scan_caret_escape(&mut literal_text),
                current => literal_text.push(current),
            }
        }

        literal_text
    }

    /// Scans a slash escape, pushing the escaped character into `literal_text`
    fn scan_slash_escape(&mut self, literal_text: &mut String) {
        // Parse escape character
        let escaped = self.current;
        let escape_start = self.cursor;

        // Character will always be nom'd (this noms the char after the slash)
        self.next_char();

        match escaped {
            '\'' => literal_text.push('\''),
            '"' => literal_text.push('"'),
            '\\' => literal_text.push('\\'),
            'b' | 'B' => literal_text.push('\x08'),
            'd' | 'D' => literal_text.push('\x7F'),
            'e' | 'E' => literal_text.push('\x1B'),
            'f' | 'F' => literal_text.push('\x0C'),
            'r' | 'R' => literal_text.push('\r'),
            'n' | 'N' => literal_text.push('\n'),
            't' | 'T' => literal_text.push('\t'),
            '^' => literal_text.push('^'), // Unescaped version is parsed in Caret Notation
            '0'..='7' => {
                // Octal str, {1-3}, 0 - 377
                let mut octal_cursor = escape_start;

                // Start at the first digit
                octal_cursor.step();

                // Nom the rest of the octal digits
                // First one already has been nom'd
                for _ in 1..3 {
                    if !matches!(self.current, '0'..='7') {
                        break;
                    }

                    self.next_char();
                    octal_cursor.columns(1);
                }

                // Select the octal digits
                octal_cursor.current_to_other(&self.cursor);

                let to_chr = u16::from_str_radix(octal_cursor.get_lexeme(self.source), 8)
                    .expect("Octal escape parsing is infalliable");

                // Check if the parsed character is in range
                if to_chr >= 256 {
                    self.context.borrow_mut().reporter.report_error(
                        &octal_cursor,
                        format_args!(
                            "Octal character value is larger than 255 (octal 377), value is {} ({})",
                            to_chr,
                            octal_cursor.get_lexeme(self.source)
                        ),
                    );

                    literal_text.push(char::REPLACEMENT_CHARACTER);
                } else {
                    // Truncated into 0 - 255 range since that's the max that octal should support
                    literal_text.push((to_chr as u8) as char);
                }
            }
            'x' if self.current.is_ascii_hexdigit() => {
                // Hex sequence, {1-2} digits
                let mut hex_cursor = self.cursor;

                // Start at the first digit
                hex_cursor.step();

                // Nom all of the hex digits
                for _ in 0..2 {
                    if !self.current.is_ascii_hexdigit() {
                        break;
                    }

                    self.next_char();
                    hex_cursor.columns(1);
                }

                // Select the hex digits
                hex_cursor.current_to_other(&self.cursor);

                let to_chr = u8::from_str_radix(hex_cursor.get_lexeme(self.source), 16)
                    .expect("Hex escape parsing is infalliable");

                // Push the parsed char
                literal_text.push(to_chr as char);
            }
            'u' | 'U' if self.current.is_ascii_hexdigit() => {
                // u: unicode character {4-8} `char::REPLACEMENT_CHARACTER` if out of range
                let mut hex_cursor = self.cursor;

                // Start at the first digit
                hex_cursor.step();

                // Nom all of the hex digits
                for _ in 0..8 {
                    if !self.current.is_ascii_hexdigit() {
                        break;
                    }

                    self.next_char();
                    hex_cursor.columns(1);
                }

                // Select the hex digits
                hex_cursor.current_to_other(&self.cursor);

                let to_chr = u32::from_str_radix(hex_cursor.get_lexeme(self.source), 16)
                    .expect("Unicode escape parsing is infalliable");

                // Check if the parsed char is in range and not a surrogate character
                if to_chr > 0x10FFFF {
                    self.context.borrow_mut().reporter.report_error(
                        &hex_cursor,
                        format_args!("Unicode codepoint value is greater than U+10FFFF"),
                    );
                    literal_text.push(char::REPLACEMENT_CHARACTER);
                } else if to_chr >= 0xD800 && to_chr <= 0xDFFF {
                    self.context.borrow_mut().reporter.report_error(
                        &hex_cursor,
                        format_args!(
                            "Surrogate codepoints (paired or unpaired) are not allowed in strings"
                        ),
                    );
                    literal_text.push(char::REPLACEMENT_CHARACTER);
                } else {
                    // Push the parsed char
                    literal_text.push(char::from_u32(to_chr).unwrap());
                }
            }
            _ => {
                // Fetch the location
                let mut bad_escape = escape_start;

                // Select the escape sequence
                bad_escape.step();
                bad_escape.current_to_other(&self.cursor);
                // Adjust everything so that the lexeme lines up with the escape sequence
                bad_escape.start -= 1;
                bad_escape.column += 1;
                bad_escape.width = 2;

                match escaped {
                    'x' | 'u' | 'U' => {
                        // Missing the hex digits
                        self.context.borrow_mut().reporter.report_error(
                            &bad_escape,
                            format_args!(
                                "Invalid escape sequence character '{}' (missing hexadecimal digits after the '{}')",
                                bad_escape.get_lexeme(self.source), escaped
                            ),
                        );
                    }
                    _ => {
                        // Bog-standard error report
                        self.context.borrow_mut().reporter.report_error(
                            &bad_escape,
                            format_args!(
                                "Invalid escape sequence character '{}'",
                                bad_escape.get_lexeme(self.source)
                            ),
                        );
                    }
                }

                // Add escaped to the string
                literal_text.push(escaped);
                // Skip over it
                self.next_char();
            }
        }
    }

    /// Scans a caret escape, pushing the escaped character into `literal_text`
    fn scan_caret_escape(&mut self, literal_text: &mut String) {
        // Parse caret notation
        // ASCII character range from '@' to '_', includes '?' (DEL)
        let escaped = self.current;
        match escaped {
            '@'..='_' | 'a'..='z' => {
                let parsed = (escaped.to_ascii_uppercase() as u8) & 0x1F;
                literal_text.push(parsed as char);
            }
            '?' => {
                // As the DEL char
                literal_text.push('\x7F');
            }
            _ => {
                // Unless the user knows what they are doing, they are likely to not intend for the ^ character to be parsed as the beginning of a caret sequence
                // Notify the user with this situation
                // Fetch the location
                let mut bad_escape = self.cursor;
                self.next_char();

                // Select the escape sequence
                bad_escape.step();
                bad_escape.current_to_other(&self.cursor);
                // Adjust everything so that the lexeme lines up with the escape sequence
                bad_escape.start -= 1;
                bad_escape.column += 1;
                bad_escape.width = 2;

                self.context.borrow_mut().reporter.report_error(
                    &bad_escape,
                    format_args!(
                        "Unknown caret notation sequence '{}' (did you mean to escape the caret by typing '\\^'?)",
                        bad_escape.get_lexeme(self.source)
                    ),
                );

                // Add as is
                literal_text.push(escaped);
            }
        }

        // Consume the character
        self.next_char();
    }

    /// Makes an identifier
    fn make_ident(&mut self) -> Token<'s> {
        // Consume all of the identifier digits
        while is_ident_char_or_digit(self.current) {
            self.next_char();
        }

        let ident_cursor = self.cursor;

        // Produce the identifier
        let ident_slice = ident_cursor.get_lexeme(self.source);
        let len = UnicodeSegmentation::graphemes(ident_slice, true).count();

        let token_type = match ident_slice {
            "addressint" => TokenType::Addressint,
            "all" => TokenType::All,
            "and" => TokenType::And,
            "array" => TokenType::Array,
            "asm" => TokenType::Asm,
            "assert" => TokenType::Assert,
            "begin" => TokenType::Begin,
            "bind" => TokenType::Bind,
            "bits" => TokenType::Bits,
            "body" => TokenType::Body,
            "boolean" => TokenType::Boolean,
            "break" => TokenType::Break,
            "by" => TokenType::By,
            "case" => TokenType::Case,
            "char" => TokenType::Char,
            "cheat" => TokenType::Cheat,
            "checked" => TokenType::Checked,
            "class" => TokenType::Class,
            "close" => TokenType::Close,
            "collection" => TokenType::Collection,
            "condition" => TokenType::Condition,
            "const" => TokenType::Const,
            "decreasing" => TokenType::Decreasing,
            "def" => TokenType::Def,
            "deferred" => TokenType::Deferred,
            "div" => TokenType::Div,
            "elif" => TokenType::Elif,
            "else" => TokenType::Else,
            "elseif" => TokenType::Elseif,
            "elsif" => TokenType::Elsif,
            "end" => TokenType::End,
            "endcase" => TokenType::EndCase,
            "endfor" => TokenType::EndFor,
            "endif" => TokenType::EndIf,
            "endloop" => TokenType::EndLoop,
            "enum" => TokenType::Enum,
            "exit" => TokenType::Exit,
            "export" => TokenType::Export,
            "external" => TokenType::External,
            "false" => TokenType::False,
            "fcn" | "function" => TokenType::Function,
            "flexible" => TokenType::Flexible,
            "for" => TokenType::For,
            "fork" => TokenType::Fork,
            "forward" => TokenType::Forward,
            "free" => TokenType::Free,
            "get" => TokenType::Get,
            "handler" => TokenType::Handler,
            "if" => TokenType::If,
            "implement" => TokenType::Implement,
            "import" => TokenType::Import,
            "in" => TokenType::In,
            "include" => TokenType::Include,
            "inherit" => TokenType::Inherit,
            "init" => TokenType::Init,
            "int" => TokenType::Int,
            "int1" => TokenType::Int1,
            "int2" => TokenType::Int2,
            "int4" => TokenType::Int4,
            "invariant" => TokenType::Invariant,
            "label" => TokenType::Label,
            "loop" => TokenType::Loop,
            "mod" => TokenType::Mod,
            "module" => TokenType::Module,
            "monitor" => TokenType::Monitor,
            "nat" => TokenType::Nat,
            "nat1" => TokenType::Nat1,
            "nat2" => TokenType::Nat2,
            "nat4" => TokenType::Nat4,
            "new" => TokenType::New,
            "nil" => TokenType::Nil,
            "not" => TokenType::Not,
            "objectclass" => TokenType::ObjectClass,
            "of" => TokenType::Of,
            "opaque" => TokenType::Opaque,
            "open" => TokenType::Open,
            "or" => TokenType::Or,
            "packed" => TokenType::Packed,
            "pause" => TokenType::Pause,
            "pervasive" => TokenType::Pervasive,
            "pointer" => TokenType::Pointer,
            "post" => TokenType::Post,
            "pre" => TokenType::Pre,
            "priority" => TokenType::Priority,
            "proc" | "procedure" => TokenType::Procedure,
            "process" => TokenType::Process,
            "put" => TokenType::Put,
            "quit" => TokenType::Quit,
            "read" => TokenType::Read,
            "real" => TokenType::Real,
            "real4" => TokenType::Real4,
            "real8" => TokenType::Real8,
            "record" => TokenType::Record,
            "register" => TokenType::Register,
            "rem" => TokenType::Rem,
            "result" => TokenType::Result_,
            "return" => TokenType::Return,
            "seek" => TokenType::Seek,
            "self" => TokenType::Self_,
            "set" => TokenType::Set,
            "shl" => TokenType::Shl,
            "shr" => TokenType::Shr,
            "signal" => TokenType::Signal,
            "skip" => TokenType::Skip,
            "string" => TokenType::String_,
            "tag" => TokenType::Tag,
            "tell" => TokenType::Tell,
            "then" => TokenType::Then,
            "timeout" => TokenType::Timeout,
            "to" => TokenType::To,
            "true" => TokenType::True,
            "type" => TokenType::Type,
            "unchecked" => TokenType::Unchecked,
            "union" => TokenType::Union,
            "unqualified" => TokenType::Unqualified,
            "var" => TokenType::Var,
            "wait" => TokenType::Wait,
            "when" => TokenType::When,
            "write" => TokenType::Write,
            "xor" => TokenType::Xor,
            _ => TokenType::Identifier,
        };

        self.make_token(token_type, len)
    }
}

impl<'s> std::iter::Iterator for Scanner<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.step();
        self.scan_token()
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

/// Tries to parse an int, giving back one of the above errors.
///
/// Just a wrapper around `u64::from_str_radix`.
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

    fn make_scanner(
        source: &str,
    ) -> (
        impl std::iter::Iterator<Item = Token<'_>> + '_,
        Rc<RefCell<CompileContext>>,
    ) {
        let context = Rc::new(RefCell::new(CompileContext::new()));
        (Scanner::scan_source(source, context.clone()), context)
    }

    #[test]
    fn test_invalid_chars() {
        // Invalid chars (outside of strings) in the current format:
        // Control chars
        // '[' ']' '{' '}' '!' '$' '?' '`' '\\'
        // Any non-ascii character (for now)
        for c in r#"[]{}!$?`\🧑‍🔬"#.chars() {
            let mut s = c.to_string();
            s.push('a');

            let (mut scanner, context) = make_scanner(&s);

            assert_eq!(
                scanner.next().unwrap().token_type,
                TokenType::Error,
                "No token produced for '{}'",
                c
            );
            assert_eq!(
                scanner.next().unwrap().location.column,
                2,
                "Column not advanced over '{}'",
                c
            );
            assert!(
                context.borrow().reporter.has_error(),
                "Error not reported for {}",
                c
            );
        }
    }

    #[test]
    fn test_identifier() {
        // Valid ident
        let source = "_source_text";
        let (scanner, context) = make_scanner(source);
        let mut scanner = scanner.peekable();

        assert_eq!(scanner.peek().unwrap().token_type, TokenType::Identifier);
        assert_eq!(scanner.peek().unwrap().location.get_lexeme(source), source);
        assert!(!context.borrow().reporter.has_error());

        // Skip over first digits
        let source = "0123_separate";
        let (scanner, context) = make_scanner(source);
        let mut scanner = scanner.peekable();

        assert_ne!(scanner.peek().unwrap().token_type, TokenType::Identifier);
        assert_ne!(
            scanner.peek().unwrap().location.get_lexeme(source),
            "0123_separate"
        );
        assert!(!context.borrow().reporter.has_error());

        // Invalid character, but "ba" should still be parsed
        let source = "ba$e";
        let (scanner, context) = make_scanner(source);
        let mut scanner = scanner.peekable();

        assert_eq!(scanner.next().unwrap().location.get_lexeme(source), "ba");
        assert_eq!(scanner.next().unwrap().location.get_lexeme(source), "$");
        assert_eq!(scanner.peek().unwrap().location.get_lexeme(source), "e");
        // Column check for invalid characters
        assert_eq!(scanner.peek().unwrap().location.column, 4);
        assert!(context.borrow().reporter.has_error());
    }

    #[test]
    fn test_int_literal_basic() {
        // Basic integer literal
        let (mut scanner, context) = make_scanner("01234560");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::NatLiteral(1234560)
        );
        assert!(!context.borrow().reporter.has_error());

        // Overflow
        let (mut scanner, context) = make_scanner("99999999999999999999");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // Digit cutoff
        let (mut scanner, context) = make_scanner("999a999");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::NatLiteral(999)
        );
        assert!(!context.borrow().reporter.has_error());
    }

    #[test]
    fn test_int_literal_radix() {
        // Integer literal with base
        let (mut scanner, context) = make_scanner("16#EABC");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::NatLiteral(0xEABC)
        );
        assert!(!context.borrow().reporter.has_error());

        // Overflow
        let (mut scanner, context) = make_scanner("10#99999999999999999999");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // No digits
        let (mut scanner, context) = make_scanner("30#");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // Out of range (> 36)
        let (mut scanner, context) = make_scanner("37#asda");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // Out of range (= 0)
        let (mut scanner, context) = make_scanner("0#0000");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // Out of range (= 1)
        let (mut scanner, context) = make_scanner("1#0000");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // Out of range (= overflow)
        let (mut scanner, context) = make_scanner("18446744073709551616#0000");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());

        // Invalid digit
        let (mut scanner, context) = make_scanner("10#999a999");
        // Should still produce a token
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(0));
        assert!(context.borrow().reporter.has_error());
    }

    #[test]
    fn test_real_literal() {
        // NOTE: May need to use Epsilon comparison if this test fails on other machines & operating systems
        // Real Literal
        let (mut scanner, context) = make_scanner("1.");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(1.0)
        );
        assert!(!context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("100.00");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(100.00)
        );
        assert!(!context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("100.00e10");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(100.00e10)
        );
        assert!(!context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("100.00e100");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(100.00e100)
        );
        assert!(!context.borrow().reporter.has_error());

        // Negative and positive exponents are valid
        let (mut scanner, context) = make_scanner("100.00e-100");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(100.00e-100)
        );
        assert!(!context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("100.00e+100");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(100.00e+100)
        );
        assert!(!context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("1e100");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(1e100)
        );
        assert!(!context.borrow().reporter.has_error());

        // Invalid format
        let (mut scanner, context) = make_scanner("1e");
        // Should still produce a value
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(0f64)
        );
        assert!(context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("1e-");
        // Should still produce a value
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(0f64)
        );
        assert!(context.borrow().reporter.has_error());

        let (mut scanner, context) = make_scanner("1e--2");
        // Should still produce a value
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(0f64)
        );
        assert!(context.borrow().reporter.has_error());

        // Too big
        let (mut scanner, context) = make_scanner("1e600");
        // Should still produce a value
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(0f64)
        );
        assert!(context.borrow().reporter.has_error());

        // Allow for leading dot
        let (mut scanner, context) = make_scanner(".12345");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(0.12345f64)
        );
        assert!(!context.borrow().reporter.has_error());

        // The following tests are from strtod_test.toml
        // See https://github.com/ahrvoje/numerics/blob/master/strtod/strtod_tests.toml

        let conversion_tests = [
            ("C21", 0x000fffffffffffff_u64, "2.225073858507201136057409796709131975934819546351645648023426109724822222021076945516529523908135087914149158913039621106870086438694594645527657207407820621743379988141063267329253552286881372149012981122451451889849057222307285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332647565300009245888316433037779791869612049497390377829704905051080609940730262937128958950003583799967207254304360284078895771796150945516748243471030702609144621572289880258182545180325707018860872113128079512233426288368622321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339850886445804001034933970427567186443383770486037861622771738545623065874679014086723327636718749999999999999999999999999999999999999e-308"),
            ("C22", 0x0010000000000000_u64, "2.22507385850720113605740979670913197593481954635164564802342610972482222202107694551652952390813508791414915891303962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230728525513315575501591439747639798341180199932396254828901710708185069063066665599493827577257201576306269066333264756530000924588831643303777979186961204949739037782970490505108060994073026293712895895000358379996720725430436028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862232150377566662250398253433597456888442390026549819838548794829220689472168983109969836584681402285424333066033985088644580400103493397042756718644338377048603786162277173854562306587467901408672332763671875e-308"),
            ("C23", 0x0010000000000000_u64, "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222507385850720138309023271733240406421921598046233183055332741688720443481391819585428315901251102056406733973103581100515243416155346010885601238537771882113077799353200233047961014744258363607192156504694250373420837525080665061665815894872049117996859163964850063590877011830487479978088775374994945158045160505091539985658247081864511353793580499211598108576605199243335211435239014879569960959128889160299264151106346631339366347758651302937176204732563178148566435087212282863764204484681140761391147706280168985324411002416144742161856716615054015428508471675290190316132277889672970737312333408698898317506783884692609277397797285865965494109136909540613646756870239867831529068098461721092462539672851562500000000000000001"),
            ("C25", 0x7fefffffffffffff_u64, "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497791.9999999999999999999999999999999999999999999999999999999999999999999999"),
            ("C29", 0x0000000000000000_u64, "2.47032822920623272e-324"),
            ("C37", 0x0000000008000000_u64, "6.631236871469758276785396630275967243399099947355303144249971758736286630139265439618068200788048744105960420552601852889715006376325666595539603330361800519107591783233358492337208057849499360899425128640718856616503093444922854759159988160304439909868291973931426625698663157749836252274523485312442358651207051292453083278116143932569727918709786004497872322193856150225415211997283078496319412124640111777216148110752815101775295719811974338451936095907419622417538473679495148632480391435931767981122396703443803335529756003353209830071832230689201383015598792184172909927924176339315507402234836120730914783168400715462440053817592702766213559042115986763819482654128770595766806872783349146967171293949598850675682115696218943412532098591327667236328125E-316"),
            ("C38", 0x0000000000010000_u64, "3.237883913302901289588352412501532174863037669423108059901297049552301970670676565786835742587799557860615776559838283435514391084153169252689190564396459577394618038928365305143463955100356696665629202017331344031730044369360205258345803431471660032699580731300954848363975548690010751530018881758184174569652173110473696022749934638425380623369774736560008997404060967498028389191878963968575439222206416981462690113342524002724385941651051293552601421155333430225237291523843322331326138431477823591142408800030775170625915670728657003151953664260769822494937951845801530895238439819708403389937873241463484205608000027270531106827387907791444918534771598750162812548862768493201518991668028251730299953143924168545708663913273994694463908672332763671875E-319"),
            ("C39", 0x0000800000000100_u64, "6.953355807847677105972805215521891690222119817145950754416205607980030131549636688806115726399441880065386399864028691275539539414652831584795668560082999889551357784961446896042113198284213107935110217162654939802416034676213829409720583759540476786936413816541621287843248433202369209916612249676005573022703244799714622116542188837770376022371172079559125853382801396219552418839469770514904192657627060319372847562301074140442660237844114174497210955449896389180395827191602886654488182452409583981389442783377001505462015745017848754574668342161759496661766020028752888783387074850773192997102997936619876226688096314989645766000479009083731736585750335262099860150896718774401964796827166283225641992040747894382698751809812609536720628966577351093292236328125E-310"),
            ("C40", 0x0000000000010800_u64, "3.339068557571188581835713701280943911923401916998521771655656997328440314559615318168849149074662609099998113009465566426808170378434065722991659642619467706034884424989741080790766778456332168200464651593995817371782125010668346652995912233993254584461125868481633343674905074271064409763090708017856584019776878812425312008812326260363035474811532236853359905334625575404216060622858633280744301892470300555678734689978476870369853549413277156622170245846166991655321535529623870646888786637528995592800436177901746286272273374471701452991433047257863864601424252024791567368195056077320885329384322332391564645264143400798619665040608077549162173963649264049738362290606875883456826586710961041737908872035803481241600376705491726170293986797332763671875E-319"),
            ("C64", 0x0000000000000000_u64, "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328124999e-324"),
            ("C65", 0x0000000000000000_u64, "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125e-324"),
            ("C66", 0x0000000000000001_u64, "2.4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772285886546332835517796989819938739800539093906315035659515570226392290858392449105184435931802849936536152500319370457678249219365623669863658480757001585769269903706311928279558551332927834338409351978015531246597263579574622766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968951340535537458516661134223766678604162159680461914467291840300530057530849048765391711386591646239524912623653881879636239373280423891018672348497668235089863388587925628302755995657524455507255189313690836254779186948667994968324049705821028513185451396213837722826145437693412532098591327667236328125001e-324"),
            ("C67", 0x0000000000000001_u64, "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984374999e-324"),
            ("C68", 0x0000000000000002_u64, "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375e-324"),
            ("C69", 0x0000000000000002_u64, "7.4109846876186981626485318930233205854758970392148714663837852375101326090531312779794975454245398856969484704316857659638998506553390969459816219401617281718945106978546710679176872575177347315553307795408549809608457500958111373034747658096871009590975442271004757307809711118935784838675653998783503015228055934046593739791790738723868299395818481660169122019456499931289798411362062484498678713572180352209017023903285791732520220528974020802906854021606612375549983402671300035812486479041385743401875520901590172592547146296175134159774938718574737870961645638908718119841271673056017045493004705269590165763776884908267986972573366521765567941072508764337560846003984904972149117463085539556354188641513168478436313080237596295773983001708984375001e-324"),
            ("C76", 0x0006c9a143590c14_u64, "94393431193180696942841837085033647913224148539854e-358"),
            ("C79", 0x0007802665fd9600_u64, "104308485241983990666713401708072175773165034278685682646111762292409330928739751702404658197872319129036519947435319418387839758990478549477777586673075945844895981012024387992135617064532141489278815239849108105951619997829153633535314849999674266169258928940692239684771590065027025835804863585454872499320500023126142553932654370362024104462255244034053203998964360882487378334860197725139151265590832887433736189468858614521708567646743455601905935595381852723723645799866672558576993978025033590728687206296379801363024094048327273913079612469982585674824156000783167963081616214710691759864332339239688734656548790656486646106983450809073750535624894296242072010195710276073042036425579852459556183541199012652571123898996574563824424330960027873516082763671875e-1075"),
        ];

        // Try and go through all of the tests
        let mut passed_all = true;

        for (name, hex_value, text) in conversion_tests.iter() {
            let (mut scanner, context) = make_scanner(text);
            let expected_value = f64::from_ne_bytes(hex_value.to_ne_bytes());

            if context.borrow().reporter.has_error() {
                // Did not scan properly, skip
                eprintln!("Failed conversion test {}", name);
                passed_all = false;
                continue;
            }

            if let TokenType::RealLiteral(scanned_value) = scanner.next().unwrap().token_type {
                if (scanned_value - expected_value).abs() >= f64::EPSILON {
                    // Not in the near range!
                    eprintln!(
                        "Bad conversion value for {} (expected {:?}, got {:?})",
                        name,
                        expected_value.to_ne_bytes(),
                        scanned_value.to_ne_bytes(),
                    );

                    passed_all = false;
                }
            } else {
                // Wrong type, very bad!
                panic!(
                    "Did not get correct conversion value for {} (got {:?})",
                    name,
                    scanner.next().unwrap().token_type
                );
            }
        }

        assert!(passed_all);
    }

    #[test]
    fn test_string_literal() {
        // String literal parsing
        let (mut scanner, context) = make_scanner("\"abcd💖\"a");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::StringLiteral("abcd💖".to_string())
        );
        // Validate column advancing
        assert_eq!(scanner.next().unwrap().location.column, 8);
        assert!(!context.borrow().reporter.has_error());

        // Invalid parsing should make a literal from the successfully parsed character

        // Ends at the end of line
        let (mut scanner, context) = make_scanner("\"abcd\n");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::StringLiteral("abcd".to_string())
        );
        assert!(context.borrow().reporter.has_error());

        // Ends at the end of file
        let (mut scanner, context) = make_scanner("\"abcd");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::StringLiteral("abcd".to_string())
        );
        assert!(context.borrow().reporter.has_error());

        // Mismatched delimiter
        let (mut scanner, context) = make_scanner("\"abcd'");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::StringLiteral("abcd\'".to_string())
        );
        assert!(context.borrow().reporter.has_error());
    }

    #[test]
    fn test_char_literal() {
        // Char(n) literal parsing
        let (mut scanner, context) = make_scanner("'abcd'");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::CharLiteral("abcd".to_string())
        );
        assert!(!context.borrow().reporter.has_error());

        // Invalid parsing should make a literal from the successfully parsed characters

        // Ends at the end of line
        let (mut scanner, context) = make_scanner("'abcd\n");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::CharLiteral("abcd".to_string())
        );
        assert!(context.borrow().reporter.has_error());

        // Ends at the end of file
        let (mut scanner, context) = make_scanner("'abcd");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::CharLiteral("abcd".to_string())
        );
        assert!(context.borrow().reporter.has_error());

        // Mismatched delimiter
        let (mut scanner, context) = make_scanner("'abcd\"");
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::CharLiteral("abcd\"".to_string())
        );
        assert!(context.borrow().reporter.has_error());
    }

    #[test]
    fn test_char_literal_escapes() {
        // Valid escapes:
        let valid_escapes = [
            ("'\\\\'", "\\"),
            ("'\\\''", "\'"),
            ("'\\\"'", "\""),
            ("'\\b'", "\x08"),
            ("'\\d'", "\x7F"),
            ("'\\e'", "\x1B"),
            ("'\\f'", "\x0C"),
            ("'\\r'", "\r"),
            ("'\\n'", "\n"),
            ("'\\t'", "\t"),
            ("'\\^'", "^"),
            ("'\\B'", "\x08"),
            ("'\\D'", "\x7F"),
            ("'\\E'", "\x1B"),
            ("'\\F'", "\x0C"),
            ("'\\T'", "\t"),
            // Octal escapes
            ("'\\0o'", "\0o"),
            ("'\\43O'", "#O"),
            ("'\\101'", "A"),
            ("'\\377'", "\u{00FF}"), // Have to use unicode characters
            ("'\\1011'", "A1"),
            // Hex escapes (non-hex digits and extra hex digits are ignored)
            ("'\\x0o'", "\0o"),
            ("'\\x00'", "\0"),
            ("'\\x00Ak'", "\0Ak"),
            ("'\\x20'", " "),
            ("'\\x20Ar'", " Ar"),
            ("'\\xfe'", "\u{00FE}"),
            // Unicode escapes (non-hex digits and extra digits are ignored)
            ("'\\u8o'", "\x08o"),
            ("'\\uA7k'", "§k"),
            ("'\\u394o'", "Δo"),
            ("'\\u2764r'", "❤r"),
            ("'\\u1f029t'", "🀩t"),
            ("'\\u10f029s'", "\u{10F029}s"),
            ("'\\u10F029i'", "\u{10F029}i"),
            ("'\\U8O'", "\x08O"),
            ("'\\Ua7l'", "§l"),
            ("'\\U394w'", "Δw"),
            ("'\\U2764X'", "❤X"),
            ("'\\U1F029z'", "🀩z"),
            ("'\\U10F029Y'", "\u{10F029}Y"),
            ("'\\U10F029jY'", "\u{10F029}jY"),
            // Caret escapes
            ("'^J'", "\n"),
            ("'^M'", "\r"),
            ("'^?'", "\x7F"),
        ];

        for (test_num, escape_test) in valid_escapes.iter().enumerate() {
            let (mut scanner, context) = make_scanner(escape_test.0);
            assert_eq!(
                scanner.next().unwrap().token_type,
                TokenType::CharLiteral(escape_test.1.to_string())
            );
            assert!(
                !context.borrow().reporter.has_error(),
                "in test #{} ({:?})",
                test_num + 1,
                valid_escapes[test_num]
            );
        }

        // Escapes at the end of lines
        let failed_escapes = [
            "'\\\n'", "'\\\r'", "'\\\0'", // Slash escapes
            "'^\n'", "'^\r'", "'^\0'", // Caret escapes
        ];

        for escape_test in failed_escapes.iter() {
            let (mut scanner, context) = make_scanner(escape_test);
            assert_eq!(
                scanner.next().unwrap().token_type,
                TokenType::CharLiteral("".to_string())
            );
            assert!(context.borrow().reporter.has_error());
        }

        // Bad escape sequences
        let failed_escapes = [
            // Greater than 255
            "'\\777'",
            // Larger than U+10FFFF
            "'\\u200000'",
            "'\\u3ffffff'",
            "'\\u3fffffff'",
            // Surrogate characters
            "'\\uD800'",
            "'\\UDFfF'",
            "'\\Ud900'",
            "'\\udab0'",
        ];

        for escape_test in failed_escapes.iter() {
            let (mut scanner, context) = make_scanner(escape_test);
            assert_eq!(
                scanner.next().unwrap().token_type,
                TokenType::CharLiteral(char::REPLACEMENT_CHARACTER.to_string())
            );
            assert!(context.borrow().reporter.has_error());
        }

        // Incorrect start of escape sequence
        let incorrect_start = [
            ("'\\8'", "8"),
            ("'^~'", "~"),
            ("'\\x'", "x"),
            ("'\\u'", "u"),
            ("'\\U'", "U"),
        ];

        for escape_test in incorrect_start.iter() {
            let (mut scanner, context) = make_scanner(escape_test.0);
            assert_eq!(
                scanner.next().unwrap().token_type,
                TokenType::CharLiteral(escape_test.1.to_string())
            );
            assert!(context.borrow().reporter.has_error());
        }
    }

    #[test]
    fn test_block_comment() {
        // Block comments
        let (mut scanner, context) = make_scanner("/* /* abcd % * / \n\n\r\n */ */ asd");
        assert!(!context.borrow().reporter.has_error());
        assert_eq!(scanner.next().unwrap().token_type, TokenType::Identifier);
        assert_eq!(scanner.next(), None);

        // End of file, mismatch
        let (mut scanner, context) = make_scanner("/* /* abcd */ ");
        let _ = scanner.next();
        assert!(context.borrow().reporter.has_error());
    }

    #[test]
    fn test_line_comment() {
        // Line comment
        let source = "% abcd asd\n asd";
        let (mut scanner, context) = make_scanner(source);
        let e = scanner.next().unwrap();
        assert_eq!(e.token_type, TokenType::Identifier);

        let eof = scanner.next();
        assert_eq!(
            &eof,
            &None,
            "is {:?}",
            eof.as_ref().map(|tok| tok.location.get_lexeme(source))
        );
        assert!(!context.borrow().reporter.has_error());

        // End of file
        let (mut scanner, context) = make_scanner("% abcd asd");
        assert_eq!(scanner.next(), None);
        assert_eq!(scanner.next(), None);
        assert!(!context.borrow().reporter.has_error());
    }

    #[test]
    fn test_keyword() {
        // Keyword as the corresponding keyword
        let (mut scanner, context) = make_scanner("and");
        assert_eq!(scanner.next().unwrap().token_type, TokenType::And);
        assert!(!context.borrow().reporter.has_error());

        // Multiple keywords in sequence
        let (mut scanner, context) = make_scanner("var ident : int");
        assert_eq!(scanner.next().unwrap().token_type, TokenType::Var);
        assert_eq!(scanner.next().unwrap().token_type, TokenType::Identifier);
        assert_eq!(scanner.next().unwrap().token_type, TokenType::Colon);
        assert_eq!(scanner.next().unwrap().token_type, TokenType::Int);
        assert!(!context.borrow().reporter.has_error());
    }

    #[test]
    fn test_aliases() {
        let (scanner, context) = make_scanner("fcn proc");
        assert_eq!(
            scanner.map(|tk| tk.token_type).collect::<Vec<TokenType>>(),
            vec![TokenType::Function, TokenType::Procedure,]
        );
        assert!(!context.borrow().reporter.has_error());
    }

    // Other Tests
    // Covers any tests that don't really fit in the above categories, or are regression tests

    #[test]
    fn test_nat_literal_before_range() {
        // Dot should not form a RealLiteral
        let (mut scanner, context) = make_scanner("1..");
        assert_eq!(scanner.next().unwrap().token_type, TokenType::NatLiteral(1));
        assert_eq!(scanner.next().unwrap().token_type, TokenType::Range);
        assert!(!context.borrow().reporter.has_error());

        // Should scan as `RealLiteral(0.0)` and `ggy`
        let source: &str = "1eggy";
        let (mut scanner, context) = make_scanner(source);
        assert_eq!(
            scanner.next().unwrap().token_type,
            TokenType::RealLiteral(0.0)
        );

        let ident = scanner.next().unwrap();
        assert_eq!(ident.token_type, TokenType::Identifier);
        assert_eq!(ident.location.get_lexeme(source), "ggy");
        assert!(context.borrow().reporter.has_error());
    }
}
