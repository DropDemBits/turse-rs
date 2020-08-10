//! Scanner for tokens
use crate::compiler::frontend::token::{Token, TokenType};
use crate::compiler::Location;
use crate::status_reporter::StatusReporter;
use std::char;
use std::num::ParseIntError;
use unicode_segmentation::UnicodeSegmentation;

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

    /// Scans the source input for all tokens
    /// Returns true if the scan was successful
    pub fn scan_tokens(&mut self) -> bool {
        while !self.is_at_end() {
            self.cursor.step();
            self.scan_token();
            self.stitch_token();
        }

        if self.tokens.is_empty() || self.tokens.last().unwrap().token_type != TokenType::Eof {
            // Add eof
            self.cursor.step();
            self.make_token(TokenType::Eof, 1);
        }

        !self.reporter.has_error()
    }

    // Checks if the end of the stream has been reached
    fn is_at_end(&self) -> bool {
        self.cursor.end >= self.source.len()
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

    /// Stiches the previous token with the current ont
    fn stitch_token(&mut self) {
        let mut reverse_iter = self.tokens.iter().rev();
        let mut next_reverse = move || {
            reverse_iter
                .next()
                .map(|tok| &tok.token_type)
                .unwrap_or(&TokenType::Eof)
        };

        match next_reverse() {
            TokenType::In => match next_reverse() {
                TokenType::Not | TokenType::Tilde => {
                    // Stitch not in -> not_in
                    let end_loc = self.tokens.pop().unwrap().location;
                    let change = self.tokens.last_mut().unwrap();

                    // Adjust location & kind
                    change.token_type = TokenType::NotIn;
                    change.location.current_to_other(&end_loc);
                }
                _ => {}
            },
            TokenType::Equ => match next_reverse() {
                TokenType::Not | TokenType::Tilde => {
                    // Stitch not = -> not_=
                    let end_loc = self.tokens.pop().unwrap().location;
                    let change = self.tokens.last_mut().unwrap();

                    // Adjust location & kind
                    change.token_type = TokenType::NotEqu;
                    change.location.current_to_other(&end_loc);
                }
                _ => {}
            },
            _ => {}
        }
    }

    /// Scan a single token
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
            '@' => self.make_token(TokenType::At, 1),
            '^' => self.make_token(TokenType::Caret, 1),
            ',' => self.make_token(TokenType::Comma, 1),
            '#' => self.make_token(TokenType::Pound, 1),
            '+' => self.make_token(TokenType::Plus, 1),
            ';' => self.make_token(TokenType::Semicolon, 1),
            '~' => self.make_token(TokenType::Tilde, 1),
            '&' => self.make_token(TokenType::And, 1),
            '|' => self.make_token(TokenType::Or, 1),
            '/' => {
                if self.match_next('*') {
                    // Block comment parsing
                    let mut depth: usize = 1;

                    while depth > 0 {
                        match self.peek {
                            '\0' => {
                                // Block comment ends at the end of the file
                                self.next_char();
                                self.cursor.step();
                                self.make_token(TokenType::Eof, 0);

                                self.reporter.report_error(
                                    &self.cursor,
                                    format_args!(
                                        "Block comment (starting here) ends at the end of the file"
                                    ),
                                );
                                break;
                            }
                            '*' => {
                                if self.peek_ahead == '/' {
                                    // Decrease depth and consume '*/'
                                    self.next_char();
                                    self.next_char();
                                    depth = depth.saturating_sub(1);
                                } else {
                                    // Consume the star
                                    self.next_char();
                                }
                            }
                            '/' => {
                                if self.peek_ahead == '*' {
                                    // Increase depth and consume '/*'
                                    self.next_char();
                                    self.next_char();
                                    depth = depth.saturating_add(1);
                                } else {
                                    // Consume the slash
                                    self.next_char();
                                }
                            }
                            '\n' => {
                                // End the line and rebase lexeme start to the beginning of the line
                                self.next_char();
                                self.cursor.lines(1);
                                self.cursor.step();
                            }
                            _ => {
                                // Consume char
                                self.next_char();
                            }
                        }
                    }

                    // Handle column stuff
                    let remaining_comment = self.cursor.get_lexeme(self.source);
                    let end_at_column =
                        UnicodeSegmentation::graphemes(remaining_comment, true).count();
                    self.cursor.columns(end_at_column);
                } else {
                    self.make_token(TokenType::Slash, 1);
                }
            }
            '%' => {
                // Line comment
                while self.peek != '\n' && !self.is_at_end() {
                    // Nom all the chars
                    self.next_char();
                }
            }
            '-' => self.make_or_default('>', TokenType::Arrow, 2, TokenType::Minus, 1),
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
                    self.cursor.columns(1);
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
        self.cursor.columns(steps);

        self.tokens.push(Token {
            token_type,
            location: self.cursor.clone(),
        });
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
        // End normal NatLiteral
        let numerals = self.cursor.get_lexeme(self.source);
        let numerals_len = numerals.len();
        //let value = numerals.parse::<u64>();

        match try_parse_int(numerals, 10) {
            Ok(num) => {
                self.make_token(TokenType::NatLiteral(num), numerals_len);
            }
            Err(e) => {
                match e {
                    IntErrKind::Overflow(_) => self
                        .reporter
                        .report_error(&self.cursor, format_args!("Integer literal is too large")),
                    IntErrKind::InvalidDigit(_) => self.reporter.report_error(
                        &self.cursor,
                        format_args!("Invalid digit found for a base 10 number"),
                    ),
                    IntErrKind::Other(e) => self.reporter.report_error(
                        &self.cursor,
                        format_args!("Failed to parse integer literal ({})", e),
                    ),
                }

                // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
                self.make_token(TokenType::NatLiteral(0), numerals_len);
            }
        }

        // Done
        return;
    }

    fn make_number_radix(&mut self) {
        // Base has already been parsed
        let base_numerals = self.cursor.get_lexeme(self.source).to_string();
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
        let radix_numerals = radix_locate
            .get_lexeme(self.source)
            .to_string()
            .to_ascii_lowercase();

        // Parse as a u64
        let base = match try_parse_int(&base_numerals, 10) {
            Ok(num) => {
                if num < 2 || num > 36 {
                    self.reporter.report_error(
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
                        self.reporter.report_error(
                            &self.cursor,
                            format_args!(
                                "Base for integer literal is not between the range of 2 - 36"
                            ),
                        );
                    }
                    IntErrKind::InvalidDigit(_) => {
                        self.reporter.report_error(
                            &self.cursor,
                            format_args!("Invalid digit found in the base specifier"),
                        );
                    } // Notify!
                    IntErrKind::Other(e) => self.reporter.report_error(
                        &self.cursor,
                        format_args!("Failed to parse base for integer literal ({})", e),
                    ),
                }

                None
            }
        };

        // Check if the base is in range
        if base.is_none() {
            // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
            self.make_token(TokenType::NatLiteral(0), base_numerals.len());
            return;
        }

        let base = base.unwrap();

        // Check if there are any numeral digits
        if radix_numerals.is_empty() {
            self.reporter.report_error(
                &self.cursor,
                format_args!("Missing digits for integer literal"),
            );

            // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
            self.make_token(TokenType::NatLiteral(0), base_numerals.len());
            return;
        }

        // Check if the range contains digits outside of the range

        match try_parse_int(&radix_numerals, base as u32) {
            Ok(num) => {
                let literal_len = self.cursor.get_lexeme(self.source).len();
                self.make_token(TokenType::NatLiteral(num), literal_len);
            }
            Err(k) => {
                match k {
                    IntErrKind::Overflow(_) => {
                        self.reporter.report_error(
                            &self.cursor,
                            format_args!("Integer literal is too large"),
                        );
                    }
                    IntErrKind::InvalidDigit(_) => {
                        self.reporter.report_error(
                        &self.cursor,
                        format_args!("Digit in integer literal is outside of the specified base's allowed digits"),
                    );
                    }
                    IntErrKind::Other(e) => self.reporter.report_error(
                        &self.cursor,
                        format_args!("Failed to parse base for integer literal ({})", e),
                    ),
                }
                // Produce a 0 value token (exact value doesn't matter, as the output will not be compiled)
                self.make_token(TokenType::NatLiteral(0), base_numerals.len());
            }
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
        let digits = self.cursor.get_lexeme(self.source);
        let digits_len = digits.len();
        let value = digits.parse::<f64>();

        // Setup the token width
        self.cursor.columns(digits_len);

        // A value is always produced
        let parsed_value = match value {
            Ok(num) if num.is_infinite() => {
                self.reporter
                    .report_error(&self.cursor, format_args!("Real literal is too large"));
                0f64
            }
            Ok(num) if num.is_nan() => {
                // Capture NaNs (What impl does)
                self.reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal"));
                0f64
            }
            Err(e) if e.to_string() == "invalid float literal" => {
                self.reporter
                    .report_error(&self.cursor, format_args!("Invalid real literal"));
                0f64
            }
            Err(e) => {
                self.reporter
                    .report_error(&self.cursor, format_args!("{}", e.to_string()));
                0f64
            }
            Ok(num) => num,
        };

        self.make_token(TokenType::RealLiteral(parsed_value), 0);
    }

    fn make_str_literal(&mut self, is_str_literal: bool, s: String, width: usize) {
        if is_str_literal {
            self.make_token(TokenType::StringLiteral(s), width);
        } else {
            self.make_token(TokenType::CharLiteral(s), width);
        }
    }

    fn make_char_sequence(&mut self, is_str_literal: bool) {
        let ending_delimiter = if is_str_literal { '"' } else { '\'' };
        let literal_text = self.extract_char_sequence(ending_delimiter);

        // Get the width of the lexeme
        let lexeme = self.cursor.get_lexeme(self.source);
        let part_width = UnicodeSegmentation::graphemes(lexeme, true).count();
        // Advance to the correct location
        self.cursor.columns(part_width);

        match self.peek {
            '\r' | '\n' => {
                self.reporter.report_error(
                    &self.cursor,
                    format_args!("String literal ends at the end of the line"),
                );

                self.make_str_literal(is_str_literal, literal_text, 0);

                return;
            }
            '\0' => {
                self.reporter.report_error(
                    &self.cursor,
                    format_args!("String literal ends at the end of the file"),
                );
                self.make_str_literal(is_str_literal, literal_text, part_width);

                return;
            }
            _ => {
                assert!((self.peek == '\'' || self.peek == '"'));

                // Consume other delimiter
                self.next_char();

                // Make it! (Adjust part_width by 1 to account for ending delimiter)
                self.make_str_literal(is_str_literal, literal_text, 1);
            }
        }
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
        // this issue by converting the UTF-8 strings into ASCII strings.

        // Keep going along the string until the end of the line, or the delimiter
        while self.peek != ending_delimiter && !matches!(self.peek, '\r' | '\n' | '\0') {
            let current = self.next_char();
            match current {
                '\\' => {
                    // Parse escape character
                    let escaped = self.peek;

                    match escaped {
                        '\r' | '\n' | '\0' => break, // Reacched the end of the literal
                        '\'' => {
                            literal_text.push('\'');
                            self.next_char();
                        }
                        '"' => {
                            literal_text.push('"');
                            self.next_char();
                        }
                        '\\' => {
                            literal_text.push('\\');
                            self.next_char();
                        }
                        'b' | 'B' => {
                            literal_text.push('\x08');
                            self.next_char();
                        }
                        'd' | 'D' => {
                            literal_text.push('\x7F');
                            self.next_char();
                        }
                        'e' | 'E' => {
                            literal_text.push('\x1B');
                            self.next_char();
                        }
                        'f' | 'F' => {
                            literal_text.push('\x0C');
                            self.next_char();
                        }
                        'r' | 'R' => {
                            literal_text.push('\r');
                            self.next_char();
                        }
                        'n' | 'N' => {
                            literal_text.push('\n');
                            self.next_char();
                        }
                        't' | 'T' => {
                            literal_text.push('\t');
                            self.next_char();
                        }
                        '^' => {
                            // Unescaped version is parsed in Caret Notation
                            literal_text.push('^');
                            self.next_char();
                        }
                        '0'..='7' => {
                            // Octal str, {1-3}, 0 - 377
                            let mut octal_cursor = self.cursor.clone();

                            // Start at the first digit
                            octal_cursor.step();

                            // Nom all of the octal digits
                            for _ in 0..3 {
                                if !matches!(self.peek, '0'..='7') {
                                    break;
                                }

                                self.next_char();
                                octal_cursor.columns(1);
                            }

                            // Select the octal digits
                            octal_cursor.current_to_other(&self.cursor);

                            let to_chr = u16::from_str_radix(octal_cursor.get_lexeme(self.source), 8);

                            if let Err(err) = to_chr {
                                // Can't parse the digits, push an invalid char
                                self.reporter.report_error(&octal_cursor, format_args!("Can't parse digits for the octal character value: {}", err));
                                literal_text.push('�');
                                continue;
                            }

                            let to_chr = to_chr.ok().unwrap();

                            // Check if the parsed character is in range
                            if to_chr >= 256 {
                                self.reporter.report_error(
                                    &octal_cursor,
                                    format_args!(
                                        "Octal character value is larger than 255 (octal 377)"
                                    ),
                                );

                                literal_text.push('�');
                            } else {
                                literal_text.push((to_chr as u8) as char);
                            }
                        }
                        'x' if self.peek_ahead.is_ascii_hexdigit() => {
                            // Hex sequence, {1-2} digits
                            // nom 'x'
                            self.next_char();

                            let mut hex_cursor = self.cursor.clone();

                            // Start at the first digit
                            hex_cursor.step();

                            // Nom all of the hex digits
                            for _ in 0..2 {
                                if !self.peek.is_ascii_hexdigit() {
                                    break;
                                }

                                self.next_char();
                                hex_cursor.columns(1);
                            }

                            // Select the hex digits
                            hex_cursor.current_to_other(&self.cursor);

                            let to_chr =
                                u8::from_str_radix(hex_cursor.get_lexeme(self.source), 16);

                            if let Err(err) = to_chr {
                                // Can't parse the digits, push an invalid char
                                self.reporter.report_error(&hex_cursor, format_args!("Can't parse digits for hex character value: {}", err));
                                literal_text.push('�');
                                continue;
                            }

                            let to_chr = to_chr.ok().unwrap();

                            // Push the parsed char
                            literal_text.push(to_chr as char);
                        }
                        'u' | 'U' if self.peek_ahead.is_ascii_hexdigit() => {
                            // u: unicode character {4-8} � if out of range
                            // nom 'u' or 'U'
                            self.next_char();

                            let mut hex_cursor = self.cursor.clone();

                            // Start at the first digit
                            hex_cursor.step();

                            // Nom all of the hex digits
                            for _ in 0..8 {
                                if !self.peek.is_ascii_hexdigit() {
                                    break;
                                }

                                self.next_char();
                                hex_cursor.columns(1);
                            }

                            // Select the hex digits
                            hex_cursor.current_to_other(&self.cursor);

                            let to_chr =
                                u32::from_str_radix(hex_cursor.get_lexeme(self.source), 16);

                            if let Err(err) = to_chr {
                                // Can't parse the digits, push an invalid char
                                self.reporter.report_error(&hex_cursor, format_args!("Can't parse digits for unicode codepoint: {}", err));
                                literal_text.push('�');
                                continue;
                            }

                            let to_chr = to_chr.ok().unwrap();

                            // Check if the parsed char is in range
                            if to_chr > 0x10FFFF {
                                self.reporter.report_error(
                                    &hex_cursor,
                                    format_args!(
                                        "Unicode codepoint value is greater than U+10FFFF"
                                    ),
                                );
                                literal_text.push('�');
                            } else {
                                // Push the parsed char
                                literal_text.push(char::from_u32(to_chr).unwrap());
                            }
                        }
                        _ => {
                            // Fetch the location
                            let mut bad_escape = self.cursor.clone();
                            self.next_char();

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
                                    self.reporter.report_error(
                                        &bad_escape,
                                        format_args!(
                                            "Invalid escape sequence character '{}' (missing hexadecimal digits after the '{}')",
                                            bad_escape.get_lexeme(self.source), escaped
                                        ),
                                    );
                                }
                                _ => {
                                    // Bog-standard error report
                                    self.reporter.report_error(
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
                        }
                    }
                }
                '^' => {
                    // Parse caret notation
                    // ASCII character range from '@' to '_', includes '?' (DEL)
                    let escaped = self.peek;
                    match escaped {
                        '\r' | '\n' | '\0' => break, // Reacched the end of the literal
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
                            let mut bad_escape = self.cursor.clone();
                            self.next_char();

                            // Select the escape sequence
                            bad_escape.step();
                            bad_escape.current_to_other(&self.cursor);
                            // Adjust everything so that the lexeme lines up with the escape sequence
                            bad_escape.start -= 1;
                            bad_escape.column += 1;
                            bad_escape.width = 2;

                            self.reporter.report_error(
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
                _ => literal_text.push(current),
            }
        }

        literal_text
    }

    fn make_ident(&mut self) {
        // Consume all of the identifier digits
        while is_ident_char_or_digit(self.peek) {
            self.next_char();
        }

        // Produce the identifier
        let ident_slice = self.cursor.get_lexeme(self.source);
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
            "fcn" => TokenType::Function,
            "flexible" => TokenType::Flexible,
            "for" => TokenType::For,
            "fork" => TokenType::Fork,
            "forward" => TokenType::Forward,
            "free" => TokenType::Free,
            "function" => TokenType::Function,
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
            "proc" => TokenType::Procedure,
            "procedure" => TokenType::Procedure,
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

        self.make_token(token_type, len);
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

            if scanner.scan_tokens() {
                panic!("Invalid char {} passed as valid", c);
            } else if scanner.tokens[0].location.column != 2 {
                panic!("Column not advanced over invalid charater");
            }
        }
    }

    #[test]
    fn test_identifier() {
        // Valid ident
        let mut scanner = Scanner::new("_source_text");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::Identifier);
        assert_eq!(
            scanner.tokens[0].location.get_lexeme(scanner.source),
            "_source_text"
        );

        // Skip over first digits
        let mut scanner = Scanner::new("0123_separate");
        assert!(scanner.scan_tokens());
        assert_ne!(scanner.tokens[0].token_type, TokenType::Identifier);
        assert_ne!(
            scanner.tokens[0].location.get_lexeme(scanner.source),
            "0123_separate"
        );

        // Invalid character, but "ba" should still be parsed
        let mut scanner = Scanner::new("ba$e");
        assert!(!scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].location.get_lexeme("ba$e"), "ba");
        assert_eq!(scanner.tokens[1].location.get_lexeme("ba$e"), "e");
        // Column check for invalid characters
        assert_eq!(scanner.tokens[1].location.column, 4);
    }

    #[test]
    fn test_int_literal_basic() {
        // Basic integer literal
        let mut scanner = Scanner::new("01234560");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(1234560));

        // Overflow
        let mut scanner = Scanner::new("99999999999999999999");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));

        // Digit cutoff
        let mut scanner = Scanner::new("999a999");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(999));
    }

    #[test]
    fn test_int_literal_radix() {
        // Integer literal with base
        let mut scanner = Scanner::new("16#EABC");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0xEABC));

        // Overflow
        let mut scanner = Scanner::new("10#99999999999999999999");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));

        // No digits
        let mut scanner = Scanner::new("30#");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));

        // Out of range (> 36)
        let mut scanner = Scanner::new("37#asda");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));

        // Out of range (= 0)
        let mut scanner = Scanner::new("0#0000");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));

        // Out of range (= 1)
        let mut scanner = Scanner::new("1#0000");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));

        // Invalid digit
        let mut scanner = Scanner::new("10#999a999");
        assert!(!scanner.scan_tokens());
        // Should still produce a token
        assert_eq!(scanner.tokens[0].token_type, TokenType::NatLiteral(0));
    }

    #[test]
    fn test_real_literal() {
        // NOTE: May need to use Epsilon comparison if this test fails on other machines & operating systems
        // Real Literal
        let mut scanner = Scanner::new("1.");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::RealLiteral(1.0));

        let mut scanner = Scanner::new("100.00");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::RealLiteral(100.00));

        let mut scanner = Scanner::new("100.00e10");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::RealLiteral(100.00e10)
        );

        let mut scanner = Scanner::new("100.00e100");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::RealLiteral(100.00e100)
        );

        let mut scanner = Scanner::new("1e100");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens[0].token_type, TokenType::RealLiteral(1e100));

        // Invalid format
        let mut scanner = Scanner::new("1e");
        assert!(!scanner.scan_tokens());
        // Should still produce a value
        assert_eq!(scanner.tokens[0].token_type, TokenType::RealLiteral(0f64));

        // Too big
        let mut scanner = Scanner::new("1e600");
        assert!(!scanner.scan_tokens());
        // Should still produce a value
        assert_eq!(scanner.tokens[0].token_type, TokenType::RealLiteral(0f64));
    }

    #[test]
    fn test_string_literal() {
        // String literal parsing
        let mut scanner = Scanner::new("\"abcd💖\"a");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::StringLiteral("abcd💖".to_string())
        );

        // Validate column advancing
        assert_eq!(scanner.tokens[1].location.column, 8);

        // Invalid parsing should make a literal from the successfully parsed character

        // Ends at the end of line
        let mut scanner = Scanner::new("\"abcd\n");
        assert!(!scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::StringLiteral("abcd".to_string())
        );

        // Ends at the end of file
        let mut scanner = Scanner::new("\"abcd");
        assert!(!scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::StringLiteral("abcd".to_string())
        );

        // Mismatched delimiter
        let mut scanner = Scanner::new("\"abcd'");
        assert!(!scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::StringLiteral("abcd\'".to_string())
        );
    }

    #[test]
    fn test_char_literal() {
        // Char(n) literal parsing
        let mut scanner = Scanner::new("'abcd'");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::CharLiteral("abcd".to_string())
        );

        // Invalid parsing should make a literal from the successfully parsed characters

        // Ends at the end of line
        let mut scanner = Scanner::new("'abcd\n");
        assert!(!scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::CharLiteral("abcd".to_string())
        );

        // Ends at the end of file
        let mut scanner = Scanner::new("'abcd");
        assert!(!scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::CharLiteral("abcd".to_string())
        );

        // Mismatched delimiter
        let mut scanner = Scanner::new("'abcd\"");
        assert!(!scanner.scan_tokens());
        assert_eq!(
            scanner.tokens[0].token_type,
            TokenType::CharLiteral("abcd\"".to_string())
        );
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
            let mut scanner = Scanner::new(escape_test.0);
            eprintln!("test #{}", test_num + 1);
            assert!(scanner.scan_tokens());
            assert_eq!(
                scanner.tokens[0].token_type,
                TokenType::CharLiteral(escape_test.1.to_string())
            );
        }

        // Escapes at the end of lines
        let failed_escapes = [
            "'\\\n'", "'\\\r'", "'\\\0'", // Slash escapes
            "'^\n'", "'^\r'", "'^\0'", // Caret escapes
        ];

        for escape_test in failed_escapes.iter() {
            let mut scanner = Scanner::new(escape_test);
            assert!(!scanner.scan_tokens());
            assert_eq!(
                scanner.tokens[0].token_type,
                TokenType::CharLiteral("".to_string())
            );
        }

        // Bad escape sequences
        let failed_escapes = [
            // Greater than 255
            "'\\777'",
            // Larger than U+10FFFF
            "'\\u200000'",
            "'\\u3ffffff'",
            "'\\u3fffffff'",
        ];

        for escape_test in failed_escapes.iter() {
            let mut scanner = Scanner::new(escape_test);
            assert!(!scanner.scan_tokens());
            assert_eq!(
                scanner.tokens[0].token_type,
                TokenType::CharLiteral('�'.to_string())
            );
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
            let mut scanner = Scanner::new(escape_test.0);
            assert!(!scanner.scan_tokens());
            assert_eq!(
                scanner.tokens[0].token_type,
                TokenType::CharLiteral(escape_test.1.to_string())
            );
        }
    }

    #[test]
    fn test_block_comment() {
        // Block comments
        let mut scanner = Scanner::new("/* /* abcd % * / \n\n\r\n */ */ asd");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens.len(), 2);
        assert_eq!(scanner.tokens[0].token_type, TokenType::Identifier);

        // End of file, mismatch
        let mut scanner = Scanner::new("/* /* abcd */ ");
        assert!(!scanner.scan_tokens());
    }

    #[test]
    fn test_line_comment() {
        // Line comment
        let mut scanner = Scanner::new("% abcd asd\n asd");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens.len(), 2);
        assert_eq!(scanner.tokens[0].token_type, TokenType::Identifier);

        // End of file
        let mut scanner = Scanner::new("% abcd asd");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens.len(), 1);
    }

    #[test]
    fn test_keyword() {
        // Keyword as the corresponding keyword
        let mut scanner = Scanner::new("and");
        assert!(scanner.scan_tokens());
        assert_eq!(scanner.tokens.len(), 2);
        assert_eq!(scanner.tokens[0].token_type, TokenType::And);
    }

    #[test]
    fn test_not_in_stitching() {
        let mut scanner = Scanner::new("not in ~ in ~in in in not");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner
                .tokens
                .iter()
                .map(|tk| &tk.token_type)
                .collect::<Vec<&TokenType>>(),
            vec![
                &TokenType::NotIn,
                &TokenType::NotIn,
                &TokenType::NotIn,
                &TokenType::In,
                &TokenType::In,
                &TokenType::Not,
                &TokenType::Eof,
            ]
        );
    }

    #[test]
    fn test_not_eq_stitching() {
        let mut scanner = Scanner::new("not = not= ~ = ~= = = not");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner
                .tokens
                .iter()
                .map(|tk| &tk.token_type)
                .collect::<Vec<&TokenType>>(),
            vec![
                &TokenType::NotEqu,
                &TokenType::NotEqu,
                &TokenType::NotEqu,
                &TokenType::NotEqu,
                &TokenType::Equ,
                &TokenType::Equ,
                &TokenType::Not,
                &TokenType::Eof,
            ]
        );
    }

    #[test]
    fn test_aliases() {
        let mut scanner = Scanner::new("fcn proc");
        assert!(scanner.scan_tokens());
        assert_eq!(
            scanner
                .tokens
                .iter()
                .map(|tk| &tk.token_type)
                .collect::<Vec<&TokenType>>(),
            vec![&TokenType::Function, &TokenType::Procedure, &TokenType::Eof,]
        )
    }
}
