//! Scanner for tokens
use crate::compiler::token::{Token, TokenType};
use crate::compiler::Location;
use crate::status_reporter::StatusReporter;
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

    /// Checks if the scan was successfully performed
    pub fn is_valid_scan(&self) -> bool {
        !self.reporter.has_error()
    }

    /// Scans the source input for all tokens
    pub fn scan_tokens(&mut self) {
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
                    change.token_type = TokenType::NotEq;
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
            '-' => self.make_or_default('>', TokenType::Deref, 2, TokenType::Minus, 1),
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
        let numerals = self.cursor.get_lexeme(self.source);
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
                let literal_len = self.cursor.get_lexeme(self.source).len();
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
        let digits = self.cursor.get_lexeme(self.source);
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
        let lexeme = self.cursor.get_lexeme(self.source);
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

                let text_slice = text_locate.get_lexeme(self.source).to_string();

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
            "body" => TokenType::Body,
            "boolean" => TokenType::Boolean,
            "by" => TokenType::By,
            "case" => TokenType::Case,
            "char" => TokenType::Char,
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
            "else" => TokenType::Else,
            "elsif" => TokenType::Elsif,
            "end" => TokenType::End,
            "enum" => TokenType::Enum,
            "exit" => TokenType::Exit,
            "export" => TokenType::Export,
            "external" => TokenType::External,
            "false" => TokenType::False,
            "fcn" => TokenType::Fcn,
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
            "proc" => TokenType::Proc,
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
        assert_eq!(scanner.tokens[0].token_type, TokenType::Identifier);
        assert_eq!(
            scanner.tokens[0].location.get_lexeme(scanner.source),
            "_source_text"
        );

        // Skip over first digits
        let mut scanner = Scanner::new("0123_separate");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_ne!(scanner.tokens[0].token_type, TokenType::Identifier);
        assert_ne!(
            scanner.tokens[0].location.get_lexeme(scanner.source),
            "0123_separate"
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

    #[test]
    fn test_block_comment() {
        // Block comments
        let mut scanner = Scanner::new("/* /* abcd % * / */ */ asd");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens.len(), 2);
        assert_eq!(scanner.tokens[0].token_type, TokenType::Identifier);

        // End of file, mismatch
        let mut scanner = Scanner::new("/* /* abcd */ ");
        scanner.scan_tokens();
        assert!(!scanner.is_valid_scan());
    }

    #[test]
    fn test_line_comment() {
        // Line comment
        let mut scanner = Scanner::new("% abcd asd\n asd");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens.len(), 2);
        assert_eq!(scanner.tokens[0].token_type, TokenType::Identifier);

        // End of file
        let mut scanner = Scanner::new("% abcd asd");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens.len(), 1);
    }

    #[test]
    fn test_keyword() {
        // Keyword as the corresponding keyword
        let mut scanner = Scanner::new("and");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(scanner.tokens.len(), 2);
        assert_eq!(scanner.tokens[0].token_type, TokenType::And);
    }

    #[test]
    fn test_not_in_stitching() {
        let mut scanner = Scanner::new("not in ~ in ~in in in not");
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
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
        scanner.scan_tokens();
        assert!(scanner.is_valid_scan());
        assert_eq!(
            scanner
                .tokens
                .iter()
                .map(|tk| &tk.token_type)
                .collect::<Vec<&TokenType>>(),
            vec![
                &TokenType::NotEq,
                &TokenType::NotEq,
                &TokenType::NotEq,
                &TokenType::NotEq,
                &TokenType::Equ,
                &TokenType::Equ,
                &TokenType::Not,
                &TokenType::Eof,
            ]
        );
    }
}
