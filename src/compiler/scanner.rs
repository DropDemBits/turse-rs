//! Scanner for tokens

/// Location of a token in a file/text stream
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    /// Starting byte of a lexeme
    start: usize,
    /// Ending byte of a lexeme
    end: usize,
    /// Line number of the lexeme
    line: usize,
    /// Starting column of the lexeme
    column: usize,
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

    /// Advances the location to the next lexeme
    pub fn step(&mut self) {
        self.start = self.end;
    }

    /// Moves the end of the lexeme to the given byte index
    pub fn current_to(&mut self, next_end: usize) {
        self.end = next_end;
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
    // However, these cases would be handled by the parser.

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
    Result,
    Return,
    Seek,
    Set,
    Shl,
    Shr,
    Signal,
    Skip,
    String,
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
    IntLiteral(i64),
    RealLiteral(f64),

    // Other
    Eof,
}

/// Scanner for tokens
pub struct Scanner<'a> {
    /// Scanning source
    source: &'a str,
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
            tokens: vec![],
            next_indicies,
            chars,
            peek,
            peek_ahead,
            cursor: Location::new(),
        }
    }

    /// Scans the source input for all tokens
    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.cursor.step();
            self.scan_token();
        }
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
            '\0' => self.make_token(TokenType::Eof),
            // Whitespace
            ' ' | '\t' | '\r' => {}
            // Meaningful tokens
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '&' => self.make_token(TokenType::Ampersand),
            '@' => self.make_token(TokenType::At),
            '^' => self.make_token(TokenType::Caret),
            ',' => self.make_token(TokenType::Comma),
            '#' => self.make_token(TokenType::Pound),
            '-' => self.make_or_default('>', TokenType::Deref, TokenType::Dash),
            '|' => self.make_token(TokenType::Bar),
            '+' => self.make_token(TokenType::Plus),
            ';' => self.make_token(TokenType::Semicolon),
            '/' => self.make_token(TokenType::Slash),
            '~' => self.make_token(TokenType::Tilde),
            '=' => self.make_or_default('>', TokenType::Imply, TokenType::Equ),
            ':' => self.make_or_default('=', TokenType::Assign, TokenType::Colon),
            '>' => self.make_or_default('=', TokenType::GreaterEqu, TokenType::Greater),
            '<' => self.make_or_default('=', TokenType::LessEqu, TokenType::Less),
            '.' => self.make_or_default('.', TokenType::Range, TokenType::Dot),
            '*' => self.make_or_default('*', TokenType::Exp, TokenType::Star),

            _ => {
                eprintln!("{:?} Unrecognized character '{}'", self.cursor, chr);
            }
        }
    }

    /// Makes a token and adds it to the token list
    fn make_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            location: self.cursor.clone(),
        })
    }

    /// Makes the `does_match` token if the char matched, otherwise makes the `no_match` token
    fn make_or_default(&mut self, expect: char, does_match: TokenType, no_match: TokenType) {
        if self.match_next(expect) {
            self.make_token(does_match);
        } else {
            self.make_token(no_match);
        }
    }
}
