//! Scanner for lexing tokens
use std::iter::Peekable;

use logos::Logos;

/// All Tokens scanned by the Scanner
#[derive(Logos, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u16)]
pub enum TokenKind {
    // Character Tokens
    #[token("@")]
    At,
    #[token("->")]
    Arrow,
    #[token("^")]
    Caret,
    #[token(":")]
    Colon,
    #[token(":=")]
    Assign,
    #[token(",")]
    Comma,
    #[token("..")]
    Range,
    #[token(".")]
    Dot,
    #[token("=")]
    Equ,
    #[token(">=")]
    GreaterEqu,
    #[token(">")]
    Greater,
    #[token("#")]
    Pound,
    #[token("=>")]
    Imply,
    #[token("<=")]
    LessEqu,
    #[token("(")]
    LeftParen,
    #[token("<")]
    Less,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token(")")]
    RightParen,
    #[token(";")]
    Semicolon,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("**")]
    Exp,
    #[token("~")]
    Tilde,

    // Keywords
    #[token("addressint")]
    Addressint,
    #[token("all")]
    All,
    #[token("and")]
    And,
    #[token("array")]
    Array,
    #[token("asm")]
    Asm,
    #[token("assert")]
    Assert,
    #[token("begin")]
    Begin,
    #[token("bind")]
    Bind,
    #[token("bits")]
    Bits,
    #[token("body")]
    Body,
    #[token("boolean")]
    Boolean,
    #[token("break")]
    Break,
    #[token("by")]
    By,
    #[token("case")]
    Case,
    #[token("char")]
    Char,
    #[token("cheat")]
    Cheat,
    #[token("checked")]
    Checked,
    #[token("class")]
    Class,
    #[token("close")]
    Close,
    #[token("collection")]
    Collection,
    #[token("condition")]
    Condition,
    #[token("const")]
    Const,
    #[token("decreasing")]
    Decreasing,
    #[token("def")]
    Def,
    #[token("deferred")]
    Deferred,
    #[token("div")]
    Div,
    #[token("elif")]
    Elif, // New keyword typo checking
    #[token("else")]
    Else,
    #[token("elseif")]
    Elseif,
    #[token("elsif")]
    Elsif,
    #[token("end")]
    End,
    #[token("endcase")]
    EndCase, // New keyword typo checking
    #[token("endfor")]
    EndFor,
    #[token("endif")]
    EndIf,
    #[token("endloop")]
    EndLoop,
    #[token("enum")]
    Enum,
    #[token("exit")]
    Exit,
    #[token("export")]
    Export,
    #[token("external")]
    External,
    #[token("false")]
    False,
    #[token("flexible")]
    Flexible,
    #[token("for")]
    For,
    #[token("fork")]
    Fork,
    #[token("forward")]
    Forward,
    #[token("free")]
    Free,
    #[token("fcn")]
    #[token("function")]
    Function,
    #[token("get")]
    Get,
    #[token("handler")]
    Handler,
    #[token("if")]
    If,
    #[token("implement")]
    Implement,
    #[token("import")]
    Import,
    #[token("in")]
    In,
    #[token("include")]
    Include,
    #[token("inherit")]
    Inherit,
    #[token("init")]
    Init,
    #[token("int")]
    Int,
    #[token("int1")]
    Int1,
    #[token("int2")]
    Int2,
    #[token("int4")]
    Int4,
    #[token("invariant")]
    Invariant,
    #[token("label")]
    Label,
    #[token("loop")]
    Loop,
    #[token("mod")]
    Mod,
    #[token("module")]
    Module,
    #[token("monitor")]
    Monitor,
    #[token("nat")]
    Nat,
    #[token("nat1")]
    Nat1,
    #[token("nat2")]
    Nat2,
    #[token("nat4")]
    Nat4,
    #[token("new")]
    New,
    #[token("nil")]
    Nil,
    #[token("not")]
    Not,
    #[token("objectclass")]
    ObjectClass,
    #[token("of")]
    Of,
    #[token("opaque")]
    Opaque,
    #[token("open")]
    Open,
    #[token("or")]
    Or,
    #[token("packed")]
    Packed,
    #[token("pause")]
    Pause,
    #[token("pervasive")]
    Pervasive,
    #[token("pointer")]
    Pointer,
    #[token("post")]
    Post,
    #[token("pre")]
    Pre,
    #[token("priority")]
    Priority,
    #[token("proc")]
    #[token("procedure")]
    Procedure,
    #[token("process")]
    Process,
    #[token("put")]
    Put,
    #[token("quit")]
    Quit,
    #[token("read")]
    Read,
    #[token("real")]
    Real,
    #[token("real4")]
    Real4,
    #[token("real8")]
    Real8,
    #[token("record")]
    Record,
    #[token("register")]
    Register,
    #[token("rem")]
    Rem,
    #[token("result")]
    Result_,
    #[token("return")]
    Return,
    #[token("seek")]
    Seek,
    #[token("self")]
    Self_,
    #[token("set")]
    Set,
    #[token("shl")]
    Shl,
    #[token("shr")]
    Shr,
    #[token("signal")]
    Signal,
    #[token("skip")]
    Skip,
    #[token("string")]
    String_,
    #[token("tag")]
    Tag,
    #[token("tell")]
    Tell,
    #[token("then")]
    Then,
    #[token("timeout")]
    Timeout,
    #[token("to")]
    To,
    #[token("true")]
    True,
    #[token("type")]
    Type,
    #[token("unchecked")]
    Unchecked,
    #[token("union")]
    Union,
    #[token("unit")]
    Unit,
    #[token("unqualified")]
    Unqualified,
    #[token("var")]
    Var,
    #[token("wait")]
    Wait,
    #[token("when")]
    When,
    #[token("write")]
    Write,
    #[token("xor")]
    Xor,

    // Literals
    #[regex("[[:alpha:]_][[:alnum:]_]*")]
    Identifier,
    #[regex("'[^\r\n']*('?)")]
    CharLiteral,
    #[regex("\"[^\r\n\"]*(\"?)")]
    StringLiteral,
    #[regex("\\.?[[:digit:]]+", nom_number_literal)]
    NumberLiteral(NumberKind),

    /// All whitespace (including line delimiters)
    #[regex("[ \t\r\n]+")]
    Whitespace,

    /// All comments
    #[regex("%[^\r\n]*")]
    #[token("/*", lex_block_comment)]
    Comment,

    #[error]
    Error,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberKind {
    Int,
    Real,
    Radix,
}

fn lex_block_comment(lex: &mut logos::Lexer<TokenKind>) {
    // Continue to lex everything
    let mut bump_len = 0_usize;
    let mut comment_nesting = 1_usize;
    let remainder: &str = lex.remainder();

    // Track all the endings
    for in_between in remainder.split_terminator("*/") {
        // Adjust nesting
        comment_nesting = comment_nesting
            .saturating_add(in_between.matches("/*").count())
            .saturating_sub(1);
        // Increase bump length (include `*/`)
        bump_len = bump_len.saturating_add(in_between.len() + 2);

        if comment_nesting == 0 {
            // Outside of nesting depth, done
            // Cap to maximum length of remainder to prevent
            // an out of bounds bump
            lex.bump(bump_len.min(remainder.len()));
            return;
        }
    }

    // died
    lex.bump(bump_len);
}

fn nom_number_literal(lexer: &mut logos::Lexer<TokenKind>) -> NumberKind {
    /// Maybe noms on a character
    fn maybe_nom(
        chars: &mut Peekable<impl Iterator<Item = char>>,
        lexer: &mut logos::Lexer<TokenKind>,
        condition: impl FnOnce(&char) -> bool,
    ) -> bool {
        if chars.peek().map(condition).unwrap_or_default() {
            chars.next();
            lexer.bump(1);
            true
        } else {
            false
        }
    }

    fn maybe_nom_decimals(
        chars: &mut Peekable<impl Iterator<Item = char>>,
        lexer: &mut logos::Lexer<TokenKind>,
    ) {
        let mut bump_count = 0;

        while chars
            .peek()
            .map(|c| ('0'..='9').contains(c))
            .unwrap_or_default()
        {
            chars.next();
            bump_count += 1;
        }

        lexer.bump(bump_count);
    }

    // previous set of chars is a sequence of decimal digits, maybe prefixed
    // with a dot
    let is_fractional_part = lexer.slice().starts_with(".");

    let remainder: &str = lexer.remainder();
    let mut remaining = remainder.chars().peekable();

    match remaining.next() {
        Some('.') if !is_fractional_part && !matches!(remaining.peek(), Some('.')) => {
            // Don't consume if we're already the fractional part, or the dot is part of a '..'

            // bump the dot char
            lexer.bump(1);

            // Bump the decimal portion
            maybe_nom_decimals(&mut remaining, lexer);

            // Maybe nom the exponent portion
            // [eE][+-]?[0-9]*
            if maybe_nom(&mut remaining, lexer, |c| matches!(c, 'e' | 'E')) {
                maybe_nom(&mut remaining, lexer, |c| matches!(c, '+' | '-'));
                maybe_nom_decimals(&mut remaining, lexer);
            }

            NumberKind::Real
        }
        Some('e') | Some('E') => {
            // Bump the 'e'
            lexer.bump(1);

            maybe_nom(&mut remaining, lexer, |c| matches!(c, '+' | '-'));
            maybe_nom_decimals(&mut remaining, lexer);

            NumberKind::Real
        }
        Some('#') => {
            // bump the radix char
            lexer.bump(1);

            // Bump all the radix bits
            lexer.bump(remaining.take_while(|c| c.is_alphanumeric()).count());

            NumberKind::Radix
        }
        _ if is_fractional_part => NumberKind::Real,
        _ => NumberKind::Int,
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: &'src str,
}

impl<'s> Token<'s> {
    pub(crate) fn new(kind: TokenKind, lexeme: &'s str) -> Self {
        Self { kind, lexeme }
    }
}

/// Scanner for tokens
pub struct Scanner<'s> {
    inner: logos::Lexer<'s, TokenKind>,
}

impl<'s> Scanner<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            inner: TokenKind::lexer(source),
        }
    }
}

impl<'s> std::iter::Iterator for Scanner<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some(Token::new(kind, text))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// Runs the lexer over the given text, and asserts if the expected token
    /// has been scanned
    #[track_caller] // Issue isn't here, but from caller
    fn expect(source: &str, kind: &TokenKind) {
        let mut scanner = Scanner::new(source);

        // Should be the expected token & the same source text
        let text = scanner.next();
        assert_eq!(text, Some(Token::new(*kind, source)));
    }

    /// Runs the lexer over the given text, and asserts if the expected tokens
    /// have been scanned
    #[track_caller] // Issue isn't here, but from caller
    fn expect_seq(source: &str, kinds: &[Token]) {
        let scanner = Scanner::new(source);

        // Should be the expected tokens & the same source texts
        let toks: Vec<Token> = scanner.collect();
        assert_eq!(toks.is_empty(), kinds.is_empty());
        assert_eq!(toks, kinds);
    }

    #[test]
    fn scan_whitespace() {
        expect("     ", &TokenKind::Whitespace);
        expect("     \t", &TokenKind::Whitespace);
        expect("   \n   ", &TokenKind::Whitespace);
        expect("   \r\n   ", &TokenKind::Whitespace);
    }

    #[test]
    fn scan_invalid_chars() {
        // Lossless scanning
        expect("[", &TokenKind::Error);
        expect("]", &TokenKind::Error);
        expect("{", &TokenKind::Error);
        expect("}", &TokenKind::Error);
        expect("!", &TokenKind::Error);
        expect("$", &TokenKind::Error);
        expect("?", &TokenKind::Error);
        expect("`", &TokenKind::Error);
        expect("\\", &TokenKind::Error);
        // Was originally `🧑‍🔬` but that gets split up into multiple tokens
        expect("🧑", &TokenKind::Error);
    }

    #[test]
    #[ignore = "no normalization planned yet for identifiers"]
    fn text_normalization() {
        // Normalization issues: `â` and `â` are distinct
        // Text should be normalized, ie both should be characters
        expect("â", &TokenKind::Error);
        expect("â", &TokenKind::Error);
    }

    #[test]
    fn scan_real_literals() {
        // Allow for leading dot
        expect(".12345", &TokenKind::NumberLiteral(NumberKind::Real));
        expect_seq(
            ".12345.6789",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Real), ".12345"),
                Token::new(TokenKind::NumberLiteral(NumberKind::Real), ".6789"),
            ],
        );

        expect("1.", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("100.00", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("100.00e10", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("100.00e100", &TokenKind::NumberLiteral(NumberKind::Real));

        // Negative and positive exponents are valid
        expect("100.00e-100", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("100.00e+100", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("1e100", &TokenKind::NumberLiteral(NumberKind::Real));

        // Invalid format
        expect("1e+", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("1e-", &TokenKind::NumberLiteral(NumberKind::Real));
        expect("1e", &TokenKind::NumberLiteral(NumberKind::Real));

        // Don't consume extra '+', '-' or decimal digits
        expect_seq(
            "1.0+",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Real), "1.0"),
                Token::new(TokenKind::Plus, "+"),
            ],
        );
        expect_seq(
            "1.0-",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Real), "1.0"),
                Token::new(TokenKind::Minus, "-"),
            ],
        );
        expect_seq(
            "1.0-1000",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Real), "1.0"),
                Token::new(TokenKind::Minus, "-"),
                Token::new(TokenKind::NumberLiteral(NumberKind::Int), "1000"),
            ],
        );

        // Too big (not captured here)
        expect("1e600", &TokenKind::NumberLiteral(NumberKind::Real));
    }

    #[test]
    fn scan_radix_ints() {
        expect("16#EABC", &TokenKind::NumberLiteral(NumberKind::Radix));

        // Overflow
        expect(
            "10#99999999999999999999",
            &TokenKind::NumberLiteral(NumberKind::Radix),
        );

        // All errors below here are not captured in the scanner

        // No digits
        expect("30#", &TokenKind::NumberLiteral(NumberKind::Radix));

        // Out of range (> 36)
        expect("37#asda", &TokenKind::NumberLiteral(NumberKind::Radix));

        // Out of range (= 0)
        expect("0#0000", &TokenKind::NumberLiteral(NumberKind::Radix));

        // Out of range (= 1)
        expect("1#0000", &TokenKind::NumberLiteral(NumberKind::Radix));

        // Out of range (= overflow)
        expect(
            "18446744073709551616#0000",
            &TokenKind::NumberLiteral(NumberKind::Radix),
        );

        // Invalid digit
        expect("10#999a999", &TokenKind::NumberLiteral(NumberKind::Radix));
    }

    #[test]
    fn scan_basic_ints() {
        expect("01234560", &TokenKind::NumberLiteral(NumberKind::Int));

        // Overflow, not detected
        expect(
            "99999999999999999999",
            &TokenKind::NumberLiteral(NumberKind::Int),
        );

        // Digit cutoff
        expect_seq(
            "999a999",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Int), "999"),
                Token::new(TokenKind::Identifier, "a999"),
            ],
        );
    }

    #[test]
    fn scan_literal_and_range() {
        expect_seq(
            "..1",
            &[
                Token::new(TokenKind::Range, ".."),
                Token::new(TokenKind::NumberLiteral(NumberKind::Int), "1"),
            ],
        );
        expect_seq(
            "1..",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Int), "1"),
                Token::new(TokenKind::Range, ".."),
            ],
        );
        expect_seq(
            "1eggy",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Real), "1e"),
                Token::new(TokenKind::Identifier, "ggy"),
            ],
        );
    }

    #[test]
    fn scan_punct() {
        expect("@", &TokenKind::At);
        expect("->", &TokenKind::Arrow);
        expect("^", &TokenKind::Caret);
        expect(":", &TokenKind::Colon);
        expect(":=", &TokenKind::Assign);
        expect(",", &TokenKind::Comma);
        expect(".", &TokenKind::Dot);
        expect("..", &TokenKind::Range);
        expect("=", &TokenKind::Equ);
        expect(">=", &TokenKind::GreaterEqu);
        expect(">", &TokenKind::Greater);
        expect("#", &TokenKind::Pound);
        expect("=>", &TokenKind::Imply);
        expect("<=", &TokenKind::LessEqu);
        expect("(", &TokenKind::LeftParen);
        expect("<", &TokenKind::Less);
        expect("-", &TokenKind::Minus);
        expect("+", &TokenKind::Plus);
        expect(")", &TokenKind::RightParen);
        expect(";", &TokenKind::Semicolon);
        expect("/", &TokenKind::Slash);
        expect("*", &TokenKind::Star);
        expect("**", &TokenKind::Exp);
        expect("~", &TokenKind::Tilde);
    }

    #[test]
    fn scan_keywords() {
        expect("addressint", &TokenKind::Addressint);
        expect("all", &TokenKind::All);
        expect("and", &TokenKind::And);
        expect("array", &TokenKind::Array);
        expect("asm", &TokenKind::Asm);
        expect("assert", &TokenKind::Assert);
        expect("begin", &TokenKind::Begin);
        expect("bind", &TokenKind::Bind);
        expect("bits", &TokenKind::Bits);
        expect("body", &TokenKind::Body);
        expect("boolean", &TokenKind::Boolean);
        expect("break", &TokenKind::Break);
        expect("by", &TokenKind::By);
        expect("case", &TokenKind::Case);
        expect("char", &TokenKind::Char);
        expect("cheat", &TokenKind::Cheat);
        expect("checked", &TokenKind::Checked);
        expect("class", &TokenKind::Class);
        expect("close", &TokenKind::Close);
        expect("collection", &TokenKind::Collection);
        expect("condition", &TokenKind::Condition);
        expect("const", &TokenKind::Const);
        expect("decreasing", &TokenKind::Decreasing);
        expect("def", &TokenKind::Def);
        expect("deferred", &TokenKind::Deferred);
        expect("div", &TokenKind::Div);
        expect("elif", &TokenKind::Elif);
        expect("else", &TokenKind::Else);
        expect("elseif", &TokenKind::Elseif);
        expect("elsif", &TokenKind::Elsif);
        expect("end", &TokenKind::End);
        expect("endcase", &TokenKind::EndCase);
        expect("endfor", &TokenKind::EndFor);
        expect("endif", &TokenKind::EndIf);
        expect("endloop", &TokenKind::EndLoop);
        expect("enum", &TokenKind::Enum);
        expect("exit", &TokenKind::Exit);
        expect("export", &TokenKind::Export);
        expect("external", &TokenKind::External);
        expect("false", &TokenKind::False);
        expect("flexible", &TokenKind::Flexible);
        expect("for", &TokenKind::For);
        expect("fork", &TokenKind::Fork);
        expect("forward", &TokenKind::Forward);
        expect("free", &TokenKind::Free);
        expect("function", &TokenKind::Function);
        expect("get", &TokenKind::Get);
        expect("handler", &TokenKind::Handler);
        expect("if", &TokenKind::If);
        expect("implement", &TokenKind::Implement);
        expect("import", &TokenKind::Import);
        expect("in", &TokenKind::In);
        expect("include", &TokenKind::Include);
        expect("inherit", &TokenKind::Inherit);
        expect("init", &TokenKind::Init);
        expect("int", &TokenKind::Int);
        expect("int1", &TokenKind::Int1);
        expect("int2", &TokenKind::Int2);
        expect("int4", &TokenKind::Int4);
        expect("invariant", &TokenKind::Invariant);
        expect("label", &TokenKind::Label);
        expect("loop", &TokenKind::Loop);
        expect("mod", &TokenKind::Mod);
        expect("module", &TokenKind::Module);
        expect("monitor", &TokenKind::Monitor);
        expect("nat", &TokenKind::Nat);
        expect("nat1", &TokenKind::Nat1);
        expect("nat2", &TokenKind::Nat2);
        expect("nat4", &TokenKind::Nat4);
        expect("new", &TokenKind::New);
        expect("nil", &TokenKind::Nil);
        expect("not", &TokenKind::Not);
        expect("objectclass", &TokenKind::ObjectClass);
        expect("of", &TokenKind::Of);
        expect("opaque", &TokenKind::Opaque);
        expect("open", &TokenKind::Open);
        expect("or", &TokenKind::Or);
        expect("packed", &TokenKind::Packed);
        expect("pause", &TokenKind::Pause);
        expect("pervasive", &TokenKind::Pervasive);
        expect("pointer", &TokenKind::Pointer);
        expect("post", &TokenKind::Post);
        expect("pre", &TokenKind::Pre);
        expect("priority", &TokenKind::Priority);
        expect("procedure", &TokenKind::Procedure);
        expect("process", &TokenKind::Process);
        expect("put", &TokenKind::Put);
        expect("quit", &TokenKind::Quit);
        expect("read", &TokenKind::Read);
        expect("real", &TokenKind::Real);
        expect("real4", &TokenKind::Real4);
        expect("real8", &TokenKind::Real8);
        expect("record", &TokenKind::Record);
        expect("register", &TokenKind::Register);
        expect("rem", &TokenKind::Rem);
        expect("result", &TokenKind::Result_);
        expect("return", &TokenKind::Return);
        expect("seek", &TokenKind::Seek);
        expect("self", &TokenKind::Self_);
        expect("set", &TokenKind::Set);
        expect("shl", &TokenKind::Shl);
        expect("shr", &TokenKind::Shr);
        expect("signal", &TokenKind::Signal);
        expect("skip", &TokenKind::Skip);
        expect("string", &TokenKind::String_);
        expect("tag", &TokenKind::Tag);
        expect("tell", &TokenKind::Tell);
        expect("then", &TokenKind::Then);
        expect("timeout", &TokenKind::Timeout);
        expect("to", &TokenKind::To);
        expect("true", &TokenKind::True);
        expect("type", &TokenKind::Type);
        expect("unchecked", &TokenKind::Unchecked);
        expect("union", &TokenKind::Union);
        expect("unit", &TokenKind::Unit);
        expect("unqualified", &TokenKind::Unqualified);
        expect("var", &TokenKind::Var);
        expect("wait", &TokenKind::Wait);
        expect("when", &TokenKind::When);
        expect("write", &TokenKind::Write);
        expect("xor", &TokenKind::Xor);
    }

    #[test]
    fn scan_identifiers() {
        expect("_source_text", &TokenKind::Identifier);
        // Skip over initial digits
        expect_seq(
            "0123_separate",
            &[
                Token::new(TokenKind::NumberLiteral(NumberKind::Int), "0123"),
                Token::new(TokenKind::Identifier, "_separate"),
            ],
        );
        // Invalid character, but "ba" & "e" should still be parsed
        expect_seq(
            "ba$e",
            &[
                Token::new(TokenKind::Identifier, "ba"),
                Token::new(TokenKind::Error, "$"),
                Token::new(TokenKind::Identifier, "e"),
            ],
        );
    }

    #[test]
    fn scan_line_comments() {
        expect_seq(
            "% abcd asd\n asd",
            &[
                Token::new(TokenKind::Comment, "% abcd asd"),
                Token::new(TokenKind::Whitespace, "\n "),
                Token::new(TokenKind::Identifier, "asd"),
            ],
        );

        expect("% abcd asd", &TokenKind::Comment);
    }

    #[test]
    fn scan_block_comments() {
        expect_seq(
            "/* /* abcd % * / \n\n\r\n */ */ asd",
            &[
                Token::new(TokenKind::Comment, "/* /* abcd % * / \n\n\r\n */ */"),
                Token::new(TokenKind::Whitespace, " "),
                Token::new(TokenKind::Identifier, "asd"),
            ],
        );

        // Sibling nodes
        expect("/* /* abcd */ /* ae */ */", &TokenKind::Comment);

        // Missing terminating */
        expect("/* /* abcd */", &TokenKind::Comment);
        expect("/* /* abcd */ ", &TokenKind::Comment);
    }

    #[test]
    fn scan_string_literal() {
        // String literal parsing
        expect_seq(
            r#""abcd💖"a"#,
            &[
                Token::new(TokenKind::StringLiteral, r#""abcd💖""#),
                Token::new(TokenKind::Identifier, r#"a"#),
            ],
        );

        // Invalid scanning should make a literal from the successfully parsed character

        // Ends at the end of line
        expect_seq(
            "\"abcd\n",
            &[
                Token::new(TokenKind::StringLiteral, "\"abcd"),
                Token::new(TokenKind::Whitespace, "\n"),
            ],
        );

        expect_seq(
            "\"abcd\r\n",
            &[
                Token::new(TokenKind::StringLiteral, "\"abcd"),
                Token::new(TokenKind::Whitespace, "\r\n"),
            ],
        );

        // Ends at the end of file
        expect("\"abcd", &TokenKind::StringLiteral);

        // Mismatched delimiter
        expect("\"abcd'", &TokenKind::StringLiteral);
    }

    #[test]
    fn scan_char_literal() {
        // Char(n) literal parsing
        expect_seq(
            "'abcd💖'a",
            &[
                Token::new(TokenKind::CharLiteral, "'abcd💖'"),
                Token::new(TokenKind::Identifier, "a"),
            ],
        );

        // Invalid scanning should make a literal from the successfully parsed characters

        // Ends at the end of line
        expect_seq(
            "'abcd\n",
            &[
                Token::new(TokenKind::CharLiteral, "'abcd"),
                Token::new(TokenKind::Whitespace, "\n"),
            ],
        );

        expect_seq(
            "'abcd\r\n",
            &[
                Token::new(TokenKind::CharLiteral, "'abcd"),
                Token::new(TokenKind::Whitespace, "\r\n"),
            ],
        );

        // Ends at the end of file
        expect("'abcd", &TokenKind::CharLiteral);

        // Mismatched delimiter
        expect("'abcd\"", &TokenKind::CharLiteral);
    }

    #[test]
    fn token_aliases() {
        // Aliases
        expect("fcn", &TokenKind::Function);
        expect("proc", &TokenKind::Procedure);
    }
}
