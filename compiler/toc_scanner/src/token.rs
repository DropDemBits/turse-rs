//! Token related items
use logos::Logos;
use std::iter::Peekable;

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

    // Produced by `NumberLiteral`
    IntLiteral,
    RealLiteral,
    RadixLiteral,

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberKind {
    Int,
    Radix,
    Real,
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
    let is_fractional_part = lexer.slice().starts_with('.');

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
