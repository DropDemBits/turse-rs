//! Token related items
use super::ErrorFerry;
pub use text_size::TextRange as TokenRange;

use logos::Logos;
use std::convert::TryFrom;
use std::fmt;
use std::iter::Peekable;
use std::ops::Range;
use text_size::TextSize;

#[derive(Debug, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: &'src str,
    pub range: TokenRange,
}

impl<'s> Token<'s> {
    pub(crate) fn new(kind: TokenKind, lexeme: &'s str, range: TokenRange) -> Self {
        Self {
            kind,
            lexeme,
            range,
        }
    }
}

/// All Tokens scanned by the Scanner
#[derive(Logos, Debug, Copy, Clone, PartialEq, Eq)]
#[logos(extras = ErrorFerry)]
#[repr(u16)]
pub enum TokenKind {
    // Character Tokens
    #[token("@")]
    At,
    #[token("&")]
    Ampersand,
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
    #[token("|")]
    Pipe,
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
    #[token("sizeof")]
    SizeOf,
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
    #[regex("'", nom_char_literal)]
    CharLiteral,
    #[regex("\"", nom_string_literal)]
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

    // Preprocessor
    #[token("#if")]
    PreprocIf,
    #[token("#elseif")]
    PreprocElseIf,
    #[token("#elsif")]
    PreprocElsIf,
    #[token("#else")]
    PreprocElse,
    #[token("#end")]
    PreprocEnd,
    #[token("#endif")]
    PreprocEndIf,

    #[error]
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberKind {
    Int,
    Radix,
    Real,
}

fn lex_block_comment(lexer: &mut logos::Lexer<TokenKind>) {
    // Continue to lex everything
    let mut bump_len = 0_usize;
    let mut comment_nesting = 1_usize;
    let mut remainder: &str = lexer.remainder();

    // Track all the endings
    // TODO: replace with `split_inclusive` once it gets stablilized
    while !remainder.is_empty() {
        let at = remainder
            .find("*/")
            .map_or_else(|| remainder.len(), |at| at.saturating_add(2));
        let (in_between, new_remainder) = remainder.split_at(at);
        remainder = new_remainder;

        // Adjust nesting
        comment_nesting = comment_nesting
            .saturating_add(in_between.matches("/*").count())
            .saturating_sub(1);
        // Increase bump length (`*/` is included)
        bump_len = bump_len.saturating_add(in_between.len());

        if comment_nesting == 0 {
            // Outside of nesting depth, done
            // Cap to maximum length of remainder to prevent
            // an out of bounds bump
            lexer.bump(bump_len.min(lexer.remainder().len()));
            return;
        }
    }

    // missing terminator
    lexer.bump(bump_len);

    lexer
        .extras
        .push_error("block comment is missing terminating ’*/’", lexer.span());
}

fn nom_char_literal(lexer: &mut logos::Lexer<TokenKind>) {
    let (bump_count, is_terminated, is_terminator_escaped) =
        check_charseq_termination(lexer.remainder(), '\'');
    lexer.bump(bump_count);

    if !is_terminated {
        // report error!
        if is_terminator_escaped {
            lexer.extras.push_error(
                "char literal is missing terminator (terminator is escaped)",
                lexer.span(),
            );
        } else {
            lexer
                .extras
                .push_error("char literal is missing terminator", lexer.span());
        }
    }
}

fn nom_string_literal(lexer: &mut logos::Lexer<TokenKind>) {
    let (bump_count, is_terminated, is_terminator_escaped) =
        check_charseq_termination(lexer.remainder(), '"');
    lexer.bump(bump_count);

    if !is_terminated {
        // report error!
        if is_terminator_escaped {
            lexer.extras.push_error(
                "string literal is missing terminator (terminator is escaped)",
                lexer.span(),
            );
        } else {
            lexer
                .extras
                .push_error("string literal is missing terminator", lexer.span());
        }
    }
}

fn check_charseq_termination(remainder: &str, terminator: char) -> (usize, bool, bool) {
    let mut chars = remainder.char_indices().peekable();
    let mut is_escaped = false;

    while let Some((bump_to, chr)) = chars.next() {
        match chr {
            '\\' => {
                let mut slash_count = 1_usize;

                while let Some((_, '\\')) = chars.peek() {
                    slash_count += 1;
                    chars.next();
                }

                if slash_count % 2 == 1 {
                    // possibly escaped terminator
                    if let Some((_, c)) = chars.peek() {
                        if *c == terminator {
                            // is escaped, nom on it
                            is_escaped = true;
                            chars.next();
                        }
                    }
                }
            }
            '\r' | '\n' => {
                // also a terminating char, but not the right one
                return (bump_to, false, is_escaped);
            }
            c if c == terminator => {
                // is terminated (include terminator)
                // safe to add by 1 since '\n' is always a 1 byte char in utf-8
                return (bump_to + 1, true, false);
            }
            _ => {}
        }
    }

    // bump it all, not terminated, maybe escaped
    (remainder.len(), false, is_escaped)
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
    ) -> bool {
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

        bump_count > 0
    }

    // previous set of chars is a sequence of decimal digits, maybe prefixed
    // with a dot
    let is_fractional_part = lexer.slice().starts_with('.');

    let remainder: &str = lexer.remainder();
    let mut remaining = remainder.chars().peekable();

    let kind = match remaining.next() {
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
            lexer.bump(remaining.take_while(|c| c.is_ascii_alphanumeric()).count());

            NumberKind::Radix
        }
        _ if is_fractional_part => NumberKind::Real,
        _ => NumberKind::Int,
    };

    // Validate literal
    match kind {
        NumberKind::Real => {
            // Check that the real literal is valid
            let value = lexical::parse_lossy::<f64, _>(&lexer.slice());
            match value {
                Ok(num) if num.is_infinite() => lexer
                    .extras
                    .push_error("real literal is too large", lexer.span()),
                Ok(num) if num.is_nan() => lexer
                    .extras
                    .push_error("invalid real literal", lexer.span()),
                Err(err) => match err.code {
                    lexical::ErrorCode::Overflow => lexer
                        .extras
                        .push_error("real literal is too large", lexer.span()),
                    lexical::ErrorCode::Underflow => lexer
                        .extras
                        .push_error("real literal is too small", lexer.span()),
                    lexical::ErrorCode::EmptyExponent => lexer
                        .extras
                        .push_error("real literal is missing exponent digits", lexer.span()),
                    _ => lexer
                        .extras
                        .push_error("invalid real literal", lexer.span()),
                    // all other cases are protected by what is parsed, but still push out an error
                },
                Ok(_) => {}
            }
        }
        NumberKind::Int => {
            // Check that the int literal is valid
            let value = lexical::parse::<u64, _>(&lexer.slice());
            if let Err(err) = value {
                match err.code {
                    lexical::ErrorCode::Overflow => lexer
                        .extras
                        .push_error("int literal is too large", lexer.span()),
                    _ => lexer.extras.push_error("invalid int literal", lexer.span()),
                }
            }
        }
        NumberKind::Radix => {
            let slice: &str = lexer.slice();
            let (radix_slice, digits_slice) = slice.split_at(slice.find('#').unwrap());
            let digits_slice = &digits_slice[1..]; // skip over #

            let radix = lexical::parse::<u8, _>(radix_slice);
            match radix {
                Ok(radix) if (2..=36).contains(&radix) => {
                    // valid radix
                    let value = lexical::parse_radix::<u64, _>(&digits_slice, radix);
                    match value {
                        Err(err) => match err.code {
                            lexical::ErrorCode::Overflow => lexer
                                .extras
                                .push_error("explicit int literal is too large", lexer.span()),
                            lexical::ErrorCode::InvalidDigit => {
                                // Get the exact span of the invalid digit
                                // account for width of the radix & '#'
                                let start_slice = radix_slice.len() + 1 + err.index;
                                let end_slice = start_slice + 1;

                                // Chars are guarranteed to be in the ascii range
                                assert!(lexer.slice().get(start_slice..end_slice).is_some());

                                // Adjust based off of the offset of the span
                                let start_slice = start_slice.saturating_add(lexer.span().start);
                                let end_slice = end_slice.saturating_add(lexer.span().start);

                                lexer.extras.push_error(
                                    "invalid digit for the specified base",
                                    start_slice..end_slice,
                                )
                            }
                            lexical::ErrorCode::Empty => lexer.extras.push_error(
                                "explicit int literal is missing radix digits",
                                lexer.span(),
                            ),
                            _ => lexer.extras.push_error("invalid int literal", lexer.span()),
                        },
                        Ok(_) => { /* valid literal, don't do anything */ }
                    }
                }
                _ => {
                    // invalid radix value
                    lexer
                        .extras
                        .push_error("base for int literal is not between 2 - 36", lexer.span())
                }
            }
        }
    }

    kind
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment | Self::Error)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TokenKind::At => "’@’",
            TokenKind::Ampersand => "’&’",
            TokenKind::Arrow => "’->’",
            TokenKind::Caret => "’^’",
            TokenKind::Colon => "’:’",
            TokenKind::Assign => "’:=’",
            TokenKind::Comma => "’,’",
            TokenKind::Range => "’..’",
            TokenKind::Dot => "’.’",
            TokenKind::Equ => "’=’",
            TokenKind::GreaterEqu => "’>=’",
            TokenKind::Greater => "’>’",
            TokenKind::Pound => "’#’",
            TokenKind::Imply => "’=>’",
            TokenKind::LessEqu => "’<=’",
            TokenKind::LeftParen => "’(’",
            TokenKind::Less => "’<’",
            TokenKind::Minus => "’-’",
            TokenKind::Plus => "’+’",
            TokenKind::Pipe => "’|’",
            TokenKind::RightParen => "’)’",
            TokenKind::Semicolon => "’;’",
            TokenKind::Slash => "’/’",
            TokenKind::Star => "’*’",
            TokenKind::Exp => "’**’",
            TokenKind::Tilde => "’~’",
            TokenKind::Addressint => "’addressint’",
            TokenKind::All => "’all’",
            TokenKind::And => "’and’",
            TokenKind::Array => "’array’",
            TokenKind::Asm => "’asm’",
            TokenKind::Assert => "’assert’",
            TokenKind::Begin => "’begin’",
            TokenKind::Bind => "’bind’",
            TokenKind::Bits => "’bits’",
            TokenKind::Body => "’body’",
            TokenKind::Boolean => "’boolean’",
            TokenKind::Break => "’break’",
            TokenKind::By => "’by’",
            TokenKind::Case => "’case’",
            TokenKind::Char => "’char’",
            TokenKind::Cheat => "’cheat’",
            TokenKind::Checked => "’checked’",
            TokenKind::Class => "’class’",
            TokenKind::Close => "’close’",
            TokenKind::Collection => "’collection’",
            TokenKind::Condition => "’condition’",
            TokenKind::Const => "’const’",
            TokenKind::Decreasing => "’decreasing’",
            TokenKind::Def => "’def’",
            TokenKind::Deferred => "’deferred’",
            TokenKind::Div => "’div’",
            TokenKind::Elif => "’elif’",
            TokenKind::Else => "’else’",
            TokenKind::Elseif => "’elseif’",
            TokenKind::Elsif => "’elsif’",
            TokenKind::End => "’end’",
            TokenKind::EndCase => "’endcase’",
            TokenKind::EndFor => "’endfor’",
            TokenKind::EndIf => "’endif’",
            TokenKind::EndLoop => "’endloop’",
            TokenKind::Enum => "’enum’",
            TokenKind::Exit => "’exit’",
            TokenKind::Export => "’export’",
            TokenKind::External => "’external’",
            TokenKind::False => "’false’",
            TokenKind::Flexible => "’flexible’",
            TokenKind::For => "’for’",
            TokenKind::Fork => "’fork’",
            TokenKind::Forward => "’forward’",
            TokenKind::Free => "’free’",
            TokenKind::Function => "’function’",
            TokenKind::Get => "’get’",
            TokenKind::Handler => "’handler’",
            TokenKind::If => "’if’",
            TokenKind::Implement => "’implement’",
            TokenKind::Import => "’import’",
            TokenKind::In => "’in’",
            TokenKind::Include => "’include’",
            TokenKind::Inherit => "’inherit’",
            TokenKind::Init => "’init’",
            TokenKind::Int => "’int’",
            TokenKind::Int1 => "’int1’",
            TokenKind::Int2 => "’int2’",
            TokenKind::Int4 => "’int4’",
            TokenKind::Invariant => "’invariant’",
            TokenKind::Label => "’label’",
            TokenKind::Loop => "’loop’",
            TokenKind::Mod => "’mod’",
            TokenKind::Module => "’module’",
            TokenKind::Monitor => "’monitor’",
            TokenKind::Nat => "’nat’",
            TokenKind::Nat1 => "’nat1’",
            TokenKind::Nat2 => "’nat2’",
            TokenKind::Nat4 => "’nat4’",
            TokenKind::New => "’new’",
            TokenKind::Nil => "’nil’",
            TokenKind::Not => "’not’",
            TokenKind::ObjectClass => "’objectclass’",
            TokenKind::Of => "’of’",
            TokenKind::Opaque => "’opaque’",
            TokenKind::Open => "’open’",
            TokenKind::Or => "’or’",
            TokenKind::Packed => "’packed’",
            TokenKind::Pause => "’pause’",
            TokenKind::Pervasive => "’pervasive’",
            TokenKind::Pointer => "’pointer’",
            TokenKind::Post => "’post’",
            TokenKind::Pre => "’pre’",
            TokenKind::Priority => "’priority’",
            TokenKind::Procedure => "’procedure’",
            TokenKind::Process => "’process’",
            TokenKind::Put => "’put’",
            TokenKind::Quit => "’quit’",
            TokenKind::Read => "’read’",
            TokenKind::Real => "’real’",
            TokenKind::Real4 => "’real4’",
            TokenKind::Real8 => "’real8’",
            TokenKind::Record => "’record’",
            TokenKind::Register => "’register’",
            TokenKind::Rem => "’rem’",
            TokenKind::Result_ => "’result’",
            TokenKind::Return => "’return’",
            TokenKind::Seek => "’seek’",
            TokenKind::Self_ => "’self’",
            TokenKind::Set => "’set’",
            TokenKind::Shl => "’shl’",
            TokenKind::Shr => "’shr’",
            TokenKind::Signal => "’signal’",
            TokenKind::SizeOf => "’sizeof’",
            TokenKind::Skip => "’skip’",
            TokenKind::String_ => "’string’",
            TokenKind::Tag => "’tag’",
            TokenKind::Tell => "’tell’",
            TokenKind::Then => "’then’",
            TokenKind::Timeout => "’timeout’",
            TokenKind::To => "’to’",
            TokenKind::True => "’true’",
            TokenKind::Type => "’type’",
            TokenKind::Unchecked => "’unchecked’",
            TokenKind::Union => "’union’",
            TokenKind::Unit => "’unit’",
            TokenKind::Unqualified => "’unqualified’",
            TokenKind::Var => "’var’",
            TokenKind::Wait => "’wait’",
            TokenKind::When => "’when’",
            TokenKind::Write => "’write’",
            TokenKind::Xor => "’xor’",
            TokenKind::Identifier => "identifier",
            TokenKind::CharLiteral => "char literal",
            TokenKind::StringLiteral => "string literal",
            TokenKind::IntLiteral => "int literal",
            TokenKind::RealLiteral => "real literal",
            TokenKind::RadixLiteral => "explicit int literal",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Comment => "comment",
            TokenKind::Error => "invalid token",
            TokenKind::NumberLiteral(_) => unreachable!("never passed through"),
            TokenKind::PreprocIf => "#if",
            TokenKind::PreprocElseIf => "#elseif",
            TokenKind::PreprocElsIf => "#elsif",
            TokenKind::PreprocElse => "#else",
            TokenKind::PreprocEnd => "#end",
            TokenKind::PreprocEndIf => "#endif",
        })
    }
}

pub(super) fn span_to_text_range(span: Range<usize>) -> TokenRange {
    let (start, end) = (
        TextSize::try_from(span.start).unwrap(),
        TextSize::try_from(span.end).unwrap(),
    );
    TokenRange::new(start, end)
}
