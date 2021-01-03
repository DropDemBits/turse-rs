//! Scanner for lexing tokens
pub mod token;

use logos::Logos;
use std::convert::TryFrom;
use text_size::{TextRange, TextSize};
use token::{NumberKind, Token, TokenKind};

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
        let kind = match self.inner.next()? {
            TokenKind::NumberLiteral(number_kind) => match number_kind {
                NumberKind::Int => TokenKind::IntLiteral,
                NumberKind::Radix => TokenKind::RadixLiteral,
                NumberKind::Real => TokenKind::RealLiteral,
            },
            other => other,
        };

        let text = self.inner.slice();
        let range = self.inner.span();
        let (start, end) = (
            TextSize::try_from(range.start).unwrap(),
            TextSize::try_from(range.end).unwrap(),
        );
        let range = TextRange::new(start, end);

        Some(Token::new(kind, text, range))
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
        let token = scanner.next().unwrap();
        assert_eq!(token.kind, *kind);
        assert_eq!(token.lexeme, source);
    }

    /// Runs the lexer over the given text, and asserts if the expected tokens
    /// have been scanned
    #[track_caller] // Issue isn't here, but from caller
    fn expect_seq(source: &str, kinds: &[(TokenKind, &str)]) {
        let scanner = Scanner::new(source);

        // Should be the expected tokens & the same source texts
        let toks: Vec<(TokenKind, &str)> = scanner.map(|tok| (tok.kind, tok.lexeme)).collect();
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
        expect(".12345", &TokenKind::RealLiteral);
        expect_seq(
            ".12345.6789",
            &[
                (TokenKind::RealLiteral, ".12345"),
                (TokenKind::RealLiteral, ".6789"),
            ],
        );

        expect("1.", &TokenKind::RealLiteral);
        expect("100.00", &TokenKind::RealLiteral);
        expect("100.00e10", &TokenKind::RealLiteral);
        expect("100.00e100", &TokenKind::RealLiteral);

        // Negative and positive exponents are valid
        expect("100.00e-100", &TokenKind::RealLiteral);
        expect("100.00e+100", &TokenKind::RealLiteral);
        expect("1e100", &TokenKind::RealLiteral);

        // Invalid format
        expect("1e+", &TokenKind::RealLiteral);
        expect("1e-", &TokenKind::RealLiteral);
        expect("1e", &TokenKind::RealLiteral);

        // Don't consume extra '+', '-' or decimal digits
        expect_seq(
            "1.0+",
            &[(TokenKind::RealLiteral, "1.0"), (TokenKind::Plus, "+")],
        );
        expect_seq(
            "1.0-",
            &[(TokenKind::RealLiteral, "1.0"), (TokenKind::Minus, "-")],
        );
        expect_seq(
            "1.0-1000",
            &[
                (TokenKind::RealLiteral, "1.0"),
                (TokenKind::Minus, "-"),
                (TokenKind::IntLiteral, "1000"),
            ],
        );

        // Too big (not captured here)
        expect("1e600", &TokenKind::RealLiteral);
    }

    #[test]
    fn scan_radix_ints() {
        expect("16#EABC", &TokenKind::RadixLiteral);

        // Overflow
        expect("10#99999999999999999999", &TokenKind::RadixLiteral);

        // All errors below here are not captured in the scanner

        // No digits
        expect("30#", &TokenKind::RadixLiteral);

        // Out of range (> 36)
        expect("37#asda", &TokenKind::RadixLiteral);

        // Out of range (= 0)
        expect("0#0000", &TokenKind::RadixLiteral);

        // Out of range (= 1)
        expect("1#0000", &TokenKind::RadixLiteral);

        // Out of range (= overflow)
        expect("18446744073709551616#0000", &TokenKind::RadixLiteral);

        // Invalid digit
        expect("10#999a999", &TokenKind::RadixLiteral);
    }

    #[test]
    fn scan_basic_ints() {
        expect("01234560", &TokenKind::IntLiteral);

        // Overflow, not detected
        expect("99999999999999999999", &TokenKind::IntLiteral);

        // Digit cutoff
        expect_seq(
            "999a999",
            &[
                (TokenKind::IntLiteral, "999"),
                (TokenKind::Identifier, "a999"),
            ],
        );
    }

    #[test]
    fn scan_literal_and_range() {
        expect_seq(
            "..1",
            &[(TokenKind::Range, ".."), (TokenKind::IntLiteral, "1")],
        );
        expect_seq(
            "1..",
            &[(TokenKind::IntLiteral, "1"), (TokenKind::Range, "..")],
        );
        expect_seq(
            "1eggy",
            &[
                (TokenKind::RealLiteral, "1e"),
                (TokenKind::Identifier, "ggy"),
            ],
        );
    }

    #[test]
    fn scan_punct() {
        expect("@", &TokenKind::At);
        expect("&", &TokenKind::Ampersand);
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
        expect("|", &TokenKind::Pipe);
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
                (TokenKind::IntLiteral, "0123"),
                (TokenKind::Identifier, "_separate"),
            ],
        );
        // Invalid character, but "ba" & "e" should still be parsed
        expect_seq(
            "ba$e",
            &[
                (TokenKind::Identifier, "ba"),
                (TokenKind::Error, "$"),
                (TokenKind::Identifier, "e"),
            ],
        );
    }

    #[test]
    fn scan_line_comments() {
        expect_seq(
            "% abcd asd\n asd",
            &[
                (TokenKind::Comment, "% abcd asd"),
                (TokenKind::Whitespace, "\n "),
                (TokenKind::Identifier, "asd"),
            ],
        );

        expect("% abcd asd", &TokenKind::Comment);
    }

    #[test]
    fn scan_block_comments() {
        expect_seq(
            "/* /* abcd % * / \n\n\r\n */ */ asd",
            &[
                (TokenKind::Comment, "/* /* abcd % * / \n\n\r\n */ */"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Identifier, "asd"),
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
                (TokenKind::StringLiteral, r#""abcd💖""#),
                (TokenKind::Identifier, r#"a"#),
            ],
        );

        // Invalid scanning should make a literal from the successfully parsed character

        // Ends at the end of line
        expect_seq(
            "\"abcd\n",
            &[
                (TokenKind::StringLiteral, "\"abcd"),
                (TokenKind::Whitespace, "\n"),
            ],
        );

        expect_seq(
            "\"abcd\r\n",
            &[
                (TokenKind::StringLiteral, "\"abcd"),
                (TokenKind::Whitespace, "\r\n"),
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
                (TokenKind::CharLiteral, "'abcd💖'"),
                (TokenKind::Identifier, "a"),
            ],
        );

        // Invalid scanning should make a literal from the successfully parsed characters

        // Ends at the end of line
        expect_seq(
            "'abcd\n",
            &[
                (TokenKind::CharLiteral, "'abcd"),
                (TokenKind::Whitespace, "\n"),
            ],
        );

        expect_seq(
            "'abcd\r\n",
            &[
                (TokenKind::CharLiteral, "'abcd"),
                (TokenKind::Whitespace, "\r\n"),
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

    // Picked up by the scanner

    #[test]
    fn block_comment_missing_endings() {
        expect("/*/*", &TokenKind::Comment);
    }

    #[test]
    fn radix_literal_dont_parse_non_ascii_chars() {
        expect_seq(
            "2#پ",
            &[(TokenKind::RadixLiteral, "2#"), (TokenKind::Error, "پ")],
        );
    }
}
