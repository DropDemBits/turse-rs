//! Extensions to the generated nodes
use std::iter;

use super::nodes::*;
use crate::ast::{helper, AstNode};
use crate::{
    AssignOp, CharSeqParseError, InfixOp, IoKind, LiteralParseError, LiteralValue, PrefixOp,
    PrimitiveKind, SyntaxElement, SyntaxKind, SyntaxToken,
};

impl PPBinaryExpr {
    pub fn lhs(&self) -> Option<PPExpr> {
        helper::nodes(self.syntax()).next()
    }

    pub fn op_kind(&self) -> Option<InfixOp> {
        let op = self.op_node()?;

        match op.kind() {
            SyntaxKind::KwOr => Some(InfixOp::Or),
            SyntaxKind::Pipe => Some(InfixOp::Or),
            SyntaxKind::KwAnd => Some(InfixOp::And),
            SyntaxKind::Ampersand => Some(InfixOp::And),
            _ => None,
        }
    }

    pub fn op_node(&self) -> Option<SyntaxElement> {
        self.syntax()
            .children_with_tokens()
            .find(|n| n.kind().is_binary_op())
    }

    pub fn rhs(&self) -> Option<PPExpr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl PPUnaryExpr {
    pub fn op_kind(&self) -> Option<PrefixOp> {
        let op = self.op_token()?;

        match op.kind() {
            SyntaxKind::KwNot | SyntaxKind::Tilde => Some(PrefixOp::Not),
            _ => None,
        }
    }

    pub fn op_token(&self) -> Option<SyntaxElement> {
        self.syntax()
            .children_with_tokens()
            .find(|n| n.kind().is_unary_op())
    }

    pub fn rhs(&self) -> Option<PPExpr> {
        helper::nodes(self.syntax()).next()
    }
}

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).next()
    }

    pub fn op_kind(&self) -> Option<InfixOp> {
        let op = self.op_node()?;

        match op.kind() {
            SyntaxKind::Imply => Some(InfixOp::Imply),
            SyntaxKind::KwOr => Some(InfixOp::Or),
            SyntaxKind::Pipe => Some(InfixOp::Or),
            SyntaxKind::KwAnd => Some(InfixOp::And),
            SyntaxKind::Ampersand => Some(InfixOp::And),
            SyntaxKind::Less => Some(InfixOp::Less),
            SyntaxKind::Greater => Some(InfixOp::Greater),
            SyntaxKind::LessEqu => Some(InfixOp::LessEq),
            SyntaxKind::GreaterEqu => Some(InfixOp::GreaterEq),
            SyntaxKind::Equ => Some(InfixOp::Equal),
            SyntaxKind::NotEq => Some(InfixOp::NotEqual),
            SyntaxKind::KwIn => Some(InfixOp::In),
            SyntaxKind::NotIn => Some(InfixOp::NotIn),
            SyntaxKind::Plus => Some(InfixOp::Add),
            SyntaxKind::Minus => Some(InfixOp::Sub),
            SyntaxKind::KwXor => Some(InfixOp::Xor),
            SyntaxKind::Star => Some(InfixOp::Mul),
            SyntaxKind::Slash => Some(InfixOp::RealDiv),
            SyntaxKind::KwDiv => Some(InfixOp::Div),
            SyntaxKind::KwMod => Some(InfixOp::Mod),
            SyntaxKind::KwRem => Some(InfixOp::Rem),
            SyntaxKind::KwShl => Some(InfixOp::Shl),
            SyntaxKind::KwShr => Some(InfixOp::Shr),
            SyntaxKind::Exp => Some(InfixOp::Exp),
            _ => None,
        }
    }

    pub fn op_node(&self) -> Option<SyntaxElement> {
        self.syntax()
            .children_with_tokens()
            .find(|n| n.kind().is_binary_op())
    }

    pub fn rhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl UnaryExpr {
    pub fn op_kind(&self) -> Option<PrefixOp> {
        let op = self.op_node()?;

        match op.kind() {
            SyntaxKind::KwNot | SyntaxKind::Tilde => Some(PrefixOp::Not),
            SyntaxKind::Plus => Some(PrefixOp::Identity),
            SyntaxKind::Minus => Some(PrefixOp::Negate),
            _ => None,
        }
    }

    pub fn op_node(&self) -> Option<SyntaxElement> {
        self.syntax()
            .children_with_tokens()
            .find(|n| n.kind().is_unary_op())
    }

    pub fn rhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).next()
    }
}

impl LiteralExpr {
    pub fn literal(&self) -> Option<(LiteralValue, Option<Vec<LiteralParseError>>)> {
        let literal = self.syntax().first_token()?;

        match literal.kind() {
            SyntaxKind::IntLiteral => Some(Self::parse_int_literal(literal.text())),
            SyntaxKind::RadixLiteral => Some(Self::parse_radix_literal(literal.text())),
            SyntaxKind::RealLiteral => Some(Self::parse_real_literal(literal.text())),
            SyntaxKind::CharLiteral => Some(Self::parse_char_seq_literal(literal.text(), false)),
            SyntaxKind::StringLiteral => Some(Self::parse_char_seq_literal(literal.text(), true)),
            SyntaxKind::KwTrue => Some((LiteralValue::Boolean(true), None)),
            SyntaxKind::KwFalse => Some((LiteralValue::Boolean(false), None)),
            _ => None,
        }
    }

    fn parse_int_literal(text: &str) -> (LiteralValue, Option<Vec<LiteralParseError>>) {
        // Basic integer, parsing with radix 10
        let value = lexical::parse::<u64, _>(text);

        match value {
            Ok(num) => (LiteralValue::Int(num), None),
            Err(err) => {
                let err = match err.code {
                    lexical::ErrorCode::Overflow => LiteralParseError::IntTooLarge,
                    _ => LiteralParseError::IntInvalid,
                };
                (LiteralValue::Int(0), Some(vec![err]))
            }
        }
    }

    fn parse_radix_literal(text: &str) -> (LiteralValue, Option<Vec<LiteralParseError>>) {
        // Radix Integer parsing
        let (radix_slice, digits_slice) = text.split_at(text.find('#').unwrap());
        let digits_slice = &digits_slice[1..]; // skip over #

        let radix = match lexical::parse::<u8, _>(radix_slice) {
            Ok(radix) if (2..=36).contains(&radix) => radix,
            _ => {
                // invalid radix value
                return (
                    LiteralValue::Int(0),
                    Some(vec![LiteralParseError::IntInvalidBase]),
                );
            }
        };

        // valid radix, parse the tail
        match lexical::parse_radix::<u64, _>(&digits_slice, radix) {
            Ok(num) => (LiteralValue::Int(num), None),
            Err(err) => {
                let err = match err.code {
                    lexical::ErrorCode::Overflow => LiteralParseError::IntRadixTooLarge,
                    lexical::ErrorCode::InvalidDigit => {
                        // Get the exact span of the invalid digit
                        // account for width of the radix & '#'
                        let start_slice = radix_slice.len() + 1 + err.index;
                        let end_slice = start_slice + 1;

                        // Chars are guarranteed to be in the ascii range
                        assert!(text.get(start_slice..end_slice).is_some());

                        LiteralParseError::IntRadixInvalidDigit(start_slice, end_slice)
                    }
                    lexical::ErrorCode::Empty => LiteralParseError::IntMissingRadix,
                    _ => LiteralParseError::IntInvalid,
                };
                (LiteralValue::Int(0), Some(vec![err]))
            }
        }
    }

    fn parse_real_literal(text: &str) -> (LiteralValue, Option<Vec<LiteralParseError>>) {
        // Pretty simple real parsing
        match lexical::parse_lossy::<f64, _>(text) {
            Ok(num) if num.is_infinite() => (
                LiteralValue::Real(0.0),
                Some(vec![LiteralParseError::RealTooLarge]),
            ),
            Ok(num) if num.is_nan() => (
                LiteralValue::Real(0.0),
                Some(vec![LiteralParseError::RealInvalid]),
            ),
            Err(err) => {
                let err = match err.code {
                    lexical::ErrorCode::Overflow => LiteralParseError::RealTooLarge,
                    lexical::ErrorCode::Underflow => LiteralParseError::RealTooSmall,
                    lexical::ErrorCode::EmptyExponent => LiteralParseError::RealMissingExponent,
                    // all other cases are protected by what is parsed, but still push out an error
                    _ => LiteralParseError::RealInvalid,
                };
                (LiteralValue::Real(0.0), Some(vec![err]))
            }
            Ok(num) => (LiteralValue::Real(num), None),
        }
    }

    fn parse_char_seq_literal(
        text: &str,
        as_str_literal: bool,
    ) -> (LiteralValue, Option<Vec<LiteralParseError>>) {
        let ending_delimiter = if as_str_literal { '"' } else { '\'' };
        let (inner_text, errors) = CharSeqExtractor::extract(text, ending_delimiter);

        // Don't need to report a missing terminator, since that's already reported by the scanner

        if as_str_literal {
            (LiteralValue::String(inner_text), errors)
        } else {
            (LiteralValue::Char(inner_text), errors)
        }
    }
}

/// Extractor for char sequences
struct CharSeqExtractor<'a> {
    /// Source text to extract from
    text: &'a str,
    /// Character source
    char_indices: iter::Peekable<std::str::CharIndices<'a>>,
    /// Extracted string
    extracted_text: String,
    /// Any errors encountered during processing
    errors: Vec<LiteralParseError>,
    /// Ending delimiter to stop at
    ending_delimiter: char,

    /// Current character
    current: Option<(usize, char)>,
}

impl<'a> CharSeqExtractor<'a> {
    /// Extracts the char sequence's text, applying character escapes
    fn extract(text: &'a str, ending_delimiter: char) -> (String, Option<Vec<LiteralParseError>>) {
        let mut char_indices = text.char_indices().peekable();
        // Skip over starting delimiter
        char_indices.next();

        let mut extractor = Self {
            text,
            char_indices,
            errors: vec![],
            extracted_text: String::with_capacity(256),
            ending_delimiter,
            current: None,
        };

        // Do the extraction
        extractor.do_extraction();

        let Self {
            extracted_text,
            errors,
            ..
        } = extractor;
        let has_errors = !errors.is_empty();

        (extracted_text, Some(errors).filter(|_| has_errors))
    }

    fn do_extraction(&mut self) {
        // Keep going along the string until there's no more chars, or until the end delimiter is reached
        while let Some(current) = self.bump() {
            match current {
                '\\' => self.eat_slash_escape(),
                '^' => self.eat_caret_escape(),
                _ => {
                    if current == self.ending_delimiter {
                        // At the ending delimiter, stop
                        return;
                    } else {
                        // Append character
                        self.push(current);
                    }
                }
            }
        }

        // Reached the end of the inner text, but no terminator was encountered
        self.push_error(CharSeqParseError::UnterminatedLiteral, 0, self.text.len());
    }

    /// Advances the char cursor by one, returning the current char
    fn bump(&mut self) -> Option<char> {
        self.current = self.char_indices.next();
        Some(self.current?.1)
    }

    /// Gets the offset of the current char, relative to the start of the source string
    fn current_pos(&self) -> usize {
        self.current
            .map(|(pos, _)| pos)
            .unwrap_or_else(|| self.text.len())
    }

    /// Gets the offset of the next char, relative to the start of the source string
    fn peek_pos(&mut self) -> usize {
        self.char_indices
            .peek()
            .map(|(pos, _)| *pos)
            .unwrap_or_else(|| self.text.len())
    }

    /// Peeks at the next char after the cursor
    fn peek(&mut self) -> Option<char> {
        self.char_indices.peek().map(|(_, c)| *c)
    }

    /// Appends a character to the extracted string
    fn push(&mut self, chr: char) {
        self.extracted_text.push(chr);
    }

    /// Appends an error to the error list
    fn push_error(&mut self, error: CharSeqParseError, start: usize, end: usize) {
        let as_str_literal = self.ending_delimiter == '"';

        if as_str_literal {
            self.errors
                .push(LiteralParseError::StringError(error, start, end))
        } else {
            self.errors
                .push(LiteralParseError::CharError(error, start, end))
        }
    }

    /// Eats all variants of the slash escape
    fn eat_slash_escape(&mut self) {
        use std::char;
        use std::convert::TryFrom;

        let escape_start = self.current_pos();

        let escaped = if let Some(chr) = self.bump() {
            chr
        } else {
            // Missing escaped character
            let escape_end = self.peek_pos();
            self.push_error(
                CharSeqParseError::InvalidSlashEscape,
                escape_start,
                escape_end,
            );

            return;
        };

        match escaped {
            // Simple escapes
            '\'' => self.push('\''),
            '"' => self.push('"'),
            '\\' => self.push('\\'),
            // Escaped control characters
            'b' | 'B' => self.push('\x08'),
            'd' | 'D' => self.push('\x7F'),
            'e' | 'E' => self.push('\x1B'),
            'f' | 'F' => self.push('\x0C'),
            'r' | 'R' => self.push('\r'),
            'n' | 'N' => self.push('\n'),
            't' | 'T' => self.push('\t'),
            // Escaped caret
            '^' => self.push('^'),
            '0'..='7' => {
                // Octal str, {1-3}, octal 0..400
                let digits_start = self.current_pos();

                // Eat up to 2 more octal digits
                for _ in 0..2 {
                    if !matches!(self.peek(), Some('0'..='7')) {
                        break;
                    }

                    // Eat it
                    self.bump();
                }

                let digits_end = self.peek_pos();
                let digits = &self.text[digits_start..digits_end];

                // Convert to character & check if it's in range
                // Since we're only feeding in chars that are 0..7 and only octal values up to
                // 777 (decimal 512), parsing should be infalliable
                let chr_value = lexical::parse_radix::<u16, _>(digits, 8).unwrap();

                if let Ok(chr) = u8::try_from(chr_value) {
                    // Successful conversion, push it
                    self.push(chr as char);
                } else {
                    // Out of range
                    self.push_error(
                        CharSeqParseError::InvalidOctalChar,
                        escape_start,
                        digits_end,
                    );

                    self.push(char::REPLACEMENT_CHARACTER);
                }
            }
            'x' => {
                // Hex sequence, {1-2} digits
                let digits_start = self.peek_pos();

                // Eat up to 2 more hex digits
                for _ in 0..2 {
                    if !self
                        .peek()
                        .map(|c| c.is_ascii_hexdigit())
                        .unwrap_or_default()
                    {
                        break;
                    }

                    // Eat it
                    self.bump();
                }

                let digits_end = self.peek_pos();
                let digits = &self.text[digits_start..digits_end];

                if digits.is_empty() {
                    // Missing hex digits after position
                    self.push_error(
                        CharSeqParseError::MissingHexDigits,
                        escape_start,
                        digits_end,
                    );

                    // Push `escaped`
                    self.push(escaped);

                    return;
                }

                // Convert to character value & push it
                // Since we're only feeding in chars that are '0'..'F' and only hex values up to
                // `u8::MAX`, parsing should be infalliable
                let chr_value = lexical::parse_radix::<u8, _>(digits, 16).unwrap();
                self.push(chr_value as char);
            }
            'u' | 'U' => {
                // u: unicode character {1-8} `char::REPLACEMENT_CHARACTER` if out of range
                let digits_start = self.peek_pos();

                // Eat up to 8 more hex digits
                for _ in 0..8 {
                    if !self
                        .peek()
                        .map(|c| c.is_ascii_hexdigit())
                        .unwrap_or_default()
                    {
                        break;
                    }

                    // Eat it
                    self.bump();
                }

                let digits_end = self.peek_pos();
                let digits = &self.text[digits_start..digits_end];

                if digits.is_empty() {
                    // Missing hex digits after position
                    self.push_error(
                        CharSeqParseError::MissingHexDigits,
                        escape_start,
                        digits_end,
                    );

                    // Push `escaped`
                    self.push(escaped);

                    return;
                }

                // Convert to Unicode codepoint
                // Since we're only feeding in chars that are '0'..'F' and only hex values up to
                // `u32::MAX`, parsing should be infalliable
                let codepoint = lexical::parse_radix::<u32, _>(digits, 16).unwrap();

                // Try to convert codepoint into a `char`
                if let Ok(chr) = char::try_from(codepoint) {
                    // Codepoint is okay
                    self.push(chr);
                } else {
                    if codepoint > 0x10FFFF {
                        // Invalid codepoint
                        self.push_error(
                            CharSeqParseError::InvalidUnicodeChar,
                            escape_start,
                            digits_end,
                        );
                    } else if (0xD800..=0xDFFF).contains(&codepoint) {
                        // Surrogate character, not allowed
                        self.push_error(CharSeqParseError::SurrogateChar, escape_start, digits_end);
                    }

                    // Push replacement character
                    self.push(char::REPLACEMENT_CHARACTER);
                }
            }
            _ => {
                // Bad escape character
                let escape_end = self.peek_pos();

                self.push_error(
                    CharSeqParseError::InvalidSlashEscape,
                    escape_start,
                    escape_end,
                );

                // Push character unmodified
                self.push(escaped);
            }
        }
    }

    /// Eats all variants of the caret escape
    fn eat_caret_escape(&mut self) {
        let escape_start = self.current_pos();

        let escaped = if let Some(chr) = self.bump() {
            chr
        } else {
            // Missing escaped character
            let escape_end = self.peek_pos();
            self.push_error(
                CharSeqParseError::InvalidCaretEscape,
                escape_start,
                escape_end,
            );

            return;
        };

        // Parse escaped caret character
        match escaped {
            '@'..='_' | 'a'..='z' => {
                let parsed = (escaped.to_ascii_uppercase() as u8) & 0x1F;
                self.push(parsed as char);
            }
            '?' => {
                // As the DEL char
                self.push('\x7F');
            }
            _ => {
                // Unless the user knows what they are doing, they are likely to not intend for the ^ character to be
                // parsed as the beginning of a caret sequence
                let escape_end = self.peek_pos();

                self.push_error(
                    CharSeqParseError::InvalidCaretEscape,
                    escape_start,
                    escape_end,
                );

                // Push escaped
                self.push(escaped);
            }
        }
    }
}

impl Reference {
    pub fn into_expr(self) -> Expr {
        match self {
            Reference::NameExpr(expr) => Expr::NameExpr(expr),
            Reference::SelfExpr(expr) => Expr::SelfExpr(expr),
            Reference::FieldExpr(expr) => Expr::FieldExpr(expr),
            Reference::DerefExpr(expr) => Expr::DerefExpr(expr),
            Reference::CheatExpr(expr) => Expr::CheatExpr(expr),
            Reference::NatCheatExpr(expr) => Expr::NatCheatExpr(expr),
            Reference::ArrowExpr(expr) => Expr::ArrowExpr(expr),
            Reference::IndirectExpr(expr) => Expr::IndirectExpr(expr),
            Reference::BitsExpr(expr) => Expr::BitsExpr(expr),
            Reference::CallExpr(expr) => Expr::CallExpr(expr),
        }
    }
}

impl IoCap {
    pub fn io_kind(&self) -> Option<IoKind> {
        match self.syntax().first_token()?.kind() {
            SyntaxKind::KwGet => Some(IoKind::Get),
            SyntaxKind::KwPut => Some(IoKind::Put),
            SyntaxKind::KwRead => Some(IoKind::Read),
            SyntaxKind::KwWrite => Some(IoKind::Write),
            SyntaxKind::KwSeek => Some(IoKind::Seek),
            SyntaxKind::KwMod => Some(IoKind::Mod),
            _ => None,
        }
    }

    pub fn io_kind_token(&self) -> Option<SyntaxToken> {
        self.syntax().first_child_or_token()?.into_token()
    }
}

impl AsnOp {
    pub fn asn_kind(&self) -> Option<AssignOp> {
        let op = self.asn_node()?;

        let kind = match op.kind() {
            SyntaxKind::Assign => AssignOp::None,
            SyntaxKind::Imply => AssignOp::Imply,
            SyntaxKind::KwOr => AssignOp::Or,
            SyntaxKind::Pipe => AssignOp::Or,
            SyntaxKind::KwAnd => AssignOp::And,
            SyntaxKind::Ampersand => AssignOp::And,
            SyntaxKind::Plus => AssignOp::Add,
            SyntaxKind::Minus => AssignOp::Sub,
            SyntaxKind::KwXor => AssignOp::Xor,
            SyntaxKind::Star => AssignOp::Mul,
            SyntaxKind::Slash => AssignOp::RealDiv,
            SyntaxKind::KwDiv => AssignOp::Div,
            SyntaxKind::KwMod => AssignOp::Mod,
            SyntaxKind::KwRem => AssignOp::Rem,
            SyntaxKind::KwShl => AssignOp::Shl,
            SyntaxKind::KwShr => AssignOp::Shr,
            SyntaxKind::Exp => AssignOp::Exp,
            _ => return None,
        };

        Some(kind)
    }

    pub fn asn_node(&self) -> Option<SyntaxElement> {
        self.syntax().children_with_tokens().next()
    }
}

impl AssignStmt {
    pub fn lhs(&self) -> Option<Reference> {
        helper::nodes(self.syntax()).next()
    }

    pub fn rhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl PutItem {
    pub fn width(&self) -> Option<PutOpt> {
        helper::nodes(self.syntax()).next()
    }

    pub fn fraction(&self) -> Option<PutOpt> {
        helper::nodes(self.syntax()).nth(1)
    }

    pub fn exp_width(&self) -> Option<PutOpt> {
        helper::nodes(self.syntax()).nth(2)
    }
}

impl TagStmt {
    pub fn tag_ref(&self) -> Option<Reference> {
        helper::nodes(self.syntax()).next()
    }

    pub fn tag_val(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl WaitStmt {
    pub fn wait_ref(&self) -> Option<Reference> {
        helper::nodes(self.syntax()).next()
    }

    pub fn wait_val(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl PrimType {
    pub fn prim(&self) -> Option<PrimitiveKind> {
        let prim_node = self.prim_node()?;
        match prim_node.kind() {
            SyntaxKind::KwInt => Some(PrimitiveKind::Int),
            SyntaxKind::KwInt1 => Some(PrimitiveKind::Int1),
            SyntaxKind::KwInt2 => Some(PrimitiveKind::Int2),
            SyntaxKind::KwInt4 => Some(PrimitiveKind::Int4),
            SyntaxKind::KwNat => Some(PrimitiveKind::Nat),
            SyntaxKind::KwNat1 => Some(PrimitiveKind::Nat1),
            SyntaxKind::KwNat2 => Some(PrimitiveKind::Nat2),
            SyntaxKind::KwNat4 => Some(PrimitiveKind::Nat4),
            SyntaxKind::KwReal => Some(PrimitiveKind::Real),
            SyntaxKind::KwReal4 => Some(PrimitiveKind::Real4),
            SyntaxKind::KwReal8 => Some(PrimitiveKind::Real8),
            SyntaxKind::KwBoolean => Some(PrimitiveKind::Boolean),
            SyntaxKind::KwAddressint => Some(PrimitiveKind::AddressInt),
            SyntaxKind::KwChar => Some(PrimitiveKind::Char),
            SyntaxKind::KwString => Some(PrimitiveKind::String),
            _ => {
                // Try casting
                let node = prim_node.into_node()?;
                crate::match_ast! {
                    match node {
                        ast::SizedCharType(node) => Some(PrimitiveKind::SizedChar(node.seq_length())),
                        ast::SizedStringType(node) => Some(PrimitiveKind::SizedString(node.seq_length())),
                        _ => None,
                    }
                }
            }
        }
    }

    pub fn prim_node(&self) -> Option<SyntaxElement> {
        self.syntax().children_with_tokens().next()
    }
}

impl RangeType {
    pub fn begin(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).next()
    }

    pub fn end(&self) -> Option<EndBound> {
        helper::nodes(self.syntax()).nth(1)
    }
}
