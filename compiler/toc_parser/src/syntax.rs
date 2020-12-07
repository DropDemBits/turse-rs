//! Syntax constructs
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;
use toc_scanner::ScannerToken;

/// Syntax tokens present
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SyntaxKind(ScannerToken);
/*
pub enum SyntaxKind {
    At,
    Arrow,
    Caret,
    Colon,
    Assign,
    Comma,
    Dot,
    Range,
    Equ,
    GreaterEqu,
    Greater,
    Pound,
    Imply,
    LessEqu,
    LeftParen,
    Less,
    Minus,
    Plus,
    RightParen,
    Semicolon,
    Slash,
    Star,
    Exp,
    Tilde,

    // Keywords
    Addressint,
    All,
    And,
    Array,
    Asm,
    Assert,
    Begin,
    Bind,
    Bits,
    Body,
    Boolean,
    Break,
    By,
    Case,
    Char,
    Cheat,
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
    Elif, // New keyword typo checking
    Else,
    Elseif,
    Elsif,
    End,
    EndCase, // New keyword typo checking
    EndFor,
    EndIf,
    EndLoop,
    Enum,
    Exit,
    Export,
    External,
    False,
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
    ObjectClass,
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
    Self_,
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
    Unit,
    Unqualified,
    Var,
    Wait,
    When,
    Write,
    Xor,

    // Literals
    Identifier,
    CharLiteral,
    StringLiteral,
    /// Normal integer literal
    IntLiteral,
    /// Real Literals
    RealLiteral,
    /// All whitespace
    Whitespace,
    /// All comments
    Comment,

    /// End of line
    LineEnd,
    Error,

    // Composite & Composed tokens
    Root,
    NotIn,
    NotEq,
}

impl From<ScannerToken> for SyntaxKind {
    fn from(tok: ScannerToken) -> Self {
        match tok {
            ScannerToken::At => SyntaxKind::At,
            ScannerToken::Arrow => SyntaxKind::Arrow,
            ScannerToken::Caret => SyntaxKind::Caret,
            ScannerToken::Colon => SyntaxKind::Colon,
            ScannerToken::Assign => SyntaxKind::Assign,
            ScannerToken::Comma => SyntaxKind::Comma,
            ScannerToken::Dot => SyntaxKind::Dot,
            ScannerToken::Range => SyntaxKind::Range,
            ScannerToken::Equ => SyntaxKind::Equ,
            ScannerToken::GreaterEqu => SyntaxKind::GreaterEqu,
            ScannerToken::Greater => SyntaxKind::Greater,
            ScannerToken::Pound => SyntaxKind::Pound,
            ScannerToken::Imply => SyntaxKind::Imply,
            ScannerToken::LessEqu => SyntaxKind::LessEqu,
            ScannerToken::LeftParen => SyntaxKind::LeftParen,
            ScannerToken::Less => SyntaxKind::Less,
            ScannerToken::Minus => SyntaxKind::Minus,
            ScannerToken::Plus => SyntaxKind::Plus,
            ScannerToken::RightParen => SyntaxKind::RightParen,
            ScannerToken::Semicolon => SyntaxKind::Semicolon,
            ScannerToken::Slash => SyntaxKind::Slash,
            ScannerToken::Star => SyntaxKind::Star,
            ScannerToken::Exp => SyntaxKind::Exp,
            ScannerToken::Tilde => SyntaxKind::Tilde,
            ScannerToken::Addressint => SyntaxKind::Addressint,
            ScannerToken::All => SyntaxKind::All,
            ScannerToken::And => SyntaxKind::And,
            ScannerToken::Array => SyntaxKind::Array,
            ScannerToken::Asm => SyntaxKind::Asm,
            ScannerToken::Assert => SyntaxKind::Assert,
            ScannerToken::Begin => SyntaxKind::Begin,
            ScannerToken::Bind => SyntaxKind::Bind,
            ScannerToken::Bits => SyntaxKind::Bits,
            ScannerToken::Body => SyntaxKind::Body,
            ScannerToken::Boolean => SyntaxKind::Boolean,
            ScannerToken::Break => SyntaxKind::Break,
            ScannerToken::By => SyntaxKind::By,
            ScannerToken::Case => SyntaxKind::Case,
            ScannerToken::Char => SyntaxKind::Char,
            ScannerToken::Cheat => SyntaxKind::Cheat,
            ScannerToken::Checked => SyntaxKind::Checked,
            ScannerToken::Class => SyntaxKind::Class,
            ScannerToken::Close => SyntaxKind::Close,
            ScannerToken::Collection => SyntaxKind::Collection,
            ScannerToken::Condition => SyntaxKind::Condition,
            ScannerToken::Const => SyntaxKind::Const,
            ScannerToken::Decreasing => SyntaxKind::Decreasing,
            ScannerToken::Def => SyntaxKind::Def,
            ScannerToken::Deferred => SyntaxKind::Deferred,
            ScannerToken::Div => SyntaxKind::Div,
            ScannerToken::Elif => SyntaxKind::Elif,
            ScannerToken::Else => SyntaxKind::Else,
            ScannerToken::Elseif => SyntaxKind::Elseif,
            ScannerToken::Elsif => SyntaxKind::Elsif,
            ScannerToken::End => SyntaxKind::End,
            ScannerToken::EndCase => SyntaxKind::EndCase,
            ScannerToken::EndFor => SyntaxKind::EndFor,
            ScannerToken::EndIf => SyntaxKind::EndIf,
            ScannerToken::EndLoop => SyntaxKind::EndLoop,
            ScannerToken::Enum => SyntaxKind::Enum,
            ScannerToken::Exit => SyntaxKind::Exit,
            ScannerToken::Export => SyntaxKind::Export,
            ScannerToken::External => SyntaxKind::External,
            ScannerToken::False => SyntaxKind::False,
            ScannerToken::Flexible => SyntaxKind::Flexible,
            ScannerToken::For => SyntaxKind::For,
            ScannerToken::Fork => SyntaxKind::Fork,
            ScannerToken::Forward => SyntaxKind::Forward,
            ScannerToken::Free => SyntaxKind::Free,
            ScannerToken::Function => SyntaxKind::Function,
            ScannerToken::Get => SyntaxKind::Get,
            ScannerToken::Handler => SyntaxKind::Handler,
            ScannerToken::If => SyntaxKind::If,
            ScannerToken::Implement => SyntaxKind::Implement,
            ScannerToken::Import => SyntaxKind::Import,
            ScannerToken::In => SyntaxKind::In,
            ScannerToken::Include => SyntaxKind::Include,
            ScannerToken::Inherit => SyntaxKind::Inherit,
            ScannerToken::Init => SyntaxKind::Init,
            ScannerToken::Int => SyntaxKind::Int,
            ScannerToken::Int1 => SyntaxKind::Int1,
            ScannerToken::Int2 => SyntaxKind::Int2,
            ScannerToken::Int4 => SyntaxKind::Int4,
            ScannerToken::Invariant => SyntaxKind::Invariant,
            ScannerToken::Label => SyntaxKind::Label,
            ScannerToken::Loop => SyntaxKind::Loop,
            ScannerToken::Mod => SyntaxKind::Mod,
            ScannerToken::Module => SyntaxKind::Module,
            ScannerToken::Monitor => SyntaxKind::Monitor,
            ScannerToken::Nat => SyntaxKind::Nat,
            ScannerToken::Nat1 => SyntaxKind::Nat1,
            ScannerToken::Nat2 => SyntaxKind::Nat2,
            ScannerToken::Nat4 => SyntaxKind::Nat4,
            ScannerToken::New => SyntaxKind::New,
            ScannerToken::Nil => SyntaxKind::Nil,
            ScannerToken::Not => SyntaxKind::Not,
            ScannerToken::ObjectClass => SyntaxKind::ObjectClass,
            ScannerToken::Of => SyntaxKind::Of,
            ScannerToken::Opaque => SyntaxKind::Opaque,
            ScannerToken::Open => SyntaxKind::Open,
            ScannerToken::Or => SyntaxKind::Or,
            ScannerToken::Packed => SyntaxKind::Packed,
            ScannerToken::Pause => SyntaxKind::Pause,
            ScannerToken::Pervasive => SyntaxKind::Pervasive,
            ScannerToken::Pointer => SyntaxKind::Pointer,
            ScannerToken::Post => SyntaxKind::Post,
            ScannerToken::Pre => SyntaxKind::Pre,
            ScannerToken::Priority => SyntaxKind::Priority,
            ScannerToken::Procedure => SyntaxKind::Procedure,
            ScannerToken::Process => SyntaxKind::Process,
            ScannerToken::Put => SyntaxKind::Put,
            ScannerToken::Quit => SyntaxKind::Quit,
            ScannerToken::Read => SyntaxKind::Read,
            ScannerToken::Real => SyntaxKind::Real,
            ScannerToken::Real4 => SyntaxKind::Real4,
            ScannerToken::Real8 => SyntaxKind::Real8,
            ScannerToken::Record => SyntaxKind::Record,
            ScannerToken::Register => SyntaxKind::Register,
            ScannerToken::Rem => SyntaxKind::Rem,
            ScannerToken::Result_ => SyntaxKind::Result_,
            ScannerToken::Return => SyntaxKind::Return,
            ScannerToken::Seek => SyntaxKind::Seek,
            ScannerToken::Self_ => SyntaxKind::Self_,
            ScannerToken::Set => SyntaxKind::Set,
            ScannerToken::Shl => SyntaxKind::Shl,
            ScannerToken::Shr => SyntaxKind::Shr,
            ScannerToken::Signal => SyntaxKind::Signal,
            ScannerToken::Skip => SyntaxKind::Skip,
            ScannerToken::String_ => SyntaxKind::String_,
            ScannerToken::Tag => SyntaxKind::Tag,
            ScannerToken::Tell => SyntaxKind::Tell,
            ScannerToken::Then => SyntaxKind::Then,
            ScannerToken::Timeout => SyntaxKind::Timeout,
            ScannerToken::To => SyntaxKind::To,
            ScannerToken::True => SyntaxKind::True,
            ScannerToken::Type => SyntaxKind::Type,
            ScannerToken::Unchecked => SyntaxKind::Unchecked,
            ScannerToken::Union => SyntaxKind::Union,
            ScannerToken::Unit => SyntaxKind::Unit,
            ScannerToken::Unqualified => SyntaxKind::Unqualified,
            ScannerToken::Var => SyntaxKind::Var,
            ScannerToken::Wait => SyntaxKind::Wait,
            ScannerToken::When => SyntaxKind::When,
            ScannerToken::Write => SyntaxKind::Write,
            ScannerToken::Xor => SyntaxKind::Xor,
            ScannerToken::Identifier => SyntaxKind::Identifier,
            ScannerToken::CharLiteral => SyntaxKind::CharLiteral,
            ScannerToken::StringLiteral => SyntaxKind::StringLiteral,
            ScannerToken::IntLiteral => SyntaxKind::IntLiteral,
            ScannerToken::RealLiteral => SyntaxKind::RealLiteral,
            ScannerToken::Whitespace => SyntaxKind::Whitespace,
            ScannerToken::Comment => SyntaxKind::Comment,
            ScannerToken::LineEnd => SyntaxKind::LineEnd,
            ScannerToken::Error => SyntaxKind::Error,
            ScannerToken::NumberLiteral(_) => unreachable!(),
        }
    }
}
*/

impl SyntaxKind {
    pub fn from_token(token: ScannerToken) -> Self {
        Self(token)
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind.0.to_u16().unwrap())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind(ScannerToken::from_u16(raw.0).unwrap())
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

// Re-export things requiring Lang
#[allow(unused)]
pub type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;