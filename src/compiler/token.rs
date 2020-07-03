use crate::compiler::Location;

/// Parsed token
#[derive(Debug, PartialEq)]
pub struct Token {
    /// Type of the token
    pub token_type: TokenType,
    /// Location of the lexeme in the file/text stream
    pub location: Location,
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
    // However, these cases are handled by the parser.

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
    Result_,
    Return,
    Seek,
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
    IntLiteral(u64),
    RealLiteral(f64),

    // Other
    Eof,
}
