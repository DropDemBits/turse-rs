use std::fmt;
use toc_core::Location;

/// Parsed token
#[derive(Debug, PartialEq, Clone)]
pub struct Token<'src> {
    /// Source the token is a part of.
    /// The token is only valid for the lifetime of the source.
    source: &'src str,
    /// Type of the token
    pub token_type: TokenType,
    /// Location of the lexeme in the file/text stream
    pub location: Location,
}

impl<'s> Token<'s> {
    pub fn new(token_type: TokenType, location: Location, source: &'s str) -> Token<'s> {
        Self {
            token_type,
            location,
            source,
        }
    }

    /// Gets the lexeme of the token
    pub fn get_lexeme(&self) -> &str {
        self.location.get_lexeme(self.source)
    }
}

/// Valid tokens in Turing
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    /// Generic error token
    Error,
    // Character Tokens
    /// @
    At,
    /// ->
    Arrow,
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
    Minus,
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

    // Composite Tokens
    // Stitched together by the scanner
    /// not in | ~in | ~ in
    NotIn,
    /// not = | not= | ~= | ~ =
    NotEqu,

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
    CharLiteral(String),
    StringLiteral(String),
    NatLiteral(u64),
    RealLiteral(f64),

    // Other
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TokenType::Error => "<error>",
            TokenType::At => "@",
            TokenType::Arrow => "->",
            TokenType::Caret => "^",
            TokenType::Colon => ":",
            TokenType::Assign => ":=",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Range => "..",
            TokenType::Equ => "=",
            TokenType::GreaterEqu => ">=",
            TokenType::Greater => ">",
            TokenType::Pound => "#",
            TokenType::Imply => "=>",
            TokenType::LessEqu => "<=",
            TokenType::LeftParen => "(",
            TokenType::Less => "<",
            TokenType::Minus => "-",
            TokenType::Plus => "+",
            TokenType::RightParen => ")",
            TokenType::Semicolon => ";",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::Exp => "**",
            TokenType::Tilde => "~",
            TokenType::NotIn => "~in",
            TokenType::NotEqu => "~=",
            TokenType::Addressint => "addressint",
            TokenType::All => "all",
            TokenType::And => "and",
            TokenType::Array => "array",
            TokenType::Asm => "asm",
            TokenType::Assert => "assert",
            TokenType::Begin => "begin",
            TokenType::Bind => "bind",
            TokenType::Bits => "bits",
            TokenType::Body => "body",
            TokenType::Boolean => "boolean",
            TokenType::Break => "break",
            TokenType::By => "by",
            TokenType::Case => "case",
            TokenType::Char => "char",
            TokenType::Checked => "checked",
            TokenType::Class => "class",
            TokenType::Close => "close",
            TokenType::Collection => "collection",
            TokenType::Condition => "condition",
            TokenType::Const => "const",
            TokenType::Decreasing => "decreasing",
            TokenType::Def => "def",
            TokenType::Deferred => "deferred",
            TokenType::Div => "div",
            TokenType::Elif => "elif",
            TokenType::Else => "else",
            TokenType::Elseif => "elseif",
            TokenType::Elsif => "elsif",
            TokenType::End => "end",
            TokenType::EndCase => "endcase",
            TokenType::EndFor => "endfor",
            TokenType::EndIf => "endif",
            TokenType::EndLoop => "endloop",
            TokenType::Enum => "enum",
            TokenType::Exit => "exit",
            TokenType::Export => "export",
            TokenType::External => "external",
            TokenType::False => "false",
            TokenType::Flexible => "flexible",
            TokenType::For => "for",
            TokenType::Fork => "fork",
            TokenType::Forward => "forward",
            TokenType::Free => "free",
            TokenType::Function => "function",
            TokenType::Get => "get",
            TokenType::Handler => "handler",
            TokenType::If => "if",
            TokenType::Implement => "implement",
            TokenType::Import => "import",
            TokenType::In => "in",
            TokenType::Include => "include",
            TokenType::Inherit => "inherit",
            TokenType::Init => "init",
            TokenType::Int => "int",
            TokenType::Int1 => "int1",
            TokenType::Int2 => "int2",
            TokenType::Int4 => "int4",
            TokenType::Invariant => "invariant",
            TokenType::Label => "label",
            TokenType::Loop => "loop",
            TokenType::Mod => "mod",
            TokenType::Module => "module",
            TokenType::Monitor => "monitor",
            TokenType::Nat => "nat",
            TokenType::Nat1 => "nat1",
            TokenType::Nat2 => "nat2",
            TokenType::Nat4 => "nat4",
            TokenType::New => "new",
            TokenType::Nil => "nil",
            TokenType::Not => "not",
            TokenType::ObjectClass => "objectclass",
            TokenType::Of => "of",
            TokenType::Opaque => "opaque",
            TokenType::Open => "open",
            TokenType::Or => "or",
            TokenType::Packed => "packed",
            TokenType::Pause => "pause",
            TokenType::Pervasive => "pervasive",
            TokenType::Pointer => "pointer",
            TokenType::Post => "post",
            TokenType::Pre => "pre",
            TokenType::Priority => "priority",
            TokenType::Procedure => "procedure",
            TokenType::Process => "process",
            TokenType::Put => "put",
            TokenType::Quit => "quit",
            TokenType::Read => "read",
            TokenType::Real => "real",
            TokenType::Real4 => "real4",
            TokenType::Real8 => "real8",
            TokenType::Record => "record",
            TokenType::Register => "register",
            TokenType::Rem => "rem",
            TokenType::Result_ => "result",
            TokenType::Return => "return",
            TokenType::Seek => "seek",
            TokenType::Self_ => "self",
            TokenType::Set => "set",
            TokenType::Shl => "shl",
            TokenType::Shr => "shr",
            TokenType::Signal => "signal",
            TokenType::Skip => "skip",
            TokenType::String_ => "string",
            TokenType::Tag => "tag",
            TokenType::Tell => "tell",
            TokenType::Then => "then",
            TokenType::Timeout => "timeout",
            TokenType::To => "to",
            TokenType::True => "true",
            TokenType::Type => "type",
            TokenType::Unchecked => "unchecked",
            TokenType::Union => "union",
            TokenType::Unqualified => "unqualified",
            TokenType::Var => "var",
            TokenType::Wait => "wait",
            TokenType::When => "when",
            TokenType::Write => "write",
            TokenType::Xor => "xor",
            TokenType::Eof => "<end of file>",
            _ => "<other literal>",
        })
    }
}
