//! Concrete Syntax Tree for the Turing language
#[allow(clippy::match_like_matches_macro)] // Code generation generates them like this
#[allow(clippy::upper_case_acronyms)] // Names are pulled from the grammar file exactly
pub mod ast;

use std::{fmt, ops::Range};

use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;
use toc_span::TextRange;

#[macro_use]
extern crate num_derive;

/// Syntax tokens present
#[allow(clippy::upper_case_acronyms)] // Names must match the grammar file exactly
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    // Character Tokens
    At,
    Ampersand,
    Arrow,
    Caret,
    Colon,
    Assign,
    Comma,
    Range,
    Dot,
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
    Pipe,
    RightParen,
    Semicolon,
    Slash,
    Star,
    Exp,
    Tilde,

    // Keywords
    KwAddressint,
    KwAll,
    KwAnd,
    KwArray,
    KwAsm,
    KwAssert,
    KwBegin,
    KwBind,
    KwBits,
    KwBody,
    KwBoolean,
    KwBreak,
    KwBy,
    KwCase,
    KwChar,
    KwCheat,
    KwChecked,
    KwClass,
    KwClose,
    KwCollection,
    KwCondition,
    KwConst,
    KwDecreasing,
    KwDef,
    KwDeferred,
    KwDiv,
    KwElif,
    KwElse,
    KwElseif,
    KwElsif,
    KwEnd,
    KwEndCase,
    KwEndFor,
    KwEndIf,
    KwEndLoop,
    KwEnum,
    KwExit,
    KwExport,
    KwExternal,
    KwFalse,
    KwFlexible,
    KwFor,
    KwFork,
    KwForward,
    KwFree,
    KwFunction,
    KwGet,
    KwHandler,
    KwIf,
    KwImplement,
    KwImport,
    KwIn,
    KwInclude,
    KwInherit,
    KwInit,
    KwInt,
    KwInt1,
    KwInt2,
    KwInt4,
    KwInvariant,
    KwLabel,
    KwLoop,
    KwMod,
    KwModule,
    KwMonitor,
    KwNat,
    KwNat1,
    KwNat2,
    KwNat4,
    KwNew,
    KwNil,
    KwNot,
    KwObjectClass,
    KwOf,
    KwOpaque,
    KwOpen,
    KwOr,
    KwPacked,
    KwPause,
    KwPervasive,
    KwPointer,
    KwPost,
    KwPre,
    KwPriority,
    KwProcedure,
    KwProcess,
    KwPut,
    KwQuit,
    KwRead,
    KwReal,
    KwReal4,
    KwReal8,
    KwRecord,
    KwRegister,
    KwRem,
    KwResult,
    KwReturn,
    KwSeek,
    KwSelf,
    KwSet,
    KwShl,
    KwShr,
    KwSignal,
    KwSizeOf,
    KwSkip,
    KwString,
    KwTag,
    KwTell,
    KwThen,
    KwTimeout,
    KwTo,
    KwTrue,
    KwType,
    KwUnchecked,
    KwUnion,
    KwUnit,
    KwUnqualified,
    KwVar,
    KwWait,
    KwWhen,
    KwWrite,
    KwXor,

    // Preprocessor keywords
    PPKwIf,
    PPKwElseif,
    PPKwElsif,
    PPKwElse,
    PPKwEnd,
    PPKwEndIf,

    // Literals
    /// Normal integer literal
    IntLiteral,
    /// Real Literal
    RealLiteral,
    /// Integer with custom radix base
    RadixLiteral,
    /// Character literal
    CharLiteral,
    /// String literal
    StringLiteral,
    /// Identifier
    Identifier,

    /// Whitespace (including newlines)
    Whitespace,
    /// Comments (both block & line comments)
    Comment,

    // Composite & Misc Syntax Tokens //
    /// Error tokens produced by the scanner
    Error,
    /// "not in" composite token (`"not" "whitespace" "in"`)
    NotIn,
    /// "not=" composite token (`"not" "whitespace" "eq"`)
    NotEq,

    // Generated from 'turing.ungram'
    Name,
    NameList,
    UnqualifiedAttr,
    PervasiveAttr,
    RegisterAttr,
    ConstAttr,
    VarAttr,
    CheatAttr,
    ForwardAttr,
    OpaqueAttr,
    Source,
    StmtList,
    ImportStmt,
    PreprocGlob,
    PPInclude,
    PPIf,
    PPTokenBody,
    PPElseif,
    PPElse,
    PPEndIf,
    PPParenExpr,
    PPBinaryExpr,
    PPUnaryExpr,
    PPNameExpr,
    ConstVarDecl,
    TypeDecl,
    BindDecl,
    ProcDecl,
    FcnDecl,
    ProcessDecl,
    ProcessHeader,
    ExternalDecl,
    ExternalVar,
    ForwardDecl,
    DeferredDecl,
    ModuleDecl,
    ClassDecl,
    MonitorDecl,
    AssignStmt,
    AsnOp,
    OpenStmt,
    CloseStmt,
    PutStmt,
    GetStmt,
    ReadStmt,
    WriteStmt,
    SeekStmt,
    TellStmt,
    ForStmt,
    LoopStmt,
    ExitStmt,
    IfStmt,
    CaseStmt,
    InvariantStmt,
    AssertStmt,
    CallStmt,
    ReturnStmt,
    ResultStmt,
    NewStmt,
    FreeStmt,
    TagStmt,
    ForkStmt,
    SignalStmt,
    PauseStmt,
    QuitStmt,
    BreakStmt,
    CheckednessStmt,
    BlockStmt,
    BindItem,
    ProcHeader,
    DeviceSpec,
    SubprogBody,
    ParamSpec,
    FcnHeader,
    FcnResult,
    PreStmt,
    InitStmt,
    PostStmt,
    HandlerStmt,
    InitVar,
    BodyDecl,
    PlainHeader,
    ModuleBody,
    InheritStmt,
    ImplementStmt,
    ImplementByStmt,
    ExportStmt,
    ImportList,
    ImportItem,
    ExternalItem,
    ExportItem,
    CompoundOp,
    OldOpen,
    NewOpen,
    OpenPath,
    OpenMode,
    IoCap,
    OldClose,
    NewClose,
    StreamNum,
    PutItem,
    PutOpt,
    GetItem,
    GetWidth,
    BinaryIO,
    BinaryItem,
    RequestSize,
    ActualSize,
    ForBounds,
    StepBy,
    IfBody,
    ElseStmt,
    ElseifStmt,
    CaseArm,
    ExprList,
    ParamList,
    WaitStmt,
    ForkStatus,
    StackSize,
    ProcessDesc,
    QuitCause,
    Checkedness,
    LiteralExpr,
    CheatExpr,
    ObjClassExpr,
    InitExpr,
    NilExpr,
    SizeOfExpr,
    BinaryExpr,
    UnaryExpr,
    ParenExpr,
    RefExpr,
    NameExpr,
    SelfExpr,
    FieldExpr,
    DerefExpr,
    NatCheatExpr,
    ArrowExpr,
    IndirectExpr,
    BitsExpr,
    CallExpr,
    SizeSpec,
    PrimType,
    NameType,
    RangeSpec,
    Param,
    AllItem,
    RangeItem,
    RelativeBound,
    RangeType,
    EnumType,
    ArrayType,
    SetType,
    RecordType,
    UnionType,
    PointerType,
    CollectionType,
    ConditionType,
    SizedCharType,
    SizedStringType,
    SeqLength,
    RecordField,
    UnionVariant,
    FcnType,
    ProcType,
    ConstVarParam,
    SubprogParam,
    ConditionKind,
    RangeList,
    EndGroup,
    UnsizedBound,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind.to_u16().unwrap())
    }
}

impl SyntaxKind {
    fn is_binary_op(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Imply
                | SyntaxKind::KwOr
                | SyntaxKind::Pipe
                | SyntaxKind::KwAnd
                | SyntaxKind::Ampersand
                | SyntaxKind::Less
                | SyntaxKind::Greater
                | SyntaxKind::LessEqu
                | SyntaxKind::GreaterEqu
                | SyntaxKind::Equ
                | SyntaxKind::NotEq
                | SyntaxKind::KwIn
                | SyntaxKind::NotIn
                | SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::KwXor
                | SyntaxKind::Star
                | SyntaxKind::Slash
                | SyntaxKind::KwDiv
                | SyntaxKind::KwMod
                | SyntaxKind::KwRem
                | SyntaxKind::KwShl
                | SyntaxKind::KwShr
                | SyntaxKind::Exp
        )
    }

    fn is_unary_op(&self) -> bool {
        matches!(
            self,
            SyntaxKind::KwNot | SyntaxKind::Tilde | SyntaxKind::Plus | SyntaxKind::Minus
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

// Re-export things requiring Lang
pub type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

// Operators

pub const MIN_REF_BINDING_POWER: u8 = 19;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum InfixOp {
    /// Addition / Set Union / String Concatenation (`+`)
    Add,
    /// Subtraction / Set Subtraction (`-`)
    Sub,
    /// Multiplication / Set Intersection (`*`)
    Mul,
    /// Integer Division (`div`)
    Div,
    /// Real Division (`/`)
    RealDiv,
    /// Modulo (`mod`)
    Mod,
    /// Remainder (`rem`)
    Rem,
    /// Exponentiation (`**`)
    Exp,
    /// Bitwise/boolean And (`and`)
    And,
    /// Bitwise/boolean Or (`or`)
    Or,
    /// Bitwise/boolean Exclusive-Or (`xor`)
    Xor,
    /// Logical Shift Left (`shl`)
    Shl,
    /// Logical Shift Right (`shr`)
    Shr,
    /// Less than (`<`)
    Less,
    /// Less than or Equal (`<=`)
    LessEq,
    /// Greater than (`>`)
    Greater,
    /// Greater than or Equal (`>=`)
    GreaterEq,
    /// Equality (`=` or `=`)
    Equal,
    /// Inequality (`not=` or `~=`)
    NotEqual,
    /// Set inclusion (`in`)
    In,
    /// Set exclusion (`not in`)
    NotIn,
    /// Material Implication (`=>`)
    Imply,
    /// Arrow (`->`)
    Arrow,
    /// Dot (`.`)
    Dot,
    /// Call (`(`)
    Call,
}

impl InfixOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Imply => (1, 2),
            Self::Or => (3, 4),
            Self::And => (5, 6),
            // ((), 8) is for the "not" operator
            Self::Less
            | Self::Greater
            | Self::Equal
            | Self::LessEq
            | Self::GreaterEq
            | Self::NotEqual
            | Self::In
            | Self::NotIn => (9, 10),
            Self::Add | Self::Sub | Self::Xor => (11, 12),
            Self::Mul
            | Self::RealDiv
            | Self::Div
            | Self::Mod
            | Self::Rem
            | Self::Shl
            | Self::Shr => (13, 14),
            // ((), 16) is for prefix "+" and "-"
            Self::Exp => (18, 17),
            // ((), 20) is for "#"
            Self::Call | Self::Arrow | Self::Dot => (21, 22),
            // ((), 24) is for "^"
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PrefixOp {
    /// Binary/boolean negation operator (`not`)
    Not,
    /// Integer identity (`+`)
    Identity,
    /// Integer negation (`-`)
    Negate,
    /// Nat cheat (`#`)
    NatCheat,
    /// Pointer dereferencing operator (`^`)
    Deref,
}

impl PrefixOp {
    pub fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Not => ((), 8),
            Self::Identity | Self::Negate => ((), 16),
            Self::NatCheat => ((), 20),
            Self::Deref => ((), 24),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IoKind {
    /// `get` operation is allowed
    Get,
    /// `put` operation is allowed
    Put,
    /// `read` operation is allowed
    Read,
    /// `write` operation is allowed
    Write,
    /// `seek` and `tell` operations are allowed
    Seek,
    /// Existing file is opened for modification, instead of overwriting it
    Mod,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignOp {
    /// Plain assignment
    None,
    /// Addition / Set Union / String Concatenation (`+`)
    Add,
    /// Subtraction / Set Subtraction (`-`)
    Sub,
    /// Multiplication / Set Intersection (`*`)
    Mul,
    /// Integer Division (`div`)
    Div,
    /// Real Division (`/`)
    RealDiv,
    /// Modulo (`mod`)
    Mod,
    /// Remainder (`rem`)
    Rem,
    /// Exponentiation (`**`)
    Exp,
    /// Bitwise/boolean And (`and`)
    And,
    /// Bitwise/boolean Or (`or`)
    Or,
    /// Bitwise/boolean Exclusive-Or (`xor`)
    Xor,
    /// Logical Shift Left (`shl`)
    Shl,
    /// Logical Shift Right (`shr`)
    Shr,
    /// Material Implication (`=>`)
    Imply,
}

impl AssignOp {
    pub fn as_binary_op(self) -> Option<InfixOp> {
        let op = match self {
            AssignOp::Add => InfixOp::Add,
            AssignOp::Sub => InfixOp::Sub,
            AssignOp::Mul => InfixOp::Mul,
            AssignOp::Div => InfixOp::Div,
            AssignOp::RealDiv => InfixOp::RealDiv,
            AssignOp::Mod => InfixOp::Mod,
            AssignOp::Rem => InfixOp::Rem,
            AssignOp::Exp => InfixOp::Exp,
            AssignOp::And => InfixOp::And,
            AssignOp::Or => InfixOp::Or,
            AssignOp::Xor => InfixOp::Xor,
            AssignOp::Shl => InfixOp::Shl,
            AssignOp::Shr => InfixOp::Shr,
            AssignOp::Imply => InfixOp::Imply,
            _ => return None,
        };

        Some(op)
    }
}

// Possible literal values
// ??? Investigate usage of `Cow` here
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int(u64),
    Real(f64),
    Char(String),
    String(String),
    Boolean(bool),
}

#[derive(Debug, thiserror::Error)]
pub enum LiteralParseError {
    #[error("invalid real literal: {0: }")]
    Real(InvalidReal),
    #[error("invalid int literal: {0:}")]
    Int(InvalidInt),
    #[error("invalid string literal\n{0:}")]
    String(InvalidCharsList),
    #[error("invalid char literal\n{0:}")]
    Char(InvalidCharsList),
}

impl LiteralParseError {
    pub fn header(&self) -> &str {
        match self {
            LiteralParseError::Real(_) => "invalid real literal",
            LiteralParseError::Int(_) => "invalid int literal",
            LiteralParseError::String(_) => "invalid string literal",
            LiteralParseError::Char(_) => "invalid char literal",
        }
    }

    pub fn parts(&self, base_span: TextRange) -> Vec<(String, TextRange)> {
        match self {
            LiteralParseError::Real(kind) => vec![(kind.to_string(), base_span)],
            LiteralParseError::Int(kind) => vec![(kind.to_string(), kind.span(base_span))],
            LiteralParseError::String(errs) | LiteralParseError::Char(errs) => errs
                .0
                .iter()
                .map(|(err, at)| (err.to_string(), offset_span(at, base_span)))
                .collect(),
        }
    }
}

/// Offsets the given range pair by `source_span`
fn offset_span(range: &Range<usize>, base_span: TextRange) -> TextRange {
    use toc_span::TextSize;

    let (start, end): (TextSize, TextSize) = (
        range
            .start
            .try_into()
            .unwrap_or_else(|_| TextSize::from(u32::MAX)),
        range
            .end
            .try_into()
            .unwrap_or_else(|_| TextSize::from(u32::MAX)),
    );

    TextRange::new(base_span.start() + start, base_span.start() + end)
}

#[derive(Debug, thiserror::Error)]
pub enum InvalidReal {
    #[error("number is too large")]
    TooLarge,
    #[error("number is too small")]
    TooSmall,
    #[error("missing exponent digits")]
    MissingExponent,
    #[error("invalid literal")]
    Invalid,
}

#[derive(Debug, thiserror::Error)]
pub enum InvalidInt {
    #[error("number is too large")]
    TooLarge,
    #[error("number is too large")]
    RadixTooLarge(Range<usize>),
    #[error("`{0:}` is not a digit in base {1:}")]
    BaseInvalidDigit(char, u8, Range<usize>),
    #[error("missing radix digits")]
    MissingRadix,
    #[error("base is not between 2 - 36")]
    InvalidBase(Range<usize>),
    #[error("invalid literal")]
    Invalid,
}

impl InvalidInt {
    fn span(&self, span: TextRange) -> TextRange {
        match self {
            InvalidInt::RadixTooLarge(range)
            | InvalidInt::BaseInvalidDigit(_, _, range)
            | InvalidInt::InvalidBase(range) => offset_span(range, span),
            _ => span,
        }
    }
}

#[derive(Debug)]
pub struct InvalidCharsList(pub(crate) Vec<(InvalidChar, Range<usize>)>);

impl fmt::Display for InvalidCharsList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (err, range) in self.0.iter() {
            writeln!(f, "| at {range:?}: {err}")?;
        }

        Ok(())
    }
}

impl std::error::Error for InvalidCharsList {}

#[derive(Debug, thiserror::Error)]
pub enum InvalidChar {
    #[error("octal character value is greater than \\377 (decimal 255)")]
    InvalidOctalChar,
    #[error("unicode codepoint value is greater than U+10FFFF")]
    InvalidUnicodeChar,
    #[error("surrogate chars are not allowed in char sequences")]
    SurrogateChar,
    #[error("missing hex digits after here")]
    MissingHexDigits,
    #[error("unknown backslash escape")]
    InvalidSlashEscape,
    #[error("unknown caret escape")]
    InvalidCaretEscape,
    #[error("missing terminator character")]
    UnterminatedLiteral,
    #[error("no characters in literal")]
    EmptySequence,
}

#[derive(Debug)]
pub enum PrimitiveKind {
    Int,
    Int1,
    Int2,
    Int4,
    Nat,
    Nat1,
    Nat2,
    Nat4,
    Real,
    Real4,
    Real8,
    Boolean,
    AddressInt,
    Char,
    String,
    SizedChar(Option<ast::SeqLength>),
    SizedString(Option<ast::SeqLength>),
}
