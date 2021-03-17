//! Concrete Syntax Tree for the Turing language
#[allow(clippy::match_like_matches_macro)]
pub mod ast;

use num_traits::{FromPrimitive, ToPrimitive};
use rowan::Language;
use toc_scanner::token::TokenKind;

#[macro_use]
extern crate num_derive;

/// Syntax tokens present
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

impl From<TokenKind> for SyntaxKind {
    fn from(token: TokenKind) -> Self {
        match token {
            TokenKind::At => SyntaxKind::At,
            TokenKind::Ampersand => SyntaxKind::Ampersand,
            TokenKind::Arrow => SyntaxKind::Arrow,
            TokenKind::Caret => SyntaxKind::Caret,
            TokenKind::Colon => SyntaxKind::Colon,
            TokenKind::Assign => SyntaxKind::Assign,
            TokenKind::Comma => SyntaxKind::Comma,
            TokenKind::Range => SyntaxKind::Range,
            TokenKind::Dot => SyntaxKind::Dot,
            TokenKind::Equ => SyntaxKind::Equ,
            TokenKind::GreaterEqu => SyntaxKind::GreaterEqu,
            TokenKind::Greater => SyntaxKind::Greater,
            TokenKind::Pound => SyntaxKind::Pound,
            TokenKind::Imply => SyntaxKind::Imply,
            TokenKind::LessEqu => SyntaxKind::LessEqu,
            TokenKind::LeftParen => SyntaxKind::LeftParen,
            TokenKind::Less => SyntaxKind::Less,
            TokenKind::Minus => SyntaxKind::Minus,
            TokenKind::Plus => SyntaxKind::Plus,
            TokenKind::Pipe => SyntaxKind::Pipe,
            TokenKind::RightParen => SyntaxKind::RightParen,
            TokenKind::Semicolon => SyntaxKind::Semicolon,
            TokenKind::Slash => SyntaxKind::Slash,
            TokenKind::Star => SyntaxKind::Star,
            TokenKind::Exp => SyntaxKind::Exp,
            TokenKind::Tilde => SyntaxKind::Tilde,
            TokenKind::Addressint => SyntaxKind::KwAddressint,
            TokenKind::All => SyntaxKind::KwAll,
            TokenKind::And => SyntaxKind::KwAnd,
            TokenKind::Array => SyntaxKind::KwArray,
            TokenKind::Asm => SyntaxKind::KwAsm,
            TokenKind::Assert => SyntaxKind::KwAssert,
            TokenKind::Begin => SyntaxKind::KwBegin,
            TokenKind::Bind => SyntaxKind::KwBind,
            TokenKind::Bits => SyntaxKind::KwBits,
            TokenKind::Body => SyntaxKind::KwBody,
            TokenKind::Boolean => SyntaxKind::KwBoolean,
            TokenKind::Break => SyntaxKind::KwBreak,
            TokenKind::By => SyntaxKind::KwBy,
            TokenKind::Case => SyntaxKind::KwCase,
            TokenKind::Char => SyntaxKind::KwChar,
            TokenKind::Cheat => SyntaxKind::KwCheat,
            TokenKind::Checked => SyntaxKind::KwChecked,
            TokenKind::Class => SyntaxKind::KwClass,
            TokenKind::Close => SyntaxKind::KwClose,
            TokenKind::Collection => SyntaxKind::KwCollection,
            TokenKind::Condition => SyntaxKind::KwCondition,
            TokenKind::Const => SyntaxKind::KwConst,
            TokenKind::Decreasing => SyntaxKind::KwDecreasing,
            TokenKind::Def => SyntaxKind::KwDef,
            TokenKind::Deferred => SyntaxKind::KwDeferred,
            TokenKind::Div => SyntaxKind::KwDiv,
            TokenKind::Elif => SyntaxKind::KwElif,
            TokenKind::Else => SyntaxKind::KwElse,
            TokenKind::Elseif => SyntaxKind::KwElseif,
            TokenKind::Elsif => SyntaxKind::KwElsif,
            TokenKind::End => SyntaxKind::KwEnd,
            TokenKind::EndCase => SyntaxKind::KwEndCase,
            TokenKind::EndFor => SyntaxKind::KwEndFor,
            TokenKind::EndIf => SyntaxKind::KwEndIf,
            TokenKind::EndLoop => SyntaxKind::KwEndLoop,
            TokenKind::Enum => SyntaxKind::KwEnum,
            TokenKind::Exit => SyntaxKind::KwExit,
            TokenKind::Export => SyntaxKind::KwExport,
            TokenKind::External => SyntaxKind::KwExternal,
            TokenKind::False => SyntaxKind::KwFalse,
            TokenKind::Flexible => SyntaxKind::KwFlexible,
            TokenKind::For => SyntaxKind::KwFor,
            TokenKind::Fork => SyntaxKind::KwFork,
            TokenKind::Forward => SyntaxKind::KwForward,
            TokenKind::Free => SyntaxKind::KwFree,
            TokenKind::Function => SyntaxKind::KwFunction,
            TokenKind::Get => SyntaxKind::KwGet,
            TokenKind::Handler => SyntaxKind::KwHandler,
            TokenKind::If => SyntaxKind::KwIf,
            TokenKind::Implement => SyntaxKind::KwImplement,
            TokenKind::Import => SyntaxKind::KwImport,
            TokenKind::In => SyntaxKind::KwIn,
            TokenKind::Include => SyntaxKind::KwInclude,
            TokenKind::Inherit => SyntaxKind::KwInherit,
            TokenKind::Init => SyntaxKind::KwInit,
            TokenKind::Int => SyntaxKind::KwInt,
            TokenKind::Int1 => SyntaxKind::KwInt1,
            TokenKind::Int2 => SyntaxKind::KwInt2,
            TokenKind::Int4 => SyntaxKind::KwInt4,
            TokenKind::Invariant => SyntaxKind::KwInvariant,
            TokenKind::Label => SyntaxKind::KwLabel,
            TokenKind::Loop => SyntaxKind::KwLoop,
            TokenKind::Mod => SyntaxKind::KwMod,
            TokenKind::Module => SyntaxKind::KwModule,
            TokenKind::Monitor => SyntaxKind::KwMonitor,
            TokenKind::Nat => SyntaxKind::KwNat,
            TokenKind::Nat1 => SyntaxKind::KwNat1,
            TokenKind::Nat2 => SyntaxKind::KwNat2,
            TokenKind::Nat4 => SyntaxKind::KwNat4,
            TokenKind::New => SyntaxKind::KwNew,
            TokenKind::Nil => SyntaxKind::KwNil,
            TokenKind::Not => SyntaxKind::KwNot,
            TokenKind::ObjectClass => SyntaxKind::KwObjectClass,
            TokenKind::Of => SyntaxKind::KwOf,
            TokenKind::Opaque => SyntaxKind::KwOpaque,
            TokenKind::Open => SyntaxKind::KwOpen,
            TokenKind::Or => SyntaxKind::KwOr,
            TokenKind::Packed => SyntaxKind::KwPacked,
            TokenKind::Pause => SyntaxKind::KwPause,
            TokenKind::Pervasive => SyntaxKind::KwPervasive,
            TokenKind::Pointer => SyntaxKind::KwPointer,
            TokenKind::Post => SyntaxKind::KwPost,
            TokenKind::Pre => SyntaxKind::KwPre,
            TokenKind::Priority => SyntaxKind::KwPriority,
            TokenKind::Procedure => SyntaxKind::KwProcedure,
            TokenKind::Process => SyntaxKind::KwProcess,
            TokenKind::Put => SyntaxKind::KwPut,
            TokenKind::Quit => SyntaxKind::KwQuit,
            TokenKind::Read => SyntaxKind::KwRead,
            TokenKind::Real => SyntaxKind::KwReal,
            TokenKind::Real4 => SyntaxKind::KwReal4,
            TokenKind::Real8 => SyntaxKind::KwReal8,
            TokenKind::Record => SyntaxKind::KwRecord,
            TokenKind::Register => SyntaxKind::KwRegister,
            TokenKind::Rem => SyntaxKind::KwRem,
            TokenKind::Result_ => SyntaxKind::KwResult,
            TokenKind::Return => SyntaxKind::KwReturn,
            TokenKind::Seek => SyntaxKind::KwSeek,
            TokenKind::Self_ => SyntaxKind::KwSelf,
            TokenKind::Set => SyntaxKind::KwSet,
            TokenKind::Shl => SyntaxKind::KwShl,
            TokenKind::Shr => SyntaxKind::KwShr,
            TokenKind::Signal => SyntaxKind::KwSignal,
            TokenKind::SizeOf => SyntaxKind::KwSizeOf,
            TokenKind::Skip => SyntaxKind::KwSkip,
            TokenKind::String_ => SyntaxKind::KwString,
            TokenKind::Tag => SyntaxKind::KwTag,
            TokenKind::Tell => SyntaxKind::KwTell,
            TokenKind::Then => SyntaxKind::KwThen,
            TokenKind::Timeout => SyntaxKind::KwTimeout,
            TokenKind::To => SyntaxKind::KwTo,
            TokenKind::True => SyntaxKind::KwTrue,
            TokenKind::Type => SyntaxKind::KwType,
            TokenKind::Unchecked => SyntaxKind::KwUnchecked,
            TokenKind::Union => SyntaxKind::KwUnion,
            TokenKind::Unit => SyntaxKind::KwUnit,
            TokenKind::Unqualified => SyntaxKind::KwUnqualified,
            TokenKind::Var => SyntaxKind::KwVar,
            TokenKind::Wait => SyntaxKind::KwWait,
            TokenKind::When => SyntaxKind::KwWhen,
            TokenKind::Write => SyntaxKind::KwWrite,
            TokenKind::Xor => SyntaxKind::KwXor,
            TokenKind::Identifier => SyntaxKind::Identifier,
            TokenKind::CharLiteral => SyntaxKind::CharLiteral,
            TokenKind::StringLiteral => SyntaxKind::StringLiteral,
            TokenKind::IntLiteral => SyntaxKind::IntLiteral,
            TokenKind::RealLiteral => SyntaxKind::RealLiteral,
            TokenKind::RadixLiteral => SyntaxKind::RadixLiteral,
            TokenKind::Whitespace => SyntaxKind::Whitespace,
            TokenKind::Comment => SyntaxKind::Comment,
            TokenKind::Error => SyntaxKind::Error,
            TokenKind::NumberLiteral(_) => unreachable!(), // always converted out
            TokenKind::PreprocIf => SyntaxKind::PPKwIf,
            TokenKind::PreprocElseIf => SyntaxKind::PPKwElseif,
            TokenKind::PreprocElsIf => SyntaxKind::PPKwElsif,
            TokenKind::PreprocElse => SyntaxKind::PPKwElse,
            TokenKind::PreprocEnd => SyntaxKind::PPKwEnd,
            TokenKind::PreprocEndIf => SyntaxKind::PPKwEndIf,
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind.to_u16().unwrap())
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
pub enum BinaryOp {
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

impl BinaryOp {
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
pub enum UnaryOp {
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

impl UnaryOp {
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
    pub fn as_binary_op(self) -> Option<BinaryOp> {
        let op = match self {
            AssignOp::Add => BinaryOp::Add,
            AssignOp::Sub => BinaryOp::Sub,
            AssignOp::Mul => BinaryOp::Mul,
            AssignOp::Div => BinaryOp::Div,
            AssignOp::RealDiv => BinaryOp::RealDiv,
            AssignOp::Mod => BinaryOp::Mod,
            AssignOp::Rem => BinaryOp::Rem,
            AssignOp::Exp => BinaryOp::Exp,
            AssignOp::And => BinaryOp::And,
            AssignOp::Or => BinaryOp::Or,
            AssignOp::Xor => BinaryOp::Xor,
            AssignOp::Shl => BinaryOp::Shl,
            AssignOp::Shr => BinaryOp::Shr,
            AssignOp::Imply => BinaryOp::Imply,
            _ => return None,
        };

        Some(op)
    }
}

// Possible literal values
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
    // Real Literals
    #[error("real literal is too large")]
    RealTooLarge,
    #[error("real literal is too small")]
    RealTooSmall,
    #[error("invalid real literal")]
    RealInvalid,
    #[error("real literal is missing exponent digits")]
    RealMissingExponent,
    // Int Literals
    #[error("int literal is too large")]
    IntTooLarge,
    #[error("explicit int literal is too large")]
    IntRadixTooLarge,
    #[error("invalid digit for the specified base")]
    IntRadixInvalidDigit(usize, usize),
    #[error("explicit int literal is missing radix digits")]
    IntMissingRadix,
    #[error("base for explicit int literal is not between 2 - 36")]
    IntInvalidBase,
    #[error("invalid int literal")]
    IntInvalid,
    // Char Sequences (should be expandned into separate errors)
    #[error("string literal errors: {0:?}")]
    StringErrors(Vec<(CharSeqParseError, usize, usize)>),
    #[error("char literal errors: {0:?}")]
    CharErrors(Vec<(CharSeqParseError, usize, usize)>),
}

#[derive(Debug, thiserror::Error)]
pub enum CharSeqParseError {
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
