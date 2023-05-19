//! Sink for events
use crate::{event::Event, ParseTree};

use rowan::GreenNodeBuilder;
use std::mem;
use toc_reporting::{CompileResult, FileRange, MessageBundle};
use toc_scanner::token::{Token, TokenKind};
use toc_syntax::SyntaxKind;

// TODO: Move tail trivia somewhere else!

pub(super) struct Sink<'t, 'src> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'src>],
    cursor: usize,
    depth: usize,
    events: Vec<Event>,
    messages: MessageBundle<FileRange>,
}

impl<'t, 'src> Sink<'t, 'src> {
    pub(super) fn new(
        tokens: &'t [Token<'src>],
        events: Vec<Event>,
        messages: MessageBundle<FileRange>,
    ) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            depth: 0,
            events,
            messages,
        }
    }

    pub(super) fn finish(mut self) -> CompileResult<ParseTree, FileRange> {
        for idx in 0..self.events.len() {
            match mem::take(&mut self.events[idx]) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    if self.depth > 0 {
                        // Give trivia to the parent node
                        self.skip_trivia();
                    }

                    let mut look_at = forward_parent.map(|off| idx + off);
                    let mut parent_kinds = vec![kind];

                    while let Some(parent_at) = look_at {
                        // Pull forward parent node
                        let node = mem::take(&mut self.events[parent_at]);

                        if let Event::StartNode {
                            kind,
                            forward_parent,
                        } = node
                        {
                            // Jump to the next parent
                            look_at = forward_parent.map(|off| parent_at + off);
                            parent_kinds.push(kind);
                        } else {
                            unreachable!();
                        }
                    }

                    // Push the nodes, from outermost (last) to innermost (first)
                    parent_kinds.into_iter().rev().for_each(|kind| {
                        self.builder.start_node(kind.into());
                        self.depth += 1;
                    });
                }
                Event::AddToken => {
                    // Always nom on trivia before the actual token
                    self.skip_trivia();
                    self.token();
                }
                Event::FinishNode => {
                    if self.depth == 1 {
                        // Only on the last node do we consume trivia,
                        // since we don't want extra trivia on the end of other nodes
                        // (this affects error reporting span)
                        self.skip_trivia();
                    }
                    self.depth -= 1;
                    self.builder.finish_node();
                }
                Event::Tombstone | Event::Placeholder => {} // No-op
            }
        }

        let tree = ParseTree {
            node: self.builder.finish(),
        };

        CompileResult::new(tree, self.messages)
    }

    fn token(&mut self) {
        let token = &self.tokens[self.cursor];
        let kind: SyntaxKind = TokenWrapper(token.kind).into();
        let text = token.lexeme;

        self.builder.token(kind.into(), text);
        self.cursor += 1;
    }

    /// Skips all whitespace, including comments
    fn skip_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            self.token()
        }
    }
}

/// Wrapper type for locally impl'ing `From`/`Into`
#[repr(transparent)]
struct TokenWrapper(TokenKind);

impl From<TokenWrapper> for SyntaxKind {
    fn from(token: TokenWrapper) -> Self {
        match token.0 {
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
