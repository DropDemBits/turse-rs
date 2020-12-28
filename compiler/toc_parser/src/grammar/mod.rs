//! Grammar parsing

/// Helper for matching tokens
macro_rules! match_token {
    (|$parser:ident| match {
        $($tok:expr => $action:block $(,)?)+
        _ .=> $otherwise:expr $(,)?
    }) => {
        match () {
            $( _ if $parser.at($tok) => $action )+
            _ => $otherwise
        }
    };
}

mod expr;
mod stmt;

pub(self) use match_token;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use toc_scanner::token::TokenKind;
use toc_syntax::SyntaxKind;

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    let root = p.start();
    while !p.at_end() {
        stmt::stmt(p);
    }
    root.complete(p, SyntaxKind::Root)
}
