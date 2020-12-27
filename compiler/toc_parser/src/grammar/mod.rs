//! Grammar parsing
mod expr;
mod stmt;

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
