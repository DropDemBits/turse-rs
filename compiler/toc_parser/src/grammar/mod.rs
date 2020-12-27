//! Grammar parsing
mod expr;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use toc_scanner::token::TokenKind;
use toc_syntax::SyntaxKind;

pub(crate) fn root(parser: &mut Parser) -> CompletedMarker {
    let root = parser.start();
    while parser.peek().is_some() {
        expr::expr(parser);
    }
    root.complete(parser, SyntaxKind::Root)
}
