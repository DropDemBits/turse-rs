//! Grammar parsing
mod expr;
mod stmt;
mod ty;

/// Helper for matching tokens
macro_rules! match_token {
    (|$parser:ident| match {
        $($tok:expr => $action:block $(,)?)+
        _ => $otherwise:expr $(,)?
    }) => {
        match () {
            $( _ if $parser.at($tok) => $action )+
            _ => $otherwise
        }
    };
}

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

pub(self) fn name(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    if p.expect(TokenKind::Identifier) {
        Some(m.complete(p, SyntaxKind::Name))
    } else {
        // not found, drop marker
        m.forget(p);
        None
    }
}

pub(self) fn name_list(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();
    self::name(p);

    while p.at(TokenKind::Comma) {
        p.bump();

        self::name(p);
    }

    Some(m.complete(p, SyntaxKind::NameList))
}

/// ParamList ( `'(' Param ( ',' Param )* ')'` )
pub(self) fn param_list(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    if let Some((_, true)) = param(p) {
        loop {
            match param(p) {
                Some((_, true)) => {}      // parsed param, expecting more
                Some((_, false)) => break, // parsed param, end of list
                None => {
                    // missing next param
                    p.error();
                    break;
                }
            }
        }
    }

    Some(m.complete(p, SyntaxKind::ParamList))
}

pub(self) fn param(p: &mut Parser) -> Option<(CompletedMarker, bool)> {
    let m = p.start();
    expr::expr(p);

    let found_comma = p.at(TokenKind::Comma);

    if found_comma {
        // bump ',' onto param
        p.bump();
    }

    Some((m.complete(p, SyntaxKind::Param), found_comma))
}
