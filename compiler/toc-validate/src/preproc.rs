//! Checking for preprocessor directives
#[cfg(test)]
mod test;

use toc_syntax::ast::{self, AstNode};

use crate::{ValidateCtx, match_ast, without_matching};

pub(super) fn validate_preproc_glob(glob: ast::PreprocGlob, ctx: &mut ValidateCtx) {
    let directive = glob.directive().unwrap();
    match_ast!(match (directive.syntax()) {
        ast::PPElseif(nd) => without_matching(nd.syntax(), "#if", ctx),
        ast::PPElse(nd) => without_matching(nd.syntax(), "#if", ctx),
        ast::PPEndIf(nd) => without_matching(nd.syntax(), "#if", ctx),
        _ => (),
    });
}
