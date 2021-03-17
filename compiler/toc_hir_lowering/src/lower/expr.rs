//! Lowering into `Expr` HIR nodes
use toc_hir::expr;
use toc_reporting::{MessageKind, MessageSink};
use toc_span::TextRange;
use toc_syntax::ast::{self, AstNode};
use toc_syntax::{CharSeqParseError, LiteralParseError, LiteralValue};

impl super::LoweringCtx {
    pub(super) fn lower_expr(&mut self, expr: ast::Expr) -> (expr::Expr, TextRange) {
        let span = expr.syntax().text_range();

        let expr = match expr {
            ast::Expr::LiteralExpr(expr) => self.lower_literal_expr(expr, span),
            ast::Expr::ObjClassExpr(_) => todo!(),
            ast::Expr::InitExpr(_) => todo!(),
            ast::Expr::NilExpr(_) => todo!(),
            ast::Expr::SizeOfExpr(_) => todo!(),
            ast::Expr::BinaryExpr(_) => todo!(),
            ast::Expr::UnaryExpr(_) => todo!(),
            ast::Expr::ParenExpr(_) => todo!(),
            ast::Expr::NameExpr(expr) => self.lower_name_expr(expr),
            ast::Expr::SelfExpr(_) => todo!(),
            ast::Expr::FieldExpr(_) => todo!(),
            ast::Expr::DerefExpr(_) => todo!(),
            ast::Expr::CheatExpr(_) => todo!(),
            ast::Expr::NatCheatExpr(_) => todo!(),
            ast::Expr::ArrowExpr(_) => todo!(),
            ast::Expr::IndirectExpr(_) => todo!(),
            ast::Expr::BitsExpr(_) => todo!(),
            ast::Expr::CallExpr(_) => todo!(),
        }
        .unwrap_or_else(|| expr::Expr::Missing);

        (expr, span)
    }

    /// Spreads out char sequence errors into individual messages
    fn spread_char_seq_errors(
        context: &str,
        messages: &mut MessageSink,
        errors: Vec<(CharSeqParseError, usize, usize)>,
        source_span: TextRange,
    ) {
        for (err, start, end) in errors {
            // Adjust to be at the the invalid character
            messages.report(
                MessageKind::Error,
                &format!("{}: {}", context, err.to_string()),
                super::offset_span(start, end, source_span),
            );
            // TODO: Add note saying to escape the caret for `InvalidCaretEscape`
        }
    }

    fn lower_literal_expr(
        &mut self,
        expr: ast::LiteralExpr,
        span: TextRange,
    ) -> Option<expr::Expr> {
        let (value, err) = expr.literal()?;
        if let Some(err) = err {
            // Report error
            match err {
                LiteralParseError::IntRadixInvalidDigit(start, end) => {
                    // Adjust to be at the the invalid character
                    self.messages.report(
                        MessageKind::Error,
                        &err.to_string(),
                        super::offset_span(start, end, span),
                    );
                }
                LiteralParseError::CharErrors(errors) => {
                    Self::spread_char_seq_errors(
                        "invalid char literal",
                        &mut self.messages,
                        errors,
                        span,
                    );
                }
                LiteralParseError::StringErrors(errors) => {
                    Self::spread_char_seq_errors(
                        "invalid string literal",
                        &mut self.messages,
                        errors,
                        span,
                    );
                }
                _ => {
                    self.messages
                        .report(MessageKind::Error, &err.to_string(), span);
                }
            }
        }

        let value = match value {
            LiteralValue::Int(v) => expr::Literal::Integer(v),
            LiteralValue::Real(v) => expr::Literal::Real(v),
            LiteralValue::Char(v) => expr::Literal::CharSeq(v),
            LiteralValue::String(v) => expr::Literal::String(v),
            LiteralValue::Boolean(v) => expr::Literal::Boolean(v),
        };

        Some(expr::Expr::Literal(value))
    }

    fn lower_name_expr(&mut self, expr: ast::NameExpr) -> Option<expr::Expr> {
        let name = expr.name()?.identifier_token()?;
        let use_id = self.scopes.use_sym(name.text(), name.text_range());
        Some(expr::Expr::Name(expr::Name::Name(use_id)))
    }
}
