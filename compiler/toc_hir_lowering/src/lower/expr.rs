//! Lowering into `Expr` HIR nodes
use toc_hir::expr;
use toc_reporting::{MessageKind, MessageSink};
use toc_span::TextRange;
use toc_syntax::ast::{self, AstNode};
use toc_syntax::{CharSeqParseError, LiteralParseError, LiteralValue};

impl super::LoweringCtx {
    /// Lowers a required expr, but the input Expr may not be present
    pub(super) fn maybe_lower_expr(&mut self, expr: Option<ast::Expr>) -> expr::ExprIdx {
        if let Some(expr) = expr {
            self.lower_expr(expr)
        } else {
            // Allocate a generice span
            self.database
                .expr_nodes
                .alloc_spanned(expr::Expr::Missing, TextRange::default())
        }
    }

    /// Lowers an expr
    pub(super) fn lower_expr(&mut self, expr: ast::Expr) -> expr::ExprIdx {
        let span = expr.syntax().text_range();

        let expr = match expr {
            ast::Expr::LiteralExpr(expr) => self.lower_literal_expr(expr, span),
            ast::Expr::ObjClassExpr(_) => todo!(),
            ast::Expr::InitExpr(_) => todo!(),
            ast::Expr::NilExpr(_) => todo!(),
            ast::Expr::SizeOfExpr(_) => todo!(),
            ast::Expr::BinaryExpr(expr) => self.lower_binary_expr(expr),
            ast::Expr::UnaryExpr(expr) => self.lower_unary_expr(expr),
            ast::Expr::ParenExpr(expr) => self.lower_paren_expr(expr),
            ast::Expr::NameExpr(expr) => self.lower_name_expr(expr),
            ast::Expr::SelfExpr(expr) => self.lower_self_expr(expr),
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

        self.database.expr_nodes.alloc_spanned(expr, span)
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
                    spread_char_seq_errors(
                        "invalid char literal",
                        &mut self.messages,
                        errors,
                        span,
                    );
                }
                LiteralParseError::StringErrors(errors) => {
                    spread_char_seq_errors(
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

    fn lower_binary_expr(&mut self, expr: ast::BinaryExpr) -> Option<expr::Expr> {
        let op = syntax_to_hir_binary_op(expr.op_kind()?);
        let lhs = self.maybe_lower_expr(expr.lhs());
        let rhs = self.maybe_lower_expr(expr.rhs());

        Some(expr::Expr::Binary(expr::Binary { lhs, op, rhs }))
    }

    fn lower_unary_expr(&mut self, expr: ast::UnaryExpr) -> Option<expr::Expr> {
        let op = syntax_to_hir_unary_op(expr.op_kind()?);
        let rhs = self.maybe_lower_expr(expr.rhs());

        Some(expr::Expr::Unary(expr::Unary { op, rhs }))
    }

    fn lower_paren_expr(&mut self, expr: ast::ParenExpr) -> Option<expr::Expr> {
        let expr = self.maybe_lower_expr(expr.expr());
        Some(expr::Expr::Paren(expr::Paren { expr }))
    }

    fn lower_name_expr(&mut self, expr: ast::NameExpr) -> Option<expr::Expr> {
        let name = expr.name()?.identifier_token()?;
        let use_id = self.scopes.use_sym(name.text(), name.text_range());
        Some(expr::Expr::Name(expr::Name::Name(use_id)))
    }

    fn lower_self_expr(&mut self, _expr: ast::SelfExpr) -> Option<expr::Expr> {
        Some(expr::Expr::Name(expr::Name::Self_))
    }
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

fn syntax_to_hir_binary_op(op: toc_syntax::BinaryOp) -> expr::BinaryOp {
    match op {
        toc_syntax::BinaryOp::Add => expr::BinaryOp::Add,
        toc_syntax::BinaryOp::Sub => expr::BinaryOp::Sub,
        toc_syntax::BinaryOp::Mul => expr::BinaryOp::Mul,
        toc_syntax::BinaryOp::Div => expr::BinaryOp::Div,
        toc_syntax::BinaryOp::RealDiv => expr::BinaryOp::RealDiv,
        toc_syntax::BinaryOp::Mod => expr::BinaryOp::Mod,
        toc_syntax::BinaryOp::Rem => expr::BinaryOp::Rem,
        toc_syntax::BinaryOp::Exp => expr::BinaryOp::Exp,
        toc_syntax::BinaryOp::And => expr::BinaryOp::And,
        toc_syntax::BinaryOp::Or => expr::BinaryOp::Or,
        toc_syntax::BinaryOp::Xor => expr::BinaryOp::Xor,
        toc_syntax::BinaryOp::Shl => expr::BinaryOp::Shl,
        toc_syntax::BinaryOp::Shr => expr::BinaryOp::Shr,
        toc_syntax::BinaryOp::Less => expr::BinaryOp::Less,
        toc_syntax::BinaryOp::LessEq => expr::BinaryOp::LessEq,
        toc_syntax::BinaryOp::Greater => expr::BinaryOp::Greater,
        toc_syntax::BinaryOp::GreaterEq => expr::BinaryOp::GreaterEq,
        toc_syntax::BinaryOp::Equal => expr::BinaryOp::Equal,
        toc_syntax::BinaryOp::NotEqual => expr::BinaryOp::NotEqual,
        toc_syntax::BinaryOp::In => expr::BinaryOp::In,
        toc_syntax::BinaryOp::NotIn => expr::BinaryOp::NotIn,
        toc_syntax::BinaryOp::Imply => expr::BinaryOp::Imply,
        toc_syntax::BinaryOp::Arrow => expr::BinaryOp::Arrow,
        toc_syntax::BinaryOp::Dot => expr::BinaryOp::Dot,
        toc_syntax::BinaryOp::Call => expr::BinaryOp::Call,
    }
}

fn syntax_to_hir_unary_op(op: toc_syntax::UnaryOp) -> expr::UnaryOp {
    match op {
        toc_syntax::UnaryOp::Not => expr::UnaryOp::Not,
        toc_syntax::UnaryOp::Identity => expr::UnaryOp::Identity,
        toc_syntax::UnaryOp::Negate => expr::UnaryOp::Negate,
        toc_syntax::UnaryOp::NatCheat => expr::UnaryOp::NatCheat,
        toc_syntax::UnaryOp::Deref => expr::UnaryOp::Deref,
    }
}
