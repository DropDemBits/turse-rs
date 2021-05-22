//! Lowering into `Expr` HIR nodes
use toc_hir::expr;
use toc_reporting::MessageKind;
use toc_span::{Span, Spanned};
use toc_syntax::ast::{self, AstNode};
use toc_syntax::LiteralValue;

impl super::LoweringCtx {
    /// Lowers a required expr. If not present, constructs a `Expr::Missing` node in-place
    pub(super) fn lower_required_expr(&mut self, expr: Option<ast::Expr>) -> expr::ExprIdx {
        if let Some(expr) = expr {
            self.lower_expr(expr)
        } else {
            // Allocate a generic span
            self.database
                .expr_nodes
                .alloc_spanned(expr::Expr::Missing, Default::default())
        }
    }

    /// Lowers an optional expr.
    /// Effectively a mapping function
    pub(super) fn try_lower_expr(&mut self, expr: Option<ast::Expr>) -> Option<expr::ExprIdx> {
        expr.map(|expr| self.lower_expr(expr))
    }

    /// Lowers an expr
    pub(super) fn lower_expr(&mut self, expr: ast::Expr) -> expr::ExprIdx {
        let span = Span::new(self.file, expr.syntax().text_range());

        let expr = match expr {
            ast::Expr::LiteralExpr(expr) => self.lower_literal_expr(expr),
            ast::Expr::ObjClassExpr(_) => self.unsupported_expr(span),
            ast::Expr::InitExpr(_) => self.unsupported_expr(span),
            ast::Expr::NilExpr(_) => self.unsupported_expr(span),
            ast::Expr::SizeOfExpr(_) => self.unsupported_expr(span),
            ast::Expr::BinaryExpr(expr) => self.lower_binary_expr(expr),
            ast::Expr::UnaryExpr(expr) => self.lower_unary_expr(expr),
            ast::Expr::ParenExpr(expr) => self.lower_paren_expr(expr),
            ast::Expr::NameExpr(expr) => self.lower_name_expr(expr),
            ast::Expr::SelfExpr(_) => self.unsupported_expr(span),
            ast::Expr::FieldExpr(_) => self.unsupported_expr(span),
            ast::Expr::DerefExpr(_) => self.unsupported_expr(span),
            ast::Expr::CheatExpr(_) => self.unsupported_expr(span),
            ast::Expr::NatCheatExpr(_) => self.unsupported_expr(span),
            ast::Expr::ArrowExpr(_) => self.unsupported_expr(span),
            ast::Expr::IndirectExpr(_) => self.unsupported_expr(span),
            ast::Expr::BitsExpr(_) => self.unsupported_expr(span),
            ast::Expr::CallExpr(_) => self.unsupported_expr(span),
        }
        .unwrap_or(expr::Expr::Missing);

        self.database.expr_nodes.alloc_spanned(expr, span)
    }

    fn unsupported_expr(&mut self, span: Span) -> Option<expr::Expr> {
        self.messages
            .report(MessageKind::Error, "unsupported expression", span);
        None
    }

    fn lower_literal_expr(&mut self, expr: ast::LiteralExpr) -> Option<expr::Expr> {
        let (value, errs) = expr.literal()?;

        if let Some(errs) = errs {
            let range = expr.syntax().text_range();

            // Report errors
            // TODO: Add note saying to escape the caret for `InvalidCaretEscape`
            for (range, err) in errs.iter().map(|msg| msg.message_at(range)) {
                let span = Span::new(self.file, range);

                self.messages
                    .report(MessageKind::Error, &err.to_string(), span);
            }
        }

        let value = match value {
            LiteralValue::Int(v) => expr::Literal::Integer(v),
            LiteralValue::Real(v) => expr::Literal::Real(v),
            LiteralValue::Char(v) if v.is_empty() => return None,
            LiteralValue::Char(v) if v.len() == 1 => expr::Literal::Char(v.chars().next().unwrap()),
            LiteralValue::Char(v) => expr::Literal::CharSeq(v),
            LiteralValue::String(v) => expr::Literal::String(v),
            LiteralValue::Boolean(v) => expr::Literal::Boolean(v),
        };

        Some(expr::Expr::Literal(value))
    }

    fn lower_binary_expr(&mut self, expr: ast::BinaryExpr) -> Option<expr::Expr> {
        let op_span = toc_span::Span::new(self.file, expr.op_node()?.text_range());
        let op = syntax_to_hir_binary_op(expr.op_kind()?);
        let op = Spanned::new(op, op_span);
        let lhs = self.lower_required_expr(expr.lhs());
        let rhs = self.lower_required_expr(expr.rhs());

        Some(expr::Expr::Binary(expr::Binary { lhs, op, rhs }))
    }

    fn lower_unary_expr(&mut self, expr: ast::UnaryExpr) -> Option<expr::Expr> {
        let op_span = toc_span::Span::new(self.file, expr.op_node()?.text_range());
        let op = syntax_to_hir_unary_op(expr.op_kind()?);
        let op = Spanned::new(op, op_span);
        let rhs = self.lower_required_expr(expr.rhs());

        Some(expr::Expr::Unary(expr::Unary { op, rhs }))
    }

    fn lower_paren_expr(&mut self, expr: ast::ParenExpr) -> Option<expr::Expr> {
        let expr = self.lower_required_expr(expr.expr());
        Some(expr::Expr::Paren(expr::Paren { expr }))
    }

    fn lower_name_expr(&mut self, expr: ast::NameExpr) -> Option<expr::Expr> {
        let name = expr.name()?.identifier_token()?;
        let span = Span::new(self.file, name.text_range());
        let use_id = self.scopes.use_sym(name.text(), span);
        Some(expr::Expr::Name(expr::Name::Name(use_id)))
    }
}

fn syntax_to_hir_binary_op(op: toc_syntax::InfixOp) -> expr::BinaryOp {
    match op {
        toc_syntax::InfixOp::Add => expr::BinaryOp::Add,
        toc_syntax::InfixOp::Sub => expr::BinaryOp::Sub,
        toc_syntax::InfixOp::Mul => expr::BinaryOp::Mul,
        toc_syntax::InfixOp::Div => expr::BinaryOp::Div,
        toc_syntax::InfixOp::RealDiv => expr::BinaryOp::RealDiv,
        toc_syntax::InfixOp::Mod => expr::BinaryOp::Mod,
        toc_syntax::InfixOp::Rem => expr::BinaryOp::Rem,
        toc_syntax::InfixOp::Exp => expr::BinaryOp::Exp,
        toc_syntax::InfixOp::And => expr::BinaryOp::And,
        toc_syntax::InfixOp::Or => expr::BinaryOp::Or,
        toc_syntax::InfixOp::Xor => expr::BinaryOp::Xor,
        toc_syntax::InfixOp::Shl => expr::BinaryOp::Shl,
        toc_syntax::InfixOp::Shr => expr::BinaryOp::Shr,
        toc_syntax::InfixOp::Less => expr::BinaryOp::Less,
        toc_syntax::InfixOp::LessEq => expr::BinaryOp::LessEq,
        toc_syntax::InfixOp::Greater => expr::BinaryOp::Greater,
        toc_syntax::InfixOp::GreaterEq => expr::BinaryOp::GreaterEq,
        toc_syntax::InfixOp::Equal => expr::BinaryOp::Equal,
        toc_syntax::InfixOp::NotEqual => expr::BinaryOp::NotEqual,
        toc_syntax::InfixOp::In => expr::BinaryOp::In,
        toc_syntax::InfixOp::NotIn => expr::BinaryOp::NotIn,
        toc_syntax::InfixOp::Imply => expr::BinaryOp::Imply,
        _ => unreachable!(),
    }
}

fn syntax_to_hir_unary_op(op: toc_syntax::PrefixOp) -> expr::UnaryOp {
    match op {
        toc_syntax::PrefixOp::Not => expr::UnaryOp::Not,
        toc_syntax::PrefixOp::Identity => expr::UnaryOp::Identity,
        toc_syntax::PrefixOp::Negate => expr::UnaryOp::Negate,
        _ => unreachable!(),
    }
}
