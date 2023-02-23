//! Lowering into `Expr` HIR nodes
use toc_hir::{
    body, expr,
    span::{HasSpanTable, SpanId, Spanned},
};
use toc_span::{Span, TextRange};
use toc_syntax::{
    ast::{self, AstNode},
    LiteralValue,
};

impl super::BodyLowering<'_, '_> {
    /// Lowers a required expr. If not present, constructs a `Expr::Missing` node in-place
    pub(super) fn lower_required_expr(&mut self, expr: Option<ast::Expr>) -> expr::ExprId {
        if let Some(expr) = expr {
            self.lower_expr(expr)
        } else {
            self.missing_expr()
        }
    }

    /// Lowers a required expr body. If not present, constructs an empty expression body
    pub(super) fn lower_required_expr_body(&mut self, expr: Option<ast::Expr>) -> body::BodyId {
        match expr {
            Some(expr) => self.ctx.lower_expr_body(expr),
            None => self.ctx.lower_empty_expr_body(),
        }
    }

    /// Lowers an expression body
    pub(super) fn lower_expr_body(&mut self, expr: ast::Expr) -> body::BodyId {
        self.ctx.lower_expr_body(expr)
    }

    /// Lowers an optional expr.
    /// Effectively a mapping function
    pub(super) fn try_lower_expr(&mut self, expr: Option<ast::Expr>) -> Option<expr::ExprId> {
        expr.map(|expr| self.lower_expr(expr))
    }

    /// Lowers an expr
    pub(super) fn lower_expr(&mut self, expr: ast::Expr) -> expr::ExprId {
        let span = self.ctx.intern_range(expr.syntax().text_range());

        let kind = match expr {
            ast::Expr::LiteralExpr(expr) => self.lower_literal_expr(expr),

            ast::Expr::ObjClassExpr(_) => self.unsupported_expr(span),

            ast::Expr::InitExpr(expr) => self.lower_init_expr(expr),

            ast::Expr::NilExpr(_) => self.unsupported_expr(span),
            ast::Expr::SizeOfExpr(_) => self.unsupported_expr(span),

            ast::Expr::BinaryExpr(expr) => self.lower_binary_expr(expr),
            ast::Expr::UnaryExpr(expr) => self.lower_unary_expr(expr),
            ast::Expr::ParenExpr(expr) => {
                // Explicitly unwrap paren exprs (they don't exist in the HIR tree)
                return self.lower_required_expr(expr.expr());
            }
            ast::Expr::NameExpr(expr) => self.lower_name_expr(expr),
            ast::Expr::SelfExpr(_) => self.unsupported_expr(span),
            ast::Expr::FieldExpr(expr) => self.lower_field_expr(expr),
            ast::Expr::DerefExpr(expr) => self.lower_deref_expr(expr),

            ast::Expr::CheatExpr(_) => self.unsupported_expr(span),
            ast::Expr::NatCheatExpr(_) => self.unsupported_expr(span),
            ast::Expr::ArrowExpr(_) => self.unsupported_expr(span),
            ast::Expr::IndirectExpr(_) => self.unsupported_expr(span),
            ast::Expr::BitsExpr(_) => self.unsupported_expr(span),

            ast::Expr::CallExpr(expr) => self.lower_call_expr(expr),
        }
        .unwrap_or(expr::ExprKind::Missing);

        let expr = expr::Expr { kind, span };

        self.body.add_expr(expr)
    }

    fn mk_expr(&mut self, kind: expr::ExprKind, span: TextRange) -> expr::ExprId {
        let span = self.ctx.intern_range(span);
        self.body.add_expr(expr::Expr { kind, span })
    }

    fn missing_expr(&mut self) -> expr::ExprId {
        // Allocate a generic span
        let missing = expr::Expr {
            kind: expr::ExprKind::Missing,
            span: self.ctx.library.span_table().dummy_span(),
        };
        self.body.add_expr(missing)
    }

    fn unsupported_expr(&mut self, span: SpanId) -> Option<expr::ExprKind> {
        let span = span.lookup_in(&self.ctx.library);
        self.ctx.messages.error(
            "unsupported expression",
            "this expression is not supported yet",
            span,
        );
        None
    }

    fn lower_literal_expr(&mut self, expr: ast::LiteralExpr) -> Option<expr::ExprKind> {
        let (value, errs) = expr.literal()?;

        if let Some(errors) = errs {
            let range = expr.syntax().text_range();

            // Report errors
            // TODO: Add note saying to escape the caret for `InvalidCaretEscape`
            let span = self.ctx.mk_span(range);
            let mut message = self.ctx.messages.error_detailed(errors.header(), span);

            for (msg, range) in errors.parts(range) {
                let span = Span::new(self.ctx.file, range);
                message = message.with_error(&msg, span);
            }

            message.finish();
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

        Some(expr::ExprKind::Literal(value))
    }

    fn lower_init_expr(&mut self, expr: ast::InitExpr) -> Option<expr::ExprKind> {
        let exprs = expr
            .expr_list()
            .unwrap()
            .exprs()
            .map(|expr| self.lower_expr_body(expr))
            .collect();

        Some(expr::ExprKind::Init(expr::Init { exprs }))
    }

    fn lower_binary_expr(&mut self, expr: ast::BinaryExpr) -> Option<expr::ExprKind> {
        let op = syntax_to_hir_binary_op(expr.op_kind()?);
        let op_span = self.ctx.intern_range(expr.op_node()?.text_range());
        let op = Spanned::new(op, op_span);
        let lhs = self.lower_required_expr(expr.lhs());
        let rhs = self.lower_required_expr(expr.rhs());

        Some(expr::ExprKind::Binary(expr::Binary { lhs, op, rhs }))
    }

    fn lower_unary_expr(&mut self, expr: ast::UnaryExpr) -> Option<expr::ExprKind> {
        let op = syntax_to_hir_unary_op(expr.op_kind()?);
        let op_span = self.ctx.intern_range(expr.op_node()?.text_range());
        let op = Spanned::new(op, op_span);
        let rhs = self.lower_required_expr(expr.rhs());

        Some(expr::ExprKind::Unary(expr::Unary { op, rhs }))
    }

    fn lower_name_expr(&mut self, expr: ast::NameExpr) -> Option<expr::ExprKind> {
        let name = expr.name()?.identifier_token()?;
        let span = self.ctx.intern_range(name.text_range());

        Some(expr::ExprKind::Name(expr::Name::Name(Spanned::new(
            name.text().into(),
            span,
        ))))
    }

    fn lower_field_expr(&mut self, expr: ast::FieldExpr) -> Option<expr::ExprKind> {
        let lhs = self.lower_required_expr(expr.expr());
        let field = expr.name()?.identifier_token()?;
        let span = self.ctx.intern_range(field.text_range());

        Some(expr::ExprKind::Field(expr::Field {
            lhs,
            field: Spanned::new(field.text().into(), span),
        }))
    }

    fn lower_deref_expr(&mut self, expr: ast::DerefExpr) -> Option<expr::ExprKind> {
        let op = self.ctx.intern_range(expr.caret_token()?.text_range());
        let rhs = self.lower_required_expr(expr.expr());

        Some(expr::ExprKind::Deref(expr::Deref { op, rhs }))
    }

    fn lower_call_expr(&mut self, expr: ast::CallExpr) -> Option<expr::ExprKind> {
        // ast::CallExpr always has the param list specified
        let lhs = self.lower_required_expr(expr.expr());
        let arguments = self.lower_expr_arg_list(expr.param_list()).unwrap();

        Some(expr::ExprKind::Call(expr::Call { lhs, arguments }))
    }

    pub(super) fn lower_expr_arg_list(
        &mut self,
        args: Option<ast::ParamList>,
    ) -> Option<expr::ArgList> {
        let mut arg_list = vec![];

        for arg in args?.param() {
            let arg = match arg.param_kind() {
                Some(ast::ParamKind::Expr(expr)) => self.lower_expr(expr),
                Some(ast::ParamKind::AllItem(expr)) => {
                    // `all expression`
                    self.mk_expr(expr::ExprKind::All, expr.syntax().text_range())
                }
                Some(ast::ParamKind::RangeItem(expr)) => {
                    // Range expression
                    let mut lower_range_bound = |range_bound: Option<ast::RangeBound>| {
                        let Some(range_bound) = range_bound else {
                            return expr::RangeBound::FromStart(self.missing_expr());
                        };

                        match range_bound {
                            ast::RangeBound::RelativeBound(bound) => {
                                if bound.minus_token().is_some() {
                                    // From end
                                    expr::RangeBound::FromEnd(
                                        self.lower_required_expr(bound.expr()),
                                    )
                                } else {
                                    // At end
                                    expr::RangeBound::AtEnd(
                                        self.ctx
                                            .intern_range(bound.star_token().unwrap().text_range()),
                                    )
                                }
                            }
                            ast::RangeBound::Expr(expr) => {
                                expr::RangeBound::FromStart(self.lower_expr(expr))
                            }
                        }
                    };

                    let span = expr.syntax().text_range();
                    let start = lower_range_bound(expr.start());
                    let end = expr.end().map(|end| lower_range_bound(Some(end)));

                    self.mk_expr(expr::ExprKind::Range(expr::Range { start, end }), span)
                }
                None => self.missing_expr(),
            };

            arg_list.push(arg);
        }

        Some(arg_list)
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
