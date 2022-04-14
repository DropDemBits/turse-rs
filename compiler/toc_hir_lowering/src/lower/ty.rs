//! Lowering into `Type` HIR nodes
use toc_hir::symbol::syms;
use toc_hir::{symbol, ty};
use toc_span::Span;
use toc_syntax::ast::{self, AstNode};

impl super::BodyLowering<'_, '_> {
    /// Lowers a required type. If not present, constructs a `Type::Missing` node in-place
    pub(super) fn lower_required_type(&mut self, ty: Option<ast::Type>) -> ty::TypeId {
        ty.and_then(|ty| self.lower_type(ty)).unwrap_or_else(|| {
            // Allocate a generic span
            let ty = ty::Type {
                kind: ty::TypeKind::Missing,
                span: self.ctx.library.span_map.dummy_span(),
            };
            self.ctx.library.intern_type(ty)
        })
    }

    /// Lowers a type.
    pub(super) fn lower_type(&mut self, ty: ast::Type) -> Option<ty::TypeId> {
        let span = self.ctx.mk_span(ty.syntax().text_range());

        let kind = match ty {
            ast::Type::PrimType(ty) => self.lower_prim_type(ty),
            ast::Type::NameType(ty) => self.lower_name_type(ty),

            ast::Type::RangeType(_) => self.unsupported_ty(span),
            ast::Type::EnumType(_) => self.unsupported_ty(span),
            ast::Type::ArrayType(_) => self.unsupported_ty(span),

            ast::Type::SetType(ty) => self.lower_set_type(ty),

            ast::Type::RecordType(_) => self.unsupported_ty(span),
            ast::Type::UnionType(_) => self.unsupported_ty(span),

            ast::Type::PointerType(ty) => self.lower_pointer_type(ty),
            ast::Type::FcnType(ty) => self.lower_fcn_type(ty),
            ast::Type::ProcType(ty) => self.lower_proc_type(ty),

            ast::Type::CollectionType(_) => self.unsupported_ty(span),
            ast::Type::ConditionType(_) => self.unsupported_ty(span),
        }?;

        let span = self.ctx.library.intern_span(span);
        let ty = ty::Type { kind, span };
        Some(self.ctx.library.intern_type(ty))
    }

    fn unsupported_ty(&mut self, span: Span) -> Option<ty::TypeKind> {
        self.ctx
            .messages
            .error("unsupported type", "this type is not handled yet", span);
        None
    }

    fn lower_prim_type(&mut self, ty: ast::PrimType) -> Option<ty::TypeKind> {
        let kind = match ty.prim()? {
            toc_syntax::PrimitiveKind::Int => ty::Primitive::Int,
            toc_syntax::PrimitiveKind::Int1 => ty::Primitive::Int1,
            toc_syntax::PrimitiveKind::Int2 => ty::Primitive::Int2,
            toc_syntax::PrimitiveKind::Int4 => ty::Primitive::Int4,
            toc_syntax::PrimitiveKind::Nat => ty::Primitive::Nat,
            toc_syntax::PrimitiveKind::Nat1 => ty::Primitive::Nat1,
            toc_syntax::PrimitiveKind::Nat2 => ty::Primitive::Nat2,
            toc_syntax::PrimitiveKind::Nat4 => ty::Primitive::Nat4,
            toc_syntax::PrimitiveKind::Real => ty::Primitive::Real,
            toc_syntax::PrimitiveKind::Real4 => ty::Primitive::Real4,
            toc_syntax::PrimitiveKind::Real8 => ty::Primitive::Real8,
            toc_syntax::PrimitiveKind::Boolean => ty::Primitive::Boolean,
            toc_syntax::PrimitiveKind::AddressInt => ty::Primitive::AddressInt,
            toc_syntax::PrimitiveKind::Char => ty::Primitive::Char,
            toc_syntax::PrimitiveKind::String => ty::Primitive::String,
            toc_syntax::PrimitiveKind::SizedChar(node) => {
                ty::Primitive::SizedChar(self.lower_seq_length(node))
            }
            toc_syntax::PrimitiveKind::SizedString(node) => {
                ty::Primitive::SizedString(self.lower_seq_length(node))
            }
        };

        Some(ty::TypeKind::Primitive(kind))
    }

    fn lower_seq_length(&mut self, node: Option<ast::SeqLength>) -> ty::SeqLength {
        match node {
            Some(node) if node.star_token().is_some() => ty::SeqLength::Any,
            seq_length => {
                let expr = seq_length.and_then(|node| node.expr());
                let body = self.lower_required_expr_body(expr);
                ty::SeqLength::Expr(body)
            }
        }
    }

    fn lower_name_type(&mut self, ty: ast::NameType) -> Option<ty::TypeKind> {
        // First node should either be a NameExpr or a FieldExpr
        let span = self.ctx.mk_span(ty.syntax().text_range());

        match ty.expr()? {
            ast::Expr::NameExpr(expr) => {
                // simple alias
                let name = expr.name()?.identifier_token()?;
                let def_id = self.ctx.use_sym(name.text().into(), span);

                Some(ty::TypeKind::Alias(ty::Alias(def_id)))
            }
            ast::Expr::FieldExpr(_expr) => {
                // FIXME: Support type paths
                // (type) path, which is not supported yet
                self.ctx
                    .messages
                    .error("unsupported type", "type paths are not handled yet", span);
                None
            }
            _ => {
                // Not a valid named type
                self.ctx
                    .messages
                    .error("invalid type", "expressions can't be used as types", span);
                None
            }
        }
    }

    fn lower_set_type(&mut self, ty: ast::SetType) -> Option<ty::TypeKind> {
        let span = self.ctx.intern_range(ty.syntax().text_range());
        let elem = self.lower_required_type(ty.elem_ty());
        let def_id = self
            .ctx
            .library
            .add_def(*syms::Anonymous, span, symbol::SymbolKind::Declared);

        Some(ty::TypeKind::Set(ty::Set {
            def_id,
            elem_ty: elem,
        }))
    }

    fn lower_pointer_type(&mut self, ty: ast::PointerType) -> Option<ty::TypeKind> {
        let is_checked = ty
            .is_checked()
            .map(|checked| checked.unchecked_token().is_none())
            .unwrap_or(true);
        let ty = self.lower_required_type(ty.to_ty());

        let checked = if is_checked {
            ty::Checked::Checked
        } else {
            ty::Checked::Unchecked
        };

        Some(ty::TypeKind::Pointer(ty::Pointer { checked, ty }))
    }

    fn lower_proc_type(&mut self, ty: ast::ProcType) -> Option<ty::TypeKind> {
        let param_list = self
            .lower_formals_spec(ty.params())
            .map(|params| params.tys);
        let return_ty = {
            let span = self.ctx.intern_range(ty.syntax().text_range());
            self.ctx.library.intern_type(ty::Type {
                kind: ty::TypeKind::Void,
                span,
            })
        };

        Some(ty::TypeKind::Subprogram(ty::Subprogram {
            kind: symbol::SubprogramKind::Procedure,
            param_list,
            result_ty: return_ty,
        }))
    }

    fn lower_fcn_type(&mut self, ty: ast::FcnType) -> Option<ty::TypeKind> {
        let param_list = self
            .lower_formals_spec(ty.params())
            .map(|params| params.tys);
        let return_ty = self.lower_required_type(ty.ty());

        Some(ty::TypeKind::Subprogram(ty::Subprogram {
            kind: symbol::SubprogramKind::Function,
            param_list,
            result_ty: return_ty,
        }))
    }
}
