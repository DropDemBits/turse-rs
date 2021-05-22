//! Lowering into `Type` HIR nodes
use toc_hir::ty;
use toc_reporting::MessageKind;
use toc_span::Span;
use toc_syntax::ast::{self, AstNode};

impl super::LoweringCtx {
    pub(super) fn lower_type(&mut self, ty: ast::Type) -> Option<ty::TypeIdx> {
        let span = Span::new(self.file, ty.syntax().text_range());

        let ty = match ty {
            ast::Type::PrimType(ty) => self.lower_prim_type(ty),
            ast::Type::NameType(_) => self.unsupported_ty(span),
            ast::Type::RangeType(_) => self.unsupported_ty(span),
            ast::Type::EnumType(_) => self.unsupported_ty(span),
            ast::Type::ArrayType(_) => self.unsupported_ty(span),
            ast::Type::SetType(_) => self.unsupported_ty(span),
            ast::Type::RecordType(_) => self.unsupported_ty(span),
            ast::Type::UnionType(_) => self.unsupported_ty(span),
            ast::Type::PointerType(_) => self.unsupported_ty(span),
            ast::Type::FcnType(_) => self.unsupported_ty(span),
            ast::Type::ProcType(_) => self.unsupported_ty(span),
            ast::Type::CollectionType(_) => self.unsupported_ty(span),
            ast::Type::ConditionType(_) => self.unsupported_ty(span),
        }?;

        Some(self.database.type_nodes.alloc_spanned(ty, span))
    }

    fn unsupported_ty(&mut self, span: Span) -> Option<ty::Type> {
        self.messages
            .report(MessageKind::Error, "unsupported type", span);
        None
    }

    fn lower_prim_type(&mut self, ty: ast::PrimType) -> Option<ty::Type> {
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

        Some(ty::Type::Primitive(kind))
    }

    fn lower_seq_length(&mut self, node: Option<ast::SeqLength>) -> ty::SeqLength {
        match node {
            Some(node) if node.star_token().is_some() => ty::SeqLength::Dynamic,
            Some(node) => ty::SeqLength::Expr(self.lower_required_expr(node.expr())),
            None => {
                // Missing expression
                ty::SeqLength::Expr(self.lower_required_expr(None))
            }
        }
    }
}
