//! Lowering into `Type` HIR nodes
use toc_hir::ty;
use toc_syntax::ast::{self, AstNode};

impl super::LoweringCtx {
    pub(super) fn lower_type(&mut self, ty: ast::Type) -> Option<ty::TypeIdx> {
        let span = ty.syntax().text_range();

        let ty = match ty {
            ast::Type::PrimType(ty) => self.lower_prim_type(ty),
            ast::Type::NameType(_) => todo!(),
            ast::Type::RangeType(_) => todo!(),
            ast::Type::EnumType(_) => todo!(),
            ast::Type::ArrayType(_) => todo!(),
            ast::Type::SetType(_) => todo!(),
            ast::Type::RecordType(_) => todo!(),
            ast::Type::UnionType(_) => todo!(),
            ast::Type::PointerType(_) => todo!(),
            ast::Type::FcnType(_) => todo!(),
            ast::Type::ProcType(_) => todo!(),
            ast::Type::CollectionType(_) => todo!(),
            ast::Type::ConditionType(_) => todo!(),
        }?;

        Some(self.database.type_nodes.alloc_spanned(ty, span))
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
        if let Some(node) = node {
            if node.star_token().is_some() {
                ty::SeqLength::Dynamic
            } else {
                // Lower expr (if not present, then a none)
                ty::SeqLength::Expr(self.lower_required_expr(node.expr()))
            }
        } else {
            // Missing expression
            ty::SeqLength::Expr(self.lower_required_expr(None))
        }
    }
}
