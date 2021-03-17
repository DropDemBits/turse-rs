//! Lowering into `Type` HIR nodes
use toc_hir::ty;
use toc_span::TextRange;
use toc_syntax::ast::{self, AstNode};

impl super::LoweringCtx {
    pub(super) fn lower_type(&mut self, ty: ast::Type) -> Option<(ty::Type, TextRange)> {
        let _span = ty.syntax().text_range();

        match ty {
            ast::Type::PrimType(_) => todo!(),
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
        }
    }
}
