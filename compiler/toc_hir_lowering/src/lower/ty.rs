//! Lowering into `Type` HIR nodes
use toc_hir::ty;
use toc_syntax::ast::{self, AstNode};

impl super::LoweringCtx {
    #[allow(unreachable_code)] // No types parsed for now
    #[allow(unused_variables)]
    pub(super) fn lower_type(&mut self, ty: ast::Type) -> Option<ty::TypeIdx> {
        let span = ty.syntax().text_range();

        let ty = match ty {
            ast::Type::PrimType(_) => Some(todo!()),
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
}
