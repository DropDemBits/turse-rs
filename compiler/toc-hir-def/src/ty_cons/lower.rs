//! Lowers a type constructor from the AST representation to the equivalent HIR representation.

use toc_hir_expand::{SemanticFile, SemanticNodePtr, UnstableSemanticLoc};
use toc_salsa_collections::arena::SalsaArena;
use toc_syntax::{PrimitiveKind, ast};

use crate::{
    Db,
    ty_cons::{
        IntSize, LoweredTyCons, NatSize, RealSize, TyCons, TyConsInner, TyConsSpans, TyKind, TyNode,
    },
};

pub(crate) fn lower_ty_cons<'db>(
    db: &'db dyn Db,
    ty_cons: UnstableSemanticLoc<'db, ast::Type>,
) -> LoweredTyCons<'db> {
    let (cons, spans, errors) =
        LowerCtx::new(db, ty_cons.file()).lower_root_ty(ty_cons.to_node(db));
    LoweredTyCons::new(db, cons, spans, errors)
}

struct LowerCtx<'db> {
    db: &'db dyn Db,
    file: SemanticFile<'db>,

    types: SalsaArena<TyKind>,

    spans: TyConsSpans<'db>,
    errors: Vec<TyLowerError<'db>>,
}

impl<'db> LowerCtx<'db> {
    fn new(db: &'db dyn Db, file: SemanticFile<'db>) -> Self {
        Self {
            db,
            file,
            types: Default::default(),
            spans: Default::default(),
            errors: vec![],
        }
    }

    // Lowers a type from the root node position.
    // This allows us to optimize lowering primitive types, as those can share the same inner cons right away.
    fn lower_root_ty(
        mut self,
        ty: ast::Type,
    ) -> (TyCons<'db>, TyConsSpans<'db>, Vec<TyLowerError<'db>>) {
        let root = self.lower_ty(ty);

        let LowerCtx {
            db,
            mut types,
            mut spans,
            mut errors,
            ..
        } = self;

        types.shrink_to_fit();
        spans.shrink_to_fit();
        errors.shrink_to_fit();

        let cons = TyCons::new(db, TyConsInner { types, root });

        (cons, spans, errors)
    }

    fn lower_ty(&mut self, ty: ast::Type) -> TyNode {
        // only needed for pointing to unhandled types
        let ptr: SemanticNodePtr = UnstableSemanticLoc::new(self.file, &ty).into();

        match ty {
            ast::Type::PrimType(prim_ty) => self.lower_primitive_ty(prim_ty),
            ty @ (ast::Type::NameType(_)
            | ast::Type::RangeType(_)
            | ast::Type::EnumType(_)
            | ast::Type::ArrayType(_)
            | ast::Type::SetType(_)
            | ast::Type::RecordType(_)
            | ast::Type::UnionType(_)
            | ast::Type::PointerType(_)
            | ast::Type::FcnType(_)
            | ast::Type::ProcType(_)
            | ast::Type::CollectionType(_)
            | ast::Type::ConditionType(_)) => {
                self.errors.push(TyLowerError::UnhandledTy {
                    ty: SemanticNodePtr::from(ptr),
                });
                self.alloc_ty(TyKind::Missing, ty)
            }
        }
    }

    fn lower_primitive_ty(&mut self, prim_ty: ast::PrimType) -> TyNode {
        let Some(kind) = prim_ty.prim() else {
            return self.alloc_ty(TyKind::Missing, prim_ty);
        };

        let kind = match kind {
            PrimitiveKind::Int => TyKind::Int(IntSize::Int),
            PrimitiveKind::Int1 => TyKind::Int(IntSize::Int1),
            PrimitiveKind::Int2 => TyKind::Int(IntSize::Int2),
            PrimitiveKind::Int4 => TyKind::Int(IntSize::Int4),
            PrimitiveKind::Nat => TyKind::Nat(NatSize::Nat),
            PrimitiveKind::Nat1 => TyKind::Nat(NatSize::Nat1),
            PrimitiveKind::Nat2 => TyKind::Nat(NatSize::Nat2),
            PrimitiveKind::Nat4 => TyKind::Nat(NatSize::Nat4),
            PrimitiveKind::Real => TyKind::Real(RealSize::Real),
            PrimitiveKind::Real4 => TyKind::Real(RealSize::Real4),
            PrimitiveKind::Real8 => TyKind::Real(RealSize::Real8),
            PrimitiveKind::Boolean => TyKind::Boolean,
            PrimitiveKind::AddressInt => TyKind::Nat(NatSize::AddressInt),
            PrimitiveKind::Char => TyKind::Char,
            PrimitiveKind::String => TyKind::String,
            PrimitiveKind::SizedChar(_) | PrimitiveKind::SizedString(_) => {
                self.errors.push(TyLowerError::UnhandledTy {
                    ty: SemanticNodePtr::from(UnstableSemanticLoc::new(self.file, &prim_ty)),
                });
                return self.alloc_ty(TyKind::Missing, prim_ty);
            }
        };

        self.alloc_ty(kind, prim_ty)
    }

    fn alloc_ty(&mut self, ty: TyKind, node: impl Into<ast::Type>) -> TyNode {
        let place = self.types.alloc(ty);
        self.spans
            .types
            .insert(place, UnstableSemanticLoc::new(self.file, &node.into()));

        TyNode(place)
    }
}

// Errors encountered during the type constructor lowering process
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub enum TyLowerError<'db> {
    /// This type isn't lowered yet
    UnhandledTy { ty: SemanticNodePtr<'db> },
}
