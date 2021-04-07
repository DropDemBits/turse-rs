//! Type checking
#[cfg(test)]
mod test;

use std::collections::HashMap;

use toc_hir::{expr, stmt, ty as hir_ty};
use toc_reporting::ReportMessage;

use crate::ty::{self, DefKind, TyCtx, TyRef};

// ???: Can we build up a type ctx without doing type propogation?
// Type propogation is inferring of types from inputs
// E.g. this stmt involves some type propogation
// ```ignore
// const k := 3
// ```
// Type of DefId(0) is now `int` (after reifying `Integer`)

// ???: What is the purpose of a type ctx?
// TypeCtx primarily holds mappings from TypeId's to the underlying raw types
// ???: Does it map DefId to TypeId as well?
// The only place that DefId's types are required is when performing const eval
// Type building also needs to be aware of external DefId's, and the export attributes (e.g. if it's `opaque`)
// ???: What about mutability of exports?
// - Mutability in general falls under responsibility of assignability
// - Export mutability matters for mutation outside of the local unit scope, normal const/var rules apply for local units

// Was working on:
// - Type building
// - HIR tree exploration

// Will be working on:
// - Type checking for simple expressions

pub fn typecheck_unit(unit: &toc_hir::Unit) -> (TyCtx, Vec<ReportMessage>) {
    TypeCheck::check_unit(unit)
}

struct TypeCheck {
    ty_ctx: TyCtx,
    eval_kinds: HashMap<expr::ExprIdx, EvalKind>,
    reporter: toc_reporting::MessageSink,
}

impl TypeCheck {
    fn check_unit(unit: &toc_hir::Unit) -> (TyCtx, Vec<ReportMessage>) {
        let mut typeck = Self {
            ty_ctx: TyCtx::new(),
            eval_kinds: HashMap::new(),
            reporter: toc_reporting::MessageSink::new(),
        };

        // Walk tree
        unit.walk_nodes(&mut typeck);

        let Self {
            ty_ctx, reporter, ..
        } = typeck;

        (ty_ctx, reporter.finish())
    }
}

/// ???: Mapping concrete TypeId's back to ty::TypeIdx?
/// Pass in the ty::TypeIdx, which is mapped by the TyCtx
impl toc_hir::HirVisitor for TypeCheck {
    fn visit_unit(&mut self, _unit: &toc_hir::Unit) {}

    fn visit_constvar(&mut self, _id: stmt::StmtIdx, decl: &stmt::ConstVar) {
        // TODO: Map DefId's to the inferred type
        let ty_ref = if let Some(type_spec) = decl.type_spec {
            // From type_spec
            self.ty_ctx.get_type(type_spec).unwrap()
        } else if let Some(expr) = decl.init_expr {
            // From inferred init expr
            let eval_kind = self.eval_kinds.get(&expr).unwrap();

            match eval_kind {
                EvalKind::Ref(def_kind) => match def_kind {
                    DefKind::Const(ty_ref) | DefKind::Var(ty_ref) | DefKind::Error(ty_ref) => {
                        *ty_ref
                    }
                    DefKind::Type(_) => {
                        // Not an expr, is error
                        // TODO: Report this error
                        self.ty_ctx.add_type(ty::Type::Error)
                    }
                },
                EvalKind::Value(ty_ref) => {
                    // Value, take directly
                    *ty_ref
                }
            }
        } else {
            // Bare var decl, checked in parser
            panic!("Encountered bare ConstVar decl");
        };

        // Wrap type in the appropriate def kind
        let def_kind = if decl.is_const {
            DefKind::Const(ty_ref)
        } else {
            DefKind::Var(ty_ref)
        };

        for def in &decl.names {
            self.ty_ctx.map_def_id(*def, def_kind);
        }
    }

    fn visit_assign(&mut self, _id: stmt::StmtIdx, _stmt: &stmt::Assign) {
        // TODO: type check assignment
    }

    fn visit_literal(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Literal) {
        let ty = match expr {
            toc_hir::expr::Literal::Int(_) => ty::Type::Int(ty::IntSize::Int),
            toc_hir::expr::Literal::Nat(_) => ty::Type::Nat(ty::NatSize::Nat),
            toc_hir::expr::Literal::Integer(_) => ty::Type::Integer,
            toc_hir::expr::Literal::Real(_) => ty::Type::Real(ty::RealSize::Real8),
            // TODO: Use sized char type for default string type
            toc_hir::expr::Literal::CharSeq(_) => ty::Type::String,
            toc_hir::expr::Literal::String(_) => ty::Type::String,
            toc_hir::expr::Literal::Boolean(_) => ty::Type::Boolean,
        };

        // Post the type of the literal to eval type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn visit_binary(&mut self, id: toc_hir::expr::ExprIdx, _expr: &toc_hir::expr::Binary) {
        // TODO: do proper binexpr typechecks
        let ty = ty::Type::Error;

        // Post binexpr type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn visit_unary(&mut self, id: toc_hir::expr::ExprIdx, _expr: &toc_hir::expr::Unary) {
        // TODO: do proper unexpr typechecks
        let ty = ty::Type::Error;

        // Post unexpr type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn visit_paren(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Paren) {
        // Same eval kind as the inner
        let eval_kind = self.eval_kinds[&expr.expr];
        self.eval_kinds.insert(id, eval_kind);
    }

    fn visit_name(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Name) {
        // If def-id, fetch type from def id map
        // If self, then fetch type from ???
        match expr {
            toc_hir::expr::Name::Name(use_id) => {
                let eval_ty = if let Some(ty_ref) = self.ty_ctx.get_def_id_kind(use_id.as_def()) {
                    ty_ref
                } else {
                    // Not declared
                    // TODO: Report this error
                    let err_ref = self.ty_ctx.add_type(ty::Type::Error);
                    let def_kind = DefKind::Error(err_ref);
                    self.ty_ctx.map_def_id(use_id.as_def(), def_kind);

                    def_kind
                };

                // Evalutes to a reference
                self.eval_kinds.insert(id, EvalKind::Ref(eval_ty));
            }
            toc_hir::expr::Name::Self_ => todo!(),
        }
    }

    fn visit_primitive(&mut self, id: hir_ty::TypeIdx, ty: &hir_ty::Primitive) {
        // Create the correct type based off of the base primitive type
        let ty = match ty {
            hir_ty::Primitive::Int => ty::Type::Int(ty::IntSize::Int),
            hir_ty::Primitive::Int1 => ty::Type::Int(ty::IntSize::Int1),
            hir_ty::Primitive::Int2 => ty::Type::Int(ty::IntSize::Int2),
            hir_ty::Primitive::Int4 => ty::Type::Int(ty::IntSize::Int4),
            hir_ty::Primitive::Nat => ty::Type::Nat(ty::NatSize::Nat),
            hir_ty::Primitive::Nat1 => ty::Type::Nat(ty::NatSize::Nat1),
            hir_ty::Primitive::Nat2 => ty::Type::Nat(ty::NatSize::Nat2),
            hir_ty::Primitive::Nat4 => ty::Type::Nat(ty::NatSize::Nat4),
            hir_ty::Primitive::Real => ty::Type::Real(ty::RealSize::Real),
            hir_ty::Primitive::Real4 => ty::Type::Real(ty::RealSize::Real4),
            hir_ty::Primitive::Real8 => ty::Type::Real(ty::RealSize::Real8),
            hir_ty::Primitive::Boolean => ty::Type::Boolean,
            hir_ty::Primitive::AddressInt => ty::Type::Nat(ty::NatSize::AddressInt),
            hir_ty::Primitive::Char => ty::Type::Char,
            hir_ty::Primitive::String => ty::Type::String,
            // TODO: Produce sized charseq types
            hir_ty::Primitive::SizedChar(_) => todo!(),
            hir_ty::Primitive::SizedString(_) => todo!(),
        };

        // Maps the type id to the current type node
        let ty_ref = self.ty_ctx.add_type(ty);
        self.ty_ctx.map_type(id, ty_ref);
    }
}

/// What an expression evaluates to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvalKind {
    Ref(DefKind),
    Value(TyRef),
}
