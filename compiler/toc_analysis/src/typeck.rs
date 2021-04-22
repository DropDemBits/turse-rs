//! Type checking
#[cfg(test)]
mod test;

use std::collections::HashMap;

use toc_hir::{expr, stmt, ty as hir_ty};
use toc_reporting::{MessageKind, ReportMessage};
use toc_span::Spanned;

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

pub fn typecheck_unit(unit: &toc_hir::Unit) -> (TyCtx, Vec<ReportMessage>) {
    TypeCheck::check_unit(unit)
}

struct TypeCheck<'a> {
    unit: &'a toc_hir::Unit,
    ty_ctx: TyCtx,
    eval_kinds: HashMap<expr::ExprIdx, EvalKind>,
    reporter: toc_reporting::MessageSink,
}

impl<'a> TypeCheck<'a> {
    fn check_unit(unit: &'a toc_hir::Unit) -> (TyCtx, Vec<ReportMessage>) {
        let mut typeck = Self {
            unit,
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

    fn require_expr_ty(&mut self, eval_kind: EvalKind) -> TyRef {
        if let Some(ty) = eval_kind.as_expr_ty() {
            ty
        } else {
            // TODO: Report this error once type decls are lowered
            self.ty_ctx.add_type(ty::Type::Error)
        }
    }
}

/// ???: Mapping concrete TypeId's back to ty::TypeIdx?
/// Pass in the ty::TypeIdx, which is mapped by the TyCtx
impl toc_hir::HirVisitor for TypeCheck<'_> {
    fn visit_unit(&mut self, _unit: &toc_hir::Unit) {}

    fn visit_constvar(&mut self, _id: stmt::StmtIdx, decl: &stmt::ConstVar) {
        let ty_ref = match &decl.tail {
            stmt::ConstVarTail::Both(ty_spec, _) | stmt::ConstVarTail::TypeSpec(ty_spec) => {
                // From type_spec
                self.ty_ctx.get_type(*ty_spec).unwrap()
            }
            stmt::ConstVarTail::InitExpr(expr) => {
                // From inferred init expr
                let eval_kind = *self.eval_kinds.get(expr).unwrap();
                self.require_expr_ty(eval_kind)
            }
        };

        // Make the type concrete
        let ty_ref = if *ty_ref == ty::Type::Integer {
            // Integer decomposes into a normal `int`
            self.ty_ctx.add_type(ty::Type::Int(ty::IntSize::Int))
        } else {
            ty_ref
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
            toc_hir::expr::Literal::Real(_) => ty::Type::Real(ty::RealSize::Real),
            // TODO: Use sized char type for default string type
            toc_hir::expr::Literal::CharSeq(_) => ty::Type::String,
            toc_hir::expr::Literal::String(_) => ty::Type::String,
            toc_hir::expr::Literal::Boolean(_) => ty::Type::Boolean,
        };

        // Post the type of the literal to eval type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn visit_binary(&mut self, id: toc_hir::expr::ExprIdx, expr: &toc_hir::expr::Binary) {
        // TODO: do full binexpr typechecks
        let ty = self.type_check_binary_op(expr.lhs, expr.op, expr.rhs);

        // Post binexpr type
        let ty = self.ty_ctx.add_type(ty);
        self.eval_kinds.insert(id, EvalKind::Value(ty));
    }

    fn visit_unary(&mut self, id: toc_hir::expr::ExprIdx, _expr: &toc_hir::expr::Unary) {
        // TODO: do full unexpr typechecks
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
        let (use_id, ty_ref) = match expr {
            toc_hir::expr::Name::Name(use_id) => {
                (use_id, self.ty_ctx.get_def_id_kind(use_id.as_def()))
            }
            toc_hir::expr::Name::Self_ => todo!(),
        };

        let eval_ty = if let Some(ty_ref) = ty_ref {
            ty_ref
        } else {
            // Nab symbol info
            let sym_name = &self.unit.symbol_table.get_symbol(use_id.as_def()).name;
            let expr_range = self.unit.database.expr_nodes.spans[&id];

            // Not declared, no type provided by any decls
            self.reporter.report(
                MessageKind::Error,
                &format!("`{}` is not declared", sym_name),
                expr_range,
            );

            // Build error type
            let err_ref = self.ty_ctx.add_type(ty::Type::Error);
            let def_kind = DefKind::Error(err_ref);
            self.ty_ctx.map_def_id(use_id.as_def(), def_kind);

            def_kind
        };

        // Evalutes to a reference
        self.eval_kinds.insert(id, EvalKind::Ref(eval_ty));
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

impl TypeCheck<'_> {
    fn type_check_binary_op(
        &mut self,
        lhs_id: expr::ExprIdx,
        op: Spanned<expr::BinaryOp>,
        rhs_id: expr::ExprIdx,
    ) -> ty::Type {
        // Type classes:
        // `charseq`: string, char, string(n), char(n)
        // `number`: real, int, nat
        // `integer`: int, nat

        // lhs - rhs
        // (int/nat) - real => real
        // real - (int/nat) => real
        // int - (int/nat) => int
        // (int/nat) - int => int
        // nat - nat => nat
        use ty::{IntSize, NatSize, RealSize, Type};

        fn is_number(ty: &Type) -> bool {
            matches!(
                ty,
                Type::Integer | Type::Real(_) | Type::Int(_) | Type::Nat(_)
            )
        }

        fn is_integer(ty: &Type) -> bool {
            matches!(ty, Type::Integer | Type::Int(_) | Type::Nat(_))
        }

        fn is_nat(ty: &Type) -> bool {
            matches!(ty, Type::Integer | Type::Nat(_))
        }

        fn is_error(ty: &Type) -> bool {
            matches!(ty, Type::Error)
        }

        fn check_number_operands(lhs_ty: &Type, rhs_ty: &Type) -> Option<Type> {
            match (lhs_ty, rhs_ty) {
                // Pass through integer inferrence
                (Type::Integer, Type::Integer) => Some(Type::Integer),

                // Normal operands
                (operand, Type::Real(_)) | (Type::Real(_), operand) if is_number(operand) => {
                    Some(Type::Real(RealSize::Real))
                }
                (operand, Type::Int(_)) | (Type::Int(_), operand) if is_integer(operand) => {
                    Some(Type::Int(IntSize::Int))
                }
                (operand, Type::Nat(_)) | (Type::Nat(_), operand) if is_nat(operand) => {
                    Some(Type::Nat(NatSize::Nat))
                }
                _ => None,
            }
        }

        let (_lhs, _lhs_eval, lhs_ty) = (
            &self.unit.database[lhs_id],
            self.eval_kinds[&lhs_id],
            self.require_expr_ty(self.eval_kinds[&lhs_id]),
        );
        let (_rhs, _rhs_eval, rhs_ty) = (
            &self.unit.database[rhs_id],
            self.eval_kinds[&rhs_id],
            self.require_expr_ty(self.eval_kinds[&rhs_id]),
        );

        // Short circuit for error types
        // Don't duplicate errors
        if is_error(&lhs_ty) || is_error(&rhs_ty) {
            return Type::Error;
        }

        match op.item() {
            expr::BinaryOp::Add => {
                // Operations:
                // x String concatenation (charseq, charseq => charseq)
                // x Set union (set, set => set)
                // - Addition (number, number => number)

                if let Some(result_ty) = check_number_operands(&lhs_ty, &rhs_ty) {
                    // Addition
                    result_ty
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for addition",
                            op.span(),
                        )
                        .with_info("operands must both be numbers, strings, or sets", None)
                        .finish();
                    Type::Error
                }
            }
            expr::BinaryOp::Sub => {
                // Operations:
                // x Set difference (set, set => set)
                // - Subtraction (number, number => number)

                if let Some(result_ty) = check_number_operands(&lhs_ty, &rhs_ty) {
                    // Subtraction
                    result_ty
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for subtraction",
                            op.span(),
                        )
                        .with_info("operands must both be numbers or sets", None)
                        .finish();
                    Type::Error
                }
            }
            expr::BinaryOp::Mul => {
                // Operations:
                // x Set intersection (set, set => set)
                // - Multiplication (number, number => number)

                if let Some(result_ty) = check_number_operands(&lhs_ty, &rhs_ty) {
                    // Multiplication
                    result_ty
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for multiplication",
                            op.span(),
                        )
                        .with_info("operands must both be numbers or sets", None)
                        .finish();
                    Type::Error
                }
            }
            expr::BinaryOp::Div => {
                // Operations:
                // - Integer division (number, number => integer)

                match (&*lhs_ty, &*rhs_ty) {
                    // Pass through type inferrence
                    (Type::Integer, Type::Integer) => Type::Integer,
                    (operand, Type::Nat(_)) | (Type::Nat(_), operand) if is_nat(operand) => {
                        Type::Nat(NatSize::Nat)
                    }
                    (lhs, rhs) if is_number(lhs) && is_number(rhs) => Type::Int(IntSize::Int),
                    _ => {
                        // Type error
                        self.reporter
                            .report_detailed(
                                MessageKind::Error,
                                "incompatible types for integer division",
                                op.span(),
                            )
                            .with_info("operands must both be numbers", None)
                            .finish();
                        Type::Error
                    }
                }
            }
            expr::BinaryOp::RealDiv => {
                // Operations:
                // - Floating point division (number, number => real)

                if is_number(&lhs_ty) && is_number(&rhs_ty) {
                    Type::Real(RealSize::Real)
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for division",
                            op.span(),
                        )
                        .with_info("operands must both be numbers", None)
                        .finish();
                    Type::Error
                }
            }
            expr::BinaryOp::Mod => {
                // Operations:
                // - Modulo (number, number => number)

                if let Some(result_ty) = check_number_operands(&lhs_ty, &rhs_ty) {
                    // Modulo
                    result_ty
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for modulo",
                            op.span(),
                        )
                        .with_info("operands must both be numbers", op.span())
                        .finish();
                    Type::Error
                }
            }
            expr::BinaryOp::Rem => {
                // Operations:
                // - Remainder (number, number => number)

                if let Some(result_ty) = check_number_operands(&lhs_ty, &rhs_ty) {
                    // Remainder
                    result_ty
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for remainder",
                            op.span(),
                        )
                        .with_info("operands must both be numbers", None)
                        .finish();
                    Type::Error
                }
            }
            expr::BinaryOp::Exp => {
                // Operations:
                // - Exponation (number, number => number)

                if let Some(result_ty) = check_number_operands(&lhs_ty, &rhs_ty) {
                    // Exponentiation
                    result_ty
                } else {
                    // Type error
                    self.reporter
                        .report_detailed(
                            MessageKind::Error,
                            "incompatible types for exponentiation",
                            op.span(),
                        )
                        .with_info("operands must both be numbers", None)
                        .finish();
                    Type::Error
                }
            }
            // Bitwise operators (integer, integer => nat)
            // + Logical operators (boolean, boolean => boolean)
            expr::BinaryOp::And => todo!(),
            expr::BinaryOp::Or => todo!(),
            expr::BinaryOp::Xor => todo!(),
            // Pure bitwise operators
            expr::BinaryOp::Shl => todo!(),
            expr::BinaryOp::Shr => todo!(),
            // Pure logical operator
            expr::BinaryOp::Imply => todo!(),
            // Comparison (a, b => boolean where a, b: Comparable)
            expr::BinaryOp::Less => todo!(),
            expr::BinaryOp::LessEq => todo!(),
            expr::BinaryOp::Greater => todo!(),
            expr::BinaryOp::GreaterEq => todo!(),
            expr::BinaryOp::Equal => todo!(),
            expr::BinaryOp::NotEqual => todo!(),
            // Set membership tests (set(a), a => boolean)
            expr::BinaryOp::In => todo!(),
            expr::BinaryOp::NotIn => todo!(),
            // Field access (a, b => c)
            expr::BinaryOp::Arrow => todo!(),
            expr::BinaryOp::Dot => todo!(),
            // Call expression (f (a, ...) => r)
            expr::BinaryOp::Call => todo!(),
        }
    }
}

/// What an expression evaluates to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EvalKind {
    Ref(DefKind),
    Value(TyRef),
}

impl EvalKind {
    fn as_expr_ty(self) -> Option<TyRef> {
        match self {
            EvalKind::Ref(DefKind::Const(ty))
            | EvalKind::Ref(DefKind::Var(ty))
            | EvalKind::Ref(DefKind::Error(ty))
            | EvalKind::Value(ty) => Some(ty),
            EvalKind::Ref(DefKind::Type(_)) => None,
        }
    }
}
