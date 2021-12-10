//! Code generation backend based on the HIR tree

use toc_analysis::db::HirAnalysis;
use toc_analysis::ty;
use toc_hir::symbol::DefId;
use toc_hir::{
    body as hir_body, expr as hir_expr, item as hir_item, library as hir_library, stmt as hir_stmt,
};
use toc_reporting::CompileResult;

use crate::instruction::Opcode;

mod instruction;

pub struct CodeBlob();

/// Generates code from the given HIR database,
/// or producing nothing if an error was encountered before code generation.
pub fn generate_code(db: &dyn HirAnalysis) -> CompileResult<Option<CodeBlob>> {
    // Bail if there are any errors from analysis
    let res = db.analyze_libraries();

    if res.messages().has_errors() {
        return res.map(|_| None);
    }

    // Start producing blobs for each library
    let lib_graph = db.library_graph();
    for (_, library_id) in lib_graph.library_roots() {
        let library = db.library(library_id);

        // Generate code for each statement body
        for body_id in library.body_ids() {
            let body = library.body(body_id);
            eprintln!("{:?}: {:?}", body_id, body);
        }

        for body_id in library.body_ids() {
            let body = library.body(body_id);

            match &body.kind {
                hir_body::BodyKind::Stmts(_, _) => {
                    eprintln!("gen body {:?}", body_id);
                    // For simple statements (e.g invariant, assign, constvar init)
                    // we can deal with them easily as they correspond to a linear
                    // sequence of instructions.
                    //
                    // For control flow statements, we need to keep track of where we should
                    // branch to, thus we can't fall back on simple HIR walking.
                    let body_code =
                        BodyCodeGenerator::generate_body(db, library_id, library.as_ref(), body_id);

                    println!("gen code:");
                    for opc in &body_code.0.opcodes {
                        println!("    {:?}", opc);
                    }
                }
                hir_body::BodyKind::Exprs(_expr) => {
                    // We don't start code generation from expr bodies
                    // (those are associated with `const` and `var`s)
                    continue;
                }
            }
        }
    }

    // TODO: toposort calls to module initialization bodies

    res.map(|_| None)
}

#[derive(Default)]
struct BodyCode(CodeFragment);

struct BodyCodeGenerator<'a> {
    db: &'a dyn HirAnalysis,
    library_id: hir_library::LibraryId,
    library: &'a hir_library::Library,
    body_id: hir_body::BodyId,
    body: &'a hir_body::Body,
    code_fragment: &'a mut CodeFragment,
}

impl BodyCodeGenerator<'_> {
    fn generate_body(
        db: &dyn HirAnalysis,
        library_id: hir_library::LibraryId,
        library: &hir_library::Library,
        body_id: hir_body::BodyId,
    ) -> BodyCode {
        let mut code_fragment = Default::default();
        let mut gen = BodyCodeGenerator {
            db,
            library_id,
            library,
            body_id,
            body: library.body(body_id),
            code_fragment: &mut code_fragment,
        };

        gen.inline_body(body_id);

        BodyCode(code_fragment)
    }

    fn inline_body(&mut self, body_id: hir_body::BodyId) {
        // hacky workarounds for actual hacks...
        let mut gen = BodyCodeGenerator {
            db: self.db,
            library_id: self.library_id,
            library: self.library,
            body_id,
            body: self.library.body(body_id),
            code_fragment: self.code_fragment,
        };

        match &self.library.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, _) => {
                for stmt_id in stmts {
                    gen.generate_stmt(*stmt_id);
                }
            }
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }
    }

    fn generate_stmt(&mut self, stmt_id: hir_stmt::StmtId) {
        match &self.body.stmt(stmt_id).kind {
            hir_stmt::StmtKind::Item(item_id) => self.generate_item(*item_id),
            hir_stmt::StmtKind::Assign(stmt) => self.generate_stmt_assign(stmt),
            hir_stmt::StmtKind::Put(_) => todo!(),
            hir_stmt::StmtKind::Get(_) => todo!(),
            hir_stmt::StmtKind::For(_) => todo!(),
            hir_stmt::StmtKind::Loop(_) => todo!(),
            hir_stmt::StmtKind::Exit(_) => todo!(),
            hir_stmt::StmtKind::If(_) => todo!(),
            hir_stmt::StmtKind::Case(_) => todo!(),
            hir_stmt::StmtKind::Block(_) => todo!(),
        }
    }

    fn generate_stmt_assign(&mut self, stmt: &hir_stmt::Assign) {
        // Steps
        // - Check if any type coercion needs to be done
        // - Generate each code for the initializing operand (dropping in type casts as necessary)
        // - Store the value in the referenced expression
        let lhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, stmt.lhs).into())
            .in_db(self.db)
            .peel_ref();

        let coerce_to = if matches!(lhs_ty.kind(), ty::TypeKind::Real(_)) {
            Some(CoerceTo::Real)
        } else {
            None
        };

        // Evaluation order is important, side effects from the rhs are visible when looking at lhs
        // For assignment, we don't want side effects from rhs eval to be visible during lookup
        self.generate_ref_expr(stmt.lhs);

        if let Some(coerce_to) = coerce_to {
            self.generate_coerced_expr(stmt.rhs, coerce_to)
        } else {
            self.generate_expr(stmt.rhs)
        };

        self.code_fragment.emit_assign(&lhs_ty);

        eprintln!("assigning reference (first operand) to value (second operand)");
    }

    fn generate_item(&mut self, item_id: hir_item::ItemId) {
        match &self.library.item(item_id).kind {
            hir_item::ItemKind::ConstVar(item) => self.generate_item_constvar(item),
            hir_item::ItemKind::Module(_) => {
                // We already generate code for module bodies as part of walking
                // over all of the bodies in a library
            }
        }
    }

    fn generate_item_constvar(&mut self, item: &hir_item::ConstVar) {
        // Steps
        // - Reserve space for the variable
        // - if an initializer body is present:
        //   - inline the initializer body (it's always an expr body)
        //   - assign the variable to the produced value
        // - otherwise
        //   - initialize the variable with the corresponding uninit pattern for the type
        eprintln!("reserving space for def {:?}", item.def_id);

        let def_ty = self
            .db
            .type_of(DefId(self.library_id, item.def_id).into())
            .in_db(self.db)
            .peel_ref();
        self.code_fragment.allocate_local(
            self.db,
            DefId(self.library_id, item.def_id),
            def_ty.id(),
        );

        if let Some(init_body) = item.init_expr {
            eprintln!("inlining init body {:?}", init_body);
            self.inline_body(init_body);

            eprintln!("assigning def {:?} to previously produced value", init_body);
            self.code_fragment
                .emit_locate_local(DefId(self.library_id, item.def_id));
            self.code_fragment.emit_assign_into_var(&def_ty);
        } else if has_uninit(&def_ty) {
            eprintln!(
                "assigning def {:?} to uninit pattern for type `{}`",
                item.def_id, def_ty
            );
            self.code_fragment
                .emit_locate_local(DefId(self.library_id, item.def_id));
            self.code_fragment.emit_assign_uninit(&def_ty);
        }
    }

    fn generate_coerced_expr(&mut self, expr_id: hir_expr::ExprId, coerce_to: CoerceTo) {
        let expr_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr_id).into())
            .in_db(self.db)
            .peel_ref();

        let coerce_op = match (coerce_to, expr_ty.kind()) {
            (CoerceTo::Real, ty::TypeKind::Nat(_)) => Some(Opcode::NATREAL()),
            (CoerceTo::Real, int) if int.is_integer() => Some(Opcode::INTREAL()),
            _ => None,
        };

        self.generate_expr(expr_id);

        if let Some(opcode) = coerce_op {
            eprintln!("coercing into {:?}", coerce_op);
            self.code_fragment.emit_opcode(opcode);
        }
    }

    fn generate_expr(&mut self, expr_id: hir_expr::ExprId) {
        match &self.body.expr(expr_id).kind {
            hir_expr::ExprKind::Missing => unreachable!("malformed code reached code generation"),
            hir_expr::ExprKind::Literal(expr) => self.generate_expr_literal(expr),
            hir_expr::ExprKind::Binary(expr) => self.generate_expr_binary(expr),
            hir_expr::ExprKind::Unary(expr) => self.generate_expr_unary(expr),
            hir_expr::ExprKind::Name(expr) => self.generate_expr_name(expr),
        }
    }

    fn generate_expr_literal(&mut self, expr: &hir_expr::Literal) {
        // Steps
        // - Push value onto the operand stack
        eprintln!("pushing value {:?} to operand stack", expr);
        match expr {
            hir_expr::Literal::Integer(value) => match value {
                0..=0xFF => self
                    .code_fragment
                    .emit_opcode(Opcode::PUSHINT1(*value as u8)),
                0..=0xFFFF => self
                    .code_fragment
                    .emit_opcode(Opcode::PUSHINT2(*value as u16)),
                0..=0xFFFFFFFF => self
                    .code_fragment
                    .emit_opcode(Opcode::PUSHINT(*value as u32)),
                _ => unreachable!("this backend does not support values larger than u32::MAX"),
            },
            hir_expr::Literal::Real(value) => {
                self.code_fragment.emit_opcode(Opcode::PUSHREAL(*value))
            }
            hir_expr::Literal::Char(value) => {
                if *value as u32 <= 0xFF {
                    self.code_fragment
                        .emit_opcode(Opcode::PUSHINT1(*value as u8))
                } else {
                    unreachable!("this backend does not support non-(extended) ascii code points")
                }
            }
            hir_expr::Literal::CharSeq(_) => todo!(),
            hir_expr::Literal::String(_) => todo!(),
            hir_expr::Literal::Boolean(value) => self.code_fragment.emit_opcode(
                value
                    .then(Opcode::PUSHVAL1)
                    .unwrap_or_else(Opcode::PUSHVAL0),
            ),
        }
    }

    fn generate_expr_binary(&mut self, expr: &hir_expr::Binary) {
        // Steps
        // - Check if any type coercion needs to be done
        // - Generate each code for the corresponding operands (dropping in type casts as necessary)
        // - Emit instruction corresponding to the operation

        let lhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr.lhs).into())
            .in_db(self.db)
            .peel_ref();
        let rhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr.rhs).into())
            .in_db(self.db)
            .peel_ref();

        let opcode = match expr.op.item() {
            hir_expr::BinaryOp::Add => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(),
                    rhs_ty.kind(),
                    Opcode::ADDREAL(),
                    Opcode::ADDNAT(),
                    Opcode::ADDNATINT(),
                    Opcode::ADDINTNAT(),
                    Opcode::ADDINT(),
                )
                .unwrap(),
            hir_expr::BinaryOp::Sub => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(),
                    rhs_ty.kind(),
                    Opcode::SUBREAL(),
                    Opcode::SUBNAT(),
                    Opcode::SUBNATINT(),
                    Opcode::SUBINTNAT(),
                    Opcode::SUBINT(),
                )
                .unwrap(),
            hir_expr::BinaryOp::Mul => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::MULREAL()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MULNAT()
                }
                (lhs, rhs) | (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MULINT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::Div => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::DIVREAL()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::DIVNAT()
                }
                (ty, _) | (_, ty) if ty.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::DIVINT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::RealDiv => {
                self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                Opcode::REALDIVIDE()
            }
            hir_expr::BinaryOp::Mod => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::MODREAL()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MODNAT()
                }
                (ty, _) | (_, ty) if ty.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MODINT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::Rem => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::REMREAL()
                }
                (ty::TypeKind::Int(_) | ty::TypeKind::Integer, _)
                | (_, ty::TypeKind::Int(_) | ty::TypeKind::Integer) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::REMINT()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MODNAT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::Exp => match (lhs_ty.kind(), rhs_ty.kind()) {
                (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::EXPINTINT()
                }
                (ty::TypeKind::Real(_), rhs) if rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::EXPREALINT()
                }
                (ty::TypeKind::Real(_), ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::EXPREALREAL()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::And => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::AND()
            }
            hir_expr::BinaryOp::Or => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::OR()
            }
            hir_expr::BinaryOp::Xor => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::XOR()
            }
            hir_expr::BinaryOp::Shl => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::SHL()
            }
            hir_expr::BinaryOp::Shr => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::SHR()
            }
            hir_expr::BinaryOp::Less => {
                let cmp_op = self
                    .dispatch_over_numbers(
                        expr,
                        lhs_ty.kind(),
                        rhs_ty.kind(),
                        Opcode::GEREAL(),
                        Opcode::GENAT(),
                        Opcode::GENATINT(),
                        Opcode::GEINTNAT(),
                        Opcode::GEINT(),
                    )
                    .unwrap();
                self.code_fragment.emit_opcode(cmp_op);
                Opcode::NOT()
            }
            hir_expr::BinaryOp::LessEq => {
                let cmp_op = self
                    .dispatch_over_numbers(
                        expr,
                        lhs_ty.kind(),
                        rhs_ty.kind(),
                        Opcode::LEREAL(),
                        Opcode::LENAT(),
                        Opcode::LENATINT(),
                        Opcode::LEINTNAT(),
                        Opcode::LEINT(),
                    )
                    .unwrap();
                cmp_op
            }
            hir_expr::BinaryOp::Greater => {
                let cmp_op = self
                    .dispatch_over_numbers(
                        expr,
                        lhs_ty.kind(),
                        rhs_ty.kind(),
                        Opcode::LEREAL(),
                        Opcode::LENAT(),
                        Opcode::LENATINT(),
                        Opcode::LEINTNAT(),
                        Opcode::LEINT(),
                    )
                    .unwrap();
                self.code_fragment.emit_opcode(cmp_op);
                Opcode::NOT()
            }
            hir_expr::BinaryOp::GreaterEq => {
                let cmp_op = self
                    .dispatch_over_numbers(
                        expr,
                        lhs_ty.kind(),
                        rhs_ty.kind(),
                        Opcode::GEREAL(),
                        Opcode::GENAT(),
                        Opcode::GENATINT(),
                        Opcode::GEINTNAT(),
                        Opcode::GEINT(),
                    )
                    .unwrap();
                cmp_op
            }
            hir_expr::BinaryOp::Equal => {
                let cmp_op = self
                    .dispatch_over_numbers(
                        expr,
                        lhs_ty.kind(),
                        rhs_ty.kind(),
                        Opcode::EQREAL(),
                        Opcode::EQNAT(),
                        Opcode::EQINTNAT(),
                        Opcode::EQINTNAT(),
                        Opcode::EQINT(),
                    )
                    .unwrap();
                cmp_op
            }
            hir_expr::BinaryOp::NotEqual => {
                let cmp_op = self
                    .dispatch_over_numbers(
                        expr,
                        lhs_ty.kind(),
                        rhs_ty.kind(),
                        Opcode::EQREAL(),
                        Opcode::EQNAT(),
                        Opcode::EQINTNAT(),
                        Opcode::EQINTNAT(),
                        Opcode::EQINT(),
                    )
                    .unwrap();
                self.code_fragment.emit_opcode(cmp_op);
                Opcode::NOT()
            }
            hir_expr::BinaryOp::In => todo!(),
            hir_expr::BinaryOp::NotIn => todo!(),
            hir_expr::BinaryOp::Imply => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.code_fragment.emit_opcode(Opcode::NOT());
                self.generate_expr(expr.rhs);
                Opcode::OR()
            }
        };

        eprintln!(
            "applying operation {:?} to previous two operand",
            expr.op.item()
        );

        self.code_fragment.emit_opcode(opcode);
    }

    #[allow(clippy::too_many_arguments)]
    fn dispatch_over_numbers(
        &mut self,
        expr: &hir_expr::Binary,
        lhs: &ty::TypeKind,
        rhs: &ty::TypeKind,
        real: Opcode,
        nat_nat: Opcode,
        nat_int: Opcode,
        int_nat: Opcode,
        int_int: Opcode,
    ) -> Option<Opcode> {
        match (lhs, rhs) {
            (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                Some(real)
            }
            (ty::TypeKind::Nat(_), ty::TypeKind::Nat(_)) => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Some(nat_nat)
            }
            (ty::TypeKind::Nat(_), other) if other.is_integer() => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Some(nat_int)
            }
            (other, ty::TypeKind::Nat(_)) if other.is_integer() => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Some(int_nat)
            }
            (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Some(int_int)
            }
            _ => None,
        }
    }

    fn generate_expr_unary(&mut self, expr: &hir_expr::Unary) {
        // Steps
        // - Generate each code for the corresponding operands
        // - Emit instruction corresponding to the operation

        eprintln!(
            "applying operation {:?} to previous operand",
            expr.op.item()
        );

        let rhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr.rhs).into())
            .in_db(self.db)
            .peel_ref();

        self.generate_expr(expr.rhs);

        match expr.op.item() {
            hir_expr::UnaryOp::Identity => {} // no-op
            hir_expr::UnaryOp::Not => {
                if rhs_ty.kind().is_integer() {
                    // Composite
                    self.code_fragment.emit_opcode(Opcode::PUSHINT(!0));
                    self.code_fragment.emit_opcode(Opcode::XOR());
                } else {
                    self.code_fragment.emit_opcode(Opcode::NOT())
                }
            }
            hir_expr::UnaryOp::Negate => match rhs_ty.kind() {
                ty::TypeKind::Int(_) | ty::TypeKind::Nat(_) => {
                    // should already be dealing with promoted types
                    self.code_fragment.emit_opcode(Opcode::NEGINT())
                }
                ty::TypeKind::Real(_) => self.code_fragment.emit_opcode(Opcode::NEGREAL()),
                _ => unreachable!(),
            },
        }
    }

    fn generate_expr_name(&mut self, expr: &hir_expr::Name) {
        // Steps
        // - Load value from the referenced def (may need to perform canonical name resolution)
        match expr {
            hir_expr::Name::Name(def_id) => {
                let info = self.library.local_def(*def_id);
                eprintln!("loading value from def {:?} ({:?})", info, def_id);

                let def_ty = self
                    .db
                    .type_of(DefId(self.library_id, *def_id).into())
                    .in_db(self.db)
                    .peel_ref();

                self.code_fragment
                    .emit_locate_local(DefId(self.library_id, *def_id));
                self.code_fragment.emit_fetch_value(&def_ty);
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }

    /// Like `generate_expr`, but for producing references to locations
    fn generate_ref_expr(&mut self, expr_id: hir_expr::ExprId) {
        match &self.body.expr(expr_id).kind {
            hir_expr::ExprKind::Name(ref_expr) => self.generate_ref_expr_name(ref_expr),
            // These can never be in reference producing position
            hir_expr::ExprKind::Missing
            | hir_expr::ExprKind::Literal(_)
            | hir_expr::ExprKind::Binary(_)
            | hir_expr::ExprKind::Unary(_) => {
                unreachable!("malformed code reached code generation")
            }
        }
    }

    fn generate_ref_expr_name(&mut self, expr: &hir_expr::Name) {
        // Steps
        // - Produce pointer to referenced def (may need to perform canonical name resolution)
        //   - If it's a stack var, it should be to the locals space
        //   - If it's a global var, it should be from a relocatable patch to the globals space
        match expr {
            hir_expr::Name::Name(def_id) => {
                let info = self.library.local_def(*def_id);
                eprintln!("locating value from def {:?} ({:?})", info, def_id);

                self.code_fragment
                    .emit_locate_local(DefId(self.library_id, *def_id))
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }
}

enum CoerceTo {
    Real,
}

#[derive(Clone, Copy)]
struct StackSlot {
    offset: u32,
    _size: u32,
}

#[derive(Default)]
struct CodeFragment {
    locals: indexmap::IndexMap<DefId, StackSlot>,
    locals_size: u32,

    opcodes: Vec<Opcode>,
}

impl CodeFragment {
    fn allocate_local(&mut self, db: &dyn HirAnalysis, def_id: DefId, def_ty: ty::TypeId) {
        // Align to the nearest stack slot
        let size = align_up_to(size_of_ty(db, def_ty), 4).try_into().unwrap();
        let offset = self.locals_size;

        self.locals.insert(
            def_id,
            StackSlot {
                offset,
                _size: size,
            },
        );
        self.locals_size += size;
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        self.opcodes.push(opcode)
    }

    fn emit_locate_local(&mut self, def_id: DefId) {
        let slot_info = self.locals.get(&def_id).expect("def not reserved yet");
        let offset = slot_info.offset;

        self.emit_opcode(Opcode::LOCATELOCAL(offset));
    }

    fn emit_assign_into_var(&mut self, into_tyref: &ty::TyRef<dyn HirAnalysis>) {
        let opcode = match into_tyref.kind() {
            ty::TypeKind::Boolean => Opcode::ASNINT1INV(),
            ty::TypeKind::Int(ty::IntSize::Int1) => Opcode::ASNINT1INV(),
            ty::TypeKind::Int(ty::IntSize::Int2) => Opcode::ASNINT2INV(),
            ty::TypeKind::Int(ty::IntSize::Int4) => Opcode::ASNINT4INV(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::ASNINTINV(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => Opcode::ASNNAT1INV(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => Opcode::ASNNAT2INV(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => Opcode::ASNNAT4INV(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::ASNNATINV(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::ASNADDRINV(),
            ty::TypeKind::Real(ty::RealSize::Real4) => Opcode::ASNREAL4INV(),
            ty::TypeKind::Real(ty::RealSize::Real8) => Opcode::ASNREAL8INV(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::ASNREALINV(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => todo!(),
            ty::TypeKind::String => todo!(),
            ty::TypeKind::CharN(_) => todo!(),
            ty::TypeKind::StringN(_) => todo!(),
            ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    fn emit_assign(&mut self, into_tyref: &ty::TyRef<dyn HirAnalysis>) {
        let opcode = match into_tyref.kind() {
            ty::TypeKind::Boolean => Opcode::ASNINT1(),
            ty::TypeKind::Int(ty::IntSize::Int1) => Opcode::ASNINT1(),
            ty::TypeKind::Int(ty::IntSize::Int2) => Opcode::ASNINT2(),
            ty::TypeKind::Int(ty::IntSize::Int4) => Opcode::ASNINT4(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::ASNINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => Opcode::ASNNAT1(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => Opcode::ASNNAT2(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => Opcode::ASNNAT4(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::ASNNAT(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::ASNADDR(),
            ty::TypeKind::Real(ty::RealSize::Real4) => Opcode::ASNREAL4(),
            ty::TypeKind::Real(ty::RealSize::Real8) => Opcode::ASNREAL8(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::ASNREAL(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => todo!(),
            ty::TypeKind::String => todo!(),
            ty::TypeKind::CharN(_) => todo!(),
            ty::TypeKind::StringN(_) => todo!(),
            ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    // Expects a destination address to be present
    fn emit_assign_uninit(&mut self, uninit_ty: &ty::TyRef<dyn HirAnalysis>) {
        let opcode = match uninit_ty.kind() {
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::UNINITADDR(),
            ty::TypeKind::Boolean => Opcode::UNINITBOOLEAN(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::UNINITINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::UNINITNAT(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::UNINITREAL(),
            ty::TypeKind::String => Opcode::UNINITSTR(),
            _ => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    fn emit_fetch_value(&mut self, ty_ref: &ty::TyRef<dyn HirAnalysis>) {
        let opcode = match ty_ref.kind() {
            ty::TypeKind::Boolean => Opcode::FETCHBOOL(),
            ty::TypeKind::Int(ty::IntSize::Int1) => Opcode::FETCHINT1(),
            ty::TypeKind::Int(ty::IntSize::Int2) => Opcode::FETCHINT2(),
            ty::TypeKind::Int(ty::IntSize::Int4) => Opcode::FETCHINT4(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::FETCHINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => Opcode::FETCHNAT1(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => Opcode::FETCHNAT2(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => Opcode::FETCHNAT4(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::FETCHNAT(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::FETCHADDR(),
            ty::TypeKind::Real(ty::RealSize::Real4) => Opcode::FETCHREAL4(),
            ty::TypeKind::Real(ty::RealSize::Real8) => Opcode::FETCHREAL8(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::FETCHREAL(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => todo!(),
            ty::TypeKind::String => todo!(),
            ty::TypeKind::CharN(_) => todo!(),
            ty::TypeKind::StringN(_) => todo!(),
            ty::TypeKind::Error | ty::TypeKind::Ref(_, _) => unreachable!(),
        };

        self.emit_opcode(opcode);
    }
}

fn size_of_ty(db: &dyn HirAnalysis, ty: ty::TypeId) -> usize {
    let ty_ref = ty.in_db(db);

    match ty_ref.kind() {
        ty::TypeKind::Boolean => 1,
        ty::TypeKind::Int(ty::IntSize::Int1) => 1,
        ty::TypeKind::Int(ty::IntSize::Int2) => 2,
        ty::TypeKind::Int(ty::IntSize::Int4) => 4,
        ty::TypeKind::Int(ty::IntSize::Int) => 4,
        ty::TypeKind::Nat(ty::NatSize::Nat1) => 1,
        ty::TypeKind::Nat(ty::NatSize::Nat2) => 2,
        ty::TypeKind::Nat(ty::NatSize::Nat4) => 4,
        ty::TypeKind::Nat(ty::NatSize::Nat) => 4,
        ty::TypeKind::Nat(ty::NatSize::AddressInt) => 4,
        ty::TypeKind::Real(ty::RealSize::Real4) => 4,
        ty::TypeKind::Real(ty::RealSize::Real8) => 8,
        ty::TypeKind::Real(ty::RealSize::Real) => 8,
        ty::TypeKind::Integer => unreachable!("type should be concrete"),
        ty::TypeKind::Char => 1,
        ty::TypeKind::String => 255,
        ty::TypeKind::CharN(_seq_size) => todo!(),
        ty::TypeKind::StringN(_seq_size) => todo!(),
        ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
    }
}

fn has_uninit(ty_ref: &ty::TyRef<dyn HirAnalysis>) -> bool {
    matches!(
        ty_ref.kind(),
        ty::TypeKind::Boolean
            | ty::TypeKind::Int(ty::IntSize::Int)
            | ty::TypeKind::Nat(ty::NatSize::AddressInt | ty::NatSize::Nat)
            | ty::TypeKind::Real(ty::RealSize::Real)
            | ty::TypeKind::String
    )
}

fn align_up_to(value: usize, align: usize) -> usize {
    assert!(align.is_power_of_two());
    let mask = align - 1;

    (value + mask) & !mask
}
