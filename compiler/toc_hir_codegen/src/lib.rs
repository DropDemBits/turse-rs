//! Code generation backend based on the HIR tree

use toc_analysis::db::HirAnalysis;
use toc_hir::symbol::DefId;
use toc_hir::{
    body as hir_body, expr as hir_expr, item as hir_item, library as hir_library, stmt as hir_stmt,
};
use toc_reporting::CompileResult;

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

            match &body.kind {
                hir_body::BodyKind::Stmts(_, _) => {
                    // For simple statements (e.g invariant, assign, constvar init)
                    // we can deal with them easily as they correspond to a linear
                    // sequence of instructions.
                    //
                    // For control flow statements, we need to keep track of where we should
                    // branch to, thus we can't fall back on simple HIR walking.
                    let _body_code =
                        BodyCodeGenerator::generate_body(db, library_id, library.as_ref(), body_id);
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

struct BodyCode {}

struct BodyCodeGenerator<'a> {
    db: &'a dyn HirAnalysis,
    library_id: hir_library::LibraryId,
    library: &'a hir_library::Library,
    _body_id: hir_body::BodyId,
    body: &'a hir_body::Body,
}

impl BodyCodeGenerator<'_> {
    fn generate_body(
        db: &dyn HirAnalysis,
        library_id: hir_library::LibraryId,
        library: &hir_library::Library,
        body_id: hir_body::BodyId,
    ) -> BodyCode {
        let mut gen = BodyCodeGenerator {
            db,
            library_id,
            library,
            _body_id: body_id,
            body: library.body(body_id),
        };

        gen.inline_body(body_id);

        todo!()
    }

    fn inline_body(&mut self, body_id: hir_body::BodyId) {
        match &self.library.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, _) => {
                for stmt_id in stmts {
                    self.generate_stmt(*stmt_id);
                }
            }
            hir_body::BodyKind::Exprs(expr) => self.generate_expr(*expr),
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

        // Evaluation order is important, side effects from the rhs are visible when looking at lhs
        // ???: Do we want side effects from rhs eval to be visible in lhs lookup?
        self.generate_expr(stmt.rhs);
        self.generate_ref_expr(stmt.lhs);

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

        if let Some(init_body) = item.init_expr {
            eprintln!("inlining init body {:?}", init_body);
            self.inline_body(init_body);
            eprintln!("assigning def {:?} to previously produced value", init_body);
        } else {
            eprintln!(
                "assigning def {:?} to uninit pattern for type `{}`",
                item.def_id,
                self.db
                    .type_of(DefId(self.library_id, item.def_id).into())
                    .in_db(self.db)
                    .peel_ref()
            );
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
    }

    fn generate_expr_binary(&mut self, expr: &hir_expr::Binary) {
        // Steps
        // - Check if any type coercion needs to be done
        // - Generate each code for the corresponding operands (dropping in type casts as necessary)
        // - Emit instruction corresponding to the operation

        self.generate_expr(expr.lhs);
        self.generate_expr(expr.rhs);

        eprintln!(
            "applying operation {:?} to previous two operand",
            expr.op.item()
        );
    }

    fn generate_expr_unary(&mut self, expr: &hir_expr::Unary) {
        // Steps
        // - Generate each code for the corresponding operands
        // - Emit instruction corresponding to the operation

        self.generate_expr(expr.rhs);

        eprintln!(
            "applying operation {:?} to previous operand",
            expr.op.item()
        );
    }

    fn generate_expr_name(&mut self, expr: &hir_expr::Name) {
        // Steps
        // - Load value from the referenced def (may need to perform canonical name resolution)
        match expr {
            hir_expr::Name::Name(def_id) => eprintln!("loading value from def {:?}", def_id),
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
            hir_expr::Name::Name(def_id) => eprintln!("loading value from def {:?}", def_id),
            hir_expr::Name::Self_ => todo!(),
        }
    }
}
