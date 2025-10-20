//! Type inference for bodies and items
//!
//! This defines how the types and constraints for bodies are made.

use std::collections::HashMap;

use toc_hir_def::{body, expr, local, scope, stmt};

use crate::{
    Db,
    ty::{FlexTy, FlexVar, Ty, TyKind, make},
};

#[salsa_macros::tracked]
pub struct BodyInfer<'db> {
    #[tracked]
    #[returns(ref)]
    pub(crate) expr_tys: HashMap<expr::LocalExpr<'db>, Ty<'db>>,

    #[tracked]
    #[returns(ref)]
    pub(crate) local_tys: HashMap<local::LocalId<'db>, Ty<'db>>,
}

#[derive(Debug, Default)]
struct InferEnv<'db> {
    flex_vars: ena::unify::InPlaceUnificationTable<FlexVar<'db>>,
    delayed_expr_tys: HashMap<expr::LocalExpr<'db>, FlexVar<'db>>,
    delayed_local_tys: HashMap<local::LocalId<'db>, FlexVar<'db>>,
}

impl<'db> InferEnv<'db> {
    pub fn fresh_var(&mut self) -> FlexVar<'db> {
        self.flex_vars.new_key(FlexTy::Unknown)
    }

    pub fn concrete_var(&mut self, ty: Ty<'db>) -> FlexVar<'db> {
        self.flex_vars.new_key(FlexTy::Concrete(ty))
    }

    pub fn flex_ty(&mut self, flex_ty: FlexTy<'db>) -> FlexVar<'db> {
        self.flex_vars.new_key(flex_ty)
    }
}

/// Infers the types of a [`Body`].
///
/// [`Body`]: body::Body
#[salsa::tracked]
pub fn infer_body<'db>(db: &'db dyn Db, body: body::Body<'db>) -> BodyInfer<'db> {
    // We assume that the body is well-formed enough, i.e. we are resilient in the face of body lowering errors.
    let contents = body.contents(db);
    let mut cctx = ExprStoreConstraints::new(contents);

    for stmt in body.top_level_stmts(db) {
        cctx.check_stmt(db, stmt.stmt());
    }

    let expr_tys = cctx
        .env
        .delayed_expr_tys
        .into_keys()
        .map(|expr| (expr, make::mk_error(db)))
        .collect();

    let local_tys = cctx
        .env
        .delayed_local_tys
        .into_keys()
        .map(|local| (local, make::mk_error(db)))
        .collect();

    BodyInfer::new(db, expr_tys, local_tys)
}

/// A constraint against a set of flexible type variables.
///
/// Constraints direct what each flexible type variable should infer to.
#[derive(Debug)]
pub enum Constraint<'db> {
    /// Constrains `'a` to be a subtype of `'b` (`'a <: 'b`).
    Subtype(FlexVar<'db>, FlexVar<'db>),
    /// Constrains `'a` to unify with `'b` (`'a = 'b`).
    Unify(FlexVar<'db>, FlexVar<'db>),
    /// Infers `'a` from a resolution.
    ResolutionOf(scope::QueryKey<'db>, FlexVar<'db>),
    /// `'a` must be a type that implements a [`BuiltinInterface`].
    ImplsBuiltin(BuiltinInterface<'db>, FlexVar<'db>),
}

/// Built-in interfaces that the language implements for certain types.
#[derive(Debug)]
pub enum BuiltinInterface<'db> {
    /// Type supports the `get 'a` operation.
    GetToken,
    /// Type supports the `get 'a : *` operation.
    GetLine,
    /// Type supports the `get 'a : expr` operation.
    GetChars { width: FlexVar<'db> },
    /// Type supports the `put 'a` and `put 'a : expr` operations.
    Put,
    /// Type supports the `put 'a : expr : precision` operation.
    PutWithPrecision { precision: FlexVar<'db> },
    /// Type supports the `put 'a : expr : precision : exponent` operation.
    PutWithExponent {
        precision: FlexVar<'db>,
        exponent: FlexVar<'db>,
    },
    /// Type supports the `read 'a` operation.
    Read,
    /// Type supports the `write 'a` operation.
    Write,
    /// Type supports the `ord('a)` operation.
    Ordinal,
}

#[derive(Debug)]
struct ExprStoreConstraints<'db> {
    store: &'db body::BodyContents<'db>,
    env: InferEnv<'db>,
    constraints: Vec<Constraint<'db>>,
}

impl<'db> ExprStoreConstraints<'db> {
    fn new(store: &'db body::BodyContents<'db>) -> Self {
        Self {
            store,
            env: Default::default(),
            constraints: vec![],
        }
    }

    fn check_stmt(&mut self, db: &'db dyn Db, stmt: stmt::LocalStmt<'db>) {
        match self.store.stmt(stmt) {
            stmt::Stmt::InitializeConstVar(_, _) => todo!(),
            stmt::Stmt::InitializeBindItem(_, _) => todo!(),
            stmt::Stmt::LocalConstVar(local_const_var) => {
                // FIXME: Handle inferring from tycons
                // - invoke tycons wf

                let expr_ty = match local_const_var.initializer {
                    Some(expr) => self.infer_expr(db, expr),
                    None => self.env.concrete_var(make::mk_error(db)),
                };
                // Local constvars always correspond to a place of some type.
                // For now, assume the type is inferred from the initializer.
                let local_ty = self.env.flex_ty(FlexTy::Ty(Box::new(TyKind::Place(
                    Box::new(TyKind::FlexVar(expr_ty)),
                    local_const_var.mutability,
                ))));

                self.env
                    .delayed_local_tys
                    .insert(local_const_var.local, local_ty);
            }
            stmt::Stmt::Assign(_) => todo!(),
            stmt::Stmt::Put(put) => {
                if let Some(stream_num) = put.stream_num {
                    self.check_expr(db, stream_num, make::mk_int(db).into());
                }

                for put_item in &put.items {
                    let stmt::Skippable::Item(put_item) = put_item else {
                        continue;
                    };

                    let item_ty = self.infer_expr(db, put_item.expr);

                    // All put variants allow a width specifier.
                    if let Some(width_opt) = put_item.opts.width() {
                        self.check_expr(db, width_opt, make::mk_int(db).into());
                    }

                    match put_item.opts {
                        stmt::PutOpts::None | stmt::PutOpts::WithWidth { width: _ } => {
                            self.constraints
                                .push(Constraint::ImplsBuiltin(BuiltinInterface::Put, item_ty));
                        }
                        stmt::PutOpts::WithPrecision {
                            width: _,
                            precision,
                        } => {
                            let precision_ty = self.infer_expr(db, precision);

                            self.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::PutWithPrecision {
                                    precision: precision_ty,
                                },
                                item_ty,
                            ));
                        }
                        stmt::PutOpts::WithExponentWidth {
                            width: _,
                            precision,
                            exponent_width,
                        } => {
                            let precision_ty = self.infer_expr(db, precision);
                            let exponent_ty = self.infer_expr(db, exponent_width);

                            self.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::PutWithExponent {
                                    precision: precision_ty,
                                    exponent: exponent_ty,
                                },
                                item_ty,
                            ));
                        }
                    }
                }
            }
            stmt::Stmt::Get(get) => {
                if let Some(stream_num) = get.stream_num {
                    self.check_expr(db, stream_num, make::mk_int(db).into());
                }

                for get_item in &get.items {
                    let stmt::Skippable::Item(get_item) = get_item else {
                        continue;
                    };

                    let item_ty = self.infer_expr(db, get_item.expr);

                    match get_item.width {
                        stmt::GetWidth::Token => {
                            self.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::GetToken,
                                item_ty,
                            ));
                        }
                        stmt::GetWidth::Line => {
                            self.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::GetToken,
                                item_ty,
                            ));
                        }
                        stmt::GetWidth::Chars(expr) => {
                            let width_ty = self.infer_expr(db, expr);
                            self.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::GetChars { width: width_ty },
                                item_ty,
                            ));
                        }
                    }
                }
            }
            stmt::Stmt::For(_) => todo!(),
            stmt::Stmt::Loop(loop_stmt) => {
                for &stmt in &loop_stmt.stmts {
                    self.check_stmt(db, stmt);
                }
            }
            stmt::Stmt::Exit(exit_stmt) => {
                if let Some(when_condition) = exit_stmt.when_condition {
                    self.check_expr(db, when_condition, make::mk_boolean(db).into());
                }
            }
            stmt::Stmt::If(if_stmt) => {
                self.check_expr(db, if_stmt.condition, make::mk_boolean(db).into());

                // Apply condition constraints to branches.
                let mut false_branch = if_stmt.false_branch;
                loop {
                    let stmt::FalseBranch::ElseIf(elseif_stmt) = false_branch else {
                        break;
                    };
                    let stmt::Stmt::If(elseif_stmt) = self.store.stmt(elseif_stmt) else {
                        unreachable!()
                    };

                    self.check_expr(db, elseif_stmt.condition, make::mk_boolean(db).into());

                    false_branch = elseif_stmt.false_branch;
                }
            }
            stmt::Stmt::Case(_) => todo!(),
            stmt::Stmt::Block(block_stmt) => {
                for &stmt in &block_stmt.stmts {
                    self.check_stmt(db, stmt);
                }
            }
            stmt::Stmt::Call(_) => todo!(),
            stmt::Stmt::Return(_) => todo!(),
            stmt::Stmt::Result(_) => todo!(),
        }
    }

    fn check_expr(&mut self, db: &'db dyn Db, expr: expr::LocalExpr<'db>, flex_ty: FlexTy<'db>) {
        match self.store.expr(expr) {
            expr::Expr::Init(_) => {
                // checks: everything is a const-var
                // synth takes care of issuing arg checks
                unimplemented!()
            }
            expr::Expr::Binary(_) => {
                // checks: is impl'd binary op
            }
            expr::Expr::Unary(_) => {
                // checks: is impl'd unary op
            }
            expr::Expr::Range(_) => {
                // checks: if bounds check to the same type
                // synth both & issue a type eq
                unimplemented!()
            }
            expr::Expr::Field(_) => {
                // checks: something to do with place projection
                unimplemented!()
            }
            expr::Expr::Deref(_) => {
                // checks: expr synths to pointer type
                unimplemented!()
            }
            expr::Expr::Call(_) => {
                // checks:
                // arg is callable
            }
            _ => {
                let expr_ty = self.infer_expr(db, expr);
                self.check_subtype_of(FlexTy::Var(expr_ty), flex_ty);
            }
        }
    }

    fn infer_expr(&mut self, db: &'db dyn Db, expr: expr::LocalExpr<'db>) -> FlexVar<'db> {
        let ty_var = match self.store.expr(expr) {
            expr::Expr::Missing => {
                // Expr holes infer to type holes
                self.env.concrete_var(make::mk_error(db))
            }
            expr::Expr::Literal(literal) => self.env.concrete_var(match literal {
                expr::Literal::Integer(_) => make::mk_integer(db),
                expr::Literal::Real(_) => make::mk_number(db),
                expr::Literal::Char(_) => make::mk_char(db),
                expr::Literal::CharSeq(_) => todo!(),
                expr::Literal::String(_) => make::mk_string(db),
                expr::Literal::Boolean(_) => make::mk_boolean(db),
            }),
            expr::Expr::Init(_) => unimplemented!(),
            expr::Expr::Binary(_) => unimplemented!(),
            expr::Expr::Unary(_) => unimplemented!(),
            expr::Expr::All => {
                // todo: as unbounded range expr ty
                unimplemented!()
            }
            expr::Expr::Range(_) => unimplemented!(),
            expr::Expr::Name(name) => match name {
                expr::Name::Name(query_key) => {
                    // Type inference is deferred to solving
                    let ty_var = self.env.fresh_var();
                    self.check_resolution_of(*query_key, ty_var);
                    ty_var
                }
                expr::Name::Self_ => unimplemented!(),
            },
            expr::Expr::Field(_) => unimplemented!(),
            expr::Expr::Deref(_) => unimplemented!(),
            expr::Expr::Call(_) => unimplemented!(),
        };

        // Write-back the proper expression to use during type elaboration.
        self.env.delayed_expr_tys.insert(expr, ty_var);

        ty_var
    }

    fn check_subtype_of(&mut self, a: FlexTy<'db>, b: FlexTy<'db>) {
        let a = self.into_flex_var(a);
        let b = self.into_flex_var(b);
        self.constraints.push(Constraint::Subtype(a, b));
    }

    #[allow(unused)]
    fn check_unify_of(&mut self, a: FlexTy<'db>, b: FlexTy<'db>) {
        let a = self.into_flex_var(a);
        let b = self.into_flex_var(b);
        self.constraints.push(Constraint::Unify(a, b));
    }

    fn check_resolution_of(&mut self, query_key: scope::QueryKey<'db>, ty_var: FlexVar<'db>) {
        self.constraints
            .push(Constraint::ResolutionOf(query_key, ty_var));
    }

    fn into_flex_var(&mut self, flex_ty: FlexTy<'db>) -> FlexVar<'db> {
        match flex_ty {
            FlexTy::Unknown => self.env.fresh_var(),
            FlexTy::Var(flex_var) => flex_var,
            FlexTy::Concrete(ty) => self.env.concrete_var(ty),
            flex_ty @ FlexTy::Ty(_) => self.env.flex_ty(flex_ty),
        }
    }
}
