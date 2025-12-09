//! Type inference for bodies

use std::collections::HashMap;

use toc_hir_def::{body, expr, item, local, scope, stmt};

use crate::{
    ConstVarTyExt, Db,
    infer::{BuiltinInterface, Constraint, InferEnv, InferError, Solver},
    ty::{self, FlexTy, FlexVar, Ty, TyKind, make},
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

#[salsa::tracked]
pub(crate) fn infer_body<'db>(db: &'db dyn Db, body: body::Body<'db>) -> BodyInfer<'db> {
    // We assume that the body is well-formed enough, i.e. we are resilient in the face of body lowering errors.
    let contents = body.contents(db);
    let mut cctx = ExprStoreConstraints::new(contents);

    for stmt in body.top_level_stmts(db) {
        cctx.check_stmt(db, stmt.stmt());
    }
    cctx.fill_body_resolutions(db, body.resolved_names(db));

    let mut solver = Solver::new(cctx.env.clone());
    if let Err(err) = solver.solve(db) {
        eprintln!("solver err: {err:?}");
    }
    if !solver.errors.is_empty() {
        eprintln!("infer err: {:#?}", solver.errors);
    }
    cctx.env = solver.env;

    let expr_tys = cctx
        .delayed_expr_tys
        .into_iter()
        .map(|(expr, var)| (expr, cctx.env.substitute(db, var.into())))
        .collect();

    let local_tys = cctx
        .delayed_local_tys
        .into_iter()
        .map(|(local, var)| (local, cctx.env.substitute(db, var.into())))
        .collect();

    BodyInfer::new(db, expr_tys, local_tys)
}

#[derive(Debug)]
struct ExprStoreConstraints<'db> {
    store: &'db body::BodyContents<'db>,
    env: InferEnv<'db>,
    delayed_expr_tys: HashMap<expr::LocalExpr<'db>, FlexVar<'db>>,
    delayed_local_tys: HashMap<local::LocalId<'db>, FlexVar<'db>>,
    errors: Vec<InferError<'db>>,
}

impl<'db> ExprStoreConstraints<'db> {
    fn new(store: &'db body::BodyContents<'db>) -> Self {
        Self {
            store,
            env: Default::default(),
            delayed_expr_tys: Default::default(),
            delayed_local_tys: Default::default(),
            errors: vec![],
        }
    }

    fn check_stmt(&mut self, db: &'db dyn Db, stmt: stmt::LocalStmt<'db>) {
        match self.store.stmt(stmt) {
            stmt::Stmt::InitializeConstVar(_, _) => todo!(),
            stmt::Stmt::InitializeBindItem(_, _) => todo!(),
            stmt::Stmt::LocalConstVar(local_const_var) => {
                let decl_ty = local_const_var
                    .ty_cons
                    .map(|ty_cons| ty::lower::from_cons(db, ty_cons.cons(db)));

                let expr_ty = local_const_var
                    .initializer
                    .map(|expr| self.infer_expr(db, expr));

                if let Some((decl_ty, expr_ty)) = decl_ty.zip(expr_ty) {
                    // Initializer should be convertable into the declaration type>
                    self.check_supertype_of(decl_ty.into(), expr_ty.into());
                }

                let inferred_ty = if let Some(decl_ty) = decl_ty {
                    // Use the type spec as the canonical type.
                    decl_ty.kind(db).clone().instantiate()
                } else if let Some(expr_ty) = expr_ty {
                    // Infer it from the initializer.
                    TyKind::FlexVar(expr_ty)
                } else {
                    // No other place to infer it from.
                    TyKind::Error
                };

                // Local constvars always correspond to a place of some type.
                let inferred_ty = self.env.flex_ty(FlexTy::Ty(Box::new(TyKind::Place(
                    Box::new(inferred_ty),
                    local_const_var.mutability,
                ))));

                self.delayed_local_tys
                    .insert(local_const_var.local, inferred_ty);
            }
            stmt::Stmt::Assign(_) => todo!(),
            stmt::Stmt::Put(put) => {
                if let Some(stream_num) = put.stream_num {
                    self.check_expr(db, stream_num, make::mk_int(db).into());
                }

                // FIXME: Insert place coercions
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
                            self.env
                                .constraints
                                .push(Constraint::ImplsBuiltin(BuiltinInterface::Put, item_ty));
                        }
                        stmt::PutOpts::WithPrecision {
                            width: _,
                            precision,
                        } => {
                            let precision_ty = self.infer_expr(db, precision);

                            self.env.constraints.push(Constraint::ImplsBuiltin(
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

                            self.env.constraints.push(Constraint::ImplsBuiltin(
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

                // FIXME: Insert place coercions
                for get_item in &get.items {
                    let stmt::Skippable::Item(get_item) = get_item else {
                        continue;
                    };

                    let item_ty = self.infer_expr(db, get_item.expr);

                    match get_item.width {
                        stmt::GetWidth::Token => {
                            self.env.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::GetToken,
                                item_ty,
                            ));
                        }
                        stmt::GetWidth::Line => {
                            self.env.constraints.push(Constraint::ImplsBuiltin(
                                BuiltinInterface::GetToken,
                                item_ty,
                            ));
                        }
                        stmt::GetWidth::Chars(expr) => {
                            let width_ty = self.infer_expr(db, expr);
                            self.env.constraints.push(Constraint::ImplsBuiltin(
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
                // FIXME: Make this 'a<int...>
                expr::Literal::Integer(_) => make::mk_int(db),
                // FIXME: Make this 'a<real...>
                expr::Literal::Real(_) => make::mk_real(db),
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

        eprintln!("sything {expr:?} to {ty_var:?}");

        // Write-back the proper expression to use during type elaboration.
        self.delayed_expr_tys.insert(expr, ty_var);

        ty_var
    }

    fn fill_body_resolutions(&mut self, db: &'db dyn Db, resolutions: body::ResolvedNames<'db>) {
        let mut to_fill = vec![];
        self.env.constraints.retain(|constraint| {
            let Constraint::ResolutionOf(query, to_var) = constraint else {
                return true;
            };

            to_fill.push((*query, *to_var));

            false
        });

        for (query_key, to_var) in to_fill {
            match resolutions.binding_of(db, query_key) {
                Some(binding) => match binding {
                    scope::Binding::Item(item) => {
                        eprintln!("filling {to_var:?} with {item:?}");

                        let ty = match item {
                            item::Item::ConstVar(const_var) => const_var.type_of(db),
                            item @ item::Item::Module(_) => {
                                self.errors.push(InferError::ItemNotAType {
                                    item,
                                    ty: FlexTy::Var(to_var),
                                });
                                make::mk_error(db)
                            }
                        };

                        self.env
                            .unify_var_to(to_var, FlexTy::Concrete(ty))
                            .expect("should always unify");
                    }
                    scope::Binding::Local(local) => {
                        eprintln!("filling {to_var:?} with {local:?}");
                        let local_ty = self
                            .delayed_local_tys
                            .get(&local)
                            .expect("should have encountered local");

                        self.env
                            .unify_vars(to_var, *local_ty)
                            .expect("should always unify");
                    }
                },
                None => self
                    .env
                    .unify_var_to(to_var, FlexTy::Concrete(make::mk_error(db)))
                    .expect("should be unresolved"),
            }
        }
    }

    fn check_subtype_of(&mut self, a: FlexTy<'db>, b: FlexTy<'db>) {
        let a = self.into_flex_var(a);
        let b = self.into_flex_var(b);
        self.env.constraints.push(Constraint::Subtype(a, b));
    }

    fn check_supertype_of(&mut self, a: FlexTy<'db>, b: FlexTy<'db>) {
        let a = self.into_flex_var(a);
        let b = self.into_flex_var(b);
        self.env.constraints.push(Constraint::Subtype(b, a));
    }

    #[allow(unused)]
    fn check_unify_of(&mut self, a: FlexTy<'db>, b: FlexTy<'db>) {
        let a = self.into_flex_var(a);
        let b = self.into_flex_var(b);
        self.env.constraints.push(Constraint::Unify(a, b));
    }

    fn check_resolution_of(&mut self, query_key: scope::QueryKey<'db>, ty_var: FlexVar<'db>) {
        self.env
            .constraints
            .push(Constraint::ResolutionOf(query_key, ty_var));
    }

    fn into_flex_var(&mut self, flex_ty: FlexTy<'db>) -> FlexVar<'db> {
        match flex_ty {
            FlexTy::Var(flex_var) => flex_var,
            FlexTy::Concrete(ty) => self.env.concrete_var(ty),
            flex_ty @ FlexTy::Ty(_) => self.env.flex_ty(flex_ty),
        }
    }
}
