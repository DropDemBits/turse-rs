//! Type inference for bodies and items
//!
//! This defines how the types and constraints for bodies are made.

use std::{
    collections::{HashMap, VecDeque},
    mem,
};

use toc_hir_def::{body, expr, local, scope, stmt};

use crate::{
    Db,
    ty::{FlexTy, FlexVar, IntSize, NatSize, RealSize, Ty, TyKind, make},
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

#[derive(Debug, Default, Clone)]
struct InferEnv<'db> {
    flex_vars: ena::unify::InPlaceUnificationTable<FlexVar<'db>>,
    constraints: Vec<Constraint<'db>>,
}

impl<'db> InferEnv<'db> {
    pub fn fresh_var(&mut self) -> FlexVar<'db> {
        self.flex_vars.new_key(None)
    }

    pub fn concrete_var(&mut self, ty: Ty<'db>) -> FlexVar<'db> {
        self.flex_vars.new_key(Some(FlexTy::Concrete(ty)))
    }

    pub fn flex_ty(&mut self, flex_ty: FlexTy<'db>) -> FlexVar<'db> {
        self.flex_vars.new_key(Some(flex_ty))
    }

    pub fn lookup_ty(&mut self, ty_var: FlexVar<'db>) -> Option<FlexTy<'db>> {
        self.flex_vars.probe_value(ty_var)
    }

    pub fn normalize_var(&mut self, ty_var: FlexVar<'db>) -> FlexVar<'db> {
        self.flex_vars.find(ty_var)
    }

    pub fn unify_vars(
        &mut self,
        a: FlexVar<'db>,
        b: FlexVar<'db>,
    ) -> Result<(), (FlexTy<'db>, FlexTy<'db>)> {
        self.flex_vars.unify_var_var(a, b)
    }

    pub fn unify_var_to(
        &mut self,
        var: FlexVar<'db>,
        ty: FlexTy<'db>,
    ) -> Result<(), (FlexTy<'db>, FlexTy<'db>)> {
        self.flex_vars.unify_var_value(var, Some(ty))
    }

    pub fn substitute(&mut self, db: &'db dyn Db, ty: FlexTy<'db>) -> Ty<'db> {
        match ty {
            FlexTy::Var(var) => match self.lookup_ty(var) {
                Some(ty) => self.substitute(db, ty),
                None => make::mk_error(db),
            },
            FlexTy::Concrete(ty) => ty,
            FlexTy::Ty(ty_kind) => match *ty_kind {
                TyKind::Error => make::mk_error(db),
                TyKind::Boolean => make::mk_boolean(db),
                TyKind::Int(IntSize::Int1) => make::mk_int1(db),
                TyKind::Int(IntSize::Int2) => make::mk_int2(db),
                TyKind::Int(IntSize::Int4) => make::mk_int4(db),
                TyKind::Int(IntSize::Int) => make::mk_int(db),
                TyKind::Nat(NatSize::Nat1) => make::mk_nat1(db),
                TyKind::Nat(NatSize::Nat2) => make::mk_nat2(db),
                TyKind::Nat(NatSize::Nat4) => make::mk_nat4(db),
                TyKind::Nat(NatSize::Nat) => make::mk_nat(db),
                TyKind::Nat(NatSize::AddressInt) => make::mk_addressint(db),
                TyKind::Real(RealSize::Real4) => make::mk_real4(db),
                TyKind::Real(RealSize::Real8) => make::mk_real8(db),
                TyKind::Real(RealSize::Real) => make::mk_real(db),
                TyKind::Integer => make::mk_integer(db),
                TyKind::Number => make::mk_number(db),
                TyKind::Char => make::mk_char(db),
                TyKind::String => make::mk_string(db),
                // Flexible variables are normalized into their raw types
                TyKind::FlexVar(var) => return self.substitute(db, FlexTy::Var(var)),
                // Any other type that can contain other types has their component types normalized.
                TyKind::Place(ty_kind, mutability) => {
                    let place_ty = self.substitute(db, FlexTy::Ty(ty_kind)).kind(db).clone();

                    Ty::new(db, TyKind::Place(Box::new(place_ty), mutability))
                }
            },
        }
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

/// A constraint against a set of flexible type variables.
///
/// Constraints direct what each flexible type variable should infer to.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
    delayed_expr_tys: HashMap<expr::LocalExpr<'db>, FlexVar<'db>>,
    delayed_local_tys: HashMap<local::LocalId<'db>, FlexVar<'db>>,
}

impl<'db> ExprStoreConstraints<'db> {
    fn new(store: &'db body::BodyContents<'db>) -> Self {
        Self {
            store,
            env: Default::default(),
            delayed_expr_tys: Default::default(),
            delayed_local_tys: Default::default(),
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

                self.delayed_local_tys
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
                        // TODO: Synthesize types from items
                        self.env
                            .unify_var_to(to_var, FlexTy::Concrete(make::mk_error(db)))
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

const DEFAULT_FUEL: usize = 1000;

#[derive(Debug)]
pub enum InferError<'db> {
    MismatchedTypes {
        left: FlexTy<'db>,
        right: FlexTy<'db>,
    },
    BuiltinNotImplemented {
        interface: BuiltinInterface<'db>,
        ty: FlexTy<'db>,
    },
}

#[derive(Debug)]
pub enum SolverError {
    NoProgress,
    FuelExhausted,
}

struct Solver<'db> {
    env: InferEnv<'db>,
    deferred_constraints: VecDeque<Constraint<'db>>,
    errors: Vec<InferError<'db>>,
}

pub enum ConstraintResult {
    Finished,
    Stalled,
}

impl<'db> Solver<'db> {
    fn new(env: InferEnv<'db>) -> Self {
        Self {
            env,
            deferred_constraints: Default::default(),
            errors: Default::default(),
        }
    }

    fn solve(&mut self, db: &'db dyn Db) -> Result<(), SolverError> {
        let mut active_constraints = VecDeque::from(mem::take(&mut self.env.constraints));
        let mut stalled_constraints = VecDeque::with_capacity(active_constraints.len());
        let mut fuel = DEFAULT_FUEL;
        let mut progress_made = false;

        'restart: loop {
            let Some(constraint) = active_constraints.pop_front() else {
                if stalled_constraints.is_empty() {
                    // No more constraints to solve
                    return Ok(());
                }

                if !progress_made {
                    return Err(SolverError::NoProgress);
                }
                let Some(next_fuel) = fuel.checked_sub(1) else {
                    return Err(SolverError::FuelExhausted);
                };

                mem::swap(&mut stalled_constraints, &mut active_constraints);
                fuel = next_fuel;
                progress_made = false;
                continue 'restart;
            };

            eprintln!("solving {constraint:?}");

            let res = match constraint.clone() {
                Constraint::Subtype(left, right) => self.try_subtype_of(db, left, right),
                Constraint::Unify(_left, _right) => todo!(),
                Constraint::ResolutionOf(_, _) => unreachable!("encountered unfilled resolution"),
                Constraint::ImplsBuiltin(interface, ty) => {
                    self.try_impls_builtin(db, interface, ty)
                }
            };

            match res {
                ConstraintResult::Finished => {
                    // We've made *some* amount of forward progress, we aren't fully stalled yet.
                    progress_made = true;
                }
                ConstraintResult::Stalled => {
                    // Requeue constraint to evaluate later.
                    stalled_constraints.push_back(constraint);
                }
            }

            // Evaluate any deferred constraints first.
            for deferred in self.deferred_constraints.drain(..).rev() {
                active_constraints.push_front(deferred);
            }
        }
    }

    fn normalize_ty(&mut self, db: &'db dyn Db, ty: FlexTy<'db>) -> FlexTy<'db> {
        match ty {
            // Normalize to either the representative var, or the original type
            FlexTy::Var(var) => match self.env.lookup_ty(var) {
                Some(ty) => self.normalize_ty(db, ty),
                None => FlexTy::Var(self.env.normalize_var(var)),
            },
            // Concrete types always normalize to themselves
            FlexTy::Concrete(ty) => FlexTy::Concrete(ty),
            // Deep-normalize any types with flexible vars
            FlexTy::Ty(ty) => match *ty {
                // Types that cannot be made flexible are converted into their corresponding concrete types
                TyKind::Error => FlexTy::Concrete(make::mk_error(db)),
                TyKind::Boolean => FlexTy::Concrete(make::mk_boolean(db)),
                TyKind::Int(IntSize::Int1) => FlexTy::Concrete(make::mk_int1(db)),
                TyKind::Int(IntSize::Int2) => FlexTy::Concrete(make::mk_int2(db)),
                TyKind::Int(IntSize::Int4) => FlexTy::Concrete(make::mk_int4(db)),
                TyKind::Int(IntSize::Int) => FlexTy::Concrete(make::mk_int(db)),
                TyKind::Nat(NatSize::Nat1) => FlexTy::Concrete(make::mk_nat1(db)),
                TyKind::Nat(NatSize::Nat2) => FlexTy::Concrete(make::mk_nat2(db)),
                TyKind::Nat(NatSize::Nat4) => FlexTy::Concrete(make::mk_nat4(db)),
                TyKind::Nat(NatSize::Nat) => FlexTy::Concrete(make::mk_nat(db)),
                TyKind::Nat(NatSize::AddressInt) => FlexTy::Concrete(make::mk_addressint(db)),
                TyKind::Real(RealSize::Real4) => FlexTy::Concrete(make::mk_real4(db)),
                TyKind::Real(RealSize::Real8) => FlexTy::Concrete(make::mk_real8(db)),
                TyKind::Real(RealSize::Real) => FlexTy::Concrete(make::mk_real(db)),
                TyKind::Integer => FlexTy::Concrete(make::mk_integer(db)),
                TyKind::Number => FlexTy::Concrete(make::mk_number(db)),
                TyKind::Char => FlexTy::Concrete(make::mk_char(db)),
                TyKind::String => FlexTy::Concrete(make::mk_string(db)),
                // Flexible variables are normalized into their raw types
                TyKind::FlexVar(var) => return self.normalize_ty(db, FlexTy::Var(var)),
                // Any other type that can contain other types has their component types normalized.
                TyKind::Place(ty_kind, mutability) => {
                    let place_ty = self.normalize_ty(db, FlexTy::Ty(ty_kind)).into_ty_kind(db);

                    FlexTy::Ty(Box::new(TyKind::Place(Box::new(place_ty), mutability)))
                }
            },
        }
    }

    fn try_subtype_of(
        &mut self,
        db: &'db dyn Db,
        left: FlexVar<'db>,
        right: FlexVar<'db>,
    ) -> ConstraintResult {
        let left = self.normalize_ty(db, left.into());
        let right = self.normalize_ty(db, right.into());

        match (left, right) {
            (FlexTy::Concrete(left), FlexTy::Concrete(right)) if left == right => {}
            (FlexTy::Var(left), FlexTy::Var(right)) => {
                if let Err((left, right)) = self.env.unify_vars(left, right) {
                    self.errors
                        .push(InferError::MismatchedTypes { left, right });
                }
            }
            (FlexTy::Var(var), ty) | (ty, FlexTy::Var(var)) => {
                if let Err((left, right)) = self.env.unify_var_to(var, ty) {
                    self.errors
                        .push(InferError::MismatchedTypes { left, right });
                }
            }
            (left, right) => {
                self.errors
                    .push(InferError::MismatchedTypes { left, right });
            }
        }

        ConstraintResult::Finished
    }

    fn try_impls_builtin(
        &mut self,
        db: &'db dyn Db,
        interface: BuiltinInterface<'db>,
        ty: FlexVar<'db>,
    ) -> ConstraintResult {
        let ty = self.normalize_ty(db, ty.into());
        let kind = match ty.clone() {
            FlexTy::Var(_) => return ConstraintResult::Stalled,
            FlexTy::Concrete(ty) => ty.kind(db).clone().into(),
            FlexTy::Ty(ty_kind) => *ty_kind,
        };

        if matches!(kind, TyKind::Error) {
            return ConstraintResult::Finished;
        }

        let impls_interface = match interface {
            BuiltinInterface::GetToken => todo!(),
            BuiltinInterface::GetLine => todo!(),
            BuiltinInterface::GetChars { width: _ } => todo!(),
            BuiltinInterface::Put => {
                fn impls_put<V>(kind: &TyKind<V>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put(kind),
                        TyKind::Boolean
                        | TyKind::Int(_)
                        | TyKind::Nat(_)
                        | TyKind::Real(_)
                        | TyKind::Integer
                        | TyKind::Number
                        | TyKind::Char
                        | TyKind::String => true,
                        _ => false,
                    }
                }

                impls_put(&kind)
            }
            BuiltinInterface::PutWithPrecision { precision } => {
                fn impls_put_with_precision<V>(kind: &TyKind<V>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put_with_precision(kind),
                        TyKind::Int(_)
                        | TyKind::Nat(_)
                        | TyKind::Real(_)
                        | TyKind::Integer
                        | TyKind::Number => true,
                        _ => false,
                    }
                }

                self.deferred_constraints.push_front(Constraint::Subtype(
                    precision,
                    self.env.concrete_var(make::mk_int(db)),
                ));

                impls_put_with_precision(&kind)
            }
            BuiltinInterface::PutWithExponent {
                precision,
                exponent,
            } => {
                fn impls_put_with_exponent<V>(kind: &TyKind<V>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put_with_exponent(kind),
                        TyKind::Int(_)
                        | TyKind::Nat(_)
                        | TyKind::Real(_)
                        | TyKind::Integer
                        | TyKind::Number => true,
                        _ => false,
                    }
                }

                self.deferred_constraints.push_front(Constraint::Subtype(
                    precision,
                    self.env.concrete_var(make::mk_int(db)),
                ));
                self.deferred_constraints.push_front(Constraint::Subtype(
                    exponent,
                    self.env.concrete_var(make::mk_int(db)),
                ));

                impls_put_with_exponent(&kind)
            }
            BuiltinInterface::Read => todo!(),
            BuiltinInterface::Write => todo!(),
            BuiltinInterface::Ordinal => todo!(),
        };

        if !impls_interface {
            self.errors
                .push(InferError::BuiltinNotImplemented { interface, ty });
        }

        ConstraintResult::Finished
    }
}
