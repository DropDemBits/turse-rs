//! Type inference for bodies and items
//!
//! This defines how the types and constraints for bodies are made.

use std::{collections::VecDeque, mem};

use either::Either;
use toc_hir_def::scope;

use crate::{
    Db,
    ty::{
        self, FlexTy, FlexVar, IntSize, NatSize, RealSize, SubstFolder, Ty, TyKind, Variance,
        WithSubstContext, ir, make,
    },
};

pub mod body;
pub mod item;

#[derive(Debug, Default, Clone)]
struct ConstraintGenEnv<'db> {
    flex_vars: bicubify::BiunificationVariables<FlexVar<'db>>,
    constraints: Vec<Constraint<'db>>,
}

impl<'db> ConstraintGenEnv<'db> {
    pub fn fresh_var(&mut self) -> FlexVar<'db> {
        self.flex_vars.fresh_var()
    }

    pub fn concrete_var(&mut self, ty: Ty<'db>) -> FlexVar<'db> {
        let var = self.flex_vars.fresh_var();
        self.constraints.push(Constraint::Relate(
            FlexTy::Var(var),
            FlexTy::Concrete(ty),
            Variance::Covariant,
        ));
        var
    }

    fn clone_for_solving(&self) -> InferEnv<'db> {
        InferEnv {
            unify: self.flex_vars.clone_for_solving(),
            constraints: self.constraints.clone(),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct InferEnv<'db> {
    unify: bicubify::BiunificationTable<FlexVar<'db>>,
    constraints: Vec<Constraint<'db>>,
}

impl<'db> InferEnv<'db> {
    pub fn _fresh_var(&mut self) -> FlexVar<'db> {
        self.unify.fresh_var()
    }

    pub fn lookup_ty(&mut self, ty_var: FlexVar<'db>) -> Option<FlexTy<'db>> {
        let ty_var = self.unify.root_var(ty_var);

        let bounds = self.unify.get_var_bounds(ty_var);

        let inv_len = bounds.bounds_len(bicubify::unify::Polarity::Invariant);
        let upper_len = bounds.bounds_len(bicubify::unify::Polarity::Upper);
        let lower_len = bounds.bounds_len(bicubify::unify::Polarity::Lower);

        // Preference:
        // Invariant > Upper > Lower
        match (lower_len, inv_len, upper_len) {
            (_, inv_len, _) if inv_len == 1 => bounds.invariant_bounds().next().cloned(),
            (0, 0, 1) => bounds.upper_bounds().next().cloned(),
            (1, 0, 0) => bounds.lower_bounds().next().cloned(),
            _ => None,
        }
    }

    pub fn substitute(&mut self, db: &'db dyn Db, ty: FlexTy<'db>) -> Ty<'db> {
        match ty {
            FlexTy::Var(var) => match self.lookup_ty(var) {
                Some(ty) => self.substitute(db, ty),
                None => make::mk_error(db),
            },
            FlexTy::Concrete(ty) => ty,
            FlexTy::Ty(ty_kind) => ty_kind
                .substitute(WithSubstContext {
                    folder: self,
                    context: db,
                })
                .intern(db),
        }
    }
}

impl<'db> SubstFolder<ir::Infer<'db>, ir::Rigid>
    for WithSubstContext<&mut InferEnv<'db>, &'db dyn Db>
{
    fn subst_ty_var(&mut self, var: <ir::Infer<'db> as ir::TypeIr>::TyVar) -> TyKind<ir::Rigid> {
        let db = self.context;
        match self.folder.lookup_ty(var) {
            Some(ty) => self.folder.substitute(db, ty).kind(db).clone(),
            None => TyKind::Error,
        }
    }
}

/// A constraint against a set of flexible type variables.
///
/// Constraints direct what each flexible type variable should infer to.
#[derive(Debug, Clone)]
pub enum Constraint<'db> {
    /// Constrains `'a` to relate to`'b` (`'a • 'b`).
    Relate(FlexTy<'db>, FlexTy<'db>, Variance),
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

const DEFAULT_FUEL: usize = 1000;

#[derive(Debug)]
pub enum InferError<'db> {
    MismatchedTypes {
        left: FlexTy<'db>,
        right: FlexTy<'db>,
        variance: Variance,
    },
    BuiltinNotImplemented {
        interface: BuiltinInterface<'db>,
        ty: FlexTy<'db>,
    },
    ItemNotAType {
        item: toc_hir_def::item::Item<'db>,
        ty: FlexTy<'db>,
    },
}

#[derive(Debug)]
pub enum SolverError {
    NoProgress,
    FuelExhausted,
}

struct Solver<'db> {
    db: &'db dyn Db,
    env: InferEnv<'db>,
    deferred_constraints: VecDeque<Constraint<'db>>,
    errors: Vec<InferError<'db>>,
}

pub enum ConstraintResult {
    Finished,
    Stalled,
}

impl<'db> Solver<'db> {
    fn new(db: &'db dyn Db, env: InferEnv<'db>) -> Self {
        Self {
            db,
            env,
            deferred_constraints: Default::default(),
            errors: Default::default(),
        }
    }

    fn solve(&mut self) -> Result<(), SolverError> {
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
                    dbg!((
                        active_constraints,
                        stalled_constraints,
                        self.deferred_constraints.clone()
                    ));
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
                Constraint::Relate(left, right, variance) => {
                    self.try_relate_of(left, right, variance)
                }
                Constraint::ResolutionOf(_, _) => unreachable!("encountered unfilled resolution"),
                Constraint::ImplsBuiltin(interface, ty) => self.try_impls_builtin(interface, ty),
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
            // Normalize to the representative variable.
            FlexTy::Var(var) => FlexTy::Var(self.env.unify.root_var(var)),
            // Concrete types always normalize to themselves>
            FlexTy::Concrete(ty) => FlexTy::Concrete(ty),
            // Deep-normalize any types with flexible vars.
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
                TyKind::Char => FlexTy::Concrete(make::mk_char(db)),
                TyKind::String => FlexTy::Concrete(make::mk_string(db)),
                // Flexible variables are normalized into their representative variables.
                TyKind::FlexVar(var) => self.normalize_ty(db, FlexTy::Var(var)),
                // Any other type that can contain other types has their component types normalized.
                TyKind::Place(ty_kind, mutability) => {
                    let place_ty = self.normalize_ty(db, FlexTy::Ty(ty_kind)).into_ty_kind(db);

                    FlexTy::Ty(Box::new(TyKind::Place(Box::new(place_ty), mutability)))
                }
            },
        }
    }

    fn try_relate_of(
        &mut self,
        left: FlexTy<'db>,
        right: FlexTy<'db>,
        variance: Variance,
    ) -> ConstraintResult {
        let db = self.db;
        let left_ty = self.normalize_ty(db, left.clone());
        let right_ty = self.normalize_ty(db, right.clone());

        let (left_ty, right_ty) = match variance {
            Variance::Covariant | Variance::Invariant => (left_ty, right_ty),
            Variance::Contravariant => (right_ty, left_ty),
        };

        let mut relate_ty_ty = |relate: Either<
            bicubify::unify::LowerBound<'_, FlexVar<'db>>,
            bicubify::unify::UpperBound<'_, FlexVar<'db>>,
        >| {
            match relate {
                Either::Left(lower) => {
                    self.deferred_constraints.push_front(Constraint::Relate(
                        lower.lower.clone(),
                        lower.term.clone(),
                        Variance::Covariant,
                    ));
                }
                Either::Right(upper) => {
                    self.deferred_constraints.push_front(Constraint::Relate(
                        upper.upper.clone(),
                        upper.term.clone(),
                        Variance::Contravariant,
                    ));
                }
            }
        };

        let res = match (&left_ty, &right_ty) {
            // Equate any vars up-front.
            (FlexTy::Var(left), FlexTy::Var(right)) => {
                let (left, right) = (*left, *right);

                if self.env.unify.shallow_unify_vars(left, right).is_err() {
                    self.env.unify.full_unify_var_var(left, right, relate_ty_ty);
                }

                Ok(())
            }
            (FlexTy::Var(var), right_ty) => {
                self.env.unify.lub_var(*var, right_ty.clone(), |relate| {
                    relate_ty_ty(Either::Left(relate))
                });

                Ok(())
            }
            (left_ty, FlexTy::Var(var)) => {
                self.env.unify.glb_var(left_ty.clone(), *var, |relate| {
                    relate_ty_ty(Either::Right(relate))
                });

                Ok(())
            }
            // Try structurally relating.
            (FlexTy::Concrete(left_ty), FlexTy::Concrete(right_ty)) => left_ty
                .kind(db)
                .relate_with(right_ty.kind(db), variance, self),
            (FlexTy::Concrete(ty), FlexTy::Ty(flex_ty)) => {
                ty.kind(db).relate_with(flex_ty, variance, self)
            }
            (FlexTy::Ty(flex_ty), FlexTy::Concrete(ty)) => {
                flex_ty.relate_with(ty.kind(db), variance, self)
            }
            (FlexTy::Ty(left_flex), FlexTy::Ty(right_flex)) => {
                left_flex.relate_with(right_flex, variance, self)
            }
        };

        if res.is_err() {
            self.errors.push(InferError::MismatchedTypes {
                left,
                right,
                variance,
            });
        }

        ConstraintResult::Finished
    }

    fn try_impls_builtin(
        &mut self,
        interface: BuiltinInterface<'db>,
        ty: FlexVar<'db>,
    ) -> ConstraintResult {
        let ty = self.normalize_ty(self.db, ty.into());
        let kind = match ty.clone().into_ty_kind(self.db) {
            TyKind::FlexVar(_) => return ConstraintResult::Stalled,
            kind => kind,
        };

        if matches!(kind, TyKind::Error) {
            return ConstraintResult::Finished;
        }

        let impls_interface = match interface {
            BuiltinInterface::GetToken => todo!(),
            BuiltinInterface::GetLine => todo!(),
            BuiltinInterface::GetChars { width: _ } => todo!(),
            BuiltinInterface::Put => {
                fn impls_put<I: ir::TypeIr>(kind: &TyKind<I>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put(kind),
                        TyKind::Boolean
                        | TyKind::Int(_)
                        | TyKind::Nat(_)
                        | TyKind::Real(_)
                        | TyKind::Char
                        | TyKind::String => true,
                        _ => false,
                    }
                }

                impls_put(&kind)
            }
            BuiltinInterface::PutWithPrecision { precision } => {
                fn impls_put_with_precision<I: ir::TypeIr>(kind: &TyKind<I>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put_with_precision(kind),
                        TyKind::Int(_) | TyKind::Nat(_) | TyKind::Real(_) => true,
                        _ => false,
                    }
                }

                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Var(precision),
                    FlexTy::Concrete(make::mk_int(self.db)),
                    Variance::Covariant,
                ));

                impls_put_with_precision(&kind)
            }
            BuiltinInterface::PutWithExponent {
                precision,
                exponent,
            } => {
                fn impls_put_with_exponent<I: ir::TypeIr>(kind: &TyKind<I>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put_with_exponent(kind),
                        TyKind::Int(_) | TyKind::Nat(_) | TyKind::Real(_) => true,
                        _ => false,
                    }
                }

                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Var(precision),
                    FlexTy::Concrete(make::mk_int(self.db)),
                    Variance::Covariant,
                ));
                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Var(exponent),
                    FlexTy::Concrete(make::mk_int(self.db)),
                    Variance::Covariant,
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

impl<'db> ty::RelateContext<ir::Rigid, ir::Rigid> for Solver<'db> {
    fn relate_tys(&mut self, relate: ty::TyRelation<ir::Rigid, ir::Rigid>) -> Result<(), ()> {
        match relate.always_rigid() {}
    }
}

impl<'db> ty::RelateContext<ir::Rigid, ir::Infer<'db>> for Solver<'db> {
    fn relate_tys(&mut self, relate: ty::TyRelation<ir::Rigid, ir::Infer<'db>>) -> Result<(), ()> {
        match relate {
            ty::TyRelation::VarVar(left, _) | ty::TyRelation::VarTy(left, _) => match left {},
            ty::TyRelation::TyVar(ty, var) => {
                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Concrete(ty.clone().intern(self.db)),
                    FlexTy::Var(var),
                    Variance::Covariant,
                ));

                Ok(())
            }
        }
    }
}

impl<'db> ty::RelateContext<ir::Infer<'db>, ir::Rigid> for Solver<'db> {
    fn relate_tys(&mut self, relate: ty::TyRelation<ir::Infer<'db>, ir::Rigid>) -> Result<(), ()> {
        self.relate_tys(relate.reverse())
    }
}

impl<'db> ty::RelateContext<ir::Infer<'db>, ir::Infer<'db>> for Solver<'db> {
    fn relate_tys(
        &mut self,
        relate: ty::TyRelation<ir::Infer<'db>, ir::Infer<'db>>,
    ) -> Result<(), ()> {
        match relate {
            ty::TyRelation::VarVar(left, right) => {
                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Var(left),
                    FlexTy::Var(right),
                    Variance::Covariant,
                ));

                Ok(())
            }
            ty::TyRelation::VarTy(var, ty) => {
                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Var(var),
                    FlexTy::Ty(Box::new(ty.clone())),
                    Variance::Covariant,
                ));

                Ok(())
            }
            ty::TyRelation::TyVar(ty, var) => {
                self.deferred_constraints.push_front(Constraint::Relate(
                    FlexTy::Ty(Box::new(ty.clone())),
                    FlexTy::Var(var),
                    Variance::Covariant,
                ));

                Ok(())
            }
        }
    }
}
