//! Type inference for bodies and items
//!
//! This defines how the types and constraints for bodies are made.

use std::{collections::VecDeque, mem};

use toc_hir_def::{Mutability, scope};

use crate::{
    Db,
    ty::{
        FlexTy, FlexVar, IntSize, NatSize, RealSize, SubstFolder, Ty, TyKind, Variance,
        WithSubstContext, ir, make,
    },
};

pub mod body;
pub mod item;

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
                TyKind::Char => FlexTy::Concrete(make::mk_char(db)),
                TyKind::String => FlexTy::Concrete(make::mk_string(db)),
                // Flexible variables are normalized into their raw types
                TyKind::FlexVar(var) => self.normalize_ty(db, FlexTy::Var(var)),
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
        let variance = Variance::Covariant;
        let left = self.normalize_ty(db, left.into());
        let right = self.normalize_ty(db, right.into());

        match (left, right) {
            // Equate any vars up-front.
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
            // Try structurally relating.
            (FlexTy::Concrete(left), FlexTy::Concrete(right)) => {
                if let Err(()) = self.probe_and_relate_ty_ty(db, left, right, variance) {
                    self.errors.push(InferError::MismatchedTypes {
                        left: FlexTy::Concrete(left),
                        right: FlexTy::Concrete(right),
                    });
                }
            }
            (FlexTy::Concrete(ty), FlexTy::Ty(flex_ty)) => {
                if let Err(()) = self.probe_and_relate_ty_flex(db, ty, &flex_ty, variance) {
                    self.errors.push(InferError::MismatchedTypes {
                        left: FlexTy::Concrete(ty),
                        right: FlexTy::Ty(flex_ty),
                    });
                }
            }
            (FlexTy::Ty(flex_ty), FlexTy::Concrete(ty)) => {
                if let Err(()) = self.probe_and_relate_ty_flex(db, ty, &flex_ty, variance.invert())
                {
                    self.errors.push(InferError::MismatchedTypes {
                        left: FlexTy::Ty(flex_ty),
                        right: FlexTy::Concrete(ty),
                    });
                }
            }
            (FlexTy::Ty(left), FlexTy::Ty(right)) => {
                if let Err(()) = self.probe_and_relate_flex_flex(&left, &right, variance) {
                    self.errors.push(InferError::MismatchedTypes {
                        left: FlexTy::Ty(left),
                        right: FlexTy::Ty(right),
                    });
                }
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
        let kind = match ty.clone().into_ty_kind(db) {
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
                fn impls_put_with_exponent<I: ir::TypeIr>(kind: &TyKind<I>) -> bool {
                    match kind {
                        TyKind::Place(kind, _) => impls_put_with_exponent(kind),
                        TyKind::Int(_) | TyKind::Nat(_) | TyKind::Real(_) => true,
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

    fn probe_and_relate_ty_ty(
        &mut self,
        db: &'db dyn Db,
        left: Ty<'db>,
        right: Ty<'db>,
        variance: Variance,
    ) -> Result<(), ()> {
        self.probe_and_relate(
            left.kind(db),
            right.kind(db),
            variance,
            |_, relate| match relate.never() {},
        )
    }

    fn probe_and_relate_ty_flex(
        &mut self,
        db: &'db dyn Db,
        left: Ty<'db>,
        right: &TyKind<ir::Infer<'db>>,
        variance: Variance,
    ) -> Result<(), ()> {
        self.probe_and_relate(
            left.kind(db),
            right,
            variance,
            |this, relate| match relate {
                TyRelate::VarVar(left, _) | TyRelate::VarTy(left, _) => match left {},
                TyRelate::TyVar(ty, var) => this
                    .env
                    .unify_var_to(var, FlexTy::Concrete(ty.clone().intern(db)))
                    .map_err(|_| ()),
            },
        )
    }

    fn probe_and_relate_flex_flex(
        &mut self,
        left: &TyKind<ir::Infer<'db>>,
        right: &TyKind<ir::Infer<'db>>,
        variance: Variance,
    ) -> Result<(), ()> {
        self.probe_and_relate(left, right, variance, |this, relate| match relate {
            TyRelate::VarVar(left, right) => this.env.unify_vars(left, right).map_err(|_| ()),
            TyRelate::VarTy(var, ty) | TyRelate::TyVar(ty, var) => this
                .env
                .unify_var_to(var, FlexTy::Ty(Box::new(ty.clone())))
                .map_err(|_| ()),
        })
    }

    fn probe_and_relate<'a, L, R>(
        &mut self,
        left: &'a TyKind<L>,
        right: &'a TyKind<R>,
        variance: Variance,
        relate_ty_vars: impl Fn(&mut Self, TyRelate<'a, L, R>) -> Result<(), ()>,
    ) -> Result<(), ()>
    where
        L: ir::TypeIr,
        R: ir::TypeIr,
    {
        match (left, right) {
            // At inferrence variables, lift variance to variables.
            (TyKind::FlexVar(left), TyKind::FlexVar(right)) => {
                relate_ty_vars(self, TyRelate::VarVar(*left, *right))
            }
            (TyKind::FlexVar(left), right) => relate_ty_vars(self, TyRelate::VarTy(*left, right)),
            (left, TyKind::FlexVar(right)) => relate_ty_vars(self, TyRelate::TyVar(left, *right)),
            // Short-circuit on errors.
            (TyKind::Error, _) | (_, TyKind::Error) => Ok(()),
            // Apply variance structurally until we meet vars.
            (TyKind::Place(left, left_mutl), TyKind::Place(right, right_mutl)) => {
                self.probe_and_relate(left, right, variance, relate_ty_vars)?;

                let (left_mutl, right_mutl) = match variance {
                    Variance::Covariant | Variance::Invariant => (left_mutl, right_mutl),
                    Variance::Contravariant => (right_mutl, left_mutl),
                };

                match (left_mutl, right_mutl) {
                    (left, right) if left == right => Ok(()),
                    (Mutability::Var, Mutability::Const) if !variance.is_invariant() => Ok(()),
                    _ => Err(()),
                }
            }
            (TyKind::Boolean, TyKind::Boolean) => Ok(()),
            (TyKind::Int(left), TyKind::Int(right)) if left == right => Ok(()),
            (TyKind::Nat(left), TyKind::Nat(right)) if left == right => Ok(()),
            (TyKind::Real(left), TyKind::Real(right)) if left == right => Ok(()),
            (TyKind::Char, TyKind::Char) => Ok(()),
            (TyKind::String, TyKind::String) => Ok(()),
            _ => Err(()),
        }
    }
}

enum TyRelate<'a, L, R>
where
    L: ir::TypeIr,
    R: ir::TypeIr,
{
    VarVar(L::TyVar, R::TyVar),
    VarTy(L::TyVar, &'a TyKind<R>),
    TyVar(&'a TyKind<L>, R::TyVar),
}

impl<'a> TyRelate<'a, ir::Rigid, ir::Rigid> {
    fn never(self) -> ! {
        match self {
            TyRelate::VarVar(never, _) | TyRelate::VarTy(never, _) | TyRelate::TyVar(_, never) => {
                match never {}
            }
        }
    }
}
