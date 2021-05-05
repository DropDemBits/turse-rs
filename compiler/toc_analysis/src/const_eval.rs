//! Constant evaluation stuff
mod integer;
mod ops;
#[cfg(test)]
mod test;

use std::fmt;
use std::{
    convert::{TryFrom, TryInto},
    sync::{Arc, RwLock},
};

use indexmap::IndexMap;
use toc_hir::Unit;
use toc_hir::{expr, symbol::GlobalDefId, UnitId, UnitMap};
use toc_span::Spanned;

pub use integer::ConstInt;
use ops::ConstOp;

// Evaluation process:
// Starting at const expr
// Unfold expr tree in post order traversal

// TODO: Deal with const vars with type errors
// Only need to map the var to an error const expr

/// Collects all `const` definitions from the unit into the given `ConstEvalCtx`
pub fn collect_const_vars(unit: &Unit, const_eval: Arc<ConstEvalCtx>) {
    use toc_hir::{stmt, HirVisitor};

    struct Visitor {
        unit_id: UnitId,
        const_eval: Arc<ConstEvalCtx>,
    }

    impl HirVisitor for Visitor {
        fn visit_constvar(&mut self, _id: stmt::StmtIdx, decl: &stmt::ConstVar) {
            if decl.is_const {
                if let Some(init_expr) = decl.tail.init_expr() {
                    let const_expr = self.const_eval.defer_expr(self.unit_id, init_expr);
                    // Add mappings
                    for def in &decl.names {
                        self.const_eval
                            .add_var(def.into_global(self.unit_id), const_expr);
                    }
                }
            };
        }

        // TODO: Visit type stmt for enum constvar & constvalue
    }

    let mut visitor = Visitor {
        unit_id: unit.id,
        const_eval,
    };

    unit.walk_nodes(&mut visitor);
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    /// General integer value
    Integer(ConstInt),
    /// Floating point value
    Real(f64),
    /// Boolean value
    Bool(bool),
}

impl ConstValue {
    /// Unwraps a `ConstValue` as `ConstInt`
    ///
    /// ## Returns
    /// If `self` is a `ConstValue::Integer`, returns the corresponding ConstInt value.
    /// Otherwise, returns `None`.
    pub fn as_int(&self) -> Option<ConstInt> {
        match self {
            ConstValue::Integer(v) => Some(*v),
            _ => None,
        }
    }

    /// Gets the human readable version of the value's type
    pub fn type_name(&self) -> &str {
        match self {
            ConstValue::Integer(_) => "integer value",
            ConstValue::Real(_) => "real value",
            ConstValue::Bool(_) => "boolean value",
        }
    }

    /// Converts a `ConstValue` into a `ConstInt`.
    ///
    /// The only value types that are allowed to be cast into `ConstInt` are:
    ///
    /// - `Integer`
    fn cast_into_int(self) -> Result<ConstInt, ConstError> {
        match self {
            ConstValue::Integer(v) => Ok(v),
            _ => Err(ConstError::WrongType),
        }
    }

    /// Converts a `ConstValue` into a `f64`.
    ///
    /// The only value types that are allowed to be cast into a `f64` are:
    ///
    /// - `Integer`
    /// - `Real`
    fn cast_into_real(self) -> Result<f64, ConstError> {
        match self {
            ConstValue::Integer(v) => Ok(v.into_f64()),
            ConstValue::Real(v) => Ok(v),
            _ => Err(ConstError::WrongType),
        }
    }

    /// Converts a `ConstValue` into a `bool`.
    ///
    /// The only value types that are allowed to be cast into `bool` are:
    ///
    /// - `Bool`
    fn cast_into_bool(self) -> Result<bool, ConstError> {
        match self {
            ConstValue::Bool(v) => Ok(v),
            _ => Err(ConstError::WrongType),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum ConstError {
    // Traversal errors
    /// Encountered an evaluation cycle
    #[error("detected a compile-time evaluation cycle")]
    EvalCycle,
    /// Missing expression operand
    #[error("operand is an invalid expression")]
    MissingExpr,
    /// Not a valid const eval operation
    #[error("operation cannot be computed at compile-time")]
    NotConstOp,
    /// No const expr is associated with this identifer.
    /// Provided span is the span of the symbol's definition
    #[error("reference cannot be computed at compile-time")]
    NoConstExpr(toc_span::TextRange),
    /// Error is already reported
    #[error("compile-time evaluation error already reported")]
    Reported,

    // Computation errors
    /// Wrong operand type in eval expression
    #[error("wrong type for compile-time expression")]
    WrongType,
    /// Integer overflow
    #[error("integer overflow in compile-time expression")]
    IntOverflow,
    /// Floating point overflow
    #[error("real overflow in compile-time expression")]
    RealOverflow,
    /// Division by zero
    #[error("division by zero in compile-time expression")]
    DivByZero,
    /// Negative int exponent provided during power raising
    #[error("raising integer to a negative exponent")]
    NegativeIntExp,
    /// Negative int shift provided during bit shifting
    #[error("bit shifting integer by a negative amount")]
    NegativeIntShift,
}

pub type ConstResult<T> = Result<T, Spanned<ConstError>>;

/// Constant evaluation context
#[derive(Debug)]
pub struct ConstEvalCtx {
    inner: RwLock<InnerCtx>,
}

impl ConstEvalCtx {
    pub(crate) fn new(unit_map: Arc<UnitMap>) -> Self {
        Self {
            inner: RwLock::new(InnerCtx::new(unit_map)),
        }
    }

    /// Defers the evaluation of an expression for later, giving back a handle
    /// for later evaluation
    pub fn defer_expr(&self, unit_id: UnitId, expr: expr::ExprIdx) -> ConstExpr {
        let mut inner = self.inner.write().unwrap();
        inner.defer_expr(unit_id, expr)
    }

    /// Adds a reference to a constant variable that can be constant evaluable
    ///
    /// The `init_expr` expression must be assignable with respect to the
    /// constant's type spec (excluding bounded value restrictions).
    pub fn add_var(&self, def_id: GlobalDefId, init_expr: ConstExpr) {
        let mut inner = self.inner.write().unwrap();
        inner.add_var(def_id, init_expr);
    }

    /// Evaluates the value of an expression
    pub fn eval_expr(&self, expr: ConstExpr) -> ConstResult<ConstValue> {
        // TODO: Try to look for a cached result before entering the actual computation

        let mut inner = self.inner.write().unwrap();
        inner.eval_expr(expr)
    }

    /// Evaluates the value of a constant variable
    pub fn eval_var(&self, var: GlobalDefId) -> ConstResult<ConstValue> {
        // TODO: Try to look for a cached result before entering the actual computation

        let mut inner = self.inner.write().unwrap();
        inner.eval_var(var)
    }
}

/// Reference to an expression to be evaluated
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstExpr {
    id: usize,
}
// Maps `ConstExpr` to unit local `toc_hir::expr::ExprIdx`

impl fmt::Debug for ConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("ConstExpr {{ id: {:?} }}", self.id))
    }
}

struct InnerCtx {
    /// Mapping for all of the units
    unit_map: Arc<UnitMap>,
    /// Evaluation info of constant exprs
    eval_infos: Vec<EvalInfo>,
    /// Mapping GlobalDefId's into the corresponding ConstExpr
    var_to_expr: IndexMap<GlobalDefId, ConstExpr>,
}

impl InnerCtx {
    fn new(unit_map: Arc<UnitMap>) -> Self {
        Self {
            unit_map,
            eval_infos: Vec::new(),
            var_to_expr: IndexMap::new(),
        }
    }

    fn defer_expr(&mut self, unit_id: UnitId, expr: expr::ExprIdx) -> ConstExpr {
        let v = ConstExpr {
            id: self.eval_infos.len(),
        };

        let span = self.unit_map.get_unit(unit_id).database.expr_nodes.spans[&expr];

        let info = EvalInfo {
            unit_id,
            expr_id: expr,
            span,
            state: State::Unevaluated,
        };

        self.eval_infos.push(info);
        v
    }

    fn add_var(&mut self, def_id: GlobalDefId, init_expr: ConstExpr) {
        // Map the def to the const expr
        self.var_to_expr.insert(def_id, init_expr);
    }

    fn eval_expr(&mut self, expr: ConstExpr) -> ConstResult<ConstValue> {
        let info = &self.eval_infos[expr.id];
        let span = info.span;

        // Lookup the initial state of the expression
        match &info.state {
            // Give cached value
            State::Value(v) => return Ok(v.clone()),
            State::Error(_) => {
                // Error should already be reported
                return Err(Spanned::new(ConstError::Reported, span));
            }
            State::Evaluating => {
                // Encountered an evaluation cycle, update the evaluation state
                self.eval_infos[expr.id].state = State::Error(ConstError::EvalCycle);

                let err = Spanned::new(ConstError::EvalCycle, span);
                return Err(err);
            }
            State::Unevaluated => (),
        };

        // Do the eval
        let (unit_id, root_expr) = (info.unit_id, info.expr_id);
        let result = self.do_eval_expr(unit_id, root_expr, expr);

        match result {
            Ok(v) => Ok(v),
            Err(err) => {
                let err = if let ConstError::Reported = err.item() {
                    // Adjust the report location to the current expr
                    Spanned::new(ConstError::Reported, span)
                } else {
                    err
                };

                // Update the eval state with the corresponding error
                self.eval_infos[expr.id].state = State::Error(err.item().clone());

                Err(err)
            }
        }
    }

    fn eval_var(&mut self, var: GlobalDefId) -> ConstResult<ConstValue> {
        // TODO: Handle evaluation restrictions
        let const_expr = *self.var_to_expr.get(&var).ok_or_else(|| {
            // Fetch the span of the declaration
            let (unit_id, def_id) = (var.unit_id(), var.as_local());

            let span = self
                .unit_map
                .get_unit(unit_id)
                .symbol_table
                .get_def_span(def_id);

            Spanned::new(ConstError::NoConstExpr(span), span)
        })?;

        self.eval_expr(const_expr)
    }

    fn do_eval_expr(
        &mut self,
        unit_id: UnitId,
        root_expr: expr::ExprIdx,
        const_expr: ConstExpr,
    ) -> ConstResult<ConstValue> {
        #[derive(Debug)]
        enum Eval {
            Expr(expr::ExprIdx),
            Op(ConstOp, toc_span::TextRange),
        }

        // Unevaluated, evaluate the expression
        // Update the evaluation state to catch any evaluation cycles
        self.eval_infos[const_expr.id].state = State::Evaluating;

        // TODO: Feed in restrictions from somewhere
        let allow_64bit_ops = false;

        // Do the actual evaluation, as a stack maching
        let mut eval_stack = vec![Eval::Expr(root_expr)];
        let mut operand_stack = vec![];

        loop {
            eprintln!("> {:?}", eval_stack);
            eprintln!("? {:?}", operand_stack);

            let local_expr = match eval_stack.pop() {
                Some(Eval::Expr(expr)) => expr,
                Some(Eval::Op(op, span)) => {
                    // Perform operation
                    let result = op
                        .evaluate(&mut operand_stack, allow_64bit_ops)
                        .map_err(|err| Spanned::new(err, span))?;
                    operand_stack.push(result);
                    continue;
                }
                None => break,
            };

            // Fetch here to deal with borrowck
            // Always reacquire the new unit
            let unit = &self.unit_map[unit_id];
            let expr_span = unit.database.expr_nodes.spans[&local_expr];

            // ???: How to deal with enum field accesses?
            // We don't have access to a TyCtx
            // - type with enum decl could add a const expr containing the entire enum def
            match &unit.database[local_expr] {
                expr::Expr::Missing => {
                    // Bail out
                    return Err(Spanned::new(ConstError::MissingExpr, expr_span));
                }
                expr::Expr::Literal(expr) => {
                    // ???: How to deal with 32-bit vs 64-bit integers?
                    // - Could yoink info from somewhere?
                    // - Only need to know if ops need to be done in either 32 or 64 bit mode

                    // Convert into a Constvalue
                    let operand = match expr {
                        expr::Literal::Integer(v) => {
                            let v = ConstInt::from_unsigned(*v, allow_64bit_ops)
                                .map_err(|err| Spanned::new(err, expr_span))?;
                            ConstValue::Integer(v)
                        }
                        expr::Literal::Real(v) => ConstValue::Real(*v),
                        expr::Literal::CharSeq(_str) => todo!(),
                        expr::Literal::String(_str) => todo!(),
                        expr::Literal::Boolean(v) => ConstValue::Bool(*v),
                    };

                    operand_stack.push(operand);
                }
                expr::Expr::Binary(expr) => {
                    // Push both expression operands and the operation
                    let (op, span) = (expr.op.try_into()?, expr.op.span());
                    eval_stack.push(Eval::Op(op, span));
                    eval_stack.push(Eval::Expr(expr.rhs));
                    eval_stack.push(Eval::Expr(expr.lhs));
                }
                expr::Expr::Unary(expr) => {
                    // Push expr operand & operator
                    let (op, span) = (expr.op.try_into()?, expr.op.span());
                    eval_stack.push(Eval::Op(op, span));
                    eval_stack.push(Eval::Expr(expr.rhs));
                }
                expr::Expr::Paren(expr) => {
                    // Push the inner expr
                    eval_stack.push(Eval::Expr(expr.expr));
                }
                expr::Expr::Name(name) => {
                    // May or may not reference a constant expression
                    match name {
                        expr::Name::Name(use_id) => {
                            // Lookup var
                            // ???: How to lookup identifiers imported from different units
                            // - `Unit` should have an import table mapping local `UseId`s to
                            //   `GlobalDefId`'s
                            let global_def = use_id.as_def().into_global(unit_id);

                            // Eval var
                            let value = self.eval_var(global_def).map_err(|err| {
                                if let ConstError::NoConstExpr(def_span) = err.item() {
                                    // Change the span to reflect the use
                                    Spanned::new(ConstError::NoConstExpr(*def_span), expr_span)
                                } else {
                                    // Keep as-is
                                    err
                                }
                            })?;

                            // Push (cached) value to the operand stack
                            operand_stack.push(value);
                        }
                        expr::Name::Self_ => {
                            // Never a const expr
                            // TODO: Use the self's associated def_id
                            return Err(Spanned::new(
                                ConstError::NoConstExpr(Default::default()),
                                expr_span,
                            ));
                        }
                    }
                }
            }
        }

        eprintln!("Operand Stack: {:?}", operand_stack);

        // Should be one value remaining
        let result = operand_stack
            .pop()
            .expect("All values popped off of operand stack");
        assert!(operand_stack.is_empty());

        // Update the evaluation state
        self.eval_infos[const_expr.id].state = State::Value(result.clone());

        Ok(result)
    }
}

impl fmt::Debug for InnerCtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InnerCtx")
            .field("eval_infos", &self.eval_infos)
            .field("var_to_expr", &self.var_to_expr)
            .finish()
    }
}

/// Associated info for a constant expression
struct EvalInfo {
    unit_id: UnitId,
    expr_id: expr::ExprIdx,
    /// Span of the constant expression
    span: toc_span::TextRange,
    /// Current evaluation state of the constant expression
    state: State,
}

impl fmt::Debug for EvalInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "EvalInfo {{ unit_id: {:?}, span: {:?}, state: {:?} }}",
            self.unit_id, self.span, self.state
        ))
    }
}

/// Evaluation state of a constant expression
enum State {
    /// The expression has not been evaluated yet
    Unevaluated,
    /// The expression is in the process of being evaluated
    Evaluating,
    /// The expression has been evaluated to a valid value
    Value(ConstValue),
    /// The expression has been evaluated, but not to a valid value
    Error(ConstError),
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            State::Unevaluated => f.write_fmt(format_args!("Unevaluated")),
            State::Evaluating => f.write_fmt(format_args!("Evaluating")),
            State::Value(v) => f.write_fmt(format_args!("Value({:?})", v)),
            State::Error(v) => f.write_fmt(format_args!("Error({:?})", v)),
        }
    }
}

impl TryFrom<Spanned<expr::BinaryOp>> for ConstOp {
    type Error = Spanned<ConstError>;

    fn try_from(op: Spanned<expr::BinaryOp>) -> Result<Self, Self::Error> {
        Ok(match op.item() {
            expr::BinaryOp::Add => Self::Add,
            expr::BinaryOp::Sub => Self::Sub,
            expr::BinaryOp::Mul => Self::Mul,
            expr::BinaryOp::Div => Self::Div,
            expr::BinaryOp::RealDiv => Self::RealDiv,
            expr::BinaryOp::Mod => Self::Mod,
            expr::BinaryOp::Rem => Self::Rem,
            expr::BinaryOp::Exp => Self::Exp,
            expr::BinaryOp::And => Self::And,
            expr::BinaryOp::Or => Self::Or,
            expr::BinaryOp::Xor => Self::Xor,
            expr::BinaryOp::Shl => Self::Shl,
            expr::BinaryOp::Shr => Self::Shr,
            expr::BinaryOp::Less => Self::Less,
            expr::BinaryOp::LessEq => Self::LessEq,
            expr::BinaryOp::Greater => Self::Greater,
            expr::BinaryOp::GreaterEq => Self::GreaterEq,
            expr::BinaryOp::Equal => Self::Equal,
            expr::BinaryOp::NotEqual => Self::NotEqual,
            expr::BinaryOp::Imply => Self::Imply,
            // Not a compile-time operation
            _ => return Err(Spanned::new(ConstError::NotConstOp, op.span())),
        })
    }
}

impl TryFrom<Spanned<expr::UnaryOp>> for ConstOp {
    type Error = Spanned<ConstError>;

    fn try_from(op: Spanned<expr::UnaryOp>) -> Result<Self, Self::Error> {
        match op.item() {
            expr::UnaryOp::Not => Ok(Self::Not),
            expr::UnaryOp::Identity => Ok(Self::Identity),
            expr::UnaryOp::Negate => Ok(Self::Negate),
        }
    }
}
