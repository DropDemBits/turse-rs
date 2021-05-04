//! Constant evaluation stuff
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

// Evaluation process:
// Starting at const expr
// Unfold expr tree in post order traversal

// TODO: Deal with const vars with type errors
// Only need to map the var to an error const expr

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

#[derive(Debug, Clone)]
pub enum ConstValue {
    /// General integer value
    Integer(ConstInt),
    /// Floating point value
    Real(f64),
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
    /// No const expr is associated with this identifer
    #[error("reference cannot be computed at compile-time")]
    NoConstExpr,

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

struct InnerCtx {
    /// Mapping for all of the units
    unit_map: Arc<UnitMap>,
    /// Evaluation state of constant exprs
    eval_state: Vec<EvalState>,
    /// Mapping GlobalDefId's into the corresponding ConstExpr
    var_to_expr: IndexMap<GlobalDefId, ConstExpr>,
}

impl InnerCtx {
    fn new(unit_map: Arc<UnitMap>) -> Self {
        Self {
            unit_map,
            eval_state: Vec::new(),
            var_to_expr: IndexMap::new(),
        }
    }

    fn defer_expr(&mut self, unit_id: UnitId, expr: expr::ExprIdx) -> ConstExpr {
        let v = ConstExpr {
            id: self.eval_state.len(),
        };
        self.eval_state.push(EvalState::Unevaluated(unit_id, expr));
        v
    }

    fn add_var(&mut self, def_id: GlobalDefId, init_expr: ConstExpr) {
        // Map the def to the const expr
        self.var_to_expr.insert(def_id, init_expr);
    }

    fn eval_expr(&mut self, expr: ConstExpr) -> ConstResult<ConstValue> {
        // Lookup the initial state of the expression
        let (unit_id, root_expr) = match &self.eval_state[expr.id] {
            // Give cached value
            EvalState::Value(v) => return Ok(v.clone()),
            EvalState::Error(v) => return Err(v.clone()),
            EvalState::Evaluating(unit_id, root_expr) => {
                // Encountered an evaluation cycle, update the evaluation state
                let span = self.unit_map.get_unit(*unit_id).database.expr_nodes.spans[&root_expr];
                let err = Spanned::new(ConstError::EvalCycle, span);

                self.eval_state[expr.id] = EvalState::Error(err.clone());
                return Err(err);
            }
            EvalState::Unevaluated(unit_id, root_expr) => (*unit_id, *root_expr),
        };

        let result = self.do_eval_expr(unit_id, root_expr, expr);

        if let Err(err) = &result {
            // Update the eval state with the corresponding error
            self.eval_state[expr.id] = EvalState::Error(err.clone());
        }

        result
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

            Spanned::new(ConstError::NoConstExpr, span)
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
        self.eval_state[const_expr.id] = EvalState::Evaluating(unit_id, root_expr);

        // TODO: Feed in restrictions from somewhere
        let allow_64bit_ops = false;

        // Do the actual evaluation, as a stack maching
        let mut eval_stack = vec![Eval::Expr(root_expr)];
        let mut operand_stack = vec![];
        let unit = &self.unit_map[unit_id];

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
                        expr::Literal::Boolean(_v) => todo!(),
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
                expr::Expr::Name(_expr) => {
                    // TODO: Deal with name exprs
                    // May or may not reference a constant expression
                    todo!()
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
        self.eval_state[const_expr.id] = EvalState::Value(result.clone());

        Ok(result)
    }
}

impl fmt::Debug for InnerCtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InnerCtx")
            .field("eval_state", &self.eval_state)
            .field("var_to_expr", &self.var_to_expr)
            .finish()
    }
}

/// Evaluation state of a constant expression
enum EvalState {
    /// The expression has not been evaluated yet
    Unevaluated(UnitId, expr::ExprIdx),
    /// The expression is in the process of being evaluated
    Evaluating(UnitId, expr::ExprIdx),
    /// The expression has been evaluated to a valid value
    Value(ConstValue),
    /// The expression has been evaluated, but not to a valid value
    Error(Spanned<ConstError>),
}

impl fmt::Debug for EvalState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalState::Unevaluated(u, v) => {
                f.write_fmt(format_args!("Unevaluated({:?}, {:?})", u, v))
            }
            EvalState::Evaluating(u, v) => {
                f.write_fmt(format_args!("Evaluating({:?}, {:?})", u, v))
            }
            EvalState::Value(v) => f.write_fmt(format_args!("Value({:?})", v)),
            EvalState::Error(v) => f.write_fmt(format_args!("Error({:?})", v)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ConstOp {
    // Binary operations
    Add,
    Sub,
    Mul,
    Div,
    RealDiv,
    Mod,
    Rem,
    Exp,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    NotEqual,
    Imply,
    // Unary operations
    Not,
    Identity,
    Negate,
}

impl ConstOp {
    fn evaluate(
        &self,
        operand_stack: &mut Vec<ConstValue>,
        allow_64bit_ops: bool,
    ) -> Result<ConstValue, ConstError> {
        match self {
            ConstOp::Add => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs + rhs {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_add(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::Sub => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs - rhs {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_sub(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::Mul => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        match lhs * rhs {
                            v if v.is_infinite() => Err(ConstError::RealOverflow),
                            v => Ok(ConstValue::Real(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_mul(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::Div => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                match (lhs, rhs) {
                    (lhs @ ConstValue::Real(_), rhs) | (lhs, rhs @ ConstValue::Real(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_real()?, rhs.cast_into_real()?);

                        // Divide & truncate, then convert to an integer
                        match (lhs / rhs).trunc() {
                            _ if rhs == 0.0 => Err(ConstError::DivByZero),
                            v => ConstInt::from_signed_real(v, allow_64bit_ops)
                                .map(|v| ConstValue::Integer(v)),
                        }
                    }
                    (lhs @ ConstValue::Integer(_), rhs) | (lhs, rhs @ ConstValue::Integer(_)) => {
                        let (lhs, rhs) = (lhs.cast_into_int()?, rhs.cast_into_int()?);
                        lhs.checked_div(rhs).map(|v| ConstValue::Integer(v))
                    }
                }
            }
            ConstOp::RealDiv => todo!(),
            ConstOp::Mod => todo!(),
            ConstOp::Rem => todo!(),
            ConstOp::Exp => todo!(),
            ConstOp::And => todo!(),
            ConstOp::Or => todo!(),
            ConstOp::Xor => todo!(),
            ConstOp::Shl => todo!(),
            ConstOp::Shr => todo!(),
            ConstOp::Less => todo!(),
            ConstOp::LessEq => todo!(),
            ConstOp::Greater => todo!(),
            ConstOp::GreaterEq => todo!(),
            ConstOp::Equal => todo!(),
            ConstOp::NotEqual => todo!(),
            ConstOp::Imply => todo!(),
            ConstOp::Not => todo!(),
            ConstOp::Identity => {
                // Rhs must be a number
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    rhs @ ConstValue::Integer(_) => Ok(rhs),
                    rhs @ ConstValue::Real(_) => Ok(rhs),
                }
            }
            ConstOp::Negate => {
                let rhs = operand_stack.pop().unwrap();

                match rhs {
                    ConstValue::Integer(v) => v.negate().map(|v| ConstValue::Integer(v)),
                    ConstValue::Real(v) => Ok(ConstValue::Real(-v)),
                }
            }
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

/// Constant Integer representation
#[derive(Debug, Clone, Copy)]
pub struct ConstInt {
    /// invariant: must be representable with the given `sign` and `magnitude`
    magnitude: u64,
    sign: IntSign,
    width: IntWidth,
}

impl ConstInt {
    /// Constructs a new integer constant from an unsigned value
    ///
    /// ## Parameters
    /// - `value`: The unsigned value of the corresponding integer constant
    /// - `allow_64bit_values`: If 64-bit values can be constructed from applying
    ///   operations to this integer constant
    pub fn from_unsigned(value: u64, allow_64bit_ops: bool) -> Result<Self, ConstError> {
        let width = if allow_64bit_ops {
            // Allow 64-bit operations, any value is allowed
            IntWidth::As64
        } else {
            // Apply only 32-bit operations
            if value > u32::MAX as u64 {
                // Already overflowing
                return Err(ConstError::IntOverflow);
            }

            IntWidth::As32
        };

        Ok(Self {
            sign: IntSign::Positive,
            magnitude: value,
            width,
        })
    }

    /// Constructs a new integer constant from a signed floating point value
    ///
    /// ## Parameters
    /// - `value`: The signed floating point value of the corresponding integer constant
    /// - `allow_64bit_values`: If 64-bit values can be constructed from applying
    ///   operations to this integer constant
    pub fn from_signed_real(value: f64, allow_64bit_ops: bool) -> Result<Self, ConstError> {
        let width = if allow_64bit_ops {
            // Allow 64-bit operations, check against u64 bounds
            if (value.is_sign_negative() && value < i64::MIN as f64)
                || (value.is_sign_positive() && value > u64::MAX as f64)
            {
                // Already overflowing
                return Err(ConstError::IntOverflow);
            }

            IntWidth::As64
        } else {
            // Apply only 32-bit operations
            if (value.is_sign_negative() && value < i32::MIN as f64)
                || (value.is_sign_positive() && value > u32::MAX as f64)
            {
                // Already overflowing
                return Err(ConstError::IntOverflow);
            }

            IntWidth::As32
        };

        let (magnitude, sign) = {
            let magnitude = value.abs().trunc() as u64;

            if magnitude == 0 {
                // Always +0
                (0, IntSign::Positive)
            } else {
                let sign = if value.is_sign_negative() {
                    IntSign::Negative
                } else {
                    IntSign::Positive
                };

                (magnitude, sign)
            }
        };

        Ok(Self {
            sign,
            magnitude,
            width,
        })
    }

    /// Converts the `ConstInt` into the corresponding `u32` value.
    ///
    /// ## Returns
    /// Returns `Some(u32)` if the `ConstInt` is a positive integer (including zero)
    /// and is actually representable as a u32, or `None` otherwise.
    pub fn into_u32(self) -> Option<u32> {
        match self.sign {
            IntSign::Positive => self.magnitude.try_into().ok(),
            IntSign::Negative => None,
        }
    }

    /// Converts the `ConstInt` into the corresponding `u64` value.
    ///
    /// ## Returns
    /// Returns `Some(u64)` if the `ConstInt` is a positive integer (including zero),
    /// or `None` otherwise.
    pub fn into_u64(self) -> Option<u64> {
        match self.sign {
            IntSign::Positive => Some(self.magnitude),
            IntSign::Negative => None,
        }
    }

    /// Converts the `ConstInt` into the corresponding `f64` value.
    pub fn into_f64(self) -> f64 {
        match self.sign {
            IntSign::Positive => self.magnitude as f64,
            IntSign::Negative => -(self.magnitude as f64),
        }
    }

    /// Checked integer addition.
    /// Computes `self + rhs`, returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_add(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // Potential signs:
        // lhs rhs    magnitude op
        // +   +   => +
        // -   +   => -
        // +   -   => -
        // -   -   => +
        let (value, new_sign) = if self.sign == rhs.sign {
            // Sign is the same as the original
            (self.magnitude.checked_add(rhs.magnitude), self.sign)
        } else {
            // Sign can change
            let (magnitude, sign) = {
                let (magnitude, wrapped) = self.magnitude.overflowing_sub(rhs.magnitude);

                if wrapped {
                    // Undo two's compliment
                    // Flip the sign
                    (!magnitude + 1, self.sign.negate())
                } else {
                    (magnitude, self.sign)
                }
            };

            (Some(magnitude), sign)
        };

        Self::check_overflow(value, new_sign, effective_width)
    }

    /// Checked integer subtraction.
    /// Computes `self - rhs`, returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_sub(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        // Reuse addition code
        self.checked_add(rhs.unchecked_negate())
    }

    /// Checked integer multiplication.
    /// Computes `self * rhs`, returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_mul(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // Potential signs:
        // lhs rhs    final sign
        // +   +   => +
        // -   +   => -
        // +   -   => -
        // -   -   => +

        // Magnitude is unaffected by the initial signs
        let value = self.magnitude.checked_mul(rhs.magnitude);

        let new_sign = if self.sign == rhs.sign {
            // Will always be positive
            IntSign::Positive
        } else {
            // Will always be negative
            IntSign::Negative
        };

        Self::check_overflow(value, new_sign, effective_width)
    }

    /// Checked integer division.
    /// Computes `self div rhs`, returning `Err(ConstError::DivByZero)` if `rhs == 0`.
    pub fn checked_div(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // Potential signs:
        // lhs rhs    final sign
        // +   +   => +
        // -   +   => -
        // +   -   => -
        // -   -   => +

        // Magnitude is unaffected by the initial signs
        let value = self
            .magnitude
            .checked_div(rhs.magnitude)
            .ok_or(ConstError::DivByZero)?;

        let new_sign = if self.sign == rhs.sign {
            // Will always be positive
            IntSign::Positive
        } else {
            // Will always be negative
            IntSign::Negative
        };

        Self::check_overflow(Some(value), new_sign, effective_width)
    }

    /// Negates the sign of the integer.
    /// Does nothing for a magnitude of 0.
    pub fn negate(self) -> Result<ConstInt, ConstError> {
        Self::check_overflow(Some(self.magnitude), self.sign.negate(), self.width)
    }

    /// Negates the sign of the integer.
    /// Does nothing for a magnitude of 0.
    ///
    /// Note: This can break the invariant that for a width of `As32` and sign of `Negative`,
    /// magnitude must be in the range `[0, 0x7FFFFFFF]`.
    /// The only allowed usage of this function is for negating the `rhs` in `checked_sub`
    fn unchecked_negate(mut self) -> Self {
        if self.magnitude > 0 {
            self.sign = self.sign.negate()
        }

        self
    }

    fn effective_width(lhs: IntWidth, rhs: IntWidth) -> IntWidth {
        // Potential widths
        // lhs rhs    effective width
        // 32  32  => 32
        // 64  32  => 32
        // 32  64  => 32
        // 64  64  => 64

        if lhs == IntWidth::As64 && lhs == rhs {
            IntWidth::As64
        } else {
            IntWidth::As32
        }
    }

    fn check_overflow(
        value: Option<u64>,
        new_sign: IntSign,
        effective_width: IntWidth,
    ) -> Result<ConstInt, ConstError> {
        let (magnitude, sign) = {
            let effective_magnitude = value.ok_or(ConstError::IntOverflow)?;

            if effective_magnitude == 0 {
                // `0` is always a "positive" number
                (0, IntSign::Positive)
            } else {
                // Keep the same magnitude
                (effective_magnitude, new_sign)
            }
        };

        // Check for overflow
        let overflowed = match effective_width {
            // [0, 0xFFFF_FFFF]
            IntWidth::As32 if sign.is_positive() => magnitude > u32::MAX as u64,
            // [0, 0x8000_0000]
            IntWidth::As32 if sign.is_negative() => magnitude > i32::MIN.unsigned_abs() as u64,
            // [0, 0xFFFFFFFF_FFFFFFFF] or all values of u64
            IntWidth::As64 if sign.is_positive() => false,
            // [0, 0x80000000_00000000] or all values of u64
            IntWidth::As64 if sign.is_negative() => magnitude > i64::MIN.unsigned_abs() as u64,
            // All cases already covered
            _ => unreachable!(),
        };

        if !overflowed {
            Ok(Self {
                magnitude,
                sign,
                width: effective_width,
            })
        } else {
            Err(ConstError::IntOverflow)
        }
    }
}

impl fmt::Display for ConstInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let magnitude = if f.alternate() {
            // Show the width
            let width = match self.width {
                IntWidth::As32 => "i32",
                IntWidth::As64 => "i64",
            };

            format!("{}{}", self.magnitude, width)
        } else {
            // Just the magnitude
            format!("{}", self.magnitude)
        };

        f.pad_integral(matches!(self.sign, IntSign::Positive), "", &magnitude)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntWidth {
    As32,
    As64,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntSign {
    Positive,
    Negative,
}

impl IntSign {
    fn negate(self) -> Self {
        match self {
            IntSign::Positive => IntSign::Negative,
            IntSign::Negative => IntSign::Positive,
        }
    }

    fn is_positive(self) -> bool {
        matches!(self, IntSign::Positive)
    }

    fn is_negative(self) -> bool {
        matches!(self, IntSign::Negative)
    }
}
