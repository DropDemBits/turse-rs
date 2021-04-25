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
    /// Untyped integer value
    Int(i128),
}

impl ConstValue {
    pub fn into_integer(self) -> Result<i128, ConstError> {
        match self {
            ConstValue::Int(v) => Ok(v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstError {
    // Traversal errors
    /// Encountered an evaluation cycle
    EvalCycle,
    /// Missing expression operand
    MissingExpr,
    /// Not a valid const eval operation
    NotConstOp(toc_span::TextRange),
    /// No const expr is associated with this identifer
    NoConstExpr,

    // Computation errors
    /// Wrong operand type in eval expression
    WrongType,
    /// Integer overflow
    IntOverflow,
    /// Division by zero
    DivByZero,
}

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
    pub fn eval_expr(&self, expr: ConstExpr) -> Result<ConstValue, ConstError> {
        // TODO: Try to look for a cached result before entering the actual computation

        let mut inner = self.inner.write().unwrap();
        inner.eval_expr(expr)
    }

    /// Evaluates the value of a constant variable
    pub fn eval_var(&self, var: GlobalDefId) -> Result<ConstValue, ConstError> {
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

    fn eval_expr(&mut self, expr: ConstExpr) -> Result<ConstValue, ConstError> {
        // Lookup the initial state of the expression
        let (unit_id, root_expr) = match &self.eval_state[expr.id] {
            // Give cached value
            EvalState::Value(v) => return Ok(v.clone()),
            EvalState::Error(v) => return Err(v.clone()),
            EvalState::Evaluating => {
                // Encountered an evaluation cycle, update the evaluation state
                self.eval_state[expr.id] = EvalState::Error(ConstError::EvalCycle);
                return Err(ConstError::EvalCycle);
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

    fn eval_var(&mut self, var: GlobalDefId) -> Result<ConstValue, ConstError> {
        // TODO: Handle evaluation restrictions
        let const_expr = *self.var_to_expr.get(&var).ok_or(ConstError::NoConstExpr)?;
        self.eval_expr(const_expr)
    }

    fn do_eval_expr(
        &mut self,
        unit_id: UnitId,
        root_expr: expr::ExprIdx,
        const_expr: ConstExpr,
    ) -> Result<ConstValue, ConstError> {
        #[derive(Debug)]
        enum Eval {
            Expr(expr::ExprIdx),
            Op(ConstOp),
        }

        // Unevaluated, evaluate the expression
        // Update the evaluation state to catch any evaluation cycles
        self.eval_state[const_expr.id] = EvalState::Evaluating;

        // Do the actual evaluation, as a stack maching
        let mut eval_stack = vec![Eval::Expr(root_expr)];
        let mut operand_stack = vec![];
        let unit = &self.unit_map[unit_id];

        loop {
            eprintln!("> {:?}", eval_stack);
            eprintln!("? {:?}", operand_stack);

            let local_expr = match eval_stack.pop() {
                Some(Eval::Expr(expr)) => expr,
                Some(Eval::Op(op)) => {
                    // Perform operation
                    let result = op.evaluate(&mut operand_stack)?;
                    operand_stack.push(result);
                    continue;
                }
                None => break,
            };

            // ???: How to deal with enum field accesses?
            // We don't have access to a TyCtx
            // - type with enum decl could add a const expr containing the entire enum def
            match &unit.database[local_expr] {
                expr::Expr::Missing => {
                    // Bail out
                    return Err(ConstError::MissingExpr);
                }
                expr::Expr::Literal(expr) => {
                    // ???: How to deal with 32-bit vs 64-bit integers?
                    // - Could yoink info from somewhere?
                    // - Only need to know if ops need to be done in either 32 or 64 bit mode

                    // Convert into a Constvalue
                    let operand = match expr {
                        expr::Literal::Int(_v) => todo!(),
                        expr::Literal::Nat(_v) => todo!(),
                        expr::Literal::Integer(v) => ConstValue::Int(*v as i128),
                        expr::Literal::Real(_v) => todo!(),
                        expr::Literal::CharSeq(_str) => todo!(),
                        expr::Literal::String(_str) => todo!(),
                        expr::Literal::Boolean(_v) => todo!(),
                    };

                    operand_stack.push(operand);
                }
                expr::Expr::Binary(expr) => {
                    // Push both expression operands and the operation
                    eval_stack.push(Eval::Op(expr.op.try_into()?));
                    eval_stack.push(Eval::Expr(expr.rhs));
                    eval_stack.push(Eval::Expr(expr.lhs));
                }
                expr::Expr::Unary(expr) => {
                    // Push expr operand & operator
                    eval_stack.push(Eval::Op(expr.op.try_into()?));
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
    Evaluating,
    /// The expression has been evaluated to a valid value
    Value(ConstValue),
    /// The expression has been evaluated, but not to a valid value
    Error(ConstError),
}

impl fmt::Debug for EvalState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalState::Unevaluated(u, v) => {
                f.write_fmt(format_args!("Unevaluated({:?}, {:?})", u, v))
            }
            EvalState::Evaluating => f.write_fmt(format_args!("Evaluating")),
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
    fn evaluate(&self, operand_stack: &mut Vec<ConstValue>) -> Result<ConstValue, ConstError> {
        match self {
            ConstOp::Add => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                let lhs = lhs.into_integer()?;
                let rhs = rhs.into_integer()?;
                let res = lhs.checked_add(rhs).ok_or(ConstError::IntOverflow)?;
                Ok(ConstValue::Int(res))
            }
            ConstOp::Sub => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                let lhs = lhs.into_integer()?;
                let rhs = rhs.into_integer()?;
                let res = lhs.checked_sub(rhs).ok_or(ConstError::IntOverflow)?;
                Ok(ConstValue::Int(res))
            }
            ConstOp::Mul => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                let lhs = lhs.into_integer()?;
                let rhs = rhs.into_integer()?;
                let res = lhs.checked_mul(rhs).ok_or(ConstError::IntOverflow)?;
                Ok(ConstValue::Int(res))
            }
            ConstOp::Div => {
                let rhs = operand_stack.pop().unwrap();
                let lhs = operand_stack.pop().unwrap();

                let lhs = lhs.into_integer()?;
                let rhs = rhs.into_integer()?;
                let res = lhs.checked_div(rhs).ok_or(ConstError::DivByZero)?;
                Ok(ConstValue::Int(res))
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
                let rhs = operand_stack.pop().unwrap();
                Ok(rhs)
            }
            ConstOp::Negate => todo!(),
        }
    }
}

impl TryFrom<Spanned<expr::BinaryOp>> for ConstOp {
    type Error = ConstError;

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
            _ => return Err(ConstError::NotConstOp(op.span())),
        })
    }
}

impl TryFrom<Spanned<expr::UnaryOp>> for ConstOp {
    type Error = ConstError;

    fn try_from(op: Spanned<expr::UnaryOp>) -> Result<Self, Self::Error> {
        match op.item() {
            expr::UnaryOp::Not => Ok(Self::Not),
            expr::UnaryOp::Identity => Ok(Self::Identity),
            expr::UnaryOp::Negate => Ok(Self::Negate),
        }
    }
}
