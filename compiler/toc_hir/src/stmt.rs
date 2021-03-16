//! Statement nodes
use la_arena::Idx;

use crate::{expr, symbol, ty};

pub type StmtIdx = Idx<Stmt>;

#[derive(Debug)]
pub enum Stmt {
    /// Combined representation for `const` and `var` declarations
    /// (disambiguated by `is_const`)
    ConstVar {
        is_pervasive: bool,
        is_register: bool,
        is_const: bool,
        names: Vec<symbol::DefId>,
        type_spec: Option<ty::TypeIdx>,
        init_expr: Option<expr::ExprIdx>,
    },
    // Type { .. },
    // Bind { .. },
    // Proc { .. },
    // Fcn { .. },
    // Process { .. },
    // External { .. },
    // Forward { .. },
    // Deferred { .. },
    // Body { .. },
    // Module { .. },
    // Class { .. },
    // Monitor { .. },
    /// Assignment statement
    /// (also includes compound assignments)
    Assign {
        /// Left hand side of an assignment expression
        lhs: expr::ExprIdx,
        op: AssignOp,
        rhs: expr::ExprIdx,
    },
    // Open { .. },
    // Close { .. },
    // Put { .. },
    // Get { .. },
    // Read { .. },
    // Write { .. },
    // Seek { .. },
    // Tell { .. },
    // For { .. },
    // Loop { .. },
    // Exit { .. },
    // If { .. },
    // Case { .. },
    /// Block statement (`begin ... end`)
    Block { stmts: Vec<StmtIdx> },
    // Invariant { .. }
    // Assert { .. }
    // Call { .. }
    // Return { .. }
    // Result { .. }
    // New { .. }
    // Free { .. }
    // Tag { .. }
    // Fork { .. }
    // Signal { .. }
    // Pause { .. }
    // Quit { .. }
    // Break { .. }
    // Checkedness { .. }
    // Pre { .. }
    // Init { .. }
    // Post { .. }
    // Handler { .. }
    // ???: Should the below be real stmts?
    // Inherit { .. }
    // Implement { .. }
    // ImplementBy { .. }
    // Import { .. }
    // Export { .. }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignOp {
    /// Plain assignment
    None,
    /// Addition / Set Union / String Concatenation (`+`)
    Add,
    /// Subtraction / Set Subtraction (`-`)
    Sub,
    /// Multiplication / Set Intersection (`*`)
    Mul,
    /// Integer Division (`div`)
    Div,
    /// Real Division (`/`)
    RealDiv,
    /// Modulo (`mod`)
    Mod,
    /// Remainder (`rem`)
    Rem,
    /// Exponentiation (`**`)
    Exp,
    /// Bitwise/boolean And (`and`)
    And,
    /// Bitwise/boolean Or (`or`)
    Or,
    /// Bitwise/boolean Exclusive-Or (`xor`)
    Xor,
    /// Logical Shift Left (`shl`)
    Shl,
    /// Logical Shift Right (`shr`)
    Shr,
    /// Material Implication (`=>`)
    Imply,
}

impl AssignOp {
    pub fn as_binary_op(self) -> Option<expr::BinaryOp> {
        let op = match self {
            AssignOp::Add => expr::BinaryOp::Add,
            AssignOp::Sub => expr::BinaryOp::Sub,
            AssignOp::Mul => expr::BinaryOp::Mul,
            AssignOp::Div => expr::BinaryOp::Div,
            AssignOp::RealDiv => expr::BinaryOp::RealDiv,
            AssignOp::Mod => expr::BinaryOp::Mod,
            AssignOp::Rem => expr::BinaryOp::Rem,
            AssignOp::Exp => expr::BinaryOp::Exp,
            AssignOp::And => expr::BinaryOp::And,
            AssignOp::Or => expr::BinaryOp::Or,
            AssignOp::Xor => expr::BinaryOp::Xor,
            AssignOp::Shl => expr::BinaryOp::Shl,
            AssignOp::Shr => expr::BinaryOp::Shr,
            AssignOp::Imply => expr::BinaryOp::Imply,
            _ => return None,
        };

        Some(op)
    }
}
