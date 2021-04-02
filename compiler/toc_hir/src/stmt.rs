//! Statement nodes
use la_arena::Idx;

use crate::{expr, symbol, ty};

pub type StmtIdx = Idx<Stmt>;

#[derive(Debug)]
pub enum Stmt {
    /// Combined representation for `const` and `var` declarations
    /// (disambiguated by `is_const`)
    ConstVar {
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
        /// Operation performed between the arguments before assignment
        op: AssignOp,
        /// Right hand side of an assignment expression
        rhs: expr::ExprIdx,
    },
    // Open { .. },
    // Close { .. },
    /// Put Statement
    Put {
        /// Stream handle to put the text on.
        /// If absent, should be put on the `stdout` stream.
        stream_num: Option<expr::ExprIdx>,
        /// The items to put out on the stream.
        items: Vec<Skippable<PutItem>>,
        /// If a newline is appended after the all of the put items
        append_newline: bool,
    },
    /// Get Statement
    Get {
        /// Stream handle to get text from.
        /// If absent, should be fetched from the `stdin` stream.
        stream_num: Option<expr::ExprIdx>,
        /// The items to get from the stream.
        items: Vec<Skippable<GetItem>>,
    },
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

/// A generic type representing anything skippable.
/// Only used for text I/O statements (`PutStmt` & `GetStmt`).
#[derive(Debug)]
pub enum Skippable<T> {
    /// This I/O item is skipped.
    Skip,
    /// This I/O item is actually used.
    Item(T),
}

/// A single put item, as part of a put statement.
///
/// If any later optional field is a `Some`, then the earlier fields are
/// guaranteed to also be a `Some` (e.g. if `precision` was `Some`, then
/// `width` is also guaranteed to be `Some`, but not `exponent_width`).
#[derive(Debug)]
pub struct PutItem {
    /// The expression to be put.
    pub expr: expr::ExprIdx,
    /// The minimum printing width.
    pub width: Option<expr::ExprIdx>,
    /// The amount of decimials put for `real*` types.
    pub precision: Option<expr::ExprIdx>,
    /// The minimum printing width for exponents of `real*` types.
    pub exponent_width: Option<expr::ExprIdx>,
    // to maintain doc variant
    _priv: (),
}

impl PutItem {
    /// Constructs a new `PutItem` without any optional arguments.
    pub fn new(expr: expr::ExprIdx) -> Self {
        Self {
            expr,
            width: None,
            precision: None,
            exponent_width: None,
            _priv: (),
        }
    }

    /// Constructs a new `PutItem` with the `width` argument.
    pub fn with_width(expr: expr::ExprIdx, width: expr::ExprIdx) -> Self {
        Self {
            expr,
            width: Some(width),
            precision: None,
            exponent_width: None,
            _priv: (),
        }
    }

    /// Constructs a new `PutItem` with the `precision` argument, and any prerequesite arguments.
    pub fn with_precision(
        expr: expr::ExprIdx,
        width: expr::ExprIdx,
        precision: expr::ExprIdx,
    ) -> Self {
        Self {
            expr,
            width: Some(width),
            precision: Some(precision),
            exponent_width: None,
            _priv: (),
        }
    }

    /// Constructs a new `PutItem` with the `exponent_width` argument, and any prerequesite arguments.
    pub fn with_exponent_width(
        expr: expr::ExprIdx,
        width: expr::ExprIdx,
        precision: expr::ExprIdx,
        exponent_width: expr::ExprIdx,
    ) -> Self {
        Self {
            expr,
            width: Some(width),
            precision: Some(precision),
            exponent_width: Some(exponent_width),
            _priv: (),
        }
    }
}

/// A get item.
#[derive(Debug)]
pub struct GetItem {
    /// The expression to put.
    /// Must be a reference expression.
    pub expr: expr::ExprIdx,
    /// The amount of text to extract.
    pub width: GetWidth,
}

/// The fetch width of a get item.
#[derive(Debug)]
pub enum GetWidth {
    /// Fetch a space delimited portion of text.
    Token,
    /// Fetch a newline terminated portion of text.
    Line,
    /// Fetch a specific amount of characters.
    Chars(expr::ExprIdx),
}
