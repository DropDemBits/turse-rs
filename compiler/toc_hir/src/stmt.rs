//! Statement nodes
use toc_span::Spanned;

use crate::{expr, symbol, ty, HirId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub(crate) HirId);

impl From<StmtId> for HirId {
    fn from(id: StmtId) -> Self {
        id.0
    }
}

impl From<&StmtId> for HirId {
    fn from(id: &StmtId) -> Self {
        id.0
    }
}

#[derive(Debug)]
pub enum Stmt {
    /// Combined representation for `const` and `var` declarations
    /// (disambiguated by `is_const`)
    ConstVar(ConstVar),
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
    Assign(Assign),
    // Open { .. },
    // Close { .. },
    /// Put Statement
    Put(Put),
    /// Get Statement
    Get(Get),
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
    Block(Block),
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

#[derive(Debug)]
pub struct ConstVar {
    pub is_register: bool,
    pub is_const: bool,
    pub names: Vec<symbol::DefId>,
    pub tail: ConstVarTail,
}

#[derive(Debug)]
pub enum ConstVarTail {
    /// Only the type spec is specified
    TypeSpec(ty::TypeId),
    /// Only the init expr is specified
    InitExpr(expr::ExprId),
    /// Both the type spec and init expr are specified
    Both(ty::TypeId, expr::ExprId),
}

impl ConstVarTail {
    pub fn type_spec(&self) -> Option<ty::TypeId> {
        match self {
            ConstVarTail::TypeSpec(ty_spec) => Some(*ty_spec),
            ConstVarTail::InitExpr(_) => None,
            ConstVarTail::Both(ty_spec, _) => Some(*ty_spec),
        }
    }

    pub fn init_expr(&self) -> Option<expr::ExprId> {
        match self {
            ConstVarTail::TypeSpec(_) => None,
            ConstVarTail::InitExpr(init_expr) => Some(*init_expr),
            ConstVarTail::Both(_, init_expr) => Some(*init_expr),
        }
    }
}

#[derive(Debug)]
pub struct Assign {
    /// Left hand side of an assignment expression
    pub lhs: expr::ExprId,
    /// Operation performed between the arguments before assignment
    pub op: Spanned<AssignOp>,
    /// Right hand side of an assignment expression
    pub rhs: expr::ExprId,
}

#[derive(Debug)]
pub struct Put {
    /// Stream handle to put the text on.
    /// If absent, should be put on the `stdout` stream.
    pub stream_num: Option<expr::ExprId>,
    /// The items to put out on the stream.
    pub items: Vec<Skippable<PutItem>>,
    /// If a newline is appended after the all of the put items
    pub append_newline: bool,
}

#[derive(Debug)]
pub struct Get {
    /// Stream handle to get text from.
    /// If absent, should be fetched from the `stdin` stream.
    pub stream_num: Option<expr::ExprId>,
    /// The items to get from the stream.
    pub items: Vec<Skippable<GetItem>>,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
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
#[derive(Debug)]
pub struct PutItem {
    /// The expression to be put.
    pub expr: expr::ExprId,
    /// Extra display options
    pub opts: PutOpts,
}

/// Extra display options for `put` items
///
/// If any later argument is present, then the earlier fields are
/// guaranteed to also be present (e.g. if `precision` was specified, then
/// `width` is also guaranteed to be specified, but not `exponent_width`).
#[derive(Debug)]
pub enum PutOpts {
    // /// `put` item with no args
    None,
    /// `put` item with 1 arg
    WithWidth {
        /// The minimum printing width.
        width: expr::ExprId,
    },
    /// `put` item with 2 args
    WithPrecision {
        /// The minimum printing width.
        width: expr::ExprId,
        /// The amount of decimals put for `real*` types.
        precision: expr::ExprId,
    },
    /// `put` item with 3 args
    WithExponentWidth {
        /// The minimum printing width.
        width: expr::ExprId,
        /// The amount of decimals put for `real*` types.
        precision: expr::ExprId,
        /// The minimum printing width for exponents of `real*` types.
        exponent_width: expr::ExprId,
    },
}

impl PutOpts {
    pub fn width(&self) -> Option<expr::ExprId> {
        match self {
            PutOpts::None => None,
            PutOpts::WithWidth { width }
            | PutOpts::WithPrecision { width, .. }
            | PutOpts::WithExponentWidth { width, .. } => Some(*width),
        }
    }

    pub fn precision(&self) -> Option<expr::ExprId> {
        match self {
            PutOpts::None | PutOpts::WithWidth { .. } => None,
            PutOpts::WithPrecision { precision, .. }
            | PutOpts::WithExponentWidth { precision, .. } => Some(*precision),
        }
    }

    pub fn exponent_width(&self) -> Option<expr::ExprId> {
        match self {
            PutOpts::None | PutOpts::WithWidth { .. } | PutOpts::WithPrecision { .. } => None,
            PutOpts::WithExponentWidth { exponent_width, .. } => Some(*exponent_width),
        }
    }
}

/// A get item.
#[derive(Debug)]
pub struct GetItem {
    /// The expression to put.
    /// Must be a reference expression.
    pub expr: expr::ExprId,
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
    Chars(expr::ExprId),
}
