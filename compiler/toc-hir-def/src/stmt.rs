//! Statement nodes

use toc_hir_expand::SemanticLoc;
use toc_syntax::ast;

use crate::{expr, Symbol};

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to a statement.
    ///
    /// [`Body`]: crate::body::Body
    pub struct StmtId(Stmt);
    /// Alias for the stmt arena index
    pub(crate) type StmtIndex = Index;
);

// impl StmtId {
//     pub fn in_body(self, body_id: body::BodyId) -> BodyStmt {
//         BodyStmt(body_id, self)
//     }
// }

// /// Uniquely identifies a statement within a package
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct BodyStmt(pub body::BodyId, pub StmtId);

// impl BodyStmt {
//     pub fn with_stmt(self, stmt: StmtId) -> Self {
//         BodyStmt(self.0, stmt)
//     }

//     pub fn body(self) -> BodyId {
//         self.0
//     }

//     pub fn stmt(self) -> StmtId {
//         self.1
//     }
// }

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    /// Initialize the given [`ast::ConstVar`] at this point
    InitializeConstVar(SemanticLoc<ast::ConstVarDecl>),
    /// Initialize the given [`ast::BindItem`] at this point
    InitializeBindItem(SemanticLoc<ast::BindItem>),
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
    /// For-loop statement
    For(For),
    /// Loop statement
    Loop(Loop),
    /// Exit statement
    Exit(Exit),
    /// If statement
    If(If),
    /// Case statement
    Case(Case),
    /// Block statement (`begin ... end`)
    Block(Block),
    // Invariant { .. }
    // Assert { .. }
    // Calling expression, in statement position
    Call(Call),
    /// Return statement (`return`, for `procedure`s and `process`es)
    Return(Return),
    /// Result statement (`result (expr)`, only for `function`s)
    Result(Result),
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
    // Likely not, since this info can be encoded inside of the associated defined type
    // Inherit { .. }
    // Implement { .. }
    // ImplementBy { .. }
    // Import { .. }
    // Export { .. }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assign {
    /// Left hand side of an assignment expression
    pub lhs: expr::ExprId,
    /// Right hand side of an assignment expression
    pub rhs: expr::ExprId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Put {
    /// Stream handle to put the text on.
    /// If absent, should be put on the `stdout` stream.
    pub stream_num: Option<expr::ExprId>,
    /// The items to put out on the stream.
    pub items: Vec<Skippable<PutItem>>,
    /// If a newline is appended after the all of the put items
    pub append_newline: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Get {
    /// Stream handle to get text from.
    /// If absent, should be fetched from the `stdin` stream.
    pub stream_num: Option<expr::ExprId>,
    /// The items to get from the stream.
    pub items: Vec<Skippable<GetItem>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct For {
    /// If the for-loop has the `decreasing` modifier on it
    pub is_decreasing: bool,
    /// Name of the optional counter variable
    pub counter_def: Option<Symbol>,
    /// Bounds of the for-loop
    pub bounds: ForBounds,
    /// Optional `by ...` expression, to change the counter delta
    pub step_by: Option<expr::ExprId>,
    /// Body of the for-loop
    pub stmts: Vec<StmtId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForBounds {
    /// Bounds of the for-loop are implied by the type referenced by this reference
    Implicit(expr::ExprId),
    /// Bounds of the are explicitly laid out in the `from` and `to` (respectively) expressions
    Full(expr::ExprId, expr::ExprId),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Loop {
    pub stmts: Vec<StmtId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Exit {
    pub when_condition: Option<expr::ExprId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct If {
    /// Condition expression
    pub condition: expr::ExprId,
    /// True branch, executed if the condition is true
    pub true_branch: StmtId,
    /// Optional false branch
    pub false_branch: FalseBranch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FalseBranch {
    ElseIf(StmtId),
    Else(StmtId),
    None,
}

impl FalseBranch {
    /// Extracts the [`StmtId`], if there is one.
    pub fn stmt(self) -> Option<StmtId> {
        match self {
            FalseBranch::ElseIf(id) | FalseBranch::Else(id) => Some(id),
            FalseBranch::None => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Case {
    pub discriminant: expr::ExprId,
    pub arms: Vec<CaseArm>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CaseArm {
    pub selectors: CaseSelector,
    pub stmts: Vec<StmtId>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CaseSelector {
    /// Default selection arm
    Default,
    /// Arm is selected if the discriminant is equal to any of these expressions
    Exprs(Vec<expr::ExprId>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub kind: BlockKind,
    pub stmts: Vec<StmtId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockKind {
    /// A regular block statement (`begin ... end`).
    Normal,
}

/// A generic type representing anything skippable.
/// Only used for text I/O statements (`PutStmt` & `GetStmt`).
#[derive(Debug, PartialEq, Eq)]
pub enum Skippable<T> {
    /// This I/O item is skipped.
    Skip,
    /// This I/O item is actually used.
    Item(T),
}

/// A single put item, as part of a put statement.
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
pub struct GetItem {
    /// The expression to get.
    /// Must be a reference expression.
    pub expr: expr::ExprId,
    /// The amount of text to extract.
    pub width: GetWidth,
}

/// The fetch width of a get item.
#[derive(Debug, PartialEq, Eq)]
pub enum GetWidth {
    /// Fetch a space delimited portion of text.
    Token,
    /// Fetch a newline terminated portion of text.
    Line,
    /// Fetch a specific amount of characters.
    Chars(expr::ExprId),
}

/// Calling statement
#[derive(Debug, PartialEq, Eq)]
pub struct Call {
    /// Reference to the calling expression
    pub lhs: expr::ExprId,
    /// Arguments to the call, which may not be present
    pub arguments: Option<expr::ArgList>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Return;

#[derive(Debug, PartialEq, Eq)]
pub struct Result {
    pub expr: expr::ExprId,
}
