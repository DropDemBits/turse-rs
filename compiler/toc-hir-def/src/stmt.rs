//! Statement nodes

use toc_hir_expand::SemanticLoc;
use toc_salsa_collections::arena::SalsaArenaMap;
use toc_syntax::ast;

use crate::{
    Symbol,
    body::{Body, ModuleBlock},
    expr,
};

crate::arena_id_wrapper!(
    /// A [`Body`] local reference to a statement.
    ///
    /// [`Body`]: crate::body::Body
    pub struct LocalStmt<'db>(Stmt<'db>);
    /// Alias for the stmt arena index
    pub(crate) type StmtIndex<'db> = Index;
);

impl<'db> LocalStmt<'db> {
    pub fn in_body(self, body: Body<'db>) -> StmtId<'db> {
        StmtId(body, self)
    }
}

/// Uniquely identifies a statement within a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub struct StmtId<'db>(Body<'db>, LocalStmt<'db>);

impl<'db> StmtId<'db> {
    pub fn with_stmt(self, stmt: LocalStmt<'db>) -> Self {
        Self(self.0, stmt)
    }

    pub fn body(self) -> Body<'db> {
        self.0
    }

    pub fn stmt(self) -> LocalStmt<'db> {
        self.1
    }
}

pub type StmtMap<'db, V> = SalsaArenaMap<StmtIndex<'db>, V>;

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum Stmt<'db> {
    /// Initialize the given [`ast::ConstVarDecl`] at this point
    InitializeConstVar(
        SemanticLoc<'db, ast::ConstVarDeclName>,
        expr::LocalExpr<'db>,
    ),
    /// Initialize the given [`ast::BindItem`] at this point
    InitializeBindItem(SemanticLoc<'db, ast::BindItem>, expr::LocalExpr<'db>),
    /// Assignment statement
    /// (also includes compound assignments)
    Assign(Assign<'db>),
    // Open { .. },
    // Close { .. },
    /// Put Statement
    Put(Put<'db>),
    /// Get Statement
    Get(Get<'db>),
    // Read { .. },
    // Write { .. },
    // Seek { .. },
    // Tell { .. },
    /// For-loop statement
    For(For<'db>),
    /// Loop statement
    Loop(Loop<'db>),
    /// Exit statement
    Exit(Exit<'db>),
    /// If statement
    If(If<'db>),
    /// Case statement
    Case(Case<'db>),
    /// Block statement (`begin ... end`)
    Block(Block<'db>),
    // Invariant { .. }
    // Assert { .. }
    // Calling expression, in statement position
    Call(StmtCall<'db>),
    /// Return statement (`return`, for `procedure`s and `process`es)
    Return(Return),
    /// Result statement (`result (expr)`, only for `function`s)
    Result(Result<'db>),
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

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Assign<'db> {
    /// Left hand side of an assignment expression
    pub lhs: expr::LocalExpr<'db>,
    /// Operation used for assignment
    pub op: Option<expr::BinaryOp>,
    /// Right hand side of an assignment expression
    pub rhs: expr::LocalExpr<'db>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Put<'db> {
    /// Stream handle to put the text on.
    /// If absent, should be put on the `stdout` stream.
    pub stream_num: Option<expr::LocalExpr<'db>>,
    /// The items to put out on the stream.
    pub items: Vec<Skippable<PutItem<'db>>>,
    /// If a newline is appended after the all of the put items
    pub append_newline: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Get<'db> {
    /// Stream handle to get text from.
    /// If absent, should be fetched from the `stdin` stream.
    pub stream_num: Option<expr::LocalExpr<'db>>,
    /// The items to get from the stream.
    pub items: Vec<Skippable<GetItem<'db>>>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct For<'db> {
    /// If the for-loop has the `decreasing` modifier on it
    pub is_decreasing: bool,
    /// Name of the optional counter variable
    pub counter_def: Option<Symbol>,
    /// Bounds of the for-loop
    pub bounds: ForBounds<'db>,
    /// Optional `by ...` expression, to change the counter delta
    pub step_by: Option<expr::LocalExpr<'db>>,
    /// Body of the for-loop
    pub stmts: Vec<LocalStmt<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum ForBounds<'db> {
    /// Bounds of the for-loop are implied by the type referenced by this reference
    Implicit(expr::LocalExpr<'db>),
    /// Bounds of the are explicitly laid out in the `from` and `to` (respectively) expressions
    Full(expr::LocalExpr<'db>, expr::LocalExpr<'db>),
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Loop<'db> {
    pub stmts: Vec<LocalStmt<'db>>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Exit<'db> {
    pub when_condition: Option<expr::LocalExpr<'db>>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct If<'db> {
    /// Condition expression
    pub condition: expr::LocalExpr<'db>,
    /// True branch, executed if the condition is true
    pub true_branch: LocalStmt<'db>,
    /// Optional false branch
    pub false_branch: FalseBranch<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum FalseBranch<'db> {
    ElseIf(LocalStmt<'db>),
    Else(LocalStmt<'db>),
    None,
}

impl<'db> FalseBranch<'db> {
    /// Extracts the [`StmtId`], if there is one.
    pub fn stmt(self) -> Option<LocalStmt<'db>> {
        match self {
            FalseBranch::ElseIf(id) | FalseBranch::Else(id) => Some(id),
            FalseBranch::None => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Case<'db> {
    pub discriminant: expr::LocalExpr<'db>,
    pub arms: Vec<CaseArm<'db>>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct CaseArm<'db> {
    pub selectors: CaseSelector<'db>,
    pub stmts: Vec<LocalStmt<'db>>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum CaseSelector<'db> {
    /// Default selection arm
    Default,
    /// Arm is selected if the discriminant is equal to any of these expressions
    Exprs(Vec<expr::LocalExpr<'db>>),
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Block<'db> {
    pub module_block: Option<ModuleBlock<'db>>,
    pub kind: BlockKind,
    pub stmts: Box<[LocalStmt<'db>]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockKind {
    /// A regular block statement (`begin ... end`).
    Normal,
}

/// A generic type representing anything skippable.
/// Only used for text I/O statements (`PutStmt` & `GetStmt`).
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum Skippable<T: salsa::Update> {
    /// This I/O item is skipped.
    Skip,
    /// This I/O item is actually used.
    Item(T),
}

/// A single put item, as part of a put statement.
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct PutItem<'db> {
    /// The expression to be put.
    pub expr: expr::LocalExpr<'db>,
    /// Extra display options
    pub opts: PutOpts<'db>,
}

/// Extra display options for `put` items
///
/// If any later argument is present, then the earlier fields are
/// guaranteed to also be present (e.g. if `precision` was specified, then
/// `width` is also guaranteed to be specified, but not `exponent_width`).
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum PutOpts<'db> {
    // /// `put` item with no args
    None,
    /// `put` item with 1 arg
    WithWidth {
        /// The minimum printing width.
        width: expr::LocalExpr<'db>,
    },
    /// `put` item with 2 args
    WithPrecision {
        /// The minimum printing width.
        width: expr::LocalExpr<'db>,
        /// The amount of decimals put for `real*` types.
        precision: expr::LocalExpr<'db>,
    },
    /// `put` item with 3 args
    WithExponentWidth {
        /// The minimum printing width.
        width: expr::LocalExpr<'db>,
        /// The amount of decimals put for `real*` types.
        precision: expr::LocalExpr<'db>,
        /// The minimum printing width for exponents of `real*` types.
        exponent_width: expr::LocalExpr<'db>,
    },
}

impl<'db> PutOpts<'db> {
    pub fn width(&self) -> Option<expr::LocalExpr<'db>> {
        match self {
            PutOpts::None => None,
            PutOpts::WithWidth { width }
            | PutOpts::WithPrecision { width, .. }
            | PutOpts::WithExponentWidth { width, .. } => Some(*width),
        }
    }

    pub fn precision(&self) -> Option<expr::LocalExpr<'db>> {
        match self {
            PutOpts::None | PutOpts::WithWidth { .. } => None,
            PutOpts::WithPrecision { precision, .. }
            | PutOpts::WithExponentWidth { precision, .. } => Some(*precision),
        }
    }

    pub fn exponent_width(&self) -> Option<expr::LocalExpr<'db>> {
        match self {
            PutOpts::None | PutOpts::WithWidth { .. } | PutOpts::WithPrecision { .. } => None,
            PutOpts::WithExponentWidth { exponent_width, .. } => Some(*exponent_width),
        }
    }
}

/// A get item.
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct GetItem<'db> {
    /// The expression to get.
    /// Must be a reference expression.
    pub expr: expr::LocalExpr<'db>,
    /// The amount of text to extract.
    pub width: GetWidth<'db>,
}

/// The fetch width of a get item.
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum GetWidth<'db> {
    /// Fetch a space delimited portion of text.
    Token,
    /// Fetch a newline terminated portion of text.
    Line,
    /// Fetch a specific amount of characters.
    Chars(expr::LocalExpr<'db>),
}

/// Calling statement
#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct StmtCall<'db> {
    /// Reference to the calling expression
    pub lhs: expr::LocalExpr<'db>,
    /// Arguments to the call, which may not be present
    pub arguments: Option<expr::ArgList<'db>>,
}

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Return;

#[derive(Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct Result<'db> {
    pub expr: expr::LocalExpr<'db>,
}
