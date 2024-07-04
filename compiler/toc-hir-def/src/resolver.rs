//! Resolving names to definitions

// Conceptual model:
// A scope tree is a collection of scope branches,
// with one of the branches being the root.
// A scope branch is a contiguous run of scopes,
// which may be rooted at a parent branch's scope.
// A scope is a place where a declaration is defined in.
// ?: more than one declaration possible?

// Goal:
// Name ->

// advantage of a separate unbound use query: if output doesn't change, rest of the deps stay the same
// same if we have a separate scope query
// adding / removing statements & exprs means that uses is more volatile

// could do:
// generate scope tree
// generate use list based on scope tree (since scope tree changes less often than uses)

// Hmm, we'll need 2 sets of scope trees:
// - Item scope tree
// - Expr scope tree (oh no)
// Q: how do we augment an item scope tree with expression scopes?
//    - expr scopes are interleaved between definitions
// Q: would elaborating imports be a separate step?

use indexmap::IndexMap;

use crate::{
    body::{Body, BodyContents},
    expr::{self, LocalExpr},
    item::{self, HasItems, Item, Module, RootModule},
    stmt::{self, LocalStmt},
    Db, Symbol,
};

crate::arena_id_wrapper!(
    /// A branch of scopes within a [`ScopeTree`].
    pub struct ScopeBranch<Entry>(ScopeBranchData<Entry>);
    pub(crate) type ScopeBranchIndex<Entry> = Index;
);

crate::arena_id_wrapper!(
    /// A scope entry within a [`ScopeBranch`].
    pub struct ScopeLeaf<Entry>(ScopeLeafData<Entry>);
    pub(crate) type ScopeLeafIndex<Entry> = Index;
);

/// A reference to a specific scope within a [`ScopeTree`]
pub struct Scope<Entry> {
    pub branch: ScopeBranch<Entry>,
    pub leaf: ScopeLeaf<Entry>,
}

impl<Entry> Scope<Entry> {
    pub fn new(branch: ScopeBranch<Entry>, leaf: ScopeLeaf<Entry>) -> Self {
        Self { branch, leaf }
    }
}

impl<Entry> std::fmt::Debug for Scope<Entry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Scope")
            .field("branch", &self.branch)
            .field("scope", &self.leaf)
            .finish()
    }
}

impl<Entry> Clone for Scope<Entry> {
    fn clone(&self) -> Self {
        Self {
            branch: self.branch.clone(),
            leaf: self.leaf.clone(),
        }
    }
}

impl<Entry> Copy for Scope<Entry> {}

impl<Entry> PartialEq for Scope<Entry> {
    fn eq(&self, other: &Self) -> bool {
        self.branch == other.branch && self.leaf == other.leaf
    }
}

impl<Entry> Eq for Scope<Entry> {}

impl<Entry> std::hash::Hash for Scope<Entry> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.branch.hash(state);
        self.leaf.hash(state);
    }
}

pub struct ScopeBuilder<Entry> {
    branches: la_arena::Arena<ScopeBranchData<Entry>>,
    branch_stack: Vec<Scope<Entry>>,
    current: Scope<Entry>,
}

impl<Entry> ScopeBuilder<Entry> {
    pub fn new() -> Self {
        let mut root_branch = ScopeBranchData::new(None);
        let root_scope = root_branch.add_scope();

        let mut branches = la_arena::Arena::new();
        let root_branch = ScopeBranch(branches.alloc(root_branch));

        Self {
            branches,
            branch_stack: vec![],
            current: Scope {
                branch: root_branch,
                leaf: root_scope,
            },
        }
    }

    pub fn fork_branch(&mut self, in_branch: impl FnOnce(&mut Self)) {
        let mut new_branch = ScopeBranchData::new(Some(self.current.branch));
        let new_branch_root = new_branch.add_scope();
        let new_branch = ScopeBranch(self.branches.alloc(new_branch));

        let current_branch = &mut self.branches[self.current.branch.0];
        let branch_anchor_scope = current_branch.add_scope();
        self.branches[self.current.branch.0]
            .child_branches
            .insert(branch_anchor_scope.0, new_branch.0);

        let old = std::mem::replace(&mut self.current, Scope::new(new_branch, new_branch_root));
        self.branch_stack.push(old);

        in_branch(self);

        self.current = self.branch_stack.pop().unwrap();
    }

    pub fn current_scope(&self) -> Scope<Entry> {
        self.current
    }
}

pub struct ScopeTree<Entry> {
    root: ScopeBranch<Entry>,
    branches: la_arena::Arena<ScopeBranch<Entry>>,
}

#[derive(Default)]
pub struct ScopeBranchData<Entry> {
    parent: Option<ScopeBranch<Entry>>,
    scopes: la_arena::Arena<ScopeLeafData<Entry>>,
    child_branches: la_arena::ArenaMap<ScopeLeafIndex<Entry>, ScopeBranchIndex<Entry>>,
}

impl<Entry> ScopeBranchData<Entry> {
    fn new(parent: Option<ScopeBranch<Entry>>) -> Self {
        Self {
            parent,
            scopes: Default::default(),
            child_branches: Default::default(),
        }
    }

    fn add_scope(&mut self) -> ScopeLeaf<Entry> {
        ScopeLeaf(self.scopes.alloc(Default::default()))
    }
}

pub struct ScopeLeafData<Entry> {
    declarations: IndexMap<Symbol, Entry>,
}

impl<Entry> Default for ScopeLeafData<Entry> {
    fn default() -> Self {
        Self {
            declarations: Default::default(),
        }
    }
}

// --- Item-level scope-tree (all items) --- //

type ItemScopeBuilder = ScopeBuilder<Item>;

pub fn root_module_item_scopes(db: &dyn Db, module: RootModule) {
    let mut builder = ItemScopeBuilder::new();
    let it = module.nested_items(db);

    for it in &**it {
        match it {
            item::NestedItem::Item(item) => {
                // declare it!
            }
            item::NestedItem::Nested(block_items) => {
                // fork & go!
            }
        }
    }
}

pub fn module_item_scopes(db: &dyn Db, module: Module) {}

// --- Expr-level scope-tree (all items + imports + exprs) --- //

pub enum ExprDef {
    Item(Item),
}

type ExprScopeBuilder = ScopeBuilder<ExprDef>;

pub fn expr_scopes(db: &dyn Db, body: Body) {
    let mut ctx = ExprScopeCtx {
        db,
        contents: &body.contents(db),
        scope_builder: ScopeBuilder::new(),
        unbound_uses: vec![],
    };

    ctx.collect_stmt_list(&*body.top_level_stmts(db));

    // for &stmt in body.top_level_stmts(db).iter() {
    //     ctx.collect_stmt(stmt);
    // }
}

struct NameUse(LocalExpr);

impl NameUse {
    fn new(contents: &BodyContents, expr: LocalExpr) -> Option<Self> {
        let expr::Expr::Name(_) = contents.expr(expr) else {
            return None;
        };

        Some(Self(expr))
    }

    fn as_expr<'contents>(&self, contents: &'contents BodyContents) -> &'contents expr::Name {
        let expr::Expr::Name(name) = contents.expr(self.0) else {
            unreachable!();
        };

        name
    }
}

struct ExprScopeCtx<'db> {
    db: &'db dyn Db,
    contents: &'db BodyContents,
    scope_builder: ExprScopeBuilder,
    unbound_uses: Vec<NameUse>,
}

impl<'db> ExprScopeCtx<'db> {
    fn collect_stmt_list(&mut self, ids: &[LocalStmt]) {
        for &stmt in ids {
            // TODO: inject item levels into expr scope
            self.collect_stmt(stmt);
        }

        // TODO: inject item-level defs into expr scope
    }

    fn collect_stmt(&mut self, id: LocalStmt) {
        match self.contents.stmt(id) {
            // Declares an item
            stmt::Stmt::InitializeConstVar(_, init) => {
                //
            }
            stmt::Stmt::InitializeBindItem(_, init) => {
                //
            }
            //
            stmt::Stmt::For(_) => todo!(),
            stmt::Stmt::Loop(_) => todo!(),
            stmt::Stmt::Exit(_) => todo!(),
            stmt::Stmt::If(_) => todo!(),
            stmt::Stmt::Case(_) => todo!(),
            stmt::Stmt::Block(_) => todo!(),

            // Collect constraints from subexprs
            stmt::Stmt::Assign(stmt) => {
                self.collect_expr(stmt.lhs);
                self.collect_expr(stmt.rhs);
            }
            stmt::Stmt::Put(stmt) => {
                if let Some(expr) = stmt.stream_num {
                    self.collect_expr(expr);
                }

                for items in &*stmt.items {
                    let stmt::Skippable::Item(item) = items else {
                        continue;
                    };

                    self.collect_expr(item.expr);
                    if let Some(expr) = item.opts.width() {
                        self.collect_expr(expr);
                    }
                    if let Some(expr) = item.opts.precision() {
                        self.collect_expr(expr);
                    }
                    if let Some(expr) = item.opts.exponent_width() {
                        self.collect_expr(expr);
                    }
                }
            }
            stmt::Stmt::Get(stmt) => {
                if let Some(expr) = stmt.stream_num {
                    self.collect_expr(expr);
                }

                for items in &*stmt.items {
                    let stmt::Skippable::Item(item) = items else {
                        continue;
                    };

                    self.collect_expr(item.expr);
                    if let stmt::GetWidth::Chars(expr) = item.width {
                        self.collect_expr(expr);
                    }
                }
            }
            stmt::Stmt::Call(stmt) => {
                self.collect_expr(stmt.lhs);

                if let Some(args) = stmt.arguments.as_deref() {
                    for &arg in args {
                        self.collect_expr(arg);
                    }
                }
            }
            stmt::Stmt::Return(_) => {}
            stmt::Stmt::Result(stmt) => {
                self.collect_expr(stmt.expr);
            }
        }
    }

    fn collect_expr(&mut self, id: LocalExpr) {
        match self.contents.expr(id) {
            expr::Expr::Missing | expr::Expr::Literal(_) | expr::Expr::All => (),

            expr::Expr::Name(_) => {
                // make_constraint: resolve (name) in (current_scope)
            }
            expr::Expr::Field(field) => {
                // make_constraint: resolve (field.field) in scope_of(field.expr)
            }

            // Collect constraints from subexprs
            expr::Expr::Init(_) => {
                // FIXME: These are resolved in the local scope, but they may be const-evaluatable?
                todo!()
            }
            expr::Expr::Binary(expr) => {
                self.collect_expr(expr.lhs);
                self.collect_expr(expr.rhs);
            }
            expr::Expr::Unary(expr) => {
                self.collect_expr(expr.rhs);
            }
            expr::Expr::Range(expr) => {
                if let Some(expr) = expr.start.expr() {
                    self.collect_expr(expr);
                }
                if let Some(expr) = expr.end.and_then(|end| end.expr()) {
                    self.collect_expr(expr);
                }
            }
            expr::Expr::Deref(expr) => {
                self.collect_expr(expr.rhs);
            }
            expr::Expr::Call(expr) => {
                self.collect_expr(expr.lhs);

                for &arg in &*expr.arguments {
                    self.collect_expr(arg);
                }
            }
        }
    }
}
