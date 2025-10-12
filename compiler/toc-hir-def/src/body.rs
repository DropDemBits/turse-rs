use std::{marker::PhantomData, ops::Index};

use rustc_hash::FxHashMap;
use toc_hir_expand::{ErasedSemanticLoc, UnstableSemanticLoc};
use toc_salsa_collections::arena::SalsaArena;
use toc_syntax::ast::{self, AstNode as _};

use crate::{
    Db, expr,
    item::{HasItems, ModuleBlock, ModuleLike},
    local, scope,
    stmt::{self, StmtId},
};

pub(crate) mod lower;
pub mod pretty;

/// Executable block of code
#[salsa::interned(debug)]
pub struct Body<'db> {
    origin: BodyOrigin<'db>,
}

/// Where a `Body` comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum BodyOrigin<'db> {
    /// Attached to a module-like item
    ModuleBody(ModuleLike<'db>),
    /// Attached to a function-like item
    FunctionBody(ErasedSemanticLoc<'db>),
}

/// Lowered body contents
#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct BodyContents<'db> {
    exprs: SalsaArena<expr::Expr<'db>>,
    stmts: SalsaArena<stmt::Stmt<'db>>,
    /// Local variable definitions.
    locals: SalsaArena<local::Local<'db>>,
    /// Which statements own a local.
    locals_owners: FxHashMap<local::LocalId<'db>, stmt::LocalStmt<'db>>,
    /// Local bindings defined within the body.
    local_bindings: scope::BodyBindings<'db>,
    /// Basic name scope queries (i.e. queries that don't depend on scope inference).
    queries: scope::ScopeQueries<'db>,

    top_level: Box<[StmtId<'db>]>,
    // `None` if the body's immediate block has no items, or if the body's origin owns the items.
    root_block: Option<ModuleBlock<'db>>,
}

impl<'db> Index<local::LocalId<'db>> for BodyContents<'db> {
    type Output = local::Local<'db>;

    fn index(&self, index: local::LocalId<'db>) -> &Self::Output {
        &self.locals[index.0]
    }
}

impl<'db> BodyContents<'db> {
    pub fn expr(&self, expr: expr::LocalExpr<'db>) -> &expr::Expr<'db> {
        &self.exprs[expr.0]
    }

    pub fn stmt(&self, stmt: stmt::LocalStmt<'db>) -> &stmt::Stmt<'db> {
        &self.stmts[stmt.0]
    }

    pub fn root_block(&self) -> Option<ModuleBlock<'db>> {
        self.root_block
    }
}

/// Spans of a body
#[derive(Debug, Default, PartialEq, Eq, salsa::Update, Hash)]
pub(crate) struct BodySpans<'db> {
    exprs: expr::ExprMap<'db, UnstableSemanticLoc<'db, ast::Expr>>,
    stmts: stmt::StmtMap<'db, UnstableSemanticLoc<'db, ast::Stmt>>,
}

#[salsa::tracked(debug)]
pub(crate) struct BodyLowerResult<'db> {
    #[tracked]
    #[returns(ref)]
    contents: BodyContents<'db>,
    #[tracked]
    #[returns(ref)]
    spans: BodySpans<'db>,
}

#[salsa::tracked]
impl<'db> Body<'db> {
    // Top-level stmts
    pub fn top_level_stmts(self, db: &'db dyn Db) -> &'db [StmtId<'db>] {
        &self.contents(db).top_level
    }

    /// Contains all of the statements and expressions within a body.
    pub fn contents(self, db: &'db dyn Db) -> &'db BodyContents<'db> {
        self.lower_contents(db).contents(db)
    }

    /// Resolutions for names that don't require looking outside of the body.
    pub fn local_resolved_names(self, db: &'db dyn Db) -> ResolvedNames<'db> {
        let contents = self.contents(db);
        let item_bindings = match self.origin(db) {
            BodyOrigin::ModuleBody(module_like) => Some(
                module_like
                    .child_items(db)
                    .item_bindings(db)
                    .map_bindings(scope::BodyBinding::from),
            ),
            BodyOrigin::FunctionBody(_) => None,
        };

        let resolvers: &[&dyn scope_trees::Resolver<_, _, _>] = match item_bindings.as_ref() {
            Some(item_bindings) => &[item_bindings, &contents.local_bindings],
            None => &[&contents.local_bindings],
        };

        let resolutions = contents.queries.resolve_all(resolvers);

        let scope_trees::ResolveList {
            resolved,
            unresolved,
            ambiguous: _,
        } = resolutions;

        let names = ResolvedNames::new(
            db,
            resolved
                .into_iter()
                .map(|(k, v)| (k, v.binding))
                .collect::<FxHashMap<_, _>>(),
            unresolved,
            (),
        );

        names
    }

    /// Fully resolved names, as well as definitely unresolved names.
    #[salsa::tracked]
    pub fn resolved_names(self, db: &'db dyn Db) -> ResolvedNames<'db> {
        self.local_resolved_names(db)
    }

    // Parts we wanna extract:
    // - Stmts + Exprs
    // - SpanMap<Expr> & SpanMap<Stmt>
    #[salsa::tracked]
    pub(crate) fn lower_contents(self, db: &'db dyn Db) -> BodyLowerResult<'db> {
        // FIXME: Accumulate errors
        let (contents, spans, _errors) = match self.origin(db) {
            BodyOrigin::ModuleBody(origin) => {
                let (origin_node, child_items) = match origin {
                    ModuleLike::Module(module) => {
                        (module.into_loc(db).into_erased(), module.child_items(db))
                    }
                    ModuleLike::RootModule(root_module) => (
                        root_module.root(db).into_erased(),
                        root_module.child_items(db),
                    ),
                };

                let file = origin_node.file();
                let Some(stmts) = origin_node
                    .to_node(db)
                    .descendants()
                    .find_map(ast::StmtList::cast)
                else {
                    unreachable!()
                };

                lower::module_body(db, self, file, stmts, child_items)
            }
            // Note that if we're an FnBody then we can have ConstVars as locals
            BodyOrigin::FunctionBody(_stmts) => todo!(),
        };

        BodyLowerResult::new(db, contents, spans)
    }
}

/// Resolved names within a body.
#[salsa::tracked(debug)]
pub struct ResolvedNames<'db> {
    #[tracked(returns(ref))]
    resolved: FxHashMap<scope::QueryKey<'db>, scope::BodyBinding<'db>>,
    #[tracked(returns(ref))]
    unresolved: Vec<scope::QueryKey<'db>>,
    #[tracked(returns(ref))]
    errors: (),
}

#[salsa::tracked]
impl<'db> ResolvedNames<'db> {
    #[salsa::tracked]
    pub fn binding_of(
        self,
        db: &'db dyn Db,
        query_key: scope::QueryKey<'db>,
    ) -> Option<scope::BodyBinding<'db>> {
        self.resolved(db).get(&query_key).copied()
    }
}

/// Arbitrary scope within a body.
///
/// May correspond to e.g. a block, or a local var declaraton.
#[salsa::tracked(constructor = __make_block_scope)]
#[derive(PartialOrd, Ord)]
pub struct BodyScope<'db> {
    unique: PhantomData<&'db ()>,
}

impl<'db> std::fmt::Debug for BodyScope<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use salsa::plumbing::AsId;

        write!(f, "BodyScope[{}]", self.as_id().index())
    }
}

impl<'db> BodyScope<'db> {
    pub(crate) fn new(db: &'db dyn Db) -> Self {
        Self::__make_block_scope(db, PhantomData)
    }
}
