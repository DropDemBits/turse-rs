//! Resolution of name nodes to specific local defs

use toc_hir::{
    body::{BodyId, BodyKind},
    expr::{BodyExpr, ExprId, ExprKind, Name, RangeBound},
    item::{DefinedType, ItemId, ItemKind, QualifyAs, SubprogramExtra},
    library::Library,
    stmt::{BodyStmt, CaseSelector, FalseBranch, ForBounds, GetWidth, Skippable, StmtId, StmtKind},
    symbol::{DeclareKind, ForwardKind, LimitedKind, LocalDefId, ResolutionMap, Resolve, Symbol},
    ty::{ConstrainedEnd, Primitive, SeqLength, TypeId, TypeKind},
};
use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, SpanId};

use crate::scopes::{PervasiveTracker, ScopeKind, ScopeTracker};

/// Resolves bindings in a library, producing a [`ResolutionMap`]
pub(crate) fn resolve_defs(
    root_items: &[(FileId, ItemId)],
    library: &Library,
    pervasive_tracker: PervasiveTracker,
) -> CompileResult<ResolutionMap> {
    let mut ctx = ResolveCtx {
        library,
        resolves: Default::default(),
        scopes: ScopeTracker::new(pervasive_tracker),
        messages: Default::default(),
    };

    for (_, root_item) in root_items {
        ctx.resolve_item(*root_item);
    }

    CompileResult::new(ctx.resolves, ctx.messages.finish())
}

struct ResolveCtx<'lib> {
    library: &'lib Library,
    resolves: ResolutionMap,
    scopes: ScopeTracker,
    messages: MessageSink,
}

impl<'a> ResolveCtx<'a> {
    // Helper for using a symbol, handling undeclared & use reporting reporting
    fn use_sym(&mut self, name: Symbol, span: SpanId) -> Resolve {
        let resolve = match self.scopes.use_sym(name) {
            Some(def) => Resolve::Def(def),
            None => {
                if self.scopes.is_first_undecl_use(name) {
                    self.messages.error(
                        format!("`{name}` is undeclared"),
                        format!("no definitions of `{name}` are in scope"),
                        span.lookup_in(&self.library),
                    );
                }

                Resolve::Err
            }
        };

        if let Resolve::Def(def) = resolve {
            if let DeclareKind::LimitedDeclared(limited) = self.scopes.declare_kind(def) {
                match limited {
                    LimitedKind::PostCondition => {
                        // FIXME: If we're in a post condition, don't report this error
                        self.messages.error(
                            format!("`cannot use {name}` here"),
                            format!("`{name}` can only be used in a `post` statement"),
                            span.lookup_in(&self.library),
                        );
                    }
                }
            }
        }

        resolve
    }

    /// Introduces a definition into the current scope, handling redecleration with any existing defs
    fn introduce_def(&mut self, def_id: LocalDefId, kind: DeclareKind) {
        // Don't bother introducing unnamed defs, since they're not meant to be real
        if self.is_unnamed(def_id) {
            return;
        }

        let def_info = self.library.local_def(def_id);
        let name = def_info.name;
        let span = def_info.def_at;

        // Bring into scope
        let old_def = self.scopes.def_sym(name, def_id, kind);

        // Resolve any associated forward decls
        if let DeclareKind::Resolved(resolve_kind) = kind {
            let forward_list = self.scopes.take_resolved_forwards(name, resolve_kind);

            if let Some(forward_list) = forward_list {
                // Point all of these local defs to this one
                for forward_def in forward_list {
                    let kind = self.scopes.declare_kind_mut(forward_def).unwrap();

                    match kind {
                        DeclareKind::Forward(_, resolve_to) => *resolve_to = Some(def_id),
                        _ => unreachable!("not a forward def"),
                    }
                }
            }
        }

        if let Some(old_def) = old_def {
            // Report redeclares, specializing based on what kind of declaration it is
            let old_def_info = self.library.local_def(old_def);
            let old_declare_kind = self.scopes.declare_kind(old_def);

            let old_span = old_def_info.def_at.lookup_in(&self.library);
            let new_span = span.lookup_in(&self.library);

            // Just use the name from the old def for both, since by definition they are the same
            let name = old_def_info.name;

            match (old_declare_kind, kind) {
                (DeclareKind::Undeclared, _) | (_, DeclareKind::Undeclared) => {
                    // Always ok to declare over undeclared symbols
                }
                (DeclareKind::Forward(other_kind, _), DeclareKind::Forward(this_kind, _))
                    if other_kind == this_kind =>
                {
                    // Duplicate forward declare
                    self.messages
                        .error_detailed(
                            format!("`{name}` is already a forward declaration"),
                            new_span,
                        )
                        .with_note("previous forward declaration here", old_span)
                        .with_error("new one here", new_span)
                        .finish();
                }
                (
                    DeclareKind::Forward(other_kind, resolve_to),
                    DeclareKind::Resolved(this_kind),
                ) if other_kind == this_kind => {
                    // resolve_to: none -> didn't change
                    // resolve_to: some(== def_id) -> did change, this resolved it
                    // resolve_to: some(!= def_id) -> didn't change, this didn't resolve it (ok to note as redeclare)

                    if resolve_to.is_none() {
                        // Forwards must be resolved to the same scope
                        // This declaration didn't resolve it, which only happens if they're not in the same scope
                        self.messages
                            .error_detailed(
                                format!("`{name}` must be resolved in the same scope"),
                                new_span,
                            )
                            .with_note(format!("forward declaration of `{name}` here"), old_span)
                            .with_error(
                                format!("resolution of `{name}` is not in the same scope"),
                                new_span,
                            )
                            .finish();
                    } else {
                        // This shouldn't ever fail, since if a symbol is moving from
                        // forward to resolved, this is the declaration that should've
                        // done it.
                        assert_eq!(
                            resolve_to,
                            Some(def_id),
                            "encountered a forward not resolved by this definition"
                        );
                    }
                }
                (_, DeclareKind::ItemExport(exported_from)) => {
                    // From an unqualified export in a local module
                    // Use the originating item as the top-level error span
                    let from_item_span = self
                        .library
                        .local_def(exported_from)
                        .def_at
                        .lookup_in(&self.library);

                    self.messages
                        .error_detailed(
                            format!("`{name}` is already declared in the parent scope"),
                            from_item_span,
                        )
                        .with_note(format!("`{name}` previously declared here"), old_span)
                        .with_error(format!("`{name}` exported from here"), new_span)
                        .finish();
                }
                (_, DeclareKind::ItemImport(_)) => {
                    // Import is declaring over something that's already visible.
                    //
                    // This isn't limited to pervasive defs, and can be because defs were introduced
                    // before the import (e.g. module's name, param decls)
                    let is_old_pervasive = self.scopes.is_pervasive(old_def);

                    if is_old_pervasive {
                        // Visible because old def was pervasive
                        self.messages
                            .error_detailed(
                                format!("`{name}` is already imported in this scope"),
                                new_span,
                            )
                            .with_note(format!("`{name}` declared pervasive here"), old_span)
                            .with_error(format!("`{name}` re-imported here"), new_span)
                            .with_info(format!(
                                "`{name}` was declared pervasively, so it's already imported"
                            ))
                            .finish();
                    } else {
                        // Declared for another reason
                        self.messages
                            .error_detailed(
                                format!("`{name}` is already declared in this scope"),
                                new_span,
                            )
                            .with_note(format!("`{name}` previously declared here"), old_span)
                            .with_error(format!("`{name}` imported here"), new_span)
                            .finish();
                    }
                }
                _ => {
                    // From a new declaration
                    self.messages
                        .error_detailed(
                            format!("`{name}` is already declared in this scope"),
                            new_span,
                        )
                        .with_note(format!("`{name}` previously declared here"), old_span)
                        .with_error(format!("`{name}` redeclared here"), new_span)
                        .finish();
                }
            }
        }
    }

    fn is_unnamed(&self, def_id: LocalDefId) -> bool {
        self.library.local_def(def_id).kind.is_none()
    }

    fn with_scope<R>(&mut self, kind: ScopeKind, f: impl FnOnce(&mut Self) -> R) -> R {
        self.scopes.push_scope(kind);
        let res = f(self);
        self.scopes.pop_scope();

        res
    }
}

// Items //
impl<'a> ResolveCtx<'a> {
    fn resolve_item(&mut self, item_id: ItemId) {
        // Consistency with `with_scope`
        let this = self;

        match &this.library.item(item_id).kind {
            ItemKind::ConstVar(item) => {
                if let Some(type_id) = item.type_spec {
                    this.resolve_type(type_id);
                }
                if let Some(body_id) = item.init_expr {
                    this.resolve_body(body_id);
                }

                // Declare names after uses to prevent def-use cycles
                this.introduce_def(item.def_id, DeclareKind::Declared);
            }
            ItemKind::Type(item) => {
                // DeclareKind is based on how this type is defined
                let decl_kind = match item.type_def {
                    DefinedType::Alias(type_id) => {
                        this.resolve_type(type_id);
                        DeclareKind::Resolved(ForwardKind::Type)
                    }
                    DefinedType::Forward(_) => DeclareKind::Forward(ForwardKind::Type, None),
                };

                // Declare name after type to prevent def-use cycles
                // ???: Do we still want to do introduce it here?
                // We can still create cycles by just introducing a forward def.
                // Plus, we plan to have an occurrence check as part of type infer,
                // making this useless.
                this.introduce_def(item.def_id, decl_kind);
            }
            ItemKind::Binding(item) => {
                this.resolve_body(item.bind_to);

                // Declare names after uses to prevent def-use cycles
                this.introduce_def(item.def_id, DeclareKind::Declared);
            }
            ItemKind::Subprogram(item) => {
                this.introduce_def(item.def_id, DeclareKind::Declared);

                if let Some(param_list) = &item.param_list {
                    // Make sure there aren't any duplicate names
                    this.with_scope(ScopeKind::SubprogramHeader, |this| {
                        for param_def in &param_list.names {
                            this.introduce_def(*param_def, DeclareKind::Declared);
                        }
                    });

                    for param in &param_list.tys {
                        this.resolve_type(param.param_ty);
                    }
                }

                this.resolve_type(item.result.ty);

                match item.extra {
                    SubprogramExtra::DeviceSpec(body_id) | SubprogramExtra::StackSize(body_id) => {
                        this.resolve_body(body_id);
                    }
                    _ => {}
                }

                this.with_scope(ScopeKind::Subprogram, |this| {
                    this.with_scope(ScopeKind::Block, |this| {
                        // Introduce params
                        if let Some(param_list) = &item.param_list {
                            for &param_def in &param_list.names {
                                let name = this.library.local_def(param_def).name;
                                this.scopes.def_sym(name, param_def, DeclareKind::Declared);
                            }
                        }

                        // Optionally result type
                        if let Some(result_name) = item.result.name {
                            this.introduce_def(
                                result_name,
                                DeclareKind::LimitedDeclared(LimitedKind::PostCondition),
                            );
                        }

                        // And any remaining imports
                        for &item_id in &item.body.imports {
                            this.resolve_item(item_id);
                        }

                        this.resolve_body(item.body.body);
                    });
                })
            }
            ItemKind::Module(item) => {
                // Always introduce it before so that it's visible to import lookup
                this.introduce_def(item.def_id, DeclareKind::Declared);

                this.with_scope(ScopeKind::Module, |this| {
                    // Make visible in the inner scope, but only if it isn't pervasive
                    // (it being pervasive would mean it's already visible)
                    if !this.scopes.is_pervasive(item.def_id) {
                        this.introduce_def(item.def_id, DeclareKind::Declared);
                    }

                    this.with_scope(ScopeKind::Block, |this| {
                        // Introduce applicable imports
                        for &item_id in &item.imports {
                            this.resolve_item(item_id);
                        }

                        this.resolve_body(item.body);
                    });
                });

                // Introduce the unqualified exports too
                for export in item.exports.iter().filter(|item| {
                    matches!(
                        item.qualify_as,
                        QualifyAs::Unqualified | QualifyAs::PervasiveUnqualified
                    )
                }) {
                    let exported_def = this.library.item(export.item_id).def_id;
                    this.introduce_def(export.def_id, DeclareKind::ItemExport(exported_def));
                }
            }
            ItemKind::Import(item) => {
                // resolve it right now
                let name = this.library.local_def(item.def_id).name;

                let imported_def = if let Some(def_id) = this.scopes.import_sym(name) {
                    Some(def_id)
                } else {
                    let def_at = this
                        .library
                        .local_def(item.def_id)
                        .def_at
                        .lookup_in(&this.library);

                    this.messages.error(
                        format!("`{name}` could not be imported"),
                        format!("no definitions of `{name}` found in the surrounding scope"),
                        def_at,
                    );

                    None
                };

                this.introduce_def(item.def_id, DeclareKind::ItemImport(imported_def));
            }
        }
    }
}

impl<'a> ResolveCtx<'a> {
    fn resolve_body(&mut self, body_id: BodyId) {
        // Consistency with `with_scope`
        let this = self;

        match &this.library.body(body_id).kind {
            BodyKind::Stmts(stmts, _, _) => {
                this.resolve_stmts(stmts, body_id);
            }
            BodyKind::Exprs(expr) => {
                this.resolve_expr(expr.in_body(body_id));
            }
        }
    }
}

// Stmts //
impl<'a> ResolveCtx<'a> {
    fn resolve_stmts(&mut self, stmts: &[StmtId], body_id: BodyId) {
        // Consistency with `with_scope`
        let this = self;

        for stmt_id in stmts {
            this.resolve_stmt(stmt_id.in_body(body_id));
        }
    }

    fn resolve_stmt(&mut self, node_id: BodyStmt) {
        // Consistency with `with_scope`
        let this = self;
        let body_id = node_id.body();

        match &this.library.body(node_id.body()).stmt(node_id.stmt()).kind {
            StmtKind::Item(item_id) => this.resolve_item(*item_id),
            StmtKind::Assign(stmt) => {
                this.resolve_expr(stmt.lhs.in_body(body_id));
                this.resolve_expr(stmt.rhs.in_body(body_id));
            }
            StmtKind::Put(stmt) => {
                if let Some(expr) = stmt.stream_num {
                    this.resolve_expr(expr.in_body(body_id))
                }

                for item in &stmt.items {
                    let put_item = match item {
                        Skippable::Skip => continue,
                        Skippable::Item(put_item) => put_item,
                    };

                    this.resolve_expr(put_item.expr.in_body(body_id));
                    this.try_resolve_expr(put_item.opts.width(), body_id);
                    this.try_resolve_expr(put_item.opts.precision(), body_id);
                    this.try_resolve_expr(put_item.opts.exponent_width(), body_id);
                }
            }
            StmtKind::Get(stmt) => {
                if let Some(expr) = stmt.stream_num {
                    this.resolve_expr(expr.in_body(body_id))
                }

                for item in &stmt.items {
                    let get_item = match item {
                        Skippable::Skip => continue,
                        Skippable::Item(get_item) => get_item,
                    };

                    this.resolve_expr(get_item.expr.in_body(body_id));

                    if let GetWidth::Chars(expr) = get_item.width {
                        this.resolve_expr(expr.in_body(body_id))
                    }
                }
            }
            StmtKind::For(stmt) => {
                match stmt.bounds {
                    ForBounds::Implicit(expr) => this.resolve_expr(expr.in_body(body_id)),
                    ForBounds::Full(start, end) => {
                        this.resolve_expr(start.in_body(body_id));
                        this.resolve_expr(end.in_body(body_id));
                    }
                }

                this.try_resolve_expr(stmt.step_by, body_id);

                this.with_scope(ScopeKind::Loop, |this| {
                    // counter is only available inside of the loop body
                    if let Some(def_id) = stmt.counter_def {
                        this.introduce_def(def_id, DeclareKind::Declared);
                    }

                    this.resolve_stmts(&stmt.stmts, body_id);
                })
            }
            StmtKind::Loop(stmt) => this.with_scope(ScopeKind::Loop, |this| {
                this.resolve_stmts(&stmt.stmts, body_id);
            }),
            StmtKind::Exit(stmt) => {
                this.try_resolve_expr(stmt.when_condition, body_id);
            }
            StmtKind::If(stmt) => {
                this.resolve_expr(stmt.condition.in_body(body_id));

                // Each branch gets its own scope
                this.with_scope(ScopeKind::Block, |this| {
                    this.resolve_stmt(stmt.true_branch.in_body(body_id));
                });

                match stmt.false_branch {
                    // `else-if` doesn't get a block scope, since it's at the same scoping
                    // as the current `if`
                    FalseBranch::ElseIf(stmt_id) => this.resolve_stmt(stmt_id.in_body(body_id)),
                    // `else` however does, since it's self-contained
                    FalseBranch::Else(stmt_id) => this.with_scope(ScopeKind::Block, |this| {
                        this.resolve_stmt(stmt_id.in_body(body_id))
                    }),
                    FalseBranch::None => {}
                }
            }
            StmtKind::Case(stmt) => {
                this.resolve_expr(stmt.discriminant.in_body(body_id));

                for arm in &stmt.arms {
                    if let CaseSelector::Exprs(exprs) = &arm.selectors {
                        for selector in exprs {
                            this.resolve_expr(selector.in_body(body_id));
                        }
                    }

                    this.with_scope(ScopeKind::Block, |this| {
                        this.resolve_stmts(&arm.stmts, body_id)
                    })
                }
            }
            StmtKind::Block(stmt) => this.with_scope(ScopeKind::Block, |this| {
                this.resolve_stmts(&stmt.stmts, body_id);
            }),
            StmtKind::Call(stmt) => {
                this.resolve_expr(stmt.lhs.in_body(body_id));

                if let Some(args) = &stmt.arguments {
                    for arg in args {
                        this.resolve_expr(arg.in_body(body_id));
                    }
                }
            }
            StmtKind::Return(_) => {}
            StmtKind::Result(stmt) => {
                this.resolve_expr(stmt.expr.in_body(body_id));
            }
        }
    }
}

// Exprs //
impl<'a> ResolveCtx<'a> {
    /// Like [`Self::resolve_expr`] + [`Option::map`]
    fn try_resolve_expr(&mut self, expr_id: Option<ExprId>, body_id: BodyId) {
        if let Some(expr) = expr_id {
            self.resolve_expr(expr.in_body(body_id));
        }
    }

    fn resolve_expr(&mut self, node_id: BodyExpr) {
        // Consistency with `with_scope`
        let this = self;
        let body_id = node_id.body();

        match &this.library.body(body_id).expr(node_id.expr()).kind {
            ExprKind::Missing => {}
            ExprKind::Literal(_) => {}
            ExprKind::Init(expr) => {
                for body_id in &expr.exprs {
                    this.resolve_body(*body_id);
                }
            }
            ExprKind::Binary(expr) => {
                this.resolve_expr(expr.lhs.in_body(body_id));
                this.resolve_expr(expr.rhs.in_body(body_id));
            }
            ExprKind::Unary(expr) => {
                this.resolve_expr(expr.rhs.in_body(body_id));
            }
            ExprKind::All => {}
            ExprKind::Range(expr) => {
                let resolve_range_bound = |this: &mut Self, range| match range {
                    RangeBound::FromStart(expr) | RangeBound::FromEnd(expr) => {
                        this.resolve_expr(expr.in_body(body_id))
                    }
                    RangeBound::AtEnd(_) => {}
                };

                resolve_range_bound(this, expr.start);
                if let Some(end) = expr.end {
                    resolve_range_bound(this, end);
                }
            }
            ExprKind::Name(expr) => match expr {
                Name::Name(binding) => {
                    let resolve = this.use_sym(*binding.item(), binding.span());
                    this.resolves.resolves.insert(*binding, resolve);
                }
                Name::Self_ => unimplemented!(),
            },
            ExprKind::Field(expr) => {
                this.resolve_expr(expr.lhs.in_body(body_id));
            }
            ExprKind::Deref(expr) => {
                this.resolve_expr(expr.rhs.in_body(body_id));
            }
            ExprKind::Call(expr) => {
                this.resolve_expr(expr.lhs.in_body(body_id));

                for arg in &expr.arguments {
                    this.resolve_expr(arg.in_body(body_id));
                }
            }
        }
    }
}

// Types //
impl<'a> ResolveCtx<'a> {
    fn resolve_type(&mut self, type_id: TypeId) {
        // Consistency with `with_scope`
        let this = self;

        match &this.library.lookup_type(type_id).kind {
            TypeKind::Missing => {}
            TypeKind::Primitive(ty) => match ty {
                Primitive::SizedChar(SeqLength::Expr(body_id))
                | Primitive::SizedString(SeqLength::Expr(body_id)) => {
                    this.resolve_body(*body_id);
                }
                _ => {}
            },
            TypeKind::Alias(ty) => {
                let binding = ty.base_def;
                let resolve = this.use_sym(*binding.item(), binding.span());
                this.resolves.resolves.insert(binding, resolve);
            }
            TypeKind::Constrained(ty) => {
                this.resolve_body(ty.start);

                if let ConstrainedEnd::Expr(body_id) = ty.end {
                    this.resolve_body(body_id);
                }
            }
            TypeKind::Enum(_ty) => {
                // FIXME: Add resolve visits when size specs are lowered
            }
            TypeKind::Array(ty) => {
                for &range in &ty.ranges {
                    this.resolve_type(range);
                }

                this.resolve_type(ty.elem_ty);
            }
            TypeKind::Set(ty) => {
                this.resolve_type(ty.elem_ty);
                // FIXME: Add body resolve visit when size specs are lowered
            }
            TypeKind::Pointer(ty) => {
                this.resolve_type(ty.ty);
            }
            TypeKind::Subprogram(ty) => {
                if let Some(param_list) = &ty.param_list {
                    for param in param_list {
                        this.resolve_type(param.param_ty);
                    }
                }

                this.resolve_type(ty.result_ty);
            }
            TypeKind::Void => {}
        }
    }
}
