//! AST-level resolution

use std::collections::HashMap;

use indexmap::IndexMap;
use toc_hir::symbol::{DefInfoTable, DefMap, DefResolve, LocalDefId, NodeSpan};
use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, Span, SpanTable, TextRange};
use toc_syntax::{
    ast::{self, AstNode},
    match_ast, SyntaxNode,
};

use crate::{
    collector::CollectRes,
    resolver::scopes::{DeclareKind, ForwardKind, ScopeKind},
    LoweringDb,
};

#[derive(Debug, Default)]
pub(crate) struct ResolveRes {
    // taken from CollectRes
    pub(crate) defs: DefInfoTable,
    pub(crate) spans: SpanTable,
    pub(crate) node_defs: HashMap<NodeSpan, LocalDefId>,

    // resolution's contribution
    pub(crate) assoc_defs: DefMap<Vec<LocalDefId>>,
    pub(crate) def_resolves: DefMap<DefResolve>,
}

pub(crate) fn resolve_defs(
    db: &dyn LoweringDb,
    reachable_files: &[FileId],
    collect_res: CollectRes,
) -> CompileResult<ResolveRes> {
    let CollectRes {
        defs,
        node_defs,
        spans,
    } = collect_res;
    let mut res = ResolveRes {
        defs,
        node_defs,
        spans,
        assoc_defs: Default::default(),
        def_resolves: Default::default(),
    };

    // Generate the scope trace, as well as the list of bindings to resolve
    // Each file gets its own scope trace

    let file_traces = reachable_files
        .iter()
        .map(|&file| {
            let root = ast::Source::cast(db.parse_file(file).result().syntax()).unwrap();
            let trace = FileTracer::trace_file(file, root, &mut res);

            (file, trace)
        })
        .collect::<IndexMap<_, _>>();

    eprintln!("{file_traces:#?}");

    CompileResult::new(res, MessageSink::default().finish())
}

#[derive(Debug)]
enum ScopeEvent {
    /// Enters into a scope, applicable for the given [`NodeSpan`].
    EnterScope(ScopeKind, NodeSpan),
    /// Introduces the [`LocalDefId`] after the span of [`NodeSpan`].
    IntroduceAfter(LocalDefId, DeclareKind, NodeSpan),
    /// Introduces an import after the span of [`NodeSpan`].
    /// Imports are treated specially, as they may also introduce extra
    /// unqualified imports.
    IntroduceImport(LocalDefId, NodeSpan),
    /// Introduces unqualified exports of the [`LocalDefId`], if it has any.
    IntroduceUnqualfieds(LocalDefId, NodeSpan),
}

#[derive(Debug, Default)]
struct FileTrace {
    scope_trace: Vec<ScopeEvent>,
    import_bindings: Vec<NodeSpan>,
    name_bindings: Vec<NodeSpan>,
}

struct FileTracer<'ctx> {
    ctx: &'ctx mut ResolveRes,
    file: FileId,
    root: ast::Source,
    res: FileTrace,
}

impl<'ctx> FileTracer<'ctx> {
    fn trace_file(file: FileId, root: ast::Source, ctx: &'ctx mut ResolveRes) -> FileTrace {
        let mut trace = Self {
            ctx,
            file,
            root,
            res: Default::default(),
        };

        trace.visit_root();

        trace.res
    }

    fn node_span(&mut self, range: TextRange) -> NodeSpan {
        NodeSpan(self.ctx.spans.intern_span(Span::new(self.file, range)))
    }

    fn name_def(&mut self, name: Option<ast::Name>) -> Option<LocalDefId> {
        let name_span = self.node_span(name?.syntax().text_range());
        let name_def = self.ctx.node_defs.get(&name_span).unwrap();

        Some(*name_def)
    }

    fn introduce_name_after(
        &mut self,
        name: Option<ast::Name>,
        decl_kind: DeclareKind,
        introduce_after: NodeSpan,
    ) {
        let name = match name {
            Some(name) => name,
            None => return,
        };
        let name_span = self.node_span(name.syntax().text_range());
        let name_def = self.ctx.node_defs.get(&name_span).unwrap();

        self.res.scope_trace.push(ScopeEvent::IntroduceAfter(
            *name_def,
            decl_kind,
            introduce_after,
        ))
    }

    fn introduce_name(&mut self, name: Option<ast::Name>, decl_kind: DeclareKind) {
        let name = match name {
            Some(name) => name,
            None => return,
        };
        let name_span = self.node_span(name.syntax().text_range());
        let name_def = self.ctx.node_defs.get(&name_span).unwrap();

        self.res
            .scope_trace
            .push(ScopeEvent::IntroduceAfter(*name_def, decl_kind, name_span))
    }

    fn introduce_unqualifieds(&mut self, name: Option<ast::Name>, after: &SyntaxNode) {
        let name_def = match self.name_def(name) {
            Some(def) => def,
            None => return,
        };
        let after = self.node_span(after.text_range());

        self.res
            .scope_trace
            .push(ScopeEvent::IntroduceUnqualfieds(name_def, after))
    }

    /// `in_scope` is used to logically denote what things are
    /// only visible inside of the scope
    fn inside_scope(
        &mut self,
        kind: ScopeKind,
        stmts: ast::StmtList,
        in_scope: impl FnOnce(&mut Self),
    ) {
        let span = self.node_span(stmts.syntax().text_range());
        self.res
            .scope_trace
            .push(ScopeEvent::EnterScope(kind, span));
        in_scope(self);
    }
}

// Scoping //
impl FileTracer<'_> {
    fn visit_root(&mut self) {
        for node in self.root.syntax().descendants() {
            {
                match_ast! {
                    match node {
                        ast::ConstVarDecl(decl) => self.visit_decl_constvar(decl),
                        ast::TypeDecl(decl) => self.visit_decl_type(decl),
                        ast::BindDecl(decl) => self.visit_decl_bind(decl),
                        ast::ProcDecl(decl) => self.visit_decl_proc(decl),
                        ast::FcnDecl(decl) => self.visit_decl_fcn(decl),
                        ast::ProcessDecl(decl) => self.visit_decl_process(decl),
                        ast::ExternalDecl(_decl) => {},
                        ast::ForwardDecl(_decl) => {},
                        ast::DeferredDecl(_decl) => {},
                        ast::BodyDecl(_decl) => {},
                        ast::ModuleDecl(decl) => self.visit_decl_module(decl),
                        ast::ClassDecl(_decl) => {},
                        ast::MonitorDecl(_decl) => {},

                        // Notable stmts //
                        ast::ForStmt(stmt) => self.visit_stmt_for(stmt),
                        ast::LoopStmt(stmt) => self.visit_stmt_loop(stmt),
                        ast::HandlerStmt(_stmt) => {},
                        ast::StmtList(stmts) => self.visit_stmt_list(stmts),

                        // Types //
                        ast::CollectionType(_ty) => {},

                        // Name refs //
                        ast::ImportItem(decl) => self.visit_decl_import(decl),
                        ast::NameExpr(expr) => self.visit_expr_name(expr),
                        _ => {}
                    }
                }
            }
        }
    }

    fn visit_decl_constvar(&mut self, decl: ast::ConstVarDecl) {
        // Collect defs from name list
        // All of them are introduced after the initializer,
        // so that they can't be used in the initializer

        // ???: Do we want to do so, but error on use?
        // Would mean cyclic evaluation, so we won't do it for now
        let introduce_after = self.node_span(decl.syntax().text_range());
        for name in decl.decl_list().unwrap().names() {
            self.introduce_name_after(Some(name), DeclareKind::Declared, introduce_after);
        }
    }

    fn visit_decl_type(&mut self, decl: ast::TypeDecl) {
        let decl_kind = if decl.forward_token().is_some() {
            DeclareKind::Resolved(ForwardKind::Type)
        } else {
            DeclareKind::Forward(ForwardKind::Type, None)
        };

        // Introduce after the item

        // ???: Do we still want to introduce after?
        // We can still create cycles by just introducing a forward def.
        // Plus, we plan to have an occurrence check as part of type infer,
        // making this useless.
        let introduce_after = self.node_span(decl.syntax().text_range());
        self.introduce_name_after(decl.decl_name(), decl_kind, introduce_after)
    }

    fn visit_decl_bind(&mut self, decl: ast::BindDecl) {
        for binding in decl.bindings() {
            let introduce_after = self.node_span(binding.syntax().text_range());
            self.introduce_name_after(binding.bind_as(), DeclareKind::Declared, introduce_after)
        }
    }

    fn visit_decl_proc(&mut self, decl: ast::ProcDecl) {
        let header = decl.proc_header().unwrap();
        self.introduce_name(header.name(), DeclareKind::Declared);

        self.inside_scope(
            ScopeKind::Subprogram,
            decl.subprog_body().unwrap().stmt_list().unwrap(),
            |this| {
                this.visit_formal_params(header.params());
            },
        );
    }

    fn visit_decl_fcn(&mut self, decl: ast::FcnDecl) {
        let header = decl.fcn_header().unwrap();
        self.introduce_name(header.name(), DeclareKind::Declared);

        self.inside_scope(
            ScopeKind::Subprogram,
            decl.subprog_body().unwrap().stmt_list().unwrap(),
            |this| {
                this.visit_formal_params(header.params());
            },
        );
    }

    fn visit_decl_process(&mut self, decl: ast::ProcessDecl) {
        let header = decl.process_header().unwrap();
        self.introduce_name(header.name(), DeclareKind::Declared);

        self.inside_scope(
            ScopeKind::Subprogram,
            decl.subprog_body().unwrap().stmt_list().unwrap(),
            |this| {
                this.visit_formal_params(header.params());
            },
        );
    }

    fn visit_formal_params(&mut self, param_list: Option<ast::ParamSpec>) {
        let param_list = match param_list {
            Some(it) => it,
            None => return,
        };

        for param in param_list.param_decl() {
            match param {
                ast::ParamDecl::ConstVarParam(param) => {
                    for name in param.param_names().unwrap().names() {
                        self.introduce_name(Some(name), DeclareKind::AlwaysShadow);
                    }
                }
                ast::ParamDecl::SubprogType(ast::SubprogType::FcnType(param)) => {
                    self.introduce_name(param.name(), DeclareKind::AlwaysShadow);
                }
                ast::ParamDecl::SubprogType(ast::SubprogType::ProcType(param)) => {
                    self.introduce_name(param.name(), DeclareKind::AlwaysShadow);
                }
            }
        }
    }

    fn visit_decl_module(&mut self, decl: ast::ModuleDecl) {
        self.introduce_name(decl.name(), DeclareKind::Declared);

        // TODO: pervasive stuff
        self.inside_scope(ScopeKind::Module, decl.stmt_list().unwrap(), |this| {
            // Make visible in the inner scope, but only if it isn't pervasive
            // (it being pervasive would mean it's already visible)
            if decl.pervasive_attr().is_none() {
                this.introduce_name(decl.name(), DeclareKind::Declared);
            }

            // Applicable imports are handled by
        });

        // Unqualifieds are explicitly introduced after
        self.introduce_unqualifieds(decl.name(), decl.stmt_list().unwrap().syntax());
    }

    fn visit_stmt_for(&mut self, stmt: ast::ForStmt) {
        self.inside_scope(ScopeKind::Loop, stmt.stmt_list().unwrap(), |this| {
            // Introduce counter, if present
            if let Some(name) = stmt.name() {
                let introduce_after = this.node_span(name.syntax().text_range());
                this.introduce_name_after(Some(name), DeclareKind::Declared, introduce_after);
            }
        });
    }

    fn visit_stmt_loop(&mut self, stmt: ast::LoopStmt) {
        self.inside_scope(ScopeKind::Loop, stmt.stmt_list().unwrap(), |_| ());
    }

    fn visit_stmt_list(&mut self, stmts: ast::StmtList) {
        self.inside_scope(ScopeKind::Block, stmts, |_| ());
    }
}

// Name refs //
impl FileTracer<'_> {
    fn visit_decl_import(&mut self, decl: ast::ImportItem) {
        // ???: How to deal with things that have no name?
        // Issue is mostly fetching that name from the referring file
        // We know what it is, it's mostly just the issue of fetching that name
        // We know what it resolves to, so we'll defer it to later

        if let Some(external_name) = decl.external_item().and_then(|item| item.name()) {
            // Name that needs to be resolved, as well as introducing a def
            let node = self.node_span(external_name.syntax().text_range());
            self.res.import_bindings.push(node);

            // Note: for import decls:
            // Only consider as an export candidate if we aren't in a forward decl
            // <ImportItem> => ImportList => ForwardDecl
            // FIXME: use a def scope instead once we deal with forward decls

            // Note: Imports on forward decls need special treatment, since
            // the resolved impls need to collect them together and be introduced
            // there, not at the forward decl
            let in_forward_decl = decl
                .syntax()
                .parent()
                .and_then(ast::ImportList::cast)
                .and_then(|n| n.syntax().parent())
                .and_then(ast::ForwardDecl::cast)
                .is_some();

            if in_forward_decl {
                let import_def = self.ctx.node_defs.get(&node).copied().unwrap();
                self.res
                    .scope_trace
                    .push(ScopeEvent::IntroduceImport(import_def, node));
            }
        } else {
            // FIXME: Handle external imports
        }
    }

    fn visit_expr_name(&mut self, expr: ast::NameExpr) {
        let name = expr.name().unwrap();
        let node = self.node_span(name.syntax().text_range());
        self.res.name_bindings.push(node);
    }
}
