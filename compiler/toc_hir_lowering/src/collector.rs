//! Collecting all defs declared in a library

use std::collections::HashMap;

use toc_hir::{
    symbol::{
        DefInfoTable, DefMap, Ident, IsMonitor, IsPervasive, IsRegister, LocalDefId, Mutability,
        SubprogramKind, SymbolKind,
    },
    ty::PassBy,
};
use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, Span, SpanId, SpanTable, TextRange};
use toc_syntax::{
    ast::{self, AstNode},
    match_ast, SyntaxKind, WalkEvent,
};

use crate::LoweringDb;

#[derive(Debug, Default)]
pub(crate) struct CollectRes {
    pub(crate) defs: DefInfoTable,
    pub(crate) ident_defs: HashMap<Ident, LocalDefId>,
    pub(crate) export_candidates: DefMap<Vec<LocalDefId>>,
    pub(crate) spans: SpanTable,
}

pub(crate) fn collect_defs(
    db: &dyn LoweringDb,
    reachable_files: &[FileId],
) -> CompileResult<CollectRes> {
    let mut res = CollectRes::default();
    let mut messages = MessageSink::default();

    for &file in reachable_files {
        let root = ast::Source::cast(db.parse_file(file).result().syntax()).unwrap();
        CollectCtx::collect(file, root, &mut res, &mut messages);
    }

    CompileResult::new(res, messages.finish())
}

/// Whether we're entering or leaving a node
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeState {
    Enter,
    Leave,
}

struct DefScope {
    defs: Vec<Vec<LocalDefId>>,
}

impl DefScope {
    fn new() -> Self {
        Self { defs: vec![vec![]] }
    }

    fn introduce_def(&mut self, local_def: LocalDefId) {
        self.defs
            .last_mut()
            .expect("root scope was popped off")
            .push(local_def);
    }

    fn push_scope(&mut self) {
        self.defs.push(vec![])
    }

    fn pop_scope(&mut self) -> Vec<LocalDefId> {
        self.defs.pop().expect("root scope was popped off")
    }
}

impl Default for DefScope {
    fn default() -> Self {
        Self::new()
    }
}

struct CollectCtx<'a> {
    file: FileId,
    root: ast::Source,
    res: &'a mut CollectRes,
    _messages: &'a mut MessageSink,
    scopes: DefScope,
}

impl<'a> CollectCtx<'a> {
    fn collect(
        file: FileId,
        root: ast::Source,
        res: &'a mut CollectRes,
        messages: &'a mut MessageSink,
    ) {
        let mut ctx = Self {
            file,
            root,
            res,
            _messages: messages,
            scopes: Default::default(),
        };

        ctx.collect_root();
    }

    fn collect_root(&mut self) {
        // FIXME: generate defs for this import statement
        // self.root.import_stmt();

        // Doesn't matter if it's a unit, since def generation is the
        // same regardless

        for event in self.root.syntax().preorder() {
            // Pull out node & state
            let (node, state) = match event {
                WalkEvent::Enter(n) => (n, NodeState::Enter),
                WalkEvent::Leave(n) => (n, NodeState::Leave),
            };

            // FIXME: Handle include globs
            // AAAAAAAAAAAAAAAAAAAAAAAAAAA

            match_ast! {
                match (node) {
                    // Decls //
                    ast::ConstVarDecl(decl) => self.collect_decl_constvar(decl, state),
                    ast::TypeDecl(decl) => self.collect_decl_type(decl, state),
                    ast::BindDecl(decl) => self.collect_decl_bind(decl, state),
                    ast::ProcDecl(decl) => self.collect_decl_proc(decl, state),
                    ast::FcnDecl(decl) => self.collect_decl_fcn(decl, state),
                    ast::ProcessDecl(decl) => self.collect_decl_process(decl, state),
                    ast::ExternalDecl(_decl) => {},
                    ast::ForwardDecl(_decl) => {},
                    ast::DeferredDecl(_decl) => {},
                    ast::BodyDecl(_decl) => {},
                    ast::ModuleDecl(decl) => self.collect_decl_module(decl, state),
                    ast::ClassDecl(_decl) => {},
                    ast::MonitorDecl(_decl) => {},

                    // Notable stmts //
                    ast::ForStmt(stmt) => self.collect_stmt_for(stmt, state),
                    ast::HandlerStmt(_stmt) => {},

                    // Scopes don't need to be precise, so long as they
                    // prevent defs from escaping & being considered
                    // export candidates
                    ast::StmtList(stmt) => self.wrap_scope(stmt, state),

                    // Types //
                    ast::CollectionType(_ty) => {},
                    _ => {}
                }
            }
        }
    }

    /// Wraps stmt nodes in a scope, but only if it doesn't have a significant scope
    /// (i.e. the scope isn't used to collect defs as export candidates)
    fn wrap_scope(&mut self, node: ast::StmtList, state: NodeState) {
        let is_significant_scope = node.syntax().parent().map_or(false, |parent| {
            matches!(
                parent.kind(),
                SyntaxKind::Source
                    | SyntaxKind::ModuleDecl
                    | SyntaxKind::ClassDecl
                    | SyntaxKind::MonitorDecl
            )
        });

        if is_significant_scope {
            return;
        }

        match state {
            NodeState::Enter => {
                self.scopes.push_scope();
            }
            NodeState::Leave => {
                self.scopes.pop_scope();
            }
        }
    }

    fn intern_range(&mut self, range: TextRange) -> SpanId {
        self.res.spans.intern_span(Span::new(self.file, range))
    }

    fn add_exported_name(
        &mut self,
        name: ast::Name,
        kind: Option<SymbolKind>,
        is_pervasive: IsPervasive,
    ) -> LocalDefId {
        let local_def = self.add_name(name, kind, is_pervasive);
        // introduce into export candidates
        self.scopes.introduce_def(local_def);
        local_def
    }

    fn add_exported_name_list(
        &mut self,
        name_list: ast::NameList,
        kind: Option<SymbolKind>,
        is_pervasive: IsPervasive,
    ) -> Vec<LocalDefId> {
        let defs = self.add_name_list(name_list, kind, is_pervasive);

        // introduce into export candidates
        for &local_def in &defs {
            self.scopes.introduce_def(local_def);
        }

        defs
    }

    fn add_name_list(
        &mut self,
        name_list: ast::NameList,
        kind: Option<SymbolKind>,
        is_pervasive: IsPervasive,
    ) -> Vec<LocalDefId> {
        name_list
            .names()
            .map(|name| self.add_name(name, kind, is_pervasive))
            .collect()
    }

    fn add_name(
        &mut self,
        name: ast::Name,
        kind: Option<SymbolKind>,
        is_pervasive: IsPervasive,
    ) -> LocalDefId {
        let name_tok = name.identifier_token().unwrap();
        let name = name_tok.text();
        let span = self.intern_range(name_tok.text_range());

        let def_id = self.res.defs.add_def(name.into(), span, kind, is_pervasive);
        self.res.ident_defs.insert(Ident(name.into(), span), def_id);

        def_id
    }

    fn lookup_def(&mut self, name: Option<ast::Name>) -> Option<LocalDefId> {
        let name = name?.identifier_token().unwrap();

        let span = self.intern_range(name.text_range());
        let ident = Ident(name.text().into(), span);
        let local_def = self
            .res
            .ident_defs
            .get(&ident)
            .copied()
            .expect("def didn't get added beforehand");

        Some(local_def)
    }
}

impl CollectCtx<'_> {
    // Decls //
    fn collect_decl_constvar(&mut self, node: ast::ConstVarDecl, state: NodeState) {
        if matches!(state, NodeState::Leave) {
            return;
        }

        let is_pervasive = node.pervasive_attr().is_some();
        let is_register = node.register_attr().is_some();
        let is_var = node.var_token().is_some();
        let mutability = Mutability::from_is_mutable(is_var);

        self.add_exported_name_list(
            node.decl_list().unwrap(),
            Some(SymbolKind::ConstVar(mutability, is_register.into())),
            is_pervasive.into(),
        );
    }

    fn collect_decl_type(&mut self, node: ast::TypeDecl, state: NodeState) {
        if matches!(state, NodeState::Leave) {
            return;
        }

        if let Some(name) = node.decl_name() {
            let is_pervasive = node.pervasive_attr().is_some();
            self.add_exported_name(name, Some(SymbolKind::Type), is_pervasive.into());
        }
    }

    fn collect_decl_bind(&mut self, node: ast::BindDecl, state: NodeState) {
        if matches!(state, NodeState::Leave) {
            return;
        }

        for binding in node.bindings() {
            if let Some(name) = binding.bind_as() {
                let mutability = Mutability::from_is_mutable(binding.as_var().is_some());
                let is_register = binding.to_register().is_some();

                // Bindings are never pervasive, since they aren't meant to escape
                // the local scope
                self.add_exported_name(
                    name,
                    Some(SymbolKind::Binding(mutability, is_register.into())),
                    IsPervasive::No,
                );
            }
        }
    }

    fn collect_decl_proc(&mut self, node: ast::ProcDecl, state: NodeState) {
        // scope wrapping handled by `wrap_scope`
        if matches!(state, NodeState::Leave) {
            return;
        }

        // name + param names
        let header = node.proc_header().unwrap();

        if let Some(name) = header.name() {
            let is_pervasive = header.pervasive_attr().is_some();
            self.add_exported_name(
                name,
                Some(SymbolKind::Subprogram(SubprogramKind::Procedure)),
                is_pervasive.into(),
            );
        }

        self.collect_formals_spec(header.params());
    }

    fn collect_decl_fcn(&mut self, node: ast::FcnDecl, state: NodeState) {
        // scope wrapping handled by `wrap_scope`
        if matches!(state, NodeState::Leave) {
            return;
        }

        // name + param names + maybe result name
        let header = node.fcn_header().unwrap();

        if let Some(name) = header.name() {
            let is_pervasive = header.pervasive_attr().is_some();
            self.add_exported_name(
                name,
                Some(SymbolKind::Subprogram(SubprogramKind::Function)),
                is_pervasive.into(),
            );
        }

        self.collect_formals_spec(header.params());

        if let Some(res_name) = header.fcn_result().and_then(|res| res.name()) {
            // ???: Does this need to be pass by value? (can it be pass by const ref?)
            self.add_name(
                res_name,
                Some(SymbolKind::Param(PassBy::Value, IsRegister::No)),
                IsPervasive::No,
            );
        }
    }

    fn collect_decl_process(&mut self, node: ast::ProcessDecl, state: NodeState) {
        // scope wrapping handled by `wrap_scope`
        if matches!(state, NodeState::Leave) {
            return;
        }

        // name + param names
        let header = node.process_header().unwrap();

        if let Some(name) = header.name() {
            let is_pervasive = header.pervasive_attr().is_some();
            self.add_exported_name(
                name,
                Some(SymbolKind::Subprogram(SubprogramKind::Process)),
                is_pervasive.into(),
            );
        }

        self.collect_formals_spec(header.params());
    }

    fn collect_formals_spec(&mut self, param_spec: Option<ast::ParamSpec>) {
        let param_spec = match param_spec {
            Some(it) => it,
            _ => return,
        };

        for param in param_spec.param_decl() {
            match param {
                ast::ParamDecl::ConstVarParam(param) => {
                    let is_register = param.bind_to_register().is_some();
                    let pass_by = match param.pass_as_ref() {
                        None => PassBy::Value,
                        Some(_) => PassBy::Reference(Mutability::Var),
                    };

                    self.add_name_list(
                        param.param_names().unwrap(),
                        Some(SymbolKind::Param(pass_by, is_register.into())),
                        IsPervasive::No,
                    );
                }
                ast::ParamDecl::SubprogType(param) => {
                    let name = match param {
                        ast::SubprogType::FcnType(header) => header.name(),
                        ast::SubprogType::ProcType(header) => header.name(),
                    };

                    if let Some(name) = name {
                        self.add_name(
                            name,
                            Some(SymbolKind::Param(PassBy::Value, IsRegister::No)),
                            IsPervasive::No,
                        );
                    }
                }
            }
        }
    }

    fn collect_decl_module(&mut self, node: ast::ModuleDecl, state: NodeState) {
        match state {
            NodeState::Enter => {
                // name
                if let Some(name) = node.name() {
                    let is_pervasive = node.pervasive_attr().is_some();
                    self.add_exported_name(
                        name,
                        Some(SymbolKind::Module(IsMonitor::No)),
                        is_pervasive.into(),
                    );
                }

                self.scopes.push_scope();
            }
            NodeState::Leave => {
                let candidates = self.scopes.pop_scope();

                if let Some(local_def) = self.lookup_def(node.name()) {
                    // associate defs as export candidates
                    self.res.export_candidates.insert(local_def, candidates);
                }
            }
        }
    }

    fn collect_stmt_for(&mut self, node: ast::ForStmt, state: NodeState) {
        // scope wrapping handled by `wrap_scope`
        if matches!(state, NodeState::Leave) {
            return;
        }

        if let Some(name) = node.name() {
            self.add_exported_name(
                name,
                Some(SymbolKind::ConstVar(Mutability::Const, IsRegister::No)),
                IsPervasive::No,
            );
        }
    }
}
