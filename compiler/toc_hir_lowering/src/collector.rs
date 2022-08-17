//! Collecting all defs declared in a library

use std::collections::HashMap;

use toc_hir::{
    symbol::{
        syms, DefInfoTable, DefMap, IsMonitor, IsPervasive, IsRegister, LocalDefId, Mutability,
        NodeSpan, SubprogramKind, Symbol, SymbolKind,
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
    pub(crate) node_defs: HashMap<NodeSpan, LocalDefId>,
    pub(crate) assoc_defs: DefMap<Vec<LocalDefId>>,
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
        FileCollector::collect(file, root, &mut res, &mut messages);
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

struct FileCollector<'a> {
    file: FileId,
    root: ast::Source,
    res: &'a mut CollectRes,
    _messages: &'a mut MessageSink,
    scopes: DefScope,
}

impl<'a> FileCollector<'a> {
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
            // FIXME: Handle include globs
            // AAAAAAAAAAAAAAAAAAAAAAAAAAA

            // Pull out node & state
            match event {
                WalkEvent::Enter(node) => {
                    let state = NodeState::Enter;

                    match_ast! {
                        match (node) {
                            // Decls //
                            ast::ConstVarDecl(decl) => self.collect_decl_constvar(decl),
                            ast::TypeDecl(decl) => self.collect_decl_type(decl),
                            ast::BindDecl(decl) => self.collect_decl_bind(decl),
                            ast::ProcDecl(decl) => self.collect_decl_proc(decl),
                            ast::FcnDecl(decl) => self.collect_decl_fcn(decl),
                            ast::ProcessDecl(decl) => self.collect_decl_process(decl),
                            ast::ExternalDecl(_decl) => {},
                            ast::ForwardDecl(_decl) => {},
                            ast::DeferredDecl(_decl) => {},
                            ast::BodyDecl(_decl) => {},
                            ast::ModuleDecl(decl) => self.collect_decl_module(decl, state),
                            ast::ClassDecl(_decl) => {},
                            ast::MonitorDecl(_decl) => {},

                            ast::ImportItem(decl) => self.collect_decl_import(decl),

                            // Notable stmts //
                            ast::ForStmt(stmt) => self.collect_stmt_for(stmt),
                            ast::HandlerStmt(_stmt) => {},

                            // Scopes don't need to be precise, so long as they
                            // prevent defs from escaping & being considered
                            // export candidates
                            ast::StmtList(stmt) => self.wrap_scope(stmt, state),

                            // Types //
                            ast::EnumType(ty) => self.collect_ty_enum(ty),
                            ast::SetType(ty) => self.collect_ty_set(ty),
                            ast::RecordType(_ty) => {},
                            ast::UnionType(_ty) => {},
                            ast::FcnType(ty) => self.collect_ty_fcn(ty),
                            ast::ProcType(ty) => self.collect_ty_proc(ty),
                            ast::CollectionType(_ty) => {},
                            _ => {}
                        }
                    }
                }
                WalkEvent::Leave(node) => {
                    let state = NodeState::Leave;

                    // These are the only nodes which care about node exit
                    match_ast! {
                        match (node) {
                            ast::ModuleDecl(decl) => self.collect_decl_module(decl, state),
                            ast::ClassDecl(_decl) => {},
                            ast::MonitorDecl(_decl) => {},
                            ast::StmtList(stmt) => self.wrap_scope(stmt, state),
                            _ => {}
                        }
                    }
                }
            };
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
        self.bind_span(def_id, span);

        def_id
    }

    fn bind_span(&mut self, local_def: LocalDefId, span: SpanId) {
        self.res.node_defs.insert(NodeSpan(span), local_def);
    }

    fn lookup_name_def(&mut self, name: Option<ast::Name>) -> Option<LocalDefId> {
        let name = name?.identifier_token().unwrap();

        let span = self.intern_range(name.text_range());
        let ident = NodeSpan(span);
        let local_def = self
            .res
            .node_defs
            .get(&ident)
            .copied()
            .expect("def didn't get added beforehand");

        Some(local_def)
    }
}

impl FileCollector<'_> {
    // Decls //
    fn collect_decl_constvar(&mut self, node: ast::ConstVarDecl) {
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

    fn collect_decl_type(&mut self, node: ast::TypeDecl) {
        if let Some(name) = node.decl_name() {
            let is_pervasive = node.pervasive_attr().is_some();
            self.add_exported_name(name, Some(SymbolKind::Type), is_pervasive.into());
        }
    }

    fn collect_decl_bind(&mut self, node: ast::BindDecl) {
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

    fn collect_decl_proc(&mut self, node: ast::ProcDecl) {
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

    fn collect_decl_fcn(&mut self, node: ast::FcnDecl) {
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

    fn collect_decl_process(&mut self, node: ast::ProcessDecl) {
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

                if let Some(local_def) = self.lookup_name_def(node.name()) {
                    // associate defs as export candidates
                    self.res.assoc_defs.insert(local_def, candidates);
                }
            }
        }
    }

    fn collect_decl_import(&mut self, node: ast::ImportItem) {
        if let Some(name) = node.external_item().and_then(|item| item.name()) {
            let local_def = self.add_name(name, Some(SymbolKind::Import), IsPervasive::No);

            // Only consider as an export candidate if we aren't in a forward decl
            // <ImportItem> => ImportList => ForwardDecl
            // TODO: use a def scope instead once we deal with forward decls
            let in_forward_decl = Some(node)
                .and_then(|n| n.syntax().parent())
                .and_then(ast::ImportList::cast)
                .and_then(|n| n.syntax().parent())
                .and_then(ast::ForwardDecl::cast)
                .is_some();

            if !in_forward_decl {
                self.scopes.introduce_def(local_def);
            }
        }
    }

    fn collect_stmt_for(&mut self, node: ast::ForStmt) {
        if let Some(name) = node.name() {
            self.add_exported_name(
                name,
                Some(SymbolKind::ConstVar(Mutability::Const, IsRegister::No)),
                IsPervasive::No,
            );
        }
    }

    fn collect_ty_enum(&mut self, node: ast::EnumType) {
        let variants = node
            .fields()
            .unwrap()
            .names()
            .map(|name| self.add_name(name, Some(SymbolKind::EnumVariant), IsPervasive::No))
            .collect();

        let span = self.intern_range(node.syntax().text_range());
        let local_def = self.res.defs.add_def(
            type_decl_name(node),
            span,
            Some(SymbolKind::Enum),
            IsPervasive::No,
        );

        // bind it to the entire type node, since it doesn't have a name
        self.bind_span(local_def, span);

        self.res.assoc_defs.insert(local_def, variants);
    }

    fn collect_ty_set(&mut self, node: ast::SetType) {
        let span = self.intern_range(node.syntax().text_range());
        let local_def = self.res.defs.add_def(
            type_decl_name(node),
            span,
            Some(SymbolKind::Set),
            IsPervasive::No,
        );

        // bind it to the entire type node, since it doesn't have a name
        self.bind_span(local_def, span);
    }

    fn collect_ty_fcn(&mut self, node: ast::FcnType) {
        // Only needed because formal lowering is shared
        self.collect_formals_spec(node.params());
    }

    fn collect_ty_proc(&mut self, node: ast::ProcType) {
        // Only needed because formal lowering is shared
        self.collect_formals_spec(node.params());
    }
}

/// Gets the name from the enclosing `type` decl, or the [`Anonymous`](syms::Anonymous) symbol
fn type_decl_name(ty: impl ast::AstNode) -> Symbol {
    ty.syntax()
        .parent()
        .and_then(ast::TypeDecl::cast)
        .and_then(|node| node.decl_name())
        .map_or(*syms::Anonymous, |name| {
            name.identifier_token().unwrap().text().into()
        })
}
