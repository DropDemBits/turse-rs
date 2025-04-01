//! Collecting all defs declared in a package

use std::collections::HashMap;

use toc_hir::{
    span::{SpanId, SpanTable},
    symbol::{
        DefInfoTable, IsMonitor, IsPervasive, IsRegister, LocalDefId, Mutability, NodeSpan,
        SubprogramKind, Symbol, SymbolKind, syms,
    },
    ty::PassBy,
};
use toc_paths::RawPath;
use toc_reporting::{CompileResult, MessageSink};
use toc_span::{FileId, Span, TextRange};
use toc_syntax::{
    ast::{self, AstNode},
    match_ast,
};
use toc_vfs_db::SourceFile;

use crate::Db;

#[derive(Debug, Default, salsa::Update)]
pub(crate) struct CollectRes {
    pub(crate) defs: DefInfoTable,
    pub(crate) node_defs: HashMap<NodeSpan, LocalDefId>,
    pub(crate) spans: SpanTable,
}

pub(crate) fn collect_defs(
    db: &dyn Db,
    reachable_files: &[SourceFile],
) -> CompileResult<CollectRes> {
    let mut res = CollectRes::default();
    let mut messages = MessageSink::default();

    for &file in reachable_files {
        let root = ast::Source::cast(toc_ast_db::parse_file(db, file).result().syntax()).unwrap();
        FileCollector::collect(
            RawPath::new(db, file.path(db)).into(),
            root,
            &mut res,
            &mut messages,
        );
    }

    CompileResult::new(res, messages.finish())
}

struct FileCollector<'a> {
    file: FileId,
    root: ast::Source,
    res: &'a mut CollectRes,
    _messages: &'a mut MessageSink,
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
        };

        ctx.collect_root();
    }

    fn collect_root(&mut self) {
        // FIXME: generate defs for this import statement
        // self.root.import_stmt();

        // Doesn't matter if it's a unit, since def generation is the
        // same regardless

        for node in self.root.syntax().descendants() {
            // FIXME: Handle include globs
            // AAAAAAAAAAAAAAAAAAAAAAAAAAA

            // Pull out node & state
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
                    ast::ModuleDecl(decl) => self.collect_decl_module(decl),
                    ast::ClassDecl(_decl) => {},
                    ast::MonitorDecl(_decl) => {},

                    ast::ImportItem(decl) => self.collect_decl_import(decl),

                    // Notable stmts //
                    ast::ForStmt(stmt) => self.collect_stmt_for(stmt),
                    ast::HandlerStmt(_stmt) => {},

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
    }

    fn intern_range(&mut self, range: TextRange) -> SpanId {
        self.res.spans.intern_span(Span::new(self.file, range))
    }

    fn add_name_list(
        &mut self,
        name_list: impl Iterator<Item = ast::Name>,
        kind: Option<SymbolKind>,
        is_pervasive: IsPervasive,
    ) -> Vec<LocalDefId> {
        name_list
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
}

impl FileCollector<'_> {
    // Decls //
    fn collect_decl_constvar(&mut self, node: ast::ConstVarDecl) {
        let is_pervasive = node.pervasive_attr().is_some();
        let is_register = node.register_attr().is_some();
        let is_var = node.var_token().is_some();
        let mutability = Mutability::from_is_mutable(is_var);

        self.add_name_list(
            node.constvar_names()
                .unwrap()
                .names()
                .flat_map(|it| it.name()),
            Some(SymbolKind::ConstVar(mutability, is_register.into())),
            is_pervasive.into(),
        );
    }

    fn collect_decl_type(&mut self, node: ast::TypeDecl) {
        if let Some(name) = node.decl_name() {
            let is_pervasive = node.pervasive_attr().is_some();
            self.add_name(name, Some(SymbolKind::Type), is_pervasive.into());
        }
    }

    fn collect_decl_bind(&mut self, node: ast::BindDecl) {
        for binding in node.bindings() {
            if let Some(name) = binding.bind_as() {
                let mutability = Mutability::from_is_mutable(binding.as_var().is_some());
                let is_register = binding.to_register().is_some();

                // Bindings are never pervasive, since they aren't meant to escape
                // the local scope
                self.add_name(
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
            self.add_name(
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
            self.add_name(
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
            self.add_name(
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
                        param
                            .param_names()
                            .unwrap()
                            .names()
                            .flat_map(|it| it.name()),
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

    fn collect_decl_module(&mut self, node: ast::ModuleDecl) {
        // name
        if let Some(name) = node.name() {
            let is_pervasive = node.pervasive_attr().is_some();
            self.add_name(
                name,
                Some(SymbolKind::Module(IsMonitor::No)),
                is_pervasive.into(),
            );
        }
    }

    fn collect_decl_import(&mut self, node: ast::ImportItem) {
        if let Some(name) = node.external_item().and_then(|item| item.name()) {
            self.add_name(name, Some(SymbolKind::Import), IsPervasive::No);
        }
    }

    fn collect_stmt_for(&mut self, node: ast::ForStmt) {
        if let Some(name) = node.name() {
            self.add_name(
                name,
                Some(SymbolKind::ConstVar(Mutability::Const, IsRegister::No)),
                IsPervasive::No,
            );
        }
    }

    fn collect_ty_enum(&mut self, node: ast::EnumType) {
        let span = self.intern_range(node.syntax().text_range());
        let local_def = self.res.defs.add_def(
            type_decl_name(&node),
            span,
            Some(SymbolKind::Enum),
            IsPervasive::No,
        );

        // bind it to the entire type node, since it doesn't have a name
        self.bind_span(local_def, span);

        // Make the variant defs after the name so that adding new variants
        // doesn't affect the primary enum def
        self.add_name_list(
            node.fields()
                .unwrap()
                .enum_variant()
                .flat_map(|it| it.name()),
            Some(SymbolKind::EnumVariant),
            IsPervasive::No,
        );
    }

    fn collect_ty_set(&mut self, node: ast::SetType) {
        let span = self.intern_range(node.syntax().text_range());
        let local_def = self.res.defs.add_def(
            type_decl_name(&node),
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

/// Gets the name from the enclosing `type` decl, or the [`Anonymous`](struct@syms::Anonymous) symbol
fn type_decl_name(ty: &impl ast::AstNode<Language = toc_syntax::Lang>) -> Symbol {
    ty.syntax()
        .parent()
        .and_then(ast::TypeDecl::cast)
        .and_then(|node| node.decl_name())
        .map_or(*syms::Anonymous, |name| {
            name.identifier_token().unwrap().text().into()
        })
}
