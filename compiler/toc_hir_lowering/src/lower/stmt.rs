//! Lowering into `Stmt` HIR nodes
use std::collections::HashMap;

use indexmap::IndexMap;
use toc_hir::symbol::{syms, IsMonitor, IsRegister, LocalDefId, SubprogramKind, SymbolKind};
use toc_hir::ty::PassBy;
use toc_hir::{
    expr, item,
    stmt::{self, Assign},
    symbol::{self, DeclareKind, ForwardKind, LimitedKind, Mutability, Symbol},
    ty,
};
use toc_span::{HasSpanTable, Span, SpanId, Spanned};
use toc_syntax::ast::{self, AstNode};

use crate::{lower::LoweredStmt, scopes::ScopeKind};

impl super::BodyLowering<'_, '_> {
    pub(super) fn lower_stmt(&mut self, stmt: ast::Stmt) -> Option<LoweredStmt> {
        let span = self.ctx.intern_range(stmt.syntax().text_range());

        let kind = match stmt {
            // `ConstVarDecl` and `BindDecl` are the only decls that can produce multiple stmts
            ast::Stmt::ConstVarDecl(decl) => return self.lower_constvar_decl(decl),
            ast::Stmt::BindDecl(decl) => return self.lower_bind_decl(decl),
            ast::Stmt::TypeDecl(decl) => self.lower_type_decl(decl),
            ast::Stmt::ProcDecl(decl) => self.lower_procedure_decl(decl),
            ast::Stmt::FcnDecl(decl) => self.lower_function_decl(decl),
            ast::Stmt::ProcessDecl(decl) => self.lower_process_decl(decl),

            ast::Stmt::ExternalDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::ForwardDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::DeferredDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::BodyDecl(_) => self.unsupported_stmt(span),

            ast::Stmt::ModuleDecl(decl) => self.lower_module_decl(decl),

            ast::Stmt::ClassDecl(_) => self.unsupported_stmt(span),
            ast::Stmt::MonitorDecl(_) => self.unsupported_stmt(span),

            ast::Stmt::AssignStmt(stmt) => self.lower_assign_stmt(stmt),

            ast::Stmt::OpenStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::CloseStmt(_) => self.unsupported_stmt(span),

            ast::Stmt::PutStmt(stmt) => self.lower_put_stmt(stmt),
            ast::Stmt::GetStmt(stmt) => self.lower_get_stmt(stmt),

            ast::Stmt::ReadStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::WriteStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::SeekStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::TellStmt(_) => self.unsupported_stmt(span),

            ast::Stmt::ForStmt(stmt) => self.lower_for_stmt(stmt),
            ast::Stmt::LoopStmt(stmt) => self.lower_loop_stmt(stmt),
            ast::Stmt::ExitStmt(stmt) => self.lower_exit_stmt(stmt),
            ast::Stmt::IfStmt(stmt) => self.lower_if_stmt(stmt),
            ast::Stmt::CaseStmt(stmt) => self.lower_case_stmt(stmt),
            ast::Stmt::BlockStmt(stmt) => self.lower_block_stmt(stmt),

            ast::Stmt::InvariantStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::AssertStmt(_) => self.unsupported_stmt(span),

            ast::Stmt::CallStmt(stmt) => self.lower_call_stmt(stmt),
            ast::Stmt::ReturnStmt(stmt) => self.lower_return_stmt(stmt),
            ast::Stmt::ResultStmt(stmt) => self.lower_result_stmt(stmt),

            ast::Stmt::NewStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::FreeStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::TagStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ForkStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::SignalStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PauseStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::QuitStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::BreakStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::CheckednessStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PreStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::InitStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::PostStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::HandlerStmt(_) => self.unsupported_stmt(span),

            // These are for versions of the statement that aren't in the correct position
            // They're already handled by `toc_validator`, so they can just be `None`
            ast::Stmt::InheritStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ImplementStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ImplementByStmt(_) => self.unsupported_stmt(span),
            ast::Stmt::ImportStmt(_) => None,
            ast::Stmt::ExportStmt(_) => None,

            ast::Stmt::PreprocGlob(_) => self.unsupported_stmt(span),
        };

        kind.map(LoweredStmt::Single)
    }

    fn unsupported_stmt(&mut self, span: SpanId) -> Option<stmt::StmtKind> {
        let span = span.lookup_in(&self.ctx.library);
        self.ctx.messages.error(
            "unsupported statement",
            "this statement is not handled yet",
            span,
        );
        None
    }

    fn unsupported_node<N: AstNode>(&mut self, node: Option<N>) {
        if let Some(node) = node {
            let span = self.ctx.mk_span(node.syntax().text_range());

            self.ctx.messages.error(
                "unsupported statement",
                "this statement is not handled yet",
                span,
            );
        }
    }

    fn lower_constvar_decl(&mut self, decl: ast::ConstVarDecl) -> Option<LoweredStmt> {
        // is actually an item
        let span = self.ctx.intern_range(decl.syntax().text_range());

        let is_pervasive = decl.pervasive_attr().is_some();
        let is_register = decl.register_attr().is_some();
        let is_var = decl.var_token().is_some();
        let mutability = Mutability::from_is_mutable(is_var);

        let type_spec = decl.type_spec().and_then(|ty| self.lower_type(ty));
        let init_expr = decl.init().map(|expr| self.ctx.lower_expr_body(expr));

        // Declare names after uses to prevent def-use cycles
        let names = self.lower_name_list(
            decl.decl_list(),
            SymbolKind::ConstVar(mutability, is_register.into()),
            is_pervasive,
        )?;

        let stmts: Vec<_> = names
            .into_iter()
            .map(|def_id| {
                let const_var = item::ConstVar {
                    is_register,
                    mutability,
                    def_id,
                    type_spec,
                    init_expr,
                };

                let item_id = self.ctx.library.add_item(item::Item {
                    kind: item::ItemKind::ConstVar(const_var),
                    def_id,
                    span,
                });

                stmt::StmtKind::Item(item_id)
            })
            .collect();

        let lowered = if stmts.len() == 1 {
            // Single declaration
            // Just a little mutable reborrow, as a treat
            let mut stmts = stmts;
            LoweredStmt::Single(stmts.pop().unwrap())
        } else {
            // Multiple declarations, pack it into a multiple
            LoweredStmt::Multiple(stmts)
        };

        Some(lowered)
    }

    fn lower_type_decl(&mut self, decl: ast::TypeDecl) -> Option<stmt::StmtKind> {
        // Procedure for resolving forward declarations (general)
        // - declare them as a forward
        //   - ScopeTracker records that there's an unresolved forward decl here
        //   - Duplicate forward reported as error, but kept track of as eventually resolving to the same thing
        // - when declaring a resolution
        //   - resolve any forwards in the same scope level
        //   - If there's already a resolution, change error report based on what the old decl was
        //     - Resolved / Declared -> Duplicate def
        //     - Forward -> Must resolve in the same scope

        let is_pervasive = decl.pervasive_attr().is_some();

        let (type_def, decl_kind) = if let Some(forward) = decl.forward_token() {
            let token_span = self.ctx.intern_range(forward.text_range());
            (
                item::DefinedType::Forward(token_span),
                DeclareKind::Forward(ForwardKind::Type, None),
            )
        } else {
            let ty = self.lower_required_type(decl.named_ty());
            (
                item::DefinedType::Alias(ty),
                DeclareKind::Resolved(ForwardKind::Type),
            )
        };

        // Declare name after type to prevent def-use cycles
        let def_id =
            self.lower_name_def(decl.decl_name()?, SymbolKind::Type, decl_kind, is_pervasive);

        let span = self.ctx.intern_range(decl.syntax().text_range());

        let item_id = self.ctx.library.add_item(item::Item {
            kind: item::ItemKind::Type(item::Type { def_id, type_def }),
            def_id,
            span,
        });

        Some(stmt::StmtKind::Item(item_id))
    }

    fn lower_bind_decl(&mut self, decl: ast::BindDecl) -> Option<LoweredStmt> {
        let stmts: Vec<_> = decl
            .bindings()
            .filter_map(|binding| {
                let mutability = if binding.as_var().is_some() {
                    Mutability::Var
                } else {
                    Mutability::Const
                };
                let is_register = binding.to_register().is_some();

                let bind_to = self.lower_required_expr_body(binding.expr());
                let def_id = self.lower_name_def(
                    binding.bind_as()?,
                    SymbolKind::Binding(mutability, is_register.into()),
                    DeclareKind::Declared,
                    false,
                );
                let span = self.ctx.intern_range(binding.syntax().text_range());

                let binding = item::Binding {
                    is_register,
                    mutability,
                    def_id,
                    bind_to,
                };

                let item_id = self.ctx.library.add_item(item::Item {
                    kind: item::ItemKind::Binding(binding),
                    def_id,
                    span,
                });

                Some(stmt::StmtKind::Item(item_id))
            })
            .collect();

        let lowered = if stmts.len() == 1 {
            // Single declaration
            // Just a little mutable reborrow, as a treat
            let mut stmts = stmts;
            LoweredStmt::Single(stmts.pop().unwrap())
        } else {
            // Multiple declarations, pack it into a multiple
            LoweredStmt::Multiple(stmts)
        };

        Some(lowered)
    }

    fn lower_procedure_decl(&mut self, decl: ast::ProcDecl) -> Option<stmt::StmtKind> {
        let subprog_header = decl.proc_header().unwrap();

        let is_pervasive = subprog_header.pervasive_attr().is_some();
        let def_id = self.lower_name_def(
            subprog_header.name()?,
            SymbolKind::Subprogram(SubprogramKind::Procedure),
            DeclareKind::Declared,
            is_pervasive,
        );
        let param_list = self.lower_formals_spec(subprog_header.params());
        let result = self.none_subprog_result(subprog_header.syntax().text_range());
        let extra = match subprog_header.device_spec().and_then(|spec| spec.expr()) {
            Some(expr) => item::SubprogramExtra::DeviceSpec(self.lower_expr_body(expr)),
            None => item::SubprogramExtra::None,
        };

        let body = self.lower_subprog_body(decl.subprog_body().unwrap(), &param_list, None);

        let span = self.ctx.intern_range(decl.syntax().text_range());
        let item_id = self.ctx.library.add_item(item::Item {
            kind: item::ItemKind::Subprogram(item::Subprogram {
                kind: symbol::SubprogramKind::Procedure,
                def_id,
                param_list,
                result,
                extra,
                body,
            }),
            def_id,
            span,
        });

        Some(stmt::StmtKind::Item(item_id))
    }

    fn lower_function_decl(&mut self, decl: ast::FcnDecl) -> Option<stmt::StmtKind> {
        let subprog_header = decl.fcn_header().unwrap();

        let is_pervasive = subprog_header.pervasive_attr().is_some();
        let def_id = self.lower_name_def(
            subprog_header.name()?,
            SymbolKind::Subprogram(SubprogramKind::Function),
            DeclareKind::Declared,
            is_pervasive,
        );
        let param_list = self.lower_formals_spec(subprog_header.params());
        let result = self.lower_subprog_result(subprog_header.fcn_result());
        let extra = item::SubprogramExtra::None;

        let body = self.lower_subprog_body(decl.subprog_body().unwrap(), &param_list, result.name);

        let span = self.ctx.intern_range(decl.syntax().text_range());
        let item_id = self.ctx.library.add_item(item::Item {
            kind: item::ItemKind::Subprogram(item::Subprogram {
                kind: symbol::SubprogramKind::Function,
                def_id,
                param_list,
                result,
                extra,
                body,
            }),
            def_id,
            span,
        });

        Some(stmt::StmtKind::Item(item_id))
    }

    fn lower_process_decl(&mut self, decl: ast::ProcessDecl) -> Option<stmt::StmtKind> {
        let subprog_header = decl.process_header().unwrap();

        let is_pervasive = subprog_header.pervasive_attr().is_some();
        let def_id = self.lower_name_def(
            subprog_header.name()?,
            SymbolKind::Subprogram(SubprogramKind::Process),
            DeclareKind::Declared,
            is_pervasive,
        );
        let param_list = self.lower_formals_spec(subprog_header.params());
        let result = self.none_subprog_result(subprog_header.syntax().text_range());
        let extra = match subprog_header.stack_size() {
            Some(expr) => item::SubprogramExtra::StackSize(self.lower_expr_body(expr)),
            None => item::SubprogramExtra::None,
        };

        let body = self.lower_subprog_body(decl.subprog_body().unwrap(), &param_list, None);

        let span = self.ctx.intern_range(decl.syntax().text_range());
        let item_id = self.ctx.library.add_item(item::Item {
            kind: item::ItemKind::Subprogram(item::Subprogram {
                kind: symbol::SubprogramKind::Process,
                def_id,
                param_list,
                result,
                extra,
                body,
            }),
            def_id,
            span,
        });

        Some(stmt::StmtKind::Item(item_id))
    }

    pub(super) fn lower_formals_spec(
        &mut self,
        formals: Option<ast::ParamSpec>,
    ) -> Option<item::ParamList> {
        use ty::Parameter;

        let formals = formals?;

        let mut param_names = vec![];
        let mut tys = vec![];

        // Prevent duplication of param names
        self.ctx.scopes.push_scope(ScopeKind::SubprogramHeader);
        {
            let missing_name = self.ctx.library.add_def(
                *syms::Unnamed,
                self.ctx.library.span_table().dummy_span(),
                None,
                DeclareKind::Declared, // TODO: Change to undeclared once verified that this is the only change
            );

            for param_def in formals.param_decl() {
                match param_def {
                    ast::ParamDecl::ConstVarParam(param) => {
                        let is_register = param.bind_to_register().is_some();
                        let pass_by = match param.pass_as_ref() {
                            None => PassBy::Value,
                            Some(_) => PassBy::Reference(Mutability::Var),
                        };
                        let coerced_type = param.coerce_type().is_some();
                        let param_ty = self.lower_required_type(param.param_ty());

                        let names = self.lower_name_list_with_missing(
                            param.param_names(),
                            SymbolKind::Param(pass_by, is_register.into()),
                            false,
                            missing_name,
                        );

                        for name in names {
                            param_names.push(name);

                            tys.push(Parameter {
                                is_register,
                                pass_by,
                                coerced_type,
                                param_ty,
                            });
                        }
                    }
                    ast::ParamDecl::SubprogType(param) => {
                        let (name, param_ty) = match param {
                            ast::SubprogType::FcnType(ty) => {
                                let name = ty.name();
                                let param_ty = self
                                    .lower_type(ast::Type::FcnType(ty))
                                    .expect("from known existing type");
                                (name, param_ty)
                            }
                            ast::SubprogType::ProcType(ty) => {
                                let name = ty.name();
                                let param_ty = self
                                    .lower_type(ast::Type::ProcType(ty))
                                    .expect("from known existing type");
                                (name, param_ty)
                            }
                        };

                        let name = name.map_or(missing_name, |name| {
                            self.lower_name_def(
                                name,
                                SymbolKind::Param(PassBy::Value, IsRegister::No),
                                DeclareKind::Declared,
                                false,
                            )
                        });
                        param_names.push(name);

                        tys.push(Parameter {
                            is_register: false,
                            pass_by: PassBy::Value,
                            coerced_type: false,
                            param_ty,
                        })
                    }
                }
            }
            self.ctx.scopes.pop_scope();
        }

        Some(item::ParamList {
            names: param_names,
            tys,
        })
    }

    fn none_subprog_result(&mut self, range: toc_span::TextRange) -> item::SubprogramResult {
        let span = self.ctx.intern_range(range);
        let void_ty = self.ctx.library.intern_type(ty::Type {
            kind: ty::TypeKind::Void,
            span,
        });

        item::SubprogramResult {
            name: None,
            ty: void_ty,
        }
    }

    fn lower_subprog_result(&mut self, result: Option<ast::FcnResult>) -> item::SubprogramResult {
        let (name, ty) = result.map_or((None, None), |result| {
            let name = result.name().map(|name| {
                self.name_to_def(
                    name,
                    // ???: Does this need to be pass by value? (can it be pass by const ref?)
                    SymbolKind::Param(PassBy::Value, IsRegister::No),
                    DeclareKind::LimitedDeclared(LimitedKind::PostCondition),
                )
            });
            (name, result.ty())
        });

        let ty = self.lower_required_type(ty);
        item::SubprogramResult { name, ty }
    }

    fn lower_subprog_body(
        &mut self,
        decl: ast::SubprogBody,
        param_list: &Option<item::ParamList>,
        result_name: Option<symbol::LocalDefId>,
    ) -> item::SubprogramBody {
        // TODO: Figure out a way of embedding these into the stmt_list
        // Probably by passing into `lower_stmt_body`
        self.unsupported_node(decl.pre_stmt());
        self.unsupported_node(decl.init_stmt());
        self.unsupported_node(decl.post_stmt());
        self.unsupported_node(decl.handler_stmt());

        let param_defs = param_list
            .as_ref()
            .map_or(vec![], |params| params.names.clone());

        self.ctx.scopes.push_scope(ScopeKind::Subprogram);
        let (body, imports) = {
            // Lowering needs to be done inside of the target scope
            let (imports, import_defs) = self.lower_import_list(decl.import_stmt());

            let (body, _) = self.ctx.lower_stmt_body(
                ScopeKind::Block,
                decl.stmt_list().unwrap(),
                param_defs,
                result_name,
                &import_defs,
            );

            (body, imports)
        };
        self.ctx.scopes.pop_scope();

        item::SubprogramBody { body, imports }
    }

    fn lower_module_decl(&mut self, decl: ast::ModuleDecl) -> Option<stmt::StmtKind> {
        let is_pervasive = decl.pervasive_attr().is_some();
        let def_id = self.lower_name_def(
            decl.name()?,
            SymbolKind::Module(IsMonitor::No),
            DeclareKind::Declared,
            is_pervasive,
        );

        self.unsupported_node(decl.implement_stmt());
        self.unsupported_node(decl.implement_by_stmt());

        self.unsupported_node(decl.pre_stmt());
        self.unsupported_node(decl.post_stmt());

        self.ctx.scopes.push_scope(ScopeKind::Module);
        let (body, declares, imports) = {
            // Build imports
            let (imports, import_defs) = self.lower_import_list(decl.import_stmt());

            if !is_pervasive {
                // Also make visible in the inner scope if it's not pervasive
                self.ctx.introduce_def(def_id, false);
            };

            // Lower the rest of the body
            let (body, declares) = self.ctx.lower_stmt_body(
                ScopeKind::Block, // We're already inside an import boundary
                decl.stmt_list().unwrap(),
                vec![],
                None,
                &import_defs,
            );

            (body, declares, imports)
        };
        self.ctx.scopes.pop_scope();

        let exports = self.lower_export_list(decl.export_stmt(), &declares);

        // Introduce unqualified exports into the current scope
        for export in exports.iter().filter(|item| {
            matches!(
                item.qualify_as,
                item::QualifyAs::Unqualified | item::QualifyAs::PervasiveUnqualified
            )
        }) {
            let is_pervasive = matches!(export.qualify_as, item::QualifyAs::PervasiveUnqualified);

            self.ctx.introduce_def(export.def_id, is_pervasive);
        }

        let span = self.ctx.intern_range(decl.syntax().text_range());
        let item_id = self.ctx.library.add_item(item::Item {
            def_id,
            kind: item::ItemKind::Module(item::Module {
                as_monitor: false,
                def_id,
                declares,
                imports,
                exports,
                body,
            }),
            span,
        });

        Some(stmt::StmtKind::Item(item_id))
    }

    // Exposed to parent mod for handling external imports
    /// This also links imported defs to the target def that it's imported to relative to the surrounding scope
    pub(super) fn lower_import_list(
        &mut self,
        imports: Option<ast::ImportStmt>,
    ) -> (Vec<item::ItemId>, Vec<LocalDefId>) {
        use std::collections::hash_map::Entry;

        let imports = if let Some(imports) = imports {
            imports
        } else {
            return Default::default();
        };

        let mut import_list = vec![];
        let mut import_defs = vec![];
        let mut already_imported = HashMap::new();

        for import in imports.imports().unwrap().import_item() {
            let ext_item = if let Some(item) = import.external_item() {
                item
            } else {
                continue;
            };

            let is_const = import.attrs().and_then(|attrs| match attrs {
                ast::ImportAttr::ConstAttr(node) => Some(node),
                _ => None,
            });
            let is_var = import.attrs().and_then(|attrs| match attrs {
                ast::ImportAttr::VarAttr(node) => Some(node),
                _ => None,
            });
            let is_forward = import.attrs().and_then(|attrs| match attrs {
                ast::ImportAttr::ForwardAttr(node) => Some(node),
                _ => None,
            });

            // Mutabilty can only be one or the other, or not specified
            let import_mut = match (is_const, is_var) {
                (None, None) => item::ImportMutability::SameAsItem,
                (Some(attr), None) => item::ImportMutability::Explicit(
                    Mutability::Const,
                    self.ctx.intern_range(attr.syntax().text_range()),
                ),
                (None, Some(attr)) => item::ImportMutability::Explicit(
                    Mutability::Var,
                    self.ctx.intern_range(attr.syntax().text_range()),
                ),
                (Some(is_const), Some(is_var)) => {
                    // Can't be both mutable and constant
                    let const_span = self.ctx.mk_span(is_const.syntax().text_range());
                    let var_span = self.ctx.mk_span(is_var.syntax().text_range());

                    // Pick reporting span to be the later one
                    let report_at = const_span.max(var_span);
                    self.ctx
                        .messages
                        .error_detailed(
                            "cannot use `const` and `var` on the same import",
                            report_at,
                        )
                        .with_error("first conflicting `const`", const_span)
                        .with_error("first conflicting `var`", var_span)
                        .finish();

                    // Just use the same mutability as the item
                    item::ImportMutability::SameAsItem
                }
            };

            // Forms:
            // Name
            // Path
            // Name in Path
            // FIXME: If we're reusing this code, places for external imports need to be handled differently
            if let Some(_path) = ext_item.path() {
                // External imports aren't allowed here
                let report_span = self.ctx.mk_span(ext_item.syntax().text_range());

                self.ctx.messages.error(
                    "cannot import external items here",
                    "importing external items is not allowed inside inner module-likes",
                    report_span,
                );

                continue;
            }
            // Skip items without names
            let name = if let Some(name) = ext_item.name() {
                name
            } else {
                continue;
            };

            // Report duplicate imports
            // ???: dealing with path imports and name in path imports
            let name_tok = name.identifier_token().unwrap();
            let name_text = name_tok.text();
            match already_imported.entry(Symbol::new(name_text)) {
                Entry::Occupied(entry) => {
                    // Already imported
                    let this_span = self.ctx.mk_span(import.syntax().text_range());
                    self.ctx
                        .messages
                        .error_detailed("import item is ignored", this_span)
                        .with_error(format!("`{name_text}` is already imported..."), this_span)
                        .with_note("by this import", *entry.get())
                        .finish();

                    continue;
                }
                Entry::Vacant(entry) => entry.insert(self.ctx.mk_span(name_tok.text_range())),
            };

            // ???: Handling forward imports :???
            // For now, we'll reject it, and properly deal with it when we deal with
            // `forward` declarations
            if let Some(is_forward) = is_forward {
                self.ctx.messages.error(
                    "unsupported import attribute",
                    "`forward` imports are not supported yet",
                    self.ctx.mk_span(is_forward.syntax().text_range()),
                );
            }

            // Make an imported def
            // Resolve it right now
            let imported_def = if let Some(def_id) = self.ctx.scopes.import_sym(name_text.into()) {
                def_id
            } else {
                // Make an undeclared
                let def_at = self.ctx.mk_span(name.syntax().text_range());
                let name = name_text;

                self.ctx.messages.error(
                    format!("`{name}` could not be imported"),
                    format!("no definitions of `{name}` found in the surrounding scope"),
                    def_at,
                );

                let def_at = self.ctx.library.intern_span(def_at);
                self.ctx
                    .library
                    .add_def(name.into(), def_at, None, symbol::DeclareKind::Undeclared)
            };

            let span = self.ctx.intern_range(import.syntax().text_range());
            let def_id = self.name_to_def(
                name,
                SymbolKind::Import,
                DeclareKind::ItemImport(imported_def),
            );
            import_defs.push(def_id);

            let item_id = self.ctx.library.add_item(item::Item {
                def_id,
                kind: item::ItemKind::Import(item::Import {
                    def_id,
                    mutability: import_mut,
                }),
                span,
            });

            import_list.push(item_id);
        }

        (import_list, import_defs)
    }

    fn lower_export_list(
        &mut self,
        exports: Option<ast::ExportStmt>,
        declares: &[item::ItemId],
    ) -> Vec<item::ExportItem> {
        let exports = if let Some(exports) = exports {
            exports
        } else {
            return vec![];
        };
        let exports_all = exports.exports().find(|item| item.all_token().is_some());

        // Deduplicate the exportable idents
        let mut exported_items = IndexMap::new();

        for item_id in declares {
            let item = self.ctx.library.item(*item_id);
            let def_info = self.ctx.library.local_def(item.def_id);
            exported_items.insert(def_info.name, *item_id);
        }

        let lower_export_attrs =
            |from_item: &ast::ExportItem, ctx: &mut crate::lower::FileLowering| {
                let is_var = from_item
                    .attrs()
                    .any(|attr| matches!(attr, ast::ExportAttr::VarAttr(_)));
                let is_unqualified = from_item
                    .attrs()
                    .any(|attr| matches!(attr, ast::ExportAttr::UnqualifiedAttr(_)));
                let is_pervasive = from_item
                    .attrs()
                    .any(|attr| matches!(attr, ast::ExportAttr::PervasiveAttr(_)));
                let is_opaque = from_item
                    .attrs()
                    .any(|attr| matches!(attr, ast::ExportAttr::OpaqueAttr(_)));

                let mutability = Mutability::from_is_mutable(is_var);
                let qualify_as = match (is_unqualified, is_pervasive) {
                    (false, false) => item::QualifyAs::Qualified,
                    (false, true) => {
                        // Pervasive exports should be unqualified
                        // or rather
                        // Attribute has no effect - pervasive exports are only meaningful for unqualified attrs
                        let pervasive_attr = from_item
                            .attrs()
                            .find_map(|attr| {
                                if let ast::ExportAttr::PervasiveAttr(attr) = attr {
                                    Some(attr)
                                } else {
                                    None
                                }
                            })
                            .unwrap();

                        let span = ctx.mk_span(pervasive_attr.syntax().text_range());
                        ctx.messages.warn("attribute has no effect", "`pervasive` only has an effect on exports when `unqualified` is also present", span);
                        item::QualifyAs::Qualified
                    }
                    (true, false) => item::QualifyAs::Unqualified,
                    (true, true) => item::QualifyAs::PervasiveUnqualified,
                };

                (mutability, qualify_as, is_opaque)
            };

        if let Some(exports_all) = exports_all {
            // Warn about ignored export items
            for exports_item in exports.exports() {
                // Skip selected `all`
                if exports_item == exports_all {
                    continue;
                }

                let export_span = self.ctx.mk_span(exports_item.syntax().text_range());
                let all_span = self
                    .ctx
                    .mk_span(exports_all.all_token().unwrap().text_range());

                if let Some(name) = exports_item.name() {
                    // Some named export
                    let name_tok = name.identifier_token().unwrap();
                    let name = name_tok.text();

                    self.ctx
                        .messages
                        .warn_detailed("export item is ignored", export_span)
                        .with_warn(format!("`{name}` is already exported..."), export_span)
                        .with_note("by this `all`", all_span)
                        .finish();
                } else if exports_item.all_token().is_some() {
                    // Duplicate `all` item
                    self.ctx
                        .messages
                        .warn_detailed("export item is ignored", export_span)
                        .with_warn("this `all` is already exported...", export_span)
                        .with_note("by this first `all`", all_span)
                        .finish();
                } else {
                    continue;
                }
            }

            let (mutability, qualify_as, is_opaque) = lower_export_attrs(&exports_all, self.ctx);
            let export_span = self
                .ctx
                .intern_range(exports_all.all_token().unwrap().text_range());

            exported_items
                .into_iter()
                .map(|(export_name, item_id)| {
                    let item = self.ctx.library.item(item_id);
                    let is_opaque = if !matches!(item.kind, item::ItemKind::Type(_)) {
                        // Opaque is only applicable to types
                        false
                    } else {
                        is_opaque
                    };

                    // Don't need to report non-applicable mutability here, since it's applied
                    // only when it is applicable
                    let mutability = if let item::ItemKind::ConstVar(cv) = &item.kind {
                        // Only carry through mutability if it was mutable in the first place
                        Mutability::from_is_mutable(
                            mutability == Mutability::Var && cv.mutability == Mutability::Var,
                        )
                    } else {
                        // Non-ConstVars are always immutable
                        Mutability::Const
                    };

                    let item_def = item.def_id;
                    let def_id = self.ctx.library.add_def(
                        export_name,
                        export_span,
                        Some(SymbolKind::Export),
                        DeclareKind::ItemExport(item_def),
                    );

                    item::ExportItem {
                        def_id,
                        mutability,
                        qualify_as,
                        is_opaque,
                        item_id,
                    }
                })
                .collect()
        } else {
            let mut already_exported = HashMap::new();

            exports
                .exports()
                .flat_map(|exports_item| {
                    let (mutability, qualify_as, is_opaque) =
                        lower_export_attrs(&exports_item, self.ctx);
                    let name = exports_item.name()?;
                    let name_tok = name.identifier_token().unwrap();
                    let name_text = Symbol::new(name_tok.text());

                    // Warn about duplicate exports
                    match already_exported.entry(name_text) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            // Already exported
                            // FIXME: Raise level to an error
                            let this_span = self.ctx.mk_span(exports_item.syntax().text_range());
                            self.ctx
                                .messages
                                .warn_detailed("export item is ignored", this_span)
                                .with_warn(
                                    format!("`{name_text}` is already exported..."),
                                    this_span,
                                )
                                .with_note("by this export", *entry.get())
                                .finish();

                            // Skip this export
                            return None;
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            // Not exported yet
                            entry.insert(self.ctx.mk_span(exports_item.syntax().text_range()));
                        }
                    }

                    let item = exported_items.get(&name_text).copied();

                    if let Some(item_id) = item {
                        let item = self.ctx.library.item(item_id);

                        // Report when opaqueness is not applicable
                        let is_opaque = if is_opaque
                            && !matches!(item.kind, item::ItemKind::Type(_))
                        {
                            let opaque_attr = exports_item
                                .attrs()
                                .find_map(|attr| {
                                    if let ast::ExportAttr::OpaqueAttr(attr) = attr {
                                        Some(attr)
                                    } else {
                                        None
                                    }
                                })
                                .unwrap();

                            let opaque_span = self.ctx.mk_span(opaque_attr.syntax().text_range());
                            let def_info = self.ctx.library.local_def(item.def_id);
                            let def_name = def_info.name;
                            let def_span = def_info.def_at.lookup_in(&self.ctx.library);

                            self.ctx
                                .messages
                                .error_detailed("cannot use `opaque` here", opaque_span)
                                .with_error(
                                    "`opaque` attribute can only be applied to types",
                                    opaque_span,
                                )
                                .with_note(format!("`{def_name}` declared here"), def_span)
                                .finish();

                            // Don't carry it through
                            false
                        } else {
                            is_opaque
                        };

                        // Report when mutability isn't applicable
                        let is_mutability_applicable =
                            if let item::ItemKind::ConstVar(cv) = &item.kind {
                                // Only applicable if it was mutable in the first place
                                cv.mutability == Mutability::Var
                            } else {
                                // Not applicable to any other item
                                false
                            };

                        let mutability =
                            if !is_mutability_applicable && mutability == Mutability::Var {
                                let var_attr = exports_item
                                    .attrs()
                                    .find_map(|attr| {
                                        if let ast::ExportAttr::VarAttr(attr) = attr {
                                            Some(attr)
                                        } else {
                                            None
                                        }
                                    })
                                    .unwrap();

                                let var_span = self.ctx.mk_span(var_attr.syntax().text_range());
                                let def_info = self.ctx.library.local_def(item.def_id);
                                let def_name = def_info.name;
                                let def_span = def_info.def_at.lookup_in(&self.ctx.library);

                                self.ctx
                                    .messages
                                    .error_detailed("cannot use `var` here", var_span)
                                    .with_error(
                                        "`var` attribute can only be applied to variables",
                                        var_span,
                                    )
                                    .with_note(format!("`{def_name}` declared here"), def_span)
                                    .finish();

                                // Don't carry it through
                                Mutability::Const
                            } else {
                                mutability
                            };

                        let item_def = item.def_id;
                        let export_span = self.ctx.intern_range(name_tok.text_range());
                        let def_id = self.ctx.library.add_def(
                            name_text,
                            export_span,
                            Some(SymbolKind::Export),
                            DeclareKind::ItemExport(item_def),
                        );

                        Some(item::ExportItem {
                            def_id,
                            mutability,
                            qualify_as,
                            is_opaque,
                            item_id,
                        })
                    } else {
                        let span = Span::new(self.ctx.file, name.syntax().text_range());
                        let name = name.identifier_token().unwrap();
                        self.ctx.messages.error(
                            format!("exported symbol `{name}` has not been declared"),
                            "not declared at the top level of this module",
                            span,
                        );

                        None
                    }
                })
                .collect()
        }
    }

    fn lower_assign_stmt(&mut self, stmt: ast::AssignStmt) -> Option<stmt::StmtKind> {
        let (op, asn_span) = {
            let asn_op = stmt.asn_op()?;
            let span = self.ctx.intern_range(asn_op.syntax().text_range());

            (asn_op.asn_kind().and_then(asn_to_bin_op), span)
        };

        // Only lhs is required to generate a node
        let lhs = self.lower_expr(stmt.lhs()?);
        let rhs = self.lower_required_expr(stmt.rhs());

        let rhs = if let Some(op) = op {
            // Insert binary expression
            let op = Spanned::new(op, asn_span);
            let kind = expr::ExprKind::Binary(expr::Binary { lhs, op, rhs });
            // Take the span of the assignment stmt
            let span = self.ctx.intern_range(stmt.syntax().text_range());

            self.body.add_expr(expr::Expr { kind, span })
        } else {
            rhs
        };

        Some(stmt::StmtKind::Assign(Assign {
            lhs,
            asn: asn_span,
            rhs,
        }))
    }

    fn lower_put_stmt(&mut self, stmt: ast::PutStmt) -> Option<stmt::StmtKind> {
        let stream_num = self.try_lower_expr(stmt.stream_num().and_then(|s| s.expr()));
        let items = stmt
            .items()
            .filter_map(|item| {
                if item.skip_token().is_some() {
                    Some(stmt::Skippable::Skip)
                } else if let Some(expr) = item.expr() {
                    let expr = self.lower_expr(expr);

                    let opts = {
                        let width = item.width().and_then(|o| o.expr());
                        let precision = item.fraction().and_then(|o| o.expr());
                        let exponent_width = item.exp_width().and_then(|o| o.expr());

                        if let Some(exponent_width) = exponent_width {
                            stmt::PutOpts::WithExponentWidth {
                                width: self.lower_required_expr(width),
                                precision: self.lower_required_expr(precision),
                                exponent_width: self.lower_required_expr(Some(exponent_width)),
                            }
                        } else if let Some(precision) = precision {
                            stmt::PutOpts::WithPrecision {
                                width: self.lower_required_expr(width),
                                precision: self.lower_required_expr(Some(precision)),
                            }
                        } else if let Some(width) = width {
                            stmt::PutOpts::WithWidth {
                                width: self.lower_required_expr(Some(width)),
                            }
                        } else {
                            stmt::PutOpts::None
                        }
                    };

                    let item = stmt::PutItem { expr, opts };

                    Some(stmt::Skippable::Item(item))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        // Presence means newline should be omitted
        let append_newline = stmt.range_token().is_none();

        if items.is_empty() && stream_num.is_none() {
            // there must be something present
            None
        } else {
            Some(stmt::StmtKind::Put(stmt::Put {
                stream_num,
                items,
                append_newline,
            }))
        }
    }

    fn lower_get_stmt(&mut self, stmt: ast::GetStmt) -> Option<stmt::StmtKind> {
        let stream_num = self.try_lower_expr(stmt.stream_num().and_then(|s| s.expr()));
        let items = stmt
            .items()
            .filter_map(|item| {
                if item.skip_token().is_some() {
                    Some(stmt::Skippable::Skip)
                } else if let Some(ref_expr) = item.expr() {
                    let expr = self.lower_expr(ref_expr);
                    let width = match item.get_width() {
                        None => stmt::GetWidth::Token,
                        Some(width) if width.star_token().is_some() => stmt::GetWidth::Line,
                        Some(width) => {
                            // Should be an expr for the count
                            stmt::GetWidth::Chars(self.lower_required_expr(width.expr()))
                        }
                    };

                    Some(stmt::Skippable::Item(stmt::GetItem { expr, width }))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if items.is_empty() && stream_num.is_none() {
            // there must be something present
            None
        } else {
            Some(stmt::StmtKind::Get(stmt::Get { stream_num, items }))
        }
    }

    fn lower_for_stmt(&mut self, stmt: ast::ForStmt) -> Option<stmt::StmtKind> {
        let is_decreasing = stmt.decreasing_token().is_some();
        let name = stmt.name();

        let for_bounds = {
            let for_bounds = stmt.for_bounds().unwrap();

            if for_bounds.range_token().is_some() {
                // Explicit bounds
                stmt::ForBounds::Full(
                    self.lower_required_expr(for_bounds.start()),
                    self.lower_required_expr(for_bounds.end()),
                )
            } else if let Some(start) = for_bounds.start() {
                // Implicit bounds
                stmt::ForBounds::Implicit(self.lower_expr(start))
            } else {
                // Make a dummy for-loop bounds
                // Full bounds satisfies all cases
                stmt::ForBounds::Full(
                    self.lower_required_expr(None),
                    self.lower_required_expr(None),
                )
            }
        };

        let step_by = stmt
            .steps()
            .and_then(|step_by| step_by.expr())
            .map(|expr| self.lower_expr(expr));

        let counter_def;
        let body_stmts;

        self.ctx.scopes.push_scope(ScopeKind::Loop);
        {
            // counter is only available inside of the loop body
            counter_def = name.map(|name| {
                self.lower_name_def(
                    name,
                    SymbolKind::ConstVar(Mutability::Const, IsRegister::No),
                    symbol::DeclareKind::Declared,
                    false,
                )
            });
            body_stmts = self.lower_stmt_list(stmt.stmt_list().unwrap());
        }
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::For(stmt::For {
            is_decreasing,
            counter_def,
            bounds: for_bounds,
            step_by,
            stmts: body_stmts,
        }))
    }

    fn lower_loop_stmt(&mut self, stmt: ast::LoopStmt) -> Option<stmt::StmtKind> {
        self.ctx.scopes.push_scope(ScopeKind::Loop);
        let stmts = self.lower_stmt_list(stmt.stmt_list().unwrap());
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::Loop(stmt::Loop { stmts }))
    }

    fn lower_exit_stmt(&mut self, stmt: ast::ExitStmt) -> Option<stmt::StmtKind> {
        Some(stmt::StmtKind::Exit(stmt::Exit {
            when_condition: self.try_lower_expr(stmt.condition()),
        }))
    }

    fn lower_if_stmt(&mut self, stmt: ast::IfStmt) -> Option<stmt::StmtKind> {
        Some(self.lower_if_stmt_body(stmt.if_body().unwrap()))
    }

    fn lower_if_stmt_body(&mut self, if_body: ast::IfBody) -> stmt::StmtKind {
        let condition = self.lower_required_expr(if_body.condition());

        // Create block stmt for the true branch
        self.ctx.scopes.push_scope(ScopeKind::Block);
        let true_branch =
            self.lower_stmt_list_to_block(if_body.true_branch().unwrap(), ScopeKind::Block);
        self.ctx.scopes.pop_scope();

        // Handle the false branch
        let false_branch = if_body.false_branch().map(|branch| match branch {
            ast::FalseBranch::ElseStmt(stmt) => {
                // Simple, just lower to a stmt block
                self.ctx.scopes.push_scope(ScopeKind::Block);
                let else_branch =
                    self.lower_stmt_list_to_block(stmt.stmt_list().unwrap(), ScopeKind::Block);
                self.ctx.scopes.pop_scope();

                else_branch
            }
            ast::FalseBranch::ElseifStmt(stmt) => {
                // Also simple, just reuse our lowering of if bodies
                let range = stmt.syntax().text_range();
                let span = self.ctx.library.intern_span(self.ctx.mk_span(range));
                let kind = self.lower_if_stmt_body(stmt.if_body().unwrap());

                self.body.add_stmt(stmt::Stmt { kind, span })
            }
        });

        stmt::StmtKind::If(stmt::If {
            condition,
            true_branch,
            false_branch,
        })
    }

    fn lower_case_stmt(&mut self, stmt: ast::CaseStmt) -> Option<stmt::StmtKind> {
        let discriminant = self.lower_required_expr(stmt.expr());
        let case_arms = stmt
            .case_arm()
            .map(|arm| {
                let selectors: Vec<_> = arm
                    .select()
                    .map(|selectors| {
                        selectors
                            .exprs()
                            .map(|expr| self.lower_expr(expr))
                            .collect()
                    })
                    .unwrap_or_default();

                let selectors = if selectors.is_empty() {
                    stmt::CaseSelector::Default
                } else {
                    stmt::CaseSelector::Exprs(selectors)
                };

                self.ctx.scopes.push_scope(ScopeKind::Block);
                let stmts = self.lower_stmt_list(arm.stmt_list().unwrap());
                self.ctx.scopes.pop_scope();

                stmt::CaseArm { selectors, stmts }
            })
            .collect();

        Some(stmt::StmtKind::Case(stmt::Case {
            discriminant,
            arms: case_arms,
        }))
    }

    fn lower_block_stmt(&mut self, stmt: ast::BlockStmt) -> Option<stmt::StmtKind> {
        let stmt_list = stmt.stmt_list()?;

        self.ctx.scopes.push_scope(ScopeKind::Block);
        let stmts = self.lower_stmt_list(stmt_list);
        self.ctx.scopes.pop_scope();

        Some(stmt::StmtKind::Block(stmt::Block {
            kind: stmt::BlockKind::Normal,
            stmts,
        }))
    }

    fn lower_call_stmt(&mut self, stmt: ast::CallStmt) -> Option<stmt::StmtKind> {
        let call = match stmt.expr() {
            Some(ast::Expr::CallExpr(call)) => {
                let lhs = self.lower_required_expr(call.expr());
                let arguments = self.lower_expr_arg_list(call.param_list());

                stmt::Call { lhs, arguments }
            }
            other_expr => {
                let lhs = self.lower_required_expr(other_expr);

                stmt::Call {
                    lhs,
                    arguments: None,
                }
            }
        };

        Some(stmt::StmtKind::Call(call))
    }

    fn lower_return_stmt(&mut self, _stmt: ast::ReturnStmt) -> Option<stmt::StmtKind> {
        Some(stmt::StmtKind::Return(stmt::Return))
    }

    fn lower_result_stmt(&mut self, stmt: ast::ResultStmt) -> Option<stmt::StmtKind> {
        Some(stmt::StmtKind::Result(stmt::Result {
            expr: self.lower_required_expr(stmt.expr()),
        }))
    }
}

impl super::BodyLowering<'_, '_> {
    // Utils //

    fn lower_stmt_list_to_block(
        &mut self,
        stmt_list: ast::StmtList,
        as_kind: ScopeKind,
    ) -> stmt::StmtId {
        let range = stmt_list.syntax().text_range();

        self.ctx.scopes.push_scope(as_kind);
        let stmts = self.lower_stmt_list(stmt_list);
        self.ctx.scopes.pop_scope();

        let kind = stmt::StmtKind::Block(stmt::Block {
            kind: stmt::BlockKind::Normal,
            stmts,
        });
        let span = self.ctx.library.intern_span(self.ctx.mk_span(range));

        self.body.add_stmt(toc_hir::stmt::Stmt { kind, span })
    }

    /// Lowers a name list, holding up the invariant that it always contains
    /// at least one identifier
    fn lower_name_list(
        &mut self,
        name_list: Option<ast::NameList>,
        kind: SymbolKind,
        is_pervasive: bool,
    ) -> Option<Vec<symbol::LocalDefId>> {
        let names = name_list?
            .names()
            .map(|name| {
                self.lower_name_def(name, kind, symbol::DeclareKind::Declared, is_pervasive)
            })
            .collect::<Vec<_>>();

        // Invariant: Names list must contain at least one name
        Some(names).filter(|names| !names.is_empty())
    }

    /// Lowers a name list, filling empty name places with `fill_with`
    fn lower_name_list_with_missing(
        &mut self,
        name_list: Option<ast::NameList>,
        kind: SymbolKind,
        is_pervasive: bool,
        fill_with: symbol::LocalDefId,
    ) -> Vec<symbol::LocalDefId> {
        let names = name_list.map_or_else(
            || vec![fill_with],
            |name_list| {
                name_list
                    .names_with_missing()
                    .map(|name| match name {
                        Some(name) => self.lower_name_def(
                            name,
                            kind,
                            symbol::DeclareKind::Declared,
                            is_pervasive,
                        ),
                        None => fill_with,
                    })
                    .collect::<Vec<_>>()
            },
        );

        // Invariant: Names list must contain at least one name
        debug_assert!(!names.is_empty());
        names
    }

    fn lower_name_def(
        &mut self,
        name: ast::Name,
        kind: SymbolKind,
        declare_kind: DeclareKind,
        is_pervasive: bool,
    ) -> symbol::LocalDefId {
        // Can't declare an undefined symbol from a name def
        assert_ne!(declare_kind, DeclareKind::Undeclared);

        let def_id = self.name_to_def(name, kind, declare_kind);

        // Bring into scope
        self.ctx.introduce_def(def_id, is_pervasive);

        def_id
    }

    fn name_to_def(
        &mut self,
        name: ast::Name,
        kind: SymbolKind,
        declare_kind: DeclareKind,
    ) -> symbol::LocalDefId {
        let token = name.identifier_token().unwrap();
        let span = self.ctx.intern_range(token.text_range());
        self.ctx
            .library
            .add_def(token.text().into(), span, Some(kind), declare_kind)
    }
}

fn asn_to_bin_op(op: toc_syntax::AssignOp) -> Option<expr::BinaryOp> {
    Some(match op {
        toc_syntax::AssignOp::None => return None,
        toc_syntax::AssignOp::Add => expr::BinaryOp::Add,
        toc_syntax::AssignOp::Sub => expr::BinaryOp::Sub,
        toc_syntax::AssignOp::Mul => expr::BinaryOp::Mul,
        toc_syntax::AssignOp::Div => expr::BinaryOp::Div,
        toc_syntax::AssignOp::RealDiv => expr::BinaryOp::RealDiv,
        toc_syntax::AssignOp::Mod => expr::BinaryOp::Mod,
        toc_syntax::AssignOp::Rem => expr::BinaryOp::Rem,
        toc_syntax::AssignOp::Exp => expr::BinaryOp::Exp,
        toc_syntax::AssignOp::And => expr::BinaryOp::And,
        toc_syntax::AssignOp::Or => expr::BinaryOp::Or,
        toc_syntax::AssignOp::Xor => expr::BinaryOp::Xor,
        toc_syntax::AssignOp::Shl => expr::BinaryOp::Shl,
        toc_syntax::AssignOp::Shr => expr::BinaryOp::Shr,
        toc_syntax::AssignOp::Imply => expr::BinaryOp::Imply,
    })
}
