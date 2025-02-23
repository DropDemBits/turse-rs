//! Code generation backend based on the HIR tree

use indexmap::IndexMap;
use toc_analysis::{db::HirAnalysis, ty};
use toc_hir::{
    body as hir_body, expr as hir_expr, item as hir_item, package as hir_package,
    package::InPackage,
    stmt as hir_stmt,
    symbol::{DefId, LocalDefId},
};
use toc_reporting::CompileResult;
use toc_span::{FileId, Span};
use turing_bytecode::{
    encode::{
        section::{ManifestAllocator, ManifestSlot, SectionData},
        BytecodeBlob, BytecodeBuilder, CodeOffset, CodeUnitRef, LocalSlot, Procedure,
        ProcedureBuilder, ProcedureRef, RelocHandle, RelocTarget, TemporarySlot,
    },
    instruction::{
        AbortReason, CheckKind, GetKind, PutKind, RelocatableOffset, StdStreamKind, StreamKind,
    },
};

use crate::instruction::ForDescriptor;

mod instruction;

pub trait CodeGenDB: HirAnalysis {}

impl<T> CodeGenDB for T where T: HirAnalysis {}

#[derive(Debug, Default)]
struct CodeGenCtx {
    blob: BytecodeBuilder,
    file_map: IndexMap<FileId, CodeUnitRef>,
    manifest_allocator: ManifestAllocator,
    // deduplicated strings list
    strings: IndexMap<String, Vec<(ProcedureRef, RelocHandle)>>,
    procedure_bodies: IndexMap<(hir_package::PackageId, hir_body::BodyId), ProcedureRef>,
    procedure_relocs: IndexMap<ProcedureRef, Vec<(DefId, Vec<RelocHandle>)>>,
}

/// Generates code from the given HIR database,
/// or producing nothing if an error was encountered before code generation.
pub fn generate_code(db: &dyn CodeGenDB) -> CompileResult<Option<BytecodeBlob>> {
    // Bail if there are any errors from analysis
    let res = db.analyze_packages();

    if res.messages().has_errors() {
        return res.map(|_| None);
    }

    // Start producing blobs for each package
    // Only deal with one package right now
    let pkg_graph = toc_source_graph::source_graph(db.up()).as_ref().unwrap();
    let mut cctx = CodeGenCtx::default();

    // Generate <No File> unit first
    {
        let mut unit = cctx.blob.build_code_unit("<No File>");
        let mut proc = unit.build_procedure();
        proc.ins().return_();
        unit.submit_procedure(proc.finish());
        cctx.blob.submit_code_unit(unit.finish());
    }

    // ???: How do we figure out which file will serve as the main body?
    // For now, we only deal with one file, so that will always serve as the main body
    // TODO: toposort calls to module initialization bodies

    if let Some(&package_id) = pkg_graph.all_packages(db.up()).first() {
        let root_file = package_id.root(db.up());
        let mut main_unit = cctx
            .blob
            .build_code_unit(root_file.raw_path(db.up()).as_str());

        // This package will act as the main file
        let package = db.package(package_id.into());
        let main_body = {
            let item_id = package
                .root_items
                .get(&FileId::from(root_file))
                .expect("no item for this file");
            if let hir_item::ItemKind::Module(a) = &package.item(*item_id).kind {
                a.body
            } else {
                unreachable!()
            }
        };

        // Reserve the first procedure for the entry point trampoline
        let mut entry_point_proc = main_unit.build_procedure();

        // Generate code for each statement body
        for body_id in package.body_ids() {
            let body = package.body(body_id);

            match &body.kind {
                hir_body::BodyKind::Stmts(_, params, ret_param) => {
                    // For simple statements (e.g invariant, assign, constvar init)
                    // we can deal with them easily as they correspond to a linear
                    // sequence of instructions.
                    //
                    // For control flow statements, we need to keep track of where we should
                    // branch to, thus we can't fall back on simple HIR walking.
                    let body_code = BodyCodeGenerator::generate_body(
                        db,
                        &mut cctx,
                        main_unit.build_procedure(),
                        package_id.into(),
                        package.as_ref(),
                        body_id,
                        params,
                        *ret_param,
                    );

                    println!("gen code:");
                    for (idx, instr) in body_code.proc.instrs().iter().enumerate() {
                        println!("{idx:4}    {instr:?}");
                    }

                    cctx.procedure_bodies
                        .insert((package_id.into(), body_id), body_code.proc.id());
                    cctx.procedure_relocs.insert(
                        body_code.proc.id(),
                        body_code.external_relocs.into_iter().collect(),
                    );

                    main_unit.submit_procedure(body_code.proc);
                }
                hir_body::BodyKind::Exprs(_expr) => {
                    // We don't start code generation from expr bodies
                    // (those are associated with `const` and `var`s)
                    continue;
                }
            }
        }

        #[derive(Debug)]
        enum DeferredTarget {
            Manifest(ManifestSlot),
            Procedure(ProcedureRef),
        }

        let mut deferred_targets = vec![];

        // Allocate interned strings
        for (text, proc_relocs) in std::mem::take(&mut cctx.strings) {
            // Convert to nul-terminated raw bytes
            let mut bytes = text.into_bytes();
            bytes.push(0);

            let slot = cctx
                .manifest_allocator
                .alloc(SectionData::Data(bytes.into_boxed_slice()), 4);

            deferred_targets.extend(
                proc_relocs
                    .into_iter()
                    .map(|(proc, handle)| (proc, handle, DeferredTarget::Manifest(slot))),
            );
        }

        main_unit.submit_manifest_section(std::mem::take(&mut cctx.manifest_allocator).finish());

        // Indirectly resolve externals
        for (proc, relocs) in std::mem::take(&mut cctx.procedure_relocs) {
            for (external_def, proc_relocs) in relocs {
                let Some(item) = db.item_of(external_def) else {
                    panic!("not an item {external_def:?}");
                };

                assert_eq!(item.package(), package_id.into());
                let package = db.package(item.package());
                let item = package.item(item.item());

                match &item.kind {
                    hir_item::ItemKind::ConstVar(_) => unimplemented!(),
                    hir_item::ItemKind::Subprogram(subprogram) => {
                        let Some(other_proc_ref) = cctx
                            .procedure_bodies
                            .get(&(package_id.into(), subprogram.body.body))
                        else {
                            panic!(
                                "no code generated for {:?} in {package_id:?}",
                                subprogram.body.body
                            );
                        };

                        deferred_targets.extend(proc_relocs.into_iter().map(|handle| {
                            (proc, handle, DeferredTarget::Procedure(*other_proc_ref))
                        }));
                    }
                    hir_item::ItemKind::Type(_)
                    | hir_item::ItemKind::Binding(_)
                    | hir_item::ItemKind::Module(_)
                    | hir_item::ItemKind::Import(_) => unreachable!(),
                }
            }
        }

        // Fill entrypoint procedure with trampoline
        cctx.blob.main_unit(main_unit.id());

        let (entry_point_id, entry_point_reloc) = {
            let proc = &mut entry_point_proc;
            let id = proc.id();

            let reloc = proc.reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));
            proc.ins().call(0);
            proc.ins().return_();
            main_unit.submit_procedure(entry_point_proc.finish());

            (id, reloc)
        };

        let main_unit_id = main_unit.id();
        cctx.blob.submit_code_unit(main_unit.finish());
        let entry_proc = cctx
            .procedure_bodies
            .get(&(package_id.into(), main_body))
            .copied()
            .expect("main body should have code");
        let entry_proc = cctx.blob[main_unit_id].procedure_offset(entry_proc);

        cctx.blob[main_unit_id].add_code_relocs([(
            entry_point_id,
            entry_point_reloc,
            RelocTarget::Local(entry_proc),
        )]);

        // Add resolutions for the rest of the relocs
        let reloc_targets: Vec<_> = deferred_targets
            .into_iter()
            .map(|(proc, handle, target)| {
                let target = match target {
                    DeferredTarget::Manifest(slot) => cctx.blob[main_unit_id].manifest_offset(slot),
                    DeferredTarget::Procedure(procedure) => {
                        cctx.blob[main_unit_id].procedure_offset(procedure)
                    }
                };

                (proc, handle, RelocTarget::Local(target))
            })
            .collect();

        cctx.blob[main_unit_id].add_code_relocs(reloc_targets.into_iter());
    } else {
        unreachable!()
    }

    let blob = cctx.blob.finish();

    eprintln!("generated blob: {blob:#x?}");

    res.map(|_| Some(blob))
}

#[derive(Debug, Clone, Copy)]
enum BlockBranches {
    Loop {
        _loop_start: CodeOffset,
        after_loop: CodeOffset,
    },
    For {
        _loop_start: CodeOffset,
        after_loop: CodeOffset,
    },
}

struct BodyCode {
    proc: Procedure,
    external_relocs: indexmap::IndexMap<DefId, Vec<RelocHandle>>,
}

#[derive(Debug, Clone, Copy)]
enum AssignOrder {
    Precomputed,
    Postcomputed,
}

struct BodyCodeGenerator<'a> {
    db: &'a dyn CodeGenDB,
    package_id: hir_package::PackageId,
    package: &'a hir_package::Package,
    body_id: hir_body::BodyId,
    body: &'a hir_body::Body,

    cctx: &'a mut CodeGenCtx,
    def_bindings: DefBindings,
    proc: ProcedureBuilder,
    last_location: Option<(usize, usize)>,
    block_stack: Vec<BlockBranches>,
}

impl BodyCodeGenerator<'_> {
    fn generate_body(
        db: &dyn CodeGenDB,
        codegen_ctx: &mut CodeGenCtx,
        proc: ProcedureBuilder,
        package_id: hir_package::PackageId,
        package: &hir_package::Package,
        body_id: hir_body::BodyId,
        param_defs: &[LocalDefId],
        ret_param: Option<LocalDefId>,
    ) -> BodyCode {
        let mut gen = BodyCodeGenerator {
            db,
            package_id,
            package,
            body_id,
            body: package.body(body_id),
            // code_fragment: &mut code_fragment,
            cctx: codegen_ctx,
            def_bindings: DefBindings::default(),
            proc,
            last_location: None,
            block_stack: vec![],
        };

        // FIXME: Bind imported defs
        gen.bind_inputs(param_defs, ret_param);

        match &package.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, ..) => gen.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }

        // Generate the appropriate footer
        {
            let item_owner = match gen
                .db
                .body_owner(InPackage(gen.package_id, gen.body_id))
                .expect("from stmt body")
            {
                hir_body::BodyOwner::Item(item_id) => item_id,
                hir_body::BodyOwner::Type(_) | hir_body::BodyOwner::Expr(_) => unreachable!(),
            };
            let item_ty = gen
                .db
                .type_of((gen.package_id, item_owner).into())
                .to_base_type(gen.db.up());

            if matches!(
                item_ty.kind(gen.db.up()),
                ty::TypeKind::Subprogram(toc_hir::symbol::SubprogramKind::Function, ..)
            ) {
                gen.proc.ins().abort(AbortReason::NoResult);
            } else {
                gen.proc.ins().return_();
            }
        }

        BodyCode {
            proc: gen.proc.finish(),
            external_relocs: gen.def_bindings.external_defs,
        }
    }

    fn inline_body(&mut self, body_id: hir_body::BodyId) {
        // Temporarily switch to lowering the inline body
        let (old_body_id, old_body) = (self.body_id, self.body);

        self.body_id = body_id;
        self.body = self.package.body(body_id);

        match &self.package.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, ..) => self.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => self.generate_expr(*expr),
        }

        self.body_id = old_body_id;
        self.body = old_body;
    }

    fn bind_inputs(&mut self, param_defs: &[LocalDefId], ret_param: Option<LocalDefId>) {
        let db = self.db;

        let item_owner = db
            .body_owner(InPackage(self.package_id, self.body_id))
            .expect("unowned body");
        let item_owner = match item_owner {
            hir_body::BodyOwner::Item(item) => item,
            hir_body::BodyOwner::Type(_) | hir_body::BodyOwner::Expr(_) => unreachable!(),
        };
        let item_ty = db
            .type_of((self.package_id, item_owner).into())
            .to_base_type(self.db.up());

        if let ty::TypeKind::Subprogram(_, params, result_ty) = item_ty.kind(self.db.up()) {
            let mut arg_offset = 0;

            // Feed in ret ty
            let ret_ty = result_ty.to_base_type(self.db.up());

            if !matches!(ret_ty.kind(self.db.up()), ty::TypeKind::Void) {
                if let Some(local_def) = ret_param {
                    self.def_bindings.bind_parameter(
                        DefId(self.package_id, local_def),
                        arg_offset as u32,
                        true,
                    );
                }

                arg_offset += 4;
            }

            // Feed in params
            if let Some(param_infos) = params.as_ref() {
                for (local_def, param_info) in param_defs.iter().zip(param_infos) {
                    // ???: How should we deal with char(*) / string(*)?
                    // - Need to recover ABI
                    let param_ty = param_info.param_ty.to_base_type(self.db.up());
                    let size_of = param_ty.size_of(self.db.up()).expect("must be sized");
                    let indirect = match param_info.pass_by {
                        ty::PassBy::Value => false,
                        ty::PassBy::Reference(_) => true,
                    };

                    self.def_bindings.bind_parameter(
                        DefId(self.package_id, *local_def),
                        arg_offset as u32,
                        indirect,
                    );
                    arg_offset += if indirect { 4 } else { size_of };
                }
            }
        }
    }

    fn emit_location(&mut self, span: Span) {
        let (file, range) = span.into_parts().unwrap();
        let info = toc_ast_db::map_byte_index(self.db.up(), file.into_raw(), range.start().into())
            .unwrap();

        let code_file = self
            .cctx
            .file_map
            .entry(file)
            .or_insert_with(|| {
                // Create a dummy unit to represent the filename
                let path = file.into_raw().raw_path(self.db.up());

                let mut unit = self.cctx.blob.build_code_unit(path.as_str());
                let id = unit.id();

                let mut proc = unit.build_procedure();
                proc.ins().return_();
                unit.submit_procedure(proc.finish());
                self.cctx.blob.submit_code_unit(unit.finish());

                id
            })
            .as_usize();

        // `LineInfo` line is zero-based, so adjust it
        let (new_file, new_line) = (code_file as u16, info.line as u16 + 1);

        if let Some((last_file, last_line)) = self.last_location.replace((code_file, info.line + 1))
        {
            if (last_file, last_line) == (code_file, info.line) {
                // Same location, don't need to emit anything
            } else if last_file != code_file {
                // Different file, file absolute location
                self.proc.ins().setfileno(new_file, new_line);
            } else if info.line.checked_sub(last_line) == Some(1) {
                // Can get away with a line relative location
                self.proc.ins().inclineno();
            } else {
                // Need a line absolute location
                self.proc.ins().setlineno(new_line);
            }
        } else {
            // Start of body, need a file absolute location
            self.proc.ins().setfileno(new_file, new_line);
        };
    }

    fn emit_absolute_location(&mut self) {
        if let Some((file, line)) = self.last_location {
            self.proc.ins().setfileno(file as u16, line as u16);
        }
    }

    fn generate_stmt(&mut self, stmt_id: hir_stmt::StmtId) {
        let stmt = self.body.stmt(stmt_id);
        let span = stmt.span.lookup_in(self.package);
        self.emit_location(span);

        // Ensure that temporaries don't leak between statements
        let old_scope = self.proc.enter_temporary_scope();
        match &stmt.kind {
            hir_stmt::StmtKind::Item(item_id) => self.generate_item(*item_id),
            hir_stmt::StmtKind::Assign(stmt) => self.generate_stmt_assign(stmt),
            hir_stmt::StmtKind::Put(stmt) => self.generate_stmt_put(stmt),
            hir_stmt::StmtKind::Get(stmt) => self.generate_stmt_get(stmt),
            hir_stmt::StmtKind::For(stmt) => self.generate_stmt_for(stmt),
            hir_stmt::StmtKind::Loop(stmt) => self.generate_stmt_loop(stmt),
            hir_stmt::StmtKind::Exit(stmt) => self.generate_stmt_exit(stmt),
            hir_stmt::StmtKind::If(stmt) => self.generate_stmt_if(stmt),
            hir_stmt::StmtKind::Case(stmt) => self.generate_stmt_case(stmt),
            hir_stmt::StmtKind::Block(stmt) => self.generate_stmt_list(&stmt.stmts),
            hir_stmt::StmtKind::Call(stmt) => self.generate_stmt_call(stmt),
            hir_stmt::StmtKind::Return(stmt) => self.generate_stmt_return(stmt),
            hir_stmt::StmtKind::Result(stmt) => self.generate_stmt_result(stmt),
        }
        self.proc.leave_temporary_scope(old_scope);
    }

    fn generate_stmt_list(&mut self, stmts: &[hir_stmt::StmtId]) {
        for stmt_id in stmts {
            self.generate_stmt(*stmt_id)
        }
    }

    fn generate_stmt_assign(&mut self, stmt: &hir_stmt::Assign) {
        // Steps
        // - Check if any type coercion needs to be done
        // - Generate each code for the initializing operand (dropping in type casts as necessary)
        // - Store the value in the referenced expression
        let lhs_ty = self
            .db
            .type_of((self.package_id, self.body_id, stmt.lhs).into())
            .to_base_type(self.db.up());
        let rhs_ty = self
            .db
            .type_of((self.package_id, self.body_id, stmt.rhs).into())
            .to_base_type(self.db.up());

        // Evaluation order is important, side effects from the rhs are visible when looking at lhs
        // For assignment, we don't want side effects from rhs eval to be visible during lookup
        self.generate_ref_expr(stmt.lhs);
        self.generate_expr(stmt.rhs);
        self.generate_coerced_op(lhs_ty, rhs_ty);
        self.emit_assign_op(lhs_ty, AssignOrder::Precomputed);

        eprintln!("assigning reference (first operand) to value (second operand)");
    }

    fn generate_coerced_op(&mut self, lhs_ty: ty::TypeId, rhs_ty: ty::TypeId) {
        let lhs_ty = lhs_ty.to_base_type(self.db.up());

        let coerce_to = match lhs_ty.kind(self.db.up()) {
            ty::TypeKind::Real(_) => Some(CoerceTo::Real),
            ty::TypeKind::Char => Some(CoerceTo::Char),
            ty::TypeKind::CharN(ty::SeqSize::Fixed(_)) => {
                let len = lhs_ty.length_of(self.db.up()).expect("never dyn");
                Some(CoerceTo::CharN(len.try_into().unwrap()))
            }
            ty::TypeKind::CharN(ty::SeqSize::Any) => {
                unreachable!("dyn is not a valid storage type")
            }
            _ => None,
        };

        self.coerce_expr_into(rhs_ty, coerce_to);
    }

    fn generate_stmt_put(&mut self, stmt: &hir_stmt::Put) {
        // Steps
        // We're only concerned with stdout emission_ty

        let old_scope = self.proc.enter_temporary_scope();

        let stream_handle = self.generate_set_stream(stmt.stream_num, None, StreamKind::Put);

        for item in &stmt.items {
            let item = match item {
                hir_stmt::Skippable::Skip => {
                    // Emit stream handle reference
                    self.proc.locate_temporary(stream_handle);
                    self.proc.ins().put(PutKind::Skip);
                    continue;
                }
                hir_stmt::Skippable::Item(put_item) => put_item,
            };

            let put_ty = self
                .db
                .type_of((self.package_id, self.body_id, item.expr).into())
                .to_base_type(self.db.up());

            let put_kind = match put_ty.kind(self.db.up()) {
                ty::TypeKind::Boolean => PutKind::Boolean,
                ty::TypeKind::Integer | ty::TypeKind::Int(_)
                    if item.opts.exponent_width().is_some() =>
                {
                    PutKind::IntExp
                }
                ty::TypeKind::Integer | ty::TypeKind::Int(_) if item.opts.precision().is_some() => {
                    PutKind::IntFract
                }
                ty::TypeKind::Integer | ty::TypeKind::Int(_) => PutKind::Int,
                ty::TypeKind::Nat(_) if item.opts.exponent_width().is_some() => PutKind::NatExp,
                ty::TypeKind::Nat(_) if item.opts.precision().is_some() => PutKind::NatFract,
                ty::TypeKind::Nat(_) => PutKind::Nat,
                ty::TypeKind::Real(_) if item.opts.exponent_width().is_some() => PutKind::RealExp,
                ty::TypeKind::Real(_) if item.opts.precision().is_some() => PutKind::RealFract,
                ty::TypeKind::Real(_) => PutKind::Real,
                ty::TypeKind::Char => PutKind::Char,
                ty::TypeKind::String | ty::TypeKind::StringN(_) => PutKind::String,
                ty::TypeKind::CharN(_) => PutKind::CharN,
                // TODO: Add case for enums
                _ => unreachable!(),
            };

            // Put value onto the stack
            self.generate_expr(item.expr);

            if let ty::TypeKind::CharN(seq_size) = put_ty.kind(self.db.up()) {
                let length = seq_size
                    .fixed_len(self.db.up(), Span::default())
                    .ok()
                    .expect("const should succeed and not be dyn");
                let length = length.into_u32().expect("should be int representable");

                self.proc.ins().pushint(length as i32);
            }

            // Deal with the put opts
            if let Some(width) = item.opts.width() {
                self.generate_expr(width);
            } else {
                // width is common to non-skip items, so have it present
                self.proc.ins().pushval0();
            }

            if let (Some(fract_width), true) =
                (item.opts.precision(), put_kind.has_fractional_width())
            {
                self.generate_expr(fract_width);
            }

            if let (Some(exp_width), true) =
                (item.opts.exponent_width(), put_kind.has_fractional_width())
            {
                self.generate_expr(exp_width);
            }

            // Emit stream handle reference
            self.proc.locate_temporary(stream_handle);
            self.proc.ins().put(put_kind);
        }

        if stmt.append_newline {
            // Emit newline
            self.proc.locate_temporary(stream_handle);
            self.proc.ins().put(PutKind::Skip);
        }

        self.proc.leave_temporary_scope(old_scope);
    }

    fn generate_stmt_get(&mut self, stmt: &hir_stmt::Get) {
        let stream_handle = self.generate_set_stream(stmt.stream_num, None, StreamKind::Get);

        for item in &stmt.items {
            let item = match item {
                hir_stmt::Skippable::Skip => {
                    // Skip whitespace
                    self.proc.locate_temporary(stream_handle);
                    self.proc.ins().get(GetKind::Skip);
                    continue;
                }
                hir_stmt::Skippable::Item(item) => item,
            };

            let get_ty = self
                .db
                .type_of((self.package_id, self.body_id, item.expr).into())
                .to_base_type(self.db.up());
            let size = get_ty.size_of(self.db.up()).expect("type must be concrete") as u32;

            let mut get_width = None;
            let get_kind = match get_ty.kind(self.db.up()) {
                ty::TypeKind::Boolean => GetKind::Boolean { size: 1 },
                ty::TypeKind::Int(_) => GetKind::Int { size },
                ty::TypeKind::Nat(_) => GetKind::Nat { size },
                ty::TypeKind::Real(_) => GetKind::Real { size },
                ty::TypeKind::Integer => unreachable!(),
                ty::TypeKind::Char => GetKind::Char { size: 1 },
                ty::TypeKind::StringN(_) | ty::TypeKind::String => match item.width {
                    hir_stmt::GetWidth::Token => GetKind::StringToken { size },
                    hir_stmt::GetWidth::Line => GetKind::StringLine { size },
                    hir_stmt::GetWidth::Chars(width) => {
                        get_width = Some(width);
                        GetKind::StringExact { size }
                    }
                },
                ty::TypeKind::CharN(_) => match item.width {
                    hir_stmt::GetWidth::Chars(width) => {
                        get_width = Some(width);
                        GetKind::CharN { size }
                    }
                    hir_stmt::GetWidth::Token => GetKind::CharN { size },
                    hir_stmt::GetWidth::Line => unreachable!(),
                },
                _ => unreachable!(),
            };

            // Put reference onto the stack
            self.generate_ref_expr(item.expr);

            if matches!(
                get_kind,
                GetKind::StringToken { .. }
                    | GetKind::StringLine { .. }
                    | GetKind::StringExact { .. }
                    | GetKind::CharN { .. }
            ) {
                // push max width
                if let Some(width) = get_width {
                    self.generate_expr(width);
                }

                // and max length
                let max_len = get_ty.length_of(self.db.up()).expect("is a charseq") as u32;
                self.proc.ins().pushint(max_len as i32);
            }

            self.proc.locate_temporary(stream_handle);
            self.proc.ins().get(get_kind);
        }
    }

    fn generate_set_stream(
        &mut self,
        stream_num: Option<hir_expr::ExprId>,
        status_expr: Option<hir_expr::ExprId>,
        op: StreamKind,
    ) -> TemporarySlot {
        // Make a temporary to store the stream handle
        let stream_handle = self.proc.alloc_temporary(4, 4);

        if let Some(stream_num) = stream_num {
            // Make temporary to store the status_var location from SETSTREAM
            let status_var = self.proc.alloc_temporary(4, 4);

            // Use this stream as the target
            self.generate_expr(stream_num);
            if let Some(_status_expr) = status_expr {
                unimplemented!("need to guarantee we generate a place expression");
            } else {
                // No place to store status
                self.proc.ins().pushval0();
            }

            // References to the temporary store
            self.proc.locate_temporary(stream_handle);
            self.proc.locate_temporary(status_var);

            self.proc.ins().setstream(op);
        } else {
            let kind: StdStreamKind = op
                .try_into()
                .expect("operation does not have a matching standard stream");
            self.proc.locate_temporary(stream_handle);

            // Use stdout as the target stream
            self.proc.ins().setstdstream(kind);
        }

        stream_handle
    }

    fn generate_stmt_for(&mut self, stmt: &hir_stmt::For) {
        let descriptor_size = std::mem::size_of::<ForDescriptor>();

        let descriptor_slot = self.proc.alloc_temporary(descriptor_size as u32, 4);
        if let Some(counter_def) = stmt.counter_def {
            // Type-pun the descriptor address to be the storage for the counter
            self.def_bindings
                .bind_temporary_slot(DefId(self.package_id, counter_def), descriptor_slot);
        }

        // Emit bounds
        match stmt.bounds {
            hir_stmt::ForBounds::Implicit(_range) => todo!(),
            hir_stmt::ForBounds::Full(start, end) => {
                self.generate_expr(start);
                self.generate_expr(end);
            }
        }

        // ... and the step by
        if let Some(step_by) = stmt.step_by {
            self.generate_expr(step_by);
            self.proc
                .ins()
                .chkrange(0, 0, i32::MAX, CheckKind::LoopStep);
        } else {
            self.proc.ins().pushval1();
        }

        // Negate if decreasing
        if stmt.is_decreasing {
            self.proc.ins().negint();
        }

        // Emit loop body
        let after_loop = self.proc.make_unanchored_label();

        self.proc.locate_temporary(descriptor_slot);
        self.proc.branch(|ins| ins.for_(0), after_loop);

        let loop_start = self.proc.make_label();
        {
            // Have a fixed location for the branch to go to
            self.emit_absolute_location();

            self.block_stack.push(BlockBranches::For {
                _loop_start: loop_start,
                after_loop,
            });
            self.generate_stmt_list(&stmt.stmts);
            self.block_stack.pop();

            self.proc.locate_temporary(descriptor_slot);
            self.proc.branch(|ins| ins.endfor(0), loop_start);
        }
        self.proc.anchor_label(after_loop, None);
        self.emit_absolute_location();
    }

    fn generate_stmt_loop(&mut self, stmt: &hir_stmt::Loop) {
        // Need to:
        // - Create a new branch, anchored to next instruction
        // - Create a forward branch for after the loop
        // - Anchor floating branch to instruction after the backward jump
        let after_loop = self.proc.make_unanchored_label();

        // Have a fixed location for the branch to go to
        let loop_start = self.proc.make_label();
        self.emit_absolute_location();
        {
            self.block_stack.push(BlockBranches::Loop {
                _loop_start: loop_start,
                after_loop,
            });
            self.generate_stmt_list(&stmt.stmts);
            self.block_stack.pop();
        }
        self.proc.branch(|ins| ins.jumpb(0), loop_start);

        self.proc.anchor_label(after_loop, None);
        self.emit_absolute_location();
    }

    fn generate_stmt_exit(&mut self, stmt: &hir_stmt::Exit) {
        // Branch to after the loop
        let block_branches = self.block_stack.last().unwrap();
        let branch_to = match block_branches {
            BlockBranches::Loop {
                _loop_start: _,
                after_loop: after_block,
            } => *after_block,
            BlockBranches::For {
                _loop_start: _,
                after_loop: after_block,
            } => *after_block,
        };

        if let Some(condition) = stmt.when_condition {
            // TODO: Use infix or as a branch if true?
            self.generate_expr(condition);
            self.proc.ins().not();
            self.proc.branch(|ins| ins.if_(0), branch_to);
        } else {
            self.proc.branch(|ins| ins.jump(0), branch_to);
        }
    }

    fn generate_stmt_if(&mut self, stmt: &hir_stmt::If) {
        // Steps:
        // - Evaluate condition
        // - If false, branch after true block
        // - Otherwise, proceed through true block and branch to after if statement
        self.generate_expr(stmt.condition);

        let after_true = self.proc.make_unanchored_label();
        let after_false = self.proc.make_unanchored_label();

        // Emit true branch
        self.proc.branch(|ins| ins.if_(0), after_true);
        {
            self.generate_stmt(stmt.true_branch);
            self.proc.branch(|ins| ins.jump(0), after_false);
        }
        self.proc.anchor_label(after_true, None);

        // Then false branch
        if let Some(false_branch) = stmt.false_branch.stmt() {
            self.emit_absolute_location();
            self.generate_stmt(false_branch);
        }

        self.proc.anchor_label(after_false, None);
        self.emit_absolute_location();
    }

    fn generate_stmt_case(&mut self, stmt: &hir_stmt::Case) {
        // Steps:
        // - Store discriminant in a temporary
        // - Generate series of cond branches matching the condition
        //   - Generate floating locations for the branch targets
        // - Generate branch for after the case statement
        // - For each branch, add inline statements and branch to after case statement
        self.generate_expr(stmt.discriminant);

        let discrim_ty = self
            .db
            .type_of((self.package_id, self.body_id, stmt.discriminant).into())
            .to_base_type(self.db.up());

        let coerce_to = match discrim_ty.kind(self.db.up()) {
            ty::TypeKind::String | ty::TypeKind::StringN(_) => Some(CoerceTo::String),
            ty::TypeKind::Char => Some(CoerceTo::Char),
            _ => None,
        };

        let discrim_slot = self.proc.alloc_temporary(
            discrim_ty.size_of(self.db.up()).expect("is concrete") as u32,
            4,
        );
        self.proc.locate_temporary(discrim_slot);
        self.emit_assign_op(discrim_ty, AssignOrder::Postcomputed);

        let mut arm_targets = vec![];
        let mut has_default = false;

        for arm in &stmt.arms {
            let target = self.proc.make_unanchored_label();
            arm_targets.push(target);

            match &arm.selectors {
                hir_stmt::CaseSelector::Exprs(exprs) => {
                    for expr in exprs {
                        let expr_ty = self
                            .db
                            .type_of((self.package_id, self.body_id, *expr).into())
                            .to_base_type(self.db.up());

                        self.generate_coerced_expr(*expr, coerce_to);
                        self.proc.locate_temporary(discrim_slot);
                        self.generate_fetch_value(discrim_ty);

                        if matches!(coerce_to, Some(CoerceTo::Char)) {
                            // Always keep coerced char type as an EQINT
                            self.proc.ins().eqint();
                        } else {
                            // Reuse normal eq selection
                            self.emit_eq_op(
                                expr_ty.kind(self.db.up()),
                                discrim_ty.kind(self.db.up()),
                            );
                        };

                        // Branch if any are true
                        self.proc.ins().not();
                        self.proc.branch(|ins| ins.if_(0), target);
                    }
                }
                hir_stmt::CaseSelector::Default => {
                    has_default = true;
                    self.proc.branch(|ins| ins.jump(0), target);
                    break;
                }
            }
        }

        let after_case = self.proc.make_unanchored_label();
        if !has_default {
            self.proc.branch(|ins| ins.jump(0), after_case);
        }

        for (arm, target) in stmt.arms.iter().zip(arm_targets.iter()) {
            self.proc.anchor_label(*target, None);
            self.emit_absolute_location();

            self.generate_stmt_list(&arm.stmts);

            self.proc.branch(|ins| ins.jump(0), after_case);
        }

        self.proc.anchor_label(after_case, None);
        self.emit_absolute_location();
    }

    fn generate_stmt_call(&mut self, stmt: &hir_stmt::Call) {
        self.generate_call(stmt.lhs, stmt.arguments.as_ref(), true);
    }

    fn generate_stmt_return(&mut self, _stmt: &hir_stmt::Return) {
        self.proc.ins().return_();
    }

    fn generate_stmt_result(&mut self, stmt: &hir_stmt::Result) {
        let db = self.db;

        let ret_ty = db.type_of((self.package_id, self.body_id).into());
        let expr_ty = db.type_of((self.package_id, self.body_id, stmt.expr).into());

        // Generate return value
        self.generate_expr(stmt.expr);
        self.generate_coerced_op(ret_ty, expr_ty);

        // Perform assignment
        self.proc.ins().locateparm(0);
        self.proc.ins().fetchaddr();
        self.emit_assign_op(ret_ty, AssignOrder::Postcomputed);

        self.proc.ins().return_();
    }

    fn generate_item(&mut self, item_id: hir_item::ItemId) {
        let item = self.package.item(item_id);
        let span = item.span.lookup_in(self.package);
        self.emit_location(span);

        match &item.kind {
            hir_item::ItemKind::ConstVar(item) => self.generate_item_constvar(item),
            hir_item::ItemKind::Binding(item) => self.generate_item_binding(item),
            hir_item::ItemKind::Type(_) => {}
            hir_item::ItemKind::Subprogram(_) => {
                // We visit every subprogram body eventually
            }
            hir_item::ItemKind::Module(_) => {
                // We already generate code for module init bodies as part of walking
                // over all of the bodies in a package
            }
            hir_item::ItemKind::Import(_) => {
                // No code needs to be generated from this item
            }
        }
    }

    fn generate_item_constvar(&mut self, item: &hir_item::ConstVar) {
        // Steps
        // - Reserve space for the variable
        // - if an initializer body is present:
        //   - inline the initializer body (it's always an expr body)
        //   - assign the variable to the produced value
        // - otherwise
        //   - initialize the variable with the corresponding uninit pattern for the type
        eprintln!("reserving space for def {:?}", item.def_id);

        let constvar_def = DefId(self.package_id, item.def_id);
        let def_ty = self
            .db
            .type_of(constvar_def.into())
            .to_base_type(self.db.up());
        let local_slot = self.proc.alloc_local(
            None,
            def_ty.size_of(self.db.up()).expect("must be sized") as u32,
            4,
        );
        self.def_bindings.bind_local_slot(constvar_def, local_slot);

        if let Some(init_body) = item.init_expr {
            eprintln!("inlining init body {init_body:?}");
            self.inline_body(init_body);

            let body_ty = self.db.type_of((self.package_id, init_body).into());
            self.generate_coerced_op(def_ty, body_ty);

            eprintln!("assigning def {init_body:?} to previously produced value");
            self.proc.locate_local(local_slot);
            self.emit_assign_op(def_ty, AssignOrder::Postcomputed);
        } else if def_ty.has_uninit(self.db.up()) {
            eprintln!(
                "assigning def {:?} to uninit pattern for type `{}`",
                item.def_id,
                def_ty.display(self.db.up())
            );
            self.proc.locate_local(local_slot);
            self.emit_assign_uninit(def_ty);
        }
    }

    fn generate_item_binding(&mut self, item: &hir_item::Binding) {
        // We only support def rebindings for now
        // For bindings to specific storage locations,
        // it should be treated as introducing a new var that is specially handled
        // (it's like an addr, though we need to pierce through indirections)

        let bind_def = DefId(self.package_id, item.def_id);
        if let Some(aliased_def) = self.db.binding_def((self.package_id, item.bind_to).into()) {
            self.def_bindings.bind_aliasd_def(bind_def, aliased_def);
        } else {
            // Indirection binding, compute address and store it.
            // TODO: Replace with machine width?
            let alias_slot = self.proc.alloc_local(Some("alias"), 4, 4);
            self.def_bindings.bind_aliased_local(bind_def, alias_slot);

            self.inline_body(item.bind_to);
            self.proc.locate_local(alias_slot);
            self.proc.ins().asnaddrinv();

            panic!("???: do we always generate places from binding targets?");
        }
    }

    fn generate_coerced_expr(&mut self, expr_id: hir_expr::ExprId, coerce_to: Option<CoerceTo>) {
        self.generate_expr(expr_id);

        let expr_ty = self
            .db
            .type_of((self.package_id, self.body_id, expr_id).into());

        self.coerce_expr_into(expr_ty, coerce_to);
    }

    fn coerce_expr_into(&mut self, from_ty: ty::TypeId, coerce_to: Option<CoerceTo>) {
        let expr_ty = from_ty.to_base_type(self.db.up());

        let Some(coerce_to) = coerce_to else { return };

        match (coerce_to, expr_ty.kind(self.db.up())) {
            // To `real`
            (CoerceTo::Real, ty::TypeKind::Nat(_)) => _ = self.proc.ins().natreal(),
            (CoerceTo::Real, int) if int.is_integer() => _ = self.proc.ins().intreal(),

            // To `char`
            (CoerceTo::Char, ty::TypeKind::String | ty::TypeKind::StringN(_)) => {
                self.proc.ins().strtochar();
            }
            (CoerceTo::Char, ty::TypeKind::CharN(ty::SeqSize::Fixed(_))) => {
                // Compile-time checked to always fit
                let len = expr_ty.length_of(self.db.up());
                assert_eq!(len, Some(1), "never dyn or not 1");

                // Fetch the first char
                self.proc.ins().fetchnat1();
            }
            (CoerceTo::Char, ty::TypeKind::CharN(ty::SeqSize::Any)) => {
                unimplemented!()
            }

            // To `char(N)`
            (CoerceTo::CharN(len), ty::TypeKind::Char) => {
                // Compile-time checked to always fit
                assert_eq!(len, 1, "never dyn or not 1");

                // Reserve enough space for the temporary char_n
                // Note: This is size is rounded up to char_n's alignment size.
                let reserve_size = len + 1;

                let temp_str = self.proc.alloc_temporary(reserve_size as u32, 4);
                self.proc.locate_temporary(temp_str);

                self.proc.ins().chartocstr();
            }
            (CoerceTo::CharN(len), ty::TypeKind::String | ty::TypeKind::StringN(_)) => {
                // Only need to verify that rhs is of the required length
                self.proc.ins().chkstrsize(len);
            }

            // To `string`
            (CoerceTo::String, ty::TypeKind::Char) => {
                // Reserve enough space for a `string(1)` (2 bytes)
                let temp_str = self.proc.alloc_temporary(2, 4);
                self.proc.locate_temporary(temp_str);

                self.proc.ins().chartostr();
            }
            (CoerceTo::String, ty::TypeKind::CharN(ty::SeqSize::Fixed(_))) => {
                // Reserve enough space for the given char_n
                let len = expr_ty.length_of(self.db.up()).expect("never dyn");

                // Include the null terminator in the reservation size
                // Never let it exceed the maximum length of a string
                let reserve_size = (len + 1).min(256);

                let temp_str = self.proc.alloc_temporary(reserve_size as u32, 4);
                self.proc.locate_temporary(temp_str);

                self.proc.ins().pushint(len as i32);
                self.proc.ins().cstrtostr();
            }
            (CoerceTo::String, ty::TypeKind::CharN(ty::SeqSize::Any)) => {
                todo!()
            }
            _ => {}
        };
    }

    fn generate_expr(&mut self, expr_id: hir_expr::ExprId) {
        let expr = self.body.expr(expr_id);
        let span = expr.span.lookup_in(self.package);
        self.emit_location(span);

        match &expr.kind {
            hir_expr::ExprKind::Missing => unreachable!("malformed code reached code generation"),
            hir_expr::ExprKind::Literal(expr) => self.generate_expr_literal(expr),
            hir_expr::ExprKind::Binary(expr) => self.generate_expr_binary(expr),
            hir_expr::ExprKind::Unary(expr) => self.generate_expr_unary(expr),
            hir_expr::ExprKind::Name(expr) => self.generate_expr_name(expr_id, expr),
            hir_expr::ExprKind::Field(_expr) => unimplemented!(),
            hir_expr::ExprKind::Call(expr) => self.generate_expr_call(expr),
            _ => unimplemented!(),
        }
    }

    fn generate_expr_literal(&mut self, expr: &hir_expr::Literal) {
        // Steps
        // - Push value onto the operand stack
        eprintln!("pushing value {expr:?} to operand stack");
        match expr {
            hir_expr::Literal::Integer(value) => match value {
                0..=0xFF => {
                    self.proc.ins().pushint1(*value as i8);
                }
                0..=0xFFFF => {
                    self.proc.ins().pushint2(*value as i16);
                }
                0..=0xFFFFFFFF => {
                    self.proc.ins().pushint(*value as i32);
                }
                _ => unreachable!("this backend does not support values larger than u32::MAX"),
            },
            hir_expr::Literal::Real(value) => {
                self.proc.ins().pushreal(*value);
            }
            hir_expr::Literal::Char(value) => {
                if *value as u32 <= 0xFF {
                    self.proc.ins().pushint1(*value as i8);
                } else {
                    unreachable!("this backend does not support non-(extended) ascii code points")
                }
            }
            hir_expr::Literal::CharSeq(value) | hir_expr::Literal::String(value) => {
                // Defer allocation to later
                let reloc_point = self
                    .proc
                    .reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));

                self.cctx
                    .strings
                    .entry(value.to_owned())
                    .or_insert(vec![])
                    .push((self.proc.id(), reloc_point));
            }
            hir_expr::Literal::Boolean(value) => {
                if *value {
                    self.proc.ins().pushval1();
                } else {
                    self.proc.ins().pushval0();
                }
            }
        }
    }

    fn generate_expr_binary(&mut self, expr: &hir_expr::Binary) {
        // Steps
        // - Check if any type coercion needs to be done
        // - Generate each code for the corresponding operands (dropping in type casts as necessary)
        // - Emit instruction corresponding to the operation

        let lhs_ty = self
            .db
            .type_of((self.package_id, self.body_id, expr.lhs).into())
            .to_base_type(self.db.up());
        let rhs_ty = self
            .db
            .type_of((self.package_id, self.body_id, expr.rhs).into())
            .to_base_type(self.db.up());

        match expr.op.item() {
            hir_expr::BinaryOp::Add => {
                match self
                    .coerce_and_select_numbers(
                        expr,
                        lhs_ty.kind(self.db.up()),
                        rhs_ty.kind(self.db.up()),
                    )
                    .expect("op over unsupported type")
                {
                    OperandPairs::IntInt => _ = self.proc.ins().addint(),
                    OperandPairs::IntNat => _ = self.proc.ins().addintnat(),
                    OperandPairs::NatInt => _ = self.proc.ins().addnatint(),
                    OperandPairs::NatNat => _ = self.proc.ins().addnat(),
                    OperandPairs::Real => _ = self.proc.ins().addreal(),
                }
            }
            hir_expr::BinaryOp::Sub => {
                match self
                    .coerce_and_select_numbers(
                        expr,
                        lhs_ty.kind(self.db.up()),
                        rhs_ty.kind(self.db.up()),
                    )
                    .expect("op over unsupported type")
                {
                    OperandPairs::IntInt => _ = self.proc.ins().subint(),
                    OperandPairs::IntNat => _ = self.proc.ins().subintnat(),
                    OperandPairs::NatInt => _ = self.proc.ins().subnatint(),
                    OperandPairs::NatNat => _ = self.proc.ins().subnat(),
                    OperandPairs::Real => _ = self.proc.ins().subreal(),
                }
            }
            hir_expr::BinaryOp::Mul => match self
                .coerce_and_select_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                )
                .expect("op over unsupported type")
            {
                OperandPairs::IntInt => _ = self.proc.ins().mulint(),
                OperandPairs::IntNat => _ = self.proc.ins().mulint(),
                OperandPairs::NatInt => _ = self.proc.ins().mulint(),
                OperandPairs::NatNat => _ = self.proc.ins().mulnat(),
                OperandPairs::Real => _ = self.proc.ins().mulreal(),
            },
            hir_expr::BinaryOp::Div => match self
                .coerce_and_select_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                )
                .expect("op over unsupported type")
            {
                OperandPairs::IntInt => _ = self.proc.ins().divint(),
                OperandPairs::IntNat => _ = self.proc.ins().divint(),
                OperandPairs::NatInt => _ = self.proc.ins().divint(),
                OperandPairs::NatNat => _ = self.proc.ins().divnat(),
                OperandPairs::Real => _ = self.proc.ins().divreal(),
            },
            hir_expr::BinaryOp::RealDiv => {
                self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
                self.proc.ins().realdivide();
            }
            hir_expr::BinaryOp::Mod => match self
                .coerce_and_select_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                )
                .expect("op over unsupported type")
            {
                OperandPairs::IntInt => _ = self.proc.ins().modint(),
                OperandPairs::IntNat => _ = self.proc.ins().modint(),
                OperandPairs::NatInt => _ = self.proc.ins().modint(),
                OperandPairs::NatNat => _ = self.proc.ins().modnat(),
                OperandPairs::Real => _ = self.proc.ins().modreal(),
            },
            hir_expr::BinaryOp::Rem => match self
                .coerce_and_select_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                )
                .expect("op over unsupported type")
            {
                OperandPairs::IntInt => _ = self.proc.ins().remint(),
                OperandPairs::IntNat => _ = self.proc.ins().remint(),
                OperandPairs::NatInt => _ = self.proc.ins().remint(),
                OperandPairs::NatNat => _ = self.proc.ins().modnat(),
                OperandPairs::Real => _ = self.proc.ins().remreal(),
            },
            hir_expr::BinaryOp::Exp => match (lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()))
            {
                (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    self.proc.ins().expintint();
                }
                (ty::TypeKind::Real(_), rhs) if rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    self.proc.ins().exprealint();
                }
                (ty::TypeKind::Real(_), ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                    self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
                    self.proc.ins().exprealreal();
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::And => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                self.proc.ins().and();
            }
            hir_expr::BinaryOp::Or => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                self.proc.ins().or();
            }
            hir_expr::BinaryOp::Xor => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                self.proc.ins().xor();
            }
            hir_expr::BinaryOp::Shl => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                self.proc.ins().shl();
            }
            hir_expr::BinaryOp::Shr => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                self.proc.ins().shr();
            }
            hir_expr::BinaryOp::Less | hir_expr::BinaryOp::GreaterEq => {
                self.coerce_to_same(expr, lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));

                if let Some(num_cmp) =
                    self.select_numbers(lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()))
                {
                    match num_cmp {
                        OperandPairs::IntInt => _ = self.proc.ins().geint(),
                        OperandPairs::IntNat => _ = self.proc.ins().geintnat(),
                        OperandPairs::NatInt => _ = self.proc.ins().genatint(),
                        OperandPairs::NatNat => _ = self.proc.ins().genat(),
                        OperandPairs::Real => _ = self.proc.ins().gereal(),
                    }
                } else {
                    match (lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up())) {
                        (ty::TypeKind::Char, ty::TypeKind::Char) => _ = self.proc.ins().genat(),
                        (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => {
                            _ = self.proc.ins().gestr()
                        }
                        _ => unreachable!(),
                    }
                };

                if matches!(expr.op.item(), &hir_expr::BinaryOp::Less) {
                    // Emit not to convert to less-than
                    self.proc.ins().not();
                }
            }
            hir_expr::BinaryOp::Greater | hir_expr::BinaryOp::LessEq => {
                self.coerce_to_same(expr, lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));

                if let Some(num_cmp) =
                    self.select_numbers(lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()))
                {
                    match num_cmp {
                        OperandPairs::IntInt => self.proc.ins().leint(),
                        OperandPairs::IntNat => self.proc.ins().leintnat(),
                        OperandPairs::NatInt => self.proc.ins().lenatint(),
                        OperandPairs::NatNat => self.proc.ins().lenat(),
                        OperandPairs::Real => self.proc.ins().lereal(),
                    };
                } else {
                    match (lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up())) {
                        (ty::TypeKind::Char, ty::TypeKind::Char) => {
                            self.proc.ins().lenat();
                        }
                        (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => {
                            self.proc.ins().lestr();
                        }
                        _ => unreachable!(),
                    }
                };

                if matches!(expr.op.item(), &hir_expr::BinaryOp::Greater) {
                    // Emit not to convert to greater-than
                    self.proc.ins().not();
                }
            }
            hir_expr::BinaryOp::Equal | hir_expr::BinaryOp::NotEqual => {
                self.coerce_to_same(expr, lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));
                self.emit_eq_op(lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));

                if matches!(expr.op.item(), &hir_expr::BinaryOp::NotEqual) {
                    // Emit not to convert to not-equal
                    self.proc.ins().not();
                }
            }
            hir_expr::BinaryOp::In => todo!(),
            hir_expr::BinaryOp::NotIn => todo!(),
            hir_expr::BinaryOp::Imply => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.proc.ins().not();
                self.generate_expr(expr.rhs);
                self.proc.ins().or();
            }
        };
    }

    fn coerce_and_select_numbers(
        &mut self,
        expr: &hir_expr::Binary,
        lhs: &ty::TypeKind,
        rhs: &ty::TypeKind,
    ) -> Option<OperandPairs> {
        self.coerce_to_same(expr, lhs, rhs);
        self.select_numbers(lhs, rhs)
    }

    fn select_numbers(&self, lhs: &ty::TypeKind, rhs: &ty::TypeKind) -> Option<OperandPairs> {
        match (lhs, rhs) {
            (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => Some(OperandPairs::Real),
            (ty::TypeKind::Nat(_), ty::TypeKind::Nat(_)) => Some(OperandPairs::NatNat),
            (ty::TypeKind::Nat(_), other) if other.is_integer() => Some(OperandPairs::NatInt),
            (other, ty::TypeKind::Nat(_)) if other.is_integer() => Some(OperandPairs::IntNat),
            (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => Some(OperandPairs::IntInt),
            _ => None,
        }
    }

    fn coerce_to_same(&mut self, expr: &hir_expr::Binary, lhs: &ty::TypeKind, rhs: &ty::TypeKind) {
        let coerce_to = match (lhs, rhs) {
            (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => Some(CoerceTo::Real),

            // For string comparisons, leave char, char untouched
            (ty::TypeKind::Char, ty::TypeKind::Char) => None,
            (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => Some(CoerceTo::String),
            _ => None,
        };

        self.generate_coerced_expr(expr.lhs, coerce_to);
        self.generate_coerced_expr(expr.rhs, coerce_to);
    }

    fn emit_eq_op(&mut self, lhs_kind: &ty::TypeKind, rhs_kind: &ty::TypeKind) {
        if let Some(num_op) = self.select_numbers(lhs_kind, rhs_kind) {
            match num_op {
                OperandPairs::IntInt => self.proc.ins().eqint(),
                OperandPairs::IntNat => self.proc.ins().eqintnat(),
                OperandPairs::NatInt => self.proc.ins().eqint(),
                // Note: We use EQINT instead of EQNAT since Turing doesn't support EQNAT.
                OperandPairs::NatNat => self.proc.ins().eqint(),
                OperandPairs::Real => self.proc.ins().eqreal(),
            };
        } else {
            match (lhs_kind, rhs_kind) {
                (ty::TypeKind::Char, ty::TypeKind::Char) => {
                    self.proc.ins().eqint();
                }
                // All other comparable charseqs are converted into string
                (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => {
                    self.proc.ins().eqstr();
                }
                _ => unreachable!(),
            }
        }
    }

    fn generate_expr_unary(&mut self, expr: &hir_expr::Unary) {
        // Steps
        // - Generate each code for the corresponding operands
        // - Emit instruction corresponding to the operation

        eprintln!(
            "applying operation {:?} to previous operand",
            expr.op.item()
        );

        let rhs_ty = self
            .db
            .type_of((self.package_id, self.body_id, expr.rhs).into())
            .to_base_type(self.db.up());

        self.generate_expr(expr.rhs);

        match expr.op.item() {
            hir_expr::UnaryOp::Identity => {} // no-op
            hir_expr::UnaryOp::Not => {
                if rhs_ty.kind(self.db.up()).is_integer() {
                    // Composite
                    self.proc.ins().pushint(u32::MAX as i32);
                    self.proc.ins().xor();
                } else if rhs_ty.kind(self.db.up()).is_boolean() {
                    self.proc.ins().not();
                } else {
                    unreachable!()
                }
            }
            hir_expr::UnaryOp::Negate => match rhs_ty.kind(self.db.up()) {
                ty::TypeKind::Integer | ty::TypeKind::Int(_) | ty::TypeKind::Nat(_) => {
                    // should already be dealing with promoted types
                    self.proc.ins().negint();
                }
                ty::TypeKind::Real(_) => {
                    self.proc.ins().negreal();
                }
                _ => unreachable!(),
            },
        }
    }

    fn generate_expr_name(&mut self, expr_id: hir_expr::ExprId, expr: &hir_expr::Name) {
        // Steps
        // - Load value from the referenced def (may need to perform canonical name resolution)
        match expr {
            hir_expr::Name::Name(binding) => {
                let def_id = self.package.binding_resolve(*binding).unwrap_def();
                let info = self.package.local_def(def_id);
                eprintln!("loading value from def {info:?} ({def_id:?})");

                let def_ty = self
                    .db
                    .type_of(DefId(self.package_id, def_id).into())
                    .to_base_type(self.db.up());

                if let ty::TypeKind::Subprogram(_, None, _) = def_ty.kind(self.db.up()) {
                    // as param-less call
                    self.generate_call(expr_id, None, false);
                } else {
                    // As normal fetch
                    self.def_bindings
                        .emit_locate_def(&mut self.proc, DefId(self.package_id, def_id));
                    self.generate_fetch_value(def_ty);
                }
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }

    fn generate_expr_call(&mut self, expr: &hir_expr::Call) {
        self.generate_call(expr.lhs, Some(&expr.arguments), false);
    }

    fn generate_call(
        &mut self,
        lhs: hir_expr::ExprId,
        arguments: Option<&hir_expr::ArgList>,
        drop_ret_val: bool,
    ) {
        let db = self.db;
        let lhs_expr = (self.package_id, self.body_id, lhs);
        let lhs_ty = if let Some(def_id) = db.binding_def(lhs_expr.into()) {
            // From an item
            db.type_of(def_id.into())
        } else {
            // From an actual expression
            db.type_of(lhs_expr.into())
        };
        let lhs_ty_ref = lhs_ty.to_base_type(db.up());
        let (params, ret_ty) =
            if let ty::TypeKind::Subprogram(_, params, ret_ty) = lhs_ty_ref.kind(db.up()) {
                (params, *ret_ty)
            } else {
                unreachable!()
            };

        // Note: Temporary must live longer than the call, so it is outside of the call temporary scope.
        let ret_ty_ref = ret_ty.to_base_type(db.up());
        let ret_val = if !matches!(ret_ty_ref.kind(db.up()), ty::TypeKind::Void) {
            let size_of = ret_ty_ref.size_of(db.up()).expect("must be sized");
            Some(self.proc.alloc_temporary(size_of as u32, 4))
        } else {
            None
        };

        let old_scope = self.proc.enter_temporary_scope();
        {
            // Fetch lhs first
            self.generate_ref_expr(lhs);
            // TODO: Replace with the size of the target arch's ptr
            let call_to = self.proc.alloc_temporary(4, 4);
            self.proc.locate_temporary(call_to);
            self.emit_assign_op(lhs_ty, AssignOrder::Postcomputed);

            let mut arg_frame_size = 0;
            let mut arg_temps = vec![];

            if let Some(ret_val) = ret_val {
                // Treat return argument as a pass by ref
                let ret_arg = self.proc.alloc_temporary(
                    ret_ty_ref.size_of(db.up()).expect("must be sized") as u32,
                    4,
                );

                self.proc.locate_temporary(ret_val);
                self.proc.locate_temporary(ret_arg);
                self.proc.ins().asnaddrinv();
                arg_temps.push((ret_arg, None));
                arg_frame_size += 4;
            }

            // Eval args
            if let Some((params, args)) = params.as_ref().zip(arguments) {
                for (param, arg) in params.iter().zip(args.iter()) {
                    let arg_expr = *arg;
                    let arg_ty = db.type_of((self.package_id, self.body_id, arg_expr).into());

                    match param.pass_by {
                        ty::PassBy::Value => {
                            let param_ty = param.param_ty;
                            let size_of = param_ty.size_of(db.up()).expect("must be sized");
                            let nth_arg = self.proc.alloc_temporary(size_of as u32, 4);
                            self.generate_expr(arg_expr);
                            self.generate_coerced_op(param_ty, arg_ty);

                            self.proc.locate_temporary(nth_arg);
                            self.emit_assign_op(param_ty, AssignOrder::Postcomputed);
                            arg_temps.push((nth_arg, Some(param_ty)));
                            arg_frame_size += size_of;
                        }
                        ty::PassBy::Reference(_) => {
                            // TODO: Replace with the size of the target arch's ptr
                            let nth_arg = self.proc.alloc_temporary(4, 4);
                            self.generate_ref_expr(arg_expr);

                            self.proc.locate_temporary(nth_arg);
                            self.proc.ins().asnaddrinv();
                            arg_temps.push((nth_arg, None));
                            arg_frame_size += 4;
                        }
                    }
                }
            }

            // Emit call for later
            self.proc.locate_temporary(call_to);
            self.proc.ins().fetchaddr();

            // Move args into reverse order
            // ( ... arg0 arg1 arg2 -- arg2 arg1 arg0 ... )
            for (arg, ty) in arg_temps.into_iter().rev() {
                self.proc.locate_temporary(arg);

                if let Some(ty) = ty {
                    self.generate_fetch_value(ty);
                } else {
                    self.proc.ins().fetchaddr();
                }
            }

            // Do the call
            let arg_frame_size = arg_frame_size.try_into().unwrap();
            self.proc.ins().call(arg_frame_size);
            self.proc.ins().incsp(arg_frame_size + 4);
        }
        self.proc.leave_temporary_scope(old_scope);

        if let Some(slot_at) = ret_val.filter(|_| !drop_ret_val) {
            self.proc.locate_temporary(slot_at);
            self.generate_fetch_value(ret_ty);
        }
    }

    /// Like `generate_expr`, but for producing references to locations
    fn generate_ref_expr(&mut self, expr_id: hir_expr::ExprId) {
        let expr = self.body.expr(expr_id);
        let span = expr.span.lookup_in(self.package);
        self.emit_location(span);

        match &expr.kind {
            // Right now, only names and field lookups can produce references
            hir_expr::ExprKind::Name(ref_expr) => self.generate_ref_expr_name(ref_expr),
            hir_expr::ExprKind::Field(_ref_expr) => unimplemented!(),
            // The rest can never be in reference producing position
            _ => {
                unreachable!("malformed code reached code generation (producing reference from non-reference expr)")
            }
        }
    }

    fn generate_ref_expr_name(&mut self, expr: &hir_expr::Name) {
        match expr {
            hir_expr::Name::Name(binding) => {
                // Steps
                // - Produce pointer to referenced def (may need to perform canonical name resolution)
                //   - If it's a bound def, it should be to within the call frame.
                //   - If it's an unbound def, it should be from a relocatable patch to the globals space
                let def_id = self.package.binding_resolve(*binding).unwrap_def();
                let info = self.package.local_def(def_id);
                eprintln!("locating value from def {info:?} ({def_id:?})");

                self.def_bindings
                    .emit_locate_def(&mut self.proc, DefId(self.package_id, def_id))
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }

    fn emit_assign_op(&mut self, into_ty: ty::TypeId, order: AssignOrder) {
        let into_tyref = into_ty;

        enum AssignKind {
            Addr,
            Int,
            Int1,
            Int2,
            Int4,
            Nat,
            Nat1,
            Nat2,
            Nat4,
            Real,
            Real4,
            Real8,
            NonScalar(u32),
            Str,
        }

        let kind = match into_tyref.kind(self.db.up()) {
            ty::TypeKind::Boolean => AssignKind::Int1,
            ty::TypeKind::Int(ty::IntSize::Int1) => AssignKind::Int1,
            ty::TypeKind::Int(ty::IntSize::Int2) => AssignKind::Int2,
            ty::TypeKind::Int(ty::IntSize::Int4) => AssignKind::Int4,
            ty::TypeKind::Int(ty::IntSize::Int) => AssignKind::Int,
            ty::TypeKind::Nat(ty::NatSize::Nat1) => AssignKind::Nat1,
            ty::TypeKind::Nat(ty::NatSize::Nat2) => AssignKind::Nat2,
            ty::TypeKind::Nat(ty::NatSize::Nat4) => AssignKind::Nat4,
            ty::TypeKind::Nat(ty::NatSize::Nat) => AssignKind::Nat,
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => AssignKind::Addr,
            ty::TypeKind::Real(ty::RealSize::Real4) => AssignKind::Real4,
            ty::TypeKind::Real(ty::RealSize::Real8) => AssignKind::Real8,
            ty::TypeKind::Real(ty::RealSize::Real) => AssignKind::Real,
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => AssignKind::Int1,
            ty::TypeKind::CharN(_) => {
                let storage_size = into_tyref.size_of(self.db.up()).expect("not dyn") as u32;
                AssignKind::NonScalar(storage_size)
            }
            ty::TypeKind::String | ty::TypeKind::StringN(_) => {
                // Push storage size (is always the first stack operand for asnstr)
                let char_len: u32 = into_tyref
                    .length_of(self.db.up())
                    .expect("should not be dyn")
                    .try_into()
                    .expect("length too large");
                self.proc.ins().pushint(char_len as i32);

                AssignKind::Str
            }
            ty::TypeKind::Opaque(_, ty) => {
                // defer to the opaque type
                return self.emit_assign_op(*ty, order);
            }
            ty::TypeKind::Constrained(..) => unimplemented!(),
            ty::TypeKind::Array(..) => unimplemented!(),
            ty::TypeKind::Enum(..) => unimplemented!(),
            ty::TypeKind::Set(..) => unimplemented!(),
            ty::TypeKind::Pointer(..) => unimplemented!(),
            ty::TypeKind::Subprogram(..) => AssignKind::Addr,
            ty::TypeKind::Error
            | ty::TypeKind::Forward
            | ty::TypeKind::Alias(_, _)
            | ty::TypeKind::Void => {
                unreachable!()
            }
        };

        match order {
            AssignOrder::Precomputed => match kind {
                AssignKind::Addr => _ = self.proc.ins().asnaddr(),
                AssignKind::Int => _ = self.proc.ins().asnint(),
                AssignKind::Int1 => _ = self.proc.ins().asnint1(),
                AssignKind::Int2 => _ = self.proc.ins().asnint2(),
                AssignKind::Int4 => _ = self.proc.ins().asnint4(),
                AssignKind::Nat => _ = self.proc.ins().asnnat(),
                AssignKind::Nat1 => _ = self.proc.ins().asnnat1(),
                AssignKind::Nat2 => _ = self.proc.ins().asnnat2(),
                AssignKind::Nat4 => _ = self.proc.ins().asnnat4(),
                AssignKind::Real => _ = self.proc.ins().asnreal(),
                AssignKind::Real4 => _ = self.proc.ins().asnreal4(),
                AssignKind::Real8 => _ = self.proc.ins().asnreal8(),
                AssignKind::NonScalar(size) => _ = self.proc.ins().asnnonscalar(size),
                AssignKind::Str => _ = self.proc.ins().asnstr(),
            },
            AssignOrder::Postcomputed => match kind {
                AssignKind::Addr => _ = self.proc.ins().asnaddrinv(),
                AssignKind::Int => _ = self.proc.ins().asnintinv(),
                AssignKind::Int1 => _ = self.proc.ins().asnint1inv(),
                AssignKind::Int2 => _ = self.proc.ins().asnint2inv(),
                AssignKind::Int4 => _ = self.proc.ins().asnint4inv(),
                AssignKind::Nat => _ = self.proc.ins().asnnatinv(),
                AssignKind::Nat1 => _ = self.proc.ins().asnnat1inv(),
                AssignKind::Nat2 => _ = self.proc.ins().asnnat2inv(),
                AssignKind::Nat4 => _ = self.proc.ins().asnnat4inv(),
                AssignKind::Real => _ = self.proc.ins().asnrealinv(),
                AssignKind::Real4 => _ = self.proc.ins().asnreal4inv(),
                AssignKind::Real8 => _ = self.proc.ins().asnreal8inv(),
                AssignKind::NonScalar(size) => _ = self.proc.ins().asnnonscalarinv(size),
                AssignKind::Str => _ = self.proc.ins().asnstrinv(),
            },
        }
    }

    // Expects a destination address to be present
    fn emit_assign_uninit(&mut self, uninit_ty: ty::TypeId) {
        enum ScalarUninit {
            Addr,
            Boolean,
            Int,
            Nat,
            Real,
            Str,
        }

        let kind = match uninit_ty.kind(self.db.up()) {
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => ScalarUninit::Addr,
            ty::TypeKind::Boolean => ScalarUninit::Boolean,
            ty::TypeKind::Int(ty::IntSize::Int) => ScalarUninit::Int,
            ty::TypeKind::Nat(ty::NatSize::Nat) => ScalarUninit::Nat,
            ty::TypeKind::Real(ty::RealSize::Real) => ScalarUninit::Real,
            ty::TypeKind::String => ScalarUninit::Str,
            ty::TypeKind::Subprogram(..) => ScalarUninit::Addr,
            _ => unimplemented!(),
        };

        match kind {
            ScalarUninit::Addr => _ = self.proc.ins().uninitaddr(),
            ScalarUninit::Boolean => _ = self.proc.ins().uninitboolean(),
            ScalarUninit::Int => _ = self.proc.ins().uninitint(),
            ScalarUninit::Nat => _ = self.proc.ins().uninitnat(),
            ScalarUninit::Real => _ = self.proc.ins().uninitreal(),
            ScalarUninit::Str => _ = self.proc.ins().uninitstr(),
        }
    }

    fn generate_fetch_value(&mut self, fetch_ty: ty::TypeId) {
        self.emit_fetch_ins(fetch_ty);
    }

    /// Emits a fetch operation for the type
    fn emit_fetch_ins(&mut self, fetch_ty: ty::TypeId) {
        match fetch_ty.kind(self.db.up()) {
            ty::TypeKind::Boolean => _ = self.proc.ins().fetchbool(),
            ty::TypeKind::Int(ty::IntSize::Int1) => _ = self.proc.ins().fetchint1(),
            ty::TypeKind::Int(ty::IntSize::Int2) => _ = self.proc.ins().fetchint2(),
            ty::TypeKind::Int(ty::IntSize::Int4) => _ = self.proc.ins().fetchint4(),
            ty::TypeKind::Int(ty::IntSize::Int) => _ = self.proc.ins().fetchint(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => _ = self.proc.ins().fetchnat1(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => _ = self.proc.ins().fetchnat2(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => _ = self.proc.ins().fetchnat4(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => _ = self.proc.ins().fetchnat(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => _ = self.proc.ins().fetchaddr(),
            ty::TypeKind::Real(ty::RealSize::Real4) => _ = self.proc.ins().fetchreal4(),
            ty::TypeKind::Real(ty::RealSize::Real8) => _ = self.proc.ins().fetchreal8(),
            ty::TypeKind::Real(ty::RealSize::Real) => _ = self.proc.ins().fetchreal(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => {
                // chars are equivalent to nat1 on regular Turing backend
                self.proc.ins().fetchnat1();
            }
            ty::TypeKind::String | ty::TypeKind::StringN(_) => _ = self.proc.ins().fetchstr(),
            ty::TypeKind::CharN(_) => {
                // already is a pointer, don't need to dereference the pointer to storage
            }
            ty::TypeKind::Opaque(_, ty) => {
                // defer to the opaque type
                self.emit_fetch_ins(*ty);
            }
            ty::TypeKind::Constrained(..) => unimplemented!(),
            ty::TypeKind::Array(..) => unimplemented!(),
            ty::TypeKind::Enum(..) => unimplemented!(),
            ty::TypeKind::Set(..) => unimplemented!(),
            ty::TypeKind::Pointer(..) => unimplemented!(),
            ty::TypeKind::Subprogram(..) => _ = self.proc.ins().fetchaddr(),
            ty::TypeKind::Error
            | ty::TypeKind::Forward
            | ty::TypeKind::Alias(_, _)
            | ty::TypeKind::Void => {
                unreachable!()
            }
        }
    }
}

#[derive(Clone, Copy)]
enum CoerceTo {
    Real,
    Char,
    CharN(u32),
    String,
}

#[derive(Clone, Copy)]
enum OperandPairs {
    /// Lhs = Int, Rhs = Int
    IntInt,
    /// Lhs = Int, Rhs = Nat
    IntNat,
    // Lhs = Nat, Rhs = Int
    NatInt,
    /// Lhs = Nat, Rhs = Nat
    NatNat,
    /// Lhs = Real, Rhs = Real
    Real,
}

#[derive(Clone, Copy)]
enum DefTarget {
    // Bound to a locals area slot (e.g. `const`, `var`).
    Local(LocalSlot),
    // Bound to a locals area slot, pointing to an aliased address (e.g. `bind`).
    AliasLocal(LocalSlot),
    // Bound to a temporaries area slot (e.g. for-descriptor).
    Temp(TemporarySlot),
    // Bound to a procedure argument.
    Param(u32, bool),
}

// Per-procedure definition bindings
#[derive(Default)]
struct DefBindings {
    // Intra-procedure definitions
    local_defs: indexmap::IndexMap<DefId, DefTarget>,
    // Definitions outside of the procedure, typically reserved for defs that aren't already bound yet
    external_defs: indexmap::IndexMap<DefId, Vec<RelocHandle>>,
}

impl DefBindings {
    fn bind_local_slot(&mut self, from: DefId, to: LocalSlot) {
        self.local_defs.insert(from, DefTarget::Local(to));
    }

    fn bind_aliasd_def(&mut self, from: DefId, to: DefId) {
        // Bind to an existing local
        let Some(def) = self.local_defs.get(&to) else {
            panic!("aliasing {from:?} to {to:?} but latter is not bound to a def target");
        };

        self.local_defs.insert(from, *def);
    }

    fn bind_aliased_local(&mut self, from: DefId, to: LocalSlot) {
        self.local_defs.insert(from, DefTarget::AliasLocal(to));
    }

    fn bind_temporary_slot(&mut self, from: DefId, to: TemporarySlot) {
        self.local_defs.insert(from, DefTarget::Temp(to));
    }

    fn bind_parameter(&mut self, from: DefId, to_offset: u32, as_indirect: bool) {
        self.local_defs
            .insert(from, DefTarget::Param(to_offset, as_indirect));
    }

    fn emit_locate_def(&mut self, proc: &mut ProcedureBuilder, def_id: DefId) {
        if let Some(local_target) = self.local_defs.get(&def_id) {
            // Local definition
            match local_target {
                DefTarget::Local(local_slot) => proc.locate_local(*local_slot),
                DefTarget::AliasLocal(local_slot) => {
                    proc.locate_local(*local_slot);
                    // slot stores the aliased address, need to load it to retrieve the target address.
                    proc.ins().fetchaddr();
                }
                DefTarget::Temp(temporary_slot) => proc.locate_temporary(*temporary_slot),
                DefTarget::Param(offset, indirect) => {
                    proc.ins().locateparm(*offset);

                    if *indirect {
                        proc.ins().fetchaddr();
                    }
                }
            }
        } else {
            // Procedure-external definition
            let reloc_entries = self.external_defs.entry(def_id).or_insert(vec![]);

            // Add a relocation to resolve later.
            let reloc = proc.reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));
            reloc_entries.push(reloc);
        }
    }
}
