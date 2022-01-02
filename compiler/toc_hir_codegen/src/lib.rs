//! Code generation backend based on the HIR tree

use indexmap::IndexSet;
use instruction::RelocatableOffset;
use toc_analysis::db::HirAnalysis;
use toc_analysis::ty;
use toc_ast_db::db::SpanMapping;
use toc_hir::symbol::DefId;
use toc_hir::{
    body as hir_body, expr as hir_expr, item as hir_item, library as hir_library, stmt as hir_stmt,
};
use toc_reporting::CompileResult;
use toc_span::{FileId, Span};

use crate::instruction::{
    CodeOffset, ForDescriptor, GetKind, Opcode, PutKind, StdStream, StreamKind, TemporarySlot,
};

mod instruction;

pub trait CodeGenDB: HirAnalysis + SpanMapping {}

impl<T> CodeGenDB for T where T: HirAnalysis + SpanMapping {}

#[derive(Default)]
pub struct CodeBlob {
    file_map: IndexSet<FileId>,

    reloc_table: Vec<RelocInfo>,
    const_bytes: Vec<u8>,
}

impl CodeBlob {
    fn add_const_str(&mut self, str: &str) -> RelocatableOffset {
        // reserve space for str bytes & null terminator
        let storage_len = str.len() + 1;
        let reloc_at = self.reserve_const_bytes(storage_len);

        // str data + null terminator
        self.const_bytes.extend_from_slice(str.as_bytes());
        self.const_bytes.push(0);

        reloc_at
    }

    fn reserve_const_bytes(&mut self, len: usize) -> RelocatableOffset {
        let start_at = self.const_bytes.len();
        self.const_bytes.reserve(len);

        let reloc_id = self.reloc_table.len();
        self.reloc_table.push(RelocInfo {
            section: RelocSection::Manifest,
            offset: start_at,
        });

        RelocatableOffset(reloc_id)
    }
}

struct RelocInfo {
    section: RelocSection,
    offset: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RelocSection {
    Manifest,
    Code,
    Global,
}

/// Generates code from the given HIR database,
/// or producing nothing if an error was encountered before code generation.
pub fn generate_code(db: &dyn CodeGenDB) -> CompileResult<Option<CodeBlob>> {
    // Bail if there are any errors from analysis
    let res = db.analyze_libraries();

    if res.messages().has_errors() {
        return res.map(|_| None);
    }

    // Start producing blobs for each library
    // Only deal with one library right now
    let lib_graph = db.library_graph();
    let mut blob = CodeBlob::default();

    if let Some((_, library_id)) = lib_graph.library_roots().next() {
        let library = db.library(library_id);

        // Generate code for each statement body
        for body_id in library.body_ids() {
            let body = library.body(body_id);

            match &body.kind {
                hir_body::BodyKind::Stmts(_, _) => {
                    // For simple statements (e.g invariant, assign, constvar init)
                    // we can deal with them easily as they correspond to a linear
                    // sequence of instructions.
                    //
                    // For control flow statements, we need to keep track of where we should
                    // branch to, thus we can't fall back on simple HIR walking.
                    let body_code = BodyCodeGenerator::generate_body(
                        db,
                        &mut blob,
                        library_id,
                        library.as_ref(),
                        body_id,
                    );

                    println!("gen code:");
                    for (idx, opc) in body_code.0.opcodes.iter().enumerate() {
                        println!("{:4}    {:?}", idx, opc);
                    }
                    println!("\noffsets:");
                    for (idx, off) in body_code.0.code_offsets.iter().enumerate() {
                        println!("{:4}    {:?}", idx, off);
                    }
                    println!("\nlocals ({} bytes):", body_code.0.locals_size);
                    for (idx, off) in body_code.0.locals.iter().enumerate() {
                        println!("{:4}    ({:?})> {:?}", idx, off.0, off.1);
                    }
                    println!("\ntemporaries ({} bytes):", body_code.0.temps_size);
                    for (idx, off) in body_code.0.temps.iter().enumerate() {
                        println!("{:4}    {:?}", idx, off);
                    }
                }
                hir_body::BodyKind::Exprs(_expr) => {
                    // We don't start code generation from expr bodies
                    // (those are associated with `const` and `var`s)
                    continue;
                }
            }
        }
    }

    // TODO: toposort calls to module initialization bodies

    res.map(|_| None)
}

#[derive(Debug, Clone, Copy)]
enum BlockBranches {
    Loop(CodeOffset, CodeOffset),
    For(ForDescriptorSlot, CodeOffset, CodeOffset),
}

#[derive(Debug, Clone, Copy)]
enum ForDescriptorSlot {
    Local(DefId),
    Temporary(TemporarySlot),
}

impl ForDescriptorSlot {
    fn emit_locate_descriptor(&self, code_fragment: &mut CodeFragment) {
        match self {
            ForDescriptorSlot::Local(def_id) => code_fragment.emit_locate_local(*def_id),
            ForDescriptorSlot::Temporary(slot) => code_fragment.emit_locate_temp(*slot),
        }
    }
}

#[derive(Default)]
struct BodyCode(CodeFragment);

struct BodyCodeGenerator<'a> {
    db: &'a dyn CodeGenDB,
    library_id: hir_library::LibraryId,
    library: &'a hir_library::Library,
    body_id: hir_body::BodyId,
    body: &'a hir_body::Body,
    code_fragment: &'a mut CodeFragment,

    code_blob: &'a mut CodeBlob,
    last_location: Option<(usize, usize)>,
    branch_stack: Vec<BlockBranches>,
}

impl BodyCodeGenerator<'_> {
    fn generate_body(
        db: &dyn CodeGenDB,
        code_blob: &mut CodeBlob,
        library_id: hir_library::LibraryId,
        library: &hir_library::Library,
        body_id: hir_body::BodyId,
    ) -> BodyCode {
        let mut code_fragment = Default::default();
        let mut gen = BodyCodeGenerator {
            db,
            library_id,
            library,
            body_id,
            body: library.body(body_id),
            code_fragment: &mut code_fragment,

            code_blob,
            last_location: None,
            branch_stack: vec![],
        };

        match &library.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, _) => gen.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }

        BodyCode(code_fragment)
    }

    fn inline_body(&mut self, body_id: hir_body::BodyId) {
        // hacky workarounds for actual hacks...
        let mut gen = BodyCodeGenerator {
            db: self.db,
            library_id: self.library_id,
            library: self.library,
            body_id,
            body: self.library.body(body_id),
            code_fragment: self.code_fragment,

            code_blob: self.code_blob,
            last_location: self.last_location,
            branch_stack: vec![],
        };

        match &self.library.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, _) => gen.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }

        self.last_location = gen.last_location;
    }

    fn emit_location(&mut self, span: Span) {
        let file = span.file.unwrap();
        let info = self
            .db
            .map_byte_index(file, span.range.start().into())
            .unwrap();

        let code_file = self.code_blob.file_map.insert_full(file).0;
        // `LineInfo` line is zero-based, so adjust it
        let (new_file, new_line) = (code_file as u16, info.line as u16 + 1);

        let opcode = if let Some((last_file, last_line)) =
            self.last_location.replace((code_file, info.line + 1))
        {
            if (last_file, last_line) == (code_file, info.line) {
                // Same location, don't need to emit anything
                None
            } else if last_file != code_file {
                // Different file, file absolute location
                Some(Opcode::SETFILENO(new_file, new_line))
            } else if info.line.checked_sub(last_line) == Some(1) {
                // Can get away with a line relative location
                Some(Opcode::INCLINENO())
            } else {
                // Need a line absolute location
                Some(Opcode::SETLINENO(new_line))
            }
        } else {
            // Start of body, need a file absolute location
            Some(Opcode::SETFILENO(new_file, new_line))
        };

        if let Some(opcode) = opcode {
            self.code_fragment.emit_opcode(opcode);
        }
    }

    fn emit_absolute_location(&mut self) {
        if let Some((file, line)) = self.last_location {
            self.code_fragment
                .emit_opcode(Opcode::SETFILENO(file as u16, line as u16));
        }
    }

    fn generate_stmt(&mut self, stmt_id: hir_stmt::StmtId) {
        let stmt = self.body.stmt(stmt_id);
        let span = self.library.lookup_span(stmt.span);
        self.emit_location(span);

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
        }
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
            .type_of((self.library_id, self.body_id, stmt.lhs).into())
            .in_db(self.db)
            .peel_ref();

        let coerce_to = if matches!(lhs_ty.kind(), ty::TypeKind::Real(_)) {
            Some(CoerceTo::Real)
        } else {
            None
        };

        // Evaluation order is important, side effects from the rhs are visible when looking at lhs
        // For assignment, we don't want side effects from rhs eval to be visible during lookup
        self.generate_ref_expr(stmt.lhs);

        if let Some(coerce_to) = coerce_to {
            self.generate_coerced_expr(stmt.rhs, coerce_to)
        } else {
            self.generate_expr(stmt.rhs)
        };

        self.code_fragment.emit_assign(&lhs_ty, self.db);

        eprintln!("assigning reference (first operand) to value (second operand)");
    }

    fn generate_stmt_put(&mut self, stmt: &hir_stmt::Put) {
        // Steps
        // We're only concerned with stdout emission_ty

        let stream_handle = self.generate_set_stream(
            stmt.stream_num,
            None,
            StdStream::Stdout(),
            StreamKind::Put(),
        );

        for item in &stmt.items {
            let item = match item {
                hir_stmt::Skippable::Skip => {
                    // Emit stream handle reference
                    self.code_fragment.emit_locate_temp(stream_handle);
                    self.code_fragment.emit_opcode(Opcode::PUT(PutKind::Skip()));
                    continue;
                }
                hir_stmt::Skippable::Item(put_item) => put_item,
            };

            let put_ty = self
                .db
                .type_of((self.library_id, self.body_id, item.expr).into())
                .in_db(self.db)
                .peel_ref();

            let put_kind = match put_ty.kind() {
                ty::TypeKind::Boolean => PutKind::Boolean(),
                ty::TypeKind::Integer | ty::TypeKind::Int(_) => {
                    if item.opts.exponent_width().is_some() {
                        PutKind::IntExp()
                    } else if item.opts.precision().is_some() {
                        PutKind::IntFract()
                    } else {
                        PutKind::Int()
                    }
                }
                ty::TypeKind::Nat(_) => {
                    if item.opts.exponent_width().is_some() {
                        PutKind::NatExp()
                    } else if item.opts.precision().is_some() {
                        PutKind::NatFract()
                    } else {
                        PutKind::Nat()
                    }
                }
                ty::TypeKind::Real(_) => {
                    if item.opts.exponent_width().is_some() {
                        PutKind::RealExp()
                    } else if item.opts.precision().is_some() {
                        PutKind::RealFract()
                    } else {
                        PutKind::Real()
                    }
                }
                ty::TypeKind::Char => PutKind::Char(),
                ty::TypeKind::String | ty::TypeKind::StringN(_) => PutKind::String(),
                ty::TypeKind::CharN(_) => todo!(),
                // TODO: Add case for enums
                _ => unreachable!(),
            };

            // Put value onto the stack
            self.generate_expr(item.expr);

            // Deal with the put opts
            if let Some(width) = item.opts.width() {
                self.generate_expr(width)
            } else {
                // width is common to non-skip items, so have it present
                self.code_fragment.emit_opcode(Opcode::PUSHVAL0())
            }

            if let (Some(fract_width), true) = (item.opts.precision(), put_kind.has_fract_opt()) {
                self.generate_expr(fract_width);
            }

            if let (Some(exp_width), true) =
                (item.opts.exponent_width(), put_kind.has_exp_width_opt())
            {
                self.generate_expr(exp_width);
            }

            // Emit stream handle reference
            self.code_fragment.emit_locate_temp(stream_handle);
            self.code_fragment.emit_opcode(Opcode::PUT(put_kind));
        }

        if stmt.append_newline {
            // Emit newline
            self.code_fragment.emit_locate_temp(stream_handle);
            self.code_fragment.emit_opcode(Opcode::PUT(PutKind::Skip()));
        }
    }

    fn generate_stmt_get(&mut self, stmt: &hir_stmt::Get) {
        let stream_handle = self.generate_set_stream(
            stmt.stream_num,
            None,
            StdStream::Stdout(),
            StreamKind::Get(),
        );

        for item in &stmt.items {
            let item = match item {
                hir_stmt::Skippable::Skip => {
                    // Skip token
                    self.code_fragment.emit_locate_temp(stream_handle);
                    self.code_fragment.emit_opcode(Opcode::GET(GetKind::Skip()));
                    continue;
                }
                hir_stmt::Skippable::Item(item) => item,
            };

            let get_ty = self
                .db
                .type_of((self.library_id, self.body_id, item.expr).into())
                .in_db(self.db)
                .peel_ref();
            let ty_size = size_of_ty(self.db, get_ty.id()) as u32;

            let mut get_width = None;
            let get_kind = match get_ty.kind() {
                ty::TypeKind::Boolean => GetKind::Boolean(),
                ty::TypeKind::Int(_) => GetKind::Int(ty_size),
                ty::TypeKind::Nat(_) => GetKind::Nat(ty_size),
                ty::TypeKind::Real(_) => GetKind::Real(ty_size),
                ty::TypeKind::Integer => unreachable!("type must be concrete"),
                ty::TypeKind::Char => GetKind::Char(),
                ty::TypeKind::StringN(_) | ty::TypeKind::String => match item.width {
                    hir_stmt::GetWidth::Token => GetKind::StringToken(ty_size),
                    hir_stmt::GetWidth::Line => GetKind::StringLine(ty_size),
                    hir_stmt::GetWidth::Chars(width) => {
                        get_width = Some(width);
                        GetKind::StringExact(ty_size)
                    }
                },
                ty::TypeKind::CharN(_) => todo!(),
                _ => unreachable!(),
            };

            // TODO: Deal with get width once we deal with strings vars

            // Put reference onto the stack
            self.generate_ref_expr(item.expr);

            if matches!(
                get_kind,
                GetKind::StringToken(_) | GetKind::StringLine(_) | GetKind::StringExact(_)
            ) {
                // push max width
                if let Some(width) = get_width {
                    self.generate_expr(width);
                }

                // and max length
                self.code_fragment.emit_opcode(Opcode::PUSHINT(ty_size - 1));
            }

            self.code_fragment.emit_locate_temp(stream_handle);
            self.code_fragment.emit_opcode(Opcode::GET(get_kind));
        }
    }

    fn generate_set_stream(
        &mut self,
        stream_num: Option<hir_expr::ExprId>,
        status_expr: Option<hir_expr::ExprId>,
        default_stream: StdStream,
        op: StreamKind,
    ) -> TemporarySlot {
        // Make a temporary to store the stream handle
        let stream_handle = self.code_fragment.allocate_temporary_space(4);

        if let Some(stream_num) = stream_num {
            // Make temporary to store the status_var location from SETSTREAM
            let status_var = self.code_fragment.allocate_temporary_space(4);

            // Use this stream as the target
            self.generate_expr(stream_num);
            if let Some(_status_expr) = status_expr {
                todo!();
            } else {
                self.code_fragment.emit_opcode(Opcode::PUSHADDR(0)); // no place to store status
            }

            // References to the temporary store
            self.code_fragment.emit_locate_temp(stream_handle);
            self.code_fragment.emit_locate_temp(status_var);

            self.code_fragment.emit_opcode(Opcode::SETSTREAM(op));
        } else {
            self.code_fragment.emit_locate_temp(stream_handle);

            // Use stdout as the target stream
            self.code_fragment
                .emit_opcode(Opcode::SETSTDSTREAM(default_stream));
        }

        stream_handle
    }

    fn generate_stmt_for(&mut self, stmt: &hir_stmt::For) {
        let descriptor_size = std::mem::size_of::<ForDescriptor>();

        let descriptor_slot = if let Some(counter_def) = stmt.counter_def {
            let local = DefId(self.library_id, counter_def);
            self.code_fragment
                .allocate_local_space(local, descriptor_size);
            ForDescriptorSlot::Local(DefId(self.library_id, counter_def))
        } else {
            // Reserve temporary space for the descriptor
            let temporary = self.code_fragment.allocate_temporary_space(descriptor_size);
            ForDescriptorSlot::Temporary(temporary)
        };

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
            // TODO: Emit CHKRANGE, within positive int4 bounds
        } else {
            self.code_fragment.emit_opcode(Opcode::PUSHVAL1());
        }

        // Negate if decreasing
        if stmt.is_decreasing {
            self.code_fragment.emit_opcode(Opcode::NEGINT());
        }

        // Emit loop body
        let loop_start = self.code_fragment.new_branch();
        let after_loop = self.code_fragment.new_branch();

        match descriptor_slot {
            ForDescriptorSlot::Local(def_id) => self.code_fragment.emit_locate_local(def_id),
            ForDescriptorSlot::Temporary(slot) => self.code_fragment.emit_locate_temp(slot),
        }
        self.code_fragment.emit_opcode(Opcode::FOR(after_loop));

        self.code_fragment.anchor_branch(loop_start);
        {
            // Have a fixed location for the branch to go to
            self.emit_absolute_location();

            self.branch_stack
                .push(BlockBranches::For(descriptor_slot, loop_start, after_loop));

            self.generate_stmt_list(&stmt.stmts);

            descriptor_slot.emit_locate_descriptor(self.code_fragment);
            self.code_fragment.emit_opcode(Opcode::ENDFOR(loop_start));
            self.branch_stack.pop();
        }
        self.code_fragment.anchor_branch(after_loop);
    }

    fn generate_stmt_loop(&mut self, stmt: &hir_stmt::Loop) {
        // Need to:
        // - Create a new branch, anchored to next instruction
        // - Create a forward branch for after the loop
        // - Anchor floating branch to instruction after the backward jump
        let loop_start = self.code_fragment.place_branch();
        let after_loop = self.code_fragment.new_branch();

        // Have a fixed location for the branch to go to
        self.emit_absolute_location();

        self.branch_stack
            .push(BlockBranches::Loop(loop_start, after_loop));
        self.generate_stmt_list(&stmt.stmts);
        self.code_fragment.emit_opcode(Opcode::JUMPB(loop_start));
        self.branch_stack.pop();
        self.code_fragment.anchor_branch(after_loop);
    }

    fn generate_stmt_exit(&mut self, stmt: &hir_stmt::Exit) {
        // Branch to after the loop
        let block_branches = self.branch_stack.last().unwrap();
        let branch_to = match block_branches {
            BlockBranches::Loop(_, after_block) => *after_block,
            BlockBranches::For(_, _, after_block) => *after_block,
        };

        if let Some(condition) = stmt.when_condition {
            // Use INFIXOR as an alias of "branch if true"
            self.generate_expr(condition);
            self.code_fragment.emit_opcode(Opcode::INFIXOR(branch_to));
        } else {
            self.code_fragment.emit_opcode(Opcode::JUMP(branch_to));
        }
    }

    fn generate_stmt_if(&mut self, stmt: &hir_stmt::If) {
        // Steps:
        // - Evaluate condition
        // - If false, branch after true block
        // - Otherwise, proceed through true block and branch to after if statement
        self.generate_expr(stmt.condition);

        let after_true = self.code_fragment.new_branch();
        let after_false = self.code_fragment.new_branch();

        // Emit true branch
        self.code_fragment.emit_opcode(Opcode::IF(after_true));
        self.generate_stmt(stmt.true_branch);
        self.code_fragment.emit_opcode(Opcode::JUMP(after_false));
        self.code_fragment.anchor_branch(after_true);
        self.emit_absolute_location();

        // Then false branch
        if let Some(false_branch) = stmt.false_branch {
            self.generate_stmt(false_branch);
        }
        self.code_fragment.anchor_branch(after_false);
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
            .type_of((self.library_id, self.body_id, stmt.discriminant).into())
            .in_db(self.db)
            .peel_ref();

        let discrim_value = self
            .code_fragment
            .allocate_temporary_space(size_of_ty(self.db, discrim_ty.id()));
        self.code_fragment.emit_locate_temp(discrim_value);
        self.code_fragment
            .emit_assign_into_var(&discrim_ty, self.db);

        let mut arm_targets = vec![];
        let mut has_default = false;

        for arm in &stmt.arms {
            let target = self.code_fragment.new_branch();
            arm_targets.push(target);

            match &arm.selectors {
                hir_stmt::CaseSelector::Exprs(exprs) => {
                    for expr in exprs {
                        // TODO: Coerce expr into the correct type
                        self.generate_expr(*expr);
                        self.code_fragment.emit_locate_temp(discrim_value);

                        let expr_ty = self
                            .db
                            .type_of((self.library_id, self.body_id, *expr).into())
                            .in_db(self.db)
                            .peel_ref();

                        let eq_op = self.select_eq_op(expr_ty.kind(), discrim_ty.kind());
                        self.code_fragment.emit_opcode(eq_op);

                        // Branch if any are true
                        self.code_fragment.emit_opcode(Opcode::INFIXOR(target));
                    }
                }
                hir_stmt::CaseSelector::Default => {
                    has_default = true;
                    self.code_fragment.emit_opcode(Opcode::JUMP(target));
                    break;
                }
            }
        }

        let after_case = self.code_fragment.new_branch();
        if !has_default {
            self.code_fragment.emit_opcode(Opcode::JUMP(after_case));
        }

        for (arm, target) in stmt.arms.iter().zip(arm_targets.iter()) {
            self.code_fragment.anchor_branch(*target);
            self.emit_absolute_location();
            self.generate_stmt_list(&arm.stmts);
            self.code_fragment.emit_opcode(Opcode::JUMP(after_case));
        }

        self.code_fragment.anchor_branch(after_case);
        self.emit_absolute_location();
    }

    fn generate_item(&mut self, item_id: hir_item::ItemId) {
        let item = self.library.item(item_id);
        let span = self.library.lookup_span(item.span);
        self.emit_location(span);

        match &item.kind {
            hir_item::ItemKind::ConstVar(item) => self.generate_item_constvar(item),
            hir_item::ItemKind::Module(_) => {
                // We already generate code for module bodies as part of walking
                // over all of the bodies in a library
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

        let def_ty = self
            .db
            .type_of(DefId(self.library_id, item.def_id).into())
            .in_db(self.db)
            .peel_ref();
        self.code_fragment.allocate_local(
            self.db,
            DefId(self.library_id, item.def_id),
            def_ty.id(),
        );

        if let Some(init_body) = item.init_expr {
            eprintln!("inlining init body {:?}", init_body);
            self.inline_body(init_body);

            eprintln!("assigning def {:?} to previously produced value", init_body);
            self.code_fragment
                .emit_locate_local(DefId(self.library_id, item.def_id));
            self.code_fragment.emit_assign_into_var(&def_ty, self.db);
        } else if has_uninit(&def_ty) {
            eprintln!(
                "assigning def {:?} to uninit pattern for type `{}`",
                item.def_id, def_ty
            );
            self.code_fragment
                .emit_locate_local(DefId(self.library_id, item.def_id));
            self.code_fragment.emit_assign_uninit(&def_ty);
        }
    }

    fn generate_coerced_expr(&mut self, expr_id: hir_expr::ExprId, coerce_to: CoerceTo) {
        let expr_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr_id).into())
            .in_db(self.db)
            .peel_ref();

        let coerce_op = match (coerce_to, expr_ty.kind()) {
            (CoerceTo::Real, ty::TypeKind::Nat(_)) => Some(Opcode::NATREAL()),
            (CoerceTo::Real, int) if int.is_integer() => Some(Opcode::INTREAL()),
            _ => None,
        };

        self.generate_expr(expr_id);

        if let Some(opcode) = coerce_op {
            eprintln!("coercing into {:?}", coerce_op);
            self.code_fragment.emit_opcode(opcode);
        }
    }

    fn generate_expr(&mut self, expr_id: hir_expr::ExprId) {
        let expr = self.body.expr(expr_id);
        let span = self.library.lookup_span(expr.span);
        self.emit_location(span);

        match &expr.kind {
            hir_expr::ExprKind::Missing => unreachable!("malformed code reached code generation"),
            hir_expr::ExprKind::Literal(expr) => self.generate_expr_literal(expr),
            hir_expr::ExprKind::Binary(expr) => self.generate_expr_binary(expr),
            hir_expr::ExprKind::Unary(expr) => self.generate_expr_unary(expr),
            hir_expr::ExprKind::Name(expr) => self.generate_expr_name(expr),
        }
    }

    fn generate_expr_literal(&mut self, expr: &hir_expr::Literal) {
        // Steps
        // - Push value onto the operand stack
        eprintln!("pushing value {:?} to operand stack", expr);
        match expr {
            hir_expr::Literal::Integer(value) => match value {
                0..=0xFF => self
                    .code_fragment
                    .emit_opcode(Opcode::PUSHINT1(*value as u8)),
                0..=0xFFFF => self
                    .code_fragment
                    .emit_opcode(Opcode::PUSHINT2(*value as u16)),
                0..=0xFFFFFFFF => self
                    .code_fragment
                    .emit_opcode(Opcode::PUSHINT(*value as u32)),
                _ => unreachable!("this backend does not support values larger than u32::MAX"),
            },
            hir_expr::Literal::Real(value) => {
                self.code_fragment.emit_opcode(Opcode::PUSHREAL(*value))
            }
            hir_expr::Literal::Char(value) => {
                if *value as u32 <= 0xFF {
                    self.code_fragment
                        .emit_opcode(Opcode::PUSHINT1(*value as u8))
                } else {
                    unreachable!("this backend does not support non-(extended) ascii code points")
                }
            }
            hir_expr::Literal::CharSeq(_) => todo!(),
            hir_expr::Literal::String(value) => {
                let str_at = self.code_blob.add_const_str(value);
                self.code_fragment.emit_opcode(Opcode::PUSHADDR1(str_at));
            }
            hir_expr::Literal::Boolean(value) => self.code_fragment.emit_opcode(
                value
                    .then(Opcode::PUSHVAL1)
                    .unwrap_or_else(Opcode::PUSHVAL0),
            ),
        }
    }

    fn generate_expr_binary(&mut self, expr: &hir_expr::Binary) {
        // Steps
        // - Check if any type coercion needs to be done
        // - Generate each code for the corresponding operands (dropping in type casts as necessary)
        // - Emit instruction corresponding to the operation

        let lhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr.lhs).into())
            .in_db(self.db)
            .peel_ref();
        let rhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr.rhs).into())
            .in_db(self.db)
            .peel_ref();

        let opcode = match expr.op.item() {
            hir_expr::BinaryOp::Add => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(),
                    rhs_ty.kind(),
                    Opcode::ADDREAL(),
                    Opcode::ADDNAT(),
                    Opcode::ADDNATINT(),
                    Opcode::ADDINTNAT(),
                    Opcode::ADDINT(),
                )
                .unwrap(),
            hir_expr::BinaryOp::Sub => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(),
                    rhs_ty.kind(),
                    Opcode::SUBREAL(),
                    Opcode::SUBNAT(),
                    Opcode::SUBNATINT(),
                    Opcode::SUBINTNAT(),
                    Opcode::SUBINT(),
                )
                .unwrap(),
            hir_expr::BinaryOp::Mul => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::MULREAL()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MULNAT()
                }
                (lhs, rhs) | (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MULINT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::Div => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::DIVREAL()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::DIVNAT()
                }
                (ty, _) | (_, ty) if ty.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::DIVINT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::RealDiv => {
                self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                Opcode::REALDIVIDE()
            }
            hir_expr::BinaryOp::Mod => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::MODREAL()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MODNAT()
                }
                (ty, _) | (_, ty) if ty.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MODINT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::Rem => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::REMREAL()
                }
                (ty::TypeKind::Int(_) | ty::TypeKind::Integer, _)
                | (_, ty::TypeKind::Int(_) | ty::TypeKind::Integer) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::REMINT()
                }
                (ty::TypeKind::Nat(_), _) | (_, ty::TypeKind::Nat(_)) => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::MODNAT()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::Exp => match (lhs_ty.kind(), rhs_ty.kind()) {
                (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::EXPINTINT()
                }
                (ty::TypeKind::Real(_), rhs) if rhs.is_integer() => {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);
                    Opcode::EXPREALINT()
                }
                (ty::TypeKind::Real(_), ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                    self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
                    Opcode::EXPREALREAL()
                }
                _ => unreachable!(),
            },
            hir_expr::BinaryOp::And => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::AND()
            }
            hir_expr::BinaryOp::Or => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::OR()
            }
            hir_expr::BinaryOp::Xor => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::XOR()
            }
            hir_expr::BinaryOp::Shl => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::SHL()
            }
            hir_expr::BinaryOp::Shr => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
                Opcode::SHR()
            }
            hir_expr::BinaryOp::Less | hir_expr::BinaryOp::GreaterEq => {
                let cmp_op = if let Some(cmp_op) = self.dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(),
                    rhs_ty.kind(),
                    Opcode::GEREAL(),
                    Opcode::GENAT(),
                    Opcode::GENATINT(),
                    Opcode::GEINTNAT(),
                    Opcode::GEINT(),
                ) {
                    cmp_op
                } else {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);

                    match (lhs_ty.kind(), rhs_ty.kind()) {
                        (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::GENAT(),
                        _ => unreachable!(),
                    }
                };

                if expr.op.item() == &hir_expr::BinaryOp::GreaterEq {
                    cmp_op
                } else {
                    self.code_fragment.emit_opcode(cmp_op);
                    Opcode::NOT()
                }
            }
            hir_expr::BinaryOp::Greater | hir_expr::BinaryOp::LessEq => {
                let cmp_op = if let Some(cmp_op) = self.dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(),
                    rhs_ty.kind(),
                    Opcode::LEREAL(),
                    Opcode::LENAT(),
                    Opcode::LENATINT(),
                    Opcode::LEINTNAT(),
                    Opcode::LEINT(),
                ) {
                    cmp_op
                } else {
                    self.generate_expr(expr.lhs);
                    self.generate_expr(expr.rhs);

                    match (lhs_ty.kind(), rhs_ty.kind()) {
                        (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::LENAT(),
                        _ => unreachable!(),
                    }
                };

                if expr.op.item() == &hir_expr::BinaryOp::LessEq {
                    cmp_op
                } else {
                    self.code_fragment.emit_opcode(cmp_op);
                    Opcode::NOT()
                }
            }
            hir_expr::BinaryOp::Equal | hir_expr::BinaryOp::NotEqual => {
                self.coerce_to_same(expr, lhs_ty.kind(), rhs_ty.kind());
                let cmp_op = self.select_eq_op(lhs_ty.kind(), rhs_ty.kind());

                if expr.op.item() == &hir_expr::BinaryOp::Equal {
                    cmp_op
                } else {
                    self.code_fragment.emit_opcode(cmp_op);
                    Opcode::NOT()
                }
            }
            hir_expr::BinaryOp::In => todo!(),
            hir_expr::BinaryOp::NotIn => todo!(),
            hir_expr::BinaryOp::Imply => {
                // TODO: This does not implement short-circuiting for booleans
                self.generate_expr(expr.lhs);
                self.code_fragment.emit_opcode(Opcode::NOT());
                self.generate_expr(expr.rhs);
                Opcode::OR()
            }
        };

        eprintln!(
            "applying operation {:?} to previous two operand",
            expr.op.item()
        );

        self.code_fragment.emit_opcode(opcode);
    }

    #[allow(clippy::too_many_arguments)]
    fn dispatch_over_numbers(
        &mut self,
        expr: &hir_expr::Binary,
        lhs: &ty::TypeKind,
        rhs: &ty::TypeKind,
        real: Opcode,
        nat_nat: Opcode,
        nat_int: Opcode,
        int_nat: Opcode,
        int_int: Opcode,
    ) -> Option<Opcode> {
        self.coerce_to_same(expr, lhs, rhs);
        self.select_over_numbers(lhs, rhs, real, nat_nat, nat_int, int_nat, int_int)
    }

    #[allow(clippy::too_many_arguments)]
    fn select_over_numbers(
        &self,
        lhs: &ty::TypeKind,
        rhs: &ty::TypeKind,
        real: Opcode,
        nat_nat: Opcode,
        nat_int: Opcode,
        int_nat: Opcode,
        int_int: Opcode,
    ) -> Option<Opcode> {
        match (lhs, rhs) {
            (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => Some(real),
            (ty::TypeKind::Nat(_), ty::TypeKind::Nat(_)) => Some(nat_nat),
            (ty::TypeKind::Nat(_), other) if other.is_integer() => Some(nat_int),
            (other, ty::TypeKind::Nat(_)) if other.is_integer() => Some(int_nat),
            (lhs, rhs) if lhs.is_integer() && rhs.is_integer() => Some(int_int),
            _ => None,
        }
    }

    fn coerce_to_same(&mut self, expr: &hir_expr::Binary, lhs: &ty::TypeKind, rhs: &ty::TypeKind) {
        match (lhs, rhs) {
            (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                self.generate_coerced_expr(expr.lhs, CoerceTo::Real);
                self.generate_coerced_expr(expr.rhs, CoerceTo::Real);
            }
            _ => {
                self.generate_expr(expr.lhs);
                self.generate_expr(expr.rhs);
            }
        }
    }

    fn select_eq_op(&self, lhs_kind: &ty::TypeKind, rhs_kind: &ty::TypeKind) -> Opcode {
        if let Some(cmp_op) = self.select_over_numbers(
            lhs_kind,
            rhs_kind,
            Opcode::EQREAL(),
            Opcode::EQNAT(),
            Opcode::EQINTNAT(),
            Opcode::EQINTNAT(),
            Opcode::EQINT(),
        ) {
            cmp_op
        } else {
            match (lhs_kind, rhs_kind) {
                (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::EQNAT(),
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
            .type_of((self.library_id, self.body_id, expr.rhs).into())
            .in_db(self.db)
            .peel_ref();

        self.generate_expr(expr.rhs);

        match expr.op.item() {
            hir_expr::UnaryOp::Identity => {} // no-op
            hir_expr::UnaryOp::Not => {
                if rhs_ty.kind().is_integer() {
                    // Composite
                    self.code_fragment.emit_opcode(Opcode::PUSHINT(!0));
                    self.code_fragment.emit_opcode(Opcode::XOR());
                } else {
                    self.code_fragment.emit_opcode(Opcode::NOT())
                }
            }
            hir_expr::UnaryOp::Negate => match rhs_ty.kind() {
                ty::TypeKind::Int(_) | ty::TypeKind::Nat(_) => {
                    // should already be dealing with promoted types
                    self.code_fragment.emit_opcode(Opcode::NEGINT())
                }
                ty::TypeKind::Real(_) => self.code_fragment.emit_opcode(Opcode::NEGREAL()),
                _ => unreachable!(),
            },
        }
    }

    fn generate_expr_name(&mut self, expr: &hir_expr::Name) {
        // Steps
        // - Load value from the referenced def (may need to perform canonical name resolution)
        match expr {
            hir_expr::Name::Name(def_id) => {
                let info = self.library.local_def(*def_id);
                eprintln!("loading value from def {:?} ({:?})", info, def_id);

                let def_ty = self
                    .db
                    .type_of(DefId(self.library_id, *def_id).into())
                    .in_db(self.db)
                    .peel_ref();

                self.code_fragment
                    .emit_locate_local(DefId(self.library_id, *def_id));
                self.code_fragment.emit_fetch_value(&def_ty);
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }

    /// Like `generate_expr`, but for producing references to locations
    fn generate_ref_expr(&mut self, expr_id: hir_expr::ExprId) {
        let expr = self.body.expr(expr_id);
        let span = self.library.lookup_span(expr.span);
        self.emit_location(span);

        match &expr.kind {
            hir_expr::ExprKind::Name(ref_expr) => self.generate_ref_expr_name(ref_expr),
            // These can never be in reference producing position
            hir_expr::ExprKind::Missing
            | hir_expr::ExprKind::Literal(_)
            | hir_expr::ExprKind::Binary(_)
            | hir_expr::ExprKind::Unary(_) => {
                unreachable!("malformed code reached code generation")
            }
        }
    }

    fn generate_ref_expr_name(&mut self, expr: &hir_expr::Name) {
        // Steps
        // - Produce pointer to referenced def (may need to perform canonical name resolution)
        //   - If it's a stack var, it should be to the locals space
        //   - If it's a global var, it should be from a relocatable patch to the globals space
        match expr {
            hir_expr::Name::Name(def_id) => {
                let info = self.library.local_def(*def_id);
                eprintln!("locating value from def {:?} ({:?})", info, def_id);

                self.code_fragment
                    .emit_locate_local(DefId(self.library_id, *def_id))
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }
}

enum CoerceTo {
    Real,
}

#[derive(Debug, Clone, Copy)]
struct StackSlot {
    offset: u32,
    _size: u32,
}

#[derive(Debug)]
enum OffsetTarget {
    Branch(Option<usize>),
}

#[derive(Default)]
struct CodeFragment {
    locals: indexmap::IndexMap<DefId, StackSlot>,
    locals_size: u32,
    temps: Vec<StackSlot>,
    temps_size: u32,

    opcodes: Vec<Opcode>,
    code_offsets: Vec<OffsetTarget>,
}

impl CodeFragment {
    fn allocate_local(&mut self, db: &dyn CodeGenDB, def_id: DefId, def_ty: ty::TypeId) {
        self.allocate_local_space(def_id, size_of_ty(db, def_ty));
    }

    fn allocate_local_space(&mut self, def_id: DefId, size: usize) {
        // Align to the nearest stack slot
        let size = align_up_to(size, 4).try_into().unwrap();
        let offset = self.locals_size;

        self.locals.insert(
            def_id,
            StackSlot {
                offset,
                _size: size,
            },
        );
        self.locals_size += size;
    }

    fn allocate_temporary_space(&mut self, size: usize) -> TemporarySlot {
        // Align to the nearest stack slot
        let size = align_up_to(size, 4).try_into().unwrap();
        let offset = self.temps_size;
        let handle = self.temps.len();

        self.temps.push(StackSlot {
            offset,
            _size: size,
        });

        self.temps_size += size;
        TemporarySlot(handle)
    }

    fn emit_opcode(&mut self, opcode: Opcode) {
        if matches!(opcode, Opcode::INCLINENO()) {
            // Fold together INCLINENO and SETFILENO / SETLINENO
            match self.opcodes.last_mut() {
                Some(Opcode::SETFILENO(_, line) | Opcode::SETLINENO(line)) => *line += 1,
                _ => self.opcodes.push(Opcode::INCLINENO()),
            }
        } else {
            self.opcodes.push(opcode)
        }
    }

    fn emit_locate_local(&mut self, def_id: DefId) {
        let slot_info = self.locals.get(&def_id).expect("def not reserved yet");
        let offset = slot_info.offset;

        self.emit_opcode(Opcode::LOCATELOCAL(offset));
    }

    fn emit_locate_temp(&mut self, temp: TemporarySlot) {
        // Don't know the final size of temporaries yet, so use temporary slots.
        self.emit_opcode(Opcode::LOCATETEMP(temp));
    }

    fn emit_assign_into_var(&mut self, into_tyref: &ty::TyRef<dyn CodeGenDB>, db: &dyn CodeGenDB) {
        let opcode = match into_tyref.kind() {
            ty::TypeKind::Boolean => Opcode::ASNINT1INV(),
            ty::TypeKind::Int(ty::IntSize::Int1) => Opcode::ASNINT1INV(),
            ty::TypeKind::Int(ty::IntSize::Int2) => Opcode::ASNINT2INV(),
            ty::TypeKind::Int(ty::IntSize::Int4) => Opcode::ASNINT4INV(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::ASNINTINV(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => Opcode::ASNNAT1INV(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => Opcode::ASNNAT2INV(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => Opcode::ASNNAT4INV(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::ASNNATINV(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::ASNADDRINV(),
            ty::TypeKind::Real(ty::RealSize::Real4) => Opcode::ASNREAL4INV(),
            ty::TypeKind::Real(ty::RealSize::Real8) => Opcode::ASNREAL8INV(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::ASNREALINV(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => Opcode::ASNINT1INV(),
            ty::TypeKind::String => {
                // Push full storage size
                self.emit_opcode(Opcode::PUSHINT(255));
                Opcode::ASNSTRINV()
            }
            ty::TypeKind::CharN(_) => todo!(),
            ty::TypeKind::StringN(seq_size) => {
                // Push corresponding storage size
                let char_len = seq_size
                    .fixed_len(db, Span::default())
                    .ok()
                    .flatten()
                    .expect("eval should succeed and not be dyn");

                self.emit_opcode(Opcode::PUSHINT(char_len.into_u32().expect("not a u32")));
                Opcode::ASNSTRINV()
            }
            ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    fn emit_assign(&mut self, into_tyref: &ty::TyRef<dyn CodeGenDB>, db: &dyn CodeGenDB) {
        let opcode = match into_tyref.kind() {
            ty::TypeKind::Boolean => Opcode::ASNINT1(),
            ty::TypeKind::Int(ty::IntSize::Int1) => Opcode::ASNINT1(),
            ty::TypeKind::Int(ty::IntSize::Int2) => Opcode::ASNINT2(),
            ty::TypeKind::Int(ty::IntSize::Int4) => Opcode::ASNINT4(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::ASNINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => Opcode::ASNNAT1(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => Opcode::ASNNAT2(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => Opcode::ASNNAT4(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::ASNNAT(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::ASNADDR(),
            ty::TypeKind::Real(ty::RealSize::Real4) => Opcode::ASNREAL4(),
            ty::TypeKind::Real(ty::RealSize::Real8) => Opcode::ASNREAL8(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::ASNREAL(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => Opcode::ASNINT1(),
            ty::TypeKind::String => {
                // Push full storage size
                self.emit_opcode(Opcode::PUSHINT(255));
                Opcode::ASNSTR()
            }
            ty::TypeKind::CharN(_) => todo!(),
            ty::TypeKind::StringN(seq_size) => {
                // Push corresponding storage size
                let char_len = seq_size
                    .fixed_len(db, Span::default())
                    .ok()
                    .flatten()
                    .expect("eval should succeed and not be dyn");

                self.emit_opcode(Opcode::PUSHINT(char_len.into_u32().expect("not a u32")));
                Opcode::ASNSTR()
            }
            ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    // Expects a destination address to be present
    fn emit_assign_uninit(&mut self, uninit_ty: &ty::TyRef<dyn CodeGenDB>) {
        let opcode = match uninit_ty.kind() {
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::UNINITADDR(),
            ty::TypeKind::Boolean => Opcode::UNINITBOOLEAN(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::UNINITINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::UNINITNAT(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::UNINITREAL(),
            ty::TypeKind::String => Opcode::UNINITSTR(),
            _ => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    fn emit_fetch_value(&mut self, ty_ref: &ty::TyRef<dyn CodeGenDB>) {
        let opcode = match ty_ref.kind() {
            ty::TypeKind::Boolean => Opcode::FETCHBOOL(),
            ty::TypeKind::Int(ty::IntSize::Int1) => Opcode::FETCHINT1(),
            ty::TypeKind::Int(ty::IntSize::Int2) => Opcode::FETCHINT2(),
            ty::TypeKind::Int(ty::IntSize::Int4) => Opcode::FETCHINT4(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::FETCHINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => Opcode::FETCHNAT1(),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => Opcode::FETCHNAT2(),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => Opcode::FETCHNAT4(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::FETCHNAT(),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::FETCHADDR(),
            ty::TypeKind::Real(ty::RealSize::Real4) => Opcode::FETCHREAL4(),
            ty::TypeKind::Real(ty::RealSize::Real8) => Opcode::FETCHREAL8(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::FETCHREAL(),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => Opcode::FETCHNAT1(), // chars are equivalent to nat1 on regular Turing backend
            ty::TypeKind::String | ty::TypeKind::StringN(_) => Opcode::FETCHSTR(),
            ty::TypeKind::CharN(_) => todo!(),
            ty::TypeKind::Error | ty::TypeKind::Ref(_, _) => unreachable!(),
        };

        self.emit_opcode(opcode);
    }

    fn place_branch(&mut self) -> CodeOffset {
        let offset = self.new_branch();
        self.anchor_branch_to(offset, self.opcodes.len());
        offset
    }

    fn new_branch(&mut self) -> CodeOffset {
        let idx = self.code_offsets.len();
        self.code_offsets.push(OffsetTarget::Branch(None));
        CodeOffset(idx)
    }

    fn anchor_branch(&mut self, offset: CodeOffset) {
        self.anchor_branch_to(offset, self.opcodes.len())
    }

    fn anchor_branch_to(&mut self, offset: CodeOffset, to_opcode: usize) {
        *self.code_offsets.get_mut(offset.0).unwrap() = OffsetTarget::Branch(Some(to_opcode))
    }
}

fn size_of_ty(db: &dyn CodeGenDB, ty: ty::TypeId) -> usize {
    let ty_ref = ty.in_db(db);

    match ty_ref.kind() {
        ty::TypeKind::Boolean => 1,
        ty::TypeKind::Int(ty::IntSize::Int1) => 1,
        ty::TypeKind::Int(ty::IntSize::Int2) => 2,
        ty::TypeKind::Int(ty::IntSize::Int4) => 4,
        ty::TypeKind::Int(ty::IntSize::Int) => 4,
        ty::TypeKind::Nat(ty::NatSize::Nat1) => 1,
        ty::TypeKind::Nat(ty::NatSize::Nat2) => 2,
        ty::TypeKind::Nat(ty::NatSize::Nat4) => 4,
        ty::TypeKind::Nat(ty::NatSize::Nat) => 4,
        ty::TypeKind::Nat(ty::NatSize::AddressInt) => 4,
        ty::TypeKind::Real(ty::RealSize::Real4) => 4,
        ty::TypeKind::Real(ty::RealSize::Real8) => 8,
        ty::TypeKind::Real(ty::RealSize::Real) => 8,
        ty::TypeKind::Integer => unreachable!("type should be concrete"),
        ty::TypeKind::Char => 1,
        ty::TypeKind::String => {
            // max chars (including null terminator)
            256
        }
        ty::TypeKind::CharN(_seq_size) => todo!(),
        ty::TypeKind::StringN(seq_size) => {
            let char_len = seq_size
                .fixed_len(db, Span::default())
                .ok()
                .flatten()
                .expect("eval should succeed and not be dyn");

            // Storage size includes the always present null terminator
            let char_len = (char_len.into_u32().expect("size should be a u32")) as usize;
            char_len + 1
        }
        ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
    }
}

fn has_uninit(ty_ref: &ty::TyRef<dyn CodeGenDB>) -> bool {
    matches!(
        ty_ref.kind(),
        ty::TypeKind::Boolean
            | ty::TypeKind::Int(ty::IntSize::Int)
            | ty::TypeKind::Nat(ty::NatSize::AddressInt | ty::NatSize::Nat)
            | ty::TypeKind::Real(ty::RealSize::Real)
            | ty::TypeKind::String
    )
}

fn align_up_to(value: usize, align: usize) -> usize {
    assert!(align.is_power_of_two());
    let mask = align - 1;

    (value + mask) & !mask
}
