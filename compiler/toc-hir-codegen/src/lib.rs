//! Code generation backend based on the HIR tree

use std::io;

use byteorder::{LittleEndian as LE, WriteBytesExt};
use indexmap::{IndexMap, IndexSet};
use instruction::{CheckKind, RelocatableOffset};
use toc_analysis::{db::HirAnalysis, ty};
use toc_hir::{
    body as hir_body, expr as hir_expr, item as hir_item, package as hir_package,
    package::InPackage,
    stmt as hir_stmt,
    symbol::{DefId, LocalDefId},
};
use toc_reporting::CompileResult;
use toc_span::{FileId, Span};

use crate::instruction::{
    AbortSource, CodeOffset, ForDescriptor, GetKind, Opcode, PutKind, StdStream, StreamKind,
    TemporarySlot,
};

mod instruction;

pub trait CodeGenDB: HirAnalysis {}

impl<T> CodeGenDB for T where T: HirAnalysis {}

#[derive(Default)]
pub struct CodeBlob {
    file_map: IndexSet<FileId>,

    reloc_table: Vec<RelocInfo>,
    const_bytes: Vec<u8>,

    main_body: Option<(hir_package::PackageId, hir_body::BodyId)>,
    body_fragments: IndexMap<(hir_package::PackageId, hir_body::BodyId), BodyCode>,
}

impl CodeBlob {
    pub fn encode_to(&self, db: &dyn CodeGenDB, out: &mut impl io::Write) -> io::Result<()> {
        const HEADER_SIG: &[u8] = b"TWEST\0";
        const MAIN_MAGIC: &[u8] = b"***MAIN PROGRAM***\0";
        // Write out bytecode header
        // Signature //
        out.write_all(HEADER_SIG)?;

        // TProlog Section //
        // close_window_on_terminate
        out.write_u32::<LE>(0)?;
        // run_with_args
        out.write_u32::<LE>(0)?;
        // center_window
        out.write_u32::<LE>(0)?;
        // dont_terminate
        out.write_u32::<LE>(0)?;

        // Main header //
        out.write_all(&[0; 0x1010 - 4 * 4 - HEADER_SIG.len() * 2])?;
        // Header tail //
        out.write_all(HEADER_SIG)?;

        // Write out file info
        assert!(self.file_map.len() < (u16::MAX - 1).into());

        for (idx, file_id) in self.file_map.iter().enumerate() {
            // Guaranteed to have less than u16::MAX files per the assert above
            let blob_id = u16::try_from(idx).unwrap() + 1;

            // file_id
            out.write_u16::<LE>(blob_id)?;
            // filename
            let filename = file_id.into_raw().raw_path(db.up()).as_str();
            let mut encoded_filename = [0; 255];
            // trunc to 256 bytes
            let filename_len = filename.len().min(encoded_filename.len());
            encoded_filename[..filename_len].copy_from_slice(&filename.as_bytes()[..filename_len]);
            out.write_all(&encoded_filename)?;
            // body_unit
            out.write_u16::<LE>(0)?;
            // stub_unit
            out.write_u16::<LE>(0)?;

            // code_table
            // can't use an empty table since TProlog & tulip-vm assumes that
            // a code table of at least 4 bytes is present
            out.write_u32::<LE>(4)?;
            out.write_u32::<LE>(0xAAAAAAAA)?;
            // manifest_table
            out.write_u32::<LE>(0)?;
            // globals_size
            out.write_u32::<LE>(0)?;

            // manifest_patches
            out.write_u32::<LE>(0xFFFFFFFF)?;

            // local_code_patch_head
            out.write_u32::<LE>(0)?;
            // local_manifest_patch_head
            out.write_u32::<LE>(0)?;
            // local_globals_patch_head
            out.write_u32::<LE>(0)?;

            // external_code_patches
            out.write_u16::<LE>(0xFFFF)?;
            // external_manifest_patches
            out.write_u16::<LE>(0xFFFF)?;
            // external_global_patches
            out.write_u16::<LE>(0xFFFF)?;
        }

        // Write out main bytecode file
        // Always occupies file section 0
        let main_id = 0;

        // file_id
        out.write_u16::<LE>(main_id)?;
        // filename
        {
            const MAIN_FILE_NAME: &[u8] = b"<MAIN FILE>";
            let mut encoded_filename = [0; 255];
            encoded_filename[..MAIN_FILE_NAME.len()].copy_from_slice(MAIN_FILE_NAME);
            out.write_all(&encoded_filename)?;
        }
        // body_unit
        out.write_u16::<LE>(0)?;
        // stub_unit
        out.write_u16::<LE>(0)?;

        // code_table
        let mut reloc_tracker = RelocationTracker::new(self);
        let mut code_table = vec![0xAA; 4]; // (initial bytes overwritten later)

        // Emit trampoline
        if let Some(main_body) = self.main_body {
            let main_body = self
                .body_fragments
                .get(&main_body)
                .expect("missing main body offset");
            let target: u32 = main_body
                .base_offset
                .try_into()
                .expect("code table is too large");
            // Account for location fixup
            let offset = target - (4 + 4);
            eprintln!("main body at {offset:x}");

            code_table.write_u32::<LE>(Opcode::JUMP(CodeOffset(0)).encoding_kind().into())?;
            code_table.write_u32::<LE>(offset)?;
        } else {
            // FIXME: This is very reachable once we lower unit modules
            unreachable!("trying to run a unit body");
        }

        // Emit bodies
        for body_code in self.body_fragments.values() {
            body_code.encode_to(&mut reloc_tracker, &mut code_table)?;
        }

        out.write_u32::<LE>(
            code_table
                .len()
                .try_into()
                .expect("code table is too large"),
        )?;
        out.write_all(&code_table)?;
        // manifest_table
        out.write_u32::<LE>(
            self.const_bytes
                .len()
                .try_into()
                .expect("manifest table is too large"),
        )?;
        out.write_all(&self.const_bytes)?;
        // globals_size
        out.write_u32::<LE>(0)?;

        // manifest_patches
        out.write_u32::<LE>(0xFFFFFFFF)?;

        // local_code_patch_head, local_manifest_patch_head, local_globals_patch_head
        for offset in &reloc_tracker.last_location {
            let starting_at: u32 = offset.unwrap_or(0).try_into().unwrap();
            out.write_u32::<LE>(starting_at)?;
        }

        // external_code_patches
        out.write_u16::<LE>(0xFFFF)?;
        // external_manifest_patches
        out.write_u16::<LE>(0xFFFF)?;
        // external_global_patches
        out.write_u16::<LE>(0xFFFF)?;

        // Write out main footer
        out.write_u16::<LE>(main_id)?;
        let mut encoded_magic = [0; 255];
        encoded_magic[..MAIN_MAGIC.len()].copy_from_slice(MAIN_MAGIC);
        out.write_all(&encoded_magic)?;

        Ok(())
    }

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
            target: RelocTarget::Offset(start_at),
        });

        RelocatableOffset(reloc_id)
    }

    fn reloc_to_code_body(
        &mut self,
        package_id: hir_package::PackageId,
        body_id: hir_body::BodyId,
    ) -> RelocatableOffset {
        let reloc_id = self.reloc_table.len();
        self.reloc_table.push(RelocInfo {
            section: RelocSection::Code,
            target: RelocTarget::Body(package_id, body_id),
        });

        RelocatableOffset(reloc_id)
    }

    fn freeze_body_offsets(&mut self) {
        // Initial 4 bytes are for the `CALLIMPLEMENTBY` opcode
        // Next bytes are for the trampoline to the main body
        let mut next_offset = 4 + Opcode::JUMP(CodeOffset(0)).size();

        for (_, body) in &mut self.body_fragments {
            body.base_offset = next_offset;
            next_offset += body.size();
        }
    }
}

struct RelocInfo {
    section: RelocSection,
    target: RelocTarget,
}

enum RelocTarget {
    Offset(usize),
    Body(hir_package::PackageId, hir_body::BodyId),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RelocSection {
    Code,
    Manifest,
    _Global,
}

/// Generates code from the given HIR database,
/// or producing nothing if an error was encountered before code generation.
pub fn generate_code(db: &dyn CodeGenDB) -> CompileResult<Option<CodeBlob>> {
    // Bail if there are any errors from analysis
    let res = db.analyze_packages();

    if res.messages().has_errors() {
        return res.map(|_| None);
    }

    // Start producing blobs for each package
    // Only deal with one package right now
    let pkg_graph = toc_source_graph::source_graph(db.up()).as_ref().unwrap();
    let mut blob = CodeBlob::default();

    if let Some(&package_id) = pkg_graph.all_packages(db.up()).first() {
        let root_file = package_id.root(db.up());

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

        blob.main_body = Some((package_id.into(), main_body));

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
                        &mut blob,
                        package_id.into(),
                        package.as_ref(),
                        body_id,
                        params,
                        *ret_param,
                    );

                    println!("gen code:");
                    for (idx, opc) in body_code.fragment.opcodes.iter().enumerate() {
                        println!("{idx:4}    {opc:?}");
                    }
                    println!("\noffsets:");
                    for (idx, off) in body_code.fragment.code_offsets.iter().enumerate() {
                        println!("{idx:4}    {off:?}");
                    }
                    println!("\nlocals ({} bytes):", body_code.fragment.locals_size);
                    for (idx, (def_id, target)) in body_code.fragment.locals.iter().enumerate() {
                        println!("{idx:4}    ({def_id:?})> {target:?}");
                    }
                    println!("\ntemporaries ({} bytes):", body_code.fragment.temps_size);
                    for (idx, off) in body_code.fragment.temps.iter().enumerate() {
                        println!("{idx:4}    {off:?}");
                    }

                    blob.body_fragments
                        .insert((package_id.into(), body_id), body_code);
                }
                hir_body::BodyKind::Exprs(_expr) => {
                    // We don't start code generation from expr bodies
                    // (those are associated with `const` and `var`s)
                    continue;
                }
            }
        }
    } else {
        unreachable!()
    }

    blob.freeze_body_offsets();

    // ???: How do we figure out which file will serve as the main body?
    // For now, we only deal with one file, so that will always serve as the main body
    // TODO: toposort calls to module initialization bodies

    res.map(|_| Some(blob))
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

struct RelocationTracker<'a> {
    last_location: [Option<usize>; 3],
    code_blob: &'a CodeBlob,
}

impl<'a> RelocationTracker<'a> {
    fn new(code_blob: &'a CodeBlob) -> Self {
        Self {
            last_location: [None; 3],
            code_blob,
        }
    }

    fn add_reloc(&mut self, relative_to: usize, reloc_offset: RelocatableOffset) -> (u32, u32) {
        let reloc_info = &self.code_blob.reloc_table[reloc_offset.0];
        let offset = match reloc_info.target {
            RelocTarget::Offset(offset) => offset,
            RelocTarget::Body(package_id, body_id) => self
                .code_blob
                .body_fragments
                .get(&(package_id, body_id))
                .map(|body| body.base_offset)
                .expect("missing body code"),
        };

        // Point to the 1st operand (patch_link, skipping opcode)
        let last_location =
            self.last_location[reloc_info.section as usize].replace(relative_to + 4);
        let patch_jump = last_location.unwrap_or(0);

        (patch_jump as u32, offset as u32)
    }
}

struct OffsetTable {
    instruction_offsets: Vec<u32>,
}

impl OffsetTable {
    fn build_table(code_fragment: &CodeFragment, base_offset: usize) -> Self {
        let mut instruction_pointer = base_offset.try_into().expect("code table is too big");
        let offsets: Vec<_> = code_fragment
            .opcodes
            .iter()
            .map(move |op| {
                let at = instruction_pointer;
                instruction_pointer += op.size() as u32;
                at
            })
            .collect();

        Self {
            instruction_offsets: offsets,
        }
    }

    fn relative_address(&self, at: usize) -> usize {
        self.instruction_offsets[at] as usize
    }

    fn backward_offset(&self, from: usize, to: usize) -> usize {
        // account for already adjusted pc
        let from_offset = self.instruction_offsets[from] + 4;
        let to_offset = self.instruction_offsets[to];

        let offset = from_offset
            .checked_sub(to_offset)
            .expect("forward branch computed with backward offset");

        offset.try_into().unwrap()
    }

    fn forward_offset(&self, from: usize, to: usize) -> usize {
        // account for already adjusted pc
        let from_offset = self.instruction_offsets[from] + 4;
        let to_offset = self.instruction_offsets[to];

        let offset = to_offset
            .checked_sub(from_offset)
            .expect("backward branch computed with forward offset");

        offset.try_into().unwrap()
    }
}

#[derive(Default)]
struct BodyCode {
    fragment: CodeFragment,
    base_offset: usize,
}

impl BodyCode {
    /// Gets the size of the body code, in bytes
    fn size(&self) -> usize {
        self.fragment.opcodes.iter().map(Opcode::size).sum()
    }

    fn encode_to(
        &self,
        reloc_tracker: &mut RelocationTracker,
        out: &mut impl io::Write,
    ) -> io::Result<()> {
        // Build table for resolving code offsets
        let offset_table = OffsetTable::build_table(&self.fragment, self.base_offset);

        for (pc, op) in self.fragment.opcodes.iter().enumerate() {
            self.write_opcode(pc, *op, out, reloc_tracker, &offset_table)?;
        }

        Ok(())
    }

    fn write_opcode(
        &self,
        pc: usize,
        opcode: Opcode,
        out: &mut impl io::Write,
        reloc_tracker: &mut RelocationTracker<'_>,
        offset_table: &OffsetTable,
    ) -> io::Result<()> {
        // Emit opcode
        out.write_u32::<LE>(opcode.encoding_kind().into())?;

        // Emit operands
        match opcode {
            Opcode::ABORT(kind) | Opcode::ABORTCOND(kind) => {
                out.write_u32::<LE>(kind.encoding_kind().into())?;
            }
            Opcode::ABSINT() => {}
            Opcode::ABSREAL() => {}
            Opcode::ADDINT() => {}
            Opcode::ADDINTNAT() => {}
            Opcode::ADDNAT() => {}
            Opcode::ADDNATINT() => {}
            Opcode::ADDREAL() => {}
            Opcode::ADDSET(_) => todo!(),
            Opcode::ALLOCFLEXARRAY() => {}
            Opcode::ALLOCGLOB() => {}
            Opcode::ALLOCGLOBARRAY() => {}
            Opcode::ALLOCLOC() => {}
            Opcode::ALLOCLOCARRAY() => {}
            Opcode::AND() => {}
            Opcode::ARRAYUPPER(_) => todo!(),
            Opcode::ASNADDR() => {}
            Opcode::ASNINT() => {}
            Opcode::ASNINT1() => {}
            Opcode::ASNINT2() => {}
            Opcode::ASNINT4() => {}
            Opcode::ASNNAT() => {}
            Opcode::ASNNAT1() => {}
            Opcode::ASNNAT2() => {}
            Opcode::ASNNAT4() => {}
            Opcode::ASNPTR() => {}
            Opcode::ASNREAL() => {}
            Opcode::ASNREAL4() => {}
            Opcode::ASNREAL8() => {}
            Opcode::ASNADDRINV() => {}
            Opcode::ASNINTINV() => {}
            Opcode::ASNINT1INV() => {}
            Opcode::ASNINT2INV() => {}
            Opcode::ASNINT4INV() => {}
            Opcode::ASNNATINV() => {}
            Opcode::ASNNAT1INV() => {}
            Opcode::ASNNAT2INV() => {}
            Opcode::ASNNAT4INV() => {}
            Opcode::ASNPTRINV() => {}
            Opcode::ASNREALINV() => {}
            Opcode::ASNREAL4INV() => {}
            Opcode::ASNREAL8INV() => {}
            Opcode::ASNNONSCALAR(len) | Opcode::ASNNONSCALARINV(len) => {
                out.write_u32::<LE>(len)?;
            }
            Opcode::ASNSTR() => {}
            Opcode::ASNSTRINV() => {}
            Opcode::BEGINHANDLER(_, _) => todo!(),
            Opcode::BITSASSIGN(_, _, _) => todo!(),
            Opcode::BITSEXTRACT(_, _) => todo!(),
            Opcode::CALL(offset) => {
                out.write_u32::<LE>(offset)?;
            }
            Opcode::CALLEXTERNAL(_) => todo!(),
            Opcode::CALLIMPLEMENTBY(_) => todo!(),
            Opcode::CASE(_) => todo!(),
            Opcode::CAT() => {}
            Opcode::CHARSUBSTR1(_) => todo!(),
            Opcode::CHARSUBSTR2(_) => todo!(),
            Opcode::CHARTOCSTR() => {}
            Opcode::CHARTOSTR() => {}
            Opcode::CHARTOSTRLEFT() => {}
            Opcode::CHKCHRSTRSIZE(len) | Opcode::CHKCSTRRANGE(len) => {
                out.write_u32::<LE>(len)?;
            }
            Opcode::CHKRANGE(peek_at, min, max, kind) => {
                out.write_u32::<LE>(peek_at)?;
                out.write_i32::<LE>(min)?;
                out.write_i32::<LE>(max)?;
                out.write_u32::<LE>(kind.encoding_kind().into())?;
            }
            Opcode::CHKSTRRANGE(len) => {
                out.write_u32::<LE>(len.into())?;
            }
            Opcode::CHKSTRSIZE(len) => {
                out.write_u32::<LE>(len)?;
            }
            Opcode::CLOSE() => {}
            Opcode::COPYARRAYDESC() => {}
            Opcode::CSTRTOCHAR() => {}
            Opcode::CSTRTOSTR() => {}
            Opcode::CSTRTOSTRLEFT() => {}
            Opcode::DEALLOCFLEXARRAY() => {}
            Opcode::DECSP(_) => todo!(),
            Opcode::DIVINT() => {}
            Opcode::DIVNAT() => {}
            Opcode::DIVREAL() => {}
            Opcode::EMPTY() => {}
            Opcode::ENDFOR(back_to) => {
                let offset = self.resolve_backward_target(pc, back_to, offset_table);
                out.write_u32::<LE>(offset)?;
            }
            Opcode::EOF() => {}
            Opcode::EQADDR() => {}
            Opcode::EQCHARN(_) => todo!(),
            Opcode::EQINT() => {}
            Opcode::EQINTNAT() => {}
            Opcode::EQNAT() => {}
            Opcode::EQREAL() => {}
            Opcode::EQSTR() => {}
            Opcode::EQSET(_) => todo!(),
            Opcode::EXPINTINT() => {}
            Opcode::EXPREALINT() => {}
            Opcode::EXPREALREAL() => {}
            Opcode::FETCHADDR() => {}
            Opcode::FETCHBOOL() => {}
            Opcode::FETCHINT() => {}
            Opcode::FETCHINT1() => {}
            Opcode::FETCHINT2() => {}
            Opcode::FETCHINT4() => {}
            Opcode::FETCHNAT() => {}
            Opcode::FETCHNAT1() => {}
            Opcode::FETCHNAT2() => {}
            Opcode::FETCHNAT4() => {}
            Opcode::FETCHPTR() => {}
            Opcode::FETCHREAL() => {}
            Opcode::FETCHREAL4() => {}
            Opcode::FETCHREAL8() => {}
            Opcode::FETCHSET(_) => todo!(),
            Opcode::FETCHSTR() => {}
            Opcode::FIELD(_) => todo!(),
            Opcode::FOR(skip_to) => {
                let offset = self.resolve_forward_target(pc, skip_to, offset_table);
                out.write_u32::<LE>(offset)?;
            }
            Opcode::FORK(_, _) => todo!(),
            Opcode::FREE(_) => todo!(),
            Opcode::FREECLASS(_) => todo!(),
            Opcode::FREEU() => {}
            Opcode::GECLASS() => {}
            Opcode::GECHARN(_) => todo!(),
            Opcode::GEINT() => {}
            Opcode::GEINTNAT() => {}
            Opcode::GENAT() => {}
            Opcode::GENATINT() => {}
            Opcode::GEREAL() => {}
            Opcode::GESTR() => {}
            Opcode::GESET(_) => todo!(),
            Opcode::GET(kind) => {
                out.write_u32::<LE>(kind.encoding_kind().into())?;

                match kind {
                    GetKind::Skip() => {}
                    GetKind::Boolean() | GetKind::Char() => {
                        out.write_u32::<LE>(1)?;
                    }
                    GetKind::CharRange(min, max) => {
                        out.write_u32::<LE>(1)?;
                        out.write_i32::<LE>(min)?;
                        out.write_i32::<LE>(max)?;
                    }
                    GetKind::CharN(size)
                    | GetKind::Enum(size)
                    | GetKind::Int(size)
                    | GetKind::Nat(size)
                    | GetKind::Real(size)
                    | GetKind::StringExact(size)
                    | GetKind::StringLine(size)
                    | GetKind::StringToken(size) => {
                        out.write_u32::<LE>(size)?;
                    }
                    GetKind::EnumRange(size, min, max) | GetKind::IntRange(size, min, max) => {
                        out.write_u32::<LE>(size)?;
                        out.write_i32::<LE>(min)?;
                        out.write_i32::<LE>(max)?;
                    }
                }
            }
            Opcode::GETPRIORITY() => {}
            Opcode::GTCLASS() => {}
            Opcode::IN(_, _, _) => todo!(),
            Opcode::INCLINENO() => {}
            Opcode::INCSP(amount) => {
                out.write_u32::<LE>(amount)?;
            }
            Opcode::INFIXAND(_) => todo!(),
            Opcode::INITARRAYDESC() => {}
            Opcode::INITCONDITION(_) => todo!(),
            Opcode::INITMONITOR(_) => todo!(),
            Opcode::INITUNIT(_, _, _) => todo!(),
            Opcode::INTREAL() => {}
            Opcode::INTREALLEFT() => {}
            Opcode::INTSTR() => {}
            Opcode::JSR(_) => todo!(),
            Opcode::IF(skip_to) | Opcode::INFIXOR(skip_to) | Opcode::JUMP(skip_to) => {
                let offset = self.resolve_forward_target(pc, skip_to, offset_table);
                out.write_u32::<LE>(offset)?;
            }
            Opcode::JUMPB(back_to) => {
                let offset = self.resolve_backward_target(pc, back_to, offset_table);
                out.write_u32::<LE>(offset)?;
            }
            Opcode::LECLASS() => {}
            Opcode::LECHARN(_) => todo!(),
            Opcode::LEINT() => {}
            Opcode::LEINTNAT() => {}
            Opcode::LENAT() => {}
            Opcode::LENATINT() => {}
            Opcode::LEREAL() => {}
            Opcode::LESTR() => {}
            Opcode::LESET(_) => todo!(),
            Opcode::LOCATEARG(_) => todo!(),
            Opcode::LOCATECLASS(_) => todo!(),
            Opcode::LOCATELOCAL(offset) => {
                out.write_u32::<LE>(offset)?;
            }
            Opcode::LOCATEPARM(offset) => {
                out.write_u32::<LE>(offset)?;
            }
            Opcode::LOCATETEMP(temp_slot) => {
                let slot_info = self.fragment.temps.get(temp_slot.0).unwrap();

                // locals_area
                out.write_u32::<LE>(self.fragment.frame_size())?;
                // temporary
                out.write_u32::<LE>(slot_info.offset)?;
            }
            Opcode::LTCLASS() => {}
            Opcode::MAXINT() => {}
            Opcode::MAXNAT() => {}
            Opcode::MAXREAL() => {}
            Opcode::MININT() => {}
            Opcode::MINNAT() => {}
            Opcode::MINREAL() => {}
            Opcode::MODINT() => {}
            Opcode::MODNAT() => {}
            Opcode::MODREAL() => {}
            Opcode::MONITORENTER() => {}
            Opcode::MONITOREXIT() => {}
            Opcode::MULINT() => {}
            Opcode::MULNAT() => {}
            Opcode::MULREAL() => {}
            Opcode::MULSET(_) => todo!(),
            Opcode::NATREAL() => {}
            Opcode::NATREALLEFT() => {}
            Opcode::NATSTR() => {}
            Opcode::NEGINT() => {}
            Opcode::NEGREAL() => {}
            Opcode::NEW() => {}
            Opcode::NEWARRAY() => {}
            Opcode::NEWCLASS() => {}
            Opcode::NEWU() => {}
            Opcode::NOT() => {}
            Opcode::NUMARRAYELEMENTS() => {}
            Opcode::OBJCLASS() => {}
            Opcode::OPEN(_, _) => todo!(),
            Opcode::OR() => {}
            Opcode::ORD() => {}
            Opcode::PAUSE() => {}
            Opcode::PRED() => {}
            Opcode::PROC(frame_size) => {
                out.write_u32::<LE>(frame_size)?;
            }
            Opcode::PUSHADDR(_) => todo!(),
            Opcode::PUSHADDR1(reloc_offset) => {
                let relative_to = offset_table.relative_address(pc);
                let (patch_link, offset) = reloc_tracker.add_reloc(relative_to, reloc_offset);

                out.write_u32::<LE>(patch_link)?;
                out.write_u32::<LE>(offset)?;
            }
            Opcode::PUSHCOPY() => {}
            Opcode::PUSHINT(value) => {
                out.write_u32::<LE>(value)?;
            }
            Opcode::PUSHINT1(value) => {
                out.write_u32::<LE>(value.into())?;
            }
            Opcode::PUSHINT2(value) => {
                out.write_u32::<LE>(value.into())?;
            }
            Opcode::PUSHREAL(value) => {
                out.write_f64::<LE>(value)?;
            }
            Opcode::PUSHVAL0() => {}
            Opcode::PUSHVAL1() => {}
            Opcode::PUT(kind) => {
                out.write_u32::<LE>(kind.encoding_kind().into())?;
            }
            Opcode::QUIT() => {}
            Opcode::READ() => {}
            Opcode::REALDIVIDE() => {}
            Opcode::REMINT() => {}
            Opcode::REMREAL() => {}
            Opcode::RESOLVEDEF(_) => todo!(),
            Opcode::RESOLVEPTR() => {}
            Opcode::RESTORESP() => {}
            Opcode::RETURN() => {}
            Opcode::RTS() => {}
            Opcode::SAVESP() => {}
            Opcode::SEEK() => {}
            Opcode::SEEKSTAR() => {}
            Opcode::SETALL(_, _) => todo!(),
            Opcode::SETCLR(_) => todo!(),
            Opcode::SETELEMENT(_, _, _) => todo!(),
            Opcode::SETFILENO(file_no, line_no) => {
                out.write_u32::<LE>(file_no.into())?;
                out.write_u32::<LE>(line_no.into())?;
            }
            Opcode::SETLINENO(line_no) => {
                out.write_u32::<LE>(line_no.into())?;
            }
            Opcode::SETPRIORITY() => {}
            Opcode::SETSTDSTREAM(stream) => {
                out.write_u32::<LE>(stream.encoding_kind().into())?;
            }
            Opcode::SETSTREAM(_) => todo!(),
            Opcode::SHL() => {}
            Opcode::SHR() => {}
            Opcode::SIGNAL(_) => todo!(),
            Opcode::STRINT() => {}
            Opcode::STRINTOK() => {}
            Opcode::STRNAT() => {}
            Opcode::STRNATOK() => {}
            Opcode::STRTOCHAR() => {}
            Opcode::SUBINT() => {}
            Opcode::SUBINTNAT() => {}
            Opcode::SUBNAT() => {}
            Opcode::SUBNATINT() => {}
            Opcode::SUBREAL() => {}
            Opcode::SUBSCRIPT() => {}
            Opcode::SUBSET(_) => todo!(),
            Opcode::SUBSTR1(_) => todo!(),
            Opcode::SUBSTR2(_) => todo!(),
            Opcode::SUCC() => {}
            Opcode::TAG(_) => todo!(),
            Opcode::TELL() => {}
            Opcode::UFIELD(_, _, _) => todo!(),
            Opcode::UNINIT() => {}
            Opcode::UNINITADDR() => {}
            Opcode::UNINITBOOLEAN() => {}
            Opcode::UNINITINT() => {}
            Opcode::UNINITNAT() => {}
            Opcode::UNINITREAL() => {}
            Opcode::UNINITSTR() => {}
            Opcode::UNLINKHANDLER() => {}
            Opcode::VSUBSCRIPT(_, _, _) => todo!(),
            Opcode::WAIT(_) => todo!(),
            Opcode::WRITE() => {}
            Opcode::XOR() => {}
            Opcode::XORSET(_) => {}
            Opcode::BREAK() => {}
            Opcode::SYSEXIT() => {}
            Opcode::ILLEGAL() => {}
        }

        Ok(())
    }

    fn resolve_backward_target(&self, from: usize, to: CodeOffset, offsets: &OffsetTable) -> u32 {
        let jump_to = match self.fragment.code_offsets[to.0] {
            OffsetTarget::Branch(to) => to.expect("unresolved branch"),
        };
        let offset = offsets.backward_offset(from, jump_to);

        offset.try_into().unwrap()
    }

    fn resolve_forward_target(&self, from: usize, to: CodeOffset, offsets: &OffsetTable) -> u32 {
        let jump_to = match self.fragment.code_offsets[to.0] {
            OffsetTarget::Branch(to) => to.expect("unresolved branch"),
        };
        let offset = offsets.forward_offset(from, jump_to);

        offset.try_into().unwrap()
    }
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
    code_fragment: &'a mut CodeFragment,

    code_blob: &'a mut CodeBlob,
    last_location: Option<(usize, usize)>,
    branch_stack: Vec<BlockBranches>,
}

impl BodyCodeGenerator<'_> {
    fn generate_body(
        db: &dyn CodeGenDB,
        code_blob: &mut CodeBlob,
        package_id: hir_package::PackageId,
        package: &hir_package::Package,
        body_id: hir_body::BodyId,
        param_defs: &[LocalDefId],
        ret_param: Option<LocalDefId>,
    ) -> BodyCode {
        let mut code_fragment = CodeFragment::default();

        let mut gen = BodyCodeGenerator {
            db,
            package_id,
            package,
            body_id,
            body: package.body(body_id),
            code_fragment: &mut code_fragment,

            code_blob,
            last_location: None,
            branch_stack: vec![],
        };

        // FIXME: Bind imported defs
        gen.bind_inputs(param_defs, ret_param);

        gen.code_fragment.emit_opcode(Opcode::PROC(0));

        match &package.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, ..) => gen.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }

        // Generate the appropriate footer
        {
            let item_owner = match gen
                .db
                .body_owner(InPackage(gen.package_id, gen.body_id))
                .expect("from stmt thing")
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
                gen.code_fragment
                    .emit_opcode(Opcode::ABORT(AbortSource::MissingResult()));
            } else {
                gen.code_fragment.emit_opcode(Opcode::RETURN());
            }
        }
        gen.code_fragment.fix_frame_size();

        BodyCode {
            fragment: code_fragment,
            base_offset: 0,
        }
    }

    fn inline_body(&mut self, body_id: hir_body::BodyId) {
        // hacky workarounds for actual hacks...
        let mut gen = BodyCodeGenerator {
            db: self.db,
            package_id: self.package_id,
            package: self.package,
            body_id,
            body: self.package.body(body_id),
            code_fragment: self.code_fragment,

            code_blob: self.code_blob,
            last_location: self.last_location,
            branch_stack: vec![],
        };

        match &self.package.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, ..) => gen.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }

        self.last_location = gen.last_location;
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
                    self.code_fragment.bind_argument(
                        DefId(self.package_id, local_def),
                        arg_offset,
                        true,
                    );
                }

                arg_offset += 4;
            }

            // Feed in params
            if let Some(param_infos) = params.as_ref() {
                for (local_def, param_info) in param_defs.iter().zip(param_infos) {
                    // ???: How should we deal with char(*) / string(*)?
                    let param_ty = param_info.param_ty.to_base_type(self.db.up());
                    let size_of = param_ty.size_of(self.db.up()).expect("must be sized");
                    let indirect = match param_info.pass_by {
                        ty::PassBy::Value => false,
                        ty::PassBy::Reference(_) => true,
                    };

                    self.code_fragment.bind_argument(
                        DefId(self.package_id, *local_def),
                        arg_offset,
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

        // `0` is reserved for "<No File>"
        let code_file = self.code_blob.file_map.insert_full(file).0 + 1;
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
        let span = stmt.span.lookup_in(self.package);
        self.emit_location(span);

        self.code_fragment.bump_temp_allocs();
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
        self.code_fragment.unbump_temp_allocs();
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
        self.generate_assign(lhs_ty, AssignOrder::Precomputed);

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
                .type_of((self.package_id, self.body_id, item.expr).into())
                .to_base_type(self.db.up());

            let put_kind = match put_ty.kind(self.db.up()) {
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
                ty::TypeKind::CharN(_) => PutKind::CharN(),
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

                self.code_fragment.emit_opcode(Opcode::PUSHINT(length));
            }

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
        let stream_handle =
            self.generate_set_stream(stmt.stream_num, None, StdStream::Stdin(), StreamKind::Get());

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
                .type_of((self.package_id, self.body_id, item.expr).into())
                .to_base_type(self.db.up());
            let ty_size = get_ty.size_of(self.db.up()).expect("type must be concrete") as u32;

            let mut get_width = None;
            let get_kind = match get_ty.kind(self.db.up()) {
                ty::TypeKind::Boolean => GetKind::Boolean(),
                ty::TypeKind::Int(_) => GetKind::Int(ty_size),
                ty::TypeKind::Nat(_) => GetKind::Nat(ty_size),
                ty::TypeKind::Real(_) => GetKind::Real(ty_size),
                ty::TypeKind::Integer => unreachable!(),
                ty::TypeKind::Char => GetKind::Char(),
                ty::TypeKind::StringN(_) | ty::TypeKind::String => match item.width {
                    hir_stmt::GetWidth::Token => GetKind::StringToken(ty_size),
                    hir_stmt::GetWidth::Line => GetKind::StringLine(ty_size),
                    hir_stmt::GetWidth::Chars(width) => {
                        get_width = Some(width);
                        GetKind::StringExact(ty_size)
                    }
                },
                ty::TypeKind::CharN(_) => match item.width {
                    hir_stmt::GetWidth::Chars(width) => {
                        get_width = Some(width);
                        GetKind::CharN(ty_size)
                    }
                    hir_stmt::GetWidth::Token => GetKind::CharN(ty_size),
                    hir_stmt::GetWidth::Line => unreachable!(),
                },
                _ => unreachable!(),
            };

            // Put reference onto the stack
            self.generate_ref_expr(item.expr);

            if matches!(
                get_kind,
                GetKind::StringToken(_)
                    | GetKind::StringLine(_)
                    | GetKind::StringExact(_)
                    | GetKind::CharN(_)
            ) {
                // push max width
                if let Some(width) = get_width {
                    self.generate_expr(width);
                }

                // and max length
                let max_len = get_ty.length_of(self.db.up()).expect("is a charseq") as u32;
                self.code_fragment.emit_opcode(Opcode::PUSHINT(max_len));
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
            let local = DefId(self.package_id, counter_def);
            self.code_fragment
                .allocate_local_space(local, descriptor_size);
            ForDescriptorSlot::Local(DefId(self.package_id, counter_def))
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
            self.code_fragment
                .emit_opcode(Opcode::CHKRANGE(0, 0, i32::MAX, CheckKind::LoopStep()));
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
        if let Some(false_branch) = stmt.false_branch.stmt() {
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
            .type_of((self.package_id, self.body_id, stmt.discriminant).into())
            .to_base_type(self.db.up());

        let coerce_to = match discrim_ty.kind(self.db.up()) {
            ty::TypeKind::String | ty::TypeKind::StringN(_) => Some(CoerceTo::String),
            ty::TypeKind::Char => Some(CoerceTo::Char),
            _ => None,
        };

        let discrim_value = self
            .code_fragment
            .allocate_temporary_space(discrim_ty.size_of(self.db.up()).expect("is concrete"));
        self.code_fragment.emit_locate_temp(discrim_value);
        self.generate_assign(discrim_ty, AssignOrder::Postcomputed);

        let mut arm_targets = vec![];
        let mut has_default = false;

        for arm in &stmt.arms {
            let target = self.code_fragment.new_branch();
            arm_targets.push(target);

            match &arm.selectors {
                hir_stmt::CaseSelector::Exprs(exprs) => {
                    for expr in exprs {
                        let expr_ty = self
                            .db
                            .type_of((self.package_id, self.body_id, *expr).into())
                            .to_base_type(self.db.up());

                        self.generate_coerced_expr(*expr, coerce_to);
                        self.code_fragment.emit_locate_temp(discrim_value);
                        self.generate_fetch_value(discrim_ty);

                        let eq_op = if matches!(coerce_to, Some(CoerceTo::Char)) {
                            // Always keep coerced char type as an EQINT
                            Opcode::EQINT()
                        } else {
                            // Reuse normal eq selection
                            self.select_eq_op(
                                expr_ty.kind(self.db.up()),
                                discrim_ty.kind(self.db.up()),
                            )
                        };
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

    fn generate_stmt_call(&mut self, stmt: &hir_stmt::Call) {
        self.generate_call(stmt.lhs, stmt.arguments.as_ref(), true);
    }

    fn generate_stmt_return(&mut self, _stmt: &hir_stmt::Return) {
        self.code_fragment.emit_opcode(Opcode::RETURN());
    }

    fn generate_stmt_result(&mut self, stmt: &hir_stmt::Result) {
        let db = self.db;

        let ret_ty = db.type_of((self.package_id, self.body_id).into());
        let expr_ty = db.type_of((self.package_id, self.body_id, stmt.expr).into());

        // Generate return value
        self.generate_expr(stmt.expr);
        self.generate_coerced_op(ret_ty, expr_ty);

        // Perform assignment
        self.code_fragment.emit_opcode(Opcode::LOCATEPARM(0));
        self.code_fragment.emit_opcode(Opcode::FETCHADDR());
        self.generate_assign(ret_ty, AssignOrder::Postcomputed);

        self.code_fragment.emit_opcode(Opcode::RETURN());
    }

    fn generate_item(&mut self, item_id: hir_item::ItemId) {
        let item = self.package.item(item_id);
        let span = item.span.lookup_in(self.package);
        self.emit_location(span);

        match &item.kind {
            hir_item::ItemKind::ConstVar(item) => self.generate_item_constvar(item),
            hir_item::ItemKind::Binding(item) => self.generate_item_binding(item),
            hir_item::ItemKind::Type(_) => {}
            hir_item::ItemKind::Subprogram(item) => self.generate_item_subprogram(item),
            hir_item::ItemKind::Module(_) => {
                // We already generate code for module bodies as part of walking
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

        let def_ty = self
            .db
            .type_of(DefId(self.package_id, item.def_id).into())
            .to_base_type(self.db.up());
        self.code_fragment
            .allocate_local(self.db, DefId(self.package_id, item.def_id), def_ty);

        if let Some(init_body) = item.init_expr {
            eprintln!("inlining init body {init_body:?}");
            self.inline_body(init_body);

            let body_ty = self.db.type_of((self.package_id, init_body).into());
            self.generate_coerced_op(def_ty, body_ty);

            eprintln!("assigning def {init_body:?} to previously produced value");
            self.code_fragment
                .emit_locate_local(DefId(self.package_id, item.def_id));
            self.generate_assign(def_ty, AssignOrder::Postcomputed);
        } else if def_ty.has_uninit(self.db.up()) {
            eprintln!(
                "assigning def {:?} to uninit pattern for type `{}`",
                item.def_id,
                def_ty.display(self.db.up())
            );
            self.code_fragment
                .emit_locate_local(DefId(self.package_id, item.def_id));
            self.generate_assign_uninit(def_ty);
        }
    }

    fn generate_item_binding(&mut self, item: &hir_item::Binding) {
        // We only support def rebindings for now
        // For bindings to specific storage locations,
        // it should be treated as introducing a new var that is specially handled
        // (it's like an addr, though we need to pierce through indirections)

        if let Some(aliased_def) = self.db.binding_def((self.package_id, item.bind_to).into()) {
            self.code_fragment
                .alias_local(aliased_def, DefId(self.package_id, item.def_id))
        } else {
            todo!("indirection binding not handled yet");
        }
    }

    fn generate_item_subprogram(&mut self, item: &hir_item::Subprogram) {
        let reloc_body = self
            .code_blob
            .reloc_to_code_body(self.package_id, item.body.body);
        self.code_fragment
            .bind_reloc_local(DefId(self.package_id, item.def_id), reloc_body);
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

        let coerce_op =
            coerce_to.and_then(|coerce_to| match (coerce_to, expr_ty.kind(self.db.up())) {
                // To `real`
                (CoerceTo::Real, ty::TypeKind::Nat(_)) => Some(Opcode::NATREAL()),
                (CoerceTo::Real, int) if int.is_integer() => Some(Opcode::INTREAL()),

                // To `char`
                (CoerceTo::Char, ty::TypeKind::String | ty::TypeKind::StringN(_)) => {
                    Some(Opcode::STRTOCHAR())
                }
                (CoerceTo::Char, ty::TypeKind::CharN(ty::SeqSize::Fixed(_))) => {
                    // Compile-time checked to always fit
                    let len = expr_ty.length_of(self.db.up());
                    assert_eq!(len, Some(1), "never dyn or not 1");

                    // Fetch the first char
                    Some(Opcode::FETCHNAT1())
                }
                (CoerceTo::Char, ty::TypeKind::CharN(ty::SeqSize::Any)) => {
                    todo!()
                }

                // To `char(N)`
                (CoerceTo::CharN(len), ty::TypeKind::Char) => {
                    // Compile-time checked to always fit
                    assert_eq!(len, 1, "never dyn or not 1");

                    // Reserve enough space for the temporary char_n
                    // Note: This is size is rounded up to char_n's alignment size.
                    let reserve_size = len + 1;

                    let temp_str = self
                        .code_fragment
                        .allocate_temporary_space(reserve_size as usize);
                    self.code_fragment.emit_locate_temp(temp_str);

                    Some(Opcode::CHARTOCSTR())
                }
                (CoerceTo::CharN(len), ty::TypeKind::String | ty::TypeKind::StringN(_)) => {
                    // Only need to verify that rhs is of the required length
                    Some(Opcode::CHKSTRSIZE(len))
                }

                // To `string`
                (CoerceTo::String, ty::TypeKind::Char) => {
                    // Reserve enough space for a `string(1)`
                    let temp_str = self.code_fragment.allocate_temporary_space(2);
                    self.code_fragment.emit_locate_temp(temp_str);

                    Some(Opcode::CHARTOSTR())
                }
                (CoerceTo::String, ty::TypeKind::CharN(ty::SeqSize::Fixed(_))) => {
                    // Reserve enough space for the given char_n
                    let len = expr_ty.length_of(self.db.up()).expect("never dyn");

                    // Include the null terminator in the reservation size
                    // Never let it exceed the maximum length of a string
                    let reserve_size = (len + 1).min(256);

                    let temp_str = self.code_fragment.allocate_temporary_space(reserve_size);
                    self.code_fragment.emit_locate_temp(temp_str);

                    self.code_fragment.emit_opcode(Opcode::PUSHINT(len as u32));
                    Some(Opcode::CSTRTOSTR())
                }
                (CoerceTo::String, ty::TypeKind::CharN(ty::SeqSize::Any)) => {
                    todo!()
                }
                _ => None,
            });

        if let Some(opcode) = coerce_op {
            eprintln!("coercing into {coerce_op:?}");
            self.code_fragment.emit_opcode(opcode);
        }
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
            hir_expr::Literal::CharSeq(value) | hir_expr::Literal::String(value) => {
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
            .type_of((self.package_id, self.body_id, expr.lhs).into())
            .to_base_type(self.db.up());
        let rhs_ty = self
            .db
            .type_of((self.package_id, self.body_id, expr.rhs).into())
            .to_base_type(self.db.up());

        let opcode = match expr.op.item() {
            hir_expr::BinaryOp::Add => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
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
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::SUBREAL(),
                    Opcode::SUBNAT(),
                    Opcode::SUBNATINT(),
                    Opcode::SUBINTNAT(),
                    Opcode::SUBINT(),
                )
                .unwrap(),
            hir_expr::BinaryOp::Mul => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::MULREAL(),
                    Opcode::MULNAT(),
                    Opcode::MULINT(),
                    Opcode::MULINT(),
                    Opcode::MULINT(),
                )
                .expect("op over unsupported type"),
            hir_expr::BinaryOp::Div => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::DIVREAL(),
                    Opcode::DIVNAT(),
                    Opcode::DIVINT(),
                    Opcode::DIVINT(),
                    Opcode::DIVINT(),
                )
                .expect("op over unsupported type"),
            hir_expr::BinaryOp::RealDiv => {
                self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
                Opcode::REALDIVIDE()
            }
            hir_expr::BinaryOp::Mod => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::MODREAL(),
                    Opcode::MODNAT(),
                    Opcode::MODINT(),
                    Opcode::MODINT(),
                    Opcode::MODINT(),
                )
                .expect("op over unsupported type"),
            hir_expr::BinaryOp::Rem => self
                .dispatch_over_numbers(
                    expr,
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::REMREAL(),
                    Opcode::MODNAT(),
                    Opcode::MODINT(),
                    Opcode::MODINT(),
                    Opcode::MODINT(),
                )
                .expect("op over unsupported type"),
            hir_expr::BinaryOp::Exp => match (lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()))
            {
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
                    self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                    self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
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
                self.coerce_to_same(expr, lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));

                let cmp_op = if let Some(cmp_op) = self.select_over_numbers(
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::GEREAL(),
                    Opcode::GENAT(),
                    Opcode::GENATINT(),
                    Opcode::GEINTNAT(),
                    Opcode::GEINT(),
                ) {
                    cmp_op
                } else {
                    match (lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up())) {
                        (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::GENAT(),
                        (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => {
                            Opcode::GESTR()
                        }
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
                self.coerce_to_same(expr, lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));

                let cmp_op = if let Some(cmp_op) = self.select_over_numbers(
                    lhs_ty.kind(self.db.up()),
                    rhs_ty.kind(self.db.up()),
                    Opcode::LEREAL(),
                    Opcode::LENAT(),
                    Opcode::LENATINT(),
                    Opcode::LEINTNAT(),
                    Opcode::LEINT(),
                ) {
                    cmp_op
                } else {
                    match (lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up())) {
                        (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::LENAT(),
                        (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => {
                            Opcode::LESTR()
                        }
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
                self.coerce_to_same(expr, lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));
                let cmp_op =
                    self.select_eq_op(lhs_ty.kind(self.db.up()), rhs_ty.kind(self.db.up()));

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
                (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::EQINT(),
                // All other comparable charseqs are converted into string
                (lhs, rhs) if lhs.is_cmp_charseq() && rhs.is_cmp_charseq() => Opcode::EQSTR(),
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
                    self.code_fragment.emit_opcode(Opcode::PUSHINT(!0));
                    self.code_fragment.emit_opcode(Opcode::XOR());
                } else {
                    self.code_fragment.emit_opcode(Opcode::NOT())
                }
            }
            hir_expr::UnaryOp::Negate => match rhs_ty.kind(self.db.up()) {
                ty::TypeKind::Integer | ty::TypeKind::Int(_) | ty::TypeKind::Nat(_) => {
                    // should already be dealing with promoted types
                    self.code_fragment.emit_opcode(Opcode::NEGINT())
                }
                ty::TypeKind::Real(_) => self.code_fragment.emit_opcode(Opcode::NEGREAL()),
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
                    self.code_fragment
                        .emit_locate_local(DefId(self.package_id, def_id));
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

        let ret_ty_ref = ret_ty.to_base_type(db.up());
        let ret_val = if !matches!(ret_ty_ref.kind(db.up()), ty::TypeKind::Void) {
            let size_of = ret_ty_ref.size_of(db.up()).expect("must be sized");
            Some(self.code_fragment.allocate_temporary_space(size_of))
        } else {
            None
        };

        self.code_fragment.bump_temp_allocs();
        {
            // Fetch lhs first
            self.generate_ref_expr(lhs);
            // TODO: Replace with the size of the target arch's ptr
            let call_to = self.code_fragment.allocate_temporary_space(4);
            self.code_fragment.emit_locate_temp(call_to);
            self.generate_assign(lhs_ty, AssignOrder::Postcomputed);

            let mut arg_frame_size = 0;
            let mut arg_temps = vec![];

            if let Some(ret_val) = ret_val {
                // Treat return argument as a pass by ref
                let ret_arg = self
                    .code_fragment
                    .allocate_temporary_space(ret_ty_ref.size_of(db.up()).expect("must be sized"));

                self.code_fragment.emit_locate_temp(ret_val);
                self.code_fragment.emit_locate_temp(ret_arg);
                self.code_fragment.emit_opcode(Opcode::ASNADDRINV());
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
                            let nth_arg = self.code_fragment.allocate_temporary_space(size_of);
                            self.generate_expr(arg_expr);
                            self.generate_coerced_op(param_ty, arg_ty);

                            self.code_fragment.emit_locate_temp(nth_arg);
                            self.generate_assign(param_ty, AssignOrder::Postcomputed);
                            arg_temps.push((nth_arg, Some(param_ty)));
                            arg_frame_size += size_of;
                        }
                        ty::PassBy::Reference(_) => {
                            // TODO: Replace with the size of the target arch's ptr
                            let nth_arg = self.code_fragment.allocate_temporary_space(4);
                            self.generate_ref_expr(arg_expr);

                            self.code_fragment.emit_locate_temp(nth_arg);
                            self.code_fragment.emit_opcode(Opcode::ASNADDRINV());
                            arg_temps.push((nth_arg, None));
                            arg_frame_size += 4;
                        }
                    }
                }
            }

            // Emit call for later
            self.code_fragment.emit_locate_temp(call_to);
            self.code_fragment.emit_opcode(Opcode::FETCHADDR());

            // Move args into reverse order
            // ( ... arg0 arg1 arg2 -- arg2 arg1 arg0 ... )
            for (arg, ty) in arg_temps.into_iter().rev() {
                self.code_fragment.emit_locate_temp(arg);
                if let Some(ty) = ty {
                    self.generate_fetch_value(ty);
                } else {
                    self.code_fragment.emit_opcode(Opcode::FETCHADDR());
                }
            }

            // Do the call
            let arg_frame_size = arg_frame_size.try_into().unwrap();
            self.code_fragment.emit_opcode(Opcode::CALL(arg_frame_size));
            self.code_fragment
                .emit_opcode(Opcode::INCSP(arg_frame_size + 4));
        }
        self.code_fragment.unbump_temp_allocs();

        if let Some(slot_at) = ret_val.filter(|_| !drop_ret_val) {
            self.code_fragment.emit_locate_temp(slot_at);
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
        // Steps
        // - Produce pointer to referenced def (may need to perform canonical name resolution)
        //   - If it's a stack var, it should be to the locals space
        //   - If it's a global var, it should be from a relocatable patch to the globals space
        match expr {
            hir_expr::Name::Name(binding) => {
                let def_id = self.package.binding_resolve(*binding).unwrap_def();
                let info = self.package.local_def(def_id);
                eprintln!("locating value from def {info:?} ({def_id:?})");

                self.code_fragment
                    .emit_locate_local(DefId(self.package_id, def_id))
            }
            hir_expr::Name::Self_ => todo!(),
        }
    }

    fn generate_assign(&mut self, into_ty: ty::TypeId, order: AssignOrder) {
        let into_tyref = into_ty;

        let (pre, post) = match into_tyref.kind(self.db.up()) {
            ty::TypeKind::Boolean => (Opcode::ASNINT1(), Opcode::ASNINT1INV()),
            ty::TypeKind::Int(ty::IntSize::Int1) => (Opcode::ASNINT1(), Opcode::ASNINT1INV()),
            ty::TypeKind::Int(ty::IntSize::Int2) => (Opcode::ASNINT2(), Opcode::ASNINT2INV()),
            ty::TypeKind::Int(ty::IntSize::Int4) => (Opcode::ASNINT4(), Opcode::ASNINT4INV()),
            ty::TypeKind::Int(ty::IntSize::Int) => (Opcode::ASNINT(), Opcode::ASNINTINV()),
            ty::TypeKind::Nat(ty::NatSize::Nat1) => (Opcode::ASNNAT1(), Opcode::ASNNAT1INV()),
            ty::TypeKind::Nat(ty::NatSize::Nat2) => (Opcode::ASNNAT2(), Opcode::ASNNAT2INV()),
            ty::TypeKind::Nat(ty::NatSize::Nat4) => (Opcode::ASNNAT4(), Opcode::ASNNAT4INV()),
            ty::TypeKind::Nat(ty::NatSize::Nat) => (Opcode::ASNNAT(), Opcode::ASNNATINV()),
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => (Opcode::ASNADDR(), Opcode::ASNADDRINV()),
            ty::TypeKind::Real(ty::RealSize::Real4) => (Opcode::ASNREAL4(), Opcode::ASNREAL4INV()),
            ty::TypeKind::Real(ty::RealSize::Real8) => (Opcode::ASNREAL8(), Opcode::ASNREAL8INV()),
            ty::TypeKind::Real(ty::RealSize::Real) => (Opcode::ASNREAL(), Opcode::ASNREALINV()),
            ty::TypeKind::Integer => unreachable!("type should be concrete"),
            ty::TypeKind::Char => (Opcode::ASNINT1(), Opcode::ASNINT1INV()),
            ty::TypeKind::CharN(_) => {
                let storage_size = into_tyref.size_of(self.db.up()).expect("not dyn") as u32;
                (
                    Opcode::ASNNONSCALAR(storage_size),
                    Opcode::ASNNONSCALARINV(storage_size),
                )
            }
            ty::TypeKind::String | ty::TypeKind::StringN(_) => {
                // Push corresponding storage size
                let char_len = into_tyref
                    .length_of(self.db.up())
                    .expect("should not be dyn");
                self.code_fragment
                    .emit_opcode(Opcode::PUSHINT(char_len.try_into().expect("not a u32")));
                (Opcode::ASNSTR(), Opcode::ASNSTRINV())
            }
            ty::TypeKind::Opaque(_, ty) => return self.generate_assign(*ty, order), // defer to the opaque type
            ty::TypeKind::Constrained(..) => unimplemented!(),
            ty::TypeKind::Array(..) => unimplemented!(),
            ty::TypeKind::Enum(..) => unimplemented!(),
            ty::TypeKind::Set(..) => unimplemented!(),
            ty::TypeKind::Pointer(..) => unimplemented!(),
            ty::TypeKind::Subprogram(..) => (Opcode::ASNADDR(), Opcode::ASNADDRINV()),
            ty::TypeKind::Error
            | ty::TypeKind::Forward
            | ty::TypeKind::Alias(_, _)
            | ty::TypeKind::Void => {
                unreachable!()
            }
        };

        let opcode = match order {
            AssignOrder::Precomputed => pre,
            AssignOrder::Postcomputed => post,
        };

        self.code_fragment.emit_opcode(opcode);
    }

    // Expects a destination address to be present
    fn generate_assign_uninit(&mut self, uninit_ty: ty::TypeId) {
        let uninit_ty = uninit_ty;

        let opcode = match uninit_ty.kind(self.db.up()) {
            ty::TypeKind::Nat(ty::NatSize::AddressInt) => Opcode::UNINITADDR(),
            ty::TypeKind::Boolean => Opcode::UNINITBOOLEAN(),
            ty::TypeKind::Int(ty::IntSize::Int) => Opcode::UNINITINT(),
            ty::TypeKind::Nat(ty::NatSize::Nat) => Opcode::UNINITNAT(),
            ty::TypeKind::Real(ty::RealSize::Real) => Opcode::UNINITREAL(),
            ty::TypeKind::String => Opcode::UNINITSTR(),
            _ => unreachable!(),
        };

        self.code_fragment.emit_opcode(opcode);
    }

    fn generate_fetch_value(&mut self, fetch_ty: ty::TypeId) {
        let opcode = match self.pick_fetch_op(fetch_ty) {
            Some(value) => value,
            None => return,
        };

        self.code_fragment.emit_opcode(opcode);
    }

    fn pick_fetch_op(&mut self, fetch_ty: ty::TypeId) -> Option<Opcode> {
        let fetch_ty = fetch_ty;
        let opcode = match fetch_ty.kind(self.db.up()) {
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
            ty::TypeKind::CharN(_) => return None, // don't need to dereference the pointer to storage
            ty::TypeKind::Opaque(_, ty) => return self.pick_fetch_op(*ty), // defer to the opaque type
            ty::TypeKind::Constrained(..) => unimplemented!(),
            ty::TypeKind::Array(..) => unimplemented!(),
            ty::TypeKind::Enum(..) => unimplemented!(),
            ty::TypeKind::Set(..) => unimplemented!(),
            ty::TypeKind::Pointer(..) => unimplemented!(),
            ty::TypeKind::Subprogram(..) => Opcode::FETCHADDR(),
            ty::TypeKind::Error
            | ty::TypeKind::Forward
            | ty::TypeKind::Alias(_, _)
            | ty::TypeKind::Void => {
                unreachable!()
            }
        };

        Some(opcode)
    }
}

#[derive(Clone, Copy)]
enum CoerceTo {
    Real,
    Char,
    CharN(u32),
    String,
}

#[derive(Debug, Clone, Copy)]
enum LocalTarget {
    Stack(StackSlot),
    Reloc(RelocatableOffset),
    Arg(usize, bool),
}

#[derive(Debug, Clone, Copy)]
struct StackSlot {
    offset: u32,
    size: u32,
}

#[derive(Debug)]
enum OffsetTarget {
    Branch(Option<usize>),
}

#[derive(Default)]
struct CodeFragment {
    locals: indexmap::IndexMap<DefId, LocalTarget>,
    locals_size: u32,
    temps: Vec<StackSlot>,
    temp_bumps: Vec<u32>,
    temps_current_size: u32,
    temps_size: u32,

    opcodes: Vec<Opcode>,
    code_offsets: Vec<OffsetTarget>,
}

impl CodeFragment {
    fn bind_reloc_local(&mut self, from: DefId, to: RelocatableOffset) {
        self.locals.insert(from, LocalTarget::Reloc(to));
    }

    fn bind_argument(&mut self, def_id: DefId, offset: usize, indirect: bool) {
        self.locals
            .insert(def_id, LocalTarget::Arg(offset, indirect));
    }

    fn alias_local(&mut self, source: DefId, target: DefId) {
        let slot_info = self
            .locals
            .get(&source)
            .copied()
            .expect("def not reserved yet");
        self.locals.insert(target, slot_info);
    }

    fn allocate_local(&mut self, db: &dyn CodeGenDB, def_id: DefId, def_ty: ty::TypeId) {
        self.allocate_local_space(
            def_id,
            def_ty
                .to_base_type(db.up())
                .size_of(db.up())
                .expect("is concrete"),
        );
    }

    fn allocate_local_space(&mut self, def_id: DefId, size: usize) {
        // Align to the nearest stack slot
        let size = ty::align_up_to(size, 4).try_into().unwrap();
        let offset = self.locals_size;

        self.locals
            .insert(def_id, LocalTarget::Stack(StackSlot { offset, size }));
        self.locals_size += size;
    }

    fn allocate_temporary_space(&mut self, size: usize) -> TemporarySlot {
        // Align to the nearest stack slot
        let size = ty::align_up_to(size, 4).try_into().unwrap();
        let offset = self.temps_current_size;
        let handle = self.temps.len();

        self.temps.push(StackSlot { offset, size });

        self.temps_current_size += size;
        self.temps_size = self.temps_size.max(self.temps_current_size);
        TemporarySlot(handle)
    }

    fn bump_temp_allocs(&mut self) {
        self.temp_bumps.push(self.temps_current_size);
    }

    fn unbump_temp_allocs(&mut self) {
        self.temps_current_size = self.temp_bumps.pop().expect("too many unbumps");
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

        match slot_info {
            LocalTarget::Stack(slot_info) => {
                let offset = slot_info.offset + slot_info.size;
                self.emit_opcode(Opcode::LOCATELOCAL(offset));
            }
            LocalTarget::Reloc(offset) => {
                let offset = *offset; // for dealing with borrowck warning
                self.emit_opcode(Opcode::PUSHADDR1(offset));
            }
            LocalTarget::Arg(offset, indirect) => {
                // for dealing with borrowck warnings
                let offset = *offset;
                let indirect = *indirect;

                self.emit_opcode(Opcode::LOCATEPARM(offset.try_into().unwrap()));
                if indirect {
                    self.emit_opcode(Opcode::FETCHADDR());
                }
            }
        }
    }

    fn emit_locate_temp(&mut self, temp: TemporarySlot) {
        // Don't know the final size of temporaries yet, so use temporary slots.
        self.emit_opcode(Opcode::LOCATETEMP(temp));
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

    fn fix_frame_size(&mut self) {
        let frame_size = self.frame_size();
        let first_opcode = self.opcodes.first_mut();
        assert!(matches!(first_opcode, Some(Opcode::PROC(0))));
        let first_opcode = first_opcode.unwrap();

        *first_opcode = Opcode::PROC(frame_size);
    }

    fn frame_size(&self) -> u32 {
        self.locals_size + self.temps_size
    }
}
