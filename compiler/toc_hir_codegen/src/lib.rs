//! Code generation backend based on the HIR tree

use std::collections::HashMap;
use std::io;

use byteorder::{LittleEndian as LE, WriteBytesExt};
use indexmap::IndexSet;
use instruction::{CheckKind, RelocatableOffset};
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

    body_fragments: HashMap<(hir_library::LibraryId, hir_body::BodyId), BodyCode>,
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
        assert!(self.file_map.len() < (u16::MAX - 2).into());

        // Emit "<No File>" (only for fprolog, should work for TProlog & tulip-vm)
        {
            // file_id
            out.write_u16::<LE>(0)?;
            // filename
            {
                const MAIN_FILE_NAME: &[u8] = b"<No File>";
                let mut encoded_filename = [0; 255];
                encoded_filename[..MAIN_FILE_NAME.len()].copy_from_slice(MAIN_FILE_NAME);
                out.write_all(&encoded_filename)?;
            }
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

        for (idx, file_id) in self.file_map.iter().enumerate() {
            // Guaranteed to have less than u16::MAX files per the assert above
            let blob_id = u16::try_from(idx).unwrap() + 1;

            // file_id
            out.write_u16::<LE>(blob_id)?;
            // filename
            let filename = db.file_path(*file_id);
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
        // Guaranteed to have less than u16::MAX files per the assert above
        let main_id = u16::try_from(self.file_map.len()).unwrap() + 1;

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

        let main_blob = self.body_fragments.iter().next().unwrap().1;
        main_blob.encode_to(&mut reloc_tracker, &mut code_table)?;

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
        out.write_all(MAIN_MAGIC)?;

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
    Code,
    Manifest,
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

                    blob.body_fragments.insert((library_id, body_id), body_code);
                }
                hir_body::BodyKind::Exprs(_expr) => {
                    // We don't start code generation from expr bodies
                    // (those are associated with `const` and `var`s)
                    continue;
                }
            }
        }
    }

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
        let offset = reloc_info.offset;

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
    fn build_table(code_fragment: &CodeFragment) -> Self {
        let mut instruction_pointer = 0x04;
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
        todo!()
    }

    fn forward_offset(&self, from: usize, to: usize) -> usize {
        todo!()
    }
}

#[derive(Default)]
struct BodyCode(CodeFragment);

impl BodyCode {
    fn encode_to(
        &self,
        reloc_tracker: &mut RelocationTracker,
        out: &mut impl io::Write,
    ) -> io::Result<()> {
        // Build table for resolving code offsets
        let offset_table = OffsetTable::build_table(&self.0);

        for (pc, op) in self.0.opcodes.iter().enumerate() {
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
            Opcode::ABORT(_) => todo!(),
            Opcode::ABORTCOND(_) => todo!(),
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
            Opcode::ASNNONSCALAR(_) => todo!(),
            Opcode::ASNNONSCALARINV(_) => todo!(),
            Opcode::ASNSTR() => {}
            Opcode::ASNSTRINV() => {}
            Opcode::BEGINHANDLER(_, _) => todo!(),
            Opcode::BITSASSIGN(_, _, _) => todo!(),
            Opcode::BITSEXTRACT(_, _) => todo!(),
            Opcode::CALL(_) => todo!(),
            Opcode::CALLEXTERNAL(_) => todo!(),
            Opcode::CALLIMPLEMENTBY(_) => todo!(),
            Opcode::CASE(_) => todo!(),
            Opcode::CAT() => {}
            Opcode::CHARSUBSTR1(_) => todo!(),
            Opcode::CHARSUBSTR2(_) => todo!(),
            Opcode::CHARTOCSTR() => {}
            Opcode::CHARTOSTR() => {}
            Opcode::CHARTOSTRLEFT() => {}
            Opcode::CHKCHRSTRSIZE(_) => todo!(),
            Opcode::CHKCSTRRANGE(_) => todo!(),
            Opcode::CHKRANGE(_, _, _, _) => todo!(),
            Opcode::CHKSTRRANGE(_) => todo!(),
            Opcode::CHKSTRSIZE(_) => todo!(),
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
            Opcode::ENDFOR(_) => todo!(),
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
            Opcode::FOR(_) => todo!(),
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
            Opcode::GET(_) => todo!(),
            Opcode::GETPRIORITY() => {}
            Opcode::GTCLASS() => {}
            Opcode::IF(_) => todo!(),
            Opcode::IN(_, _, _) => todo!(),
            Opcode::INCLINENO() => {}
            Opcode::INCSP(_) => todo!(),
            Opcode::INFIXAND(_) => todo!(),
            Opcode::INFIXOR(_) => todo!(),
            Opcode::INITARRAYDESC() => {}
            Opcode::INITCONDITION(_) => todo!(),
            Opcode::INITMONITOR(_) => todo!(),
            Opcode::INITUNIT(_, _, _) => todo!(),
            Opcode::INTREAL() => {}
            Opcode::INTREALLEFT() => {}
            Opcode::INTSTR() => {}
            Opcode::JSR(_) => todo!(),
            Opcode::JUMP(_) => todo!(),
            Opcode::JUMPB(_) => todo!(),
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
            Opcode::LOCATELOCAL(_) => todo!(),
            Opcode::LOCATEPARM(_) => todo!(),
            Opcode::LOCATETEMP(temp_slot) => {
                let slot_info = self.0.temps.get(temp_slot.0).unwrap();

                // locals_area
                out.write_u32::<LE>(self.0.frame_size())?;
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
            Opcode::PUSHINT(_) => todo!(),
            Opcode::PUSHINT1(_) => todo!(),
            Opcode::PUSHINT2(_) => todo!(),
            Opcode::PUSHREAL(_) => todo!(),
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
}

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

        gen.code_fragment.emit_opcode(Opcode::PROC(0));

        match &library.body(body_id).kind {
            hir_body::BodyKind::Stmts(stmts, _) => gen.generate_stmt_list(stmts),
            hir_body::BodyKind::Exprs(expr) => gen.generate_expr(*expr),
        }

        gen.code_fragment.emit_opcode(Opcode::RETURN());
        gen.code_fragment.fix_frame_size();

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
        let rhs_ty = self
            .db
            .type_of((self.library_id, self.body_id, stmt.rhs).into())
            .in_db(self.db)
            .peel_ref();

        // Evaluation order is important, side effects from the rhs are visible when looking at lhs
        // For assignment, we don't want side effects from rhs eval to be visible during lookup
        self.generate_ref_expr(stmt.lhs);
        self.generate_expr(stmt.rhs);
        self.generate_coerced_assignment(lhs_ty.id(), rhs_ty.id());

        self.code_fragment.emit_assign(&lhs_ty, self.db);

        eprintln!("assigning reference (first operand) to value (second operand)");
    }

    fn generate_coerced_assignment(&mut self, lhs_ty: ty::TypeId, rhs_ty: ty::TypeId) {
        let lhs_ty = lhs_ty.in_db(self.db).peel_ref();

        let coerce_to = match lhs_ty.kind() {
            ty::TypeKind::Real(_) => Some(CoerceTo::Real),
            ty::TypeKind::Char => Some(CoerceTo::Char),
            ty::TypeKind::CharN(ty::SeqSize::Fixed(_)) => {
                let len = length_of_ty(self.db, lhs_ty.id()).expect("never dyn");
                Some(CoerceTo::CharN(len.try_into().unwrap()))
            }
            ty::TypeKind::CharN(ty::SeqSize::Dynamic) => {
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
                ty::TypeKind::CharN(_) => PutKind::CharN(),
                // TODO: Add case for enums
                _ => unreachable!(),
            };

            // Put value onto the stack
            self.generate_expr(item.expr);

            if let ty::TypeKind::CharN(seq_size) = put_ty.kind() {
                let length = seq_size
                    .fixed_len(self.db, Span::default())
                    .ok()
                    .flatten()
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

            // TODO: Deal with get width once we deal with strings vars

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
                let max_len = length_of_ty(self.db, get_ty.id()).expect("is a charseq") as u32;
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

        let coerce_to = match discrim_ty.kind() {
            ty::TypeKind::String | ty::TypeKind::StringN(_) => Some(CoerceTo::String),
            ty::TypeKind::Char => Some(CoerceTo::Char),
            _ => None,
        };

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
                        self.generate_coerced_expr(*expr, coerce_to);
                        self.code_fragment.emit_locate_temp(discrim_value);

                        let expr_ty = self
                            .db
                            .type_of((self.library_id, self.body_id, *expr).into())
                            .in_db(self.db)
                            .peel_ref();

                        let eq_op = if matches!(coerce_to, Some(CoerceTo::Char)) {
                            // Always keep coerced char type as an EQNAT
                            Opcode::EQNAT()
                        } else {
                            // Reuse normal eq selection
                            self.select_eq_op(expr_ty.kind(), discrim_ty.kind())
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

            let body_ty = self.db.type_of((self.library_id, init_body).into());
            self.generate_coerced_assignment(def_ty.id(), body_ty);

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

    fn generate_coerced_expr(&mut self, expr_id: hir_expr::ExprId, coerce_to: Option<CoerceTo>) {
        self.generate_expr(expr_id);

        let expr_ty = self
            .db
            .type_of((self.library_id, self.body_id, expr_id).into());

        self.coerce_expr_into(expr_ty, coerce_to);
    }

    fn coerce_expr_into(&mut self, from_ty: ty::TypeId, coerce_to: Option<CoerceTo>) {
        let expr_ty = from_ty.in_db(self.db).peel_ref();

        let coerce_op = coerce_to.and_then(|coerce_to| match (coerce_to, expr_ty.kind()) {
            (CoerceTo::Real, ty::TypeKind::Nat(_)) => Some(Opcode::NATREAL()),
            (CoerceTo::Real, int) if int.is_integer() => Some(Opcode::INTREAL()),

            (CoerceTo::Char, ty::TypeKind::String | ty::TypeKind::StringN(_)) => {
                Some(Opcode::STRTOCHAR())
            }
            (CoerceTo::Char, ty::TypeKind::CharN(ty::SeqSize::Fixed(_))) => {
                // Compile-time checked to always fit
                let len = length_of_ty(self.db, expr_ty.id());
                assert_eq!(len, Some(1), "never dyn or not 1");

                // Fetch the first char
                Some(Opcode::FETCHNAT1())
            }
            (CoerceTo::Char, ty::TypeKind::CharN(ty::SeqSize::Dynamic)) => {
                todo!()
            }

            (CoerceTo::CharN(len), ty::TypeKind::Char) => {
                // Compile-time checked to always fit
                assert_eq!(len, 1, "never dyn or not 1");

                // Reserve enough space for the temporary char_n
                // Note: This is size is rounded up to char_n's alignemnt size.
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

            (CoerceTo::String, ty::TypeKind::Char) => {
                // Reserve enough space for a `string(1)`
                let temp_str = self.code_fragment.allocate_temporary_space(2);
                self.code_fragment.emit_locate_temp(temp_str);

                Some(Opcode::CHARTOSTR())
            }
            (CoerceTo::String, ty::TypeKind::CharN(ty::SeqSize::Fixed(_))) => {
                // Reserve enough space for the given char_n
                let len = length_of_ty(self.db, expr_ty.id()).expect("never dyn");

                // Include the null terminator in the reservation size
                // Never let it exceed the maximum length of a string
                let reserve_size = (len + 1).min(256);

                let temp_str = self.code_fragment.allocate_temporary_space(reserve_size);
                self.code_fragment.emit_locate_temp(temp_str);

                self.code_fragment.emit_opcode(Opcode::PUSHINT(len as u32));
                Some(Opcode::CSTRTOSTR())
            }
            (CoerceTo::String, ty::TypeKind::CharN(ty::SeqSize::Dynamic)) => {
                todo!()
            }
            _ => None,
        });

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
                    self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                    self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
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
                    self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                    self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
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
                self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
                Opcode::REALDIVIDE()
            }
            hir_expr::BinaryOp::Mod => match (lhs_ty.kind(), rhs_ty.kind()) {
                (ty::TypeKind::Real(_), _) | (_, ty::TypeKind::Real(_)) => {
                    self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                    self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
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
                    self.generate_coerced_expr(expr.lhs, Some(CoerceTo::Real));
                    self.generate_coerced_expr(expr.rhs, Some(CoerceTo::Real));
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
                    self.coerce_to_same(expr, lhs_ty.kind(), rhs_ty.kind());

                    match (lhs_ty.kind(), rhs_ty.kind()) {
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
                    self.coerce_to_same(expr, lhs_ty.kind(), rhs_ty.kind());

                    match (lhs_ty.kind(), rhs_ty.kind()) {
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
                (ty::TypeKind::Char, ty::TypeKind::Char) => Opcode::EQNAT(),
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

#[derive(Clone, Copy)]
enum CoerceTo {
    Real,
    Char,
    CharN(u32),
    String,
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
            ty::TypeKind::CharN(_) => {
                let storage_size = size_of_ty(db, into_tyref.id()) as u32;
                Opcode::ASNNONSCALARINV(storage_size)
            }
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
            ty::TypeKind::CharN(_) => {
                let storage_size = size_of_ty(db, into_tyref.id()) as u32;
                Opcode::ASNNONSCALAR(storage_size)
            }
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
            ty::TypeKind::CharN(_) => return, // don't need to dereference the pointer to storage
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
        ty::TypeKind::CharN(seq_size) | ty::TypeKind::StringN(seq_size) => {
            let char_len = seq_size
                .fixed_len(db, Span::default())
                .ok()
                .flatten()
                .expect("eval should succeed and not be dyn");

            let char_len = (char_len.into_u32().expect("size should be a u32")) as usize;
            if matches!(ty_ref.kind(), ty::TypeKind::StringN(_)) {
                // Storage size for strings includes the always present null terminator
                char_len + 1
            } else {
                // Storage size for char(N)'s is always rounded up to the nearest 2-byte boundary
                // ???: This can always be to the nearest byte boundary, but this depends on the
                // alignment of char(N)
                // TODO: Align up sizes for other types according to the type's alignment
                align_up_to(char_len as usize, 2)
            }
        }
        ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
    }
}

fn length_of_ty(db: &dyn CodeGenDB, ty: ty::TypeId) -> Option<usize> {
    let ty_ref = ty.in_db(db);

    let length = match ty_ref.kind() {
        ty::TypeKind::String => {
            // max chars (excluding null terminator)
            255
        }
        ty::TypeKind::CharN(seq_size) | ty::TypeKind::StringN(seq_size) => {
            let char_len = seq_size
                .fixed_len(db, Span::default())
                .ok()
                .flatten()
                .expect("eval should succeed and not be dyn");

            let char_len = (char_len.into_u32().expect("size should be a u32")) as usize;
            char_len
        }
        ty::TypeKind::Ref(_, _) | ty::TypeKind::Error => unreachable!(),
        _ => return None,
    };

    Some(length)
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
