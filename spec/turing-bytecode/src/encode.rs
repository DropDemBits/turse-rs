//! Builders for encoding bytecode blobs.

use std::{
    collections::BTreeMap,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

use instruction::{Instruction, InstructionEncoder, InstructionRef, Operand, OperandRef};
use section::{
    GlobalsAllocator, GlobalsSection, GlobalsSlot, ManifestAllocator, ManifestSection, ManifestSlot,
};

use crate::instruction::{AbortReason, Opcode, RelocatableOffset};

mod writer;

pub mod instruction;
pub mod section;

/// An opaque handle representing an instruction with a [`RelocatableOffset`] in a procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelocHandle(usize);

impl RelocHandle {
    fn from_usize(id: usize) -> Self {
        Self(id)
    }

    pub fn as_usize(self) -> usize {
        self.0
    }
}

/// Offset within a particular code unit's section.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SectionOffset {
    Code(u32),
    Manifest(u32),
    Global(u32),
}

impl SectionOffset {
    /// Section that this offset refers to.
    pub fn section(self) -> UnitSection {
        match self {
            SectionOffset::Code(_) => UnitSection::Code,
            SectionOffset::Manifest(_) => UnitSection::Manifest,
            SectionOffset::Global(_) => UnitSection::Global,
        }
    }

    /// Offset within the section
    pub fn offset(self) -> u32 {
        match self {
            SectionOffset::Code(offset) => offset,
            SectionOffset::Manifest(offset) => offset,
            SectionOffset::Global(offset) => offset,
        }
    }

    pub fn as_offset(self) -> (UnitSection, u32) {
        match self {
            SectionOffset::Code(offset) => (UnitSection::Code, offset),
            SectionOffset::Manifest(offset) => (UnitSection::Manifest, offset),
            SectionOffset::Global(offset) => (UnitSection::Global, offset),
        }
    }
}

/// A particular section within a [`CodeUnit`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnitSection {
    /// Code section, containing procedures and [`CaseTable`]s.
    Code,
    /// Manifest section, containing read-only data.
    Manifest,
    /// Globals section, containing globals that are always zeroed at startup.
    Global,
}

/// Target for a [`RelocHandle`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RelocTarget {
    Local(SectionOffset),
    External(CodeUnitRef, SectionOffset),
}

/// Bytecode builer entry point, used to build a [`BytecodeBlob`].
///
/// This is the main entry point for the rest of the builders
#[derive(Debug)]
pub struct BytecodeBuilder {
    // need:
    // - env options
    main_unit: Option<CodeUnitRef>,
    next_unit: u32,
    units: BTreeMap<CodeUnitRef, CodeUnit<UnreslovedRelocs>>,
}

impl Default for BytecodeBuilder {
    fn default() -> Self {
        Self {
            main_unit: None,
            next_unit: 1,
            units: BTreeMap::new(),
        }
    }
}

impl BytecodeBuilder {
    /// Creates a new builder to build a bytecode blob.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a builder for a [`CodeUnit`].
    ///
    /// Code units are typically associated with file system filenames, though
    /// this is not a strict requirement.
    pub fn build_code_unit(&mut self, file_name: &str) -> CodeUnitBuilder {
        let id = NonZeroU32::new(self.next_unit).map(CodeUnitRef).unwrap();
        self.next_unit += 1;

        CodeUnitBuilder::new(id, file_name)
    }

    /// Submits a code unit for inclusion into the final bytecode blob.
    /// Any relocations offsets will be resolved once the bytecode blob is
    /// finished being built.
    pub fn submit_code_unit(&mut self, unit: CodeUnit<UnreslovedRelocs>) {
        let id = unit.id();
        let None = self.units.insert(id, unit) else {
            panic!("inserting duplicate code unit {id:?}");
        };
    }

    /// Sets the code unit that will serve as the entry point of execution.
    /// Typically used for the main unit (the file that code generation started
    /// from), though this is not a strict requirement.
    pub fn main_unit(&mut self, code_unit: CodeUnitRef) {
        self.main_unit = Some(code_unit);
    }

    /// Finishes a bytecode blob, ready for encoding.
    pub fn finish(self) -> BytecodeBlob {
        let Self {
            next_unit: _,
            units,
            main_unit,
        } = self;

        // Apply relocs
        let units = units
            .into_iter()
            .map(|(id, unit)| (id, unit.apply_relocs()))
            .collect();

        BytecodeBlob { units, main_unit }
    }
}

impl Index<CodeUnitRef> for BytecodeBuilder {
    type Output = CodeUnit<UnreslovedRelocs>;

    fn index(&self, index: CodeUnitRef) -> &Self::Output {
        self.units.get(&index).expect("invalid code unit ref")
    }
}

impl IndexMut<CodeUnitRef> for BytecodeBuilder {
    fn index_mut(&mut self, index: CodeUnitRef) -> &mut Self::Output {
        self.units.get_mut(&index).expect("invalid code unit ref")
    }
}

/// Top-level bytecode structure, groups together [`CodeUnit`]s as well as
/// containing execution preferences.
#[derive(Debug)]
pub struct BytecodeBlob {
    // also have config and whatnot
    units: BTreeMap<CodeUnitRef, CodeUnit<ResolvedRelocs>>,
    main_unit: Option<CodeUnitRef>,
}

impl BytecodeBlob {
    pub fn code_units(&self) -> impl Iterator<Item = &'_ CodeUnit<ResolvedRelocs>> {
        self.units.values()
    }

    pub fn main_unit(&self) -> Option<CodeUnitRef> {
        self.main_unit
    }

    pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        writer::encode_bytecode(out, self)
    }
}

impl Index<CodeUnitRef> for BytecodeBlob {
    type Output = CodeUnit<ResolvedRelocs>;

    fn index(&self, index: CodeUnitRef) -> &Self::Output {
        self.units.get(&index).expect("invalid code unit ref")
    }
}

/// Refers to a [`CodeUnit`] in a [`BytecodeBuilder`] or [`BytecodeBlob`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct CodeUnitRef(NonZeroU32);

impl CodeUnitRef {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

/// Builder for a [`CodeUnit`].
#[derive(Debug)]
pub struct CodeUnitBuilder {
    // need:
    // - unit ref (for inter-unit referral)
    id: CodeUnitRef,
    // - file name
    file_name: Box<str>,
    // - procedure ref gen
    next_procedure: u32,
    // - procedures
    procedures: BTreeMap<ProcedureRef, Procedure>,
    // - other sections
    manifest: Option<ManifestSection>,
    globals: Option<GlobalsSection>,
}

impl CodeUnitBuilder {
    fn new(id: CodeUnitRef, file_name: &str) -> Self {
        Self {
            id,
            file_name: file_name.to_owned().into_boxed_str(),
            next_procedure: 1,
            procedures: BTreeMap::new(),
            manifest: None,
            globals: None,
        }
    }

    pub fn id(&self) -> CodeUnitRef {
        self.id
    }

    /// Creates a builder to construct a new procedure.
    pub fn build_procedure(&mut self) -> ProcedureBuilder {
        let id = NonZeroU32::new(self.next_procedure)
            .map(ProcedureRef)
            .unwrap();
        self.next_procedure += 1;

        ProcedureBuilder::new(id)
    }

    /// Submits a procedure to be part of this code unit.
    pub fn submit_procedure(&mut self, procedure: Procedure) {
        let id = procedure.id();

        let None = self.procedures.insert(id, procedure) else {
            panic!("inserting duplicate procedure {id:?}")
        };
    }

    /// Submits a manifest section to be part of this code unit.
    pub fn submit_manifest_section(&mut self, manifest: ManifestSection) {
        let None = self.manifest.replace(manifest) else {
            panic!("inserting duplicate manifest section")
        };
    }

    /// Submits a globals section to be part of this code unit.
    pub fn submit_globals_section(&mut self, globals: GlobalsSection) {
        let None = self.globals.replace(globals) else {
            panic!("inserting duplicate globals section")
        };
    }

    /// Finishes the builder to produce a sealed [`CodeUnit`] with potentially
    /// unresolved relocations.
    pub fn finish(self) -> CodeUnit<UnreslovedRelocs> {
        let Self {
            id,
            file_name,
            next_procedure: _,
            procedures,
            manifest,
            globals,
        } = self;

        // Compute procedure offsets, for code relocations
        let procedure_offsets = {
            // At the beginning of every code section, there's 4 bytes which get patched
            // by the interpreter, so we need to shift the procedures to account for this.
            let mut current_offset = 4;

            let offsets: BTreeMap<_, _> = procedures
                .iter()
                .map(|(proc_id, proc)| {
                    let at_offset = current_offset;
                    current_offset += proc.size();
                    (*proc_id, at_offset as u32)
                })
                .collect();

            offsets
        };

        let manifest = manifest.unwrap_or_else(|| ManifestAllocator::new().finish());
        let globals = globals.unwrap_or_else(|| GlobalsAllocator::new().finish());

        CodeUnit {
            id,
            file_name,
            body_unit: None,
            stub_unit: None,
            procedures,
            procedure_offsets,
            manifest,
            globals,
            reloc_info: UnreslovedRelocs {
                code_relocs: BTreeMap::new(),
            },
        }
    }
}

/// Represents a bytecode code unit, which aggregates together procedures
/// and various section data.
#[derive(Debug)]
pub struct CodeUnit<RelocInfo> {
    id: CodeUnitRef,
    file_name: Box<str>,
    body_unit: Option<CodeUnitRef>,
    stub_unit: Option<CodeUnitRef>,
    procedures: BTreeMap<ProcedureRef, Procedure>,
    procedure_offsets: BTreeMap<ProcedureRef, u32>,
    manifest: ManifestSection,
    globals: GlobalsSection,
    reloc_info: RelocInfo,
}

/// Designates a [`CodeUnit`] that still has unresolved relocations in it.
#[derive(Debug)]
pub struct UnreslovedRelocs {
    code_relocs: BTreeMap<
        (Option<CodeUnitRef>, UnitSection),
        BTreeMap<ProcedureRef, Vec<(RelocHandle, u32)>>,
    >,
}

/// Designates a [`CodeUnit`] that has all of its reloctions resolved to
/// a particular [`CodeUnit`]'s section.
#[derive(Debug)]
pub struct ResolvedRelocs {
    local_reloc_heads: BTreeMap<UnitSection, u32>,
    external_reloc_heads: BTreeMap<UnitSection, Vec<(CodeUnitRef, u32)>>,
}

impl<RelocInfo> CodeUnit<RelocInfo> {
    pub fn id(&self) -> CodeUnitRef {
        self.id
    }

    pub fn file_name(&self) -> &str {
        &self.file_name
    }

    pub fn procedures(&self) -> impl Iterator<Item = &'_ Procedure> {
        self.procedures.values()
    }

    pub fn manifest(&self) -> &ManifestSection {
        &self.manifest
    }

    pub fn globals(&self) -> &GlobalsSection {
        &self.globals
    }

    pub fn procedure_offset(&self, procedure: ProcedureRef) -> SectionOffset {
        SectionOffset::Code(
            *self
                .procedure_offsets
                .get(&procedure)
                .expect("invalid procedure ref"),
        )
    }

    pub fn manifest_offset(&self, slot: ManifestSlot) -> SectionOffset {
        SectionOffset::Manifest(self.manifest().offset(slot))
    }

    pub fn globals_offset(&self, slot: GlobalsSlot) -> SectionOffset {
        SectionOffset::Global(self.globals().offset(slot))
    }
}

impl CodeUnit<UnreslovedRelocs> {
    /// Adds code relocation targets to resolve to specific [`RelocHandle`]s.
    pub fn add_code_relocs(
        &mut self,
        relocs: impl IntoIterator<Item = (ProcedureRef, RelocHandle, RelocTarget)>,
    ) {
        for (proc, reloc_handle, reloc_target) in relocs {
            // Partition each reloc by (ExternalCodeUnitRef, CodeSection)
            // These will all be part of the same chain
            let (reloc_unit, offset) = match reloc_target {
                RelocTarget::Local(section_offset) => (None, section_offset),
                RelocTarget::External(code_unit_ref, section_offset) => {
                    (Some(code_unit_ref), section_offset)
                }
            };
            let (reloc_section, reloc_offset) = offset.as_offset();

            let proc_relocs = self
                .reloc_info
                .code_relocs
                .entry((reloc_unit, reloc_section))
                .or_insert(BTreeMap::new());
            proc_relocs
                .entry(proc)
                .or_insert(vec![])
                .push((reloc_handle, reloc_offset));
        }
    }

    pub(crate) fn apply_relocs(mut self) -> CodeUnit<ResolvedRelocs> {
        let code_relocs = std::mem::take(&mut self.reloc_info.code_relocs);
        let mut local_reloc_heads = BTreeMap::new();
        let mut external_reloc_heads = BTreeMap::new();

        // Build patch lists by groups
        for ((target_unit, target_section), reloc_targets) in code_relocs {
            #[derive(Debug, Clone, Copy)]
            struct RelocPatch {
                operand_offset: u32,
                next_link_offset: u32,
                instr: InstructionRef,
                operand: OperandRef,
                section_offset: u32,
            }

            let mut next_link_offset = 0;
            let reloc_patches: BTreeMap<_, _> = reloc_targets
                .into_iter()
                .map(|(proc, relocs)| {
                    let SectionOffset::Code(proc_base) = self.procedure_offset(proc) else {
                        unreachable!()
                    };

                    // Get patch locations associated with each reloc handle
                    let patches: Vec<_> = relocs
                        .into_iter()
                        .map(|(reloc, section_offset)| {
                            let InstrReloc {
                                operand_offset,
                                instr,
                                operand,
                            } = self[proc].reloc_patches[reloc.as_usize()];

                            let patch = RelocPatch {
                                operand_offset: proc_base + operand_offset,
                                next_link_offset,
                                instr,
                                operand,
                                section_offset,
                            };

                            // Link together with the previous patch
                            // Patches are absolute offsets to the next operand from the base of the code table
                            next_link_offset = patch.operand_offset;

                            patch
                        })
                        .collect();

                    dbg!(&patches);

                    (proc, patches)
                })
                .collect();

            // Store patch head offset & info to the first patch of the list
            // This will be the end of the list since offsets point to lower entries
            let Some(patch_head) = reloc_patches
                .last_key_value()
                .and_then(|(_, relocs)| relocs.last())
            else {
                // No patches to apply for this list, somehow
                continue;
            };
            match target_unit {
                Some(external_unit) => external_reloc_heads
                    .entry(target_section)
                    .or_insert(vec![])
                    .push((external_unit, patch_head.operand_offset)),
                None => {
                    let None = local_reloc_heads.insert(target_section, patch_head.operand_offset)
                    else {
                        unreachable!()
                    };
                }
            }

            for (proc, patches) in reloc_patches {
                let procedure = self
                    .procedures
                    .get_mut(&proc)
                    .expect("invalid procedure ref");

                // Patch instructions with links
                for patch in patches {
                    procedure.instrs[patch.instr.as_usize()][patch.operand] =
                        Operand::RelocatableOffset(RelocatableOffset::new(
                            patch.next_link_offset,
                            patch.section_offset,
                        ));
                }
            }
        }

        // Transition into the resolved relocs typestate!
        let Self {
            id,
            file_name,
            body_unit,
            stub_unit,
            procedures,
            procedure_offsets,
            manifest,
            globals,
            reloc_info: _,
        } = self;

        CodeUnit {
            id,
            file_name,
            body_unit,
            stub_unit,
            procedures,
            procedure_offsets,
            manifest,
            globals,
            reloc_info: ResolvedRelocs {
                local_reloc_heads,
                external_reloc_heads,
            },
        }
    }
}

impl CodeUnit<ResolvedRelocs> {
    /// Finds the offset for the start of the section's local relocation list.
    pub fn local_relocs_start(&self, section: UnitSection) -> Option<u32> {
        self.reloc_info.local_reloc_heads.get(&section).copied()
    }

    /// Finds the offset for the start of the section's external relocation list.
    ///
    /// Each entry refers to a different [`CodeUnit`].
    pub fn external_relocs_start(&self, section: UnitSection) -> Option<&[(CodeUnitRef, u32)]> {
        self.reloc_info
            .external_reloc_heads
            .get(&section)
            .map(Vec::as_slice)
    }
}

impl<RelocInfo> Index<ProcedureRef> for CodeUnit<RelocInfo> {
    type Output = Procedure;

    fn index(&self, index: ProcedureRef) -> &Self::Output {
        self.procedures.get(&index).expect("invalid procedure ref")
    }
}

/// Reference to a [`Procedure`] within a [`CodeUnit`] or [`CodeUnitBuilder`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ProcedureRef(NonZeroU32);

impl ProcedureRef {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

/// Builder for a [`Procedure`]
#[derive(Debug)]
pub struct ProcedureBuilder {
    // need:
    // - procedure ref (for building up reloc target side-table)
    id: ProcedureRef,
    // - local (needs to know size up-front for PROC)
    locals: LocalAllocator,
    // - temporary (for longer lived or larger sized temporary storage)
    temps: TemporaryAllocator,
    // - sig? function abi? for handling call abi & arguments
    // - relocs
    // - jump labels
    // - operand patches
    //   - for proc frame sizes
    //     - for proc
    //     - for locate temp
    //   - for local slots
    //   - for temporary slots
    //   - for code offsets
    //     - for jump labels
    //     - for case tables (the only in-code data blob)
    local_patches: Vec<(InstructionRef, OperandRef, PatchWith)>,
    //   - for relocs (deferred)
    reloc_patches: Vec<(InstructionRef, OperandRef)>,
    // - code offset targets
    code_offset_targets: Vec<CodeOffsetTarget>,
    // - case tables
    case_tables: Vec<CaseTable>,
    // - instrs
    instrs: InstructionEncoder,
    //
    // Q: referring to for-loop counter def slot?
    // - type pun with a temporary slot
}

impl ProcedureBuilder {
    fn new(id: ProcedureRef) -> Self {
        let mut instrs = InstructionEncoder::new();
        // All procedures always start with PROC
        let proc = instrs.proc(0);
        // which we'll need to patch the size of afterwards
        let locals_size = instrs[proc].operand_refs().next().unwrap();
        let local_patches = vec![(proc, locals_size, PatchWith::FrameSize)];

        Self {
            id,
            locals: LocalAllocator::new(),
            temps: TemporaryAllocator::new(),
            local_patches,
            reloc_patches: vec![],
            code_offset_targets: vec![],
            case_tables: vec![],
            instrs,
        }
    }

    /// Id to refer to the built procedure later.
    ///
    /// Used for later associating a [`RelocHandle`] with a specific [`RelocTarget`].
    pub fn id(&self) -> ProcedureRef {
        self.id
    }

    /// Places a jump label targeting the next instruction.
    pub fn make_label(&mut self) -> CodeOffset {
        let offset = CodeOffset::from_usize(self.code_offset_targets.len());
        self.code_offset_targets
            .push(CodeOffsetTarget::Instruction(self.instrs.next_ref()));
        offset
    }

    /// Creates a jump label without anchoring it to a specific instruction.
    pub fn make_unanchored_label(&mut self) -> CodeOffset {
        let offset = CodeOffset::from_usize(self.code_offset_targets.len());
        self.code_offset_targets.push(CodeOffsetTarget::Unanchored);
        offset
    }

    /// Anchors an unanchored label at the specified instruction, or to the next instruction.
    pub fn anchor_label(&mut self, offset: CodeOffset, at_instr: Option<InstructionRef>) {
        assert_eq!(
            self.code_offset_targets[offset.as_usize()],
            CodeOffsetTarget::Unanchored
        );

        let target = at_instr.unwrap_or(self.instrs.next_ref());
        self.code_offset_targets[offset.as_usize()] = CodeOffsetTarget::Instruction(target);
    }

    /// Adds a case table related to a specific CASE instruction.
    pub fn add_case_table(&mut self, table: CaseTable) {
        assert_eq!(
            self.instrs[table.case_instruction()].opcode(),
            Opcode::CASE,
            "case instruction must target a CASE opcode"
        );

        let slot = CaseTableRef::from_usize(self.case_tables.len());

        // Patch case instruction
        let offset = CodeOffset::from_usize(self.code_offset_targets.len());
        self.code_offset_targets
            .push(CodeOffsetTarget::CaseTable(slot));
        self.patch_code_offset(table.case_instruction(), offset);

        self.case_tables.push(table);
    }

    /// Helper function to easily create patch the code offset of branching instructions,
    /// and the [`InstructionRef`] is not required.
    pub fn branch(
        &mut self,
        ins: impl FnOnce(&mut InstructionEncoder) -> InstructionRef,
        code_offset: CodeOffset,
    ) {
        let instr = ins(&mut self.instrs);
        self.patch_code_offset(instr, code_offset);
    }

    /// Emits an instruction where the first operand is a [`RelocatableOffset`].
    /// The [`RelocHandle`] is an opaque handle referring to this instruction's operand.
    pub fn reloc(
        &mut self,
        ins: impl FnOnce(&mut InstructionEncoder) -> InstructionRef,
    ) -> RelocHandle {
        let instr = ins(&mut self.instrs);
        let offset = self.instrs[instr].operand_refs().nth(0).unwrap();
        assert!(matches!(
            self.instrs[instr][offset],
            Operand::RelocatableOffset(_)
        ));

        let slot = RelocHandle::from_usize(self.reloc_patches.len());
        self.reloc_patches.push((instr, offset));
        slot
    }

    /// Patches an instruction's first operand to refer to a specifc code offset.
    pub fn patch_code_offset(&mut self, instr: InstructionRef, code_offset: CodeOffset) {
        assert!(matches!(
            self.instrs[instr].operands().nth(0),
            Some(Operand::Offset(_))
        ));

        let offset = self.instrs[instr].operand_refs().nth(0).unwrap();
        self.local_patches
            .push((instr, offset, PatchWith::CodeOffset(code_offset)));
    }

    /// Allocates a temporary slot of the procedure call frame.
    pub fn alloc_temporary(&mut self, size: u32, align: u32) -> TemporarySlot {
        self.temps.alloc(size, align)
    }

    /// Allocates a local slot of the procedure call frame.
    pub fn alloc_local(&mut self, name: Option<&str>, size: u32, align: u32) -> LocalSlot {
        self.locals.alloc(name, size, align)
    }

    /// Emits a [**LOCATETEMP**] instruction
    /// for the given `TemporarySlot`.
    ///
    /// Temporary offsets are not resolved until a procedure is finished
    /// building, so this will take care of patching the correct offsets.
    ///
    /// [**LOCATETEMP**]: crate::instruction::Opcode::LOCATETEMP
    pub fn locate_temporary(&mut self, slot: TemporarySlot) {
        let locate_temp = self.instrs.locatetemp(0, 0);
        let locals_size = self.instrs[locate_temp].operand_refs().nth(0).unwrap();
        let temporary_offset = self.instrs[locate_temp].operand_refs().nth(1).unwrap();

        self.local_patches
            .push((locate_temp, locals_size, PatchWith::FrameSize));
        self.local_patches.push((
            locate_temp,
            temporary_offset,
            PatchWith::TemporarySlot(slot),
        ));
    }

    /// Emits a [**LOCATELOCAL**](Opcode::LOCATELOCAL) instruction
    /// for the given `TemporarySlot`.
    ///
    /// Temporary offsets are not resolved until a procedure is finished
    /// building, so this will take care of patching the correct offsets.
    ///
    /// [**LOCATELOCAL**]: crate::instruction::Opcode::LOCATELOCAL
    pub fn locate_local(&mut self, slot: LocalSlot) {
        let locate_local = self.instrs.locatelocal(0);
        let local_offset = self.instrs[locate_local].operand_refs().nth(0).unwrap();

        self.local_patches
            .push((locate_local, local_offset, PatchWith::LocalSlot(slot)));
    }

    /// Starts a new temporary allocation scope.
    ///
    /// After leaving this scope, the space for any [`TemporarySlot`] allocated
    /// in this function will be made available for future temporaries.
    pub fn in_temporary_scope(&mut self, in_scope: impl FnOnce(&mut Self)) {
        let old = self.temps.enter_scope();
        in_scope(self);
        self.temps.leave_scope(old);
    }

    /// Gets the instruction encoder to encode instructions in.
    ///
    /// For most instructions that do not need later patching, this is the
    /// preferred encoder entry point.
    pub fn ins(&mut self) -> &mut InstructionEncoder {
        &mut self.instrs
    }

    /// Finishes building a procedure.
    ///
    /// This resolves all [`TemporarySlot`], [`LocalSlot`], and [`CodeOffset`]
    /// offsets, as well as the call frame size. Any generated [`RelocHandle`]s
    /// are later resolved by [`BytecodeBuilder::finish`].
    pub fn finish(self) -> Procedure {
        let Self {
            id,
            locals,
            temps,
            local_patches,
            reloc_patches,
            code_offset_targets,
            case_tables,
            mut instrs,
        } = self;

        // All procedures should end with an abort with no result
        instrs.abort(AbortReason::NoResult);

        // Compute instruction offsets
        let (instr_offsets, instrs_size) = {
            let mut current_offset = 0;

            let offsets: Vec<_> = instrs
                .instrs()
                .iter()
                .map(|instr| {
                    let at_offset = current_offset;
                    current_offset += instr.size();
                    at_offset
                })
                .collect();

            (offsets, current_offset)
        };
        dbg!(&instr_offsets);

        // Compute case table offsets
        // These are located afte the last instruction of the procedure (the ABORT).
        let (case_offsets, procedure_size) = {
            let mut current_offset = instrs_size;

            let offsets: Vec<_> = case_tables
                .iter()
                .map(|table| {
                    let at_offset = current_offset;
                    current_offset += table.size();
                    at_offset
                })
                .collect();

            (offsets, current_offset)
        };

        // Call Frame:
        //
        // | stack_operands    | <- SP
        // | ...               |
        // | frame_size        |
        // | ...               |
        // | - temps_size      | temps grows down
        // | - locals_size     | locals grows up
        // | ...               | <- FP
        // | prev_fp : addrint |
        // | file : u16        |
        // | line : u16        | to higher addresses

        let locals_area = locals.finish();
        let temp_area = temps.finish();
        let frame_size = (locals_area.size() + temp_area.size()).next_multiple_of(4);

        let compute_code_offset = |instrs: &InstructionEncoder,
                                   instr: InstructionRef,
                                   offset: CodeOffset|
         -> usize {
            let start_offset = instr_offsets[instr.as_usize()];
            let end_offset = match &code_offset_targets[offset.as_usize()] {
                CodeOffsetTarget::Instruction(target_instr) => {
                    instr_offsets[target_instr.as_usize()]
                }
                CodeOffsetTarget::CaseTable(target_table) => case_offsets[target_table.as_usize()],
                CodeOffsetTarget::Unanchored => panic!("unresolved code offset {offset:?}"),
            };

            // Offsets are relative to the operand, so we'll need to either add
            // (for backwards offsets) or subtract (for forwards offsets)
            // the offset leading up to the operand.
            if end_offset < start_offset {
                // Backwards offset
                start_offset.wrapping_sub(end_offset) + instrs[instr].opcode().size()
            } else {
                // Forward offset
                end_offset.wrapping_sub(start_offset) - instrs[instr].opcode().size()
            }
        };

        // Apply local patches
        for (instr, operand, patch) in local_patches {
            instrs[instr][operand] = match patch {
                PatchWith::FrameSize => Operand::Nat4(frame_size),
                PatchWith::LocalSlot(slot) => Operand::Offset(locals_area[slot]),
                PatchWith::TemporarySlot(slot) => Operand::Offset(temp_area[slot]),
                PatchWith::CodeOffset(offset) => {
                    Operand::Offset(compute_code_offset(&instrs, instr, offset) as u32)
                }
            };
        }

        // Resolve the case tables
        let resolved_case_tables: Vec<_> = case_tables
            .into_iter()
            .map(|table| {
                table.resolve_offsets(|case_instr, offset| {
                    compute_code_offset(&instrs, case_instr, offset)
                })
            })
            .collect();

        // Associate relocation patches with the instruction offsets so we don't have to compute it later
        let reloc_patches: Vec<_> = reloc_patches
            .into_iter()
            .map(|(instr, operand)| InstrReloc {
                operand_offset: (instr_offsets[instr.as_usize()] + instrs[instr].opcode().size())
                    as u32,
                instr,
                operand,
            })
            .collect();

        let instrs = instrs.finish();
        Procedure {
            id,
            size: procedure_size,
            instrs,
            case_tables: resolved_case_tables.into_boxed_slice(),
            reloc_patches,
        }
    }
}

/// A procedure containing instructions and case tables.
#[derive(Debug)]
pub struct Procedure {
    // store:
    // - procedure ref (for resolving reloc target side-table)
    id: ProcedureRef,
    // - size (in bytes)
    size: usize,
    // - instruction list
    instrs: Box<[Instruction]>,
    // - case tables (with resolved offsets)
    case_tables: Box<[CaseTable<u32>]>,
    // - operand patches
    //   - for relocs
    reloc_patches: Vec<InstrReloc>,
}

impl Procedure {
    pub fn id(&self) -> ProcedureRef {
        self.id
    }

    pub fn instrs(&self) -> &[Instruction] {
        &self.instrs
    }

    pub fn case_tables(&self) -> &[CaseTable<u32>] {
        &self.case_tables
    }

    /// Size of the entire procedure (including encoded case tables), in bytes.
    pub fn size(&self) -> usize {
        self.size
    }
}

#[derive(Debug)]
struct InstrReloc {
    operand_offset: u32,
    instr: InstructionRef,
    operand: OperandRef,
}

/// Refers to a code section offset within a [`ProcedureBuilder`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CodeOffset(u32);

impl CodeOffset {
    fn from_usize(idx: usize) -> CodeOffset {
        Self(idx as u32)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodeOffsetTarget {
    Instruction(InstructionRef),
    CaseTable(CaseTableRef),
    Unanchored,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct CaseTableRef(u32);

impl CaseTableRef {
    fn from_usize(idx: usize) -> Self {
        Self(idx as u32)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// Builder for a [`CaseTable`].
///
/// This manages building up a case table's entry list.
#[derive(Debug)]
pub struct CaseTableBuilder {
    signedness: Option<bool>,
    branches: BTreeMap<CaseBound, CodeOffset>,
}

impl Default for CaseTableBuilder {
    fn default() -> Self {
        Self {
            signedness: None,
            branches: BTreeMap::new(),
        }
    }
}

impl CaseTableBuilder {
    /// Creates a new case table builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a case statement arm corresponding to a particular range and
    /// branch target.
    pub fn add_arm(&mut self, bound: CaseRange, target: CodeOffset) {
        if self.signedness.is_none() {
            self.signedness = Some(bound.is_signed());
        }
        assert_eq!(
            self.signedness,
            Some(bound.is_signed()),
            "case bound signedness must all match"
        );

        let target_bounds = match bound {
            CaseRange::SingleU32(it) => {
                vec![CaseBound::U32(it)]
            }
            CaseRange::SingleI32(it) => {
                vec![CaseBound::I32(it)]
            }
            CaseRange::RangeU32(lo, hi) => (lo..=hi).into_iter().map(CaseBound::U32).collect(),
            CaseRange::RangeI32(lo, hi) => (lo..=hi).into_iter().map(CaseBound::I32).collect(),
        };

        for bound in target_bounds {
            let None = self.branches.insert(bound, target) else {
                panic!(
                    "case bound {bound:?} overlaps with an already handled bound {:?}",
                    self.branches.get(&bound)
                )
            };
        }
    }

    /// Finishes a case table, associating it with a specifc [**CASE**]
    /// instruction and a default branch.
    ///
    /// [**CASE**]: crate::instruction::Opcode::CASE
    pub fn finish(self, case_instruction: InstructionRef, default_branch: CodeOffset) -> CaseTable {
        let Self {
            branches,
            signedness: _,
        } = self;

        let (lower_bound, upper_bound) = if let Some(((lower_bound, _), (upper_bound, _))) =
            branches.first_key_value().zip(branches.last_key_value())
        {
            (lower_bound.as_unsigned(), upper_bound.as_unsigned())
        } else {
            panic!("case table is missing arms")
        };

        let bounds_len = lower_bound.abs_diff(upper_bound) + 1;
        assert!(
            bounds_len < 1000,
            "case table cannot handle more than 1000 entries"
        );

        let mut targets = vec![];
        let mut last_handled_bound: Option<CaseBound> = None;

        for (bound, target) in branches {
            if let Some(last_handled_bound) = last_handled_bound {
                let gap_len = bound
                    .as_unsigned()
                    .abs_diff(last_handled_bound.as_unsigned());

                if gap_len > 1 {
                    // Fill in the gap with the default branch
                    for _ in 1..gap_len {
                        targets.push(default_branch);
                    }
                }
            }
            last_handled_bound = Some(bound);
            targets.push(target);
        }

        debug_assert_eq!(bounds_len as usize, targets.len());

        CaseTable {
            lower_bound,
            upper_bound,
            case_instruction,
            default_branch,
            arm_branches: targets.into_boxed_slice(),
        }
    }
}

/// A bound or case label value as part of a case table.
/// Since bounds may either be positive or negative integers, this allows
/// specifying bounds in terms of both.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaseRange {
    /// Specifies a single unsigned value to handle.
    SingleU32(u32),
    /// Specifies a single signed value to handle.
    SingleI32(i32),
    /// Specifies an unsigned inclusive range of values to handle.
    RangeU32(u32, u32),
    /// Specifies a signed inclusive range of values to handle.
    RangeI32(i32, i32),
}

impl CaseRange {
    /// Length of the case range.
    pub fn len(self) -> u32 {
        match self {
            CaseRange::SingleU32(_) | CaseRange::SingleI32(_) => 1,
            CaseRange::RangeU32(low, high) => high.abs_diff(low) + 1,
            CaseRange::RangeI32(low, high) => high.abs_diff(low) + 1,
        }
    }

    /// If the case range represents a signed number range.
    pub fn is_signed(self) -> bool {
        matches!(self, CaseRange::SingleI32(_) | CaseRange::RangeI32(_, _))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum CaseBound {
    U32(u32),
    I32(i32),
}

impl CaseBound {
    fn as_unsigned(self) -> u32 {
        match self {
            CaseBound::U32(it) => it,
            CaseBound::I32(it) => u32::from_ne_bytes(it.to_ne_bytes()),
        }
    }
}

/// Describes a jump table for a [**CASE**] instruction.
///
/// [**CASE**]: crate::instruction::Opcode::CASE
#[derive(Debug)]
pub struct CaseTable<Offset = CodeOffset> {
    // Even though these are treated in the OT interpreter as signed values,
    // we effectively treat them as unsigned ones in a wrapping integer space.
    // This is a valid interpretation as the inclusive absolute difference
    // between the bounds cannot exceed 1000.
    //
    // Curious how OT handles compiling bounds crossing i32::MAX?
    // It doesn't! Causes an access violation, probably from too large of an
    // allocation.
    case_instruction: InstructionRef,
    pub lower_bound: u32,
    pub upper_bound: u32,
    pub default_branch: Offset,
    pub arm_branches: Box<[Offset]>,
}

impl<Offset> CaseTable<Offset> {
    /// Size of the case table to encode, in bytes.
    pub fn size(&self) -> usize {
        // lower_bound + upper_bound + default_branch
        let descriptor_header = 4 + 4 + 4;

        // each code offset is 4 bytes
        descriptor_header + self.arm_branches.len() * 4
    }
}

impl CaseTable<CodeOffset> {
    /// Which CASE instruction is this table for.
    pub fn case_instruction(&self) -> InstructionRef {
        self.case_instruction
    }

    pub fn resolve_offsets(
        self,
        resolve_offset: impl Fn(InstructionRef, CodeOffset) -> usize,
    ) -> CaseTable<u32> {
        CaseTable::<u32> {
            case_instruction: self.case_instruction,
            lower_bound: self.lower_bound,
            upper_bound: self.upper_bound,
            default_branch: resolve_offset(self.case_instruction, self.default_branch) as u32,
            arm_branches: self
                .arm_branches
                .iter()
                .map(|offset| resolve_offset(self.case_instruction, *offset) as u32)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }
}

#[derive(Debug)]
struct TemporaryAllocator {
    // active scope information
    depth: u32,
    scope: u32,

    next_scope: u32,
    slots: Vec<TemporaryInfo>,
}

#[derive(Debug)]
struct TemporaryInfo {
    scope: TemporaryScope,
    size: u32,
    align: u32,
}

impl TemporaryAllocator {
    pub fn new() -> Self {
        Self {
            depth: 0,
            scope: 0,
            next_scope: 0,
            slots: vec![],
        }
    }

    /// Allocates a new temporary slot of the given size and alignment.
    fn alloc(&mut self, size: u32, align: u32) -> TemporarySlot {
        assert!(align > 0);
        assert!(align.is_power_of_two() && align <= 4);

        let slot = NonZeroU32::new(self.slots.len().saturating_add(1) as u32)
            .map(TemporarySlot)
            .unwrap();

        // Align size up to the align, then the next stack slot
        let size = size.next_multiple_of(align).next_multiple_of(4);

        self.slots.push(TemporaryInfo {
            scope: TemporaryScope {
                depth: self.depth,
                scope: self.scope,
            },
            size,
            align,
        });

        slot
    }

    /// Enters into a nested temporary scope.
    fn enter_scope(&mut self) -> TemporaryScope {
        let old_scope = TemporaryScope {
            depth: self.depth,
            scope: self.scope,
        };

        self.next_scope += 1;
        self.depth += 1;
        self.scope = self.next_scope;

        old_scope
    }

    /// Leaves a temporary scope.
    /// Any allocations made in the scope are effectively deallocated.
    fn leave_scope(&mut self, old_scope: TemporaryScope) {
        let TemporaryScope { depth, scope } = old_scope;

        self.depth = depth;
        self.scope = scope;
    }

    /// Finalizes the offsets of temporary slots.
    fn finish(&self) -> TemporariesArea {
        // Q: What's struct ABI?
        // max alignment: 0x4
        // possible alignments: 0x1, 0x2, 0x4 (based on size)

        // Each scope's size is the sum of the size of each slot, rounded up to the nearest stack slot
        // By construction, scopes are always ordered from least to most deep
        let mut scope_sizes = BTreeMap::new();

        // Accumulate scope sizes
        for slot in &self.slots {
            let alloc_size = slot.size.next_multiple_of(slot.align).next_multiple_of(4);
            *scope_sizes.entry(&slot.scope).or_insert(0u32) += alloc_size;
        }

        // Compute scope bases
        let mut scope_bases = BTreeMap::new();
        let mut scope_stack: Vec<(&TemporaryScope, u32, u32)> = vec![];

        for &scope in scope_sizes.keys() {
            let scope_size = *scope_sizes.get(scope).unwrap();

            if let Some((top, top_offset, top_size)) = scope_stack.last() {
                let (parent_offset, parent_size) = match scope.depth.cmp(&top.depth) {
                    std::cmp::Ordering::Less | std::cmp::Ordering::Equal => {
                        // Scope is shallower or at the same depth of the top of stack
                        // Scope depths in the stack must always be ascending
                        while let Some((top, _, _)) = scope_stack.last() {
                            if scope.depth > top.depth {
                                break;
                            }
                            scope_stack.pop();
                        }

                        scope_stack
                            .last()
                            .map(|(_, offset, size)| (*offset, *size))
                            .expect("must always be a bottom scope")
                    }
                    std::cmp::Ordering::Greater => {
                        // Scope is deeper than top of stack
                        // Fall through to common case
                        (*top_offset, *top_size)
                    }
                };

                // Scope is on top of the parent's scope
                let new_offset = parent_offset + parent_size;

                scope_bases.insert(scope, new_offset);
                scope_stack.push((scope, new_offset, scope_size));
            } else {
                // Bottom-most scope, which is at offset 0
                scope_bases.insert(scope, 0);
                scope_stack.push((scope, 0, scope_size));
            }
        }

        // Compute final slot offsets
        let mut watermark = 0;
        let mut base_offset = 0;
        let mut allocated_offsets = BTreeMap::new();
        let mut current_scope = None;
        let offsets: Vec<u32> = self
            .slots
            .iter()
            .map(|slot| {
                if current_scope != Some(&slot.scope) {
                    current_scope = Some(&slot.scope);
                    base_offset = *scope_bases.get(&&slot.scope).unwrap();
                }

                let alloc_size = slot.size.next_multiple_of(slot.align).next_multiple_of(4);
                let alloc_offset = allocated_offsets.entry(&slot.scope).or_insert(base_offset);
                let slot_offset = alloc_offset
                    .next_multiple_of(slot.align)
                    .next_multiple_of(4);
                *alloc_offset = slot_offset + alloc_size;
                watermark = watermark.max(*alloc_offset);

                slot_offset
            })
            .collect();

        TemporariesArea {
            offsets: offsets.into_boxed_slice(),
            size: watermark,
        }
    }
}

struct TemporariesArea {
    offsets: Box<[u32]>,
    size: u32,
}

impl TemporariesArea {
    fn size(&self) -> u32 {
        self.size
    }
}

impl Index<TemporarySlot> for TemporariesArea {
    type Output = u32;

    fn index(&self, index: TemporarySlot) -> &Self::Output {
        &self.offsets[index.as_usize()]
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TemporaryScope {
    depth: u32,
    scope: u32,
}

/// Refers to a temporary allocation within a procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TemporarySlot(NonZeroU32);

impl TemporarySlot {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
struct LocalAllocator {
    slots: Vec<LocalInfo>,
}

#[derive(Debug)]
struct LocalInfo {
    _name: Option<Box<str>>,
    size: u32,
    align: u32,
}

impl LocalAllocator {
    fn new() -> Self {
        Self { slots: vec![] }
    }

    fn alloc(&mut self, name: Option<&str>, size: u32, align: u32) -> LocalSlot {
        assert!(align > 0);
        assert!(align.is_power_of_two() && align <= 4);

        let slot = NonZeroU32::new(self.slots.len().saturating_add(1) as u32)
            .map(LocalSlot)
            .unwrap();
        let size = size.next_multiple_of(align);

        self.slots.push(LocalInfo {
            _name: name.map(|it| it.to_owned().into_boxed_str()),
            size,
            align,
        });
        slot
    }

    fn finish(self) -> LocalsArea {
        let mut offset = 0u32;

        let offsets: Vec<_> = self
            .slots
            .into_iter()
            .map(|slot| {
                let slot_offset = offset.next_multiple_of(slot.align).next_multiple_of(4);
                offset = slot_offset + slot.size;
                slot_offset
            })
            .collect();

        // Align up to the next stack slot
        offset = offset.next_multiple_of(4);

        LocalsArea {
            offsets: offsets.into_boxed_slice(),
            size: offset,
        }
    }
}

/// Represents the locals area of a procedure's call frame.
pub struct LocalsArea {
    size: u32,
    offsets: Box<[u32]>,
}

impl LocalsArea {
    /// Size of the locals area, in bytes.
    pub fn size(&self) -> u32 {
        self.size
    }
}

impl Index<LocalSlot> for LocalsArea {
    type Output = u32;

    fn index(&self, index: LocalSlot) -> &Self::Output {
        &self.offsets[index.as_usize()]
    }
}

/// Refers to a local allocation within a procedure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LocalSlot(NonZeroU32);

impl LocalSlot {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
enum PatchWith {
    FrameSize,
    LocalSlot(LocalSlot),
    TemporarySlot(TemporarySlot),
    CodeOffset(CodeOffset),
}

#[cfg(test)]
mod tests {
    use crate::instruction::{AbortReason, Opcode, RelocatableOffset};

    use super::*;

    #[track_caller]
    fn assert_match_instrs(instrs: &[Instruction], expected: &[(Opcode, &[Operand])]) {
        for (instr_index, (instr, (expected_opcode, expected_operands))) in
            instrs.iter().zip(expected.iter()).enumerate()
        {
            assert_eq!(
                instr.opcode(),
                *expected_opcode,
                "opcode on instruction {instr_index} does not match"
            );
            for (operand_index, (operand, expected_operand)) in
                instr.operands().zip(expected_operands.iter()).enumerate()
            {
                assert_eq!(
                    operand, expected_operand,
                    "operand {operand_index} on instruction {instr_index} does not match"
                );
            }

            assert_eq!(
                instr.operands().count(),
                expected_operands.len(),
                "instruction {instr_index} does not have expected operand count"
            );
        }

        assert_eq!(
            instrs.len(),
            expected.len(),
            "instruction count does not match"
        );
    }

    #[test]
    fn temp_alloc_align_up() {
        let mut alloc = TemporaryAllocator::new();

        let a = alloc.alloc(1, 1);
        let b = alloc.alloc(1, 1);
        let c = alloc.alloc(1, 1);
        let d = alloc.alloc(1, 1);

        let temp = alloc.finish();

        // Must be aligned to the nearest stack slot
        assert_eq!(temp.size(), 16);
        // Each smaller than stack-size slot gets rounded up
        assert_eq!(temp[a], 0);
        assert_eq!(temp[b], 4);
        assert_eq!(temp[c], 8);
        assert_eq!(temp[d], 12);
    }

    #[test]
    fn temp_alloc_no_overalign() {
        let mut alloc = TemporaryAllocator::new();

        let a = alloc.alloc(4, 4);
        let b = alloc.alloc(8, 4);
        let c = alloc.alloc(4, 4);

        let temp = alloc.finish();

        // Must be aligned to the nearest stack slot
        assert_eq!(temp.size(), 16);
        // No over-alignment should happen
        assert_eq!(temp[a], 0);
        assert_eq!(temp[b], 4);
        assert_eq!(temp[c], 12);
    }

    #[test]
    fn temp_alloc_nested() {
        let mut alloc = TemporaryAllocator::new();

        let a = alloc.alloc(4, 4);
        let b = alloc.alloc(4, 4);

        let (a_1, b_1);
        let (a_1_1, b_1_1);
        let (a_1_2, b_1_2);

        {
            let scope_1 = alloc.enter_scope();
            a_1 = alloc.alloc(8, 4);

            {
                let scope_1_1 = alloc.enter_scope();
                a_1_1 = alloc.alloc(12, 4);
                b_1_1 = alloc.alloc(4, 4);
                alloc.leave_scope(scope_1_1);
            }
            {
                let scope_1_2 = alloc.enter_scope();
                a_1_2 = alloc.alloc(4, 4);
                b_1_2 = alloc.alloc(16, 4);
                alloc.leave_scope(scope_1_2);
            }

            b_1 = alloc.alloc(8, 4);
            alloc.leave_scope(scope_1);
        }

        let c = alloc.alloc(4, 4);

        let temp = alloc.finish();

        // Temporary scope size is based on all temporaries allocated in a scope, not just which ones are live
        assert_eq!(temp.size(), 12 + 16 + 20);

        assert_eq!(temp[a], 0);
        assert_eq!(temp[b], 4);
        assert_eq!(temp[c], 8);

        assert_eq!(temp[a_1], 12);
        assert_eq!(temp[b_1], 20);

        assert_eq!(temp[a_1_1], 28);
        assert_eq!(temp[b_1_1], 40);

        assert_eq!(temp[a_1_2], 28);
        assert_eq!(temp[b_1_2], 32);
    }

    #[test]
    fn build_empty_code_unit() {
        let mut bytecode = BytecodeBuilder::new();
        let unit = bytecode.build_code_unit("<No File>");
        let unit = unit.finish();

        assert_eq!(unit.id().as_usize(), 0);
        assert_eq!(unit.file_name(), "<No File>");
    }

    #[test]
    fn build_empty_procedure() {
        let mut bytecode = BytecodeBuilder::new();
        let mut unit = bytecode.build_code_unit("<No File>");
        let procedure = unit.build_procedure();
        let procedure = procedure.finish();

        assert_eq!(procedure.id().as_usize(), 0);
        // Procedures should always start with a PROC xxx
        assert_eq!(procedure.instrs[0].opcode(), Opcode::PROC);
        // Procedures should always end with a ABORT NoResult
        assert_eq!(procedure.instrs[1].opcode(), Opcode::ABORT);
    }

    #[test]
    fn build_procedure_with_locals() {
        // adds some instructions with local locates
        let mut bytecode = BytecodeBuilder::new();
        let mut unit = bytecode.build_code_unit("<No File>");
        let mut procedure = unit.build_procedure();

        let a = procedure.alloc_local(Some("a"), 4, 4);
        let b = procedure.alloc_local(Some("b"), 4, 4);

        procedure.locate_local(a);
        procedure.locate_local(b);
        procedure.ins().addintnat();
        procedure.ins().return_();

        let procedure = procedure.finish();

        assert_eq!(procedure.id().as_usize(), 0);
        assert_match_instrs(
            procedure.instrs(),
            &[
                // Procedures should always start with a PROC xxx
                // Frame size should be 8
                (Opcode::PROC, &[Operand::Nat4(8)]),
                // Local offsets should be patched
                (Opcode::LOCATELOCAL, &[Operand::Offset(0)]),
                (Opcode::LOCATELOCAL, &[Operand::Offset(4)]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::RETURN, &[]),
                // Procedures should always end with a ABORT NoResult
                (
                    Opcode::ABORT,
                    &[Operand::AbortReason(AbortReason::NoResult)],
                ),
            ],
        );
    }

    #[test]
    fn build_procedure_with_locals_and_temps() {
        // adds some instructions with temp and local locates
        let mut bytecode = BytecodeBuilder::new();
        let mut unit = bytecode.build_code_unit("<No File>");
        let mut procedure = unit.build_procedure();

        let a = procedure.alloc_local(Some("a"), 4, 4);
        let b = procedure.alloc_local(Some("b"), 4, 4);
        let c = procedure.alloc_temporary(4, 4);
        let d = procedure.alloc_temporary(4, 4);

        procedure.locate_local(a);
        procedure.locate_local(b);
        procedure.ins().addintnat();
        procedure.locate_temporary(c);
        procedure.ins().addintnat();
        procedure.locate_temporary(d);
        procedure.ins().addintnat();
        procedure.ins().return_();

        let procedure = procedure.finish();

        assert_eq!(procedure.id().as_usize(), 0);
        assert_match_instrs(
            procedure.instrs(),
            &[
                // Procedures should always start with a PROC xxx
                // Frame size should be 16
                (Opcode::PROC, &[Operand::Nat4(16)]),
                // Local offsets should be patched
                (Opcode::LOCATELOCAL, &[Operand::Offset(0)]),
                (Opcode::LOCATELOCAL, &[Operand::Offset(4)]),
                (Opcode::ADDINTNAT, &[]),
                // Temporaries offsets should be patched
                // with the frame size and offset
                (Opcode::LOCATETEMP, &[Operand::Nat4(16), Operand::Offset(0)]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::LOCATETEMP, &[Operand::Nat4(16), Operand::Offset(4)]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::RETURN, &[]),
                // Procedures should always end with a ABORT NoResult
                (
                    Opcode::ABORT,
                    &[Operand::AbortReason(AbortReason::NoResult)],
                ),
            ],
        );
    }

    #[test]
    fn build_procedure_with_branches() {
        let mut bytecode = BytecodeBuilder::new();
        let mut unit = bytecode.build_code_unit("<No File>");
        let mut procedure = unit.build_procedure();

        let forward_branch = procedure.ins().jump(0);
        let backward_target = procedure.make_label();
        {
            let a = procedure.alloc_temporary(4, 4);
            let b = procedure.alloc_temporary(4, 4);

            procedure.locate_temporary(a);
            procedure.locate_temporary(b);
            procedure.ins().addintnat();

            procedure.ins().return_();
        }

        let forward_target = procedure.make_label();
        let backward_branch = procedure.ins().jumpb(0);
        procedure.patch_code_offset(backward_branch, backward_target);

        procedure.patch_code_offset(forward_branch, forward_target);

        let procedure = procedure.finish();

        assert_eq!(procedure.id().as_usize(), 0);
        assert_match_instrs(
            procedure.instrs(),
            &[
                // Procedures should always start with a PROC xxx
                (Opcode::PROC, &[Operand::Nat4(8)]),
                (Opcode::JUMP, &[Operand::Offset(9 * 4)]),
                (Opcode::LOCATETEMP, &[Operand::Nat4(8), Operand::Offset(0)]),
                (Opcode::LOCATETEMP, &[Operand::Nat4(8), Operand::Offset(4)]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::RETURN, &[]),
                (Opcode::JUMPB, &[Operand::Offset(9 * 4)]),
                // Procedures should always end with a ABORT NoResult
                (
                    Opcode::ABORT,
                    &[Operand::AbortReason(AbortReason::NoResult)],
                ),
            ],
        );
    }

    #[test]
    fn build_procedure_with_case_table() {
        let mut bytecode = BytecodeBuilder::new();
        let mut unit = bytecode.build_code_unit("<No File>");
        let mut procedure = unit.build_procedure();

        procedure.ins().pushval1();

        let case_instr = procedure.ins().case(0);
        let common_branch = procedure.make_unanchored_label();
        let mut table_builder = CaseTableBuilder::new();

        // Arm 0
        {
            table_builder.add_arm(CaseRange::SingleU32(1), procedure.make_label());
            procedure.ins().pushval0();
            procedure.ins().pushval1();
            procedure.ins().addintnat();

            let jump = procedure.ins().jump(0);
            procedure.patch_code_offset(jump, common_branch);
        }

        // Arm 1
        {
            table_builder.add_arm(CaseRange::SingleU32(2), procedure.make_label());
            procedure.ins().pushval1();
            procedure.ins().pushval1();
            procedure.ins().addintnat();

            let jump = procedure.ins().jump(0);
            procedure.patch_code_offset(jump, common_branch);
        }

        // Arm 2
        {
            table_builder.add_arm(CaseRange::SingleU32(3), procedure.make_label());
            procedure.ins().pushval1();
            procedure.ins().pushval0();
            procedure.ins().addintnat();

            let jump = procedure.ins().jump(0);
            procedure.patch_code_offset(jump, common_branch);
        }

        let default_branch = procedure.make_label();
        {
            let jump = procedure.ins().jump(0);
            procedure.patch_code_offset(jump, common_branch);
        }

        procedure.add_case_table(table_builder.finish(case_instr, default_branch));

        procedure.anchor_label(common_branch, None);
        procedure.ins().return_();

        let procedure = procedure.finish();

        assert_eq!(procedure.id().as_usize(), 0);
        assert_match_instrs(
            procedure.instrs(),
            &[
                // Procedures should always start with a PROC xxx
                (Opcode::PROC, &[Operand::Nat4(0)]),
                (Opcode::PUSHVAL1, &[]),
                // Case offset should be patched
                (Opcode::CASE, &[Operand::Offset(21 * 4)]), // to after all instructions
                // - Arm 0
                (Opcode::PUSHVAL0, &[]),
                (Opcode::PUSHVAL1, &[]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::JUMP, &[Operand::Offset(13 * 4)]),
                // - Arm 1
                (Opcode::PUSHVAL1, &[]),
                (Opcode::PUSHVAL1, &[]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::JUMP, &[Operand::Offset(8 * 4)]),
                // - Arm 2
                (Opcode::PUSHVAL1, &[]),
                (Opcode::PUSHVAL0, &[]),
                (Opcode::ADDINTNAT, &[]),
                (Opcode::JUMP, &[Operand::Offset(3 * 4)]),
                // - Default Branch
                (Opcode::JUMP, &[Operand::Offset(1 * 4)]),
                (Opcode::RETURN, &[]),
                // Procedures should always end with a ABORT NoResult
                (
                    Opcode::ABORT,
                    &[Operand::AbortReason(AbortReason::NoResult)],
                ),
            ],
        );

        // Ensure case tables were resolved
        assert_eq!(procedure.case_tables.len(), 1);

        let case_table = &procedure.case_tables[0];
        assert_eq!(case_table.lower_bound, 1);
        assert_eq!(case_table.upper_bound, 3);
        assert_eq!(case_table.default_branch, 16 * 4);
        assert_eq!(&*case_table.arm_branches, &[1 * 4, 6 * 4, 11 * 4]);
    }

    #[test]
    fn build_procedure_with_multiple_case_tables() {
        let mut bytecode = BytecodeBuilder::new();
        let mut unit = bytecode.build_code_unit("<No File>");
        let mut procedure = unit.build_procedure();

        // Case Table 0
        procedure.ins().pushval0();
        {
            let case_instr = procedure.ins().case(0);
            let common_branch = procedure.make_unanchored_label();
            let mut table_builder = CaseTableBuilder::new();

            // Arms 0-2
            for i in 0..3 {
                {
                    table_builder.add_arm(CaseRange::SingleU32(i), procedure.make_label());
                    procedure.branch(|ins| ins.jump(0), common_branch);
                }
            }

            let default_branch = procedure.make_label();
            {
                procedure.branch(|ins| ins.jump(0), common_branch);
            }

            procedure.add_case_table(table_builder.finish(case_instr, default_branch));

            procedure.anchor_label(common_branch, None);
        }

        // Case Table 1
        procedure.ins().pushval1();
        {
            let case_instr = procedure.ins().case(0);
            let common_branch = procedure.make_unanchored_label();
            let mut table_builder = CaseTableBuilder::new();

            // Arms 0-4
            for i in 0..5 {
                {
                    table_builder.add_arm(CaseRange::SingleU32(i), procedure.make_label());
                    procedure.branch(|ins| ins.jump(0), common_branch);
                }
            }

            let default_branch = procedure.make_label();
            {
                procedure.branch(|ins| ins.jump(0), common_branch);
            }

            procedure.add_case_table(table_builder.finish(case_instr, default_branch));

            procedure.anchor_label(common_branch, None);
        }
        procedure.ins().return_();

        let procedure = procedure.finish();

        assert_eq!(procedure.id().as_usize(), 0);
        assert_match_instrs(
            procedure.instrs(),
            &[
                // Procedures should always start with a PROC xxx
                (Opcode::PROC, &[Operand::Nat4(0)]),
                (Opcode::PUSHVAL0, &[]),
                // Case Table 0:
                // Case offset should be patched to after all instructions
                (Opcode::CASE, &[Operand::Offset(27 * 4)]),
                // - Arm 0
                (Opcode::JUMP, &[Operand::Offset(7 * 4)]),
                // - Arm 1
                (Opcode::JUMP, &[Operand::Offset(5 * 4)]),
                // - Arm 2
                (Opcode::JUMP, &[Operand::Offset(3 * 4)]),
                // - Default Branch
                (Opcode::JUMP, &[Operand::Offset(1 * 4)]),
                (Opcode::PUSHVAL1, &[]),
                // Case Table 1:
                // Case offset should be patchedt o after all instructions plus after case table 0
                // (which is 3 * 4 (header) + 3 arms * 4)
                (Opcode::CASE, &[Operand::Offset(16 * 4 + (3 * 4 + 3 * 4))]),
                // - Arm 0
                (Opcode::JUMP, &[Operand::Offset(11 * 4)]),
                // - Arm 1
                (Opcode::JUMP, &[Operand::Offset(9 * 4)]),
                // - Arm 2
                (Opcode::JUMP, &[Operand::Offset(7 * 4)]),
                // - Arm 3
                (Opcode::JUMP, &[Operand::Offset(5 * 4)]),
                // - Arm 4
                (Opcode::JUMP, &[Operand::Offset(3 * 4)]),
                // - Default Branch
                (Opcode::JUMP, &[Operand::Offset(1 * 4)]),
                (Opcode::RETURN, &[]),
                // Procedures should always end with a ABORT NoResult
                (
                    Opcode::ABORT,
                    &[Operand::AbortReason(AbortReason::NoResult)],
                ),
            ],
        );

        // Ensure case tables were resolved
        assert_eq!(procedure.case_tables.len(), 2);

        let case_table = &procedure.case_tables[0];
        assert_eq!(case_table.lower_bound, 0);
        assert_eq!(case_table.upper_bound, 2);
        assert_eq!(case_table.default_branch, 7 * 4);
        assert_eq!(&*case_table.arm_branches, &[1 * 4, 3 * 4, 5 * 4]);

        let case_table = &procedure.case_tables[1];
        assert_eq!(case_table.lower_bound, 0);
        assert_eq!(case_table.upper_bound, 4);
        assert_eq!(case_table.default_branch, 11 * 4);
        assert_eq!(
            &*case_table.arm_branches,
            &[1 * 4, 3 * 4, 5 * 4, 7 * 4, 9 * 4]
        );
    }

    #[test]
    fn test_code_unit_with_same_unit_relocs() {
        let (proc_1, reloc_1_2, reloc_1_3);
        let (proc_2, reloc_2_3, reloc_2_3_again);
        let proc_3;
        let unit_1;

        let mut bytecode = BytecodeBuilder::new();

        // Step 1: Build skeleton
        let mut unit = bytecode.build_code_unit("<No File>");

        // procedure 1
        {
            let mut procedure = unit.build_procedure();

            // call procedure 2
            reloc_1_2 = procedure.reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));
            // We let signature handling compute the size of the argument stack for us.
            procedure.ins().call(0);

            // call procedure 3
            reloc_1_3 = procedure.reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));
            // We let signature handling compute the size of the argument stack for us.
            procedure.ins().call(0);

            procedure.ins().return_();

            proc_1 = procedure.id();
            unit.submit_procedure(procedure.finish());
        }

        // procedure 2
        {
            let mut procedure = unit.build_procedure();

            // call procedure 3
            reloc_2_3 = procedure.reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));
            // We let signature handling compute the size of the argument stack for us.
            procedure.ins().call(0);
            //
            // call procedure 3 again to test out offset intraprocedure reloc links
            reloc_2_3_again = procedure.reloc(|ins| ins.pushaddr1(RelocatableOffset::empty()));
            // We let signature handling compute the size of the argument stack for us.
            procedure.ins().call(0);

            procedure.ins().return_();

            proc_2 = procedure.id();
            unit.submit_procedure(procedure.finish());
        }

        // procedure 3
        {
            let mut procedure = unit.build_procedure();
            procedure.ins().return_();

            proc_3 = procedure.id();
            unit.submit_procedure(procedure.finish());
        }
        unit_1 = unit.id();
        bytecode.submit_code_unit(unit.finish());

        // Step 2: Build reloc targets
        let relocs = vec![
            (
                proc_1,
                reloc_1_2,
                RelocTarget::Local(bytecode[unit_1].procedure_offset(proc_2)),
            ),
            (
                proc_1,
                reloc_1_3,
                RelocTarget::Local(bytecode[unit_1].procedure_offset(proc_3)),
            ),
            (
                proc_2,
                reloc_2_3,
                RelocTarget::Local(bytecode[unit_1].procedure_offset(proc_3)),
            ),
            (
                proc_2,
                reloc_2_3_again,
                RelocTarget::Local(bytecode[unit_1].procedure_offset(proc_3)),
            ),
        ];
        bytecode[unit_1].add_code_relocs(relocs);

        // Step 3: Finish blob
        let bytecode = bytecode.finish();

        let unit = &bytecode[unit_1];

        // Procedure 1, at offset 1 * 4
        {
            let procedure = &unit[proc_1];

            assert_eq!(procedure.id().as_usize(), 0);
            assert_eq!(unit.procedure_offset(proc_1), SectionOffset::Code(0x4));
            assert_match_instrs(
                procedure.instrs(),
                &[
                    // Procedures should always start with a PROC xxx
                    (Opcode::PROC, &[Operand::Nat4(0)]),
                    // Relocatable offsets to procedure should be patched with offset and link
                    // `link` is relative to the base of the code table, not to other operands
                    //
                    // List goes from higher addresses to lower addresses, but this is not a
                    // strict requirement.
                    // Last relocatable offset in a list should have link offset 0
                    (
                        Opcode::PUSHADDR1,
                        &[Operand::RelocatableOffset(RelocatableOffset::new(
                            0,
                            16 * 4,
                        ))],
                    ),
                    (Opcode::CALL, &[Operand::Offset(0)]),
                    (
                        Opcode::PUSHADDR1,
                        &[Operand::RelocatableOffset(RelocatableOffset::new(
                            4 * 4,
                            31 * 4,
                        ))],
                    ),
                    (Opcode::CALL, &[Operand::Offset(0)]),
                    (Opcode::RETURN, &[]),
                    // Procedures should always end with a ABORT NoResult
                    (
                        Opcode::ABORT,
                        &[Operand::AbortReason(AbortReason::NoResult)],
                    ),
                ],
            );
        }

        // Procedure 2, at offset 16 * 4
        {
            let procedure = &unit[proc_2];

            assert_eq!(procedure.id().as_usize(), 1);
            assert_eq!(unit.procedure_offset(proc_2), SectionOffset::Code(16 * 4));
            assert_match_instrs(
                procedure.instrs(),
                &[
                    // Procedures should always start with a PROC xxx
                    (Opcode::PROC, &[Operand::Nat4(0)]),
                    // Next patch link should point into the previous procedure
                    (
                        Opcode::PUSHADDR1,
                        &[Operand::RelocatableOffset(RelocatableOffset::new(
                            9 * 4,
                            31 * 4,
                        ))],
                    ),
                    (Opcode::CALL, &[Operand::Offset(0)]),
                    // First patch of the list, should point to within the same procedure
                    (
                        Opcode::PUSHADDR1,
                        &[Operand::RelocatableOffset(RelocatableOffset::new(
                            19 * 4,
                            31 * 4,
                        ))],
                    ),
                    (Opcode::CALL, &[Operand::Offset(0)]),
                    (Opcode::RETURN, &[]),
                    // Procedures should always end with a ABORT NoResult
                    (
                        Opcode::ABORT,
                        &[Operand::AbortReason(AbortReason::NoResult)],
                    ),
                ],
            );
        }

        // Procedure 3, at offset 31 * 4
        {
            let procedure = &unit[proc_3];

            assert_eq!(procedure.id().as_usize(), 2);
            assert_eq!(unit.procedure_offset(proc_3), SectionOffset::Code(31 * 4));
            assert_match_instrs(
                procedure.instrs(),
                &[
                    // Procedures should always start with a PROC xxx
                    (Opcode::PROC, &[Operand::Nat4(0)]),
                    (Opcode::RETURN, &[]),
                    // Procedures should always end with a ABORT NoResult
                    (
                        Opcode::ABORT,
                        &[Operand::AbortReason(AbortReason::NoResult)],
                    ),
                ],
            );
        }

        // head should be at first entry in the list
        // (which tends to be closer to the end of the code section)
        assert_eq!(unit.local_relocs_start(UnitSection::Code), Some(24 * 4));
    }
}
