use std::{
    collections::BTreeMap,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

use instruction::{AbortReason, Instruction, Opcode, Operand, OperandRef};

mod instruction;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelocHandle(usize);

// What (potential) section is the relocation meant to referring to.
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RelocDisposition {
    /// Relocation refers to the same translation unit.
    /// This is the default disposition.
    #[default]
    Local,
    /// Relocation refers to a different translation unit.
    External,
    /// Relocation may or may not refer to the same translation unit, and will
    /// be resolved at a later point.
    Unknown,
}

// Process
// - Build individual procedures
//   - build side table of reloc targets
// - Aggregate procedures into a TU
pub struct BytecodeBuilder {
    // need:
    // - env options
    // - unit ref gen
    next_unit: u32,
    units: BTreeMap<CodeUnitRef, CodeUnit>,
}

impl BytecodeBuilder {
    pub fn new() -> Self {
        Self {
            next_unit: 1,
            units: BTreeMap::new(),
        }
    }

    pub fn build_code_unit(&mut self, file_name: &str) -> CodeUnitBuilder {
        let id = NonZeroU32::new(self.next_unit).map(CodeUnitRef).unwrap();
        self.next_unit += 1;

        CodeUnitBuilder::new(id, file_name)
    }

    pub fn submit_code_unit(&mut self, unit: CodeUnit) {
        let id = unit.id();
        let None = self.units.insert(id, unit) else {
            panic!("inserting duplicate code unit {id:?}");
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct CodeUnitRef(NonZeroU32);

impl CodeUnitRef {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
pub struct CodeUnitBuilder {
    // need:
    // - unit ref (for inter-unit referral)
    id: CodeUnitRef,
    // - file name
    file_name: Box<str>,
    // - procedure ref gen
    next_procedure: u32,
    procedures: BTreeMap<ProcedureRef, Procedure>,
}

impl CodeUnitBuilder {
    fn new(id: CodeUnitRef, file_name: &str) -> Self {
        Self {
            id,
            file_name: file_name.to_owned().into_boxed_str(),
            next_procedure: 1,
            procedures: BTreeMap::new(),
        }
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

    /// Finishes the builder to produce a sealed [`CodeUnit`].
    pub fn finish(self) -> CodeUnit {
        let Self {
            id,
            file_name,
            next_procedure: _,
            procedures,
        } = self;

        CodeUnit {
            id,
            file_name,
            procedures,
        }
    }
}

#[derive(Debug)]
pub struct CodeUnit {
    // need:
    // - unit ref
    id: CodeUnitRef,
    // - file name
    file_name: Box<str>,
    // - procedures
    procedures: BTreeMap<ProcedureRef, Procedure>,
}

impl CodeUnit {
    pub fn id(&self) -> CodeUnitRef {
        self.id
    }

    pub fn file_name(&self) -> &str {
        &self.file_name
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ProcedureRef(NonZeroU32);

impl ProcedureRef {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
pub struct ProcedureBuilder {
    // need:
    // - procedure ref (for building up reloc target side-table)
    id: ProcedureRef,
    // - local (needs to know size up-front for PROC)
    locals: LocalAllocator,
    // - temporary (for longer lived or larger sized temporary storage)
    temps: TemporaryAllocator,
    // - relocs
    // - jump labels
    // - operand patches
    //   - for proc size
    //     - for proc
    //     - for locate temp
    //   - for temporary slots
    //   - for jump labels
    local_patches: Vec<(InstructionRef, OperandRef, PatchWith)>,
    //   - for relocs (deferred)
    reloc_patches: Vec<(InstructionRef, OperandRef, RelocHandle)>,
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
            instrs,
        }
    }

    pub fn id(&self) -> ProcedureRef {
        self.id
    }

    pub fn alloc_temporary(&mut self, size: u32, align: u32) -> TemporarySlot {
        self.temps.alloc(size, align)
    }

    pub fn alloc_local(&mut self, name: Option<&str>, size: u32, align: u32) -> LocalSlot {
        self.locals.alloc(name, size, align)
    }

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

    pub fn locate_local(&mut self, slot: LocalSlot) {
        let locate_local = self.instrs.locatelocal(0);
        let local_offset = self.instrs[locate_local].operand_refs().nth(0).unwrap();

        self.local_patches
            .push((locate_local, local_offset, PatchWith::LocalSlot(slot)));
    }

    pub fn in_temporary_scope(&mut self, in_scope: impl FnOnce(&mut Self)) {
        let old = self.temps.enter_scope();
        in_scope(self);
        self.temps.leave_scope(old);
    }

    pub fn ins(&mut self) -> &mut InstructionEncoder {
        &mut self.instrs
    }

    pub fn finish(self) -> Procedure {
        let Self {
            id,
            locals,
            temps,
            local_patches,
            reloc_patches,
            mut instrs,
        } = self;

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

        // Apply local patches
        for (instr, operand, patch) in local_patches {
            instrs[instr][operand] = match patch {
                PatchWith::FrameSize => Operand::Nat4(frame_size),
                PatchWith::LocalSlot(slot) => Operand::Offset(locals_area[slot]),
                PatchWith::TemporarySlot(slot) => Operand::Offset(temp_area[slot]),
            }
        }

        // All procedures should end with an abort with no result
        instrs.abort(AbortReason::NoResult);

        let instrs = instrs.finish();
        Procedure {
            id,
            instrs,
            reloc_patches,
        }
    }
}

#[derive(Debug)]
pub struct Procedure {
    // store:
    // - procedure ref (for resolving reloc target side-table)
    id: ProcedureRef,
    // - instruction list
    instrs: Box<[Instruction]>,
    // - operand patches
    //   - for relocs
    reloc_patches: Vec<(InstructionRef, OperandRef, RelocHandle)>,
}

impl Procedure {
    pub fn id(&self) -> ProcedureRef {
        self.id
    }

    pub fn instrs(&self) -> &[Instruction] {
        &self.instrs
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
pub struct TemporaryInfo {
    pub scope: TemporaryScope,
    pub size: u32,
    pub align: u32,
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
    pub fn alloc(&mut self, size: u32, align: u32) -> TemporarySlot {
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
    pub fn enter_scope(&mut self) -> TemporaryScope {
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
    pub fn leave_scope(&mut self, old_scope: TemporaryScope) {
        let TemporaryScope { depth, scope } = old_scope;

        self.depth = depth;
        self.scope = scope;
    }

    /// Finalizes the offsets of temporary slots.
    pub fn finish(&self) -> TemporariesArea {
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

pub struct TemporariesArea {
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
pub struct TemporaryScope {
    depth: u32,
    scope: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TemporarySlot(NonZeroU32);

impl TemporarySlot {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
pub struct LocalAllocator {
    slots: Vec<LocalInfo>,
}

#[derive(Debug)]
pub struct LocalInfo {
    pub name: Option<Box<str>>,
    pub size: u32,
    pub align: u32,
}

impl LocalAllocator {
    pub fn new() -> Self {
        Self { slots: vec![] }
    }

    pub fn alloc(&mut self, name: Option<&str>, size: u32, align: u32) -> LocalSlot {
        assert!(align > 0);
        assert!(align.is_power_of_two() && align <= 4);

        let slot = NonZeroU32::new(self.slots.len().saturating_add(1) as u32)
            .map(LocalSlot)
            .unwrap();
        let size = size.next_multiple_of(align);

        self.slots.push(LocalInfo {
            name: name.map(|it| it.to_owned().into_boxed_str()),
            size,
            align,
        });
        slot
    }

    pub fn finish(self) -> LocalsArea {
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

pub struct LocalsArea {
    size: u32,
    offsets: Box<[u32]>,
}

impl LocalsArea {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LocalSlot(NonZeroU32);

impl LocalSlot {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

#[derive(Debug)]
pub struct InstructionEncoder {
    instrs: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct InstructionRef(NonZeroU32);

impl InstructionRef {
    pub fn as_usize(self) -> usize {
        self.0.get().saturating_sub(1) as usize
    }
}

impl InstructionEncoder {
    pub fn new() -> Self {
        Self { instrs: vec![] }
    }

    pub fn finish(self) -> Box<[Instruction]> {
        self.instrs.into_boxed_slice()
    }

    fn add(&mut self, instr: Instruction) -> InstructionRef {
        let slot = NonZeroU32::new(self.instrs.len().saturating_add(1) as u32)
            .map(InstructionRef)
            .unwrap();
        self.instrs.push(instr);
        slot
    }
}

impl Index<InstructionRef> for InstructionEncoder {
    type Output = Instruction;

    fn index(&self, index: InstructionRef) -> &Self::Output {
        &self.instrs[index.0.get().saturating_sub(1) as usize]
    }
}

impl IndexMut<InstructionRef> for InstructionEncoder {
    fn index_mut(&mut self, index: InstructionRef) -> &mut Self::Output {
        &mut self.instrs[index.0.get().saturating_sub(1) as usize]
    }
}

// this part will be generated
impl InstructionEncoder {
    pub fn abort(&mut self, abort_kind: AbortReason) -> InstructionRef {
        self.add(Instruction::new(Opcode::ABORT).with_operand(Operand::AbortReason(abort_kind)))
    }

    pub fn addintnat(&mut self) -> InstructionRef {
        self.add(Instruction::new(Opcode::ADDINTNAT))
    }

    pub fn locatelocal(&mut self, local_offset: u32) -> InstructionRef {
        self.add(Instruction::new(Opcode::LOCATELOCAL).with_operand(Operand::Offset(local_offset)))
    }

    pub fn locatetemp(&mut self, locals_size: u32, temp_offset: u32) -> InstructionRef {
        self.add(
            Instruction::new(Opcode::LOCATETEMP)
                .with_operand(Operand::Nat4(locals_size))
                .with_operand(Operand::Offset(temp_offset)),
        )
    }

    pub fn proc(&mut self, locals_size: u32) -> InstructionRef {
        self.add(Instruction::new(Opcode::PROC).with_operand(Operand::Nat4(locals_size)))
    }

    pub fn return_(&mut self) -> InstructionRef {
        self.add(Instruction::new(Opcode::RETURN))
    }
}

#[derive(Debug)]
enum PatchWith {
    FrameSize,
    LocalSlot(LocalSlot),
    TemporarySlot(TemporarySlot),
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // adds some instructions
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
        // Procedures should always start with a PROC xxx
        assert_eq!(procedure.instrs()[0].opcode(), Opcode::PROC);
        // Frame size should be 8
        assert_eq!(
            procedure.instrs()[0].operands().nth(0),
            Some(&Operand::Nat4(8))
        );
        assert_eq!(procedure.instrs()[1].opcode(), Opcode::LOCATELOCAL);
        // Local offsets should be patched
        assert_eq!(
            procedure.instrs()[1].operands().nth(0),
            Some(&Operand::Offset(0))
        );
        assert_eq!(procedure.instrs()[2].opcode(), Opcode::LOCATELOCAL);
        assert_eq!(
            procedure.instrs()[2].operands().nth(0),
            Some(&Operand::Offset(4))
        );

        assert_eq!(procedure.instrs()[3].opcode(), Opcode::ADDINTNAT);
        assert_eq!(procedure.instrs()[4].opcode(), Opcode::RETURN);

        // Procedures should always end with a ABORT NoResult
        assert_eq!(procedure.instrs()[5].opcode(), Opcode::ABORT);
    }

    #[test]
    fn build_procedure_with_locals_and_temps() {
        // adds some instructions
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
        // Procedures should always start with a PROC xxx
        assert_eq!(procedure.instrs()[0].opcode(), Opcode::PROC);
        // Frame size should be 16
        assert_eq!(
            procedure.instrs()[0].operands().nth(0),
            Some(&Operand::Nat4(16))
        );
        assert_eq!(procedure.instrs()[1].opcode(), Opcode::LOCATELOCAL);
        // Local offsets should be patched
        assert_eq!(
            procedure.instrs()[1].operands().nth(0),
            Some(&Operand::Offset(0))
        );
        assert_eq!(procedure.instrs()[2].opcode(), Opcode::LOCATELOCAL);
        assert_eq!(
            procedure.instrs()[2].operands().nth(0),
            Some(&Operand::Offset(4))
        );
        assert_eq!(procedure.instrs()[3].opcode(), Opcode::ADDINTNAT);

        assert_eq!(procedure.instrs()[4].opcode(), Opcode::LOCATETEMP);
        // Temporaries offsets should be patched
        assert_eq!(
            procedure.instrs()[4].operands().nth(0),
            Some(&Operand::Nat4(16)) // with the frame size
        );
        assert_eq!(
            procedure.instrs()[4].operands().nth(1),
            Some(&Operand::Offset(0)) // with the offset
        );

        assert_eq!(procedure.instrs()[5].opcode(), Opcode::ADDINTNAT);

        assert_eq!(procedure.instrs()[6].opcode(), Opcode::LOCATETEMP);
        assert_eq!(
            procedure.instrs()[6].operands().nth(0),
            Some(&Operand::Nat4(16)) // with the frame size
        );
        assert_eq!(
            procedure.instrs()[6].operands().nth(1),
            Some(&Operand::Offset(4)) // with the offset
        );

        assert_eq!(procedure.instrs()[7].opcode(), Opcode::ADDINTNAT);

        assert_eq!(procedure.instrs()[8].opcode(), Opcode::RETURN);

        // Procedures should always end with a ABORT NoResult
        assert_eq!(procedure.instrs()[9].opcode(), Opcode::ABORT);
    }
}
