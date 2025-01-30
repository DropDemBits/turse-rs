// Instruction Decoder & Encoder: operates on single instructions
// Block Encoder & Decoder: operates on blocks of instructions, storing offset handles
// before writing since we store instructions, can writeback real offsets
//
// InstructionDecoder -> BlockDecoder
// BlockWriter <- InstructionEncoder?
//
// problem:
// - in a Translation Unit, reloc handles are unique
// - block encoder should care about this?
//   - if we only have 1 block at a time, simplifies things
//
// we can let TU handle transforming operands, get feedback on locations to patch?
// or, blocks could record what thing we want to patch
//
// could also have offset be an opaque handle, and have a side table to figure out what the reloc points to
// - essentially a deferred bag of bytes
//
// Q: do we need exposed instruction repr?
// - asm would need its own since args can be from anywhere
// - decoder: by necessity since flow recovery is out of scope
// - flow-ir: no
//   - flow recovery definitely as a separate crate
//
// Q: when should we bin reloc offsets?
// code relocs are binned by
// local: (table,)
// external: (table, unit)
//
// it's only a matter of if we should expose it during instruction encoding time or during a later resolve pass
// - finding out whether something is TU local or external is easy
// - figuring out the TU number needs to be deferred, since might be asking for
//   a def from a later generated TU (assuming TUs uses items from each other)
// - figuring out whether something is code or not code is easy (proc, func, process refs)
// - figuring out whether something is manifest (rodata) or global (data) is tricky?
//   - something might be rodata hoisted? though by codegen time it would already be
//     determined to be hoisted, so this is a non-problem
//
// API Shape
// Encoding: cranelift-frontend like (higher level)
// Decoding: iced-x86 like (lower level, provides bare-bones info)
//
// builder hierarchy
// TU Builder -> Function Builder -> Block builder? -> Instruction builder?
// writer would also need to abstract location handling?
//
// Q: what's our scope?
// in scope
// - writing instructions as bytes
// - writing procedures
//   - by necessity since relocs are per TU, but having such a long-lived writer is impractical
//     - TU builder can still hand that out
//   - inter-TU relocs are... interesting
// - writing TUs
//   - by necessity since relocs are per TU
// - reading instructions as bytes
//   - reading single instructions
//     - inferring stack effect
//     - inferring instruction size?
//       - for finding undecoded regions
//   - redirecting decode (forward and backward a la JUMP and JUMPB)
//     - necessary for jumping over inline case tables and procedure definitions
//     - note: jump offset is relative to after operand offset
// - reading TUs
//   - decoding versions?
//     - by options size (some versions have the same bytecode size)
//     - by extern table (if PE attached)
//     - by externs present (assuming multi TU), necessary for OT vs HS 4.1.2
//
// out of scope
// - control flow graph recovery
//   - by extension, structured control flow graph recovery
//   - domain of a future flow ir
// - function decoder discovery
//   - requires partial flow recovery & (operand) stack analysis
//   - decode -> encode is not idempotent for bytecode files sourced from
//     HS/OpenTuring, since procedures are embedded inside of the defining
//     module's init body
//   - decode -> encode from toco or 1 pass is idempotent
// - undecoded region discovery
//   - as part of flow recovery & stack analysis
//   - need to keep track of already decoded regions

pub mod encode;

#[cfg(test)]
mod tests {
    use stackful_spec::BytecodeSpec;

    #[test]
    fn generate_from_spec() {
        let spec: BytecodeSpec = include_str!("./spec.kdl").parse().unwrap();

        // want: all instructions
        for instr in &spec.instructions {
            eprintln!("{instr:#?}");
        }

        // unimplemented!();
    }
}
