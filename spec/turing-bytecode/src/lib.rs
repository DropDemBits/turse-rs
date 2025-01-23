// Instruction Decoder & Encoder: operates on single instructions
// Block Encoder & Decoder: operates on blocks of instructions, storing offset handles
// before writing since we store instructions, can writeback real offsets
//
// InstructionDecoder -> BlockDecoder
// BlockWriter <- InstructionEncoder?

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

        unimplemented!();
    }
}
