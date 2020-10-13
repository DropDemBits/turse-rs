//! Compile context things
use toc_core::StatusReporter;

/// Context for the current compilation session
#[derive(Debug)]
pub struct CompileContext {
    // error reporting, unit source maps, etc.
    /// Error reporter for the context
    pub reporter: StatusReporter,
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            reporter: StatusReporter::new(),
        }
    }
}

impl Default for CompileContext {
    fn default() -> Self {
        Self::new()
    }
}
