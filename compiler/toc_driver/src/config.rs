/*
 - Selecting what to dump (`--dump [kind]`)
 - Lint mode (`--lint-only`)
 - Changing reporting output:
   - normal
   - json (in the future)
   - legacy (for adapting to OpenTuring-QT editor)
*/

/// Compiler interface for the Turing language
#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    /// Only report warnings and errors, don't generate a compiled file
    #[clap(long)]
    pub lint: bool,

    /// Optionally dump internal data structure info
    #[clap(long, arg_enum)]
    pub dump: Option<DumpMode>,

    /// Change the reporting format
    #[clap(long, arg_enum)]
    pub report_format: Option<ReportFormat>,

    /// File to start compiling from
    pub source_file: String, // FIXME: Should probably be a camino::PathBuf
}

/// Which data structure to dump
#[derive(clap::ArgEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum DumpMode {
    /// AST/CST structures (only for the current file)
    Ast,
    /// HIR Trees, in line-based format
    Hir,
}

/// Format for report output
#[derive(clap::ArgEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportFormat {
    /// CLI report format, with color. This is the default format.
    Cli,
}

impl Default for ReportFormat {
    fn default() -> Self {
        Self::Cli
    }
}
