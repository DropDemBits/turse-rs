//! Compile context things
use toc_ast::unit::{CodeUnit, UnitId};
use toc_core::{MessageSource, ReportMessage};

use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard};

/// Status of compilation
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CompileStatus {
    /// Successful compilation
    Success,
    /// Invalid compilation
    Error,
}

#[derive(Debug)]
pub struct UnitInfo {
    pub path: String,
    pub source: String,
}

#[derive(Debug)]
pub struct SourceMap {
    /// Path to unit id
    path_to_units: HashMap<String, UnitId>,
    /// Sources and paths for the corresponding unit
    unit_infos: HashMap<UnitId, UnitInfo>,
    /// Next unit ic
    next_unit_id: u32,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            path_to_units: HashMap::new(),
            unit_infos: HashMap::new(),
            next_unit_id: 1,
        }
    }

    /// Adds a unit to the source map
    ///
    /// # Parameters
    /// - `path`: The path to the unit's source file
    /// - `source`: The contents of the unit's source file
    ///
    /// # Returns
    /// Returns the new `UnitId`
    pub fn add_unit(&mut self, path: impl AsRef<str>, source: String) -> UnitId {
        // Make a new unit id
        let unit_id: UnitId = UnitId::new(self.next_unit_id);
        // Should not have more than 4 billion units
        self.next_unit_id = self.next_unit_id.checked_add(1).unwrap();

        // Add information
        self.path_to_units.insert(path.as_ref().to_owned(), unit_id);
        self.unit_infos.insert(
            unit_id,
            UnitInfo {
                path: path.as_ref().to_owned(),
                source,
            },
        );

        unit_id
    }

    /// Gets the unit info for the given unit id
    pub fn get_unit_info(&self, id: UnitId) -> Option<&UnitInfo> {
        self.unit_infos.get(&id)
    }

    /// Tries to get the unit id from a given path
    pub fn unit_from_path(&self, path: &str) -> Option<UnitId> {
        self.path_to_units.get(path).copied()
    }
}

/// Current compilation session
pub struct CompileSession {
    compiled_units: HashMap<UnitId, CodeUnit>,
    compile_ctx: Option<Arc<CompileContext>>,
}

impl CompileSession {
    pub fn new() -> Self {
        Self {
            compiled_units: HashMap::new(),
            compile_ctx: None,
        }
    }

    /// Gets an iterator over all of the code units
    pub fn units(&self) -> impl Iterator<Item = (&UnitId, &CodeUnit)> {
        self.compiled_units.iter()
    }

    /// Gets the compile context after compilation.
    /// Must only be called after compilation.
    pub fn context(&self) -> &CompileContext {
        self.compile_ctx.as_ref().unwrap()
    }

    /// Starts compilation from the given source file
    ///
    /// # Parameters
    /// - `path`: The path to the initial file
    pub fn compile_source_file(
        &mut self,
        path: &str,
        only_parser: bool,
        mute_warnings: bool,
    ) -> (Option<UnitId>, CompileStatus) {
        use crate::{parser::Parser, scanner::Scanner, validator::Validator};
        use toc_ast::ast::VisitorMut;
        use toc_core::StatusReporter;

        // TODO: Replace below with a preprocessing stage
        let mut sources = SourceMap::new();

        // Load up the main file, adding it to the source map
        let source = match self.load_source_file(path) {
            Ok(source) => source,
            Err(_) => return (None, CompileStatus::Error),
        };
        let main_unit = sources.add_unit(path, source);
        let pending_compiles = vec![main_unit];

        // Make a new ctx
        let ctx = CompileContext::new(sources);
        let ctx = Arc::new(ctx);
        self.compile_ctx = Some(ctx.clone());

        // Spin up scanners & parsers to any pending paths

        for unit_id in pending_compiles {
            let source = &ctx
                .source_map()
                .get_unit_info(unit_id)
                .expect("invalid unit id")
                .source;
            let scanner = Scanner::scan_source(source);
            let mut parser: Parser = Parser::new(scanner, true, ctx.clone());
            parser.parse();
            ctx.aggregate_messages(&mut parser);

            let code_unit = parser.take_unit();
            self.compiled_units.insert(unit_id, code_unit);
        }

        if only_parser {
            // Only the parser stage is to be run
            // Report status messages
            let has_errors = StatusReporter::report_messages(ctx.messages().iter(), mute_warnings);

            let status = match !has_errors {
                true => CompileStatus::Success,
                false => CompileStatus::Error,
            };

            return (Some(main_unit), status);
        }

        // TODO: Provide inter-unit type resolution stage

        // Run AST validator stage
        for (_, code_unit) in &mut self.compiled_units {
            let mut validator = Validator::new(
                &mut code_unit.unit_scope,
                &mut code_unit.type_table,
                ctx.clone(),
            );

            validator.visit_stmt(&mut code_unit.root_stmt);
            ctx.aggregate_messages(&mut validator);
        }

        // Report status messages
        let has_errors = StatusReporter::report_messages(ctx.messages().iter(), mute_warnings);

        let status = match !has_errors {
            true => CompileStatus::Success,
            false => CompileStatus::Error,
        };

        (Some(main_unit), status)
    }

    /// Loads the file at the given path
    ///
    /// # Parameters
    /// - `path`: The path to the given file. Can have a `%oot` prefix to indicate
    ///           that the path is relative to the predefs directory
    fn load_source_file(&mut self, path: &str) -> std::io::Result<String> {
        // TODO: Resolve path into a real path (fully normalized, without %oot)

        // Load file source
        let source = std::fs::read_to_string(path)?;

        Ok(source)
    }
}

/// Context for the current compilation session
#[derive(Debug)]
pub struct CompileContext {
    // message aggregator, unit source map, etc.
    /// All aggregated messages
    messages: Mutex<Vec<ReportMessage>>,
    /// Source map for all units
    source_map: SourceMap,
}

impl CompileContext {
    pub fn new(source_map: SourceMap) -> Self {
        Self {
            messages: Mutex::new(vec![]),
            source_map,
        }
    }

    /// Takes reported messages from the given message source
    pub fn aggregate_messages(&self, message_source: &mut impl MessageSource) {
        self.messages
            .lock()
            .unwrap()
            .append(&mut message_source.take_reported_messages())
    }

    /// Gives a reference to all of the aggregated messages
    pub fn messages(&self) -> MutexGuard<Vec<ReportMessage>> {
        self.messages.lock().unwrap()
    }

    /// Gives a reference to the unit source maps
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }
}
