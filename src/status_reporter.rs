//! Common status reporting facility
use crate::compiler::Location;
use std::cell::Cell;
use std::fmt::{self, Arguments, Display, Formatter};

#[allow(dead_code)]
pub enum ReportKind {
    Error,
    Warning,
}

impl Display for ReportKind {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            ReportKind::Error => f.write_str("error"),
            ReportKind::Warning => f.write_str("warn"),
        }
    }
}

struct ReportMessage<'a> {
    kind: ReportKind,
    at: Location,
    message: Arguments<'a>,
}

/// Common status reporter
#[derive(Debug)]
pub struct StatusReporter {
    /// If the reporter has reported an error
    has_error: Cell<bool>,
}

impl StatusReporter {
    pub fn new() -> Self {
        Self {
            has_error: Cell::new(false),
        }
    }

    fn report_at(&self, reporting: ReportMessage) {
        let end_column = reporting.at.column + reporting.at.width;
        eprintln!(
            "{} line:{} column:{}-{} {}",
            reporting.kind, reporting.at.line, reporting.at.column, end_column, reporting.message
        );
    }

    pub fn report_error(&self, at: &Location, message: Arguments) {
        self.report_at(ReportMessage {
            kind: ReportKind::Error,
            at: at.clone(),
            message,
        });

        self.has_error.set(true);
    }

    #[allow(dead_code)]
    pub fn report_warning(&self, at: &Location, message: Arguments) {
        self.report_at(ReportMessage {
            kind: ReportKind::Warning,
            at: at.clone(),
            message,
        });
    }

    pub fn has_error(&self) -> bool {
        self.has_error.get()
    }
}
