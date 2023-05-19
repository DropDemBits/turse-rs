//! Source annotations

use std::fmt;

use crate::Location;

/// Type of annotation added to a message
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnnotateKind {
    /// Information related to the main message.
    /// May be context specific.
    Note,
    /// More detailed related to the message.
    Info,
    /// Warning annotation
    Warning,
    /// Error annotation
    Error,
}

impl fmt::Display for AnnotateKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            AnnotateKind::Note => "note",
            AnnotateKind::Info => "info",
            AnnotateKind::Warning => "warn",
            AnnotateKind::Error => "error",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceAnnotation<L: Location> {
    pub(crate) annotation: Annotation,
    pub(crate) span: L,
}

impl<L: Location> SourceAnnotation<L> {
    /// Gets the kind of annotation reported
    pub fn kind(&self) -> AnnotateKind {
        self.annotation.kind()
    }

    /// Gets the annotation's message
    pub fn message(&self) -> &str {
        self.annotation.message()
    }

    /// Gets the span of text the annotation covers.
    pub fn span(&self) -> L {
        self.span
    }
}

impl<L: Location> fmt::Display for SourceAnnotation<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Annotation { kind, msg } = &self.annotation;
        let loc = self.span();

        if f.alternate() {
            write!(f, "{kind} for {loc:?}: {msg}")?;
        } else {
            write!(f, "{kind} at {loc:?}: {msg}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    pub(crate) kind: AnnotateKind,
    pub(crate) msg: String,
}

impl Annotation {
    /// Gets the kind of annotation reported
    pub fn kind(&self) -> AnnotateKind {
        self.kind
    }

    /// Gets the annotation's message
    pub fn message(&self) -> &str {
        &self.msg
    }
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind(), self.msg)
    }
}
