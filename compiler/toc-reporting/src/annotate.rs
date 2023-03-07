//! Source annotations

use std::fmt;

use toc_span::Span;

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
pub struct SourceAnnotation {
    pub(crate) annotation: Annotation,
    pub(crate) span: Span,
}

impl SourceAnnotation {
    /// Gets the kind of annotation reported
    pub fn kind(&self) -> AnnotateKind {
        self.annotation.kind()
    }

    /// Gets the annotation's message
    pub fn message(&self) -> &str {
        self.annotation.message()
    }

    /// Gets the span of text the annotation covers.
    pub fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for SourceAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Annotation { kind, msg } = &self.annotation;
        let (file_id, range) = self.span.into_parts().expect("missing location info");
        let (start, end) = (u32::from(range.start()), u32::from(range.end()));

        write!(f, "{kind} in file {file_id:?}")?;

        if f.alternate() {
            write!(f, " for {start}..{end}: {msg}")?;
        } else {
            write!(f, " at {start}..{end}: {msg}")?;
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