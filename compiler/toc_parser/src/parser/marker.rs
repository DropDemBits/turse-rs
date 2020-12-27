//! Marker stuff

use crate::parser::event::Event;
use crate::parser::Parser;
use crate::syntax::SyntaxKind;

pub(super) struct Marker {
    pos: usize,
    completed: bool, // could use `drop_bomb`, but we want to minimize deps for now
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Marker {
            pos,
            completed: false,
        }
    }

    /// Finishes a `Marker` for the given `kind`, converting it into a `CompletedMarker`
    pub(super) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;

        // Replace placeholder with actual starting node
        let event_at_pos = &mut parser.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);
        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        // Push corresponding finish node
        parser.events.push(Event::FinishNode);

        CompletedMarker { pos: self.pos }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed {
            panic!("Incomplete marker, missing matching `complete` call");
        }
    }
}

pub(super) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    /// Transforms a `CompletedMarker` back into a `Marker` for appending more tokens,
    /// or wrapping other syntax nodes
    pub(super) fn precede(self, parser: &mut Parser) -> Marker {
        let new_marker = parser.start();

        // Modify original starting node
        if let Event::StartNode { forward_parent, .. } = &mut parser.events[self.pos] {
            // forward_parent is an offset to the new_marker node
            *forward_parent = Some(new_marker.pos - self.pos);
        } else {
            unreachable!()
        }

        new_marker
    }
}
