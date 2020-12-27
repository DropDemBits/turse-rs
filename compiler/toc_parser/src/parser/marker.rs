//! Marker stuff
use super::Parser;
use crate::event::Event;

use drop_bomb::DropBomb;
use toc_syntax::SyntaxKind;

pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Marker {
            pos,
            bomb: DropBomb::new("Incomplete marker, missing matching `complete` call"),
        }
    }

    /// Finishes a `Marker` for the given `kind`, converting it into a `CompletedMarker`
    pub(crate) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();

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

pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    /// Transforms a `CompletedMarker` back into a `Marker` for appending more tokens,
    /// or wrapping other syntax nodes
    pub(crate) fn precede(self, parser: &mut Parser) -> Marker {
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
