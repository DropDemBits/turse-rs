//! Marker stuff
use super::Parser;
use crate::event::Event;

use drop_bomb::DropBomb;
use toc_syntax::SyntaxKind;

pub(crate) struct MaybeMarker(Marker);

impl MaybeMarker {
    pub(crate) fn new(pos: usize) -> Self {
        // use a custom message
        Self(Marker {
            pos,
            bomb: DropBomb::new("Incomplete marker, missing matching `complete` or `forget` call"),
        })
    }

    /// Abandons the marker, disposing of the past node
    pub(crate) fn forget(mut self, parser: &mut Parser) {
        self.0.bomb.defuse();

        // Replace placeholder with `Tombstone`
        let event_at_pos = &mut parser.events[self.0.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);
        *event_at_pos = Event::Tombstone;
    }

    /// Finishes a `Marker` for the given `kind`, converting it into a `CompletedMarker`
    pub(crate) fn complete(self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.0.complete(parser, kind)
    }

    /// Converts the `MaybeMarker` into a marker that must be used
    fn must_use(mut self) -> Marker {
        self.0.bomb.defuse();
        Marker::new(self.0.pos)
    }
}

/// A marker for starting a new node
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

impl From<MaybeMarker> for Marker {
    fn from(maybe: MaybeMarker) -> Self {
        maybe.must_use()
    }
}

/// A completed marker representing a finished node
///
/// Used to wrap a marker inside of another node
pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    /// Transforms a `CompletedMarker` back into a `Marker` for appending more tokens,
    /// or wrapping other syntax nodes
    pub(crate) fn precede(self, parser: &mut Parser) -> Marker {
        let new_marker = parser.start().must_use();

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
