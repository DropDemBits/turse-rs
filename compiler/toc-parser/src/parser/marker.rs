//! Marker stuff
use super::Parser;
use crate::event::Event;

use drop_bomb::DropBomb;
use toc_syntax::SyntaxKind;

/// A marker for starting a new node
pub(crate) struct Marker {
    pos: usize,
    child_pos: Option<usize>,
    bomb: DropBomb,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Marker {
            pos,
            child_pos: None,
            bomb: DropBomb::new("Incomplete marker, missing matching `complete` or `forget` call"),
        }
    }

    /// Finishes a `Marker` for the given `kind`, converting it into a `CompletedMarker`
    pub(crate) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();

        if let Some(child_pos) = self.child_pos {
            // If there's a child node, modify the original starting node
            if let Event::StartNode { forward_parent, .. } = &mut parser.events[child_pos] {
                // forward_parent is an offset to this node
                *forward_parent = Some(self.pos - child_pos);
            } else {
                unreachable!()
            }
        }

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

    /// Abandons the marker, disposing of the past node
    ///
    /// # Deprecated
    /// The Parser API is being simplified, but is currently in flux.
    /// Once all nodes are parsed, all deprecated items will be removed
    #[deprecated = "should try to migrate the marker start instead"]
    #[allow(unused)]
    pub(crate) fn forget(mut self, parser: &mut Parser) {
        self.bomb.defuse();

        // Replace placeholder with `Tombstone`
        let event_at_pos = &mut parser.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);
        *event_at_pos = Event::Tombstone;
    }

    fn with_child(mut self, child_at: usize) -> Self {
        self.child_pos = Some(child_at);
        self
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
        // Modifying the child node is deferred to `complete`, allowing
        // a `precede` to be forgotten

        parser.start().with_child(self.pos)
    }
}
