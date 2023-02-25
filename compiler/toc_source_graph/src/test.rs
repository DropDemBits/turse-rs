use toc_span::FileId;

use crate::{ArtifactKind, SourceGraph, SourceLibrary};

#[test]
fn no_dedup_source_roots() {
    let mut source_graph = SourceGraph::new();
    source_graph.add_library(SourceLibrary {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::dummy(3),
    });
    source_graph.add_library(SourceLibrary {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::dummy(2),
    });
    source_graph.add_library(SourceLibrary {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::dummy(2),
    });
    source_graph.add_library(SourceLibrary {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::dummy(1),
    });

    // Don't dedup roots
    assert_eq!(
        source_graph
            .all_libraries()
            .map(|(_, lib)| lib.root)
            .collect::<Vec<_>>(),
        vec![
            FileId::dummy(3),
            FileId::dummy(2),
            FileId::dummy(2),
            FileId::dummy(1),
        ]
    );
}
