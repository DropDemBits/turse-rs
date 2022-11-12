use toc_span::FileId;

use crate::{ArtifactKind, Library, SourceGraph};

#[test]
fn no_dedup_source_roots() {
    let mut source_graph = SourceGraph::new();
    source_graph.add_library(Library {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::new_testing(3).unwrap(),
    });
    source_graph.add_library(Library {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::new_testing(2).unwrap(),
    });
    source_graph.add_library(Library {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::new_testing(2).unwrap(),
    });
    source_graph.add_library(Library {
        artifact: ArtifactKind::Binary,
        name: "a".into(),
        root: FileId::new_testing(1).unwrap(),
    });

    // Don't dedup roots
    assert_eq!(
        source_graph
            .all_libraries()
            .map(|(_, lib)| lib.root)
            .collect::<Vec<_>>(),
        vec![
            FileId::new_testing(3).unwrap(),
            FileId::new_testing(2).unwrap(),
            FileId::new_testing(2).unwrap(),
            FileId::new_testing(1).unwrap(),
        ]
    );
}
