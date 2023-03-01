use toc_span::FileId;

use crate::{ArtifactKind, Library, SourceGraph};

#[derive(Default)]
#[salsa::db(crate::Jar)]
struct TestDb {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDb {}

#[test]
fn no_dedup_source_roots() {
    let mut db = &mut TestDb::default();
    let mut source_graph = SourceGraph::new();
    source_graph.add_library(Library::new(
        db,
        "a".into(),
        FileId::dummy(3),
        ArtifactKind::Binary,
    ));
    source_graph.add_library(Library::new(
        db,
        "a".into(),
        FileId::dummy(2),
        ArtifactKind::Binary,
    ));
    source_graph.add_library(Library::new(
        db,
        "a".into(),
        FileId::dummy(2),
        ArtifactKind::Binary,
    ));
    source_graph.add_library(Library::new(
        db,
        "a".into(),
        FileId::dummy(1),
        ArtifactKind::Binary,
    ));

    // Don't dedup roots
    assert_eq!(
        source_graph
            .all_libraries()
            .map(|(_, lib)| lib.root(db))
            .collect::<Vec<_>>(),
        vec![
            FileId::dummy(3),
            FileId::dummy(2),
            FileId::dummy(2),
            FileId::dummy(1),
        ]
    );
}
