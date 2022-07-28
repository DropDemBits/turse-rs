use toc_span::FileId;

use crate::SourceGraph;

#[test]
fn dedup_source_roots() {
    let mut source_graph = SourceGraph::new();
    source_graph.add_root(FileId::new_testing(3).unwrap());
    source_graph.add_root(FileId::new_testing(2).unwrap());
    source_graph.add_root(FileId::new_testing(2).unwrap());
    source_graph.add_root(FileId::new_testing(1).unwrap());

    // Should dedup roots, preserving insert order
    assert_eq!(
        source_graph.library_roots().collect::<Vec<_>>(),
        vec![
            FileId::new_testing(3).unwrap(),
            FileId::new_testing(2).unwrap(),
            FileId::new_testing(1).unwrap(),
        ]
    );
}
