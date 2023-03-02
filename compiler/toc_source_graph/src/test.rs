use toc_span::FileId;

use crate::{ArtifactKind, DependencyList, Library, RootLibraries};

#[derive(Default)]
#[salsa::db(crate::Jar)]
struct TestDb {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDb {}

#[test]
fn no_dedup_source_roots() {
    let db = &mut TestDb::default();
    let roots = vec![
        FileId::dummy(3),
        FileId::dummy(2),
        FileId::dummy(2),
        FileId::dummy(1),
    ];
    let roots = roots
        .into_iter()
        .map(|root| {
            Library::new(
                db,
                "a".into(),
                root,
                ArtifactKind::Binary,
                DependencyList::empty(db),
            )
        })
        .collect::<Vec<_>>();

    // Set the
    RootLibraries::new(db, roots);

    // Don't dedup roots, since they're different libraries
    assert_eq!(
        crate::source_graph(db)
            .as_ref()
            .unwrap()
            .all_libraries(db)
            .iter()
            .map(|lib| lib.root(db))
            .collect::<Vec<_>>(),
        vec![
            FileId::dummy(3),
            FileId::dummy(2),
            FileId::dummy(2),
            FileId::dummy(1),
        ]
    );
}

// FIXME: add test for dep exploring
// FIXME: add test for cycle checking
