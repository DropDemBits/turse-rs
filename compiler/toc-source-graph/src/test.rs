use toc_paths::RawPath;
use toc_span::FileId;

use crate::{ArtifactKind, DependencyList, Package, RootPackages};

#[derive(Default)]
#[salsa::db(crate::Jar, toc_paths::Jar)]
struct TestDb {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDb {}

#[test]
fn no_dedup_source_roots() {
    let db = &mut TestDb::default();
    let roots = vec![
        FileId::from(RawPath::new(db, "a".into())),
        FileId::from(RawPath::new(db, "b".into())),
        FileId::from(RawPath::new(db, "b".into())),
        FileId::from(RawPath::new(db, "c".into())),
    ];
    let expected_roots = {
        let mut roots = roots.clone();
        roots.reverse();
        roots
    };
    let roots = roots
        .into_iter()
        .map(|root| {
            Package::new(
                db,
                "a".into(),
                root.into_raw(),
                ArtifactKind::Binary,
                DependencyList::empty(db),
            )
        })
        .collect::<Vec<_>>();

    // Set the
    RootPackages::new(db, roots);

    // Don't dedup roots, since they're different packages
    assert_eq!(
        crate::source_graph(db)
            .as_ref()
            .unwrap()
            .all_packages(db)
            .iter()
            .map(|pkg| pkg.root(db).into())
            .collect::<Vec<FileId>>(),
        expected_roots
    );
}

// FIXME: add test for dep exploring
// FIXME: add test for cycle checking
