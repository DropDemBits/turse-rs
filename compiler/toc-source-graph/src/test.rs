use camino::Utf8PathBuf;
use toc_paths::RawPath;
use toc_span::FileId;

use crate::{ArtifactKind, DependencyList, Package, RootPackages};

#[derive(Default, Clone)]
#[salsa::db]
struct TestDb {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDb {
}

#[test]
fn no_dedup_source_roots() {
    let db = &mut TestDb::default();
    let roots = vec![
        FileId::from(RawPath::new(db, Utf8PathBuf::from("a"))),
        FileId::from(RawPath::new(db, Utf8PathBuf::from("b"))),
        FileId::from(RawPath::new(db, Utf8PathBuf::from("b"))),
        FileId::from(RawPath::new(db, Utf8PathBuf::from("c"))),
    ];
    let expected_roots = roots.clone();
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
    let mut actual_roots: Vec<FileId> = crate::source_graph(db)
        .as_ref()
        .unwrap()
        .all_packages(db)
        .iter()
        .map(|pkg| pkg.root(db).into())
        .collect();
    actual_roots.sort_by(|a, b| a.into_raw().raw_path(db).cmp(b.into_raw().raw_path(db)));

    assert_eq!(actual_roots, expected_roots);
}

// FIXME: add test for dep exploring
// FIXME: add test for cycle checking
