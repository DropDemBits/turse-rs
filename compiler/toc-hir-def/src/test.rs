use toc_paths::RawPath;
use toc_source_graph::{ArtifactKind, DependencyList, Package, RootPackages};
use toc_vfs_db::{SourceTable, VfsBridge, VfsDbExt};

#[salsa::db(
    toc_paths::Jar,
    toc_vfs_db::Jar,
    toc_source_graph::Jar,
    toc_ast_db::Jar,
    toc_hir_expand::Jar,
    crate::Jar
)]
#[derive(Default)]
struct TestDb {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,
}

impl salsa::Database for TestDb {}

impl VfsBridge for TestDb {
    fn source_table(&self) -> &SourceTable {
        &self.source_table
    }
}

impl TestDb {
    pub(crate) fn from_source(source: &str) -> (Self, Package) {
        let mut db = TestDb::default();
        let fixture = toc_vfs::generate_vfs(source).unwrap();
        db.insert_fixture(fixture);

        let root_file = RawPath::new(&db, "src/main.t".into());
        let package = Package::new(
            &db,
            "main".into(),
            root_file,
            ArtifactKind::Binary,
            DependencyList::empty(&db),
        );
        RootPackages::new(&db, vec![package]);

        (db, package)
    }
}

#[test]
fn test_stable_item_locations_swap() {
    let (mut db, package) = TestDb::from_source(
        "
module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a

module b
    module d1 end d1
end b
",
    );
    let start_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    let swapped = toc_vfs::generate_vfs(
        "
module b
    module d1 end d1
end b

module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a
",
    )
    .unwrap();
    db.insert_fixture(swapped);

    let swapped_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    eprintln!("{start_tree} -> {swapped_tree}");

    assert_eq!(start_tree, swapped_tree);
}

#[test]
fn test_stable_item_locations_add_ws() {
    let (mut db, package) = TestDb::from_source(
        "
module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a

module b
    module d1 end d1
end b
",
    );
    let start_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    let swapped = toc_vfs::generate_vfs(
        "
module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a

% according to all known laws of aviation
% it is impossible for a bee to fly
% yet, it does so anyway because a bee does not care
% what human think is possible

module b
    module d1 end d1
end b
",
    )
    .unwrap();
    db.insert_fixture(swapped);

    let new_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    eprintln!("{start_tree} -> {new_tree}");

    assert_eq!(start_tree, new_tree);
}

#[test]
fn test_stable_item_locations_reinsert_swap() {
    let (mut db, package) = TestDb::from_source(
        "
module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a

module b
    module d1 end d1
end b

module c end c
",
    );
    let start_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    // intermediate step
    db.insert_fixture(
        toc_vfs::generate_vfs(
            "
module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a

module c end c
",
        )
        .unwrap(),
    );

    // reinsert
    let swapped = toc_vfs::generate_vfs(
        "
module b
    module d1 end d1
end b

module a
    module c1 end c1
    module c2 end c2
    module c3 end c3
end a

module c end c
",
    )
    .unwrap();
    db.insert_fixture(swapped);

    let swapped_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    eprintln!("{start_tree} -> {swapped_tree}");

    assert_eq!(start_tree, swapped_tree);
}

#[test]
fn test_stable_constvar_locations_delete_swap() {
    // constvars are a lil trickier since they also include an index
    let (mut db, package) = TestDb::from_source(
        "
const a, b, c := 1
",
    );
    let start_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    // intermediate step
    db.insert_fixture(
        toc_vfs::generate_vfs(
            "
const a, c := 1
",
        )
        .unwrap(),
    );

    // reinsert
    let swapped = toc_vfs::generate_vfs(
        "
const a, c, b := 1
",
    )
    .unwrap();
    db.insert_fixture(swapped);

    let swapped_tree = crate::item::pretty::render_item_tree(&db, package)
        .ensure_sorted()
        .render_as_tree();

    eprintln!("{start_tree} -> {swapped_tree}");

    assert_eq!(start_tree, swapped_tree);
}
