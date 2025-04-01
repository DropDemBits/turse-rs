use std::sync::{Arc, Mutex};

use camino::Utf8PathBuf;
use salsa::Database;
use toc_paths::RawPath;
use toc_source_graph::{ArtifactKind, DependencyList, Package, RootPackages};
use toc_vfs_db::{SourceTable, VfsBridge, VfsDbExt};

#[salsa::db]
#[derive(Default, Clone)]
struct TestDb {
    storage: salsa::Storage<Self>,
    source_table: SourceTable,

    logger: Arc<Mutex<Option<Vec<salsa::Event>>>>,
}

impl salsa::Database for TestDb {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        let mut events = self.logger.lock().unwrap();
        if let Some(events) = &mut *events {
            events.push(event());
        }
    }
}

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

        let root_file = RawPath::new(&db, Utf8PathBuf::from("src/main.t"));
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

    #[allow(unused)]
    fn log_output<R>(&self, f: impl FnOnce(&Self) -> R) -> (R, Vec<String>) {
        self.logger.lock().unwrap().replace(vec![]);
        self.attach(|db| {
            let res = f(db);
            let events = self.logger.lock().unwrap().take().unwrap();
            (res, format_events(self, events))
        })
    }
}

fn format_events(_db: &TestDb, events: Vec<salsa::Event>) -> Vec<String> {
    events
        .into_iter()
        .filter_map(|event| {
            let text = match event.kind {
                salsa::EventKind::WillCheckCancellation => return None,
                salsa::EventKind::DidValidateMemoizedValue { database_key } => {
                    format!("validate {:?}", database_key)
                }
                salsa::EventKind::WillBlockOn {
                    other_thread_id,
                    database_key,
                } => format!("block_on {:?} {:?}", other_thread_id, database_key),
                salsa::EventKind::WillExecute { database_key } => {
                    format!("exec {:?}", database_key)
                }
                salsa::EventKind::WillDiscardStaleOutput {
                    execute_key,
                    output_key,
                } => format!("discard_output {:?} {:?}", execute_key, output_key),
                salsa::EventKind::DidDiscard { key } => {
                    format!("discard_struct {:?}", key)
                }
                salsa::EventKind::DidDiscardAccumulated {
                    executor_key,
                    accumulator,
                } => format!("discard_accum {:?} {:?}", executor_key, accumulator),
                salsa::EventKind::DidSetCancellationFlag => format!("cancel requested"),
                salsa::EventKind::DidInternValue { id, revision } => {
                    format!("interned {id:?} {revision:?}")
                }
                salsa::EventKind::DidReinternValue { id, revision } => {
                    format!("reinterned {id:?} {revision:?}")
                }
            };

            Some(format!("{:?} -> {text}", event.thread_id))
        })
        .collect()
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
% what humans think is possible

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
    // constvars are a lil different since their locations are on the names
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
